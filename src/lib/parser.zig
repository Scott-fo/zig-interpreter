const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");
const std = @import("std");

const ParserError = error{
    UnexpectedToken,
};

const Error = struct {
    err: ParserError,
    msg: []const u8,
};

const Parser = struct {
    l: *lexer.Lexer,
    curr_token: token.Token,
    peek_token: token.Token,
    errors: std.ArrayList(Error),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) Parser {
        const e = std.ArrayList(Error).init(allocator);
        var p = Parser{ .l = l, .curr_token = undefined, .peek_token = undefined, .errors = e, .allocator = allocator };

        p.next_token();
        p.next_token();

        return p;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    fn next_token(self: *Parser) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.l.next_token();
    }

    fn parse_program(self: *Parser) !ast.Program {
        var program = ast.Program.init(self.allocator);
        errdefer program.deinit(self.allocator);

        while (self.curr_token.type != .EOF) {
            const stmt = try self.parse_statement();
            if (stmt != null) {
                try program.statements.append(stmt.?);
            }
            self.next_token();
        }

        return program;
    }

    fn parse_statement(self: *Parser) !?ast.Statement {
        return switch (self.curr_token.type) {
            token.TokenType.LET => try self.parse_let_statement(),
            else => null,
        };
    }

    fn parse_let_statement(self: *Parser) !?ast.Statement {
        const let_token = self.curr_token;
        if (!try self.expect_peek(.IDENT)) {
            return null;
        }

        const name = ast.Identifier.init(self.curr_token, self.curr_token.literal);

        if (!try self.expect_peek(.ASSIGN)) {
            return null;
        }

        while (!self.curr_token_is(.SEMICOLON)) {
            if (self.curr_token_is(.EOF)) {
                return null;
            }

            self.next_token();
        }

        return ast.Statement{ .Let = ast.LetStatement.init(let_token, name, null) };
    }

    fn curr_token_is(self: *Parser, t: token.TokenType) bool {
        return self.curr_token.type == t;
    }

    fn peek_token_is(self: *Parser, t: token.TokenType) bool {
        return self.peek_token.type == t;
    }

    fn expect_peek(self: *Parser, t: token.TokenType) !bool {
        if (self.peek_token_is(t)) {
            self.next_token();
            return true;
        }

        try self.peek_error(t);
        return false;
    }

    fn peek_error(self: *Parser, t: token.TokenType) !void {
        const msg = try std.fmt.allocPrint(self.allocator, "expected next token to be {}, got {} instead", .{ t, self.peek_token.type });
        errdefer self.allocator.free(msg);

        try self.errors.append(Error{
            .err = ParserError.UnexpectedToken,
            .msg = msg,
        });
    }

    pub fn get_errors(self: *Parser) []const Error {
        return self.errors.items;
    }
};

test "let statement" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = lexer.Lexer.init(input);
    var p = Parser.init(allocator, &l);
    defer p.deinit();

    var program = try p.parse_program();
    defer program.deinit(allocator);

    const errs = p.get_errors();
    for (errs) |err| {
        std.debug.print("Parser error: {s} - {s}\n", .{ @errorName(err.err), err.msg });
    }

    try std.testing.expectEqual(0, p.get_errors().len);
    try std.testing.expect(program.statements.items.len > 0);
    try std.testing.expectEqual(@as(usize, 3), program.statements.items.len);

    const expected_identifiers = [_][]const u8{ "x", "y", "foobar" };

    for (program.statements.items, 0..) |stmt, i| {
        try test_let_statement(&stmt, expected_identifiers[i]);
    }
}

fn test_let_statement(stmt: *const ast.Statement, expected_name: []const u8) !void {
    try std.testing.expect(stmt.* == .Let);
    const let_stmt = stmt.Let;

    try std.testing.expectEqualStrings("let", let_stmt.token.literal);
    try std.testing.expectEqualStrings(expected_name, let_stmt.name.value);
    try std.testing.expectEqualStrings(expected_name, let_stmt.name.token.literal);
}
