const lexer = @import("lexer.zig");
const Token = @import("token.zig").Token;
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
    curr_token: Token,
    peek_token: Token,
    errors: std.ArrayList(Error),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) Parser {
        const e = std.ArrayList(Error).init(allocator);
        var p = Parser{ .l = l, .curr_token = undefined, .peek_token = undefined, .errors = e, .allocator = allocator };

        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    fn nextToken(self: *Parser) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.l.nextToken();
    }

    fn parseProgram(self: *Parser) !ast.Program {
        var program = ast.Program.init(self.allocator);
        errdefer program.deinit(self.allocator);

        while (self.curr_token != .EOF) {
            const stmt = try self.parseStatement();
            if (stmt != null) {
                try program.statements.append(stmt.?);
            }
            self.nextToken();
        }

        return program;
    }

    fn parseStatement(self: *Parser) !?ast.Statement {
        return switch (self.curr_token) {
            .LET => try self.parseLetStatement(),
            else => null,
        };
    }

    fn parseLetStatement(self: *Parser) !?ast.Statement {
        const let_token = self.curr_token;
        if (!try self.expectPeek(.IDENT)) {
            return null;
        }

        const name = ast.Identifier.init(self.curr_token, switch (self.curr_token) {
            .IDENT => |ident| ident,
            else => unreachable,
        });

        if (!try self.expectPeek(.ASSIGN)) {
            return null;
        }

        while (!self.currTokenIs(.SEMICOLON)) {
            if (self.currTokenIs(.EOF)) {
                return null;
            }

            self.nextToken();
        }

        return ast.Statement{ .Let = ast.LetStatement.init(let_token, name, null) };
    }

    fn currTokenIs(self: *Parser, t: std.meta.Tag(Token)) bool {
        return std.meta.activeTag(self.curr_token) == t;
    }

    fn peekTokenIs(self: *Parser, t: std.meta.Tag(Token)) bool {
        return std.meta.activeTag(self.peek_token) == t;
    }

    fn expectPeek(self: *Parser, t: std.meta.Tag(Token)) !bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();
            return true;
        }

        try self.peekError(t);
        return false;
    }

    fn peekError(self: *Parser, t: std.meta.Tag(Token)) !void {
        const msg = try std.fmt.allocPrint(self.allocator, "expected next token to be {s}, got {s} instead", .{ @tagName(t), @tagName(self.peek_token) });
        errdefer self.allocator.free(msg);

        try self.errors.append(Error{
            .err = ParserError.UnexpectedToken,
            .msg = msg,
        });
    }

    pub fn getErrors(self: *Parser) []const Error {
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

    var program = try p.parseProgram();
    defer program.deinit(allocator);

    const errs = p.getErrors();
    for (errs) |err| {
        std.debug.print("Parser error: {s} - {s}\n", .{ @errorName(err.err), err.msg });
    }

    try std.testing.expectEqual(0, p.getErrors().len);
    try std.testing.expect(program.statements.items.len > 0);
    try std.testing.expectEqual(@as(usize, 3), program.statements.items.len);

    const expected_identifiers = [_][]const u8{ "x", "y", "foobar" };

    for (program.statements.items, 0..) |stmt, i| {
        try testLetStatement(&stmt, expected_identifiers[i]);
    }
}

fn testLetStatement(stmt: *const ast.Statement, expected_name: []const u8) !void {
    try std.testing.expect(stmt.* == .Let);
    const let_stmt = stmt.Let;

    try std.testing.expectEqualStrings("LET", let_stmt.tokenLiteral());
    try std.testing.expectEqualStrings(expected_name, let_stmt.name.value);
    try std.testing.expectEqualStrings(expected_name, let_stmt.name.tokenLiteral());
}
