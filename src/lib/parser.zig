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

const Operator = enum(u8) { LOWEST = 1, EQUALS = 2, LESSGREATER = 3, SUM = 4, PRODUCT = 5, PREFIX = 6, CALL = 7 };

const PrefixParseFn = *const fn (*Parser) ast.Expression;
const InfixParseFn = *const fn (ast.Expression) ast.Expression;

const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    l: *lexer.Lexer,
    errors: std.ArrayList(Error),
    curr_token: Token,
    peek_token: Token,

    prefixParseFns: std.AutoHashMap(std.meta.Tag(Token), PrefixParseFn),
    infixParseFns: std.AutoHashMap(std.meta.Tag(Token), InfixParseFn),

    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) !Self {
        const e = std.ArrayList(Error).init(allocator);
        var p = Parser{ .l = l, .curr_token = undefined, .peek_token = undefined, .prefixParseFns = undefined, .infixParseFns = undefined, .errors = e, .allocator = allocator };
        p.prefixParseFns = std.AutoHashMap(std.meta.Tag(Token), PrefixParseFn).init(allocator);
        try p.registerPrefix(.IDENT, parseIdentifier);

        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
    }

    fn nextToken(self: *Self) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.l.nextToken();
    }

    fn parseProgram(self: *Self) !ast.Program {
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

    fn parseExpression(self: *Self, _: Operator) !?ast.Expression {
        const prefix = self.prefixParseFns.get(self.curr_token);
        if (prefix == null) {
            return null;
        }

        const left_exp = prefix.?(self);
        return left_exp;
    }

    fn parseStatement(self: *Self) !?ast.Statement {
        return switch (self.curr_token) {
            .LET => try self.parseLetStatement(),
            .RETURN => try self.parseReturnStatement(),
            else => try self.parseExpressionStatement(),
        };
    }

    fn parseExpressionStatement(self: *Self) !?ast.Statement {
        var stmt = ast.Statement{ .Expression = ast.ExpressionStatement.init(self.curr_token, null) };
        const exp = try self.parseExpression(.LOWEST);
        if (exp == null) {
            return null;
        }

        stmt.Expression.expression = exp.?;

        while (!self.currTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return stmt;
    }

    fn parseReturnStatement(self: *Self) !?ast.Statement {
        const stmt = ast.Statement{ .Return = ast.ReturnStatement.init(self.curr_token, null) };
        self.nextToken();

        while (!self.currTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return stmt;
    }

    fn parseLetStatement(self: *Self) !?ast.Statement {
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

    fn currTokenIs(self: *Self, t: std.meta.Tag(Token)) bool {
        return std.meta.activeTag(self.curr_token) == t;
    }

    fn peekTokenIs(self: *Self, t: std.meta.Tag(Token)) bool {
        return std.meta.activeTag(self.peek_token) == t;
    }

    fn expectPeek(self: *Self, t: std.meta.Tag(Token)) !bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();
            return true;
        }

        try self.peekError(t);
        return false;
    }

    fn peekError(self: *Self, t: std.meta.Tag(Token)) !void {
        const msg = try std.fmt.allocPrint(self.allocator, "expected next token to be {s}, got {s} instead", .{ @tagName(t), @tagName(self.peek_token) });
        errdefer self.allocator.free(msg);

        try self.errors.append(Error{
            .err = ParserError.UnexpectedToken,
            .msg = msg,
        });
    }

    pub fn getErrors(self: *Self) []const Error {
        return self.errors.items;
    }

    pub fn registerPrefix(self: *Self, t: std.meta.Tag(Token), f: PrefixParseFn) !void {
        try self.prefixParseFns.put(t, f);
    }

    pub fn registerInfix(self: *Self, t: std.meta.Tag(Token), f: InfixParseFn) !void {
        try self.infixParseFns.put(t, f);
    }
};

fn parseIdentifier(p: *Parser) ast.Expression {
    return ast.Expression{ .Identifier = ast.Identifier.init(p.curr_token, p.curr_token.toLiteral()) };
}

test "identifier" {
    const input = "foobar;";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = lexer.Lexer.init(input);
    var p = try Parser.init(allocator, &l);
    defer p.deinit();

    var program = try p.parseProgram();
    defer program.deinit(allocator);

    const errs = p.getErrors();
    for (errs) |err| {
        std.debug.print("Parser error: {s} - {s}\n", .{ @errorName(err.err), err.msg });
    }

    try std.testing.expectEqual(0, p.getErrors().len);
    try std.testing.expect(program.statements.items.len > 0);
    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const stmt = program.statements.items[0];
    try std.testing.expect(stmt == .Expression);
    const expr = stmt.Expression.expression.?;
    try std.testing.expect(expr == .Identifier);

    const ident = expr.Identifier.value;
    try std.testing.expectEqualStrings("foobar", ident);
}

test "return statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = lexer.Lexer.init(input);
    var p = try Parser.init(allocator, &l);
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

    for (program.statements.items) |stmt| {
        try std.testing.expect(stmt == .Return);

        const return_stmt = stmt.Return;
        try std.testing.expectEqualStrings("return", return_stmt.tokenLiteral());
    }
}

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
    var p = try Parser.init(allocator, &l);
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
        try std.testing.expect(stmt == .Let);
        const let_stmt = stmt.Let;

        try std.testing.expectEqualStrings("let", let_stmt.tokenLiteral());
        try std.testing.expectEqualStrings(expected_identifiers[i], let_stmt.name.value);
        try std.testing.expectEqualStrings(expected_identifiers[i], let_stmt.name.tokenLiteral());
    }
}
