const lexer = @import("lexer.zig");
const Token = @import("token.zig").Token;
const ast = @import("ast.zig");
const std = @import("std");

const ParserError = error{
    UnexpectedToken,
    ParseInt,
};

const Error = struct {
    err: ParserError,
    expected: ?std.meta.Tag(Token) = null,
    got: ?std.meta.Tag(Token) = null,
    msg: ?[]const u8 = null,
};

const Operator = enum(u8) { LOWEST = 1, EQUALS = 2, LESSGREATER = 3, SUM = 4, PRODUCT = 5, PREFIX = 6, CALL = 7 };

const PrefixParseFn = *const fn (*Parser) anyerror!?*ast.Expression;
const InfixParseFn = *const fn (ast.Expression) anyerror!?*ast.Expression;

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
        try p.registerPrefix(.INT, parseIntegerLiteral);

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

    fn parseProgram(self: *Self) !*ast.Program {
        var program = ast.Program.init(self.allocator);
        errdefer program.node.deinit(self.allocator);

        while (self.curr_token != .EOF) {
            const stmt = try self.parseStatement();
            if (stmt != null) {
                try program.statements.append(stmt.?);
            }
            self.nextToken();
        }

        return program;
    }

    fn parseExpression(self: *Self, _: Operator) !?*ast.Expression {
        const prefix = self.prefixParseFns.get(self.curr_token);
        if (prefix == null) {
            return null;
        }

        const left_exp = try prefix.?(self);
        return left_exp;
    }

    fn parseStatement(self: *Self) !?*ast.Statement {
        return switch (self.curr_token) {
            .LET => try self.parseLetStatement(),
            .RETURN => try self.parseReturnStatement(),
            else => try self.parseExpressionStatement(),
        };
    }

    fn parseExpressionStatement(self: *Self) !?*ast.Statement {
        var stmt = try ast.ExpressionStatement.init(self.allocator, self.curr_token, null);
        errdefer stmt.statement.node.deinit(self.allocator);

        const exp = try self.parseExpression(.LOWEST);
        if (exp == null) {
            stmt.statement.node.deinit(self.allocator);
            return null;
        }

        stmt.expression = exp;

        while (!self.currTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return &stmt.statement;
    }

    fn parseReturnStatement(self: *Self) !?*ast.Statement {
        const stmt = try ast.ReturnStatement.init(self.allocator, self.curr_token, null);
        errdefer stmt.statement.node.deinit(self.allocator);
        self.nextToken();

        while (!self.currTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return &stmt.statement;
    }

    fn parseLetStatement(self: *Self) !?*ast.Statement {
        const let_token = self.curr_token;
        if (!try self.expectPeek(.IDENT)) {
            return null;
        }

        const name = try ast.Identifier.init(self.allocator, self.curr_token, switch (self.curr_token) {
            .IDENT => |ident| ident,
            else => unreachable,
        });
        errdefer name.expression.node.deinit(self.allocator);

        if (!try self.expectPeek(.ASSIGN)) {
            return null;
        }

        while (!self.currTokenIs(.SEMICOLON)) {
            if (self.currTokenIs(.EOF)) {
                return null;
            }

            self.nextToken();
        }

        const let_stmt = try ast.LetStatement.init(self.allocator, let_token, name, null);
        return &let_stmt.statement;
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
        try self.errors.append(Error{
            .err = ParserError.UnexpectedToken,
            .expected = t,
            .got = self.peek_token,
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

fn parseIdentifier(p: *Parser) !?*ast.Expression {
    const i = try ast.Identifier.init(p.allocator, p.curr_token, p.curr_token.toLiteral());
    errdefer i.expression.node.deinit(p.allocator);
    return &i.expression;
}

fn parseIntegerLiteral(p: *Parser) !?*ast.Expression {
    var lit = try ast.IntegerLiteral.init(p.allocator, p.curr_token, null);
    errdefer lit.expression.node.deinit(p.allocator);

    const int = std.fmt.parseInt(i64, p.curr_token.toLiteral(), 10) catch |err| {
        const error_msg = switch (err) {
            error.InvalidCharacter => "invalid character in integer literal",
            error.Overflow => "integer overflow",
        };

        p.errors.append(Error{
            .err = ParserError.ParseInt,
            .msg = error_msg,
        }) catch return null;

        return null;
    };

    lit.value = int;
    return &lit.expression;
}

fn printError(err: Error) void {
    std.debug.print("Parser error: {s}", .{@errorName(err.err)});
    if (err.msg != null) {
        std.debug.print("{s}", .{err.msg.?});
    } else if (err.expected != null and err.got != null) {
        std.debug.print(" - expected next token to be {s}, got {s} instead", .{
            @tagName(err.expected.?),
            @tagName(err.got.?),
        });
    }
}

test "parsing prefix expressions" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        operator: []const u8,
        iv: i64,
    }{
        .{ .input = "!5;", .operator = "!", .iv = 5 },
        .{ .input = "-15;", .operator = "-", .iv = 15 },
    };

    for (tests) |tt| {
        var l = lexer.Lexer.init(tt.input);
        var p = try Parser.init(allocator, &l);
        defer p.deinit();

        var program = try p.parseProgram();
        defer program.node.deinit(allocator);

        const errs = p.getErrors();
        for (errs) |err| {
            printError(err);
        }

        try std.testing.expectEqual(0, p.getErrors().len);
        try std.testing.expect(program.statements.items.len > 0);
        try std.testing.expectEqual(1, program.statements.items.len);

        try std.testing.expectEqual(ast.NodeType.ExpressionStatement, program.statements.items[0].node.getType());
        const expr_stmt: *ast.ExpressionStatement = @fieldParentPtr("statement", program.statements.items[0]);
        try std.testing.expectEqual(@TypeOf(expr_stmt), *ast.ExpressionStatement);

        try std.testing.expect(expr_stmt.expression != null);
        try std.testing.expectEqual(ast.NodeType.PrefixExpression, expr_stmt.expression.?.node.getType());
        const pe: *ast.PrefixExpression = @fieldParentPtr("expression", expr_stmt.expression.?);

        try std.testing.expectEqual(ast.NodeType.IntegerLiteral, pe.right.node.getType());
        const il: *ast.IntegerLiteral = @fieldParentPtr("expression", pe.right);
        try std.testing.expectEqual(tt.iv, il.value);
    }
}

test "integer literal expr" {
    const input = "5;";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var l = lexer.Lexer.init(input);
    var p = try Parser.init(allocator, &l);
    defer p.deinit();

    var program = try p.parseProgram();
    defer program.node.deinit(allocator);

    const errs = p.getErrors();
    for (errs) |err| {
        printError(err);
    }

    try std.testing.expectEqual(0, p.getErrors().len);
    try std.testing.expect(program.statements.items.len > 0);
    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    try std.testing.expectEqual(ast.NodeType.ExpressionStatement, program.statements.items[0].node.getType());
    const stmt: *ast.ExpressionStatement = @ptrCast(program.statements.items[0]);

    try std.testing.expect(stmt.expression != null);
    try std.testing.expectEqual(ast.NodeType.IntegerLiteral, stmt.expression.?.node.getType());
    const expr: *ast.IntegerLiteral = @ptrCast(stmt.expression.?);

    const literal = expr.value;
    try std.testing.expectEqual(5, literal.?);
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
    defer program.node.deinit(allocator);

    const errs = p.getErrors();
    for (errs) |err| {
        printError(err);
    }

    try std.testing.expectEqual(0, p.getErrors().len);
    try std.testing.expect(program.statements.items.len > 0);
    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    try std.testing.expectEqual(ast.NodeType.ExpressionStatement, program.statements.items[0].node.getType());
    const stmt: *ast.ExpressionStatement = @ptrCast(program.statements.items[0]);

    try std.testing.expect(stmt.expression != null);
    try std.testing.expectEqual(ast.NodeType.Identifier, stmt.expression.?.node.getType());
    const expr: *ast.Identifier = @ptrCast(stmt.expression.?);

    const ident = expr.value;
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
    defer program.node.deinit(allocator);

    const errs = p.getErrors();
    for (errs) |err| {
        printError(err);
    }

    try std.testing.expectEqual(0, p.getErrors().len);
    try std.testing.expect(program.statements.items.len > 0);
    try std.testing.expectEqual(@as(usize, 3), program.statements.items.len);

    for (program.statements.items) |stmt| {
        try std.testing.expectEqual(ast.NodeType.ReturnStatement, stmt.node.getType());
        try std.testing.expectEqualStrings("return", stmt.node.tokenLiteral());
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
    defer program.node.deinit(allocator);

    const errs = p.getErrors();
    for (errs) |err| {
        printError(err);
    }

    try std.testing.expectEqual(0, p.getErrors().len);
    try std.testing.expect(program.statements.items.len > 0);
    try std.testing.expectEqual(@as(usize, 3), program.statements.items.len);

    const expected_identifiers = [_][]const u8{ "x", "y", "foobar" };

    for (program.statements.items, 0..) |stmt, i| {
        try std.testing.expectEqual(ast.NodeType.LetStatement, stmt.node.getType());
        const stmt_cast: *ast.LetStatement = @ptrCast(stmt);

        try std.testing.expectEqualStrings("let", stmt.node.tokenLiteral());
        try std.testing.expectEqualStrings(expected_identifiers[i], stmt_cast.name.value);
        try std.testing.expectEqualStrings(expected_identifiers[i], stmt_cast.name.expression.node.tokenLiteral());
    }
}
