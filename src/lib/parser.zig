const lexer = @import("lexer.zig");
const Token = @import("token.zig").Token;
const ast = @import("ast.zig");
const std = @import("std");

const ParserError = error{
    UnexpectedToken,
    NoPrefixParseFunction,
    ParseInt,
};

const Error = struct {
    err: ParserError,
    expected: ?std.meta.Tag(Token) = null,
    got: ?std.meta.Tag(Token) = null,
    msg: ?[]const u8 = null,
};

const Operator = enum(u8) {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
};

const PrefixParseFn = *const fn (*Parser) anyerror!?*ast.Expression;
const InfixParseFn = *const fn (*Parser, *ast.Expression) anyerror!?*ast.Expression;

fn getPrefixFn(t: std.meta.Tag(Token)) ?PrefixParseFn {
    return switch (t) {
        .IDENT => parseIdentifier,
        .INT => parseIntegerLiteral,
        .BANG, .MINUS => parsePrefixExpression,
        .TRUE, .FALSE => parseBoolean,
        .LPAREN => parseGroupedExpression,
        .IF => parseIfExpression,
        .FUNCTION => parseFunctionLiteral,
        else => null,
    };
}

fn getInfixFn(t: std.meta.Tag(Token)) ?InfixParseFn {
    return switch (t) {
        .PLUS, .MINUS, .SLASH, .ASTERISK, .EQ, .NOT_EQ, .LT, .GT => parseInfixExpression,
        .LPAREN => parseCallExpression,
        else => null,
    };
}

fn parseCallArguments(p: *Parser) !?std.ArrayList(*ast.Expression) {
    var es = std.ArrayList(*ast.Expression).init(p.allocator);
    errdefer es.deinit();

    if (p.peekTokenIs(.RPAREN)) {
        p.nextToken();
        return es;
    }

    p.nextToken();

    var e: ?*ast.Expression = null;
    e = try p.parseExpression(.LOWEST);

    if (e != null) {
        try es.append(e.?);
    }

    while (p.peekTokenIs(.COMMA)) {
        p.nextToken();
        p.nextToken();

        e = try p.parseExpression(.LOWEST);
        if (e != null) {
            try es.append(e.?);
        }
    }

    if (!try p.expectPeek(.RPAREN)) {
        return null;
    }

    return es;
}

fn parseCallExpression(p: *Parser, func: *ast.Expression) !?*ast.Expression {
    const exp = try ast.CallExpression.init(p.allocator, p.curr_token, func);
    errdefer exp.expression.node.deinit(p.allocator);

    const args = try parseCallArguments(p);
    if (args != null) {
        exp.arguments = args.?;
    }
    return &exp.expression;
}

fn parseInfixExpression(p: *Parser, left: *ast.Expression) !?*ast.Expression {
    const curr = p.curr_token;
    const operator = p.curr_token.toLiteral();
    const precendence = p.curPrecedence();

    p.nextToken();

    const right = try p.parseExpression(precendence);
    if (right == null) {
        return null;
    }

    const ie = try ast.InfixExpression.init(p.allocator, curr, operator, left, right.?);
    errdefer ie.expression.node.deinit(p.allocator);

    return &ie.expression;
}

fn parseFunctionParams(p: *Parser) !?std.ArrayList(*ast.Identifier) {
    var idents = std.ArrayList(*ast.Identifier).init(p.allocator);
    errdefer idents.deinit();

    if (p.peekTokenIs(.RPAREN)) {
        p.nextToken();
        return idents;
    }

    p.nextToken();

    var ident: *ast.Identifier = undefined;
    ident = try ast.Identifier.init(p.allocator, p.curr_token, p.curr_token.toLiteral());
    errdefer ident.expression.node.deinit(p.allocator);

    try idents.append(ident);

    while (p.peekTokenIs(.COMMA)) {
        p.nextToken();
        p.nextToken();

        ident = try ast.Identifier.init(p.allocator, p.curr_token, p.curr_token.toLiteral());
        errdefer ident.expression.node.deinit(p.allocator);

        try idents.append(ident);
    }

    if (!try p.expectPeek(.RPAREN)) {
        return null;
    }

    return idents;
}

fn parseFunctionLiteral(p: *Parser) !?*ast.Expression {
    const curr = p.curr_token;

    if (!try p.expectPeek(.LPAREN)) {
        return null;
    }

    const params = try parseFunctionParams(p);

    if (!try p.expectPeek(.LBRACE)) {
        return null;
    }

    const body = try parseBlockStatement(p);

    var lit = try ast.FunctionLiteral.init(p.allocator, curr, body);
    errdefer lit.expression.node.deinit(p.allocator);

    if (params != null) {
        lit.parameters = params.?;
    }

    return &lit.expression;
}

fn parseBlockStatement(p: *Parser) !*ast.BlockStatement {
    var block = try ast.BlockStatement.init(p.allocator, p.curr_token);
    errdefer block.node.deinit(p.allocator);

    p.nextToken();

    while (!p.currTokenIs(.RBRACE) and !p.currTokenIs(.EOF)) {
        const stmt = try p.parseStatement();
        if (stmt != null) {
            try block.statements.append(stmt.?);
        }

        p.nextToken();
    }

    return block;
}

fn parseIfExpression(p: *Parser) !?*ast.Expression {
    const curr = p.curr_token;

    if (!try p.expectPeek(.LPAREN)) {
        return null;
    }

    p.nextToken();
    const condition = try p.parseExpression(.LOWEST);
    if (condition == null) {
        return null;
    }

    if (!try p.expectPeek(.RPAREN)) {
        return null;
    }

    if (!try p.expectPeek(.LBRACE)) {
        return null;
    }

    const consequence = try parseBlockStatement(p);

    var alternative: ?*ast.BlockStatement = null;
    if (p.peekTokenIs(.ELSE)) {
        p.nextToken();

        if (!try p.expectPeek(.LBRACE)) {
            return null;
        }

        alternative = try parseBlockStatement(p);
    }

    const ie = try ast.IfExpression.init(p.allocator, curr, condition.?, consequence, alternative);
    errdefer ie.expression.node.deinit(p.allocator);

    return &ie.expression;
}

fn parseGroupedExpression(p: *Parser) !?*ast.Expression {
    p.nextToken();

    const exp = p.parseExpression(.LOWEST);

    const closed = try p.expectPeek(.RPAREN);
    if (!closed) {
        return null;
    }

    return exp;
}

fn parsePrefixExpression(p: *Parser) !?*ast.Expression {
    const curr = p.curr_token;
    const operator = p.curr_token.toLiteral();

    p.nextToken();

    const right = try p.parseExpression(.PREFIX);
    if (right == null) {
        return null;
    }

    const pe = try ast.PrefixExpression.init(p.allocator, curr, operator, right.?);
    errdefer pe.expression.node.deinit(p.allocator);

    return &pe.expression;
}

fn parseIdentifier(p: *Parser) !?*ast.Expression {
    const i = try ast.Identifier.init(p.allocator, p.curr_token, p.curr_token.toLiteral());
    errdefer i.expression.node.deinit(p.allocator);
    return &i.expression;
}

fn parseBoolean(p: *Parser) !?*ast.Expression {
    const b = try ast.Boolean.init(p.allocator, p.curr_token, p.currTokenIs(.TRUE));
    errdefer b.expression.node.deinit(p.allocator);

    return &b.expression;
}

fn parseIntegerLiteral(p: *Parser) !?*ast.Expression {
    var lit = try ast.IntegerLiteral.init(p.allocator, p.curr_token, null);
    errdefer lit.expression.node.deinit(p.allocator);

    const int = std.fmt.parseInt(i64, p.curr_token.toLiteral(), 10) catch |err| {
        const error_msg = switch (err) {
            error.InvalidCharacter => "invalid character in integer literal",
            error.Overflow => "integer overflow",
        };

        try p.errors.append(Error{
            .err = ParserError.ParseInt,
            .msg = error_msg,
        });

        return null;
    };

    lit.value = int;
    return &lit.expression;
}

pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    l: *lexer.Lexer,
    errors: std.ArrayList(Error),
    curr_token: Token,
    peek_token: Token,

    pub fn init(allocator: std.mem.Allocator, l: *lexer.Lexer) !Self {
        const e = std.ArrayList(Error).init(allocator);
        var p = Parser{
            .l = l,
            .curr_token = undefined,
            .peek_token = undefined,
            .errors = e,
            .allocator = allocator,
        };

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

    pub fn parseProgram(self: *Self) !*ast.Program {
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

    fn parseExpression(self: *Self, precedence: Operator) !?*ast.Expression {
        const prefix = getPrefixFn(self.curr_token);
        if (prefix == null) {
            try self.noPrefixParseFnError();
            return null;
        }

        var left_exp = try prefix.?(self);
        if (left_exp == null) {
            return null;
        }

        while (!self.peekTokenIs(.SEMICOLON) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
            const infix = getInfixFn(self.peek_token);
            if (infix == null) {
                return left_exp;
            }

            self.nextToken();

            left_exp = try infix.?(self, left_exp.?);
            if (left_exp == null) {
                return null;
            }
        }
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

        stmt.expression = try self.parseExpression(.LOWEST);

        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return &stmt.statement;
    }

    fn parseReturnStatement(self: *Self) !?*ast.Statement {
        const stmt = try ast.ReturnStatement.init(self.allocator, self.curr_token, null);
        errdefer stmt.statement.node.deinit(self.allocator);

        self.nextToken();

        stmt.value = try self.parseExpression(.LOWEST);

        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return &stmt.statement;
    }

    fn parseLetStatement(self: *Self) !?*ast.Statement {
        const let_token = self.curr_token;

        if (!try self.expectPeek(.IDENT)) {
            return null;
        }

        const name = try ast.Identifier.init(
            self.allocator,
            self.curr_token,
            switch (self.curr_token) {
                .IDENT => |ident| ident,
                else => unreachable,
            },
        );
        errdefer name.expression.node.deinit(self.allocator);

        if (!try self.expectPeek(.ASSIGN)) {
            return null;
        }

        self.nextToken();

        const value = try self.parseExpression(.LOWEST);

        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        const let_stmt = try ast.LetStatement.init(
            self.allocator,
            let_token,
            name,
            value,
        );
        return &let_stmt.statement;
    }

    fn currTokenIs(self: *Self, t: std.meta.Tag(Token)) bool {
        return std.meta.activeTag(self.curr_token) == t;
    }

    fn getPrecendence(t: Token) Operator {
        return switch (t) {
            .EQ => .EQUALS,
            .NOT_EQ => .EQUALS,
            .LT => .LESSGREATER,
            .GT => .LESSGREATER,
            .PLUS => .SUM,
            .MINUS => .SUM,
            .SLASH => .PRODUCT,
            .ASTERISK => .PRODUCT,
            .LPAREN => .CALL,
            else => .LOWEST,
        };
    }

    fn curPrecedence(self: *Self) Operator {
        return getPrecendence(self.curr_token);
    }

    fn peekPrecedence(self: *Self) Operator {
        return getPrecendence(self.peek_token);
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

    fn noPrefixParseFnError(self: *Self) !void {
        try self.errors.append(Error{
            .err = ParserError.NoPrefixParseFunction,
            .got = self.curr_token,
        });
    }

    pub fn getErrors(self: *Self) []const Error {
        return self.errors.items;
    }
};

pub fn printError(err: Error) void {
    std.debug.print("Parser error: {s}", .{@errorName(err.err)});
    if (err.msg != null) {
        std.debug.print("{s}", .{err.msg.?});
    } else if (err.expected != null and err.got != null) {
        std.debug.print(" - expected next token to be {s}, got {s} instead\n", .{
            @tagName(err.expected.?),
            @tagName(err.got.?),
        });
    } else if (err.got != null) {
        std.debug.print(" - got {s}\n", .{@tagName(err.got.?)});
    }
}

const TestLiteral = union(enum) {
    int: i64,
    boolean: bool,
    ident: []const u8,
};

fn testParseProgram(allocator: std.mem.Allocator, input: []const u8) !*ast.Program {
    var l = lexer.Lexer.init(input);
    var p = try Parser.init(allocator, &l);
    errdefer p.deinit();

    var program = try p.parseProgram();
    errdefer program.node.deinit(allocator);

    const errs = p.getErrors();
    for (errs) |err| {
        printError(err);
    }

    try std.testing.expectEqual(0, p.getErrors().len);
    return program;
}

fn expectStatementLength(program: *ast.Program, length: usize) !void {
    try std.testing.expect(program.statements.items.len > 0);
    try std.testing.expectEqual(length, program.statements.items.len);
}

fn expectExpressionStatement(stmt: *ast.Statement) !*ast.ExpressionStatement {
    try std.testing.expectEqual(ast.NodeType.ExpressionStatement, stmt.node.getType());
    return @ptrCast(stmt);
}

fn expectIfExpression(expr: *ast.Expression) !*ast.IfExpression {
    try std.testing.expectEqual(ast.NodeType.IfExpression, expr.node.getType());
    return @ptrCast(expr);
}

fn expectFunctionLiteral(expr: *ast.Expression, length: usize) !*ast.FunctionLiteral {
    try std.testing.expectEqual(ast.NodeType.FunctionLiteral, expr.node.getType());
    const fe: *ast.FunctionLiteral = @ptrCast(expr);
    try std.testing.expect(fe.parameters.items.len == length);
    return fe;
}

fn expectLiteral(expr: *ast.Expression, expected: TestLiteral) !void {
    switch (expected) {
        .int => |v| try expectIntegerLiteral(expr, v),
        .boolean => |v| try expectBoolean(expr, v),
        .ident => |v| try expectIdentifier(expr, v),
    }
}

fn expectBoolean(expr: *ast.Expression, expected: bool) !void {
    try std.testing.expectEqual(ast.NodeType.Boolean, expr.node.getType());
    const il: *ast.Boolean = @ptrCast(expr);
    try std.testing.expectEqual(expected, il.value);
}

fn expectIntegerLiteral(expr: *ast.Expression, expected: i64) !void {
    try std.testing.expectEqual(ast.NodeType.IntegerLiteral, expr.node.getType());
    const il: *ast.IntegerLiteral = @ptrCast(expr);
    try std.testing.expectEqual(expected, il.value);
}

fn expectIdentifier(expr: *ast.Expression, expected: []const u8) !void {
    try std.testing.expectEqual(ast.NodeType.Identifier, expr.node.getType());
    const ident: *ast.Identifier = @ptrCast(expr);
    try std.testing.expectEqualStrings(expected, ident.value);
}

fn expectInfixExpression(
    expr: *ast.Expression,
    left: TestLiteral,
    op: []const u8,
    right: TestLiteral,
) !void {
    try std.testing.expectEqual(ast.NodeType.InfixExpression, expr.node.getType());
    const ie: *ast.InfixExpression = @ptrCast(expr);

    try std.testing.expectEqualStrings(op, ie.operator);
    try expectLiteral(ie.left, left);
    try expectLiteral(ie.right, right);
}

fn expectPrefixExpression(
    expr: *ast.Expression,
    op: []const u8,
    right: TestLiteral,
) !void {
    try std.testing.expectEqual(ast.NodeType.PrefixExpression, expr.node.getType());
    const pe: *ast.PrefixExpression = @ptrCast(expr);

    try std.testing.expectEqualStrings(op, pe.operator);
    try expectLiteral(pe.right, right);
}

test "call expression parameter parsing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedIdent: []const u8,
        expectedParams: []const []const u8,
    }{
        .{
            .input = "add();",
            .expectedIdent = "add",
            .expectedParams = &[_][]const u8{},
        },
        .{
            .input = "add(1);",
            .expectedIdent = "add",
            .expectedParams = &[_][]const u8{"1"},
        },
        .{
            .input = "add(1, 2 * 3, 4 + 5);",
            .expectedIdent = "add",
            .expectedParams = &[_][]const u8{ "1", "(2 * 3)", "(4 + 5)" },
        },
    };

    for (tests) |tt| {
        var program = try testParseProgram(allocator, tt.input);
        defer program.node.deinit(allocator);

        const stmt = try expectExpressionStatement(program.statements.items[0]);
        try std.testing.expect(stmt.expression != null);

        try std.testing.expectEqual(ast.NodeType.CallExpression, stmt.expression.?.node.getType());
        const ce: *ast.CallExpression = @ptrCast(stmt.expression.?);

        try expectIdentifier(ce.function, tt.expectedIdent);
        try std.testing.expect(ce.arguments.items.len == tt.expectedParams.len);

        for (tt.expectedParams, 0..) |param, i| {
            const str = try ce.arguments.items[i].node.string(allocator);
            defer allocator.free(str);

            try std.testing.expectEqualStrings(param, str);
        }
    }
}

test "call expression parsing" {
    const input = "add(1, 2 * 3, 4 + 5)";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);

    const stmt = try expectExpressionStatement(program.statements.items[0]);
    try std.testing.expect(stmt.expression != null);

    try std.testing.expectEqual(ast.NodeType.CallExpression, stmt.expression.?.node.getType());
    const ce: *ast.CallExpression = @ptrCast(stmt.expression.?);

    try expectIdentifier(ce.function, "add");
    try std.testing.expect(ce.arguments.items.len == 3);

    try expectLiteral(ce.arguments.items[0], .{ .int = 1 });
    try expectInfixExpression(ce.arguments.items[1], .{ .int = 2 }, "*", .{ .int = 3 });
    try expectInfixExpression(ce.arguments.items[2], .{ .int = 4 }, "+", .{ .int = 5 });
}

test "function parameter parsing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedParams: []const []const u8,
    }{
        .{ .input = "fn() {};", .expectedParams = &[_][]const u8{} },
        .{ .input = "fn(x) {};", .expectedParams = &[_][]const u8{"x"} },
        .{ .input = "fn(x, y, z) {};", .expectedParams = &[_][]const u8{ "x", "y", "z" } },
    };

    for (tests) |tt| {
        var program = try testParseProgram(allocator, tt.input);
        defer program.node.deinit(allocator);

        const stmt = try expectExpressionStatement(program.statements.items[0]);
        try std.testing.expect(stmt.expression != null);

        const func = try expectFunctionLiteral(stmt.expression.?, tt.expectedParams.len);

        for (tt.expectedParams, 0..) |ident, i| {
            try expectLiteral(&func.parameters.items[i].expression, .{ .ident = ident });
        }
    }
}

test "function literal parsing" {
    const input = "fn (x, y) { x + y;}";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);

    try expectStatementLength(program, 1);

    const stmt = try expectExpressionStatement(program.statements.items[0]);
    try std.testing.expect(stmt.expression != null);

    const func = try expectFunctionLiteral(stmt.expression.?, 2);

    try expectLiteral(&func.parameters.items[0].expression, .{ .ident = "x" });
    try expectLiteral(&func.parameters.items[1].expression, .{ .ident = "y" });

    try std.testing.expect(func.body.statements.items.len == 1);
    const body = try expectExpressionStatement(func.body.statements.items[0]);
    try std.testing.expect(body.expression != null);
    try expectInfixExpression(body.expression.?, .{ .ident = "x" }, "+", .{ .ident = "y" });
}

test "if else expression" {
    const input = "if (a < b) { a } else { b }";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);

    try expectStatementLength(program, 1);

    const stmt = try expectExpressionStatement(program.statements.items[0]);
    try std.testing.expect(stmt.expression != null);
    const expr = try expectIfExpression(stmt.expression.?);

    try expectInfixExpression(expr.condition, .{ .ident = "a" }, "<", .{ .ident = "b" });
    try std.testing.expect(expr.consequence.statements.items.len == 1);

    const consequence = try expectExpressionStatement(expr.consequence.statements.items[0]);
    try std.testing.expect(consequence.expression != null);

    try expectIdentifier(consequence.expression.?, "a");
    try std.testing.expect(expr.alternative != null);
    try std.testing.expect(expr.alternative.?.statements.items.len == 1);

    const alternative = try expectExpressionStatement(expr.alternative.?.statements.items[0]);
    try std.testing.expect(alternative.expression != null);
    try expectIdentifier(alternative.expression.?, "b");
}

test "if expression" {
    const input = "if (x < y) { x }";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);

    try expectStatementLength(program, 1);

    const stmt = try expectExpressionStatement(program.statements.items[0]);
    try std.testing.expect(stmt.expression != null);
    const expr = try expectIfExpression(stmt.expression.?);

    try expectInfixExpression(expr.condition, .{ .ident = "x" }, "<", .{ .ident = "y" });
    try std.testing.expect(expr.consequence.statements.items.len == 1);

    const consequence = try expectExpressionStatement(expr.consequence.statements.items[0]);
    try std.testing.expect(consequence.expression != null);

    try expectIdentifier(consequence.expression.?, "x");
    try std.testing.expect(expr.alternative == null);
}

test "operator precedence parsing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "-a * b", .expected = "((-a) * b)" },
        .{ .input = "!-a", .expected = "(!(-a))" },
        .{ .input = "a + b + c", .expected = "((a + b) + c)" },
        .{ .input = "a + b - c", .expected = "((a + b) - c)" },
        .{ .input = "a * b * c", .expected = "((a * b) * c)" },
        .{ .input = "a * b / c", .expected = "((a * b) / c)" },
        .{ .input = "a + b / c", .expected = "(a + (b / c))" },
        .{ .input = "a + b * c + d / e - f", .expected = "(((a + (b * c)) + (d / e)) - f)" },
        .{ .input = "3 + 4; -5 * 5", .expected = "(3 + 4)((-5) * 5)" },
        .{ .input = "5 > 4 == 3 < 4", .expected = "((5 > 4) == (3 < 4))" },
        .{ .input = "5 < 4 != 3 > 4", .expected = "((5 < 4) != (3 > 4))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ .input = "true", .expected = "true" },
        .{ .input = "false", .expected = "false" },
        .{ .input = "3 > 5 == false", .expected = "((3 > 5) == false)" },
        .{ .input = "3 < 5 == true", .expected = "((3 < 5) == true)" },
        .{ .input = "1 + (2 + 3) + 4", .expected = "((1 + (2 + 3)) + 4)" },
        .{ .input = "(5 + 5) * 2", .expected = "((5 + 5) * 2)" },
        .{ .input = "2 / (5 + 5)", .expected = "(2 / (5 + 5))" },
        .{ .input = "-(5 + 5)", .expected = "(-(5 + 5))" },
        .{ .input = "!(true == true)", .expected = "(!(true == true))" },
        .{ .input = "a + add(b * c) + d", .expected = "((a + add((b * c))) + d)" },
        .{ .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
        .{ .input = "add(a + b + c * d / f + g)", .expected = "add((((a + b) + ((c * d) / f)) + g))" },
    };

    for (tests) |tt| {
        var program = try testParseProgram(allocator, tt.input);
        defer program.node.deinit(allocator);

        const actual = try program.node.string(allocator);
        defer allocator.free(actual);

        try std.testing.expectEqualStrings(tt.expected, actual);
    }
}

test "parsing infix expressions" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        left_value: TestLiteral,
        operator: []const u8,
        right_value: TestLiteral,
    }{
        .{
            .input = "5 + 5;",
            .left_value = .{ .int = 5 },
            .operator = "+",
            .right_value = .{ .int = 5 },
        },
        .{
            .input = "5 - 5;",
            .left_value = .{ .int = 5 },
            .operator = "-",
            .right_value = .{ .int = 5 },
        },
        .{
            .input = "5 * 5;",
            .left_value = .{ .int = 5 },
            .operator = "*",
            .right_value = .{ .int = 5 },
        },
        .{
            .input = "5 / 5;",
            .left_value = .{ .int = 5 },
            .operator = "/",
            .right_value = .{ .int = 5 },
        },
        .{
            .input = "5 > 5;",
            .left_value = .{ .int = 5 },
            .operator = ">",
            .right_value = .{ .int = 5 },
        },
        .{
            .input = "5 < 5;",
            .left_value = .{ .int = 5 },
            .operator = "<",
            .right_value = .{ .int = 5 },
        },
        .{
            .input = "5 == 5;",
            .left_value = .{ .int = 5 },
            .operator = "==",
            .right_value = .{ .int = 5 },
        },
        .{
            .input = "5 != 5;",
            .left_value = .{ .int = 5 },
            .operator = "!=",
            .right_value = .{ .int = 5 },
        },
        .{
            .input = "true == true",
            .left_value = .{ .boolean = true },
            .operator = "==",
            .right_value = .{ .boolean = true },
        },
        .{
            .input = "true != false",
            .left_value = .{ .boolean = true },
            .operator = "!=",
            .right_value = .{ .boolean = false },
        },
        .{
            .input = "false == false",
            .left_value = .{ .boolean = false },
            .operator = "==",
            .right_value = .{ .boolean = false },
        },
    };

    for (tests) |tt| {
        var program = try testParseProgram(allocator, tt.input);
        defer program.node.deinit(allocator);
        try expectStatementLength(program, 1);

        const stmt = try expectExpressionStatement(program.statements.items[0]);
        try std.testing.expect(stmt.expression != null);
        try expectInfixExpression(stmt.expression.?, tt.left_value, tt.operator, tt.right_value);
    }
}

test "parsing prefix expressions" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        operator: []const u8,
        iv: TestLiteral,
    }{
        .{ .input = "!5;", .operator = "!", .iv = .{ .int = 5 } },
        .{ .input = "-15;", .operator = "-", .iv = .{ .int = 15 } },
        .{ .input = "!true;", .operator = "!", .iv = .{ .boolean = true } },
        .{ .input = "!false;", .operator = "!", .iv = .{ .boolean = false } },
    };

    for (tests) |tt| {
        var program = try testParseProgram(allocator, tt.input);
        defer program.node.deinit(allocator);

        try expectStatementLength(program, 1);
        const stmt = try expectExpressionStatement(program.statements.items[0]);
        try std.testing.expect(stmt.expression != null);
        try expectPrefixExpression(stmt.expression.?, tt.operator, tt.iv);
    }
}

test "boolean literal expr" {
    const input = "true";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);

    try expectStatementLength(program, 1);

    const stmt = try expectExpressionStatement(program.statements.items[0]);
    try std.testing.expect(stmt.expression != null);
    try expectBoolean(stmt.expression.?, true);
}

test "integer literal expr" {
    const input = "5;";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);

    try expectStatementLength(program, 1);

    const stmt = try expectExpressionStatement(program.statements.items[0]);
    try std.testing.expect(stmt.expression != null);
    try expectIntegerLiteral(stmt.expression.?, 5);
}

test "identifier" {
    const input = "foobar;";

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);

    try expectStatementLength(program, 1);

    const stmt = try expectExpressionStatement(program.statements.items[0]);
    try std.testing.expect(stmt.expression != null);
    try expectIdentifier(stmt.expression.?, "foobar");
}

test "return statement" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedValue: TestLiteral,
    }{
        .{ .input = "return 5;", .expectedValue = .{ .int = 5 } },
        .{ .input = "return true;", .expectedValue = .{ .boolean = true } },
        .{ .input = "return foobar;", .expectedValue = .{ .ident = "foobar" } },
    };

    for (tests) |tt| {
        var program = try testParseProgram(allocator, tt.input);
        defer program.node.deinit(allocator);
        try expectStatementLength(program, 1);

        try std.testing.expectEqual(ast.NodeType.ReturnStatement, program.statements.items[0].node.getType());
        const stmt_cast: *ast.ReturnStatement = @ptrCast(program.statements.items[0]);

        try std.testing.expectEqualStrings("return", program.statements.items[0].node.tokenLiteral());
        try expectLiteral(stmt_cast.value.?, tt.expectedValue);
    }
}

test "let statement" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expectedIdentifier: []const u8,
        expectedValue: TestLiteral,
    }{
        .{ .input = "let x = 5;", .expectedIdentifier = "x", .expectedValue = .{ .int = 5 } },
        .{ .input = "let y = true;", .expectedIdentifier = "y", .expectedValue = .{ .boolean = true } },
        .{ .input = "let foobar = y;", .expectedIdentifier = "foobar", .expectedValue = .{ .ident = "y" } },
    };

    for (tests) |tt| {
        var program = try testParseProgram(allocator, tt.input);
        defer program.node.deinit(allocator);
        try expectStatementLength(program, 1);

        try std.testing.expectEqual(ast.NodeType.LetStatement, program.statements.items[0].node.getType());
        const stmt_cast: *ast.LetStatement = @ptrCast(program.statements.items[0]);

        try std.testing.expectEqualStrings("let", program.statements.items[0].node.tokenLiteral());
        try std.testing.expectEqualStrings(tt.expectedIdentifier, stmt_cast.name.value);
        try expectLiteral(stmt_cast.value.?, tt.expectedValue);
    }
}
