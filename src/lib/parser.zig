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
        else => null,
    };
}

fn getInfixFn(t: std.meta.Tag(Token)) ?InfixParseFn {
    return switch (t) {
        .PLUS, .MINUS, .SLASH, .ASTERISK, .EQ, .NOT_EQ, .LT, .GT => parseInfixExpression,
        else => null,
    };
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

const Parser = struct {
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

        const exp = try self.parseExpression(.LOWEST);
        if (exp == null) {
            stmt.statement.node.deinit(self.allocator);
            return null;
        }

        stmt.expression = exp;

        while (!self.currTokenIs(.SEMICOLON) and !self.currTokenIs(.EOF)) {
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

fn printError(err: Error) void {
    std.debug.print("Parser error: {s}", .{@errorName(err.err)});
    if (err.msg != null) {
        std.debug.print("{s}", .{err.msg.?});
    } else if (err.expected != null and err.got != null) {
        std.debug.print(" - expected next token to be {s}, got {s} instead", .{
            @tagName(err.expected.?),
            @tagName(err.got.?),
        });
    } else if (err.got != null) {
        std.debug.print(" - got {s}\n", .{@tagName(err.got.?)});
    }
}

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

fn expectInfixExpression(expr: *ast.Expression, left: i64, op: []const u8, right: i64) !void {
    try std.testing.expectEqual(ast.NodeType.InfixExpression, expr.node.getType());
    const ie: *ast.InfixExpression = @ptrCast(expr);

    try std.testing.expectEqualStrings(op, ie.operator);
    try expectIntegerLiteral(ie.left, left);
    try expectIntegerLiteral(ie.right, right);
}

fn expectPrefixExpression(expr: *ast.Expression, op: []const u8, right: i64) !void {
    try std.testing.expectEqual(ast.NodeType.PrefixExpression, expr.node.getType());
    const pe: *ast.PrefixExpression = @ptrCast(expr);

    try std.testing.expectEqualStrings(op, pe.operator);
    try expectIntegerLiteral(pe.right, right);
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
        left_value: i64,
        operator: []const u8,
        right_value: i64,
    }{
        .{ .input = "5 + 5;", .left_value = 5, .operator = "+", .right_value = 5 },
        .{ .input = "5 - 5;", .left_value = 5, .operator = "-", .right_value = 5 },
        .{ .input = "5 * 5;", .left_value = 5, .operator = "*", .right_value = 5 },
        .{ .input = "5 / 5;", .left_value = 5, .operator = "/", .right_value = 5 },
        .{ .input = "5 > 5;", .left_value = 5, .operator = ">", .right_value = 5 },
        .{ .input = "5 < 5;", .left_value = 5, .operator = "<", .right_value = 5 },
        .{ .input = "5 == 5;", .left_value = 5, .operator = "==", .right_value = 5 },
        .{ .input = "5 != 5;", .left_value = 5, .operator = "!=", .right_value = 5 },
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
        iv: i64,
    }{
        .{ .input = "!5;", .operator = "!", .iv = 5 },
        .{ .input = "-15;", .operator = "-", .iv = 15 },
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
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);

    try expectStatementLength(program, 3);

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

    var program = try testParseProgram(allocator, input);
    defer program.node.deinit(allocator);
    try expectStatementLength(program, 3);

    const expected_identifiers = [_][]const u8{ "x", "y", "foobar" };

    for (program.statements.items, 0..) |stmt, i| {
        try std.testing.expectEqual(ast.NodeType.LetStatement, stmt.node.getType());
        const stmt_cast: *ast.LetStatement = @ptrCast(stmt);

        try std.testing.expectEqualStrings("let", stmt.node.tokenLiteral());
        try std.testing.expectEqualStrings(expected_identifiers[i], stmt_cast.name.value);
        try std.testing.expectEqualStrings(expected_identifiers[i], stmt_cast.name.expression.node.tokenLiteral());
    }
}
