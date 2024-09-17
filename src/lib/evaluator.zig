const std = @import("std");

const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

fn evalIntegerInfixExpression(allocator: std.mem.Allocator, operator: []const u8, left: *object.Object, right: *object.Object) !*object.Object {
    const l: *object.Integer = @fieldParentPtr("object", left);
    const r: *object.Integer = @fieldParentPtr("object", right);

    const lv = l.value;
    const rv = r.value;

    if (operator.len == 1) {
        return switch (operator[0]) {
            '+' => {
                const new = try object.Integer.init(allocator, lv + rv);
                return &new.object;
            },
            '-' => {
                const new = try object.Integer.init(allocator, lv - rv);
                return &new.object;
            },
            '*' => {
                const new = try object.Integer.init(allocator, lv * rv);
                return &new.object;
            },
            '/' => {
                const new = try object.Integer.init(allocator, @divFloor(lv, rv));
                return &new.object;
            },
            '<' => &object.Boolean.get(lv < rv).object,
            '>' => &object.Boolean.get(lv > rv).object,
            else => &object.Null.get().object,
        };
    }

    if (std.mem.eql(u8, operator, "==")) {
        return &object.Boolean.get(lv == rv).object;
    }

    if (std.mem.eql(u8, operator, "!=")) {
        return &object.Boolean.get(lv != rv).object;
    }

    return &object.Null.get().object;
}

fn evalInfixExpression(allocator: std.mem.Allocator, operator: []const u8, left: *object.Object, right: *object.Object) !*object.Object {
    if (left.objectType() == object.ObjectType.IntegerObj and right.objectType() == object.ObjectType.IntegerObj) {
        return evalIntegerInfixExpression(allocator, operator, left, right);
    }

    if (std.mem.eql(u8, operator, "==")) {
        return &object.Boolean.get(std.meta.eql(left, right)).object;
    }

    if (std.mem.eql(u8, operator, "!=")) {
        return &object.Boolean.get(!std.meta.eql(left, right)).object;
    }

    return &object.Null.get().object;
}

fn evalMinusPrefixOperatorExpression(allocator: std.mem.Allocator, right: *object.Object) !*object.Object {
    if (right.objectType() != object.ObjectType.IntegerObj) {
        return &object.Null.get().object; // Update these gets to return a reference to the object
    }

    const i: *object.Integer = @fieldParentPtr("object", right);
    const oi = try object.Integer.init(allocator, -i.value);
    return &oi.object;
}

fn evalBangOperatorExpression(right: *object.Object) *object.Object {
    return switch (right.objectType()) {
        .BooleanObj => {
            const boo: *object.Boolean = @fieldParentPtr("object", right);
            return switch (boo.value) {
                true => &object.Boolean.get(false).object,
                false => &object.Boolean.get(true).object,
            };
        },
        .NullObj => &object.Boolean.get(true).object,
        else => &object.Boolean.get(false).object,
    };
}

fn evalPrefixExpression(allocator: std.mem.Allocator, operator: u8, right: *object.Object) !*object.Object {
    return switch (operator) {
        '!' => evalBangOperatorExpression(right),
        '-' => evalMinusPrefixOperatorExpression(allocator, right),
        else => &object.Null.get().object,
    };
}

fn evalStatements(stmts: std.ArrayList(*ast.Statement), allocator: std.mem.Allocator) anyerror!?*object.Object {
    var result: ?*object.Object = null;
    for (stmts.items) |stmt| {
        result = try eval(&stmt.node, allocator);
        if (result == null) {
            return null;
        }
    }
    return result;
}

pub fn eval(node: *const ast.Node, allocator: std.mem.Allocator) !?*object.Object {
    return switch (node.getType()) {
        .Program => {
            const p: *const ast.Program = @fieldParentPtr("node", node);
            return evalStatements(p.statements, allocator);
        },
        .ExpressionStatement => {
            const statement: *const ast.Statement = @fieldParentPtr("node", node);
            const es: *const ast.ExpressionStatement = @fieldParentPtr("statement", statement);
            if (es.expression == null) {
                return null;
            }

            return eval(&es.expression.?.node, allocator);
        },
        .IntegerLiteral => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const il: *const ast.IntegerLiteral = @fieldParentPtr("expression", expression);
            if (il.value == null) {
                return null;
            }

            const i = try object.Integer.init(allocator, il.value.?);
            errdefer i.object.deinit(allocator);

            return &i.object;
        },
        .Boolean => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const be: *const ast.Boolean = @fieldParentPtr("expression", expression);
            return &object.Boolean.get(be.value).object;
        },
        .PrefixExpression => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const pe: *const ast.PrefixExpression = @fieldParentPtr("expression", expression);

            const right = try eval(&pe.right.node, allocator);
            if (right == null) {
                return null;
            }
            defer right.?.deinit(allocator);
            std.debug.assert(pe.operator.len == 1);
            return evalPrefixExpression(allocator, pe.operator[0], right.?);
        },
        .InfixExpression => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const ie: *const ast.InfixExpression = @fieldParentPtr("expression", expression);
            const left = try eval(&ie.left.node, allocator);
            if (left == null) {
                return null;
            }
            defer left.?.deinit(allocator);

            const right = try eval(&ie.right.node, allocator);
            if (right == null) {
                return null;
            }
            defer right.?.deinit(allocator);

            return evalInfixExpression(allocator, ie.operator, left.?, right.?);
        },
        else => null,
    };
}

fn testEval(allocator: std.mem.Allocator, input: []const u8) !?*object.Object {
    var l = lexer.Lexer.init(input);
    var p = try parser.Parser.init(allocator, &l);
    errdefer p.deinit();

    var program = try p.parseProgram();
    defer program.node.deinit(allocator);

    const r = try eval(&program.node, allocator);
    if (r == null) {
        return null;
    }
    errdefer r.?.deinit(allocator);

    return r.?;
}

fn testIntegerObject(obj: *object.Object, expected: i64) !void {
    try std.testing.expectEqual(object.ObjectType.IntegerObj, obj.objectType());
    const i: *const object.Integer = @ptrCast(obj);
    try std.testing.expectEqual(expected, i.value);
}

fn testBooleanObject(obj: *object.Object, expected: bool) !void {
    try std.testing.expectEqual(object.ObjectType.BooleanObj, obj.objectType());
    const b: *const object.Boolean = @ptrCast(obj);
    try std.testing.expectEqual(expected, b.value);
}

test "bang operator" {
    const allocator = std.testing.allocator;
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
        .{ .input = "!!5", .expected = true },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);
        defer ev.?.deinit(allocator);

        try testBooleanObject(ev.?, tt.expected);
    }
}

test "invalid negation" {
    const allocator = std.testing.allocator;
    const tests = [_]struct {
        input: []const u8,
        expected: ?i64,
    }{
        .{ .input = "-true", .expected = null },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);
        defer ev.?.deinit(allocator);

        try std.testing.expectEqual(object.ObjectType.NullObj, ev.?.objectType());
    }
}

test "eval integer expression" {
    const allocator = std.testing.allocator;
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
        .{ .input = "-5", .expected = -5 },
        .{ .input = "-10", .expected = -10 },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = 10 },
        .{ .input = "2 * 2 *2 * 2 * 2", .expected = 32 },
        .{ .input = "-50 + 100 + -50", .expected = 0 },
        .{ .input = "5 * 2 + 10", .expected = 20 },
        .{ .input = "5 + 2 * 10", .expected = 25 },
        .{ .input = "20 + 2 * -10", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        .{ .input = "2 * (5 + 10)", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);
        defer ev.?.deinit(allocator);

        try testIntegerObject(ev.?, tt.expected);
    }
}

test "eval boolean expression" {
    const allocator = std.testing.allocator;
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 < 1", .expected = false },
        .{ .input = "1 > 1", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "1 == 2", .expected = false },
        .{ .input = "1 != 2", .expected = true },
        .{ .input = "true == true", .expected = true },
        .{ .input = "false == false", .expected = true },
        .{ .input = "true == false", .expected = false },
        .{ .input = "true != false", .expected = true },
        .{ .input = "false != true", .expected = true },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 < 2) == false", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 > 2) == false", .expected = true },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);
        defer ev.?.deinit(allocator);

        try testBooleanObject(ev.?, tt.expected);
    }
}
