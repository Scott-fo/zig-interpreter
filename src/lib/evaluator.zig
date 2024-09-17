const std = @import("std");

const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

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
            std.debug.assert(pe.operator.len == 1); // Move the whole type to just be a u8 rather than string
            return evalPrefixExpression(allocator, pe.operator[0], right.?);
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
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);
        defer ev.?.deinit(allocator);

        try testBooleanObject(ev.?, tt.expected);
    }
}
