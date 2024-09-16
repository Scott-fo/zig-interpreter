const std = @import("std");

const object = @import("object.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

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

test "eval integer expression" {
    const allocator = std.testing.allocator;
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);
        defer ev.?.deinit(allocator);

        try testIntegerObject(ev.?, tt.expected);
    }
}
