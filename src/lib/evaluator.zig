const std = @import("std");

const object = @import("object.zig");
const environment = @import("environment.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

fn newError(
    arena: std.mem.Allocator,
    comptime format: []const u8,
    args: anytype,
) !*object.Object {
    const message = try std.fmt.allocPrint(arena, format, args);
    const e = try object.Error.init(arena, message);
    return &e.object;
}

fn isError(obj: ?*object.Object) bool {
    if (obj != null) {
        return obj.?.objectType() == object.ObjectType.Error;
    }

    return false;
}

fn isTruthy(obj: *object.Object) bool {
    return switch (obj.objectType()) {
        .Null => false,
        .Boolean => std.meta.eql(object.Boolean.get(true), obj),
        else => true,
    };
}

fn evalIfExpression(
    arena: std.mem.Allocator,
    env: *environment.Environment,
    ie: *const ast.IfExpression,
) !?*object.Object {
    const condition = try eval(arena, &ie.condition.node, env);
    if (isError(condition)) {
        return condition;
    }

    if (isTruthy(condition.?)) {
        return eval(arena, &ie.consequence.node, env);
    } else if (ie.alternative != null) {
        return eval(arena, &ie.alternative.?.node, env);
    } else {
        return object.Null.get();
    }
}

fn evalIntegerInfixExpression(
    arena: std.mem.Allocator,
    operator: []const u8,
    left: *object.Object,
    right: *object.Object,
) !*object.Object {
    const l: *object.Integer = @fieldParentPtr("object", left);
    const r: *object.Integer = @fieldParentPtr("object", right);

    const lv = l.value;
    const rv = r.value;

    if (operator.len == 1) {
        return switch (operator[0]) {
            '+' => {
                const new = try object.Integer.init(arena, lv + rv);
                return &new.object;
            },
            '-' => {
                const new = try object.Integer.init(arena, lv - rv);
                return &new.object;
            },
            '*' => {
                const new = try object.Integer.init(arena, lv * rv);
                return &new.object;
            },
            '/' => {
                const new = try object.Integer.init(arena, @divFloor(lv, rv));
                return &new.object;
            },
            '<' => object.Boolean.get(lv < rv),
            '>' => object.Boolean.get(lv > rv),
            else => newError(
                arena,
                "unknown operator: {s} {c} {s}",
                .{ left.objectType().toString(), operator, right.objectType().toString() },
            ),
        };
    }

    if (std.mem.eql(u8, operator, "==")) {
        return object.Boolean.get(lv == rv);
    }

    if (std.mem.eql(u8, operator, "!=")) {
        return object.Boolean.get(lv != rv);
    }

    return newError(
        arena,
        "unknown operator: {s} {s} {s}",
        .{ left.objectType().toString(), operator, right.objectType().toString() },
    );
}

fn evalInfixExpression(
    arena: std.mem.Allocator,
    operator: []const u8,
    left: *object.Object,
    right: *object.Object,
) !*object.Object {
    if (left.objectType() != right.objectType()) {
        return newError(
            arena,
            "type mismatch: {s} {s} {s}",
            .{ left.objectType().toString(), operator, right.objectType().toString() },
        );
    }

    if (left.objectType() == object.ObjectType.Integer and
        right.objectType() == object.ObjectType.Integer)
    {
        return evalIntegerInfixExpression(arena, operator, left, right);
    }

    if (std.mem.eql(u8, operator, "==")) {
        return object.Boolean.get(std.meta.eql(left, right));
    }

    if (std.mem.eql(u8, operator, "!=")) {
        return object.Boolean.get(!std.meta.eql(left, right));
    }

    return newError(
        arena,
        "unknown operator: {s} {s} {s}",
        .{ left.objectType().toString(), operator, right.objectType().toString() },
    );
}

fn evalMinusPrefixOperatorExpression(
    arena: std.mem.Allocator,
    right: *object.Object,
) !*object.Object {
    if (right.objectType() != object.ObjectType.Integer) {
        return newError(arena, "unknown operator: -{s}", .{right.objectType().toString()});
    }

    const i: *object.Integer = @fieldParentPtr("object", right);
    const oi = try object.Integer.init(arena, -i.value);
    return &oi.object;
}

fn evalBangOperatorExpression(right: *object.Object) *object.Object {
    return switch (right.objectType()) {
        .Boolean => {
            const boo: *object.Boolean = @fieldParentPtr("object", right);
            return switch (boo.value) {
                true => object.Boolean.get(false),
                false => object.Boolean.get(true),
            };
        },
        .Null => object.Boolean.get(true),
        else => object.Boolean.get(false),
    };
}

fn evalPrefixExpression(
    allocator: std.mem.Allocator,
    operator: u8,
    right: *object.Object,
) !*object.Object {
    return switch (operator) {
        '!' => evalBangOperatorExpression(right),
        '-' => evalMinusPrefixOperatorExpression(allocator, right),
        else => newError(allocator, "unknown operator: {c}{s}", .{
            operator,
            right.objectType().toString(),
        }),
    };
}

fn evalProgram(
    arena: std.mem.Allocator,
    env: *environment.Environment,
    stmts: std.ArrayList(*ast.Statement),
) anyerror!?*object.Object {
    var result: ?*object.Object = null;

    for (stmts.items) |stmt| {
        result = try eval(arena, &stmt.node, env);
        // Check here for error from result

        if (result != null) {
            if (result.?.objectType() == object.ObjectType.ReturnValue) {
                const rv: *object.ReturnValue = @fieldParentPtr("object", result.?);
                return rv.value;
            }

            if (result.?.objectType() == object.ObjectType.Error) {
                return result.?;
            }
        }
    }

    return result;
}

fn evalBlockStatement(
    arena: std.mem.Allocator,
    env: *environment.Environment,
    stmts: std.ArrayList(*ast.Statement),
) !?*object.Object {
    var result: ?*object.Object = null;

    for (stmts.items) |stmt| {
        result = try eval(arena, &stmt.node, env);
        if (result != null) {
            const rt = result.?.objectType();
            if (rt == object.ObjectType.Error) {
                const rv: *object.Error = @fieldParentPtr("object", result.?);
                return &rv.object;
            }

            if (rt == object.ObjectType.ReturnValue) {
                const rv: *object.ReturnValue = @fieldParentPtr("object", result.?);
                return &rv.object;
            }
        }
    }

    return result;
}

fn evalIdentifier(
    arena: std.mem.Allocator,
    env: *environment.Environment,
    ident: *const ast.Identifier,
) !*object.Object {
    const val = env.get(ident.value);
    if (val == null) {
        return newError(arena, "identifier not found: {s}", .{ident.value});
    }

    return val.?;
}

pub fn eval(
    arena: std.mem.Allocator,
    node: *const ast.Node,
    env: *environment.Environment,
) !?*object.Object {
    return switch (node.getType()) {
        .Program => {
            const p: *const ast.Program = @fieldParentPtr("node", node);
            return evalProgram(arena, env, p.statements);
        },
        .ExpressionStatement => {
            const statement: *const ast.Statement = @fieldParentPtr("node", node);
            const es: *const ast.ExpressionStatement = @fieldParentPtr("statement", statement);
            if (es.expression == null) {
                return null;
            }

            return eval(arena, &es.expression.?.node, env);
        },
        .IntegerLiteral => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const il: *const ast.IntegerLiteral = @fieldParentPtr("expression", expression);
            if (il.value == null) {
                return null;
            }

            const i = try object.Integer.init(arena, il.value.?);
            return &i.object;
        },
        .Boolean => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const be: *const ast.Boolean = @fieldParentPtr("expression", expression);
            return object.Boolean.get(be.value);
        },
        .PrefixExpression => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const pe: *const ast.PrefixExpression = @fieldParentPtr("expression", expression);

            const right = try eval(arena, &pe.right.node, env);
            if (isError(right)) {
                return right;
            }

            std.debug.assert(pe.operator.len == 1);
            return evalPrefixExpression(arena, pe.operator[0], right.?);
        },
        .InfixExpression => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const ie: *const ast.InfixExpression = @fieldParentPtr("expression", expression);

            const left = try eval(arena, &ie.left.node, env);
            if (isError(left)) {
                return left;
            }

            const right = try eval(arena, &ie.right.node, env);
            if (isError(right)) {
                return right;
            }

            return evalInfixExpression(arena, ie.operator, left.?, right.?);
        },
        .BlockStatement => {
            const bs: *const ast.BlockStatement = @fieldParentPtr("node", node);
            return evalBlockStatement(arena, env, bs.statements);
        },
        .IfExpression => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const ie: *const ast.IfExpression = @fieldParentPtr("expression", expression);
            return evalIfExpression(arena, env, ie);
        },
        .ReturnStatement => {
            const statement: *const ast.Statement = @fieldParentPtr("node", node);
            const rs: *const ast.ReturnStatement = @fieldParentPtr("statement", statement);
            if (rs.value == null) {
                return null;
            }

            const val = try eval(arena, &rs.value.?.node, env);
            if (isError(val)) {
                return val;
            }

            const rv = try object.ReturnValue.init(arena, val.?);
            return &rv.object;
        },
        .LetStatement => {
            const statement: *const ast.Statement = @fieldParentPtr("node", node);
            const ls: *const ast.LetStatement = @fieldParentPtr("statement", statement);
            if (ls.value == null) {
                return null;
            }

            const val = try eval(arena, &ls.value.?.node, env);
            if (isError(val)) {
                return val;
            }

            _ = try env.set(ls.name.value, val.?);
            return null;
        },
        .Identifier => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const id: *const ast.Identifier = @fieldParentPtr("expression", expression);
            return evalIdentifier(arena, env, id);
        },
        .FunctionLiteral => {
            const expression: *const ast.Expression = @fieldParentPtr("node", node);
            const f: *const ast.FunctionLiteral = @fieldParentPtr("expression", expression);
            const params = f.parameters;
            const body = f.body;

            const func = try object.Function.init(arena, env, body);
            func.parameters = params;

            return &func.object;
        },
        else => null,
    };
}

fn testEval(
    allocator: std.mem.Allocator,
    input: []const u8,
) !?*object.Object {
    var l = lexer.Lexer.init(input);
    var p = try parser.Parser.init(allocator, &l);
    var program = try p.parseProgram();
    const env = try environment.Environment.init(allocator);

    const r = try eval(allocator, &program.node, env);
    if (r == null) {
        return null;
    }

    return r.?;
}

fn testIntegerObject(obj: *object.Object, expected: i64) !void {
    try std.testing.expectEqual(object.ObjectType.Integer, obj.objectType());
    const i: *const object.Integer = @ptrCast(obj);
    try std.testing.expectEqual(expected, i.value);
}

fn testBooleanObject(obj: *object.Object, expected: bool) !void {
    try std.testing.expectEqual(object.ObjectType.Boolean, obj.objectType());
    const b: *const object.Boolean = @ptrCast(obj);
    try std.testing.expectEqual(expected, b.value);
}

fn testErrorObject(obj: *object.Object, expected: []const u8) !void {
    try std.testing.expectEqual(object.ObjectType.Error, obj.objectType());
    const e: *const object.Error = @ptrCast(obj);
    try std.testing.expectEqualStrings(expected, e.message);
}

test "function object" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = "fn(x) {x + 2}";
    const ev = try testEval(allocator, input);
    try std.testing.expect(ev != null);

    try std.testing.expectEqual(object.ObjectType.Function, ev.?.objectType());
    const f: *const object.Function = @ptrCast(ev.?);
    try std.testing.expectEqual(f.parameters.items.len, 1);
    try std.testing.expectEqualStrings("x", try f.parameters.items[0].expression.node.string(allocator));
    try std.testing.expectEqualStrings("(x + 2)", try f.body.node.string(allocator));
}

test "let statements" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5; let b = 6; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);
        try testIntegerObject(ev.?, tt.expected);
    }
}

test "error handling" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "5 + true", .expected = "type mismatch: INTEGER + BOOLEAN" },
        .{ .input = "5 + true; 5;", .expected = "type mismatch: INTEGER + BOOLEAN" },
        .{ .input = "-true", .expected = "unknown operator: -BOOLEAN" },
        .{ .input = "true + false;", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "5; true + false; 5", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "if (10 > 1) { true + false; }", .expected = "unknown operator: BOOLEAN + BOOLEAN" },
        .{ .input = "foobar", .expected = "identifier not found: foobar" },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);

        try testErrorObject(ev.?, tt.expected);
    }
}

test "return statements" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10;9;", .expected = 10 },
        .{ .input = "return 2 * 5;9;", .expected = 10 },
        .{ .input = "9;return 2 * 5;9;", .expected = 10 },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);

        try testIntegerObject(ev.?, tt.expected);
    }
}

test "if else expressions" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expected: parser.TestLiteral,
        empty: bool,
    }{
        .{ .input = "if (true) { 10 }", .expected = .{ .int = 10 }, .empty = false },
        .{ .input = "if (false) { 10 }", .expected = .{ .empty = {} }, .empty = true },
        .{ .input = "if (1) { 10 }", .expected = .{ .int = 10 }, .empty = false },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .int = 10 }, .empty = false },
        .{ .input = "if (1 > 2) { 10 }", .expected = .{ .empty = {} }, .empty = true },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .int = 20 }, .empty = false },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .int = 10 }, .empty = false },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);

        if (tt.empty) {
            try std.testing.expectEqual(object.ObjectType.Null, ev.?.objectType());
            const n: *object.Null = @ptrCast(ev.?);
            try std.testing.expect(std.meta.eql(object.Null.get(), &n.object));
        } else {
            try testIntegerObject(ev.?, tt.expected.int);
        }
    }
}

test "bang operator" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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

        try testBooleanObject(ev.?, tt.expected);
    }
}

test "invalid negation" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tests = [_]struct {
        input: []const u8,
        expected: ?i64,
    }{
        .{ .input = "-true", .expected = null },
    };

    for (tests) |tt| {
        const ev = try testEval(allocator, tt.input);
        try std.testing.expect(ev != null);

        try std.testing.expectEqual(object.ObjectType.Error, ev.?.objectType());
    }
}

test "eval integer expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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

        try testIntegerObject(ev.?, tt.expected);
    }
}

test "eval boolean expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

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

        try testBooleanObject(ev.?, tt.expected);
    }
}
