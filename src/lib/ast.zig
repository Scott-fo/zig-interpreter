const token = @import("token.zig");
const std = @import("std");

pub const NodeType = enum {
    Program,
    ExpressionStatement,
    ReturnStatement,
    LetStatement,
    PrefixExpression,
    IntegerLiteral,
    Identifier,
};

pub const Node = struct {
    vtable: *const VTable,

    pub const VTable = struct {
        deinitFn: *const fn (self: *Node, allocator: std.mem.Allocator) void,
        tokenLiteralFn: *const fn (self: *const Node) []const u8,
        stringFn: *const fn (self: *const Node, allocator: std.mem.Allocator) anyerror![]const u8,
        getTypeFn: *const fn (self: *const Node) NodeType,
    };

    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        self.vtable.deinitFn(self, allocator);
    }

    pub fn tokenLiteral(self: *const Node) []const u8 {
        return self.vtable.tokenLiteralFn(self);
    }

    pub fn string(self: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        return self.vtable.stringFn(self, allocator);
    }

    pub fn getType(self: *const Node) NodeType {
        return self.vtable.getTypeFn(self);
    }
};

pub const Statement = struct {
    node: Node,
};

pub const Expression = struct {
    node: Node,
};

pub const Program = struct {
    const Self = @This();

    node: Node,
    statements: std.ArrayList(*Statement),

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(allocator: std.mem.Allocator) *Self {
        const program = allocator.create(Self) catch unreachable;
        program.* = .{
            .node = .{ .vtable = &vtable },
            .statements = std.ArrayList(*Statement).init(allocator),
        };

        return program;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("node", node);

        for (self.statements.items) |stmt| {
            stmt.node.deinit(allocator);
            allocator.destroy(stmt);
        }

        self.statements.deinit();
        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .Program;
    }

    pub fn tokenLiteral(node: *const Node) []const u8 {
        const self: *const Self = @fieldParentPtr("node", node);

        if (self.statements.items.len > 0) {
            return self.statements.items[0].node.tokenLiteral();
        }

        return "";
    }

    pub fn string(node: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("node", node);

        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        for (self.statements.items) |stmt| {
            const stmt_string = try stmt.node.string(allocator);
            defer allocator.free(stmt_string);

            try buffer.appendSlice(stmt_string);
        }

        return buffer.toOwnedSlice();
    }
};

pub const ExpressionStatement = struct {
    const Self = @This();

    statement: Statement,
    token: token.Token,
    expression: ?*Expression,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(allocator: std.mem.Allocator, tok: token.Token, expression: ?*Expression) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{
            .statement = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .expression = expression,
        };

        return stmt;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const statement: *Statement = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("statement", statement);

        if (self.expression) |expression| {
            expression.node.deinit(allocator);
        }

        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .ExpressionStatement;
    }

    pub fn tokenLiteral(node: *const Node) []const u8 {
        const statement: *const Statement = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("statement", statement);
        return self.token.toLiteral();
    }

    pub fn string(node: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        const statement: *const Statement = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("statement", statement);

        if (self.expression) |expr| {
            return expr.node.string(allocator);
        }

        return "";
    }
};

pub const ReturnStatement = struct {
    const Self = @This();

    statement: Statement,
    token: token.Token,
    value: ?*Expression,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(allocator: std.mem.Allocator, tok: token.Token, value: ?*Expression) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{
            .statement = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .value = value,
        };

        return stmt;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const statement: *Statement = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("statement", statement);

        if (self.value) |value| {
            value.node.deinit(allocator);
        }

        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .ReturnStatement;
    }

    pub fn tokenLiteral(node: *const Node) []const u8 {
        const statement: *const Statement = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("statement", statement);
        return self.token.toLiteral();
    }

    pub fn string(node: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        const statement: *const Statement = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("statement", statement);

        var buffer = std.ArrayList(u8).init(allocator);

        try buffer.appendSlice(node.tokenLiteral());
        try buffer.append(' ');
        if (self.value) |value| {
            try buffer.appendSlice(try value.node.string(allocator));
        }
        try buffer.append(';');

        return buffer.toOwnedSlice();
    }
};

pub const LetStatement = struct {
    const Self = @This();

    statement: Statement,
    token: token.Token,
    name: *Identifier,
    value: ?*Expression,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(allocator: std.mem.Allocator, tok: token.Token, name: *Identifier, value: ?*Expression) !*Self {
        const let_stmt = try allocator.create(Self);
        let_stmt.* = .{
            .statement = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .name = name,
            .value = value,
        };

        return let_stmt;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const statement: *Statement = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("statement", statement);

        self.name.expression.node.deinit(allocator);
        if (self.value) |value| {
            value.node.deinit(allocator);
        }

        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .LetStatement;
    }

    pub fn tokenLiteral(node: *const Node) []const u8 {
        const statement: *const Statement = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("statement", statement);
        return self.token.toLiteral();
    }

    pub fn string(node: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        const statement: *const Statement = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("statement", statement);

        var buffer = std.ArrayList(u8).init(allocator);

        try buffer.appendSlice(node.tokenLiteral());
        try buffer.append(' ');
        try buffer.appendSlice(try self.name.expression.node.string(allocator));
        try buffer.appendSlice(" = ");
        if (self.value) |value| {
            try buffer.appendSlice(try value.node.string(allocator));
        }
        try buffer.append(';');

        return buffer.toOwnedSlice();
    }
};

pub const PrefixExpression = struct {
    const Self = @This();

    expression: Expression,
    token: token.Token,
    operator: []const u8,
    right: *Expression,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(allocator: std.mem.Allocator, tok: token.Token, operator: []const u8, right: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{
            .expression = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .operator = operator,
            .right = right,
        };

        return expr;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const expression: *Expression = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("expression", expression);

        self.right.node.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .PrefixExpression;
    }

    pub fn tokenLiteral(node: *const Node) []const u8 {
        const expression: *const Expression = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("expression", expression);
        return self.token.toLiteral();
    }

    pub fn string(node: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        const expression: *const Expression = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("expression", expression);

        var buffer = std.ArrayList(u8).init(allocator);
        errdefer buffer.deinit();

        try buffer.append('(');
        try buffer.appendSlice(self.operator);
        const right_str = try self.right.node.string(allocator);
        defer allocator.free(right_str);

        try buffer.appendSlice(right_str);
        try buffer.append(')');

        return buffer.toOwnedSlice();
    }
};

pub const IntegerLiteral = struct {
    const Self = @This();

    expression: Expression,
    token: token.Token,
    value: ?i64,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(allocator: std.mem.Allocator, tok: token.Token, value: ?i64) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{
            .expression = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .value = value,
        };

        return expr;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const expression: *Expression = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("expression", expression);
        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .IntegerLiteral;
    }

    pub fn tokenLiteral(node: *const Node) []const u8 {
        const expression: *const Expression = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("expression", expression);
        return self.token.toLiteral();
    }

    pub fn string(node: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        return allocator.dupe(u8, node.tokenLiteral());
    }
};

pub const Identifier = struct {
    const Self = @This();

    expression: Expression,
    token: token.Token,
    value: []const u8,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(allocator: std.mem.Allocator, tok: token.Token, value: []const u8) !*Self {
        const identifier = try allocator.create(Identifier);
        identifier.* = .{
            .expression = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .value = value,
        };

        return identifier;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const expression: *Expression = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("expression", expression);
        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .Identifier;
    }

    pub fn tokenLiteral(node: *const Node) []const u8 {
        const expression: *const Expression = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("expression", expression);
        return self.token.toLiteral();
    }

    pub fn string(node: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        const expression: *const Expression = @fieldParentPtr("node", node);
        const self: *const Self = @fieldParentPtr("expression", expression);
        return allocator.dupe(u8, self.value);
    }
};

test "test string representation of AST" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = Program.init(allocator);
    defer program.node.deinit(allocator);

    const name = try Identifier.init(allocator, .{ .IDENT = "myVar" }, "myVar");
    const value = try Identifier.init(allocator, .{ .IDENT = "anotherVar" }, "anotherVar");

    const let_statement = try LetStatement.init(allocator, .LET, name, &value.expression);

    try program.statements.append(&let_statement.statement);

    const program_string = try program.node.string(allocator);
    defer allocator.free(program_string);

    try std.testing.expectEqualStrings("let myVar = anotherVar;", program_string);
}
