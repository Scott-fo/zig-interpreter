const token = @import("token.zig");
const std = @import("std");

pub const NodeType = enum {
    Program,
    ExpressionStatement,
    ReturnStatement,
    LetStatement,
    BlockStatement,
    PrefixExpression,
    InfixExpression,
    IfExpression,
    CallExpression,
    IntegerLiteral,
    FunctionLiteral,
    Identifier,
    Boolean,
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
        errdefer buffer.deinit();

        for (self.statements.items) |stmt| {
            const stmt_string = try stmt.node.string(allocator);
            defer allocator.free(stmt_string);

            try buffer.appendSlice(stmt_string);
        }

        return buffer.toOwnedSlice();
    }
};

pub const BlockStatement = struct {
    const Self = @This();

    node: Node,
    token: token.Token,
    statements: std.ArrayList(*Statement),

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(allocator: std.mem.Allocator, tok: token.Token) !*Self {
        const bs = try allocator.create(Self);
        bs.* = .{
            .node = .{ .vtable = &vtable },
            .token = tok,
            .statements = std.ArrayList(*Statement).init(allocator),
        };

        return bs;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("node", node);

        for (self.statements.items) |stmt| {
            stmt.node.deinit(allocator);
        }

        self.statements.deinit();
        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .BlockStatement;
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
        errdefer buffer.deinit();

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

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        expression: ?*Expression,
    ) !*Self {
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

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        value: ?*Expression,
    ) !*Self {
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
        errdefer buffer.deinit();

        try buffer.appendSlice(node.tokenLiteral());
        try buffer.append(' ');
        if (self.value) |value| {
            const val_str = try value.node.string(allocator);
            defer allocator.free(val_str);

            try buffer.appendSlice(val_str);
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

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        name: *Identifier,
        value: ?*Expression,
    ) !*Self {
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
        errdefer buffer.deinit();

        try buffer.appendSlice(node.tokenLiteral());
        try buffer.append(' ');

        const name_str = try self.name.expression.node.string(allocator);
        defer allocator.free(name_str);

        try buffer.appendSlice(name_str);
        try buffer.appendSlice(" = ");
        if (self.value) |value| {
            const val_str = try value.node.string(allocator);
            defer allocator.free(val_str);

            try buffer.appendSlice(val_str);
        }

        try buffer.append(';');
        return buffer.toOwnedSlice();
    }
};

pub const FunctionLiteral = struct {
    const Self = @This();

    expression: Expression,
    token: token.Token,
    body: *BlockStatement,
    parameters: std.ArrayList(*Identifier),

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        body: *BlockStatement,
    ) !*Self {
        const fl = try allocator.create(Self);
        fl.* = .{
            .expression = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .body = body,
            .parameters = std.ArrayList(*Identifier).init(allocator),
        };

        return fl;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const expression: *Expression = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("expression", expression);

        self.body.node.deinit(allocator);

        for (self.parameters.items) |stmt| {
            stmt.expression.node.deinit(allocator);
        }

        self.parameters.deinit();
        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .FunctionLiteral;
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

        try buffer.appendSlice(self.expression.node.tokenLiteral());
        try buffer.append('(');

        for (self.parameters.items, 0..) |param, i| {
            const param_str = try param.expression.node.string(allocator);
            defer allocator.free(param_str);

            if (i != 0) {
                try buffer.append(',');
            }
            try buffer.appendSlice(param_str);
        }

        try buffer.append(')');

        const body_str = try self.body.node.string(allocator);
        defer allocator.free(body_str);

        try buffer.appendSlice(body_str);

        return buffer.toOwnedSlice();
    }
};

pub const CallExpression = struct {
    const Self = @This();

    expression: Expression,
    token: token.Token,
    function: *Expression,
    arguments: std.ArrayList(*Expression),

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        function: *Expression,
    ) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{
            .expression = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .function = function,
            .arguments = std.ArrayList(*Expression).init(allocator),
        };

        return expr;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const expression: *Expression = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("expression", expression);

        self.function.node.deinit(allocator);
        for (self.arguments.items) |arg| {
            arg.node.deinit(allocator);
        }
        self.arguments.deinit();

        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .CallExpression;
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

        const func_str = try self.function.node.string(allocator);
        defer allocator.free(func_str);
        try buffer.appendSlice(func_str);

        try buffer.append('(');

        for (self.arguments.items, 0..) |arg, i| {
            const arg_str = try arg.node.string(allocator);
            defer allocator.free(arg_str);

            if (i != 0) {
                try buffer.append(',');
                try buffer.append(' ');
            }
            try buffer.appendSlice(arg_str);
        }

        try buffer.append(')');
        return buffer.toOwnedSlice();
    }
};

pub const IfExpression = struct {
    const Self = @This();

    expression: Expression,
    token: token.Token,
    condition: *Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        condition: *Expression,
        consequence: *BlockStatement,
        alternative: ?*BlockStatement,
    ) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{
            .expression = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .condition = condition,
            .consequence = consequence,
            .alternative = alternative,
        };

        return expr;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const expression: *Expression = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("expression", expression);

        self.condition.node.deinit(allocator);
        self.consequence.node.deinit(allocator);
        if (self.alternative) |alt| {
            alt.node.deinit(allocator);
        }

        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .IfExpression;
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

        const condition = try self.condition.node.string(allocator);
        defer allocator.free(condition);

        const consequence = try self.consequence.node.string(allocator);
        defer allocator.free(consequence);

        try buffer.appendSlice("if");
        try buffer.appendSlice(condition);
        try buffer.append(' ');
        try buffer.appendSlice(consequence);

        if (self.alternative) |alt| {
            const alt_str = try alt.node.string(allocator);
            defer allocator.free(alt_str);

            try buffer.appendSlice("else");
            try buffer.appendSlice(alt_str);
        }

        return buffer.toOwnedSlice();
    }
};

pub const InfixExpression = struct {
    const Self = @This();

    expression: Expression,
    token: token.Token,
    left: *Expression,
    right: *Expression,
    operator: []const u8,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        operator: []const u8,
        left: *Expression,
        right: *Expression,
    ) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{
            .expression = .{ .node = .{ .vtable = &vtable } },
            .token = tok,
            .operator = operator,
            .left = left,
            .right = right,
        };

        return expr;
    }

    pub fn deinit(node: *Node, allocator: std.mem.Allocator) void {
        const expression: *Expression = @fieldParentPtr("node", node);
        const self: *Self = @fieldParentPtr("expression", expression);

        self.right.node.deinit(allocator);
        self.left.node.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn getType(_: *const Node) NodeType {
        return .InfixExpression;
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

        const left_str = try self.left.node.string(allocator);
        defer allocator.free(left_str);

        const right_str = try self.right.node.string(allocator);
        defer allocator.free(right_str);

        try buffer.append('(');
        try buffer.appendSlice(left_str);
        try buffer.append(' ');
        try buffer.appendSlice(self.operator);
        try buffer.append(' ');
        try buffer.appendSlice(right_str);
        try buffer.append(')');

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

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        operator: []const u8,
        right: *Expression,
    ) !*Self {
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

pub const Boolean = struct {
    const Self = @This();

    expression: Expression,
    token: token.Token,
    value: bool,

    const vtable = Node.VTable{
        .deinitFn = deinit,
        .tokenLiteralFn = tokenLiteral,
        .stringFn = string,
        .getTypeFn = getType,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        value: bool,
    ) !*Self {
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
        return .Boolean;
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

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        value: ?i64,
    ) !*Self {
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

    pub fn init(
        allocator: std.mem.Allocator,
        tok: token.Token,
        value: []const u8,
    ) !*Self {
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
