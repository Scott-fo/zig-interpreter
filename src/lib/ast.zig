const token = @import("token.zig");
const std = @import("std");

const Node = struct {
    tokenLiteralFn: *const fn (self: *Node) []const u8,

    pub fn token_literal(self: *Node) []const u8 {
        return self.tokenLiteralFn(self);
    }
};

pub const Statement = struct {
    node: Node,
    statementNodeFn: *const fn (self: *Statement) void,

    pub fn statement_node(self: *Statement) void {
        return self.statementNodeFn(self);
    }
};

pub const Expression = struct {
    node: Node,
    expressionNodeFn: *const fn (self: *Expression) void,

    pub fn expression_node(self: *Expression) void {
        return self.expressionNodeFn(self);
    }
};

pub const Program = struct {
    statements: std.ArrayList(*Statement),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .statements = std.ArrayList(*Statement).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.statements.items) |stmt| {
            const let_stmt: *LetStatement = @fieldParentPtr("statement", stmt);
            let_stmt.deinit(self.allocator);
        }
        self.statements.deinit();
    }

    pub fn token_literal(node: *Node) []const u8 {
        const self: *Program = @fieldParentPtr("node", node);

        if (self.statements.len > 0) {
            return self.statements[0].node.token_literal();
        }

        return "";
    }
};

pub const LetStatement = struct {
    node: Node,
    statement: Statement,
    token: token.Token,
    name: *Identifier,
    value: ?*Expression,

    pub fn init(allocator: std.mem.Allocator, t: token.Token, name: *Identifier, value: ?*Expression) !*LetStatement {
        const let_stmt = try allocator.create(LetStatement);
        let_stmt.* = .{
            .node = .{ .tokenLiteralFn = token_literal },
            .statement = .{
                .node = undefined,
                .statementNodeFn = statement_node,
            },
            .token = t,
            .name = name,
            .value = value,
        };

        let_stmt.statement.node = let_stmt.node;

        return let_stmt;
    }

    pub fn deinit(self: *LetStatement, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
        if (self.value) |_| {}
        allocator.destroy(self);
    }

    fn statement_node(statement: *Statement) void {
        _ = statement;
    }

    fn token_literal(node: *Node) []const u8 {
        const self: *LetStatement = @fieldParentPtr("node", node);
        return self.token.literal;
    }
};

pub const Identifier = struct {
    node: Node,
    expression: Expression,
    token: token.Token,
    value: []const u8,

    pub fn init(allocator: std.mem.Allocator, t: token.Token, value: []const u8) !*Identifier {
        const identifier = try allocator.create(Identifier);
        identifier.* = .{
            .node = .{ .tokenLiteralFn = token_literal },
            .expression = .{
                .node = undefined,
                .expressionNodeFn = expression_node,
            },
            .token = t,
            .value = value,
        };

        identifier.expression.node = identifier.node;

        return identifier;
    }

    pub fn deinit(self: *Identifier, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    fn expression_node(expression: *Expression) void {
        _ = expression;
    }

    fn token_literal(node: *Node) []const u8 {
        const self: *Identifier = @fieldParentPtr("node", node);
        return self.token.literal;
    }
};
