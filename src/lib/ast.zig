const token = @import("token.zig");
const std = @import("std");

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        for (self.statements.items) |*stmt| {
            stmt.deinit(allocator);
        }
        self.statements.deinit();
    }

    pub fn token_literal(self: *const Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].token_literal();
        }
        return "";
    }
};

pub const Node = union(enum) {
    Program: Program,
    Statement: Statement,
    Expression: Expression,

    pub fn deinit(self: *Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            inline else => |*n| n.deinit(allocator),
        }
    }

    pub fn token_literal(self: *const Node) []const u8 {
        return switch (self.*) {
            inline else => |*n| n.token_literal(),
        };
    }
};

pub const Statement = union(enum) {
    Let: LetStatement,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            inline else => |*stmt| stmt.deinit(allocator),
        }
    }

    pub fn token_literal(self: *const Statement) []const u8 {
        return switch (self.*) {
            inline else => |stmt| stmt.token.literal,
        };
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    value: ?Expression,

    pub fn init(t: token.Token, name: Identifier, value: ?Expression) LetStatement {
        return .{
            .token = t,
            .name = name,
            .value = value,
        };
    }

    pub fn deinit(self: *LetStatement, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
        if (self.value) |*v| v.deinit(allocator);
    }
};

pub const Expression = union(enum) {
    Identifier: Identifier,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            inline else => |*e| e.deinit(allocator),
        }
    }

    pub fn token_literal(self: *const Expression) []const u8 {
        return switch (self.*) {
            inline else => |*e| e.token.literal,
        };
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn init(t: token.Token, value: []const u8) Identifier {
        return .{
            .token = t,
            .value = value,
        };
    }

    pub fn deinit(self: *Identifier, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }
};
