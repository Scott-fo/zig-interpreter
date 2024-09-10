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

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral();
        }
        return "";
    }

    pub fn string(self: *const Program, allocator: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        for (self.statements.items) |stmt| {
            try buffer.appendSlice(try stmt.string(allocator));
        }

        return buffer.toOwnedSlice();
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

    pub fn tokenLiteral(self: *const Node) []const u8 {
        return switch (self.*) {
            inline else => |*n| n.tokenLiteral(),
        };
    }

    pub fn string(self: *const Node, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self.*) {
            inline else => |*e| e.string(allocator),
        };
    }
};

pub const Statement = union(enum) {
    Let: LetStatement,
    Return: ReturnStatement,
    Expression: ExpressionStatement,

    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            inline else => |*stmt| stmt.deinit(allocator),
        }
    }

    pub fn tokenLiteral(self: *const Statement) []const u8 {
        return switch (self.*) {
            inline else => |stmt| stmt.tokenLiteral(),
        };
    }

    pub fn string(self: *const Statement, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self.*) {
            inline else => |*s| s.string(allocator),
        };
    }
};

pub const ExpressionStatement = struct {
    const Self = @This();

    token: token.Token,
    expression: ?Expression,

    pub fn init(t: token.Token, expression: ?Expression) Self {
        return .{
            .token = t,
            .expression = expression,
        };
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.toLiteral();
    }

    pub fn string(self: *const Self, _: std.mem.Allocator) ![]const u8 {
        if (self.expression) |expr| {
            return expr.string();
        }
        return "";
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        if (self.expression) |*e| e.deinit(allocator);
    }
};

pub const ReturnStatement = struct {
    const Self = @This();

    token: token.Token,
    value: ?Expression,

    pub fn init(t: token.Token, value: ?Expression) Self {
        return .{
            .token = t,
            .value = value,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        if (self.value) |*v| v.deinit(allocator);
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.toLiteral();
    }

    pub fn string(self: *const Self, allocator: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        try buffer.appendSlice(self.tokenLiteral());
        try buffer.append(' ');
        if (self.value) |value| {
            try buffer.appendSlice(value.string());
        }
        try buffer.append(';');

        return buffer.toOwnedSlice();
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

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.toLiteral();
    }

    pub fn deinit(self: *LetStatement, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
        if (self.value) |*v| v.deinit(allocator);
    }

    pub fn string(self: *const LetStatement, allocator: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(allocator);
        defer buffer.deinit();

        try buffer.appendSlice(self.tokenLiteral());
        try buffer.append(' ');
        try buffer.appendSlice(self.name.string());
        try buffer.appendSlice(" = ");
        if (self.value) |value| {
            try buffer.appendSlice(value.string());
        }
        try buffer.append(';');

        return buffer.toOwnedSlice();
    }
};

pub const Expression = union(enum) {
    Identifier: Identifier,
    IntegerLiteral: IntegerLiteral,

    pub fn deinit(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            inline else => |*e| e.deinit(allocator),
        }
    }

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            inline else => |*e| e.tokenLiteral(),
        };
    }

    pub fn string(self: *const Expression) []const u8 {
        return switch (self.*) {
            inline else => |*e| e.string(),
        };
    }
};

pub const IntegerLiteral = struct {
    const Self = @This();

    token: token.Token,
    value: ?i64,

    pub fn init(t: token.Token, value: ?i64) Self {
        return .{
            .token = t,
            .value = value,
        };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.toLiteral();
    }

    pub fn string(self: *const Self) []const u8 {
        return self.tokenLiteral();
    }
};

pub const Identifier = struct {
    const Self = @This();

    token: token.Token,
    value: []const u8,

    pub fn init(t: token.Token, value: []const u8) Self {
        return .{
            .token = t,
            .value = value,
        };
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.toLiteral();
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }

    pub fn string(self: *const Self) []const u8 {
        return self.value;
    }
};

test "test string representation of AST" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = Program.init(allocator);
    defer program.deinit(allocator);

    const let_statement: LetStatement = .{
        .token = .LET,
        .name = Identifier.init(.{ .IDENT = "myVar" }, "myVar"),
        .value = .{ .Identifier = Identifier.init(.{ .IDENT = "anotherVar" }, "anotherVar") },
    };

    try program.statements.append(.{ .Let = let_statement });

    const program_string = try program.string(allocator);
    defer allocator.free(program_string);

    try std.testing.expectEqualStrings("let myVar = anotherVar;", program_string);
}
