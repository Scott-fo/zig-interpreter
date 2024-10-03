const std = @import("std");
const ast = @import("ast.zig");
const environment = @import("environment.zig");

pub const ObjectType = enum {
    Integer,
    Boolean,
    Null,
    ReturnValue,
    Error,
    Function,

    pub fn toString(o: ObjectType) []const u8 {
        return switch (o) {
            .Integer => "INTEGER",
            .Boolean => "BOOLEAN",
            .Null => "NULL",
            .ReturnValue => "RETURN VALUE",
            .Error => "ERROR",
            .Function => "FUNCTION",
        };
    }
};

pub const Object = struct {
    const Self = @This();

    vtable: *const VTable,

    pub const VTable = struct {
        objectTypeFn: *const fn (self: *const Self) ObjectType,
        inspectFn: *const fn (self: *const Self, arena: std.mem.Allocator) anyerror![]const u8,
        deinitFn: *const fn (self: *Self, gpa: std.mem.Allocator) void,
        cloneFn: *const fn (self: *const Self, gpa: std.mem.Allocator) anyerror!*Self,
    };

    pub fn objectType(self: *const Self) ObjectType {
        return self.vtable.objectTypeFn(self);
    }

    pub fn inspect(self: *const Self, arena: std.mem.Allocator) ![]const u8 {
        return self.vtable.inspectFn(self, arena);
    }

    pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
        return self.vtable.deinitFn(self, gpa);
    }

    pub fn clone(self: *const Self, gpa: std.mem.Allocator) !*Self {
        return self.vtable.cloneFn(self, gpa);
    }
};

pub const ReturnValue = struct {
    const Self = @This();

    object: Object,
    value: ?*Object,

    const vtable = Object.VTable{ .objectTypeFn = objectType, .inspectFn = inspect, .deinitFn = deinit, .cloneFn = clone };

    pub fn init(allocator: std.mem.Allocator, value: ?*Object) !*Self {
        const rv = try allocator.create(Self);
        rv.* = .{
            .object = .{ .vtable = &vtable },
            .value = value,
        };

        return rv;
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .ReturnValue;
    }

    pub fn inspect(object: *const Object, arena: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("object", object);
        if (self.value == null) {
            return "";
        }

        return self.value.?.inspect(arena);
    }

    pub fn deinit(object: *Object, gpa: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("object", object);
        if (self.value) |v| {
            v.deinit(gpa);
        }
        gpa.destroy(self);
    }

    pub fn clone(object: *const Object, gpa: std.mem.Allocator) !*Object {
        const self: *const Self = @fieldParentPtr("object", object);
        const cloned_value = if (self.value) |v| try v.clone(gpa) else null;

        const cs = try Self.init(gpa, cloned_value);
        return &cs.object;
    }
};

pub const Integer = struct {
    const Self = @This();

    object: Object,
    value: i64,

    const vtable = Object.VTable{
        .objectTypeFn = objectType,
        .inspectFn = inspect,
        .deinitFn = deinit,
        .cloneFn = clone,
    };

    pub fn init(allocator: std.mem.Allocator, value: i64) !*Self {
        const int = try allocator.create(Self);
        int.* = .{
            .object = .{ .vtable = &vtable },
            .value = value,
        };

        return int;
    }

    pub fn deinit(object: *Object, gpa: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("object", object);
        gpa.destroy(self);
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .Integer;
    }

    pub fn inspect(object: *const Object, arena: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("object", object);
        return std.fmt.allocPrint(arena, "{}", .{self.value});
    }

    pub fn clone(object: *const Object, gpa: std.mem.Allocator) !*Object {
        const self: *const Self = @fieldParentPtr("object", object);
        const cs = try Self.init(gpa, self.value);
        return &cs.object;
    }
};

pub const Boolean = struct {
    const Self = @This();

    object: Object,
    value: bool,

    const vtable = Object.VTable{
        .objectTypeFn = objectType,
        .inspectFn = inspect,
        .deinitFn = deinit,
        .cloneFn = clone,
    };

    pub var TRUE: Self = .{
        .object = .{ .vtable = &vtable },
        .value = true,
    };

    pub var FALSE: Self = .{
        .object = .{ .vtable = &vtable },
        .value = false,
    };

    pub fn deinit(_: *Object, _: std.mem.Allocator) void {}

    pub fn get(v: bool) *Object {
        return switch (v) {
            true => &TRUE.object,
            false => &FALSE.object,
        };
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .Boolean;
    }

    pub fn inspect(object: *const Object, arena: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("object", object);
        return std.fmt.allocPrint(arena, "{}", .{self.value});
    }

    pub fn clone(object: *const Object, _: std.mem.Allocator) !*Object {
        const self: *const Self = @fieldParentPtr("object", object);
        return Self.get(self.value);
    }
};

pub const Error = struct {
    const Self = @This();

    object: Object,
    message: []const u8,

    const vtable = Object.VTable{
        .objectTypeFn = objectType,
        .inspectFn = inspect,
        .deinitFn = deinit,
        .cloneFn = clone,
    };

    pub fn init(allocator: std.mem.Allocator, message: []const u8) !*Self {
        const err = try allocator.create(Self);
        err.* = .{
            .object = .{ .vtable = &vtable },
            .message = message,
        };

        return err;
    }

    pub fn deinit(object: *Object, gpa: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("object", object);
        gpa.free(self.message);
        gpa.destroy(self);
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .Error;
    }

    pub fn inspect(object: *const Object, arena: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("object", object);
        return std.fmt.allocPrint(arena, "ERROR: {s}", .{self.message});
    }

    pub fn clone(object: *const Object, gpa: std.mem.Allocator) !*Object {
        const self: *const Self = @fieldParentPtr("object", object);
        const cloned_message = try gpa.dupe(u8, self.message);

        const cs = try Self.init(gpa, cloned_message);
        return &cs.object;
    }
};

pub const Null = struct {
    const Self = @This();

    object: Object,

    const vtable = Object.VTable{
        .objectTypeFn = objectType,
        .inspectFn = inspect,
        .deinitFn = deinit,
        .cloneFn = clone,
    };

    pub var NULL: Self = .{
        .object = .{ .vtable = &vtable },
    };

    pub fn deinit(_: *Object, _: std.mem.Allocator) void {}

    pub fn get() *Object {
        return &NULL.object;
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .Null;
    }

    pub fn inspect(_: *const Object, _: std.mem.Allocator) ![]const u8 {
        return "null";
    }

    pub fn clone(_: *const Object, _: std.mem.Allocator) !*Object {
        return Null.get();
    }
};

pub const Function = struct {
    const Self = @This();

    object: Object,
    parameters: std.ArrayList(*ast.Identifier),
    body: *ast.BlockStatement,
    env: *environment.Environment,

    const vtable = Object.VTable{
        .objectTypeFn = objectType,
        .inspectFn = inspect,
        .deinitFn = deinit,
        .cloneFn = clone,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        env: *environment.Environment,
        body: *ast.BlockStatement,
    ) !*Self {
        const f = try allocator.create(Self);
        f.* = .{
            .object = .{ .vtable = &vtable },
            .env = env,
            .body = body,
            .parameters = std.ArrayList(*ast.Identifier).init(allocator),
        };

        return f;
    }

    pub fn deinit(object: *Object, gpa: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("object", object);

        self.body.node.deinit(gpa);
        for (self.parameters.items) |param| {
            param.expression.node.deinit(gpa);
        }

        gpa.destroy(self);
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .Function;
    }

    pub fn inspect(object: *const Object, arena: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("object", object);

        var buffer = std.ArrayList(u8).init(arena);
        try buffer.appendSlice("fn");
        try buffer.append('(');

        for (self.parameters.items, 0..) |param, i| {
            const param_str = try param.expression.node.string(arena);

            if (i != 0) {
                try buffer.append(',');
            }
            try buffer.appendSlice(param_str);
        }

        try buffer.append(')');
        try buffer.append('\n');

        const body_str = try self.body.node.string(arena);
        try buffer.appendSlice(body_str);
        try buffer.append('\n');

        return buffer.toOwnedSlice();
    }

    pub fn clone(object: *const Object, gpa: std.mem.Allocator) !*Object {
        // Need to implement clone methods for my ast if i am going to use this
        // approach with a gpa and an arena.
        // For now, using just arena to get the whole thing working, then we can
        // come back to this.
        _ = object;
        _ = gpa;
        return Null.get();
    }
};
