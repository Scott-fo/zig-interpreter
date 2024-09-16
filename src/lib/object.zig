const std = @import("std");

pub const ObjectType = enum {
    IntegerObj,
    BooleanObj,
    NullObj,
};

pub const Object = struct {
    const Self = @This();

    vtable: *const VTable,

    pub const VTable = struct {
        objectTypeFn: *const fn (self: *const Self) ObjectType,
        inspectFn: *const fn (self: *const Self, allocator: std.mem.Allocator) anyerror![]const u8,
        deinitFn: *const fn (self: *Self, allocator: std.mem.Allocator) void,
    };

    pub fn objectType(self: *const Self) ObjectType {
        return self.vtable.objectTypeFn(self);
    }

    pub fn inspect(self: *const Self, allocator: std.mem.Allocator) ![]const u8 {
        return self.vtable.inspectFn(self, allocator);
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        return self.vtable.deinitFn(self, allocator);
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
    };

    pub fn init(allocator: std.mem.Allocator, value: i64) !*Self {
        const int = try allocator.create(Self);
        int.* = .{
            .object = .{ .vtable = &vtable },
            .value = value,
        };

        return int;
    }

    pub fn deinit(object: *Object, allocator: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("object", object);
        allocator.destroy(self);
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .IntegerObj;
    }

    pub fn inspect(object: *const Object, allocator: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("object", object);
        return std.fmt.allocPrint(allocator, "{}", .{self.value});
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
    };

    pub fn init(allocator: std.mem.Allocator, value: bool) !*Self {
        const boo = try allocator.create(Self);
        boo.* = .{
            .object = .{ .vtable = &vtable },
            .value = value,
        };

        return boo;
    }

    pub fn deinit(object: *Object, allocator: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("object", object);
        allocator.destroy(self);
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .BooleanObj;
    }

    pub fn inspect(object: *const Object, allocator: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("object", object);
        return std.fmt.allocPrint(allocator, "{}", .{self.value});
    }
};

pub const Null = struct {
    const Self = @This();

    object: Object,

    const vtable = Object.VTable{
        .objectTypeFn = objectType,
        .inspectFn = inspect,
        .deinitFn = deinit,
    };

    pub fn init(allocator: std.mem.Allocator) !*Self {
        const nu = try allocator.create(Self);
        nu.* = .{
            .object = .{ .vtable = &vtable },
        };

        return nu;
    }

    pub fn deinit(object: *Object, allocator: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("object", object);
        allocator.destroy(self);
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .NullObj;
    }

    pub fn inspect(_: *const Object, _: std.mem.Allocator) ![]const u8 {
        return "null";
    }
};
