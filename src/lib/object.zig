const std = @import("std");

pub const ObjectType = enum {
    IntegerObj,
    BooleanObj,
    NullObj,
    ReturnValueObj,
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

pub const ReturnValue = struct {
    const Self = @This();

    object: Object,
    value: ?*Object,

    const vtable = Object.VTable{
        .objectTypeFn = objectType,
        .inspectFn = inspect,
        .deinitFn = deinit,
    };

    pub fn init(allocator: std.mem.Allocator, value: *Object) !*Self {
        const rv = try allocator.create(Self);
        rv.* = .{
            .object = .{ .vtable = &vtable },
            .value = value,
        };

        return rv;
    }

    pub fn deinit(object: *Object, allocator: std.mem.Allocator) void {
        const self: *Self = @fieldParentPtr("object", object);
        if (self.value) |v| {
            v.deinit(allocator);
        }
        allocator.destroy(self);
    }

    pub fn objectType(_: *const Object) ObjectType {
        return .ReturnValueObj;
    }

    pub fn inspect(object: *const Object, allocator: std.mem.Allocator) ![]const u8 {
        const self: *const Self = @fieldParentPtr("object", object);
        if (self.value == null) {
            return "";
        }

        return self.value.?.inspect(allocator);
    }

    pub fn moveValue(self: *Self) ?*Object {
        const value = self.value;
        self.value = null;
        return value;
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

    pub var TRUE: Self = .{
        .object = .{ .vtable = &vtable },
        .value = true,
    };

    pub var FALSE: Self = .{
        .object = .{ .vtable = &vtable },
        .value = false,
    };

    pub fn get(v: bool) *Self {
        return switch (v) {
            true => &TRUE,
            false => &FALSE,
        };
    }

    pub fn deinit(_: *Object, _: std.mem.Allocator) void {}

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

    pub var NULL: Self = .{
        .object = .{ .vtable = &vtable },
    };

    pub fn get() *Self {
        return &NULL;
    }

    pub fn deinit(_: *Object, _: std.mem.Allocator) void {}

    pub fn objectType(_: *const Object) ObjectType {
        return .NullObj;
    }

    pub fn inspect(_: *const Object, allocator: std.mem.Allocator) ![]const u8 {
        // Annoying, but means that I can use a consistent allocator.free
        return allocator.dupe(u8, "null");
    }
};
