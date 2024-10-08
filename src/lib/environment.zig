const object = @import("object.zig");
const std = @import("std");

pub const Environment = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    store: std.StringHashMap(*object.Object),
    outer: ?*Self,

    pub fn newEnclosedEnvironment(allocator: std.mem.Allocator, outer: *Self) !*Self {
        var e = try Self.init(allocator);
        e.outer = outer;

        return e;
    }

    pub fn init(allocator: std.mem.Allocator) !*Self {
        const e = try allocator.create(Self);
        e.* = .{
            .allocator = allocator,
            .store = std.StringHashMap(*object.Object).init(allocator),
            .outer = null,
        };

        return e;
    }

    pub fn deinit(self: *Self) void {
        var it = self.store.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.*.deinit(self.allocator);
        }

        self.store.deinit();
        self.allocator.destroy(self);
    }

    pub fn get(self: *Self, name: []const u8) ?*object.Object {
        var obj = self.store.get(name);
        if (obj == null and self.outer != null) {
            obj = self.outer.?.get(name);
        }

        return obj;
    }

    pub fn set(self: *Self, name: []const u8, val: *object.Object) !*object.Object {
        const owned_name = try self.allocator.dupe(u8, name);
        try self.store.put(owned_name, val);

        return val;
    }
};
