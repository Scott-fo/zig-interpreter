const object = @import("object.zig");
const std = @import("std");

pub const Environment = struct {
    const Self = @This();

    store: std.StringHashMap(*object.Object),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*Self {
        const e = try allocator.create(Self);
        e.* = .{
            .store = std.StringHashMap(*object.Object).init(allocator),
            .allocator = allocator,
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
        return self.store.get(name);
    }

    pub fn set(self: *Self, name: []const u8, val: *object.Object) !*object.Object {
        const owned_name = try self.allocator.dupe(u8, name);
        const cloned_val = try val.clone(self.allocator);

        const gop = try self.store.getOrPut(owned_name);
        if (gop.found_existing) {
            self.allocator.free(owned_name);
            gop.value_ptr.*.deinit(self.allocator);
        }

        gop.value_ptr.* = cloned_val;
        return val;
    }
};
