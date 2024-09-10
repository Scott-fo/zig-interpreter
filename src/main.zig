const std = @import("std");
const repl = @import("lib/repl.zig");

test {
    _ = @import("lib/token.zig");
    _ = @import("lib/lexer.zig");
    _ = @import("lib/parser.zig");
    _ = @import("lib/ast.zig");
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Welcome to Monkey REPL!\n", .{});
    try repl.start();
}
