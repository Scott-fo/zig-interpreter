const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const PROMPT = ">>";

pub fn start() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        try stdout.print("{s} ", .{PROMPT});
        var buffer: [1024]u8 = undefined;
        if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |user_input| {
            if (std.mem.eql(u8, "exit", user_input)) {
                try stdout.print("\rExiting...\n", .{});
                break;
            }
            var l = lexer.Lexer.init(user_input);
            var tok = l.next_token();
            while (tok != .EOF) : (tok = l.next_token()) {
                try stdout.print("Type: {s}", .{@tagName(tok)});

                switch (tok) {
                    .IDENT, .INT => |literal| try stdout.print(", Literal: {s}", .{literal}),
                    else => {},
                }

                try stdout.print("\n", .{});
            }
        }
    }
}
