const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const token = @import("token.zig");
const evaluator = @import("evaluator.zig");
const object = @import("object.zig");
const environment = @import("environment.zig");

const PROMPT = ">>";

pub fn start() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) @panic("Memory leak detected");
    }
    const gpa_allocator = gpa.allocator();

    const env = try environment.Environment.init(gpa_allocator);
    defer env.deinit();

    while (true) {
        var arena = std.heap.ArenaAllocator.init(gpa_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        try stdout.print("{s} ", .{PROMPT});
        var buffer: [1024]u8 = undefined;
        if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |user_input| {
            if (std.mem.eql(u8, "exit", user_input)) {
                try stdout.print("\rExiting...\n", .{});
                break;
            }
            var l = lexer.Lexer.init(user_input);
            var p = try parser.Parser.init(allocator, &l);

            var program = try p.parseProgram();

            const errs = p.getErrors();
            if (errs.len > 0) {
                for (errs) |err| {
                    parser.printError(err);
                }
                continue;
            }

            const evaluated = try evaluator.eval(allocator, &program.node, env);
            if (evaluated != null) {
                const eval_str = try evaluated.?.inspect(allocator);

                try stdout.print("{s}", .{eval_str});
                try stdout.print("\n", .{});
            }

            try printEnvContents(allocator, stdout, env);
        }
    }
}

fn printEnvContents(
    allocator: std.mem.Allocator,
    stdout: anytype,
    env: *environment.Environment,
) !void {
    try stdout.print("\n--- Environment Contents ---\n", .{});
    var it = env.store.iterator();
    while (it.next()) |entry| {
        const obj_str = try entry.value_ptr.*.inspect(allocator);
        try stdout.print("{s}: {s}\n", .{ entry.key_ptr.*, obj_str });
    }
    try stdout.print("----------------------------\n", .{});
}
