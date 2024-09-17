const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const token = @import("token.zig");
const evaluator = @import("evaluator.zig");
const object = @import("object.zig");

const PROMPT = ">>";

pub fn start() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) @panic("memory leak");
    }

    const allocator = gpa.allocator();

    while (true) {
        try stdout.print("{s} ", .{PROMPT});
        var buffer: [1024]u8 = undefined;
        if (try stdin.readUntilDelimiterOrEof(buffer[0..], '\n')) |user_input| {
            if (std.mem.eql(u8, "exit", user_input)) {
                try stdout.print("\rExiting...\n", .{});
                break;
            }
            var l = lexer.Lexer.init(user_input);
            var p = try parser.Parser.init(allocator, &l);
            errdefer p.deinit();

            var program = try p.parseProgram();
            defer program.node.deinit(allocator);

            const errs = p.getErrors();
            if (errs.len > 0) {
                for (errs) |err| {
                    parser.printError(err);
                }
                continue;
            }

            const evaluated = try evaluator.eval(&program.node, allocator);
            if (evaluated != null) {
                defer evaluated.?.deinit(allocator);

                const eval_str = try evaluated.?.inspect(allocator);
                defer allocator.free(eval_str);

                try stdout.print("{s}", .{eval_str});
                try stdout.print("\n", .{});
            }
        }
    }
}
