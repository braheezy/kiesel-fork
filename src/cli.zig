const std = @import("std");
const kiesel = @import("kiesel");

const Allocator = std.mem.Allocator;

const Agent = kiesel.execution.Agent;
const Diagnostics = kiesel.language.Diagnostics;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const Value = kiesel.types.Value;

fn run(
    allocator: Allocator,
    realm: *Realm,
    file_name: []const u8,
    source_text: []const u8,
) !?Value {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const agent = realm.agent;

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    const script = Script.parse(source_text, realm, null, .{
        .diagnostics = &diagnostics,
        .file_name = file_name,
    }) catch |err| switch (err) {
        error.ParseError => {
            try diagnostics.print(stderr);
            return null;
        },
        error.OutOfMemory => return error.OutOfMemory,
    };

    if (agent.options.debug.print_ast)
        try script.ecmascript_code.print(stdout);

    return script.evaluate() catch |err| switch (err) {
        error.BytecodeGenerationFailed => {
            try stderr.print("Bytecode generation failed\n", .{});
            return null;
        },
        // TODO: Enable this once there's bytecode instructions that can throw.
        // error.ExceptionThrown => {
        //     try stderr.print("Uncaught exception: {}\n", .{agent.exception.?});
        //     return null;
        // },
        else => return err,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    var agent = try Agent.init(.{
        .debug = .{
            .print_ast = true,
            .print_bytecode = true,
        },
    });
    try Realm.initializeHostDefinedRealm(&agent, .{});
    const realm = agent.currentRealm();

    _ = args.skip();
    if (args.next()) |path| {
        const file = try std.fs.cwd().openFile(path, .{});
        const file_name = std.fs.path.basename(path);
        const file_size = (try file.stat()).size;
        var source_text = try allocator.alloc(u8, file_size);
        defer allocator.free(source_text);
        _ = try file.readAll(source_text);
        _ = try run(allocator, realm, file_name, source_text);
    } else {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();
        while (true) {
            try stdout.print("> ", .{});
            if (try stdin.readUntilDelimiterOrEofAlloc(
                allocator,
                '\n',
                std.math.maxInt(usize),
            )) |source_text| {
                defer allocator.free(source_text);

                // Directly show another prompt when spamming enter, whitespace is evaluated
                // however (and will print 'undefined').
                if (source_text.len == 0)
                    continue;

                if (try run(allocator, realm, "repl", source_text)) |result| {
                    try stdout.print("{}\n", .{result});
                }
                // Handled exception & printed something, carry on
                else continue;
            }
            // ^C pressed, exit REPL
            else break;
        }
    }
}
