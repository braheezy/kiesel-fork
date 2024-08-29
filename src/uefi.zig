const builtin = @import("builtin");
const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Diagnostics = kiesel.language.Diagnostics;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const Value = kiesel.types.Value;

const Editor = @import("zigline").Editor;

const WriterContext = struct {
    console_out: *std.os.uefi.protocol.SimpleTextOutput,
    attribute: usize,
};

const Writer = std.io.GenericWriter(
    WriterContext,
    error{},
    struct {
        fn write(context: WriterContext, bytes: []const u8) error{}!usize {
            _ = context.console_out.setAttribute(context.attribute);
            for (bytes) |c| {
                _ = context.console_out.outputString(@ptrCast(&[2]u16{ c, 0 }));
            }
            return bytes.len;
        }
    }.write,
);

fn run(allocator: std.mem.Allocator, realm: *Realm, source_text: []const u8) !Value {
    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();
    const script = try Script.parse(source_text, realm, null, .{ .diagnostics = &diagnostics });
    return script.evaluate();
}

pub fn main() std.os.uefi.Status {
    const allocator = std.os.uefi.pool_allocator;

    const console_out = std.os.uefi.system_table.con_out.?;
    _ = console_out.reset(true);
    _ = console_out.clearScreen();

    const stdout_writer: Writer = .{
        .context = .{
            .console_out = console_out,
            .attribute = 0x7, // white
        },
    };
    const stderr_writer: Writer = .{
        .context = .{
            .console_out = console_out,
            .attribute = 0x4, // red
        },
    };

    var agent = Agent.init(allocator, .{
        .platform = .{
            .stdout = stdout_writer.any(),
            .stderr = stderr_writer.any(),
            .tty_config = .no_color,
            .stack_info = null,
            .default_locale = {},
            .currentTime = std.time.milliTimestamp,
        },
    }) catch |err| switch (err) {
        error.OutOfMemory => return .OutOfResources,
    };
    defer agent.deinit();

    Realm.initializeHostDefinedRealm(&agent, .{}) catch |err| switch (err) {
        error.OutOfMemory => return .OutOfResources,
        error.ExceptionThrown => unreachable,
    };
    const realm = agent.currentRealm();

    const stdout = agent.platform.stdout;
    const stderr = agent.platform.stderr;

    stdout.print("Kiesel {[kiesel]} [Zig {[zig]}] on uefi\r\n", .{
        .kiesel = kiesel.version,
        .zig = builtin.zig_version,
    }) catch unreachable;
    stdout.print("{[vendor]s}, 0x{[revision]x}\r\n", .{
        .vendor = std.unicode.fmtUtf16Le(std.mem.span(std.os.uefi.system_table.firmware_vendor)),
        .revision = std.os.uefi.system_table.firmware_revision,
    }) catch unreachable;

    var editor = Editor.init(allocator, .{});
    defer editor.deinit();

    while (true) {
        const source_text = editor.getLine("> ") catch |err| switch (err) {
            error.Eof => break,
            else => {
                stderr.print("Error: {!}\r\n", .{err}) catch unreachable;
                continue;
            },
        };
        defer allocator.free(source_text);

        // Directly show another prompt when spamming enter, whitespace is evaluated
        // however (and will print 'undefined').
        if (source_text.len == 0) continue;

        if (run(allocator, realm, source_text)) |result| {
            stdout.print("{pretty}\r\n", .{result}) catch unreachable;
        } else |err| switch (err) {
            error.OutOfMemory => return .OutOfResources,
            error.ParseError => stderr.print("Invalid syntax in input: {s}\r\n", .{source_text}) catch unreachable,
            error.ExceptionThrown => {
                const exception = agent.clearException();
                stderr.print("Uncaught exception: {pretty}\r\n", .{exception}) catch unreachable;
            },
        }
    }

    return .Success;
}
