const builtin = @import("builtin");
const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Diagnostics = kiesel.language.Diagnostics;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const Value = kiesel.types.Value;
const formatParseError = kiesel.utils.formatParseError;
const formatParseErrorHint = kiesel.utils.formatParseErrorHint;

const Editor = @import("zigline").Editor;

pub const std_options: std.Options = .{
    // The default log function doesn't compile for UEFI, but we don't need logging anyway.
    // There are log statements in zigline that break debug builds without this.
    .logFn = struct {
        fn logFn(
            comptime _: std.log.Level,
            comptime _: @TypeOf(.enum_literal),
            comptime _: []const u8,
            _: anytype,
        ) void {}
    }.logFn,
};

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
                if (c == '\n') {
                    _ = context.console_out.outputString(@ptrCast(&[2]u16{ '\r', 0 }));
                }
                _ = context.console_out.outputString(@ptrCast(&[2]u16{ c, 0 }));
            }
            return bytes.len;
        }
    }.write,
);

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

    stdout.print("Kiesel {[kiesel]} [Zig {[zig]}] on uefi\n", .{
        .kiesel = kiesel.version,
        .zig = builtin.zig_version,
    }) catch unreachable;
    stdout.print("{[vendor]s}, 0x{[revision]x}\n", .{
        .vendor = std.unicode.fmtUtf16Le(std.mem.span(std.os.uefi.system_table.firmware_vendor)),
        .revision = std.os.uefi.system_table.firmware_revision,
    }) catch unreachable;

    var editor = Editor.init(allocator, .{});
    defer editor.deinit();

    while (true) {
        const source_text = editor.getLine("> ") catch |err| switch (err) {
            error.Eof => break,
            else => {
                stderr.print("Error: {!}\n", .{err}) catch unreachable;
                continue;
            },
        };
        defer allocator.free(source_text);

        // Directly show another prompt when spamming enter, whitespace is evaluated
        // however (and will print 'undefined').
        if (source_text.len == 0) continue;

        var diagnostics = Diagnostics.init(allocator);
        defer diagnostics.deinit();
        const script = Script.parse(source_text, realm, null, .{
            .diagnostics = &diagnostics,
            .file_name = "repl",
        }) catch |err| switch (err) {
            error.ParseError => {
                const parse_error = diagnostics.errors.items[0];
                const parse_error_hint = formatParseErrorHint(
                    allocator,
                    parse_error,
                    source_text,
                ) catch return .OutOfResources;
                defer allocator.free(parse_error_hint);
                stderr.print("{s}\n", .{parse_error_hint}) catch unreachable;
                const syntax_error = agent.createException(
                    .syntax_error,
                    "{s}",
                    .{formatParseError(
                        agent.gc_allocator,
                        parse_error,
                    ) catch return .OutOfResources},
                ) catch return .OutOfResources;
                stderr.print(
                    "Uncaught exception: {pretty}\n",
                    .{Value.from(syntax_error)},
                ) catch unreachable;
                continue;
            },
            error.OutOfMemory => return .OutOfResources,
        };

        if (script.evaluate()) |result| {
            stdout.print("{pretty}\n", .{result}) catch unreachable;
        } else |err| switch (err) {
            error.OutOfMemory => return .OutOfResources,
            error.ExceptionThrown => {
                const exception = agent.clearException();
                stderr.print("Uncaught exception: {pretty}\n", .{exception}) catch unreachable;
            },
        }
    }

    return .Success;
}
