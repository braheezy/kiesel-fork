const builtin = @import("builtin");
const std = @import("std");

const kiesel = @import("kiesel");
const kiesel_runtime = @import("kiesel-runtime");

const Agent = kiesel.execution.Agent;
const Diagnostics = kiesel.language.Diagnostics;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const Value = kiesel.types.Value;
const fmtParseError = kiesel.language.fmtParseError;
const fmtParseErrorHint = kiesel.language.fmtParseErrorHint;

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

    // No std.posix.getrandom() on UEFI
    .cryptoRandomSeed = struct {
        fn cryptoRandomSeed(buffer: []u8) void {
            var rng: *std.os.uefi.protocol.Rng = undefined;
            const status = std.os.uefi.system_table.boot_services.?.locateProtocol(
                &std.os.uefi.protocol.Rng.guid,
                null,
                @ptrCast(&rng),
            );
            // Might return .not_found, e.g. in QEMU without virtio-rng
            if (status != .success) return;
            _ = rng.getRNG(null, buffer.len, buffer.ptr);
        }
    }.cryptoRandomSeed,

    // tlsCsprngFill() needs this to work on UEFI due to lack of TLS
    .crypto_always_getrandom = true,
};

const WriterContext = struct {
    console_out: *std.os.uefi.protocol.SimpleTextOutput,
    attribute: usize,
};

fn writeFn(context: *const anyopaque, bytes: []const u8) anyerror!usize {
    const writer_context: *const WriterContext = @alignCast(@ptrCast(context));
    const console_out = writer_context.console_out;
    _ = console_out.setAttribute(writer_context.attribute);
    for (bytes) |c| {
        if (c == '\n') {
            _ = console_out.outputString(@ptrCast(&[2]u16{ '\r', 0 }));
        }
        _ = console_out.outputString(@ptrCast(&[2]u16{ c, 0 }));
    }
    return bytes.len;
}

pub fn main() std.os.uefi.Status {
    const allocator = std.os.uefi.pool_allocator;

    const console_out = std.os.uefi.system_table.con_out.?;
    _ = console_out.reset(true);
    _ = console_out.clearScreen();

    const stdout: std.io.AnyWriter = .{
        .context = &WriterContext{
            .console_out = console_out,
            .attribute = 0x7, // white
        },
        .writeFn = writeFn,
    };
    const stderr: std.io.AnyWriter = .{
        .context = &WriterContext{
            .console_out = console_out,
            .attribute = 0x4, // red
        },
        .writeFn = writeFn,
    };

    const platform: Agent.Platform = .{
        .gc_allocator = allocator,
        .gc_allocator_atomic = allocator,
        .stdout = stdout,
        .stderr = stderr,
        .tty_config = .no_color,
        .stack_info = null,
        .default_locale = {},
        .currentTime = std.time.milliTimestamp,
    };
    defer platform.deinit();
    var agent = Agent.init(&platform, .{}) catch |err| switch (err) {
        error.OutOfMemory => return .out_of_resources,
    };
    defer agent.deinit();

    Realm.initializeHostDefinedRealm(&agent, .{}) catch |err| switch (err) {
        error.OutOfMemory => return .out_of_resources,
        error.ExceptionThrown => unreachable,
    };
    const realm = agent.currentRealm();

    kiesel_runtime.addBindings(&agent, realm, realm.global_object) catch |err| switch (err) {
        error.OutOfMemory => return .out_of_resources,
    };

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
                stderr.print(
                    "{}\n",
                    .{fmtParseErrorHint(parse_error, source_text)},
                ) catch unreachable;
                const syntax_error = agent.createErrorObject(
                    .syntax_error,
                    "{}",
                    .{fmtParseError(parse_error)},
                ) catch return .out_of_resources;
                const exception: Agent.Exception = .{
                    .value = Value.from(syntax_error),
                    .stack_trace = &.{},
                };
                stderr.print("{pretty}\n", .{exception}) catch unreachable;
                continue;
            },
            error.OutOfMemory => return .out_of_resources,
        };

        if (script.evaluate()) |result| {
            stdout.print("{pretty}\n", .{result}) catch unreachable;
        } else |err| switch (err) {
            error.OutOfMemory => return .out_of_resources,
            error.ExceptionThrown => {
                const exception = agent.clearException();
                stderr.print("{pretty}\n", .{exception}) catch unreachable;
            },
        }
    }

    return .success;
}
