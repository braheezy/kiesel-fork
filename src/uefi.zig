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
            const rng = std.os.uefi.system_table.boot_services.?.locateProtocol(
                std.os.uefi.protocol.Rng,
                null,
            ) catch {
                // No random bytes for you
                return;
            } orelse {
                // Might return null, e.g. in QEMU without virtio-rng
                return;
            };
            rng.getRNG(null, buffer) catch {};
        }
    }.cryptoRandomSeed,

    // tlsCsprngFill() needs this to work on UEFI due to lack of TLS
    .crypto_always_getrandom = true,
};

pub const Writer = struct {
    console_out: *std.os.uefi.protocol.SimpleTextOutput,
    attribute: std.os.uefi.protocol.SimpleTextOutput.Attribute,
    interface: std.Io.Writer,

    pub fn init(
        buffer: []u8,
        console_out: *std.os.uefi.protocol.SimpleTextOutput,
        attribute: std.os.uefi.protocol.SimpleTextOutput.Attribute,
    ) Writer {
        return .{
            .console_out = console_out,
            .attribute = attribute,
            .interface = .{
                .vtable = &.{
                    .drain = drain,
                },
                .buffer = buffer,
            },
        };
    }

    const WriteError =
        std.os.uefi.protocol.SimpleTextOutput.SetAttributeError ||
        std.os.uefi.protocol.SimpleTextOutput.OutputStringError;

    fn write(self: Writer, bytes: []const u8) WriteError!usize {
        try self.console_out.setAttribute(self.attribute);
        for (bytes) |c| {
            if (c == '\n') {
                _ = try self.console_out.outputString(&.{ '\r', 0 });
            }
            _ = try self.console_out.outputString(&.{ c, 0 });
        }
        return bytes.len;
    }

    fn drain(io_writer: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        const writer: *Writer = @alignCast(@fieldParentPtr("interface", io_writer));
        const buffered = io_writer.buffered();
        if (buffered.len != 0) {
            const n = writer.write(buffered) catch return error.WriteFailed;
            return io_writer.consume(n);
        }
        for (data[0 .. data.len - 1]) |buf| {
            if (buf.len == 0) continue;
            const n = writer.write(buf) catch return error.WriteFailed;
            return io_writer.consume(n);
        }
        const pattern = data[data.len - 1];
        if (pattern.len == 0 or splat == 0) return 0;
        const n = writer.write(pattern) catch return error.WriteFailed;
        return io_writer.consume(n);
    }
};

const Error =
    std.mem.Allocator.Error ||
    std.Io.Writer.Error ||
    std.os.uefi.Error;

fn mainWithErrorHandling() Error!void {
    const allocator = std.os.uefi.pool_allocator;

    const console_out = std.os.uefi.system_table.con_out.?;
    try console_out.reset(true);
    try console_out.clearScreen();
    try console_out.enableCursor(true);

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer: Writer = .init(&stdout_buffer, console_out, .{ .foreground = .white });
    const stdout = &stdout_writer.interface;

    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer: Writer = .init(&stderr_buffer, console_out, .{ .foreground = .red });
    const stderr = &stderr_writer.interface;

    const platform: Agent.Platform = .{
        .gc_allocator = allocator,
        .gc_allocator_atomic = allocator,
        .stdout = stdout,
        .stderr = stderr,
        .tty_config = .no_color,
        .stack_info = null,
        .default_locale = {},
        .default_time_zone = {},
        .currentTimeMs = std.time.milliTimestamp,
        .currentTimeNs = std.time.nanoTimestamp,
    };
    defer platform.deinit();
    var agent = try Agent.init(&platform, .{});
    defer agent.deinit();

    Realm.initializeHostDefinedRealm(&agent, .{}) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ExceptionThrown => unreachable,
    };
    const realm = agent.currentRealm();

    try kiesel_runtime.addBindings(&agent, realm, realm.global_object);

    try stdout.print("Kiesel {[kiesel]f} [Zig {[zig]f}] on uefi\n", .{
        .kiesel = kiesel.version,
        .zig = builtin.zig_version,
    });
    try stdout.print("{[vendor]f}, 0x{[revision]x}\n", .{
        .vendor = std.unicode.fmtUtf16Le(std.mem.span(std.os.uefi.system_table.firmware_vendor)),
        .revision = std.os.uefi.system_table.firmware_revision,
    });
    try stdout.flush();

    var editor = Editor.init(allocator, .{});
    defer editor.deinit();

    while (true) {
        const source_text = editor.getLine("> ") catch |err| switch (err) {
            error.Eof => break,
            else => {
                try stderr.print("Error: {t}\n", .{err});
                try stderr.flush();
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
            error.OutOfMemory => return error.OutOfMemory,
            error.ParseError => {
                const parse_error = diagnostics.errors.items[0];
                const syntax_error = try agent.createErrorObject(
                    .syntax_error,
                    "{f}",
                    .{fmtParseError(parse_error)},
                );
                const exception: Agent.Exception = .{
                    .value = Value.from(&syntax_error.object),
                    .stack_trace = &.{},
                };
                try stderr.print("{f}\n{f}\n", .{
                    fmtParseErrorHint(parse_error, source_text),
                    exception.fmtPretty(),
                });
                try stderr.flush();
                continue;
            },
        };

        if (script.evaluate()) |result| {
            try stdout.print("{f}\n", .{result});
            try stdout.flush();
        } else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ExceptionThrown => {
                const exception = agent.clearException();
                try stderr.print("{f}\n", .{exception.fmtPretty()});
                try stderr.flush();
            },
        }
    }
}

pub fn main() std.os.uefi.Status {
    // By having a main function with a much broader error set we can 'try' away all the errors and
    // then possibly turn them into UEFI-specific status codes here.
    mainWithErrorHandling() catch |err| switch (err) {
        error.OutOfMemory => return .out_of_resources,
        else => {},
    };
    return .success;
}
