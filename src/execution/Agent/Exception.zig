const builtin = @import("builtin");
const std = @import("std");

const execution = @import("../../execution.zig");
const pretty_printing = @import("../../pretty_printing.zig");
const types = @import("../../types.zig");

const ExecutionContext = execution.ExecutionContext;
const Value = types.Value;
const prettyPrintException = pretty_printing.prettyPrintException;

const Exception = @This();

value: Value,
stack_trace: StackTrace,

pub const StackTrace = []const StackFrame;

pub const StackFrame = struct {
    origin: ExecutionContext.Origin,
};

pub fn format(
    self: Exception,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = options;
    if (comptime std.mem.eql(u8, fmt, "pretty")) {
        return prettyPrintException(self, writer) catch |err| {
            // NOTE: When targeting Windows the error set contains error.Unexpected (from the
            //       `std.io.tty.Config.setColor()` calls), which `std.fmt.formatType()`
            //       doesn't include in its error set.
            if (builtin.os.tag == .windows) switch (err) {
                error.Unexpected => {},
                else => |e| return e,
            } else return err;
        };
    }
    @compileError("Exception formatting without {pretty} is not implemented");
}
