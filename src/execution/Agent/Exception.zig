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

pub fn format(_: Exception, _: *std.Io.Writer) std.Io.Writer.Error!void {
    @compileError("Plain exception formatting is not implemented, use 'fmtPretty()'");
}

pub fn formatPretty(self: Exception, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    return prettyPrintException(self, writer) catch |err| switch (err) {
        // From `std.Io.tty.Config.setColor()`
        error.Unexpected => {},
        error.WriteFailed => return error.WriteFailed,
    };
}

pub fn fmtPretty(self: Exception) std.fmt.Alt(Exception, formatPretty) {
    return .{ .data = self };
}
