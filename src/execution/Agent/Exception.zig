const std = @import("std");

const execution = @import("../../execution.zig");
const pretty_printing = @import("../../pretty_printing.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
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

const FormatPrettyData = struct {
    agent: *Agent,
    exception: Exception,
};

pub fn formatPretty(data: FormatPrettyData, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    const terminal: std.Io.Terminal = .{
        .writer = writer,
        .mode = data.agent.platform.terminal_mode,
    };
    return prettyPrintException(data.agent, data.exception, terminal) catch |err| switch (err) {
        // From `std.Io.Terminal.setColor()`
        error.Canceled, error.Unexpected => {},
        error.WriteFailed => |e| return e,
    };
}

pub fn fmtPretty(exception: Exception, agent: *Agent) std.fmt.Alt(FormatPrettyData, formatPretty) {
    return .{ .data = .{
        .agent = agent,
        .exception = exception,
    } };
}
