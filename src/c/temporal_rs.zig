const std = @import("std");

const build_options = @import("build-options");

pub const c = @cImport({
    if (!build_options.enable_temporal) {
        @compileError("temporal not enabled");
    }
    @cInclude("Calendar.h");
    @cInclude("Duration.h");
    @cInclude("ErrorKind.h");
    @cInclude("Instant.h");
    @cInclude("PlainDate.h");
    @cInclude("PlainDateTime.h");
    @cInclude("PlainMonthDay.h");
    @cInclude("PlainTime.h");
    @cInclude("PlainYearMonth.h");
    @cInclude("TimeZone.h");
    @cInclude("ZonedDateTime.h");
});

// https://github.com/boa-dev/temporal/blob/ad46374b7f5394a0641d3195f357d0d39109a93b/temporal_capi/src/instant.rs#L161-L174
pub fn fromI128Nanoseconds(ns: c.I128Nanoseconds) i128 {
    // TODO: This was ported verbatim from temporal_rs but is broken for negative values.
    // See: https://github.com/boa-dev/temporal/issues/352
    const is_neg = ns.high < 0;
    const ns_high_abs: u128 = @intCast(@abs(ns.high));
    const total: i128 = @intCast((ns_high_abs << 64) + ns.low);
    return if (is_neg) -total else total;
}

// https://github.com/boa-dev/temporal/blob/ad46374b7f5394a0641d3195f357d0d39109a93b/temporal_capi/src/instant.rs#L176-L187
pub fn toI128Nanoseconds(ns: i128) c.I128Nanoseconds {
    // TODO: This was ported verbatim from temporal_rs but is broken for negative values.
    // See: https://github.com/boa-dev/temporal/issues/352
    const is_neg = ns < 0;
    const ns_abs = @abs(ns);
    const high: i64 = @intCast(ns_abs >> 64);
    const low: u64 = @truncate(ns_abs);
    return .{ .high = if (is_neg) -high else high, .low = low };
}

pub fn fromDiplomatStringView(sv: c.DiplomatStringView) []const u8 {
    return sv.data[0..sv.len];
}

pub const DiplomatWrite = struct {
    pub const Context = struct {
        gpa: std.mem.Allocator,
        array_list: std.ArrayListUnmanaged(u8) = .empty,
    };

    inner: c.DiplomatWrite,

    pub fn init(context: *Context) DiplomatWrite {
        return .{
            .inner = .{
                .context = context,
                .buf = undefined,
                .len = 0,
                .cap = 0,
                .grow_failed = false,
                .flush = flush,
                .grow = grow,
            },
        };
    }

    pub fn deinit(self: *const DiplomatWrite) void {
        const context: *Context = @alignCast(@ptrCast(self.inner.context.?));
        context.array_list.deinit(context.gpa);
    }

    pub fn toOwnedSlice(self: *const DiplomatWrite) std.mem.Allocator.Error![]u8 {
        if (self.inner.grow_failed) return error.OutOfMemory;
        const context: *Context = @alignCast(@ptrCast(self.inner.context.?));
        return context.array_list.toOwnedSlice(context.gpa);
    }

    fn flush(inner: ?*c.DiplomatWrite) callconv(.c) void {
        const context: *Context = @alignCast(@ptrCast(inner.?.context.?));
        context.array_list.items.len = inner.?.len;
    }

    fn grow(inner: ?*c.DiplomatWrite, size: usize) callconv(.c) bool {
        const context: *Context = @alignCast(@ptrCast(inner.?.context.?));
        context.array_list.ensureTotalCapacity(context.gpa, size) catch return false;
        inner.?.buf = context.array_list.items.ptr;
        inner.?.cap = context.array_list.capacity;
        return true;
    }
};

// https://github.com/boa-dev/temporal/blob/main/temporal_capi/src/error.rs
pub const TemporalError = error{
    GenericError,
    TypeError,
    RangeError,
    SyntaxError,
};

/// Convert a Rust `Result<TemporalError, T>` to a Zig `TemporalError!T`.
pub fn temporalErrorResult(result: anytype) TemporalError!Success(@TypeOf(result)) {
    if (success(result)) |x| return x;
    return switch (result.unnamed_0.err.kind) {
        c.ErrorKind_Generic => error.GenericError,
        c.ErrorKind_Type => error.TypeError,
        c.ErrorKind_Range => error.RangeError,
        c.ErrorKind_Syntax => error.SyntaxError,
        c.ErrorKind_Assert => @panic("temporal_rs assertion reached"),
        else => unreachable,
    };
}

/// Converts a "result" value to its "success" type, or returns `null` if the value is an error.
/// This is `inline` to prevent binary bloat, because each instantiation is expected to be called
/// only once.
inline fn success(result: anytype) ?Success(@TypeOf(result)) {
    if (!result.is_ok) return null;
    if (Success(@TypeOf(result)) == void) return;
    return result.unnamed_0.ok;
}

/// Given the C API representation of a `Result<T, E>`, returns the type 'T'.
fn Success(comptime Result: type) type {
    const Union = @FieldType(Result, "unnamed_0");
    if (!@hasField(Union, "ok")) return void;
    return @FieldType(Union, "ok");
}
