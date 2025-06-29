const std = @import("std");

const build_options = @import("build-options");

pub const c = @cImport({
    if (!build_options.enable_temporal) {
        @compileError("temporal not enabled");
    }
    @cInclude("Calendar.h");
    @cInclude("Duration.h");
    @cInclude("ErrorKind.h");
    @cInclude("I128Nanoseconds.h");
    @cInclude("Instant.h");
    @cInclude("PlainDate.h");
    @cInclude("PlainDateTime.h");
    @cInclude("PlainMonthDay.h");
    @cInclude("PlainTime.h");
    @cInclude("PlainYearMonth.h");
    @cInclude("TimeZone.h");
    @cInclude("ZonedDateTime.h");
});

const u64_high_bit_mask: u64 = 1 << 63;

/// Covert a Rust `I128Nanoseconds` struct to a Zig `i128`.
///
/// Ported from temporal_rs's [`From<ffi::I128Nanoseconds>`](https://github.com/boa-dev/temporal/blob/89bfca1f5b918d00a19354664e1da11da51305ee/temporal_capi/src/instant.rs#L172-L186) trait for `i128`.
pub fn fromI128Nanoseconds(ns: c.I128Nanoseconds) i128 {
    const is_neg = (ns.high & u64_high_bit_mask) != 0;
    const ns_high: u128 = @intCast((ns.high & ~u64_high_bit_mask));
    const total: i128 = @intCast((ns_high << 64) + ns.low);
    return if (is_neg) -total else total;
}

/// Covert a Zig `i128` to a Rust `I128Nanoseconds` struct.
///
/// Ported from temporal_rs's [`From<i128>`](https://github.com/boa-dev/temporal/blob/89bfca1f5b918d00a19354664e1da11da51305ee/temporal_capi/src/instant.rs#L188-L207) trait for `ffi::I128Nanoseconds`.
pub fn toI128Nanoseconds(ns: i128) c.I128Nanoseconds {
    std.debug.assert(ns != std.math.minInt(i128));
    const is_neg = ns < 0;
    const ns_abs = @abs(ns);
    const high: u64 = @intCast(ns_abs >> 64);
    const low: u64 = @truncate(ns_abs);
    return .{ .high = if (is_neg) high | u64_high_bit_mask else high, .low = low };
}

/// Convert a Rust `DiplomatStringView` to a Zig slice.
pub fn fromDiplomatStringView(sv: c.DiplomatStringView) []const u8 {
    return sv.data[0..sv.len];
}

/// Convert a Zig slice to a Rust `DiplomatStringView`.
pub fn toDiplomatStringView(s: []const u8) c.DiplomatStringView {
    return .{ .data = s.ptr, .len = s.len };
}

// Convert a Rust `Option<T>` to a Zig `?T`.
pub fn fromOptional(value: anytype) ?Success(@TypeOf(value)) {
    return success(value);
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
