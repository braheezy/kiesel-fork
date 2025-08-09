const std = @import("std");

const build_options = @import("build-options");
const execution = @import("../execution.zig");

const Agent = execution.Agent;

pub const c = @cImport({
    if (!build_options.enable_temporal) {
        @compileError("temporal not enabled");
    }
    @cInclude("Calendar.h");
    @cInclude("Duration.h");
    @cInclude("ErrorKind.h");
    @cInclude("I128Nanoseconds.h");
    @cInclude("Instant.h");
    @cInclude("OwnedRelativeTo.h");
    @cInclude("ParsedDate.h");
    @cInclude("ParsedDateTime.h");
    @cInclude("ParsedZonedDateTime.h");
    @cInclude("PlainDate.h");
    @cInclude("PlainDateTime.h");
    @cInclude("PlainMonthDay.h");
    @cInclude("PlainTime.h");
    @cInclude("PlainYearMonth.h");
    @cInclude("RelativeTo.h");
    @cInclude("TimeZone.h");
    @cInclude("ZonedDateTime.h");
});

pub const to_string_rounding_options_auto: c.ToStringRoundingOptions = .{
    .precision = .{ .is_minute = false, .precision = .{ .is_ok = false } },
    .smallest_unit = .{ .is_ok = false },
    .rounding_mode = .{ .is_ok = false },
};

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

/// Convert a Zig slice to a Rust `DiplomatString16View`.
pub fn toDiplomatString16View(s: []const u16) c.DiplomatString16View {
    return .{ .data = s.ptr, .len = s.len };
}

/// Convert a Rust `Option<T>` to a Zig `?T`.
pub fn fromOptional(value: anytype) ?Success(@TypeOf(value)) {
    return success(value);
}

// Wraps values from a `c.RelativeTo` or `c.OwnedRelativeTo`.
pub const RelativeTo = union(enum) {
    none,
    owned_plain_date: *c.PlainDate,
    owned_zoned_date_time: *c.ZonedDateTime,
    borrowed_plain_date: *const c.PlainDate,
    borrowed_zoned_date_time: *const c.ZonedDateTime,

    pub fn fromOwned(owned: c.OwnedRelativeTo) RelativeTo {
        if (owned.date) |plain_date| {
            return .{ .owned_plain_date = plain_date };
        } else if (owned.zoned) |zoned_date_time| {
            return .{ .owned_zoned_date_time = zoned_date_time };
        } else {
            return .none;
        }
    }

    pub fn toRust(self: RelativeTo) c.RelativeTo {
        return switch (self) {
            .none => .{ .date = null, .zoned = null },
            .owned_plain_date => |plain_date| .{ .date = plain_date, .zoned = null },
            .owned_zoned_date_time => |zoned_date_time| .{ .date = null, .zoned = zoned_date_time },
            .borrowed_plain_date => |plain_date| .{ .date = plain_date, .zoned = null },
            .borrowed_zoned_date_time => |zoned_date_time| .{ .date = null, .zoned = zoned_date_time },
        };
    }

    pub fn deinit(self: RelativeTo) void {
        switch (self) {
            .owned_plain_date => |plain_date| c.temporal_rs_PlainDate_destroy(plain_date),
            .owned_zoned_date_time => |zoned_date_time| c.temporal_rs_ZonedDateTime_destroy(zoned_date_time),
            else => {},
        }
    }
};

pub const DiplomatWrite = struct {
    gpa: std.mem.Allocator,
    array_list: std.ArrayListUnmanaged(u8),
    inner: c.DiplomatWrite,

    pub fn init(gpa: std.mem.Allocator) DiplomatWrite {
        return .{
            .gpa = gpa,
            .array_list = .empty,
            .inner = .{
                // NOTE: We use `@fieldParentPtr()` on the `inner` struct field to get to the other
                //       fields instead of creating a context externally and storing a pointer.
                .context = null,
                .buf = undefined,
                .len = 0,
                .cap = 0,
                .grow_failed = false,
                .flush = flush,
                .grow = grow,
            },
        };
    }

    pub fn deinit(self: *DiplomatWrite) void {
        self.array_list.deinit(self.gpa);
    }

    pub fn toOwnedSlice(self: *DiplomatWrite) std.mem.Allocator.Error![]u8 {
        if (self.inner.grow_failed) return error.OutOfMemory;
        self.inner = undefined; // Invalidate the inner struct to prevent further writes
        return self.array_list.toOwnedSlice(self.gpa);
    }

    fn flush(inner: ?*c.DiplomatWrite) callconv(.c) void {
        const self: *DiplomatWrite = @fieldParentPtr("inner", inner.?);
        self.array_list.items.len = inner.?.len;
    }

    fn grow(inner: ?*c.DiplomatWrite, size: usize) callconv(.c) bool {
        const self: *DiplomatWrite = @fieldParentPtr("inner", inner.?);
        self.array_list.ensureTotalCapacity(self.gpa, size) catch return false;
        inner.?.buf = self.array_list.items.ptr;
        inner.?.cap = self.array_list.capacity;
        return true;
    }
};

test DiplomatWrite {
    const gpa = std.testing.allocator;
    var write = DiplomatWrite.init(gpa);
    defer write.deinit();

    const WriteImpl = struct {
        inner: *c.DiplomatWrite,

        // https://github.com/rust-diplomat/diplomat/blob/2b903255187976779798fc89df3fee7298641c80/runtime/src/write.rs#L70-L73
        pub fn flush(self: @This()) void {
            self.inner.flush.?(self.inner);
        }

        // https://github.com/rust-diplomat/diplomat/blob/2b903255187976779798fc89df3fee7298641c80/runtime/src/write.rs#L76-L94
        pub fn writeStr(self: @This(), s: []const u8) void {
            if (self.inner.grow_failed) {
                return;
            }
            const needed_len = self.inner.len + s.len;
            if (needed_len > self.inner.cap) {
                const success_ = self.inner.grow.?(self.inner, needed_len);
                if (!success_) {
                    self.inner.grow_failed = true;
                    return;
                }
            }
            std.debug.assert(needed_len <= self.inner.cap);
            @memcpy(self.inner.buf[self.inner.len..][0..s.len], s);
            self.inner.len = needed_len;
        }
    };

    var write_impl: WriteImpl = .{ .inner = &write.inner };
    write_impl.writeStr("Hello World");
    write_impl.flush();

    try std.testing.expectEqual(write.array_list.items.ptr, write.inner.buf);
    try std.testing.expectEqual(write.array_list.items.len, write.inner.len);
    try std.testing.expectEqual(write.array_list.capacity, write.inner.cap);
    try std.testing.expectEqual(false, write.inner.grow_failed);
    try std.testing.expectEqualSlices(u8, "Hello World", write.array_list.items);

    const slice = try write.toOwnedSlice();
    defer gpa.free(slice);
    try std.testing.expectEqualSlices(u8, "Hello World", slice);
    try std.testing.expectEqualSlices(u8, &.{}, write.array_list.items);
}

pub fn extractResult(agent: *Agent, result: anytype) Agent.Error!Success(@TypeOf(result)) {
    if (success(result)) |x| return x;
    const message = if (fromOptional(result.unnamed_0.err.msg)) |sv|
        fromDiplomatStringView(sv)
    else
        "";
    switch (result.unnamed_0.err.kind) {
        c.ErrorKind_Generic => return agent.throwException(.internal_error, "{s}", .{message}),
        c.ErrorKind_Type => return agent.throwException(.type_error, "{s}", .{message}),
        c.ErrorKind_Range => return agent.throwException(.range_error, "{s}", .{message}),
        c.ErrorKind_Syntax => return agent.throwException(.syntax_error, "{s}", .{message}),
        c.ErrorKind_Assert => @panic("temporal_rs assertion failed"),
        else => unreachable,
    }
}

/// Converts a "result" value to its "success" type, or returns `null` if the value is an error.
/// This is `inline` to prevent binary bloat, because each instantiation is expected to be called
/// only once.
pub inline fn success(result: anytype) ?Success(@TypeOf(result)) {
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
