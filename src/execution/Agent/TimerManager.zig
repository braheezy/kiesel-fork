const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Realm = execution.Realm;
const Value = types.Value;
const Number = types.Number;

const NS_PER_MILLISECOND = 1_000_000;
const MAX_DELAY_MS: f64 = 0x7fffffff;
const MIN_NESTED_DELAY_MS: f64 = 4.0;

const TimerRecord = struct {
    handle: u64,
    realm: *Realm,
    callback: Value,
    this_value: Value,
    arguments: []Value,
    next_fire_time_ns: u128,
    interval_ns: ?u128,
    canceled: bool,
    pending_fire: bool,
};

pub const Record = TimerRecord;

pub const TimerManager = struct {
    allocator: std.mem.Allocator,
    timers: std.ArrayList(TimerRecord),
    nested_level: u32,
    next_handle: u64,

    pub fn init(allocator: std.mem.Allocator) !TimerManager {
        return .{
            .allocator = allocator,
            .timers = try std.ArrayList(TimerRecord).initCapacity(allocator, 0),
            .nested_level = 0,
            .next_handle = 1,
        };
    }

    pub fn deinit(self: *TimerManager) void {
        for (self.timers.items) |record| {
            self.freeArguments(record.arguments);
        }
        self.timers.deinit(self.allocator);
    }

    pub fn hasTimers(self: *TimerManager) bool {
        return self.timers.items.len != 0;
    }

    pub fn nextTimerFireTime(self: *TimerManager) ?u128 {
        var min: ?u128 = null;
        for (self.timers.items) |record| {
            if (record.canceled or record.pending_fire) continue;
            if (min) |current| {
                if (record.next_fire_time_ns < current) min = record.next_fire_time_ns;
            } else {
                min = record.next_fire_time_ns;
            }
        }
        return min;
    }

    pub fn collectDueHandles(
        self: *TimerManager,
        now_ns: u128,
        handles: *std.ArrayList(u64),
    ) std.mem.Allocator.Error!void {
        var idx: usize = 0;
        for (self.timers.items) |record| {
            if (record.canceled or record.pending_fire) {
                idx += 1;
                continue;
            }
            if (record.next_fire_time_ns <= now_ns) {
                var record_ptr = &self.timers.items[idx];
                record_ptr.pending_fire = true;
                try handles.append(self.allocator, record_ptr.handle);
            }
            idx += 1;
        }
    }

    pub fn getRecord(self: *TimerManager, handle: u64) ?*TimerRecord {
        const index = self.find(handle);
        if (index) |idx| {
            return &self.timers.items[idx];
        }
        return null;
    }

    pub fn schedule(
        self: *TimerManager,
        callback: Value,
        this_value: Value,
        realm: *Realm,
        arguments: []const Value,
        delay_ns: u128,
        interval_ns: ?u128,
        current_time_ns: u128,
    ) std.mem.Allocator.Error!u64 {
        var storage = try self.allocator.alloc(Value, arguments.len);
        const args_slice = storage[0..arguments.len];
        std.mem.copyForwards(Value, args_slice, arguments);

        const handle = self.next_handle;
        self.next_handle += 1;

        const record = TimerRecord{
            .handle = handle,
            .realm = realm,
            .callback = callback,
            .this_value = this_value,
            .arguments = args_slice,
            .next_fire_time_ns = current_time_ns + delay_ns,
            .interval_ns = interval_ns,
            .canceled = false,
            .pending_fire = false,
        };
        try self.timers.append(self.allocator, record);
        return handle;
    }

    pub fn clear(self: *TimerManager, handle: u64) bool {
        const idx = self.find(handle) orelse return false;
        var record = &self.timers.items[idx];
        if (record.pending_fire) {
            record.canceled = true;
            return true;
        }
        self.removeAt(idx);
        return true;
    }

    pub fn clampDelay(self: *TimerManager, number: Number) u128 {
        var delay_ms = number.asFloat();
        if (number.isNan() or number.isNegativeInf()) {
            delay_ms = 0;
        } else if (number.isPositiveInf()) {
            delay_ms = MAX_DELAY_MS;
        } else if (delay_ms < 0) {
            delay_ms = 0;
        }

        if (self.nested_level >= 5 and delay_ms < MIN_NESTED_DELAY_MS) {
            delay_ms = MIN_NESTED_DELAY_MS;
        }
        if (delay_ms > MAX_DELAY_MS) {
            delay_ms = MAX_DELAY_MS;
        }

        const truncated = std.math.trunc(delay_ms);
        const ms: u64 = @intFromFloat(truncated);
        const ms_u128: u128 = @intCast(ms);
        return ms_u128 * NS_PER_MILLISECOND;
    }

    pub fn finalize(self: *TimerManager, handle: u64, now_ns: u128) void {
        const idx = self.find(handle) orelse return;
        var record = &self.timers.items[idx];
        record.pending_fire = false;
        if (record.canceled) {
            self.removeAt(idx);
            return;
        }

        if (record.interval_ns) |interval| {
            record.next_fire_time_ns = now_ns + interval;
            return;
        }

        self.removeAt(idx);
    }

    fn find(self: *TimerManager, handle: u64) ?usize {
        var idx: usize = 0;
        for (self.timers.items) |record| {
            if (record.handle == handle) return idx;
            idx += 1;
        }
        return null;
    }

    fn removeAt(self: *TimerManager, idx: usize) void {
        const record = self.timers.items[idx];
        self.freeArguments(record.arguments);
        _ = self.timers.orderedRemove(idx);
    }

    fn freeArguments(self: *TimerManager, args: []Value) void {
        if (args.len != 0) {
            self.allocator.free(args);
        }
    }
};
