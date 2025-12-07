//! 7. The Performance interface
//! https://w3c.github.io/hr-time/#sec-performance

const builtin = @import("builtin");
const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const MakeObject = kiesel.types.MakeObject;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const Value = kiesel.types.Value;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;

const Tag = @import("tag.zig").Tag;

/// https://w3c.github.io/hr-time/#dfn-coarsen-time
fn coarsenTime(time: f64) f64 {
    // 1. Let time resolution be 100 microseconds, or a higher implementation-defined value.
    // 2. If crossOriginIsolatedCapability is true, set time resolution to be 5 microseconds, or a
    //    higher implementation-defined value.
    // 3. In an implementation-defined manner, coarsen and potentially jitter timestamp such that
    //    its resolution will not exceed time resolution.
    // 4. Return timestamp as a moment.
    return @round(time * 1_000) / 1_000;
}

/// https://w3c.github.io/hr-time/#dfn-relative-high-resolution-time
fn relativeHighResolutionTime(time: f64, performance: *const Performance) f64 {
    // 1. Let coarse time be the result of calling coarsen time with time and global's relevant
    //    settings object's cross-origin isolated capability.
    const coarse_time = coarsenTime(time);

    // 2. Return the relative high resolution coarse time for coarse time and global.
    return relativeHighResolutionCoarseTime(coarse_time, performance);
}

/// https://w3c.github.io/hr-time/#dfn-relative-high-resolution-coarse-time
fn relativeHighResolutionCoarseTime(coarse_time: f64, performance: *const Performance) f64 {
    // The relative high resolution coarse time given a moment from the monotonic clock coarseTime
    // and a global object global, is the duration from global's relevant settings object's time
    // origin to coarseTime.
    return coarse_time - performance.fields.time_origin;
}

/// https://w3c.github.io/hr-time/#dfn-current-high-resolution-time
fn currentHighResolutionTime(performance: *const Performance) f64 {
    // The current high resolution time given a global object current global must return the result
    // of relative high resolution time given unsafe shared current time and current global.
    return relativeHighResolutionTime(unsafeSharedCurrentTime(), performance);
}

/// https://w3c.github.io/hr-time/#dfn-coarsened-shared-current-time
fn coarsenedSharedCurrentTime() f64 {
    // The coarsened shared current time given an optional boolean crossOriginIsolatedCapability
    // (default false), must return the result of calling coarsen time with the unsafe shared
    // current time and crossOriginIsolatedCapability.
    return coarsenTime(unsafeSharedCurrentTime());
}

/// https://w3c.github.io/hr-time/#dfn-unsafe-shared-current-time
fn unsafeSharedCurrentTime() f64 {
    if (builtin.target.os.tag == .freestanding) return 0;
    // The unsafe shared current time must return the unsafe current time of the monotonic clock.
    const Timestamp = @FieldType(std.time.Instant, "timestamp");
    const epoch: Timestamp = if (Timestamp == u64) 0 else .{ .sec = 0, .nsec = 0 };
    const now = std.time.Instant.now() catch return 0;
    const ns = now.since(.{ .timestamp = epoch });
    return @as(f64, @floatFromInt(ns)) / std.time.ns_per_ms;
}

pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Performance {
    return Performance.create(agent, .{
        .prototype = try prototype.create(agent, realm),
        .fields = .{
            .time_origin = coarsenedSharedCurrentTime(),
        },
    });
}

pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        try object.defineBuiltinFunctionWithAttributes(agent, "now", now, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinAccessorWithAttributes(agent, "timeOrigin", timeOrigin, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });

        return object;
    }

    /// 7.1 now() method
    /// https://w3c.github.io/hr-time/#dom-performance-now
    fn now(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The now() method MUST return the number of milliseconds in the current high resolution
        // time given this's relevant global object (a duration).
        const performance = try this_value.requireInternalSlot(agent, Performance);
        return Value.from(currentHighResolutionTime(performance));
    }

    /// 7.2 timeOrigin attribute
    /// https://w3c.github.io/hr-time/#timeorigin-attribute
    fn timeOrigin(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The timeOrigin attribute MUST return the number of milliseconds in the duration returned
        // by get time origin timestamp for the relevant global object of this.
        const performance = try this_value.requireInternalSlot(agent, Performance);
        return Value.from(performance.fields.time_origin);
    }
};

pub const Performance = MakeObject(.{
    .Fields = struct {
        time_origin: f64,
    },
    .tag = @enumFromInt(@intFromEnum(Tag.performance)),
    .display_name = "Performance",
});
