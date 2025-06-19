//! 7 Temporal.Duration Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-duration-objects

const std = @import("std");

const temporal_rs = @import("../../c/temporal_rs.zig");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 7.2 Properties of the Temporal.Duration Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-duration-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "Duration",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 7.2.1 Temporal.Duration.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.Duration.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 7.1.1 Temporal.Duration ( [ years [ , months [ , weeks [ , days [ , hours [ , minutes [ , seconds [ , milliseconds [ , microseconds [ , nanoseconds ] ] ] ] ] ] ] ] ] ] )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const years_value = arguments.get(0);
        const months_value = arguments.get(1);
        const weeks_value = arguments.get(2);
        const days_value = arguments.get(3);
        const hours_value = arguments.get(4);
        const minutes_value = arguments.get(5);
        const seconds_value = arguments.get(6);
        const milliseconds_value = arguments.get(7);
        const microseconds_value = arguments.get(8);
        const nanoseconds_value = arguments.get(9);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.Duration must be constructed with 'new'",
                .{},
            );
        };

        // 2. If years is undefined, let y be 0; else let y be ? ToIntegerIfIntegral(years).
        const years = if (years_value.isUndefined()) 0 else try years_value.toIntegerIfIntegral(agent);

        // 3. If months is undefined, let mo be 0; else let mo be ? ToIntegerIfIntegral(months).
        const months = if (months_value.isUndefined()) 0 else try months_value.toIntegerIfIntegral(agent);

        // 4. If weeks is undefined, let w be 0; else let w be ? ToIntegerIfIntegral(weeks).
        const weeks = if (weeks_value.isUndefined()) 0 else try weeks_value.toIntegerIfIntegral(agent);

        // 5. If days is undefined, let d be 0; else let d be ? ToIntegerIfIntegral(days).
        const days = if (days_value.isUndefined()) 0 else try days_value.toIntegerIfIntegral(agent);

        // 6. If hours is undefined, let h be 0; else let h be ? ToIntegerIfIntegral(hours).
        const hours = if (hours_value.isUndefined()) 0 else try hours_value.toIntegerIfIntegral(agent);

        // 7. If minutes is undefined, let m be 0; else let m be ? ToIntegerIfIntegral(minutes).
        const minutes = if (minutes_value.isUndefined()) 0 else try minutes_value.toIntegerIfIntegral(agent);

        // 8. If seconds is undefined, let s be 0; else let s be ? ToIntegerIfIntegral(seconds).
        const seconds = if (seconds_value.isUndefined()) 0 else try seconds_value.toIntegerIfIntegral(agent);

        // 9. If milliseconds is undefined, let ms be 0; else let ms be ? ToIntegerIfIntegral(milliseconds).
        const milliseconds = if (milliseconds_value.isUndefined()) 0 else try milliseconds_value.toIntegerIfIntegral(agent);

        // 10. If microseconds is undefined, let mis be 0; else let mis be ? ToIntegerIfIntegral(microseconds).
        const microseconds = if (microseconds_value.isUndefined()) 0 else try microseconds_value.toIntegerIfIntegral(agent);

        // 11. If nanoseconds is undefined, let ns be 0; else let ns be ? ToIntegerIfIntegral(nanoseconds).
        const nanoseconds = if (nanoseconds_value.isUndefined()) 0 else try nanoseconds_value.toIntegerIfIntegral(agent);

        // 12. Return ? CreateTemporalDuration(y, mo, w, d, h, m, s, ms, mis, ns, NewTarget).
        const temporal_rs_duration = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Duration_try_new(
                std.math.lossyCast(i64, years),
                std.math.lossyCast(i64, months),
                std.math.lossyCast(i64, weeks),
                std.math.lossyCast(i64, days),
                std.math.lossyCast(i64, hours),
                std.math.lossyCast(i64, minutes),
                std.math.lossyCast(i64, seconds),
                std.math.lossyCast(i64, milliseconds),
                microseconds,
                nanoseconds,
            ),
        ) catch |err| switch (err) {
            error.RangeError => return agent.throwException(.range_error, "Invalid duration", .{}),
            else => unreachable,
        };
        errdefer temporal_rs.c.temporal_rs_Duration_destroy(temporal_rs_duration.?);
        return Value.from(
            try createTemporalDuration(agent, temporal_rs_duration.?, new_target),
        );
    }
};

/// 7.3 Properties of the Temporal.Duration Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-duration-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);

        // 7.3.1 Temporal.Duration.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.Duration%"()),
        );

        // 7.3.2 Temporal.Duration.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.Duration"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 7.3.25 Temporal.Duration.prototype.valueOf ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.valueof
    fn valueOf(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Cannot convert Temporal.Duration to primitive value",
            .{},
        );
    }
};

/// 7.4 Properties of Temporal.Duration Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-duration-instances
pub const Duration = MakeObject(.{
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.Duration,
    },
    .tag = .temporal_duration,
});

/// 7.5.19 CreateTemporalDuration ( years, months, weeks, days, hours, minutes, seconds, milliseconds, microseconds, nanoseconds [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalduration
pub fn createTemporalDuration(
    agent: *Agent,
    inner: *temporal_rs.c.Duration,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. If IsValidDuration(years, months, weeks, days, hours, minutes, seconds, milliseconds,
    //    microseconds, nanoseconds) is false, throw a RangeError exception.

    // 2. If newTarget is not present, set newTarget to %Temporal.Duration%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.Duration%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.Duration.prototype%",
    //    « [[InitializedTemporalDuration]], [[Years]], [[Months]], [[Weeks]], [[Days]], [[Hours]],
    //    [[Minutes]], [[Seconds]], [[Milliseconds]], [[Microseconds]], [[Nanoseconds]] »).
    // 4. Set object.[[Years]] to ℝ(𝔽(years)).
    // 5. Set object.[[Months]] to ℝ(𝔽(months)).
    // 6. Set object.[[Weeks]] to ℝ(𝔽(weeks)).
    // 7. Set object.[[Days]] to ℝ(𝔽(days)).
    // 8. Set object.[[Hours]] to ℝ(𝔽(hours)).
    // 9. Set object.[[Minutes]] to ℝ(𝔽(minutes)).
    // 10. Set object.[[Seconds]] to ℝ(𝔽(seconds)).
    // 11. Set object.[[Milliseconds]] to ℝ(𝔽(milliseconds)).
    // 12. Set object.[[Microseconds]] to ℝ(𝔽(microseconds)).
    // 13. Set object.[[Nanoseconds]] to ℝ(𝔽(nanoseconds)).
    // 14. Return object.
    return ordinaryCreateFromConstructor(
        Duration,
        agent,
        new_target,
        "%Temporal.Duration.prototype%",
        .{ .inner = inner },
    );
}
