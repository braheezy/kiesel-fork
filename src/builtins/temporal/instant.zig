//! 8 Temporal.Instant Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-instant-objects

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

/// 8.2 Properties of the Temporal.Instant Constructor
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-instant-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            1,
            "Instant",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 8.2.1 Temporal.Instant.prototype
        // https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Temporal.Instant.prototype%"()),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = false,
            },
        );
    }

    /// 8.1.1 Temporal.Instant ( epochNanoseconds )
    /// https://tc39.es/proposal-temporal/#sec-temporal.instant
    fn impl(agent: *Agent, arguments: Arguments, maybe_new_target: ?*Object) Agent.Error!Value {
        const epoch_nanoseconds_value = arguments.get(0);

        // 1. If NewTarget is undefined, throw a TypeError exception.
        const new_target = maybe_new_target orelse {
            return agent.throwException(
                .type_error,
                "Temporal.Instant must be constructed with 'new'",
                .{},
            );
        };

        // 2. Let epochNanoseconds be ? ToBigInt(epochNanoseconds).
        const epoch_nanoseconds_bigint = try epoch_nanoseconds_value.toBigInt(agent);
        const epoch_nanoseconds = epoch_nanoseconds_bigint.managed.toInt(i128) catch std.math.maxInt(i128);

        // 3. If IsValidEpochNanoseconds(epochNanoseconds) is false, throw a RangeError exception.
        if (!isValidEpochNanoseconds(epoch_nanoseconds)) {
            return agent.throwException(
                .range_error,
                "Invalid epoch nanoseconds {}",
                .{epoch_nanoseconds_bigint.managed},
            );
        }

        // 4. Return ? CreateTemporalInstant(epochNanoseconds, NewTarget).
        const temporal_rs_instant = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Instant_try_new(temporal_rs.toI128Nanoseconds(epoch_nanoseconds)),
        ) catch unreachable;
        errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
        return Value.from(
            try createTemporalInstant(agent, temporal_rs_instant.?, new_target),
        );
    }
};

/// 8.3 Properties of the Temporal.Instant Prototype Object
/// https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-instant-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 8.3.1 Temporal.Instant.prototype.constructor
        // https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Temporal.Instant%"()),
        );

        // 8.3.2 Temporal.Instant.prototype[ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.Instant"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }
};

/// 8.4 Properties of Temporal.Instant Instances
/// https://tc39.es/proposal-temporal/#sec-properties-of-temporal-instant-instances
pub const Instant = MakeObject(.{
    .Fields = struct {
        // TODO: Add GC finalizer to destroy this
        inner: *temporal_rs.c.Instant,
    },
    .tag = .temporal_instant,
});

/// nsMaxInstant = 10**8 × nsPerDay = 8.64 × 10**21
/// https://tc39.es/proposal-temporal/#eqn-nsMaxInstant
pub const ns_max_instant = 8640000000000000000000;

/// nsPerDay = 10**6 × ℝ(msPerDay) = 8.64 × 10**13
/// https://tc39.es/proposal-temporal/#eqn-nsPerDay
pub const ns_per_day = 86400000000000;

/// nsMinInstant = -nsMaxInstant = -8.64 × 10**21
/// https://tc39.es/proposal-temporal/#eqn-nsMinInstant
pub const ns_min_instant = -8640000000000000000000;

/// 8.5.1 IsValidEpochNanoseconds ( epochNanoseconds )
/// https://tc39.es/proposal-temporal/#sec-temporal-isvalidepochnanoseconds
pub fn isValidEpochNanoseconds(epoch_nanoseconds: i128) bool {
    // 1. If ℝ(epochNanoseconds) < nsMinInstant or ℝ(epochNanoseconds) > nsMaxInstant, then
    if (epoch_nanoseconds < ns_min_instant or epoch_nanoseconds > ns_max_instant) {
        // a. Return false.
        return false;
    }

    // 2. Return true.
    return true;
}

/// 8.5.2 CreateTemporalInstant ( epochNanoseconds [ , newTarget ] )
/// https://tc39.es/proposal-temporal/#sec-temporal-createtemporalinstant
pub fn createTemporalInstant(
    agent: *Agent,
    inner: *temporal_rs.c.Instant,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    const realm = agent.currentRealm();

    // 1. Assert: IsValidEpochNanoseconds(epochNanoseconds) is true.

    // 2. If newTarget is not present, set newTarget to %Temporal.Instant%.
    const new_target = maybe_new_target orelse try realm.intrinsics.@"%Temporal.Instant%"();

    // 3. Let object be ? OrdinaryCreateFromConstructor(newTarget, "%Temporal.Instant.prototype%",
    //    « [[InitializedTemporalInstant]], [[EpochNanoseconds]] »).
    // 4. Set object.[[EpochNanoseconds]] to epochNanoseconds.
    // 5. Return object.
    return ordinaryCreateFromConstructor(
        Instant,
        agent,
        new_target,
        "%Temporal.Instant.prototype%",
        .{ .inner = inner },
    );
}
