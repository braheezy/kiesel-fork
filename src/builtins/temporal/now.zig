//! 2 The Temporal.Now Object
//! https://tc39.es/proposal-temporal/#sec-temporal-now-object

const std = @import("std");

const temporal_rs = @import("../../c/temporal_rs.zig");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createTemporalInstant = builtins.createTemporalInstant;

pub const namespace = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "instant", instant, 0, realm);
        try object.defineBuiltinFunction(agent, "timeZoneId", timeZoneId, 0, realm);

        // 2.1.1 Temporal.Now [ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal-now-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal.Now"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 2.2.2 Temporal.Now.instant ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.now.instant
    fn instant(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Let ns be SystemUTCEpochNanoseconds().
        const ns = systemUTCEpochNanoseconds(agent);

        // 2. Return ! CreateTemporalInstant(ns).
        const temporal_rs_instant = temporal_rs.temporalErrorResult(
            temporal_rs.c.temporal_rs_Instant_try_new(ns),
        ) catch unreachable;
        errdefer temporal_rs.c.temporal_rs_Instant_destroy(temporal_rs_instant.?);
        return Value.from(
            try createTemporalInstant(agent, temporal_rs_instant.?, null),
        );
    }

    /// 2.2.1 Temporal.Now.timeZoneId ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.now.timezoneid
    fn timeZoneId(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Return SystemTimeZoneIdentifier().
        return Value.from(try String.fromAscii(agent, builtins.systemTimeZoneIdentifier()));
    }
};

/// 2.3.3 SystemUTCEpochNanoseconds ( )
/// https://tc39.es/proposal-temporal/#sec-temporal-systemutcepochnanoseconds
pub fn systemUTCEpochNanoseconds(agent: *Agent) temporal_rs.c.I128Nanoseconds {
    // NOTE: The host implementation can get the global object itself if needed, passing the agent
    //       is enough.
    // 1. Let global be GetGlobalObject().
    // 2. Let nowNs be HostSystemUTCEpochNanoseconds(global).
    const now_ns = agent.host_hooks.hostSystemUTCEpochNanoseconds(agent);

    // 3. Return ℤ(nowNs).
    return temporal_rs.toI128Nanoseconds(now_ns);
}
