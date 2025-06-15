//! 8 Temporal.Instant Objects
//! https://tc39.es/proposal-temporal/#sec-temporal-instant-objects

const std = @import("std");

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
    fn impl(agent: *Agent, _: Arguments, _: ?*Object) Agent.Error!Value {
        return agent.throwException(.internal_error, "Not implemented", .{});
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
    .tag = .temporal_instant,
});
