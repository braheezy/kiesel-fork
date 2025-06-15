//! 2 The Temporal.Now Object
//! https://tc39.es/proposal-temporal/#sec-temporal-now-object

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const Object = types.Object;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;

pub const namespace = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
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

    /// 2.2.1 Temporal.Now.timeZoneId ( )
    /// https://tc39.es/proposal-temporal/#sec-temporal.now.timezoneid
    fn timeZoneId(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        // 1. Return SystemTimeZoneIdentifier().
        return Value.from(try String.fromAscii(agent, builtins.systemTimeZoneIdentifier()));
    }
};
