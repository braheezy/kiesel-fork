//! 1 The Temporal Object
//! https://tc39.es/proposal-temporal/#sec-temporal-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;

comptime {
    const build_options = @import("build-options");
    if (!build_options.enable_temporal) @compileError("Temporal is not enabled");
}

pub const duration = @import("./temporal/duration.zig");
pub const instant = @import("./temporal/instant.zig");
pub const now = @import("./temporal/now.zig");
pub const plain_date = @import("./temporal/plain_date.zig");
pub const plain_date_time = @import("./temporal/plain_date_time.zig");
pub const plain_month_day = @import("./temporal/plain_month_day.zig");
pub const plain_time = @import("./temporal/plain_time.zig");
pub const plain_year_month = @import("./temporal/plain_year_month.zig");
pub const zoned_date_time = @import("./temporal/zoned_date_time.zig");

pub const Duration = duration.Duration;
pub const Instant = instant.Instant;
pub const PlainDate = plain_date.PlainDate;
pub const PlainDateTime = plain_date_time.PlainDateTime;
pub const PlainMonthDay = plain_month_day.PlainMonthDay;
pub const PlainTime = plain_time.PlainTime;
pub const PlainYearMonth = plain_year_month.PlainYearMonth;
pub const ZonedDateTime = zoned_date_time.ZonedDateTime;

pub const namespace = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 1.1.1 Temporal [ %Symbol.toStringTag% ]
        // https://tc39.es/proposal-temporal/#sec-temporal-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Temporal"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 1.2.7 Temporal.Duration ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-duration
        try object.defineBuiltinProperty(
            agent,
            "Duration",
            Value.from(try realm.intrinsics.@"%Temporal.Duration%"()),
        );

        // 1.2.1 Temporal.Instant ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-instant
        try object.defineBuiltinProperty(
            agent,
            "Instant",
            Value.from(try realm.intrinsics.@"%Temporal.Instant%"()),
        );

        // 1.2.3 Temporal.PlainDate ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plaindate
        try object.defineBuiltinProperty(
            agent,
            "PlainDate",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDate%"()),
        );

        // 1.2.2 Temporal.PlainDateTime ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plaindatetime
        try object.defineBuiltinProperty(
            agent,
            "PlainDateTime",
            Value.from(try realm.intrinsics.@"%Temporal.PlainDateTime%"()),
        );

        // 1.2.6 Temporal.PlainMonthDay ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plainmonthday
        try object.defineBuiltinProperty(
            agent,
            "PlainMonthDay",
            Value.from(try realm.intrinsics.@"%Temporal.PlainMonthDay%"()),
        );

        // 1.2.4 Temporal.PlainTime ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plaintime
        try object.defineBuiltinProperty(
            agent,
            "PlainTime",
            Value.from(try realm.intrinsics.@"%Temporal.PlainTime%"()),
        );

        // 1.2.5 Temporal.PlainYearMonth ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-plainyearmonth
        try object.defineBuiltinProperty(
            agent,
            "PlainYearMonth",
            Value.from(try realm.intrinsics.@"%Temporal.PlainYearMonth%"()),
        );

        // 1.2.8 Temporal.ZonedDateTime ( . . . )
        // https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime
        try object.defineBuiltinProperty(
            agent,
            "ZonedDateTime",
            Value.from(try realm.intrinsics.@"%Temporal.ZonedDateTime%"()),
        );

        // 1.3.1 Temporal.Now
        // https://tc39.es/proposal-temporal/#sec-temporal-now
        try object.defineBuiltinProperty(
            agent,
            "Now",
            Value.from(try realm.intrinsics.@"%Temporal.Now%"()),
        );
    }
};
