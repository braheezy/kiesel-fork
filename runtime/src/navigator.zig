//! 8.9.1 The Navigator object
//! https://html.spec.whatwg.org/#the-navigator-object

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

pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Navigator {
    return Navigator.create(agent, .{
        .prototype = try prototype.create(agent, realm),
    });
}

pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        try object.defineBuiltinAccessorWithAttributes(agent, "userAgent", userAgent, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });

        return object;
    }

    /// https://html.spec.whatwg.org/#dom-navigator-useragent
    fn userAgent(_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        return Value.from(std.fmt.comptimePrint("Kiesel/{f}", .{kiesel.version}));
    }
};

pub const Navigator = MakeObject(.{
    .tag = @enumFromInt(@intFromEnum(Tag.navigator)),
    .display_name = "Navigator",
});
