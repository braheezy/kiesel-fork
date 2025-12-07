//! 5.5. Response class
//! https://fetch.spec.whatwg.org/#response-class

const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const MakeObject = kiesel.types.MakeObject;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const String = kiesel.types.String;
const Value = kiesel.types.Value;
const createBuiltinFunction = kiesel.builtins.createBuiltinFunction;
const newPromiseCapability = kiesel.builtins.newPromiseCapability;
const noexcept = kiesel.utils.noexcept;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;
const parseJSON = kiesel.builtins.parseJSON;

const Tag = @import("../tag.zig").Tag;

pub var response_prototype: *Object = undefined;

pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "Response",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );

        response_prototype = try prototype.create(agent, realm);
        try builtin_function.object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(response_prototype),
            .none,
        );
        try response_prototype.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(&builtin_function.object),
        );

        return &builtin_function.object;
    }

    fn impl(agent: *Agent, _: Arguments, _: ?*Object) Agent.Error!Value {
        return agent.throwException(.internal_error, "Not implemented", .{});
    }
};

pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const object = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );

        try object.defineBuiltinAccessorWithAttributes(agent, "status", status, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinAccessorWithAttributes(agent, "ok", ok, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinAccessorWithAttributes(agent, "statusText", statusText, null, realm, .{
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinFunctionWithAttributes(agent, "json", json, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });
        try object.defineBuiltinFunctionWithAttributes(agent, "text", text, 0, realm, .{
            .writable = true,
            .enumerable = true,
            .configurable = true,
        });

        return object;
    }

    /// https://fetch.spec.whatwg.org/#dom-response-status
    fn status(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The status getter steps are to return this’s response’s status.
        const response = try this_value.requireInternalSlot(agent, Response);
        return Value.from(@intFromEnum(response.fields.status));
    }

    /// https://fetch.spec.whatwg.org/#dom-response-ok
    fn ok(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The ok getter steps are to return true if this’s response’s status is an ok status;
        // otherwise false.
        const response = try this_value.requireInternalSlot(agent, Response);
        return Value.from(response.fields.status.class() == .success);
    }

    /// https://fetch.spec.whatwg.org/#dom-response-statustext
    fn statusText(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The statusText getter steps are to return this’s response’s status message.
        const response = try this_value.requireInternalSlot(agent, Response);
        return Value.from(
            try String.fromUtf8(agent, response.fields.reason),
        );
    }

    /// https://fetch.spec.whatwg.org/#dom-response-json
    fn json(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The json() method steps are to return the result of running consume body with this and
        // parse JSON from bytes.
        const realm = agent.currentRealm();
        const response = try this_value.requireInternalSlot(agent, Response);
        const utf8 = try std.fmt.allocPrint(
            agent.gc_allocator,
            "{f}",
            .{std.unicode.fmtUtf8(response.fields.body)},
        );
        const string = try String.fromUtf8(agent, utf8);
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);
        if (parseJSON(agent, string)) |result| {
            _ = try Value.from(promise_capability.resolve).callAssumeCallable(
                agent,
                .undefined,
                &.{result},
            );
        } else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ExceptionThrown => {
                const exception = agent.clearException();
                _ = try Value.from(promise_capability.reject).callAssumeCallable(
                    agent,
                    .undefined,
                    &.{exception.value},
                );
            },
        }
        return Value.from(promise_capability.promise);
    }

    /// https://fetch.spec.whatwg.org/#dom-body-text
    fn text(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // The text() method steps are to return the result of running consume body with this and
        // UTF-8 decode.
        const realm = agent.currentRealm();
        const response = try this_value.requireInternalSlot(agent, Response);
        const utf8 = try std.fmt.allocPrint(
            agent.gc_allocator,
            "{f}",
            .{std.unicode.fmtUtf8(response.fields.body)},
        );
        const string = try String.fromUtf8(agent, utf8);
        const promise_capability = newPromiseCapability(
            agent,
            Value.from(try realm.intrinsics.@"%Promise%"()),
        ) catch |err| try noexcept(err);
        _ = try Value.from(promise_capability.resolve).callAssumeCallable(
            agent,
            .undefined,
            &.{Value.from(string)},
        );
        return Value.from(promise_capability.promise);
    }
};

pub const Response = MakeObject(.{
    .Fields = struct {
        status: std.http.Status,
        reason: []const u8,
        body: []const u8,
    },
    .tag = @enumFromInt(@intFromEnum(Tag.response)),
    .display_name = "Response",
});
