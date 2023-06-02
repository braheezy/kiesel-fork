//! 20.2 Function Objects
//! https://tc39.es/ecma262/#sec-function-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const BuiltinFunction = builtins.BuiltinFunction;
const ECMAScriptFunction = builtins.ECMAScriptFunction;
const Object = types.Object;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;

/// 20.2.3 Properties of the Function Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-function-prototype-object
pub const FunctionPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createNoinit(realm);
        init(realm, object);
        return object;
    }

    pub fn createNoinit(realm: *Realm) !Object {
        return createBuiltinFunction(realm.agent, .{ .regular = behaviour }, .{
            .length = 0,
            .name = "",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) !void {
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
    }

    fn behaviour(_: *Agent, _: Value, _: ArgumentsList) !Value {
        return .undefined;
    }

    /// 20.2.3.5 Function.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-function.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let func be the this value.
        const func = this_value;

        // // 2. If func is an Object, func has a [[SourceText]] internal slot, func.[[SourceText]] is
        // //    a sequence of Unicode code points, and HostHasSourceTextAvailable(func) is true, then
        if (func == .object and func.object.is(ECMAScriptFunction)) {
            const ecmascript_function = func.object.as(ECMAScriptFunction);

            // a. Return CodePointsToString(func.[[SourceText]]).
            return Value.from(ecmascript_function.fields.source_text);
        }

        // 3. If func is a built-in function object, return an implementation-defined String source
        //    code representation of func. The representation must have the syntax of a
        //    NativeFunction. Additionally, if func has an [[InitialName]] internal slot and
        //    func.[[InitialName]] is a String, the portion of the returned String that would be
        //    matched by NativeFunctionAccessoropt PropertyName must be the value of
        //    func.[[InitialName]].
        if (func == .object and func.object.is(BuiltinFunction)) {
            const builtin_function = func.object.as(BuiltinFunction);
            const name = builtin_function.fields.initial_name orelse "";
            const source_text = try std.fmt.allocPrint(
                agent.gc_allocator,
                "function {s}() {{ [native code] }}",
                .{name},
            );
            return Value.from(source_text);
        }

        // 4. If func is an Object and IsCallable(func) is true, return an implementation-defined
        //    String source code representation of func. The representation must have the syntax of
        //    a NativeFunction.
        if (func.isCallable()) return Value.from("function () { [native code] }");

        // 5. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not a function", .{func}),
        );
    }
};
