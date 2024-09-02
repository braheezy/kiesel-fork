//! 10.4.6 Module Namespace Exotic Objects
//! https://tc39.es/ecma262/#sec-module-namespace-exotic-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const immutable_prototype = @import("immutable_prototype.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const MakeObject = types.MakeObject;
const Module = language.Module;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;
const containsSlice = utils.containsSlice;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const getModuleNamespace = language.getModuleNamespace;
const noexcept = utils.noexcept;
const ordinaryDefineOwnProperty = builtins.ordinaryDefineOwnProperty;
const ordinaryDelete = builtins.ordinaryDelete;
const ordinaryGet = builtins.ordinaryGet;
const ordinaryGetOwnProperty = builtins.ordinaryGetOwnProperty;
const ordinaryHasProperty = builtins.ordinaryHasProperty;
const ordinaryOwnPropertyKeys = builtins.ordinaryOwnPropertyKeys;
const sameValue = types.sameValue;
const setImmutablePrototype = immutable_prototype.setImmutablePrototype;

/// 10.4.6.1 [[GetPrototypeOf]] ( )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-getprototypeof
fn getPrototypeOf(_: Object) error{}!?Object {
    // 1. Return null.
    return null;
}

/// 10.4.6.2 [[SetPrototypeOf]] ( V )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-setprototypeof-v
fn setPrototypeOf(object: Object, prototype: ?Object) std.mem.Allocator.Error!bool {
    // 1. Return ! SetImmutablePrototype(O, V).
    return setImmutablePrototype(object, prototype) catch |err| try noexcept(err);
}

/// 10.4.6.3 [[IsExtensible]] ( )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-isextensible
fn isExtensible(_: Object) error{}!bool {
    // 1. Return false.
    return false;
}

/// 10.4.6.4 [[PreventExtensions]] ( )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-preventextensions
fn preventExtensions(_: Object) error{}!bool {
    // 1. Return true.
    return true;
}

/// 10.4.6.5 [[GetOwnProperty]] ( P )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-getownproperty-p
fn getOwnProperty(object: Object, property_key: PropertyKey) Agent.Error!?PropertyDescriptor {
    const agent = object.agent();

    // 1. If P is a Symbol, return OrdinaryGetOwnProperty(O, P).
    if (property_key == .symbol) return ordinaryGetOwnProperty(object, property_key);

    // 2. Let exports be O.[[Exports]].
    const exports = object.as(ModuleNamespace).fields.exports;

    const property_key_string = try (try property_key.toStringOrSymbol(agent)).string.toUtf8(agent.gc_allocator);

    // 3. If exports does not contain P, return undefined.
    if (!containsSlice(exports, property_key_string)) {
        return null;
    }

    // 4. Let value be ? O.[[Get]](P, O).
    const value = try object.internalMethods().get(object, property_key, Value.from(object));

    // 5. Return PropertyDescriptor {
    //      [[Value]]: value, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: false
    //    }.
    return .{ .value = value, .writable = true, .enumerable = true, .configurable = false };
}

/// 10.4.6.6 [[DefineOwnProperty]] ( P, Desc )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-defineownproperty-p-desc
fn defineOwnProperty(
    object: Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!bool {
    // 1. If P is a Symbol, return ! OrdinaryDefineOwnProperty(O, P, Desc).
    if (property_key == .symbol) {
        return ordinaryDefineOwnProperty(object, property_key, property_descriptor);
    }

    // 2. Let current be ? O.[[GetOwnProperty]](P).
    const current = try object.internalMethods().getOwnProperty(object, property_key);

    // 3. If current is undefined, return false.
    if (current == null) return false;

    // 4. If Desc has a [[Configurable]] field and Desc.[[Configurable]] is true, return false.
    if (property_descriptor.configurable == true) return false;

    // 5. If Desc has an [[Enumerable]] field and Desc.[[Enumerable]] is false, return false.
    if (property_descriptor.enumerable == false) return false;

    // 6. If IsAccessorDescriptor(Desc) is true, return false.
    if (property_descriptor.isAccessorDescriptor()) return false;

    // 7. If Desc has a [[Writable]] field and Desc.[[Writable]] is false, return false.
    if (property_descriptor.writable == false) return false;

    // 8. If Desc has a [[Value]] field, return SameValue(Desc.[[Value]], current.[[Value]]).
    if (property_descriptor.value) |value| return sameValue(value, current.?.value.?);

    // 9. Return true.
    return true;
}

/// 10.4.6.7 [[HasProperty]] ( P )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-hasproperty-p
fn hasProperty(object: Object, property_key: PropertyKey) std.mem.Allocator.Error!bool {
    const agent = object.agent();

    // 1. If P is a Symbol, return ! OrdinaryHasProperty(O, P).
    if (property_key == .symbol) {
        return ordinaryHasProperty(object, property_key) catch |err| try noexcept(err);
    }

    // 2. Let exports be O.[[Exports]].
    const exports = object.as(ModuleNamespace).fields.exports;

    const property_key_string = try (try property_key.toStringOrSymbol(agent)).string.toUtf8(agent.gc_allocator);

    // 3. If exports contains P, return true.
    if (containsSlice(exports, property_key_string)) {
        return true;
    }

    // 4. Return false.
    return false;
}

/// 10.4.6.8 [[Get]] ( P, Receiver )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-get-p-receiver
fn get(object: Object, property_key: PropertyKey, receiver: Value) Agent.Error!Value {
    const agent = object.agent();

    // 1. If P is a Symbol, then
    if (property_key == .symbol) {
        // a. Return ! OrdinaryGet(O, P, Receiver).
        return ordinaryGet(object, property_key, receiver) catch |err| try noexcept(err);
    }

    // 2. Let exports be O.[[Exports]].
    const exports = object.as(ModuleNamespace).fields.exports;

    const property_key_string = try (try property_key.toStringOrSymbol(agent)).string.toUtf8(agent.gc_allocator);

    // 3. If exports does not contain P, return undefined.
    if (!containsSlice(exports, property_key_string)) {
        return .undefined;
    }

    // 4. Let m be O.[[Module]].
    const module: Module = object.as(ModuleNamespace).fields.module;

    // 5. Let binding be m.ResolveExport(P).
    // 6. Assert: binding is a ResolvedBinding Record.
    const binding = (try module.resolveExport(agent, property_key_string)).?.resolved_binding;

    // 7. Let targetModule be binding.[[Module]].
    // 8. Assert: targetModule is not undefined.
    const target_module = binding.module;

    // 9. If binding.[[BindingName]] is namespace, then
    if (binding.binding_name == .namespace) {
        // a. Return GetModuleNamespace(targetModule).
        return Value.from(try getModuleNamespace(agent, target_module));
    }

    // 10. Let targetEnv be targetModule.[[Environment]].
    const target_env = switch (target_module) {
        .source_text_module => |source_text_module| source_text_module.environment,
    } orelse {
        // 11. If targetEnv is empty, throw a ReferenceError exception.
        return agent.throwException(.reference_error, "Module is not linked", .{});
    };

    // 12. Return ? targetEnv.GetBindingValue(binding.[[BindingName]], true).
    const binding_name = try String.fromUtf8(agent.gc_allocator, binding.binding_name.string);
    return target_env.getBindingValue(agent, binding_name, true);
}

/// 10.4.6.9 [[Set]] ( P, V, Receiver )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-set-p-v-receiver
fn set(_: Object, _: PropertyKey, _: Value, _: Value) error{}!bool {
    // 1. Return false.
    return false;
}

/// 10.4.6.10 [[Delete]] ( P )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-delete-p
fn delete(object: Object, property_key: PropertyKey) std.mem.Allocator.Error!bool {
    const agent = object.agent();

    // 1. If P is a Symbol, then
    if (property_key == .symbol) {
        // a. Return ! OrdinaryDelete(O, P).
        return ordinaryDelete(object, property_key) catch |err| try noexcept(err);
    }

    // 2. Let exports be O.[[Exports]].
    const exports = object.as(ModuleNamespace).fields.exports;

    const property_key_string = try (try property_key.toStringOrSymbol(agent)).string.toUtf8(agent.gc_allocator);

    // 3. If exports contains P, return false.
    if (containsSlice(exports, property_key_string)) {
        return false;
    }

    // 4. Return true.
    return true;
}

/// 10.4.6.11 [[OwnPropertyKeys]] ( )
/// https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-ownpropertykeys
fn ownPropertyKeys(object: Object) std.mem.Allocator.Error!std.ArrayList(PropertyKey) {
    const agent = object.agent();

    // 1. Let exports be O.[[Exports]].
    const exports = object.as(ModuleNamespace).fields.exports;

    // 2. Let symbolKeys be OrdinaryOwnPropertyKeys(O).
    // 3. Return the list-concatenation of exports and symbolKeys.
    var keys = try ordinaryOwnPropertyKeys(object);
    try keys.ensureUnusedCapacity(exports.len);
    for (exports) |name| {
        keys.appendAssumeCapacity(
            PropertyKey.from(try String.fromUtf8(agent.gc_allocator, name)),
        );
    }
    return keys;
}

/// 10.4.6.12 ModuleNamespaceCreate ( module, exports )
/// https://tc39.es/ecma262/#sec-modulenamespacecreate
pub fn moduleNamespaceCreate(
    agent: *Agent,
    module: Module,
    exports: []const []const u8,
) std.mem.Allocator.Error!Object {
    // 1. Assert: module.[[Namespace]] is empty.
    switch (module) {
        .source_text_module => |source_text_module| std.debug.assert(source_text_module.namespace == null),
    }

    // 6. Let sortedExports be a List whose elements are the elements of exports, sorted according
    //    to lexicographic code unit order.
    var sorted_exports = try std.ArrayList([]const u8).initCapacity(
        agent.gc_allocator,
        exports.len,
    );
    sorted_exports.appendSliceAssumeCapacity(exports);
    std.mem.sortUnstable([]const u8, sorted_exports.items, {}, struct {
        fn lessThanFn(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThanFn);

    // 2. Let internalSlotsList be the internal slots listed in Table 33.
    // 3. Let M be MakeBasicObject(internalSlotsList).
    const namespace = try ModuleNamespace.create(agent, .{
        .prototype = null,

        // 4. Set M's essential internal methods to the definitions specified in 10.4.6.
        .internal_methods = &.{
            .getPrototypeOf = getPrototypeOf,
            .setPrototypeOf = setPrototypeOf,
            .isExtensible = isExtensible,
            .preventExtensions = preventExtensions,
            .getOwnProperty = getOwnProperty,
            .defineOwnProperty = defineOwnProperty,
            .hasProperty = hasProperty,
            .get = get,
            .set = set,
            .delete = delete,
            .ownPropertyKeys = ownPropertyKeys,
        },

        .fields = .{
            // 5. Set M.[[Module]] to module.
            .module = module,

            // 7. Set M.[[Exports]] to sortedExports.
            .exports = try sorted_exports.toOwnedSlice(),
        },
    });

    // 8. Create own properties of M corresponding to the definitions in 28.3.
    {
        // 28.3.1 %Symbol.toStringTag%
        // https://tc39.es/ecma262/#sec-%symbol.tostringtag%
        try defineBuiltinProperty(namespace, "%Symbol.toStringTag%", PropertyDescriptor{
            .value = Value.from("Module"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    // 9. Set module.[[Namespace]] to M.
    module.source_text_module.namespace = namespace;

    // 10. Return M.
    return namespace;
}

/// Table 33: Internal Slots of Module Namespace Exotic Objects
/// https://tc39.es/ecma262/#table-internal-slots-of-module-namespace-exotic-objects
pub const ModuleNamespace = MakeObject(.{
    .Fields = struct {
        /// [[Module]]
        module: Module,

        // [[Exports]]
        exports: []const []const u8,
    },
});
