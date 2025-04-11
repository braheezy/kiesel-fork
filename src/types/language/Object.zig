//! 6.1.7 The Object Type
//! https://tc39.es/ecma262/#sec-object-type

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const spec = @import("../spec.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const ClassConstructorFields = builtins.builtin_function.ClassConstructorFields;
const ClassFieldDefinition = types.ClassFieldDefinition;
const Number = types.Number;
const PreferredType = Value.PreferredType;
const PrivateElement = types.PrivateElement;
const PrivateName = types.PrivateName;
const PropertyDescriptor = spec.PropertyDescriptor;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createArrayFromList = types.createArrayFromList;
const noexcept = utils.noexcept;
const sameValue = types.sameValue;
const validateNonRevokedProxy = builtins.validateNonRevokedProxy;

pub const IndexedProperties = @import("Object/IndexedProperties.zig");
pub const InternalMethods = @import("Object/InternalMethods.zig");
pub const PropertyKey = @import("Object/PropertyKey.zig").PropertyKey;
pub const PropertyStorage = @import("Object/PropertyStorage.zig");
pub const Shape = @import("Object/Shape.zig");

const Object = @This();

pub const Tag = enum(u32) {
    unset,

    // ECMA-262
    arguments,
    array,
    array_buffer,
    array_iterator,
    async_from_sync_iterator,
    async_generator,
    big_int,
    boolean,
    bound_function,
    builtin_function,
    data_view,
    date,
    ecmascript_function,
    @"error",
    finalization_registry,
    for_in_iterator,
    generator,
    iterator,
    iterator_helper,
    map,
    map_iterator,
    module_namespace,
    number,
    promise,
    proxy,
    reg_exp,
    reg_exp_string_iterator,
    set,
    set_iterator,
    shared_array_buffer,
    string,
    string_iterator,
    symbol,
    typed_array,
    weak_map,
    weak_ref,
    weak_set,
    wrap_for_valid_iterator,

    // ECMA-402
    intl_collator,
    intl_date_time_format,
    intl_display_names,
    intl_duration_format,
    intl_list_format,
    intl_locale,
    intl_plural_rules,
    intl_segmenter,
    intl_segments,
    intl_segment_iterator,

    // Custom
    _,
};

pub const IntegrityLevel = enum {
    sealed,
    frozen,
};

pub const PropertyKind = enum {
    key,
    value,
    @"key+value",
};

tag: Object.Tag,
agent: *Agent,
internal_methods: *const InternalMethods,
property_storage: PropertyStorage,

pub fn format(
    self: *const Object,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = self;
    _ = fmt;
    _ = options;
    // TODO: Print the actual object type.
    try writer.writeAll("[object Object]");
}

pub fn is(self: *const Object, comptime T: type) bool {
    comptime std.debug.assert(T.tag != .unset);
    return self.tag == T.tag;
}

pub fn as(self: *const Object, comptime T: type) *T {
    std.debug.assert(self.is(T));
    // Casting alignment is safe because we allocate objects as *T
    return @constCast(@alignCast(@fieldParentPtr("object", self)));
}

pub fn prototype(self: *const Object) ?*Object {
    return self.property_storage.shape.prototype;
}

pub fn setPrototype(self: *Object, new_prototype: ?*Object) std.mem.Allocator.Error!void {
    if (self.prototype() == new_prototype) return;
    self.property_storage.shape = try self.property_storage.shape.setPrototype(self.agent.gc_allocator, new_prototype);
}

pub fn extensible(self: *const Object) bool {
    return self.property_storage.shape.extensible;
}

pub fn setNonExtensible(self: *Object) std.mem.Allocator.Error!void {
    if (!self.extensible()) return;
    self.property_storage.shape = try self.property_storage.shape.setNonExtensible(self.agent.gc_allocator);
}

pub fn isHTMLDDA(self: *const Object) bool {
    return self.property_storage.shape.is_htmldda;
}

pub fn setIsHTMLDDA(self: *Object) std.mem.Allocator.Error!void {
    if (self.isHTMLDDA()) return;
    self.property_storage.shape = try self.property_storage.shape.setIsHTMLDDA(self.agent.gc_allocator);
}

/// Assumes the property exists, is a data property, and not lazy.
pub fn getPropertyValueDirect(self: *const Object, property_key: PropertyKey) Value {
    if (property_key.isArrayIndex()) {
        const index: u32 = @intCast(property_key.integer_index);
        return switch (self.property_storage.indexed_properties.storage) {
            .none => unreachable,
            .dense_i32 => |dense_i32| Value.from(dense_i32.items[index]),
            .dense_f64 => |dense_f64| Value.from(dense_f64.items[index]),
            .dense_value => |dense_value| dense_value.items[index],
            .sparse => |sparse| sparse.get(index).?.value_or_accessor.value,
        };
    }
    const property_metadata = self.property_storage.shape.properties.get(property_key).?;
    std.debug.assert(!self.property_storage.lazy_properties.contains(property_key));
    return switch (property_metadata.index) {
        .value => |index| self.property_storage.values.items[@intFromEnum(index)],
        .accessor => unreachable,
    };
}

/// Fast version of `createDataPropertyOrThrow()` that assumes the property does not exist yet or
/// looking it up is free of side effects. This allows us to bypass `[[DefineOwnProperty]]` and
/// thus `[[GetOwnProperty]]` and `[[IsExtensible]]`.
pub fn createDataPropertyDirect(
    self: *Object,
    property_key: PropertyKey,
    value: Value,
) std.mem.Allocator.Error!void {
    const agent = self.agent;
    if (self.internal_methods.defineOwnProperty == &builtins.ordinary.internal_methods.defineOwnProperty and
        self.internal_methods.getOwnProperty == &builtins.ordinary.internal_methods.getOwnProperty and
        self.internal_methods.isExtensible == &builtins.ordinary.internal_methods.isExtensible)
    {
        // Go directly to the property storage.
        try self.property_storage.set(agent.gc_allocator, property_key, .{
            .value_or_accessor = .{
                .value = value,
            },
            .attributes = .all,
        });
    } else {
        // Non-ordinary objects like arrays need to go through `[[DefineOwnProperty]]` to set the length.
        const result = self.internal_methods.defineOwnProperty(agent, self, property_key, .{
            .value = value,
            .writable = true,
            .enumerable = true,
            .configurable = true,
        }) catch |err| try noexcept(err);
        std.debug.assert(result);
    }
}

/// Fast version of `definePropertyOrThrow()` that assumes the property does not exist yet or
/// looking it up is free of side effects. This allows us to bypass `[[DefineOwnProperty]]` and
/// thus `[[GetOwnProperty]]` and `[[IsExtensible]]`.
pub fn definePropertyDirect(
    self: *Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) std.mem.Allocator.Error!void {
    const agent = self.agent;
    if (self.internal_methods.defineOwnProperty == &builtins.ordinary.internal_methods.defineOwnProperty and
        self.internal_methods.getOwnProperty == &builtins.ordinary.internal_methods.getOwnProperty and
        self.internal_methods.isExtensible == &builtins.ordinary.internal_methods.isExtensible)
    {
        try self.property_storage.set(
            agent.gc_allocator,
            property_key,
            .fromPropertyDescriptor(property_descriptor),
        );
    } else {
        const result = self.internal_methods.defineOwnProperty(
            agent,
            self,
            property_key,
            property_descriptor,
        ) catch |err| try noexcept(err);
        std.debug.assert(result);
    }
}

/// 7.1.1.1 OrdinaryToPrimitive ( O, hint )
/// https://tc39.es/ecma262/#sec-ordinarytoprimitive
pub fn ordinaryToPrimitive(self: *Object, hint: PreferredType) Agent.Error!Value {
    const method_names = switch (hint) {
        // 1. If hint is string, then
        //     a. Let methodNames be « "toString", "valueOf" ».
        .string => [_]PropertyKey{ PropertyKey.from("toString"), PropertyKey.from("valueOf") },
        // 2. Else,
        //     a. Let methodNames be « "valueOf", "toString" ».
        else => [_]PropertyKey{ PropertyKey.from("valueOf"), PropertyKey.from("toString") },
    };

    // 3. For each element name of methodNames, do
    for (method_names) |name| {
        // a. Let method be ? Get(O, name).
        const method = try self.get(name);

        // b. If IsCallable(method) is true, then
        if (method.isCallable()) {
            // i. Let result be ? Call(method, O).
            const result = try method.callAssumeCallable(self.agent, Value.from(self), &.{});

            // ii. If result is not an Object, return result.
            if (!result.isObject()) return result;
        }
    }

    // 4. Throw a TypeError exception.
    return self.agent.throwException(
        .type_error,
        "Could not convert object to {s}",
        .{@tagName(hint)},
    );
}

/// 7.2.5 IsExtensible ( O )
/// https://tc39.es/ecma262/#sec-isextensible-o
pub fn isExtensible(self: *Object) Agent.Error!bool {
    // 1. Return ? O.[[IsExtensible]]().
    return self.internal_methods.isExtensible(self.agent, self);
}

/// 7.3.2 Get ( O, P )
/// https://tc39.es/ecma262/#sec-get-o-p
pub fn get(self: *Object, property_key: PropertyKey) Agent.Error!Value {
    // 1. Return ? O.[[Get]](P, O).
    return self.internal_methods.get(self.agent, self, property_key, Value.from(self));
}

/// 7.3.4 Set ( O, P, V, Throw )
/// https://tc39.es/ecma262/#sec-set-o-p-v-throw
pub fn set(
    self: *Object,
    property_key: PropertyKey,
    value: Value,
    throw: enum { throw, ignore },
) Agent.Error!void {
    // 1. Let success be ? O.[[Set]](P, V, O).
    const success = try self.internal_methods.set(
        self.agent,
        self,
        property_key,
        value,
        Value.from(self),
    );

    // 2. If success is false and Throw is true, throw a TypeError exception.
    if (!success and throw == .throw)
        return self.agent.throwException(.type_error, "Could not set property", .{});

    // 3. Return unused.
}

/// 7.3.5 CreateDataProperty ( O, P, V )
/// https://tc39.es/ecma262/#sec-createdataproperty
pub fn createDataProperty(self: *Object, property_key: PropertyKey, value: Value) Agent.Error!bool {
    // 1. Let newDesc be the PropertyDescriptor {
    //      [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true
    //    }.
    const new_descriptor: PropertyDescriptor = .{
        .value = value,
        .writable = true,
        .enumerable = true,
        .configurable = true,
    };

    // 2. Return ? O.[[DefineOwnProperty]](P, newDesc).
    return self.internal_methods.defineOwnProperty(self.agent, self, property_key, new_descriptor);
}

/// 7.3.6 CreateDataPropertyOrThrow ( O, P, V )
/// https://tc39.es/ecma262/#sec-createdatapropertyorthrow
pub fn createDataPropertyOrThrow(
    self: *Object,
    property_key: PropertyKey,
    value: Value,
) Agent.Error!void {
    // 1. Let success be ? CreateDataProperty(O, P, V).
    const success = try self.createDataProperty(property_key, value);

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return self.agent.throwException(.type_error, "Could not create data property", .{});

    // 3. Return unused.
}

/// 7.3.7 CreateNonEnumerableDataPropertyOrThrow ( O, P, V )
/// https://tc39.es/ecma262/#sec-createnonenumerabledatapropertyorthrow
pub fn createNonEnumerableDataPropertyOrThrow(
    self: *Object,
    property_key: PropertyKey,
    value: Value,
) Agent.Error!void {
    // 1. Assert: O is an ordinary, extensible object with no non-configurable properties.
    std.debug.assert(
        self.extensible() and for (self.property_storage.shape.properties.values()) |entry| {
            if (!entry.attributes.configurable) break false;
        } else true and switch (self.property_storage.indexed_properties.storage) {
            .sparse => |sparse| blk: {
                var it = sparse.valueIterator();
                break :blk while (it.next()) |entry| {
                    if (!entry.attributes.configurable) break false;
                } else true;
            },
            else => true,
        },
    );

    // 2. Let newDesc be the PropertyDescriptor {
    //      [[Value]]: V, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true
    //    }.
    const new_descriptor: PropertyDescriptor = .{
        .value = value,
        .writable = true,
        .enumerable = false,
        .configurable = true,
    };

    // 3. Perform ! DefinePropertyOrThrow(O, P, newDesc).
    try self.definePropertyDirect(property_key, new_descriptor);

    // 4. Return unused.
}

/// 7.3.8 DefinePropertyOrThrow ( O, P, desc )
/// https://tc39.es/ecma262/#sec-definepropertyorthrow
pub fn definePropertyOrThrow(
    self: *Object,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!void {
    // 1. Let success be ? O.[[DefineOwnProperty]](P, desc).
    const success = try self.internal_methods.defineOwnProperty(
        self.agent,
        self,
        property_key,
        property_descriptor,
    );

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return self.agent.throwException(.type_error, "Could not define property", .{});

    // 3. Return unused.
}

/// 7.3.9 DeletePropertyOrThrow ( O, P )
/// https://tc39.es/ecma262/#sec-deletepropertyorthrow
pub fn deletePropertyOrThrow(self: *Object, property_key: PropertyKey) Agent.Error!void {
    // 1. Let success be ? O.[[Delete]](P).
    const success = try self.internal_methods.delete(self.agent, self, property_key);

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return self.agent.throwException(.type_error, "Could not delete property", .{});

    // 3. Return unused.
}

/// 7.3.11 HasProperty ( O, P )
/// https://tc39.es/ecma262/#sec-hasproperty
pub fn hasProperty(self: *Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. Return ? O.[[HasProperty]](P).
    return self.internal_methods.hasProperty(self.agent, self, property_key);
}

/// 7.3.12 HasOwnProperty ( O, P )
/// https://tc39.es/ecma262/#sec-hasownproperty
pub fn hasOwnProperty(self: *Object, property_key: PropertyKey) Agent.Error!bool {
    // 1. Let desc be ? O.[[GetOwnProperty]](P).
    const descriptor = try self.internal_methods.getOwnProperty(self.agent, self, property_key);

    // 2. If desc is undefined, return false.
    // 3. Return true.
    return descriptor != null;
}

/// 7.3.14 Construct ( F [ , argumentsList [ , newTarget ] ] )
/// https://tc39.es/ecma262/#sec-construct
pub fn construct(
    self: *Object,
    arguments_list: []const Value,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    // 1. If newTarget is not present, set newTarget to F.
    const new_target = maybe_new_target orelse self;

    // 2. If argumentsList is not present, set argumentsList to a new empty List.

    // 3. Return ? F.[[Construct]](argumentsList, newTarget).
    return self.internal_methods.construct.?(self.agent, self, Arguments.from(arguments_list), new_target);
}

/// 7.3.15 SetIntegrityLevel ( O, level )
/// https://tc39.es/ecma262/#sec-setintegritylevel
pub fn setIntegrityLevel(self: *Object, level: IntegrityLevel) Agent.Error!bool {
    // 1. Let status be ? O.[[PreventExtensions]]().
    const status = try self.internal_methods.preventExtensions(self.agent, self);

    // 2. If status is false, return false.
    if (!status) return false;

    // 3. Let keys be ? O.[[OwnPropertyKeys]]().
    var keys = try self.internal_methods.ownPropertyKeys(self.agent, self);
    defer keys.deinit(self.agent.gc_allocator);

    switch (level) {
        // 4. If level is sealed,
        .sealed => {
            // a. For each element k of keys, do
            for (keys.items) |property_key| {
                // i. Perform ? DefinePropertyOrThrow(O, k, PropertyDescriptor { [[Configurable]]: false }).
                try self.definePropertyOrThrow(property_key, .{ .configurable = false });
            }
        },

        // 5. Else,
        .frozen => {
            // a. Assert: level is frozen.

            // b. For each element k of keys, do
            for (keys.items) |property_key| {
                // i. Let currentDesc be ? O.[[GetOwnProperty]](k).
                const maybe_current_descriptor = try self.internal_methods.getOwnProperty(
                    self.agent,
                    self,
                    property_key,
                );

                // ii. If currentDesc is not undefined, then
                if (maybe_current_descriptor) |current_descriptor| {
                    var descriptor: PropertyDescriptor = undefined;

                    // 1. If IsAccessorDescriptor(currentDesc) is true, then
                    if (current_descriptor.isAccessorDescriptor()) {
                        // a. Let desc be the PropertyDescriptor { [[Configurable]]: false }.
                        descriptor = .{ .configurable = false };
                    } else {
                        // 2. Else,
                        // a. Let desc be the PropertyDescriptor {
                        //      [[Configurable]]: false, [[Writable]]: false
                        //    }.
                        descriptor = .{ .configurable = false, .writable = false };
                    }

                    // 3. Perform ? DefinePropertyOrThrow(O, k, desc).
                    try self.definePropertyOrThrow(property_key, descriptor);
                }
            }
        },
    }

    // 6. Return true.
    return true;
}

/// 7.3.16 TestIntegrityLevel ( O, level )
/// https://tc39.es/ecma262/#sec-testintegritylevel
pub fn testIntegrityLevel(self: *Object, level: IntegrityLevel) Agent.Error!bool {
    // 1. Let extensible be ? IsExtensible(O).
    const extensible_ = try self.isExtensible();

    // 2. If extensible is true, return false.
    // 3. NOTE: If the object is extensible, none of its properties are examined.
    if (extensible_) return false;

    // 4. Let keys be ? O.[[OwnPropertyKeys]]().
    var keys = try self.internal_methods.ownPropertyKeys(self.agent, self);
    defer keys.deinit(self.agent.gc_allocator);

    // 5. For each element k of keys, do
    for (keys.items) |property_key| {
        // a. Let currentDesc be ? O.[[GetOwnProperty]](k).
        const maybe_current_descriptor = try self.internal_methods.getOwnProperty(
            self.agent,
            self,
            property_key,
        );

        // b. If currentDesc is not undefined, then
        if (maybe_current_descriptor) |current_descriptor| {
            // i. If currentDesc.[[Configurable]] is true, return false.
            if (current_descriptor.configurable.?) return false;

            // ii. If level is frozen and IsDataDescriptor(currentDesc) is true, then
            if (level == .frozen and current_descriptor.isDataDescriptor()) {
                // 1. If currentDesc.[[Writable]] is true, return false.
                if (current_descriptor.writable.?) return false;
            }
        }
    }

    // 6. Return true.
    return true;
}

/// 7.3.18 LengthOfArrayLike ( obj )
/// https://tc39.es/ecma262/#sec-lengthofarraylike
pub fn lengthOfArrayLike(self: *Object) Agent.Error!u53 {
    // 1. Return ℝ(? ToLength(? Get(obj, "length"))).
    return (try self.get(PropertyKey.from("length"))).toLength(self.agent);
}

/// 7.3.22 SpeciesConstructor ( O, defaultConstructor )
/// https://tc39.es/ecma262/#sec-speciesconstructor
pub fn speciesConstructor(self: *Object, default_constructor: *Object) Agent.Error!*Object {
    // 1. Let C be ? Get(O, "constructor").
    const constructor = try self.get(PropertyKey.from("constructor"));

    // 2. If C is undefined, return defaultConstructor.
    if (constructor.isUndefined()) return default_constructor;

    // 3. If C is not an Object, throw a TypeError exception.
    if (!constructor.isObject()) {
        return self.agent.throwException(.type_error, "{} is not an Object", .{constructor});
    }

    // 4. Let S be ? Get(C, %Symbol.species%).
    const species = try constructor.asObject().get(
        PropertyKey.from(self.agent.well_known_symbols.@"%Symbol.species%"),
    );

    // 5. If S is either undefined or null, return defaultConstructor.
    if (species.isUndefined() or species.isNull()) return default_constructor;

    // 6. If IsConstructor(S) is true, return S.
    if (species.isConstructor()) return species.asObject();

    // 7. Throw a TypeError exception.
    return self.agent.throwException(
        .type_error,
        "Object's [Symbol.species] property must be a constructor",
        .{},
    );
}

/// 7.3.23 EnumerableOwnProperties ( O, kind )
/// https://tc39.es/ecma262/#sec-enumerableownproperties
pub fn enumerableOwnProperties(
    self: *Object,
    comptime kind: PropertyKind,
) Agent.Error!std.ArrayListUnmanaged(Value) {
    // 1. Let ownKeys be ? O.[[OwnPropertyKeys]]().
    var own_keys = try self.internal_methods.ownPropertyKeys(self.agent, self);
    defer own_keys.deinit(self.agent.gc_allocator);

    // 2. Let results be a new empty List.
    var results: std.ArrayListUnmanaged(Value) = .empty;

    // 3. For each element key of ownKeys, do
    for (own_keys.items) |key| {
        // a. If key is a String, then
        if (key == .string or key == .integer_index) {
            // i. Let desc be ? O.[[GetOwnProperty]](key).
            const descriptor = try self.internal_methods.getOwnProperty(self.agent, self, key);

            // ii. If desc is not undefined and desc.[[Enumerable]] is true, then
            if (descriptor != null and descriptor.?.enumerable == true) {
                // 1. If kind is key, then
                if (kind == .key) {
                    // a. Append key to results.
                    try results.append(self.agent.gc_allocator, try key.toValue(self.agent));
                } else {
                    // 2. Else,
                    // a. Let value be ? Get(O, key).
                    const value = try self.get(key);

                    // b. If kind is value, then
                    if (kind == .value) {
                        // i. Append value to results.
                        try results.append(self.agent.gc_allocator, value);
                    } else {
                        // c. Else,
                        // i. Assert: kind is key+value.
                        std.debug.assert(kind == .@"key+value");

                        // ii. Let entry be CreateArrayFromList(« key, value »).
                        const entry = Value.from(try createArrayFromList(
                            self.agent,
                            &.{ try key.toValue(self.agent), value },
                        ));

                        // iii. Append entry to results.
                        try results.append(self.agent.gc_allocator, entry);
                    }
                }
            }
        }
    }

    // 4. Return results.
    return results;
}

/// 7.3.24 GetFunctionRealm ( obj )
/// https://tc39.es/ecma262/#sec-getfunctionrealm
pub fn getFunctionRealm(self: *const Object, agent: *Agent) error{ExceptionThrown}!*Realm {
    // 1. If obj has a [[Realm]] internal slot, then
    if (self.internal_methods.call != null) {
        // a. Return obj.[[Realm]].
        if (self.is(builtins.BuiltinFunction)) {
            return self.as(builtins.BuiltinFunction).fields.realm;
        } else if (self.is(builtins.ECMAScriptFunction)) {
            return self.as(builtins.ECMAScriptFunction).fields.realm;
        } else if (!(self.is(builtins.BoundFunction) or self.is(builtins.Proxy))) {
            @panic("Unhandled function type in getFunctionRealm()");
        }
    }

    // 2. If obj is a bound function exotic object, then
    if (self.is(builtins.BoundFunction)) {
        // a. Let boundTargetFunction be obj.[[BoundTargetFunction]].
        const bound_target_function = self.as(builtins.BoundFunction).fields.bound_target_function;

        // b. Return ? GetFunctionRealm(boundTargetFunction).
        return bound_target_function.getFunctionRealm(agent);
    }

    // 3. If obj is a Proxy exotic object, then
    if (self.is(builtins.Proxy)) {
        // a. Perform ? ValidateNonRevokedProxy(obj).
        try validateNonRevokedProxy(agent, self.as(builtins.Proxy));

        // b. Let proxyTarget be obj.[[ProxyTarget]].
        const proxy_target = self.as(builtins.Proxy).fields.proxy_target.?;

        // c. Assert: proxyTarget is a function object.
        std.debug.assert(proxy_target.internal_methods.call != null);

        // d. Return ? GetFunctionRealm(proxyTarget).
        return proxy_target.getFunctionRealm(agent);
    }

    // 4. Return the current Realm Record.
    return agent.currentRealm();
}

/// 7.3.25 CopyDataProperties ( target, source, excludedItems )
/// https://tc39.es/ecma262/#sec-copydataproperties
pub fn copyDataProperties(
    self: *Object,
    source: Value,
    excluded_items: []const PropertyKey,
) Agent.Error!void {
    // 1. If source is either undefined or null, return unused.
    if (source.isUndefined() or source.isNull()) return;

    // 2. Let from be ! ToObject(source).
    const from = source.toObject(self.agent) catch |err| try noexcept(err);

    // 3. Let keys be ? from.[[OwnPropertyKeys]]().
    var keys = try from.internal_methods.ownPropertyKeys(self.agent, from);
    defer keys.deinit(self.agent.gc_allocator);

    // 4. For each element nextKey of keys, do
    for (keys.items) |next_key| {
        // a. Let excluded be false.
        // b. For each element e of excludedItems, do
        const excluded = for (excluded_items) |e| {
            // i. If SameValue(e, nextKey) is true, then
            if (e.eql(next_key)) {
                // 1. Set excluded to true.
                break true;
            }
        } else false;

        // c. If excluded is false, then
        if (!excluded) {
            // i. Let desc be ? from.[[GetOwnProperty]](nextKey).
            const descriptor = try from.internal_methods.getOwnProperty(
                self.agent,
                from,
                next_key,
            );

            // ii. If desc is not undefined and desc.[[Enumerable]] is true, then
            if (descriptor != null and descriptor.?.enumerable == true) {
                // 1. Let propValue be ? Get(from, nextKey).
                const property_value = try from.get(next_key);

                // 2. Perform ! CreateDataPropertyOrThrow(target, nextKey, propValue).
                try self.createDataPropertyDirect(next_key, property_value);
            }
        }
    }

    // 5. Return unused.
}

/// 7.3.26 PrivateElementFind ( O, P )
/// https://tc39.es/ecma262/#sec-privateelementfind
pub fn privateElementFind(self: *const Object, private_name: PrivateName) ?*PrivateElement {
    // 1. If O.[[PrivateElements]] contains a PrivateElement pe such that pe.[[Key]] is P, then
    //     a. Return pe.
    // 2. Return empty.
    return self.property_storage.private_elements.getPtr(private_name);
}

/// 7.3.27 PrivateFieldAdd ( O, P, value )
/// https://tc39.es/ecma262/#sec-privatefieldadd
pub fn privateFieldAdd(self: *Object, private_name: PrivateName, value: Value) Agent.Error!void {
    // 1. If the host is a web browser, then
    //     a. Perform ? HostEnsureCanAddPrivateElement(O).
    try self.agent.host_hooks.hostEnsureCanAddPrivateElement(self.agent, self);

    // 2. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name);

    // 3. If entry is not empty, throw a TypeError exception.
    if (entry != null) {
        return self.agent.throwException(
            .type_error,
            "Private element '#{}' already exists",
            .{private_name},
        );
    }

    // 4. Append PrivateElement { [[Key]]: P, [[Kind]]: field, [[Value]]: value } to O.[[PrivateElements]].
    try self.property_storage.private_elements.putNoClobber(self.agent.gc_allocator, private_name, .{ .field = value });

    // 5. Return unused.
}

/// 7.3.28 PrivateMethodOrAccessorAdd ( O, method )
/// https://tc39.es/ecma262/#sec-privatemethodoraccessoradd
pub fn privateMethodOrAccessorAdd(
    self: *Object,
    private_name: PrivateName,
    method: PrivateElement,
) Agent.Error!void {
    // 1. Assert: method.[[Kind]] is either method or accessor.
    std.debug.assert(method == .method or method == .accessor);

    // 2. If the host is a web browser, then
    //     a. Perform ? HostEnsureCanAddPrivateElement(O).
    try self.agent.host_hooks.hostEnsureCanAddPrivateElement(self.agent, self);

    // 3. Let entry be PrivateElementFind(O, method.[[Key]]).
    const entry = self.privateElementFind(private_name);

    // 4. If entry is not empty, throw a TypeError exception.
    if (entry != null) {
        return self.agent.throwException(
            .type_error,
            "Private element '#{}' already exists",
            .{private_name},
        );
    }

    // 5. Append method to O.[[PrivateElements]].
    try self.property_storage.private_elements.putNoClobber(self.agent.gc_allocator, private_name, method);

    // 6. Return unused.
}

/// 7.3.30 PrivateGet ( O, P )
/// https://tc39.es/ecma262/#sec-privateget
pub fn privateGet(self: *Object, private_name: PrivateName) Agent.Error!Value {
    // 1. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name) orelse {
        // 2. If entry is empty, throw a TypeError exception.
        return self.agent.throwException(
            .type_error,
            "Private element '#{}' doesn't exist",
            .{private_name},
        );
    };

    switch (entry.*) {
        // 3. If entry.[[Kind]] is either field or method, then
        //     a. Return entry.[[Value]].
        .field => |value| return value,
        .method => |object| return Value.from(object),

        // 4. Assert: entry.[[Kind]] is accessor.
        .accessor => |get_and_set| {
            // 5. If entry.[[Get]] is undefined, throw a TypeError exception.
            // 6. Let getter be entry.[[Get]].
            const getter = get_and_set.get orelse {
                return self.agent.throwException(
                    .type_error,
                    "Private element '#{}' has not getter",
                    .{private_name},
                );
            };

            // 7. Return ? Call(getter, O).
            return Value.from(getter).callAssumeCallable(self.agent, Value.from(self), &.{});
        },
    }
}

/// 7.3.31 PrivateSet ( O, P, value )
/// https://tc39.es/ecma262/#sec-privateset
pub fn privateSet(self: *Object, private_name: PrivateName, value: Value) Agent.Error!void {
    // 1. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name) orelse {
        // 2. If entry is empty, throw a TypeError exception.
        return self.agent.throwException(
            .type_error,
            "Private element '#{}' doesn't exist",
            .{private_name},
        );
    };

    switch (entry.*) {
        // 3. If entry.[[Kind]] is field, then
        .field => |*value_ptr| {
            // a. Set entry.[[Value]] to value.
            value_ptr.* = value;
        },

        // 4. Else if entry.[[Kind]] is method, then
        .method => {
            // a. Throw a TypeError exception.
            return self.agent.throwException(
                .type_error,
                "Private element '#{}' is a method and cannot be set",
                .{private_name},
            );
        },

        // 5. Else,
        //     a. Assert: entry.[[Kind]] is accessor.
        .accessor => |get_and_set| {
            // c. Let setter be entry.[[Set]].
            // b. If entry.[[Set]] is undefined, throw a TypeError exception.
            const setter = get_and_set.set orelse {
                return self.agent.throwException(
                    .type_error,
                    "Private element '#{}' has not setter",
                    .{private_name},
                );
            };

            // d. Perform ? Call(setter, O, « value »).
            _ = try Value.from(setter).callAssumeCallable(
                self.agent,
                Value.from(self),
                &.{value},
            );
        },
    }

    // 6. Return unused.
}

/// 7.3.32 DefineField ( receiver, fieldRecord )
/// https://tc39.es/ecma262/#sec-definefield
pub fn defineField(self: *Object, field: ClassFieldDefinition) Agent.Error!void {
    // 1. Let fieldName be fieldRecord.[[Name]].

    // 2. Let initializer be fieldRecord.[[Initializer]].
    // 3. If initializer is not empty, then
    const init_value: Value = if (field.initializer) |initializer| blk: {
        // a. Let initValue be ? Call(initializer, receiver).
        break :blk try Value.from(&initializer.object).callAssumeCallable(
            self.agent,
            Value.from(self),
            &.{},
        );
    } else blk: {
        // 4. Else,
        // a. Let initValue be undefined.
        break :blk .undefined;
    };

    switch (field.name) {
        // 5. If fieldName is a Private Name, then
        .private_name => |private_name| {
            // a. Perform ? PrivateFieldAdd(receiver, fieldName, initValue).
            try self.privateFieldAdd(private_name, init_value);
        },
        // 6. Else,
        .property_key => |property_key| {
            // a. Assert: fieldName is a property key.
            // b. Perform ? CreateDataPropertyOrThrow(receiver, fieldName, initValue).
            try self.createDataPropertyOrThrow(property_key, init_value);
        },
    }

    // 7. Return unused.
}

/// 7.3.33 InitializeInstanceElements ( O, constructor )
/// https://tc39.es/ecma262/#sec-initializeinstanceelements
pub fn initializeInstanceElements(self: *Object, constructor: *Object) Agent.Error!void {
    // 1. Let methods be the value of constructor.[[PrivateMethods]].
    const methods = if (constructor.is(builtins.ECMAScriptFunction)) blk: {
        break :blk constructor.as(builtins.ECMAScriptFunction).fields.private_methods;
    } else if (constructor.is(builtins.BuiltinFunction)) blk: {
        const class_constructor_fields = constructor.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
        break :blk class_constructor_fields.private_methods;
    } else unreachable;

    // 2. For each PrivateElement method of methods, do
    for (methods) |method| {
        // a. Perform ? PrivateMethodOrAccessorAdd(O, method).
        try self.privateMethodOrAccessorAdd(method.private_name, method.private_element);
    }

    // 3. Let fields be the value of constructor.[[Fields]].
    const fields = if (constructor.is(builtins.ECMAScriptFunction)) blk: {
        break :blk constructor.as(builtins.ECMAScriptFunction).fields.fields;
    } else if (constructor.is(builtins.BuiltinFunction)) blk: {
        const class_constructor_fields = constructor.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
        break :blk class_constructor_fields.fields;
    } else unreachable;

    // 4. For each element fieldRecord of fields, do
    for (fields) |field| {
        // a. Perform ? DefineField(O, fieldRecord).
        try self.defineField(field);
    }

    // 5. Return unused.
}

/// 9.2.13 GetOption ( options, property, type, values, default )
/// https://tc39.es/ecma402/#sec-getoption
pub fn getOption(
    self: *Object,
    agent: *Agent,
    comptime property: []const u8,
    comptime type_: enum {
        boolean,
        number,
        string,

        fn T(t: @This()) type {
            return switch (t) {
                .boolean => bool,
                .number => Number,
                .string => *const String,
            };
        }
    },
    values: ?[]const type_.T(),
    default: anytype,
) Agent.Error!if (@TypeOf(default) == @TypeOf(null)) ?type_.T() else type_.T() {
    if (@TypeOf(default) != @TypeOf(null) and @TypeOf(default) != type_.T() and default != .required) {
        @compileError("Invalid value for default parameter");
    }

    // 1. Let value be ? Get(options, property).
    const value = try self.get(PropertyKey.from(property));

    // 2. If value is undefined, then
    if (value.isUndefined()) {
        // a. If default is required, throw a RangeError exception.
        if (@TypeOf(default) == @TypeOf(.required)) {
            return agent.throwException(
                .range_error,
                "Required option '{s}' must not be undefined",
                .{property},
            );
        }

        // b. Return default.
        return default;
    }

    const coerced_value = switch (type_) {
        // 3. If type is boolean, then
        .boolean => blk: {
            // a. Set value to ToBoolean(value).
            break :blk value.toBoolean();
        },

        // 4. Else if type is number, then
        .number => blk: {
            // a. Set value to ? ToNumber(value).
            const number = try value.toNumber(agent);

            // b. If value is NaN, throw a RangeError exception.
            if (number.isNan()) {
                return agent.throwException(
                    .range_error,
                    "Number option '{s}' must not be NaN",
                    .{property},
                );
            }

            break :blk number;
        },

        // 5. Else,
        //     a. Assert: type is string.
        .string => blk: {
            // b. Set value to ? ToString(value).
            break :blk try value.toString(agent);
        },
    };

    // 6. If values is not empty and values does not contain value, throw a RangeError exception.
    if (values != null) {
        for (values.?) |permitted_value| {
            if (sameValue(Value.from(coerced_value), Value.from(permitted_value))) break;
        } else {
            return agent.throwException(
                .range_error,
                "Invalid value for option '{s}'",
                .{property},
            );
        }
    }

    // 7. Return value.
    return coerced_value;
}

test "format" {
    const gc = @import("../../gc.zig");
    var agent_ = try Agent.init(gc.allocator(), .{});
    defer agent_.deinit();

    const test_cases = [_]struct { *Object, []const u8 }{
        .{
            try builtins.Object.create(&agent_, .{ .prototype = null }),
            "[object Object]",
        },
    };
    for (test_cases) |test_case| {
        const object, const expected = test_case;
        try std.testing.expectFmt(expected, "{}", .{object});
    }
}
