//! 6.1.7 The Object Type
//! https://tc39.es/ecma262/#sec-object-type

const std = @import("std");

const AnyPointer = @import("any-pointer").AnyPointer;

const build_options = @import("build-options");
const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const spec = @import("../spec.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const ClassConstructorFields = builtins.ClassConstructorFields;
const ClassFieldDefinition = types.ClassFieldDefinition;
const PreferredType = Value.PreferredType;
const PrivateElement = types.PrivateElement;
const PrivateName = types.PrivateName;
const PrivateNameArrayHashMap = types.PrivateNameArrayHashMap;
const PropertyDescriptor = spec.PropertyDescriptor;
const Realm = execution.Realm;
const Value = @import("value.zig").Value;
const createArrayFromList = types.createArrayFromList;
const noexcept = utils.noexcept;
const validateNonRevokedProxy = builtins.validateNonRevokedProxy;

pub const Data = @import("Object/Data.zig");
pub const InternalMethods = @import("Object/InternalMethods.zig");
pub const PropertyKey = @import("Object/PropertyKey.zig").PropertyKey;
pub const PropertyStorage = @import("Object/PropertyStorage.zig");

const Self = @This();

pub const Tag = enum(u32) {
    // ECMA-262
    arguments,
    array,
    array_buffer,
    array_iterator,
    async_generator,
    big_int,
    boolean,
    bound_function,
    builtin_function,
    data_view,
    date,
    ecmascript_function,
    @"error",
    for_in_iterator,
    generator,
    map,
    map_iterator,
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

    // ECMA-402
    intl_locale,

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

ptr: AnyPointer,
data: *Data,
tag: ?Tag,

pub fn format(
    self: Self,
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

pub inline fn is(self: Self, comptime T: type) bool {
    comptime std.debug.assert(T.tag != null);
    if (self.tag == null) return false;
    return T.tag.? == self.tag.?;
}

pub inline fn as(self: Self, comptime T: type) *T {
    return self.ptr.cast(*T);
}

// Helper functions so we don't have to say 'data' all the time

pub inline fn prototype(self: Self) *?Self {
    return &self.data.prototype;
}

pub inline fn extensible(self: Self) *bool {
    return &self.data.extensible;
}

pub inline fn privateElements(self: Self) *PrivateNameArrayHashMap(PrivateElement) {
    return &self.data.private_elements;
}

pub inline fn isHTMLDDA(self: Self) bool {
    comptime if (!build_options.enable_annex_b) @compileError("Annex B is not enabled");
    return self.data.is_htmldda;
}

pub inline fn agent(self: Self) *Agent {
    return self.data.agent;
}

pub inline fn internalMethods(self: Self) *InternalMethods {
    return &self.data.internal_methods;
}

pub inline fn propertyStorage(self: Self) *PropertyStorage {
    return &self.data.property_storage;
}

/// Shortcut for the SameValue AO applied on two objects (i.e. pointer equality)
pub fn sameValue(self: Self, other: Self) bool {
    return self.ptr.eql(other.ptr);
}

/// 7.1.1.1 OrdinaryToPrimitive ( O, hint )
/// https://tc39.es/ecma262/#sec-ordinarytoprimitive
pub fn ordinaryToPrimitive(self: Self, hint: PreferredType) Agent.Error!Value {
    const method_names = switch (hint) {
        // 1. If hint is string, then
        //     a. Let methodNames be « "toString", "valueOf" ».
        .string => [_][]const u8{ "toString", "valueOf" },
        // 2. Else,
        //     a. Let methodNames be « "valueOf", "toString" ».
        else => [_][]const u8{ "valueOf", "toString" },
    };

    // 3. For each element name of methodNames, do
    for (method_names) |name| {
        // a. Let method be ? Get(O, name).
        const method = try self.get(PropertyKey.from(name));

        // b. If IsCallable(method) is true, then
        if (method.isCallable()) {
            // i. Let result be ? Call(method, O).
            const result = try method.callAssumeCallableNoArgs(Value.from(self));

            // ii. If result is not an Object, return result.
            if (result != .object) return result;
        }
    }

    // 4. Throw a TypeError exception.
    return self.agent().throwException(
        .type_error,
        "Could not convert object to {s}",
        .{@tagName(hint)},
    );
}

/// 7.2.5 IsExtensible ( O )
/// https://tc39.es/ecma262/#sec-isextensible-o
pub fn isExtensible(self: Self) Agent.Error!bool {
    // 1. Return ? O.[[IsExtensible]]().
    return self.internalMethods().isExtensible(self);
}

/// 7.3.2 Get ( O, P )
/// https://tc39.es/ecma262/#sec-get-o-p
pub fn get(self: Self, property_key: PropertyKey) Agent.Error!Value {
    // 1. Return ? O.[[Get]](P, O).
    return self.internalMethods().get(self, property_key, Value.from(self));
}

/// 7.3.4 Set ( O, P, V, Throw )
/// https://tc39.es/ecma262/#sec-set-o-p-v-throw
pub fn set(
    self: Self,
    property_key: PropertyKey,
    value: Value,
    throw: enum { throw, ignore },
) Agent.Error!void {
    // 1. Let success be ? O.[[Set]](P, V, O).
    const success = try self.internalMethods().set(self, property_key, value, Value.from(self));

    // 2. If success is false and Throw is true, throw a TypeError exception.
    if (!success and throw == .throw)
        return self.agent().throwException(.type_error, "Could not set property", .{});

    // 3. Return unused.
}

/// 7.3.5 CreateDataProperty ( O, P, V )
/// https://tc39.es/ecma262/#sec-createdataproperty
pub fn createDataProperty(self: Self, property_key: PropertyKey, value: Value) Agent.Error!bool {
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
    return self.internalMethods().defineOwnProperty(self, property_key, new_descriptor);
}

/// 7.3.6 CreateDataPropertyOrThrow ( O, P, V )
/// https://tc39.es/ecma262/#sec-createdatapropertyorthrow
pub fn createDataPropertyOrThrow(
    self: Self,
    property_key: PropertyKey,
    value: Value,
) Agent.Error!void {
    // 1. Let success be ? CreateDataProperty(O, P, V).
    const success = try self.createDataProperty(property_key, value);

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return self.agent().throwException(.type_error, "Could not create data property", .{});

    // 3. Return unused.
}

/// 7.3.7 CreateNonEnumerableDataPropertyOrThrow ( O, P, V )
/// https://tc39.es/ecma262/#sec-createnonenumerabledatapropertyorthrow
pub fn createNonEnumerableDataPropertyOrThrow(
    self: Self,
    property_key: PropertyKey,
    value: Value,
) Agent.Error!void {
    // 1. Assert: O is an ordinary, extensible object with no non-configurable properties.
    std.debug.assert(self.extensible().* and blk: {
        for (self.propertyStorage().hash_map.values()) |descriptor| {
            if (descriptor.configurable.? == false) break :blk false;
        }
        break :blk true;
    });

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
    self.definePropertyOrThrow(property_key, new_descriptor) catch |err| try noexcept(err);

    // 4. Return unused.
}

/// 7.3.8 DefinePropertyOrThrow ( O, P, desc )
/// https://tc39.es/ecma262/#sec-definepropertyorthrow
pub fn definePropertyOrThrow(
    self: Self,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!void {
    // 1. Let success be ? O.[[DefineOwnProperty]](P, desc).
    const success = try self.internalMethods().defineOwnProperty(
        self,
        property_key,
        property_descriptor,
    );

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return self.agent().throwException(.type_error, "Could not define property", .{});

    // 3. Return unused.
}

/// 7.3.9 DeletePropertyOrThrow ( O, P )
/// https://tc39.es/ecma262/#sec-deletepropertyorthrow
pub fn deletePropertyOrThrow(self: Self, property_key: PropertyKey) Agent.Error!void {
    // 1. Let success be ? O.[[Delete]](P).
    const success = try self.internalMethods().delete(self, property_key);

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return self.agent().throwException(.type_error, "Could not delete property", .{});

    // 3. Return unused.
}

/// 7.3.11 HasProperty ( O, P )
/// https://tc39.es/ecma262/#sec-hasproperty
pub fn hasProperty(self: Self, property_key: PropertyKey) Agent.Error!bool {
    // 1. Return ? O.[[HasProperty]](P).
    return self.internalMethods().hasProperty(self, property_key);
}

/// 7.3.12 HasOwnProperty ( O, P )
/// https://tc39.es/ecma262/#sec-hasownproperty
pub fn hasOwnProperty(self: Self, property_key: PropertyKey) Agent.Error!bool {
    // 1. Let desc be ? O.[[GetOwnProperty]](P).
    const descriptor = try self.internalMethods().getOwnProperty(self, property_key);

    // 2. If desc is undefined, return false.
    // 3. Return true.
    return descriptor != null;
}

/// 7.3.14 Construct ( F [ , argumentsList [ , newTarget ] ] )
/// https://tc39.es/ecma262/#sec-construct
pub fn construct(
    self: Self,
    arguments_list: []const Value,
    new_target: ?Self,
) Agent.Error!Self {
    // 1. If newTarget is not present, set newTarget to F.
    const new_target_ = new_target orelse self;

    // 2. If argumentsList is not present, set argumentsList to a new empty List.
    // NOTE: This is done via the NoArgs variant of the function.

    // 3. Return ? F.[[Construct]](argumentsList, newTarget).
    return self.internalMethods().construct.?(self, Arguments.from(arguments_list), new_target_);
}

pub inline fn constructNoArgs(self: Self) Agent.Error!Self {
    return self.construct(&.{}, null);
}

/// 7.3.15 SetIntegrityLevel ( O, level )
/// https://tc39.es/ecma262/#sec-setintegritylevel
pub fn setIntegrityLevel(self: Self, level: IntegrityLevel) Agent.Error!bool {
    // 1. Let status be ? O.[[PreventExtensions]]().
    const status = try self.internalMethods().preventExtensions(self);

    // 2. If status is false, return false.
    if (!status) return false;

    // 3. Let keys be ? O.[[OwnPropertyKeys]]().
    const keys = try self.internalMethods().ownPropertyKeys(self);
    defer keys.deinit();

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
                const maybe_current_descriptor = try self.internalMethods().getOwnProperty(
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
                    }
                    // 2. Else,
                    else {
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
pub fn testIntegrityLevel(self: Self, level: IntegrityLevel) Agent.Error!bool {
    // 1. Let extensible be ? IsExtensible(O).
    const extensible_ = try self.isExtensible();

    // 2. If extensible is true, return false.
    // 3. NOTE: If the object is extensible, none of its properties are examined.
    if (extensible_) return false;

    // 4. Let keys be ? O.[[OwnPropertyKeys]]().
    const keys = try self.internalMethods().ownPropertyKeys(self);
    defer keys.deinit();

    // 5. For each element k of keys, do
    for (keys.items) |property_key| {
        // a. Let currentDesc be ? O.[[GetOwnProperty]](k).
        const maybe_current_descriptor = try self.internalMethods().getOwnProperty(
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
pub fn lengthOfArrayLike(self: Self) Agent.Error!u53 {
    // 1. Return ℝ(? ToLength(? Get(obj, "length"))).
    return (try self.get(PropertyKey.from("length"))).toLength(self.agent());
}

/// 7.3.22 SpeciesConstructor ( O, defaultConstructor )
/// https://tc39.es/ecma262/#sec-speciesconstructor
pub fn speciesConstructor(self: Self, default_constructor: Self) Agent.Error!Self {
    // 1. Let C be ? Get(O, "constructor").
    const constructor = try self.get(PropertyKey.from("constructor"));

    // 2. If C is undefined, return defaultConstructor.
    if (constructor == .undefined) return default_constructor;

    // 3. If C is not an Object, throw a TypeError exception.
    if (constructor != .object) {
        return self.agent().throwException(.type_error, "{} is not an Object", .{constructor});
    }

    // 4. Let S be ? Get(C, @@species).
    const species = try constructor.object.get(
        PropertyKey.from(self.agent().well_known_symbols.@"@@species"),
    );

    // 5. If S is either undefined or null, return defaultConstructor.
    if (species == .undefined or species == .null) return default_constructor;

    // 6. If IsConstructor(S) is true, return S.
    if (species.isConstructor()) return species.object;

    // 7. Throw a TypeError exception.
    return self.agent().throwException(
        .type_error,
        "Object's [Symbol.species] property must be a constructor",
        .{},
    );
}

/// 7.3.23 EnumerableOwnProperties ( O, kind )
/// https://tc39.es/ecma262/#sec-enumerableownproperties
pub fn enumerableOwnProperties(
    self: Self,
    comptime kind: PropertyKind,
) Agent.Error!std.ArrayList(Value) {
    // 1. Let ownKeys be ? O.[[OwnPropertyKeys]]().
    const own_keys = try self.internalMethods().ownPropertyKeys(self);
    defer own_keys.deinit();

    // 2. Let results be a new empty List.
    var results = std.ArrayList(Value).init(self.agent().gc_allocator);

    // 3. For each element key of ownKeys, do
    for (own_keys.items) |key| {
        // a. If key is a String, then
        if (key == .string or key == .integer_index) {
            // i. Let desc be ? O.[[GetOwnProperty]](key).
            const descriptor = try self.internalMethods().getOwnProperty(self, key);

            // ii. If desc is not undefined and desc.[[Enumerable]] is true, then
            if (descriptor != null and descriptor.?.enumerable == true) {
                // 1. If kind is key, then
                if (kind == .key) {
                    // a. Append key to results.
                    try results.append(try key.toValue(self.agent()));
                }
                // 2. Else,
                else {
                    // a. Let value be ? Get(O, key).
                    const value = try self.get(key);

                    // b. If kind is value, then
                    if (kind == .value) {
                        // i. Append value to results.
                        try results.append(value);
                    }
                    // c. Else,
                    else {
                        // i. Assert: kind is key+value.
                        std.debug.assert(kind == .@"key+value");

                        // ii. Let entry be CreateArrayFromList(« key, value »).
                        const entry = Value.from(try createArrayFromList(
                            self.agent(),
                            &.{ try key.toValue(self.agent()), value },
                        ));

                        // iii. Append entry to results.
                        try results.append(entry);
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
pub fn getFunctionRealm(self: Self) error{ExceptionThrown}!*Realm {
    // 1. If obj has a [[Realm]] internal slot, then
    if (self.internalMethods().call != null) {
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
        return bound_target_function.getFunctionRealm();
    }

    // 3. If obj is a Proxy exotic object, then
    if (self.is(builtins.Proxy)) {
        // a. Perform ? ValidateNonRevokedProxy(obj).
        try validateNonRevokedProxy(self.as(builtins.Proxy));

        // b. Let proxyTarget be obj.[[ProxyTarget]].
        const proxy_target = self.as(builtins.Proxy).fields.proxy_target;

        // c. Return ? GetFunctionRealm(proxyTarget).
        return proxy_target.?.getFunctionRealm();
    }

    // 4. Return the current Realm Record.
    return self.agent().currentRealm();
}

/// 7.3.25 CopyDataProperties ( target, source, excludedItems )
/// https://tc39.es/ecma262/#sec-copydataproperties
pub fn copyDataProperties(
    self: *Self,
    source: Value,
    excluded_items: []const PropertyKey,
) Agent.Error!void {
    // 1. If source is either undefined or null, return unused.
    if (source == .undefined or source == .null) return;

    // 2. Let from be ! ToObject(source).
    const from = source.toObject(self.agent()) catch |err| try noexcept(err);

    // 3. Let keys be ? from.[[OwnPropertyKeys]]().
    const keys = try from.internalMethods().ownPropertyKeys(from);
    defer keys.deinit();

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
            const descriptor = try from.internalMethods().getOwnProperty(from, next_key);

            // ii. If desc is not undefined and desc.[[Enumerable]] is true, then
            if (descriptor != null and descriptor.?.enumerable == true) {
                // 1. Let propValue be ? Get(from, nextKey).
                const property_value = try from.get(next_key);

                // 2. Perform ! CreateDataPropertyOrThrow(target, nextKey, propValue).
                self.createDataPropertyOrThrow(
                    next_key,
                    property_value,
                ) catch |err| try noexcept(err);
            }
        }
    }

    // 5. Return unused.
}

/// 7.3.26 PrivateElementFind ( O, P )
/// https://tc39.es/ecma262/#sec-privateelementfind
pub fn privateElementFind(self: Self, private_name: PrivateName) ?*PrivateElement {
    // 1. If O.[[PrivateElements]] contains a PrivateElement pe such that pe.[[Key]] is P, then
    //     a. Return pe.
    // 2. Return empty.
    return self.privateElements().getPtr(private_name);
}

/// 7.3.27 PrivateFieldAdd ( O, P, value )
/// https://tc39.es/ecma262/#sec-privatefieldadd
pub fn privateFieldAdd(self: *Self, private_name: PrivateName, value: Value) Agent.Error!void {
    // 1. If the host is a web browser, then
    //     a. Perform ? HostEnsureCanAddPrivateElement(O).
    try self.agent().host_hooks.hostEnsureCanAddPrivateElement(self.agent(), self.*);

    // 2. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name);

    // 3. If entry is not empty, throw a TypeError exception.
    if (entry != null) {
        return self.agent().throwException(
            .type_error,
            "Private element '#{}' already exists",
            .{private_name},
        );
    }

    // 4. Append PrivateElement { [[Key]]: P, [[Kind]]: field, [[Value]]: value } to O.[[PrivateElements]].
    try self.privateElements().putNoClobber(private_name, .{ .field = value });

    // 5. Return unused.
}

/// 7.3.28 PrivateMethodOrAccessorAdd ( O, method )
/// https://tc39.es/ecma262/#sec-privatemethodoraccessoradd
pub fn privateMethodOrAccessorAdd(
    self: *Self,
    private_name: PrivateName,
    method: PrivateElement,
) Agent.Error!void {
    // 1. Assert: method.[[Kind]] is either method or accessor.
    std.debug.assert(method == .method or method == .accessor);

    // 2. If the host is a web browser, then
    //     a. Perform ? HostEnsureCanAddPrivateElement(O).
    try self.agent().host_hooks.hostEnsureCanAddPrivateElement(self.agent(), self.*);

    // 3. Let entry be PrivateElementFind(O, method.[[Key]]).
    const entry = self.privateElementFind(private_name);

    // 4. If entry is not empty, throw a TypeError exception.
    if (entry != null) {
        return self.agent().throwException(
            .type_error,
            "Private element '#{}' already exists",
            .{private_name},
        );
    }

    // 5. Append method to O.[[PrivateElements]].
    try self.privateElements().putNoClobber(private_name, method);

    // 6. Return unused.
}

/// 7.3.30 PrivateGet ( O, P )
/// https://tc39.es/ecma262/#sec-privateget
pub fn privateGet(self: Self, private_name: PrivateName) Agent.Error!Value {
    // 1. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name) orelse {
        // 2. If entry is empty, throw a TypeError exception.
        return self.agent().throwException(
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
                return self.agent().throwException(
                    .type_error,
                    "Private element '#{}' has not getter",
                    .{private_name},
                );
            };

            // 7. Return ? Call(getter, O).
            return Value.from(getter).callAssumeCallableNoArgs(Value.from(self));
        },
    }
}

/// 7.3.31 PrivateSet ( O, P, value )
/// https://tc39.es/ecma262/#sec-privateset
pub fn privateSet(self: *Self, private_name: PrivateName, value: Value) Agent.Error!void {
    // 1. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name) orelse {
        // 2. If entry is empty, throw a TypeError exception.
        return self.agent().throwException(
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
            return self.agent().throwException(
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
                return self.agent().throwException(
                    .type_error,
                    "Private element '#{}' has not setter",
                    .{private_name},
                );
            };

            // d. Perform ? Call(setter, O, « value »).
            _ = try Value.from(setter).callAssumeCallable(Value.from(self.*), &.{value});
        },
    }

    // 6. Return unused.
}

/// 7.3.32 DefineField ( receiver, fieldRecord )
/// https://tc39.es/ecma262/#sec-definefield
pub fn defineField(self: *Self, field: ClassFieldDefinition) Agent.Error!void {
    // 1. Let fieldName be fieldRecord.[[Name]].

    // 2. Let initializer be fieldRecord.[[Initializer]].
    // 3. If initializer is not empty, then
    const init_value = if (field.initializer) |initializer| blk: {
        // a. Let initValue be ? Call(initializer, receiver).
        break :blk try Value.from(initializer.object()).callAssumeCallableNoArgs(Value.from(self.*));
    }
    // 4. Else,
    else blk: {
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
pub fn initializeInstanceElements(self: *Self, constructor: Self) Agent.Error!void {
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

test "format" {
    const gc = @import("../../gc.zig");
    var agent_ = try Agent.init(gc.allocator(), .{});
    defer agent_.deinit();
    const object = try builtins.Object.create(&agent_, .{
        .prototype = null,
    });
    const string = try std.fmt.allocPrint(std.testing.allocator, "{}", .{object});
    defer std.testing.allocator.free(string);
    try std.testing.expectEqualStrings(string, "[object Object]");
}
