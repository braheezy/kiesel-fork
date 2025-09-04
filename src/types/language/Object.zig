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
const Behaviour = builtins.builtin_function.Behaviour;
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
const createBuiltinFunction = builtins.createBuiltinFunction;
const noexcept = utils.noexcept;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
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
    temporal_duration,
    temporal_instant,
    temporal_plain_date,
    temporal_plain_date_time,
    temporal_plain_month_day,
    temporal_plain_time,
    temporal_plain_year_month,
    temporal_zoned_date_time,
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
internal_methods: *const InternalMethods,
property_storage: PropertyStorage,

pub fn format(self: *const Object, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    _ = self;
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
    return @alignCast(@constCast(@fieldParentPtr("object", self)));
}

pub fn cast(self: *const Object, comptime T: type) ?*T {
    return if (self.is(T)) self.as(T) else null;
}

pub fn prototype(self: *const Object) ?*Object {
    return self.property_storage.shape.prototype;
}

pub fn setPrototype(self: *Object, agent: *Agent, new_prototype: ?*Object) std.mem.Allocator.Error!void {
    if (self.prototype() == new_prototype) return;
    self.property_storage.shape = try self.property_storage.shape.setPrototype(agent.gc_allocator, new_prototype);
}

pub fn extensible(self: *const Object) bool {
    return self.property_storage.shape.extensible;
}

pub fn setNonExtensible(self: *Object, agent: *Agent) std.mem.Allocator.Error!void {
    if (!self.extensible()) return;
    self.property_storage.shape = try self.property_storage.shape.setNonExtensible(agent.gc_allocator);
}

pub fn isHTMLDDA(self: *const Object) bool {
    return self.property_storage.shape.is_htmldda;
}

pub fn setIsHTMLDDA(self: *Object, agent: *Agent) std.mem.Allocator.Error!void {
    if (self.isHTMLDDA()) return;
    self.property_storage.shape = try self.property_storage.shape.setIsHTMLDDA(agent.gc_allocator);
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
    agent: *Agent,
    property_key: PropertyKey,
    value: Value,
) std.mem.Allocator.Error!void {
    const has_ordinary_internal_methods = self.internal_methods.flags.supersetOf(.initMany(&.{
        .ordinary_define_own_property,
        .ordinary_get_own_property,
        .ordinary_is_extensible,
    }));

    if (has_ordinary_internal_methods) {
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
    agent: *Agent,
    property_key: PropertyKey,
    property_descriptor: PropertyStorage.CompletePropertyDescriptor,
) std.mem.Allocator.Error!void {
    const has_ordinary_internal_methods = self.internal_methods.flags.supersetOf(.initMany(&.{
        .ordinary_define_own_property,
        .ordinary_get_own_property,
        .ordinary_is_extensible,
    }));

    if (has_ordinary_internal_methods) {
        try self.property_storage.set(
            agent.gc_allocator,
            property_key,
            property_descriptor,
        );
    } else {
        const result = self.internal_methods.defineOwnProperty(
            agent,
            self,
            property_key,
            property_descriptor.toPropertyDescriptor(),
        ) catch |err| try noexcept(err);
        std.debug.assert(result);
    }
}

// NOTE: A lot of this behaviour is implied for all builtins and described at the end of
// https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects.

fn getFunctionName(comptime name: []const u8) []const u8 {
    if (comptime std.mem.startsWith(u8, name, "%Symbol.")) {
        comptime std.debug.assert(std.mem.endsWith(u8, name, "%"));
        return std.fmt.comptimePrint("[{s}]", .{name[1 .. name.len - 1]});
    } else {
        return name;
    }
}

fn getPropertyKey(comptime name: []const u8, agent: *Agent) PropertyKey {
    if (comptime std.mem.startsWith(u8, name, "%Symbol.")) {
        comptime std.debug.assert(std.mem.endsWith(u8, name, "%"));
        return PropertyKey.from(@field(agent.well_known_symbols, name));
    } else {
        return PropertyKey.from(name);
    }
}

pub fn defineBuiltinAccessor(
    self: *Object,
    agent: *Agent,
    comptime name: []const u8,
    comptime getter: ?Behaviour.Function,
    comptime setter: ?Behaviour.Function,
    realm: *Realm,
) std.mem.Allocator.Error!void {
    return self.defineBuiltinAccessorWithAttributes(
        agent,
        name,
        getter,
        setter,
        realm,
        .{ .enumerable = false, .configurable = true },
    );
}

pub fn defineBuiltinAccessorWithAttributes(
    self: *Object,
    agent: *Agent,
    comptime name: []const u8,
    comptime getter: ?Behaviour.Function,
    comptime setter: ?Behaviour.Function,
    realm: *Realm,
    attributes: struct {
        enumerable: bool,
        configurable: bool,
    },
) std.mem.Allocator.Error!void {
    comptime std.debug.assert(getter != null or setter != null);
    const getter_function = if (getter) |function| blk: {
        const function_name = std.fmt.comptimePrint("get {s}", .{comptime getFunctionName(name)});
        break :blk try createBuiltinFunction(
            agent,
            .{ .function = function },
            0,
            function_name,
            .{ .realm = realm },
        );
    } else {};
    const setter_function = if (setter) |function| blk: {
        const function_name = std.fmt.comptimePrint("set {s}", .{comptime getFunctionName(name)});
        break :blk try createBuiltinFunction(
            agent,
            .{ .function = function },
            0,
            function_name,
            .{ .realm = realm },
        );
    } else {};
    const property_key = getPropertyKey(name, agent);
    const attributes_: Object.PropertyStorage.Attributes = .{
        .writable = false,
        .enumerable = attributes.enumerable,
        .configurable = attributes.configurable,
    };
    self.property_storage.shape = try self.property_storage.shape.setPropertyWithoutTransition(
        agent.gc_allocator,
        property_key,
        attributes_,
        .{ .accessor = @enumFromInt(self.property_storage.accessors.items.len) },
    );
    try self.property_storage.accessors.append(agent.gc_allocator, .{
        .get = if (@TypeOf(getter_function) != void) &getter_function.object else null,
        .set = if (@TypeOf(setter_function) != void) &setter_function.object else null,
    });
}

pub fn defineBuiltinFunction(
    self: *Object,
    agent: *Agent,
    comptime name: []const u8,
    comptime function: Behaviour.Function,
    comptime length: u32,
    realm: *Realm,
) std.mem.Allocator.Error!void {
    return self.defineBuiltinFunctionWithAttributes(
        agent,
        name,
        function,
        length,
        realm,
        .builtin_default,
    );
}

pub fn defineBuiltinFunctionWithAttributes(
    self: *Object,
    agent: *Agent,
    comptime name: []const u8,
    comptime function: Behaviour.Function,
    comptime length: u32,
    realm: *Realm,
    attributes: Object.PropertyStorage.Attributes,
) std.mem.Allocator.Error!void {
    const function_name = comptime getFunctionName(name);
    const builtin_function = try createBuiltinFunction(
        agent,
        .{ .function = function },
        length,
        function_name,
        .{ .realm = realm },
    );
    try self.defineBuiltinPropertyWithAttributes(
        agent,
        name,
        Value.from(&builtin_function.object),
        attributes,
    );
}

pub fn defineBuiltinFunctionLazy(
    self: *Object,
    agent: *Agent,
    comptime name: []const u8,
    comptime function: Behaviour.Function,
    comptime length: u32,
    realm: *Realm,
    attributes: Object.PropertyStorage.Attributes,
) std.mem.Allocator.Error!void {
    const function_name = comptime getFunctionName(name);
    try self.defineBuiltinPropertyLazy(
        agent,
        name,
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                const builtin_function = try createBuiltinFunction(
                    agent_,
                    .{ .function = function },
                    length,
                    function_name,
                    .{ .realm = realm_ },
                );
                return Value.from(&builtin_function.object);
            }
        }.initializer,
        realm,
        attributes,
    );
}

pub fn defineBuiltinProperty(
    self: *Object,
    agent: *Agent,
    comptime name: []const u8,
    value: Value,
) std.mem.Allocator.Error!void {
    return self.defineBuiltinPropertyWithAttributes(agent, name, value, .builtin_default);
}

pub fn defineBuiltinPropertyWithAttributes(
    self: *Object,
    agent: *Agent,
    comptime name: []const u8,
    value: Value,
    attributes: Object.PropertyStorage.Attributes,
) std.mem.Allocator.Error!void {
    const property_key = getPropertyKey(name, agent);
    self.property_storage.shape = try self.property_storage.shape.setPropertyWithoutTransition(
        agent.gc_allocator,
        property_key,
        attributes,
        .{ .value = @enumFromInt(self.property_storage.values.items.len) },
    );
    try self.property_storage.values.append(agent.gc_allocator, value);
}

pub fn defineBuiltinPropertyLazy(
    object: *Object,
    agent: *Agent,
    comptime name: []const u8,
    comptime initializer: fn (*Agent, *Realm) std.mem.Allocator.Error!Value,
    realm: *Realm,
    attributes: Object.PropertyStorage.Attributes,
) std.mem.Allocator.Error!void {
    const property_key = getPropertyKey(name, agent);
    object.property_storage.shape = try object.property_storage.shape.setPropertyWithoutTransition(
        agent.gc_allocator,
        property_key,
        attributes,
        .{ .value = @enumFromInt(object.property_storage.values.items.len) },
    );
    try object.property_storage.values.append(agent.gc_allocator, undefined);
    try object.property_storage.lazy_properties.putNoClobber(
        agent.gc_allocator,
        property_key,
        .{
            .realm = realm,
            .initializer = .{ .value = initializer },
        },
    );
}

/// 7.1.1.1 OrdinaryToPrimitive ( O, hint )
/// https://tc39.es/ecma262/#sec-ordinarytoprimitive
pub fn ordinaryToPrimitive(self: *Object, agent: *Agent, hint: PreferredType) Agent.Error!Value {
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
        const method = try self.get(agent, name);

        // b. If IsCallable(method) is true, then
        if (method.isCallable()) {
            // i. Let result be ? Call(method, O).
            const result = try method.callAssumeCallable(agent, Value.from(self), &.{});

            // ii. If result is not an Object, return result.
            if (!result.isObject()) return result;
        }
    }

    // 4. Throw a TypeError exception.
    return agent.throwException(.type_error, "Could not convert object to {t}", .{hint});
}

/// 7.2.5 IsExtensible ( O )
/// https://tc39.es/ecma262/#sec-isextensible-o
pub fn isExtensible(self: *Object, agent: *Agent) Agent.Error!bool {
    // 1. Return ? O.[[IsExtensible]]().
    return self.internal_methods.isExtensible(agent, self);
}

/// 7.3.2 Get ( O, P )
/// https://tc39.es/ecma262/#sec-get-o-p
pub fn get(self: *Object, agent: *Agent, property_key: PropertyKey) Agent.Error!Value {
    // 1. Return ? O.[[Get]](P, O).
    return self.internal_methods.get(agent, self, property_key, Value.from(self));
}

/// 7.3.4 Set ( O, P, V, Throw )
/// https://tc39.es/ecma262/#sec-set-o-p-v-throw
pub fn set(
    self: *Object,
    agent: *Agent,
    property_key: PropertyKey,
    value: Value,
    throw: enum { throw, ignore },
) Agent.Error!void {
    // 1. Let success be ? O.[[Set]](P, V, O).
    const success = try self.internal_methods.set(
        agent,
        self,
        property_key,
        value,
        Value.from(self),
    );

    // 2. If success is false and Throw is true, throw a TypeError exception.
    if (!success and throw == .throw)
        return agent.throwException(.type_error, "Could not set property", .{});

    // 3. Return unused.
}

/// 7.3.5 CreateDataProperty ( O, P, V )
/// https://tc39.es/ecma262/#sec-createdataproperty
pub fn createDataProperty(self: *Object, agent: *Agent, property_key: PropertyKey, value: Value) Agent.Error!bool {
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
    return self.internal_methods.defineOwnProperty(agent, self, property_key, new_descriptor);
}

/// 7.3.6 CreateDataPropertyOrThrow ( O, P, V )
/// https://tc39.es/ecma262/#sec-createdatapropertyorthrow
pub fn createDataPropertyOrThrow(
    self: *Object,
    agent: *Agent,
    property_key: PropertyKey,
    value: Value,
) Agent.Error!void {
    // 1. Let success be ? CreateDataProperty(O, P, V).
    const success = try self.createDataProperty(agent, property_key, value);

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return agent.throwException(.type_error, "Could not create data property", .{});

    // 3. Return unused.
}

/// 7.3.7 CreateNonEnumerableDataPropertyOrThrow ( O, P, V )
/// https://tc39.es/ecma262/#sec-createnonenumerabledatapropertyorthrow
pub fn createNonEnumerableDataPropertyOrThrow(
    self: *Object,
    agent: *Agent,
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
    const new_descriptor: PropertyStorage.CompletePropertyDescriptor = .{
        .value_or_accessor = .{
            .value = value,
        },
        .attributes = .builtin_default,
    };

    // 3. Perform ! DefinePropertyOrThrow(O, P, newDesc).
    try self.definePropertyDirect(agent, property_key, new_descriptor);

    // 4. Return unused.
}

/// 7.3.8 DefinePropertyOrThrow ( O, P, desc )
/// https://tc39.es/ecma262/#sec-definepropertyorthrow
pub fn definePropertyOrThrow(
    self: *Object,
    agent: *Agent,
    property_key: PropertyKey,
    property_descriptor: PropertyDescriptor,
) Agent.Error!void {
    // 1. Let success be ? O.[[DefineOwnProperty]](P, desc).
    const success = try self.internal_methods.defineOwnProperty(
        agent,
        self,
        property_key,
        property_descriptor,
    );

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return agent.throwException(.type_error, "Could not define property", .{});

    // 3. Return unused.
}

/// 7.3.9 DeletePropertyOrThrow ( O, P )
/// https://tc39.es/ecma262/#sec-deletepropertyorthrow
pub fn deletePropertyOrThrow(self: *Object, agent: *Agent, property_key: PropertyKey) Agent.Error!void {
    // 1. Let success be ? O.[[Delete]](P).
    const success = try self.internal_methods.delete(agent, self, property_key);

    // 2. If success is false, throw a TypeError exception.
    if (!success)
        return agent.throwException(.type_error, "Could not delete property", .{});

    // 3. Return unused.
}

/// 7.3.11 HasProperty ( O, P )
/// https://tc39.es/ecma262/#sec-hasproperty
pub fn hasProperty(self: *Object, agent: *Agent, property_key: PropertyKey) Agent.Error!bool {
    // 1. Return ? O.[[HasProperty]](P).
    return self.internal_methods.hasProperty(agent, self, property_key);
}

/// 7.3.12 HasOwnProperty ( O, P )
/// https://tc39.es/ecma262/#sec-hasownproperty
pub fn hasOwnProperty(self: *Object, agent: *Agent, property_key: PropertyKey) Agent.Error!bool {
    // 1. Let desc be ? O.[[GetOwnProperty]](P).
    const descriptor = try self.internal_methods.getOwnProperty(agent, self, property_key);

    // 2. If desc is undefined, return false.
    // 3. Return true.
    return descriptor != null;
}

/// 7.3.14 Construct ( F [ , argumentsList [ , newTarget ] ] )
/// https://tc39.es/ecma262/#sec-construct
pub fn construct(
    self: *Object,
    agent: *Agent,
    arguments_list: []const Value,
    maybe_new_target: ?*Object,
) Agent.Error!*Object {
    // 1. If newTarget is not present, set newTarget to F.
    const new_target = maybe_new_target orelse self;

    // 2. If argumentsList is not present, set argumentsList to a new empty List.

    // 3. Return ? F.[[Construct]](argumentsList, newTarget).
    return self.internal_methods.construct.?(agent, self, Arguments.from(arguments_list), new_target);
}

/// 7.3.15 SetIntegrityLevel ( O, level )
/// https://tc39.es/ecma262/#sec-setintegritylevel
pub fn setIntegrityLevel(self: *Object, agent: *Agent, level: IntegrityLevel) Agent.Error!bool {
    // 1. Let status be ? O.[[PreventExtensions]]().
    const status = try self.internal_methods.preventExtensions(agent, self);

    // 2. If status is false, return false.
    if (!status) return false;

    // 3. Let keys be ? O.[[OwnPropertyKeys]]().
    const keys = try self.internal_methods.ownPropertyKeys(agent, self);
    defer agent.gc_allocator.free(keys);

    switch (level) {
        // 4. If level is sealed,
        .sealed => {
            // a. For each element k of keys, do
            for (keys) |property_key| {
                // i. Perform ? DefinePropertyOrThrow(O, k, PropertyDescriptor { [[Configurable]]: false }).
                try self.definePropertyOrThrow(agent, property_key, .{ .configurable = false });
            }
        },

        // 5. Else,
        .frozen => {
            // a. Assert: level is frozen.

            // b. For each element k of keys, do
            for (keys) |property_key| {
                // i. Let currentDesc be ? O.[[GetOwnProperty]](k).
                const maybe_current_descriptor = try self.internal_methods.getOwnProperty(
                    agent,
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
                    try self.definePropertyOrThrow(agent, property_key, descriptor);
                }
            }
        },
    }

    // 6. Return true.
    return true;
}

/// 7.3.16 TestIntegrityLevel ( O, level )
/// https://tc39.es/ecma262/#sec-testintegritylevel
pub fn testIntegrityLevel(self: *Object, agent: *Agent, level: IntegrityLevel) Agent.Error!bool {
    // 1. Let extensible be ? IsExtensible(O).
    const extensible_ = try self.isExtensible(agent);

    // 2. If extensible is true, return false.
    // 3. NOTE: If the object is extensible, none of its properties are examined.
    if (extensible_) return false;

    // 4. Let keys be ? O.[[OwnPropertyKeys]]().
    const keys = try self.internal_methods.ownPropertyKeys(agent, self);
    defer agent.gc_allocator.free(keys);

    // 5. For each element k of keys, do
    for (keys) |property_key| {
        // a. Let currentDesc be ? O.[[GetOwnProperty]](k).
        const maybe_current_descriptor = try self.internal_methods.getOwnProperty(
            agent,
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
pub fn lengthOfArrayLike(self: *Object, agent: *Agent) Agent.Error!u53 {
    // 1. Return ℝ(? ToLength(? Get(obj, "length"))).
    return (try self.get(agent, PropertyKey.from("length"))).toLength(agent);
}

/// 7.3.22 SpeciesConstructor ( O, defaultConstructor )
/// https://tc39.es/ecma262/#sec-speciesconstructor
pub fn speciesConstructor(self: *Object, agent: *Agent, default_constructor: *Object) Agent.Error!*Object {
    // 1. Let C be ? Get(O, "constructor").
    const constructor = try self.get(agent, PropertyKey.from("constructor"));

    // 2. If C is undefined, return defaultConstructor.
    if (constructor.isUndefined()) return default_constructor;

    // 3. If C is not an Object, throw a TypeError exception.
    if (!constructor.isObject()) {
        return agent.throwException(.type_error, "{f} is not an Object", .{constructor});
    }

    // 4. Let S be ? Get(C, %Symbol.species%).
    const species = try constructor.asObject().get(
        agent,
        PropertyKey.from(agent.well_known_symbols.@"%Symbol.species%"),
    );

    // 5. If S is either undefined or null, return defaultConstructor.
    if (species.isUndefined() or species.isNull()) return default_constructor;

    // 6. If IsConstructor(S) is true, return S.
    if (species.isConstructor()) return species.asObject();

    // 7. Throw a TypeError exception.
    return agent.throwException(
        .type_error,
        "Object's [Symbol.species] property must be a constructor",
        .{},
    );
}

/// 7.3.23 EnumerableOwnProperties ( O, kind )
/// https://tc39.es/ecma262/#sec-enumerableownproperties
pub fn enumerableOwnProperties(
    self: *Object,
    agent: *Agent,
    comptime kind: PropertyKind,
) Agent.Error!std.ArrayList(Value) {
    // 1. Let ownKeys be ? O.[[OwnPropertyKeys]]().
    const own_keys = try self.internal_methods.ownPropertyKeys(agent, self);
    defer agent.gc_allocator.free(own_keys);

    // 2. Let results be a new empty List.
    var results: std.ArrayList(Value) = .empty;

    // 3. For each element key of ownKeys, do
    for (own_keys) |key| {
        // a. If key is a String, then
        if (key == .string or key == .integer_index) {
            // i. Let desc be ? O.[[GetOwnProperty]](key).
            const descriptor = try self.internal_methods.getOwnProperty(agent, self, key);

            // ii. If desc is not undefined and desc.[[Enumerable]] is true, then
            if (descriptor != null and descriptor.?.enumerable == true) {
                // 1. If kind is key, then
                if (kind == .key) {
                    // a. Append key to results.
                    try results.append(agent.gc_allocator, try key.toValue(agent));
                } else {
                    // 2. Else,
                    // a. Let value be ? Get(O, key).
                    const value = try self.get(agent, key);

                    // b. If kind is value, then
                    if (kind == .value) {
                        // i. Append value to results.
                        try results.append(agent.gc_allocator, value);
                    } else {
                        // c. Else,
                        // i. Assert: kind is key+value.
                        std.debug.assert(kind == .@"key+value");

                        // ii. Let entry be CreateArrayFromList(« key, value »).
                        const entry = try createArrayFromList(
                            agent,
                            &.{ try key.toValue(agent), value },
                        );

                        // iii. Append entry to results.
                        try results.append(agent.gc_allocator, Value.from(&entry.object));
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
        if (self.cast(builtins.BuiltinFunction)) |builtin_function| {
            return builtin_function.fields.realm;
        } else if (self.cast(builtins.ECMAScriptFunction)) |ecmascript_function| {
            return ecmascript_function.fields.realm;
        } else if (!(self.is(builtins.BoundFunction) or self.is(builtins.Proxy))) {
            @panic("Unhandled function type in getFunctionRealm()");
        }
    }

    // 2. If obj is a bound function exotic object, then
    if (self.cast(builtins.BoundFunction)) |bound_function| {
        // a. Let boundTargetFunction be obj.[[BoundTargetFunction]].
        const bound_target_function = bound_function.fields.bound_target_function;

        // b. Return ? GetFunctionRealm(boundTargetFunction).
        return bound_target_function.getFunctionRealm(agent);
    }

    // 3. If obj is a Proxy exotic object, then
    if (self.cast(builtins.Proxy)) |proxy| {
        // a. Perform ? ValidateNonRevokedProxy(obj).
        try validateNonRevokedProxy(agent, proxy);

        // b. Let proxyTarget be obj.[[ProxyTarget]].
        const proxy_target = proxy.fields.proxy_target.?;

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
    agent: *Agent,
    source: Value,
    excluded_items: []const PropertyKey,
) Agent.Error!void {
    // 1. If source is either undefined or null, return unused.
    if (source.isUndefined() or source.isNull()) return;

    // 2. Let from be ! ToObject(source).
    const from = source.toObject(agent) catch |err| try noexcept(err);

    // 3. Let keys be ? from.[[OwnPropertyKeys]]().
    const keys = try from.internal_methods.ownPropertyKeys(agent, from);
    defer agent.gc_allocator.free(keys);

    // 4. For each element nextKey of keys, do
    for (keys) |next_key| {
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
                agent,
                from,
                next_key,
            );

            // ii. If desc is not undefined and desc.[[Enumerable]] is true, then
            if (descriptor != null and descriptor.?.enumerable == true) {
                // 1. Let propValue be ? Get(from, nextKey).
                const property_value = try from.get(agent, next_key);

                // 2. Perform ! CreateDataPropertyOrThrow(target, nextKey, propValue).
                try self.createDataPropertyDirect(agent, next_key, property_value);
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
pub fn privateFieldAdd(self: *Object, agent: *Agent, private_name: PrivateName, value: Value) Agent.Error!void {
    // 1. If the host is a web browser, then
    //     a. Perform ? HostEnsureCanAddPrivateElement(O).
    try agent.host_hooks.hostEnsureCanAddPrivateElement(agent, self);

    // 2. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name);

    // 3. If entry is not empty, throw a TypeError exception.
    if (entry != null) {
        return agent.throwException(
            .type_error,
            "Private element '{f}' already exists",
            .{private_name},
        );
    }

    // 4. Append PrivateElement { [[Key]]: P, [[Kind]]: field, [[Value]]: value } to O.[[PrivateElements]].
    try self.property_storage.private_elements.putNoClobber(agent.gc_allocator, private_name, .{ .field = value });

    // 5. Return unused.
}

/// 7.3.28 PrivateMethodOrAccessorAdd ( O, method )
/// https://tc39.es/ecma262/#sec-privatemethodoraccessoradd
pub fn privateMethodOrAccessorAdd(
    self: *Object,
    agent: *Agent,
    private_name: PrivateName,
    method: PrivateElement,
) Agent.Error!void {
    // 1. Assert: method.[[Kind]] is either method or accessor.
    std.debug.assert(method == .method or method == .accessor);

    // 2. If the host is a web browser, then
    //     a. Perform ? HostEnsureCanAddPrivateElement(O).
    try agent.host_hooks.hostEnsureCanAddPrivateElement(agent, self);

    // 3. Let entry be PrivateElementFind(O, method.[[Key]]).
    const entry = self.privateElementFind(private_name);

    // 4. If entry is not empty, throw a TypeError exception.
    if (entry != null) {
        return agent.throwException(
            .type_error,
            "Private element '{f}' already exists",
            .{private_name},
        );
    }

    // 5. Append method to O.[[PrivateElements]].
    try self.property_storage.private_elements.putNoClobber(agent.gc_allocator, private_name, method);

    // 6. Return unused.
}

/// 7.3.30 PrivateGet ( O, P )
/// https://tc39.es/ecma262/#sec-privateget
pub fn privateGet(self: *Object, agent: *Agent, private_name: PrivateName) Agent.Error!Value {
    // 1. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name) orelse {
        // 2. If entry is empty, throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Private element '{f}' doesn't exist",
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
                return agent.throwException(
                    .type_error,
                    "Private element '{f}' has no getter",
                    .{private_name},
                );
            };

            // 7. Return ? Call(getter, O).
            return Value.from(getter).callAssumeCallable(agent, Value.from(self), &.{});
        },
    }
}

/// 7.3.31 PrivateSet ( O, P, value )
/// https://tc39.es/ecma262/#sec-privateset
pub fn privateSet(self: *Object, agent: *Agent, private_name: PrivateName, value: Value) Agent.Error!void {
    // 1. Let entry be PrivateElementFind(O, P).
    const entry = self.privateElementFind(private_name) orelse {
        // 2. If entry is empty, throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "Private element '{f}' doesn't exist",
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
            return agent.throwException(
                .type_error,
                "Private element '{f}' is a method and cannot be set",
                .{private_name},
            );
        },

        // 5. Else,
        //     a. Assert: entry.[[Kind]] is accessor.
        .accessor => |get_and_set| {
            // c. Let setter be entry.[[Set]].
            // b. If entry.[[Set]] is undefined, throw a TypeError exception.
            const setter = get_and_set.set orelse {
                return agent.throwException(
                    .type_error,
                    "Private element '{f}' has no setter",
                    .{private_name},
                );
            };

            // d. Perform ? Call(setter, O, « value »).
            _ = try Value.from(setter).callAssumeCallable(
                agent,
                Value.from(self),
                &.{value},
            );
        },
    }

    // 6. Return unused.
}

/// 7.3.32 DefineField ( receiver, fieldRecord )
/// https://tc39.es/ecma262/#sec-definefield
pub fn defineField(self: *Object, agent: *Agent, field: ClassFieldDefinition) Agent.Error!void {
    // 1. Let fieldName be fieldRecord.[[Name]].

    // 2. Let initializer be fieldRecord.[[Initializer]].
    // 3. If initializer is not empty, then
    const init_value: Value = if (field.initializer) |initializer| blk: {
        // a. Let initValue be ? Call(initializer, receiver).
        break :blk try Value.from(&initializer.object).callAssumeCallable(
            agent,
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
            try self.privateFieldAdd(agent, private_name, init_value);
        },
        // 6. Else,
        .property_key => |property_key| {
            // a. Assert: fieldName is a property key.
            // b. Perform ? CreateDataPropertyOrThrow(receiver, fieldName, initValue).
            try self.createDataPropertyOrThrow(agent, property_key, init_value);
        },
    }

    // 7. Return unused.
}

/// 7.3.33 InitializeInstanceElements ( O, constructor )
/// https://tc39.es/ecma262/#sec-initializeinstanceelements
pub fn initializeInstanceElements(
    self: *Object,
    agent: *Agent,
    constructor: *Object,
) Agent.Error!void {
    // 1. Let methods be constructor.[[PrivateMethods]].
    const methods = if (constructor.cast(builtins.ECMAScriptFunction)) |ecmascript_function| blk: {
        break :blk ecmascript_function.fields.private_methods;
    } else if (constructor.cast(builtins.BuiltinFunction)) |builtin_function| blk: {
        const class_constructor_fields = builtin_function.fields.additional_fields.cast(*ClassConstructorFields);
        break :blk class_constructor_fields.private_methods;
    } else unreachable;

    // 2. For each PrivateElement method of methods, do
    for (methods) |method| {
        // a. Perform ? PrivateMethodOrAccessorAdd(O, method).
        try self.privateMethodOrAccessorAdd(agent, method.private_name, method.private_element);
    }

    // 3. Let fields be constructor.[[Fields]].
    const fields = if (constructor.cast(builtins.ECMAScriptFunction)) |ecmascript_function| blk: {
        break :blk ecmascript_function.fields.fields;
    } else if (constructor.cast(builtins.BuiltinFunction)) |builtin_function| blk: {
        const class_constructor_fields = builtin_function.fields.additional_fields.cast(*ClassConstructorFields);
        break :blk class_constructor_fields.fields;
    } else unreachable;

    // 4. For each element fieldRecord of fields, do
    for (fields) |field| {
        // a. Perform ? DefineField(O, fieldRecord).
        try self.defineField(agent, field);
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
    const value = try self.get(agent, PropertyKey.from(property));

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
    const platform = Agent.Platform.default();
    defer platform.deinit();
    var agent_ = try Agent.init(&platform, .{});
    defer agent_.deinit();

    const test_cases = [_]struct { *Object, []const u8 }{
        .{ try ordinaryObjectCreate(&agent_, null), "[object Object]" },
    };
    for (test_cases) |test_case| {
        const object, const expected = test_case;
        try std.testing.expectFmt(expected, "{f}", .{object});
    }
}
