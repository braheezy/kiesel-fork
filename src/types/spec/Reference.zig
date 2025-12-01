//! 6.2.5 The Reference Record Specification Type
//! https://tc39.es/ecma262/#sec-reference-record-specification-type

const std = @import("std");

const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const Object = types.Object;
const PrivateName = types.PrivateName;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;

const Reference = @This();

/// [[Base]]
base: union(enum) {
    value: Value,
    environment: Environment,
    unresolvable,
},

/// [[ReferencedName]]
referenced_name: union(enum) {
    value: Value,
    private_name: PrivateName,
},

/// [[Strict]]
strict: bool,

/// [[ThisValue]]
this_value: ?Value,

maybe_lookup_cache_entry: ?*?Object.Shape.PropertyLookupCacheEntry = null,

/// 6.2.5.1 IsPropertyReference ( V )
/// https://tc39.es/ecma262/#sec-ispropertyreference
pub fn isPropertyReference(self: Reference) bool {
    switch (self.base) {
        // 1. if V.[[Base]] is unresolvable, return false.
        .unresolvable => return false,

        // 2. If V.[[Base]] is an Environment Record, return false; otherwise return true.
        .environment => return false,
        .value => return true,
    }
}

/// 6.2.5.2 IsUnresolvableReference ( V )
/// https://tc39.es/ecma262/#sec-isunresolvablereference
pub fn isUnresolvableReference(self: Reference) bool {
    // 1. If V.[[Base]] is unresolvable, return true; otherwise return false.
    return self.base == .unresolvable;
}

/// 6.2.5.3 IsSuperReference ( V )
/// https://tc39.es/ecma262/#sec-issuperreference
pub fn isSuperReference(self: Reference) bool {
    // 1. If V.[[ThisValue]] is not empty, return true; otherwise return false.
    return self.this_value != null;
}

/// 6.2.5.4 IsPrivateReference ( V )
pub fn isPrivateReference(self: Reference) bool {
    // 1. If V.[[ReferencedName]] is a Private Name, return true; otherwise return false.
    return self.referenced_name == .private_name;
}

/// 6.2.5.5 GetValue ( V )
/// https://tc39.es/ecma262/#sec-getvalue
pub fn getValue(self: Reference, agent: *Agent) Agent.Error!Value {
    // 1. If V is not a Reference Record, return V.
    // NOTE: This is handled at the call site.

    // 2. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
    if (self.isUnresolvableReference()) {
        @branchHint(.unlikely);
        return agent.throwException(
            .reference_error,
            "'{f}' is not defined",
            .{self.referenced_name.value.asString().fmtRaw()},
        );
    }

    // 3. If IsPropertyReference(V) is true, then
    if (self.isPropertyReference()) {
        // a. Let baseObj be ? ToObject(V.[[Base]]).
        // NOTE: For property lookups on primitives we can directly go to the prototype instead
        //       of creating a wrapper object, or return the value for `"string".length`.
        const base_object = switch (self.base.value.type()) {
            .object => self.base.value.asObject(),
            .string => switch (self.referenced_name) {
                .value => |value| blk: {
                    if (value.type() == .string and value.asString().eql(String.fromLiteral("length"))) {
                        return Value.from(self.base.value.asString().length);
                    } else if (value.type() == .symbol) {
                        break :blk (try self.base.value.synthesizePrototype(agent)).?;
                    }
                    // Might be a property handled by String's [[GetOwnProperty]], we need to make an object
                    // TODO: This could have a fast path for numeric property lookups
                    break :blk try self.base.value.toObject(agent);
                },
                .private_name => (try self.base.value.synthesizePrototype(agent)).?,
            },
            // Guaranteed to throw
            .null, .undefined => try self.base.value.toObject(agent),
            else => (try self.base.value.synthesizePrototype(agent)).?,
        };

        if (self.maybe_lookup_cache_entry) |lookup_cache_entry| {
            if (lookup_cache_entry.*) |cache| {
                if (base_object.property_storage.shape == cache.shape) {
                    switch (cache.type) {
                        .value => {
                            const value = base_object.property_storage.properties.items[@intFromEnum(cache.index)].value;
                            return value;
                        },
                        .accessor => {
                            const maybe_getter = base_object.property_storage.properties.items[@intFromEnum(cache.index)].getter_or_setter;
                            // Excerpt from ordinaryGet()
                            const getter = maybe_getter orelse return .undefined;
                            const receiver = self.getThisValue();
                            return Value.from(getter).callAssumeCallable(agent, receiver, &.{});
                        },
                    }
                } else {
                    lookup_cache_entry.* = null;
                }
            }
        }

        // b. If IsPrivateReference(V) is true, then
        if (self.isPrivateReference()) {
            // i. Return ? PrivateGet(baseObj, V.[[ReferencedName]]).
            return base_object.privateGet(agent, self.referenced_name.private_name);
        }

        // c. If V.[[ReferencedName]] is not a property key, then
        const property_key = blk: {
            const property_key_value = self.referenced_name.value;
            if (property_key_value.__isI32() and property_key_value.__asI32() >= 0)
                break :blk PropertyKey.from(@as(u53, @intCast(property_key_value.__asI32())));
            if (property_key_value.isString()) break :blk PropertyKey.from(property_key_value.asString());
            if (property_key_value.isSymbol()) break :blk PropertyKey.from(property_key_value.asSymbol());
            // i. Set V.[[ReferencedName]] to ? ToPropertyKey(V.[[ReferencedName]]).
            break :blk try property_key_value.toPropertyKey(agent);
        };

        // d. Return ? baseObj.[[Get]](V.[[ReferencedName]], GetThisValue(V)).
        const value = try base_object.internal_methods.get(
            agent,
            base_object,
            property_key,
            self.getThisValue(),
        );

        if (self.maybe_lookup_cache_entry) |lookup_cache_entry| {
            const shape = base_object.property_storage.shape;
            if (shape.properties.get(property_key)) |property_metadata| {
                lookup_cache_entry.* = .{
                    .shape = shape,
                    .type = property_metadata.type,
                    .index = property_metadata.index,
                };
            }
        }

        return value;
    } else {
        // 4. Else,
        // a. Let base be V.[[Base]].
        // b. Assert: base is an Environment Record.
        const base = self.base.environment;

        // c. Return ? base.GetBindingValue(V.[[ReferencedName]], V.[[Strict]]) (see 9.1).
        return base.getBindingValue(agent, self.referenced_name.value.asString(), self.strict);
    }
}

/// 6.2.5.6 PutValue ( V, W )
/// https://tc39.es/ecma262/#sec-putvalue
pub fn putValue(self: Reference, agent: *Agent, value: Value) Agent.Error!void {
    // 1. If V is not a Reference Record, throw a ReferenceError exception.
    // NOTE: This is handled at the call site.

    // 2. If IsUnresolvableReference(V) is true, then
    if (self.isUnresolvableReference()) {
        @branchHint(.unlikely);
        // a. If V.[[Strict]] is true, throw a ReferenceError exception.
        if (self.strict) {
            return agent.throwException(
                .reference_error,
                "'{f}' is not defined",
                .{self.referenced_name.value.asString().fmtEscaped()},
            );
        }

        // b. Let globalObj be GetGlobalObject().
        const global_obj = agent.getGlobalObject();

        // c. Perform ? Set(globalObj, V.[[ReferencedName]], W, false).
        try global_obj.set(
            agent,
            PropertyKey.from(self.referenced_name.value.asString()),
            value,
            .ignore,
        );

        // d. Return unused.
        return;
    }

    // 3. If IsPropertyReference(V) is true, then
    if (self.isPropertyReference()) {
        // a. Let baseObj be ? ToObject(V.[[Base]]).
        var base_object = try self.base.value.toObject(agent);

        // b. If IsPrivateReference(V) is true, then
        if (self.isPrivateReference()) {
            // i. Return ? PrivateSet(baseObj, V.[[ReferencedName]], W).
            return base_object.privateSet(agent, self.referenced_name.private_name, value);
        }

        // c. If V.[[ReferencedName]] is not a property key, then
        const property_key = blk: {
            const property_key_value = self.referenced_name.value;
            if (property_key_value.__isI32() and property_key_value.__asI32() >= 0)
                break :blk PropertyKey.from(@as(u53, @intCast(property_key_value.__asI32())));
            if (property_key_value.isString()) break :blk PropertyKey.from(property_key_value.asString());
            if (property_key_value.isSymbol()) break :blk PropertyKey.from(property_key_value.asSymbol());
            // i. Set V.[[ReferencedName]] to ? ToPropertyKey(V.[[ReferencedName]]).
            break :blk try property_key_value.toPropertyKey(agent);
        };

        // d. Let succeeded be ? baseObj.[[Set]](V.[[ReferencedName]], W, GetThisValue(V)).
        const succeeded = try base_object.internal_methods.set(
            agent,
            base_object,
            property_key,
            value,
            self.getThisValue(),
        );

        // e. If succeeded is false and V.[[Strict]] is true, throw a TypeError exception.
        if (!succeeded and self.strict) {
            @branchHint(.unlikely);
            return agent.throwException(.type_error, "Could not set property", .{});
        }

        // f. Return unused.
        return;
    }

    // 4. Else,
    // a. Let base be V.[[Base]].
    // b. Assert: base is an Environment Record.
    const base = self.base.environment;

    // c. Return ? base.SetMutableBinding(V.[[ReferencedName]], W, V.[[Strict]]) (see 9.1).
    return base.setMutableBinding(agent, self.referenced_name.value.asString(), value, self.strict);
}

/// 6.2.5.7 GetThisValue ( V )
/// https://tc39.es/ecma262/#sec-getthisvalue
pub fn getThisValue(self: Reference) Value {
    // 1. Assert: IsPropertyReference(V) is true.
    std.debug.assert(self.isPropertyReference());

    // 2. If IsSuperReference(V) is true, return V.[[ThisValue]]; otherwise return V.[[Base]].
    return if (self.isSuperReference()) self.this_value.? else self.base.value;
}

/// 6.2.5.8 InitializeReferencedBinding ( V, W )
/// https://tc39.es/ecma262/#sec-initializereferencedbinding
pub fn initializeReferencedBinding(self: Reference, agent: *Agent, value: Value) Agent.Error!void {
    // 1. Assert: IsUnresolvableReference(V) is false.
    std.debug.assert(!self.isUnresolvableReference());

    // 2. Let base be V.[[Base]].
    // 3. Assert: base is an Environment Record.
    const base = self.base.environment;

    // 4. Return ? base.InitializeBinding(V.[[ReferencedName]], W).
    return base.initializeBinding(agent, self.referenced_name.value.asString(), value);
}
