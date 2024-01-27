//! 6.2.5 The Reference Record Specification Type
//! https://tc39.es/ecma262/#sec-reference-record-specification-type

const std = @import("std");

const execution = @import("../../execution.zig");
const language = @import("../language.zig");

const Agent = execution.Agent;
const Environment = execution.Environment;
const PropertyKey = language.Object.PropertyKey;
const String = language.String;
const Symbol = language.Symbol;
const Value = language.Value;

const Self = @This();

/// [[Base]]
base: union(enum) {
    value: Value,
    environment: Environment,
    unresolvable,
},

/// [[ReferencedName]]
referenced_name: union(enum) {
    string: []const u8,
    symbol: Symbol,
    private_name: void, // TODO: Implement private names
},

/// [[Strict]]
strict: bool,

/// [[ThisValue]]
this_value: ?Value,

/// 6.2.5.1 IsPropertyReference ( V )
/// https://tc39.es/ecma262/#sec-ispropertyreference
pub fn isPropertyReference(self: Self) bool {
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
pub fn isUnresolvableReference(self: Self) bool {
    // 1. If V.[[Base]] is unresolvable, return true; otherwise return false.
    return self.base == .unresolvable;
}

/// 6.2.5.3 IsSuperReference ( V )
/// https://tc39.es/ecma262/#sec-issuperreference
pub fn isSuperReference(self: Self) bool {
    // 1. If V.[[ThisValue]] is not empty, return true; otherwise return false.
    return self.this_value != null;
}

/// 6.2.5.4 IsPrivateReference ( V )
pub fn isPrivateReference(self: Self) bool {
    // 1. If V.[[ReferencedName]] is a Private Name, return true; otherwise return false.
    return self.referenced_name == .private_name;
}

/// 6.2.5.5 GetValue ( V )
/// https://tc39.es/ecma262/#sec-getvalue
pub fn getValue(self: Self, agent: *Agent) Agent.Error!Value {
    const realm = agent.currentRealm();
    _ = realm;

    // 1. If V is not a Reference Record, return V.
    // NOTE: This is handled at the call site.

    // 2. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
    if (self.isUnresolvableReference()) {
        return switch (self.referenced_name) {
            .string => |string| agent.throwException(
                .reference_error,
                "'{s}' is not defined",
                .{string},
            ),
            else => agent.throwException(.reference_error, "Cannot resolve reference", .{}),
        };
    }

    // 3. If IsPropertyReference(V) is true, then
    if (self.isPropertyReference()) {
        const referenced_name = switch (self.referenced_name) {
            .string => |string| PropertyKey.from(string),
            .symbol => |symbol| PropertyKey.from(symbol),
            .private_name => unreachable,
        };

        // a. Let baseObj be ? ToObject(V.[[Base]]).
        // NOTE: For property lookups on primitives we can directly go to the prototype instead
        //       of creating a wrapper object, unless we're doing a numeric or 'length' property
        //       lookup on a string.
        const base_object = (if (self.base.value != .string or switch (referenced_name) {
            // TODO: Avoid creating the object just for "string".length lookups
            .string => |string| !string.eql(String.from("length")),
            .integer_index => false,
            .symbol => true,
        })
            try self.base.value.synthesizePrototype(agent)
        else
            null) orelse try self.base.value.toObject(agent);

        // b. If IsPrivateReference(V) is true, then
        if (self.isPrivateReference()) {
            // TODO: i. Return ? PrivateGet(baseObj, V.[[ReferencedName]]).
            @panic("Not implemented");
        }

        // c. Return ? baseObj.[[Get]](V.[[ReferencedName]], GetThisValue(V)).
        return base_object.internalMethods().get(
            base_object,
            referenced_name,
            self.getThisValue(),
        );
    }
    // 4. Else,
    else {
        // a. Let base be V.[[Base]].
        // b. Assert: base is an Environment Record.
        const base = self.base.environment;

        // c. Return ? base.GetBindingValue(V.[[ReferencedName]], V.[[Strict]]) (see 9.1).
        return base.getBindingValue(agent, self.referenced_name.string, self.strict);
    }
}

/// 6.2.5.6 PutValue ( V, W )
/// https://tc39.es/ecma262/#sec-putvalue
pub fn putValue(self: Self, agent: *Agent, value: Value) Agent.Error!void {
    // 1. If V is not a Reference Record, throw a ReferenceError exception.
    // NOTE: This is handled at the call site.

    // 2. If IsUnresolvableReference(V) is true, then
    if (self.isUnresolvableReference()) {
        // a. If V.[[Strict]] is true, throw a ReferenceError exception.
        if (self.strict) {
            return agent.throwException(
                .reference_error,
                "'{s}' is not defined",
                .{self.referenced_name.string},
            );
        }

        // b. Let globalObj be GetGlobalObject().
        const global_obj = agent.getGlobalObject();

        // c. Perform ? Set(globalObj, V.[[ReferencedName]], W, false).
        try global_obj.set(PropertyKey.from(self.referenced_name.string), value, .ignore);

        // d. Return unused.
        return;
    }

    // 3. If IsPropertyReference(V) is true, then
    if (self.isPropertyReference()) {
        // a. Let baseObj be ? ToObject(V.[[Base]]).
        const base_object = try self.base.value.toObject(agent);

        // b. If IsPrivateReference(V) is true, then
        if (self.isPrivateReference()) {
            // TODO: i. Return ? PrivateSet(baseObj, V.[[ReferencedName]], W).
            @panic("Not implemented");
        }

        // c. Let succeeded be ? baseObj.[[Set]](V.[[ReferencedName]], W, GetThisValue(V)).
        const referenced_name = switch (self.referenced_name) {
            .string => |string| PropertyKey.from(string),
            .symbol => |symbol| PropertyKey.from(symbol),
            .private_name => unreachable,
        };
        const succeeded = try base_object.internalMethods().set(
            base_object,
            referenced_name,
            value,
            self.getThisValue(),
        );

        // d. If succeeded is false and V.[[Strict]] is true, throw a TypeError exception.
        if (!succeeded and self.strict) {
            return agent.throwException(.type_error, "Could not set property", .{});
        }

        // e. Return unused.
        return;
    }

    // 4. Else,
    // a. Let base be V.[[Base]].
    // b. Assert: base is an Environment Record.
    const base = self.base.environment;

    // c. Return ? base.SetMutableBinding(V.[[ReferencedName]], W, V.[[Strict]]) (see 9.1).
    const referenced_name = self.referenced_name.string;
    return base.setMutableBinding(agent, referenced_name, value, self.strict);
}

/// 6.2.5.7 GetThisValue ( V )
/// https://tc39.es/ecma262/#sec-getthisvalue
pub fn getThisValue(self: Self) Value {
    // 1. Assert: IsPropertyReference(V) is true.
    std.debug.assert(self.isPropertyReference());

    // 2. If IsSuperReference(V) is true, return V.[[ThisValue]]; otherwise return V.[[Base]].
    return if (self.isSuperReference()) self.this_value.? else self.base.value;
}

/// 6.2.5.8 InitializeReferencedBinding ( V, W )
/// https://tc39.es/ecma262/#sec-initializereferencedbinding
pub fn initializeReferencedBinding(self: Self, agent: *Agent, value: Value) Agent.Error!void {
    // 1. Assert: IsUnresolvableReference(V) is false.
    std.debug.assert(!self.isUnresolvableReference());

    // 2. Let base be V.[[Base]].
    // 3. Assert: base is an Environment Record.
    const base = self.base.environment;

    // 4. Return ? base.InitializeBinding(V.[[ReferencedName]], W).
    return base.initializeBinding(agent, self.referenced_name.string, value);
}
