//! 20.4 Symbol Objects
//! https://tc39.es/ecma262/#sec-symbol-objects

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;

/// 20.4.2 Properties of the Symbol Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-symbol-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            0,
            "Symbol",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        // 20.4.2.1 Symbol.asyncIterator
        // https://tc39.es/ecma262/#sec-symbol.asynciterator
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "asyncIterator",
            Value.from(agent.well_known_symbols.@"%Symbol.asyncIterator%"),
            .none,
        );

        try object.defineBuiltinFunction(agent, "for", @"for", 1, realm);

        // 20.4.2.3 Symbol.hasInstance
        // https://tc39.es/ecma262/#sec-symbol.hasinstance
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "hasInstance",
            Value.from(agent.well_known_symbols.@"%Symbol.hasInstance%"),
            .none,
        );

        // 20.4.2.4 Symbol.isConcatSpreadable
        // https://tc39.es/ecma262/#sec-symbol.isconcatspreadable
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "isConcatSpreadable",
            Value.from(agent.well_known_symbols.@"%Symbol.isConcatSpreadable%"),
            .none,
        );

        // 20.4.2.5 Symbol.iterator
        // https://tc39.es/ecma262/#sec-symbol.iterator
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "iterator",
            Value.from(agent.well_known_symbols.@"%Symbol.iterator%"),
            .none,
        );

        try object.defineBuiltinFunction(agent, "keyFor", keyFor, 1, realm);

        // 20.4.2.7 Symbol.match
        // https://tc39.es/ecma262/#sec-symbol.match
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "match",
            Value.from(agent.well_known_symbols.@"%Symbol.match%"),
            .none,
        );

        // 20.4.2.8 Symbol.matchAll
        // https://tc39.es/ecma262/#sec-symbol.matchall
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "matchAll",
            Value.from(agent.well_known_symbols.@"%Symbol.matchAll%"),
            .none,
        );

        // 20.4.2.9 Symbol.prototype
        // https://tc39.es/ecma262/#sec-symbol.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%Symbol.prototype%"()),
            .none,
        );

        // 20.4.2.10 Symbol.replace
        // https://tc39.es/ecma262/#sec-symbol.replace
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "replace",
            Value.from(agent.well_known_symbols.@"%Symbol.replace%"),
            .none,
        );

        // 20.4.2.11 Symbol.search
        // https://tc39.es/ecma262/#sec-symbol.search
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "search",
            Value.from(agent.well_known_symbols.@"%Symbol.search%"),
            .none,
        );

        // 20.4.2.12 Symbol.species
        // https://tc39.es/ecma262/#sec-symbol.species
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "species",
            Value.from(agent.well_known_symbols.@"%Symbol.species%"),
            .none,
        );

        // 20.4.2.13 Symbol.split
        // https://tc39.es/ecma262/#sec-symbol.split
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "split",
            Value.from(agent.well_known_symbols.@"%Symbol.split%"),
            .none,
        );

        // 20.4.2.14 Symbol.toPrimitive
        // https://tc39.es/ecma262/#sec-symbol.toprimitive
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "toPrimitive",
            Value.from(agent.well_known_symbols.@"%Symbol.toPrimitive%"),
            .none,
        );

        // 20.4.2.15 Symbol.toStringTag
        // https://tc39.es/ecma262/#sec-symbol.tostringtag
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "toStringTag",
            Value.from(agent.well_known_symbols.@"%Symbol.toStringTag%"),
            .none,
        );

        // 20.4.2.16 Symbol.unscopables
        // https://tc39.es/ecma262/#sec-symbol.unscopables
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "unscopables",
            Value.from(agent.well_known_symbols.@"%Symbol.unscopables%"),
            .none,
        );
    }

    /// 20.4.1.1 Symbol ( [ description ] )
    /// https://tc39.es/ecma262/#sec-symbol-description
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const description = arguments.get(0);

        // 1. If NewTarget is not undefined, throw a TypeError exception.
        if (new_target != null) {
            return agent.throwException(.type_error, "Symbol is not a constructor", .{});
        }

        const description_string = blk: {
            // 2. If description is undefined, let descString be undefined.
            if (description.isUndefined()) break :blk null;

            // 3. Else, let descString be ? ToString(description).
            break :blk try description.toString(agent);
        };

        // 4. Return a new Symbol whose [[Description]] is descString.
        return Value.from(try types.Symbol.init(agent.gc_allocator, description_string));
    }

    /// 20.4.2.2 Symbol.for ( key )
    /// https://tc39.es/ecma262/#sec-symbol.for
    fn @"for"(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const key = arguments.get(0);

        // 1. Let stringKey be ? ToString(key).
        const string_key = try key.toString(agent);

        // 2. For each element e of the GlobalSymbolRegistry List, do
        //     a. If e.[[Key]] is stringKey, return e.[[Symbol]].
        const gop = try agent.global_symbol_registry.getOrPut(agent.gc_allocator, string_key);
        if (gop.found_existing) return Value.from(gop.value_ptr.*);

        // 3. Assert: The GlobalSymbolRegistry List does not currently contain an entry for
        //    stringKey.

        // 4. Let newSymbol be a new Symbol whose [[Description]] is stringKey.
        const new_symbol = try types.Symbol.init(agent.gc_allocator, string_key);

        // 5. Append the GlobalSymbolRegistry Record { [[Key]]: stringKey, [[Symbol]]: newSymbol }
        //    to the GlobalSymbolRegistry List.
        gop.value_ptr.* = new_symbol;

        // 6. Return newSymbol.
        return Value.from(new_symbol);
    }

    /// 20.4.2.6 Symbol.keyFor ( sym )
    /// https://tc39.es/ecma262/#sec-symbol.keyfor
    fn keyFor(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const symbol = arguments.get(0);

        // 1. If sym is not a Symbol, throw a TypeError exception.
        if (!symbol.isSymbol()) {
            return agent.throwException(.type_error, "{f} is not a Symbol", .{symbol});
        }

        // 2. Return KeyForSymbol(sym).
        return Value.from(keyForSymbol(agent, symbol.asSymbol()) orelse return .undefined);
    }
};

/// 20.4.3 Properties of the Symbol Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-symbol-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinAccessor(agent, "description", description, null, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinFunction(agent, "valueOf", valueOf, 0, realm);
        try object.defineBuiltinFunctionWithAttributes(
            agent,
            "%Symbol.toPrimitive%",
            @"%Symbol.toPrimitive%",
            1,
            realm,
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        // 20.4.3.1 Symbol.prototype.constructor
        // https://tc39.es/ecma262/#sec-symbol.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%Symbol%"()),
        );

        // 20.4.3.6 Symbol.prototype [ %Symbol.toStringTag% ]
        // https://tc39.es/ecma262/#sec-symbol.prototype-%symbol.tostringtag%
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("Symbol"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );
    }

    /// 20.4.3.4.1 ThisSymbolValue ( value )
    /// https://tc39.es/ecma262/#sec-thissymbolvalue
    fn thisSymbolValue(agent: *Agent, value: Value) error{ExceptionThrown}!*const types.Symbol {
        // 1. If value is a Symbol, return value.
        if (value.isSymbol()) return value.asSymbol();

        // 2. If value is an Object and value has a [[SymbolData]] internal slot, then
        if (value.isObject() and value.asObject().is(Symbol)) {
            // a. Let s be value.[[SymbolData]].
            // b. Assert: s is a Symbol.
            const s = value.asObject().as(Symbol).fields.symbol_data;

            // c. Return s.
            return s;
        }

        // 3. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "This value must be a symbol or Symbol object",
            .{},
        );
    }

    /// 20.4.3.2 get Symbol.prototype.description
    /// https://tc39.es/ecma262/#sec-symbol.prototype.description
    fn description(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let s be the this value.
        // 2. Let sym be ? ThisSymbolValue(s).
        const symbol = try thisSymbolValue(agent, this_value);

        // 3. Return sym.[[Description]].
        return Value.from(symbol.description orelse return .undefined);
    }

    /// 20.4.3.3 Symbol.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-symbol.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let sym be ? ThisSymbolValue(this value).
        const symbol = try thisSymbolValue(agent, this_value);

        // 2. Return SymbolDescriptiveString(sym).
        return Value.from(try symbol.descriptiveString(agent));
    }

    /// 20.4.3.4 Symbol.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-symbol.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? ThisSymbolValue(this value).
        return Value.from(try thisSymbolValue(agent, this_value));
    }

    /// 20.4.3.5 Symbol.prototype [ %Symbol.toPrimitive% ] ( hint )
    /// https://tc39.es/ecma262/#sec-symbol.prototype-%symbol.toprimitive%
    fn @"%Symbol.toPrimitive%"(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return ? ThisSymbolValue(this value).
        // NOTE: The argument is ignored.
        return Value.from(try thisSymbolValue(agent, this_value));
    }
};

/// 20.4.4 Properties of Symbol Instances
/// https://tc39.es/ecma262/#sec-properties-of-symbol-instances
pub const Symbol = MakeObject(.{
    .Fields = struct {
        /// [[SymbolData]]
        symbol_data: *const types.Symbol,
    },
    .tag = .symbol,
});

/// 20.4.5.1 KeyForSymbol ( sym )
/// https://tc39.es/ecma262/#sec-keyforsymbol
pub fn keyForSymbol(agent: *Agent, symbol: *const types.Symbol) ?*const String {
    // 1. For each element e of the GlobalSymbolRegistry List, do
    var it = agent.global_symbol_registry.iterator();
    while (it.next()) |entry| {
        // a. If SameValue(e.[[Symbol]], sym) is true, return e.[[Key]].
        if (entry.value_ptr.* == symbol) return entry.key_ptr.*;
    }

    // 2. Assert: The GlobalSymbolRegistry List does not currently contain an entry for sym.
    // 3. Return undefined.
    return null;
}
