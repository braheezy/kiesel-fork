//! 20.4 Symbol Objects
//! https://tc39.es/ecma262/#sec-symbol-objects

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;

/// 20.4.2 Properties of the Symbol Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-symbol-constructor
pub const SymbolConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const agent = realm.agent;
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 0,
            .name = "Symbol",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 20.4.2.1 Symbol.asyncIterator
        // https://tc39.es/ecma262/#sec-symbol.asynciterator
        try defineBuiltinProperty(object, "asyncIterator", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@asyncIterator"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.3 Symbol.hasInstance
        // https://tc39.es/ecma262/#sec-symbol.hasinstance
        try defineBuiltinProperty(object, "hasInstance", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@hasInstance"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.4 Symbol.isConcatSpreadable
        // https://tc39.es/ecma262/#sec-symbol.isconcatspreadable
        try defineBuiltinProperty(object, "isConcatSpreadable", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@isConcatSpreadable"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.5 Symbol.iterator
        // https://tc39.es/ecma262/#sec-symbol.iterator
        try defineBuiltinProperty(object, "iterator", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@iterator"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.7 Symbol.match
        // https://tc39.es/ecma262/#sec-symbol.match
        try defineBuiltinProperty(object, "match", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@match"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.8 Symbol.matchAll
        // https://tc39.es/ecma262/#sec-symbol.matchall
        try defineBuiltinProperty(object, "matchAll", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@matchAll"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.9 Symbol.prototype
        // https://tc39.es/ecma262/#sec-symbol.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Symbol.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.10 Symbol.replace
        // https://tc39.es/ecma262/#sec-symbol.replace
        try defineBuiltinProperty(object, "replace", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@replace"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.11 Symbol.search
        // https://tc39.es/ecma262/#sec-symbol.search
        try defineBuiltinProperty(object, "search", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@search"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.12 Symbol.species
        // https://tc39.es/ecma262/#sec-symbol.species
        try defineBuiltinProperty(object, "species", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@species"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.13 Symbol.split
        // https://tc39.es/ecma262/#sec-symbol.split
        try defineBuiltinProperty(object, "split", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@split"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.14 Symbol.toPrimitive
        // https://tc39.es/ecma262/#sec-symbol.toprimitive
        try defineBuiltinProperty(object, "toPrimitive", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@toPrimitive"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.15 Symbol.toStringTag
        // https://tc39.es/ecma262/#sec-symbol.tostringtag
        try defineBuiltinProperty(object, "toStringTag", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@toStringTag"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.2.16 Symbol.unscopables
        // https://tc39.es/ecma262/#sec-symbol.unscopables
        try defineBuiltinProperty(object, "unscopables", PropertyDescriptor{
            .value = Value.from(agent.well_known_symbols.@"@@unscopables"),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.4.3.1 Symbol.prototype.constructor
        // https://tc39.es/ecma262/#sec-symbol.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Symbol.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 20.4.1.1 Symbol ( [ description ] )
    /// https://tc39.es/ecma262/#sec-symbol-description
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const description = arguments.get(0);

        // 1. If NewTarget is not undefined, throw a TypeError exception.
        if (new_target != null) {
            return agent.throwException(.type_error, "Symbol is not a constructor");
        }

        const description_string = blk: {
            // 2. If description is undefined, let descString be undefined.
            if (description == .undefined) break :blk null;

            // 3. Else, let descString be ? ToString(description).
            break :blk try description.toString(agent);
        };

        // 4. Return a new Symbol whose [[Description]] is descString.
        return Value.from(agent.createSymbol(description_string) catch |err| switch (err) {
            error.Overflow => return agent.throwException(
                .internal_error,
                "Maximum number of symbols exceeded",
            ),
        });
    }
};

/// 20.4.3 Properties of the Symbol Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-symbol-prototype-object
pub const SymbolPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunction(object, "valueOf", valueOf, 0, realm);
        try defineBuiltinFunction(object, "@@toPrimitive", @"@@toPrimitive", 1, realm);

        // 20.4.3.6 Symbol.prototype [ @@toStringTag ]
        // https://tc39.es/ecma262/#sec-symbol.prototype-@@tostringtag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("String"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    /// https://tc39.es/ecma262/#thissymbolvalue
    fn thisSymbolValue(agent: *Agent, value: Value) !types.Symbol {
        switch (value) {
            // 1. If value is a Symbol, return value.
            .symbol => |symbol| return symbol,

            // 2. If value is an Object and value has a [[SymbolData]] internal slot, then
            .object => |object| if (object.is(Symbol)) {
                // a. Let s be value.[[SymbolData]].
                // b. Assert: s is a Symbol.
                const s = object.as(Symbol).fields.symbol_data;

                // c. Return s.
                return s;
            },

            else => {},
        }

        // 3. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            "This value must be a symbol or Symbol object",
        );
    }

    /// 20.4.3.3 Symbol.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-symbol.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let sym be ? thisSymbolValue(this value).
        const symbol = try thisSymbolValue(agent, this_value);

        // 2. Return SymbolDescriptiveString(sym).
        return Value.from(try symbol.descriptiveString(agent));
    }

    /// 20.4.3.4 Symbol.prototype.valueOf ( )
    /// https://tc39.es/ecma262/#sec-symbol.prototype.valueof
    fn valueOf(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? thisSymbolValue(this value).
        return Value.from(try thisSymbolValue(agent, this_value));
    }

    /// 20.4.3.5 Symbol.prototype [ @@toPrimitive ] ( hint )
    /// https://tc39.es/ecma262/#sec-symbol.prototype-@@toprimitive
    fn @"@@toPrimitive"(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Return ? thisSymbolValue(this value).
        // NOTE: The argument is ignored.
        return Value.from(try thisSymbolValue(agent, this_value));
    }
};

/// 20.4.4 Properties of Symbol Instances
/// https://tc39.es/ecma262/#sec-properties-of-symbol-instances
pub const Symbol = Object.Factory(.{
    .Fields = struct {
        /// [[SymbolData]]
        symbol_data: types.Symbol,
    },
    .tag = .symbol,
});
