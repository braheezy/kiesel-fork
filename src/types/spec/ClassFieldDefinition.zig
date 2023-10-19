//! 6.2.11 The ClassFieldDefinition Record Specification Type
//! https://tc39.es/ecma262/#sec-classfielddefinition-record-specification-type

const builtins = @import("../../builtins.zig");
const types = @import("../../types.zig");

const PropertyKey = types.PropertyKey;

/// [[Name]]
name: union(enum) {
    property_key: PropertyKey,
    private_name: void, // TODO: Implement private names
},

/// [[Initializer]]
initializer: ?*builtins.ECMAScriptFunction,
