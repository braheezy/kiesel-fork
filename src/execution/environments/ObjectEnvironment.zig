//! 9.1.1.2 Object Environment Records
//! https://tc39.es/ecma262/#sec-object-environment-records

const environments = @import("../environments.zig");
const types = @import("../../types.zig");

const Environment = environments.Environment;
const Object = types.Object;
const PropertyKey = types.PropertyKey;

const Self = @This();

/// [[BindingObject]]
binding_object: Object,

/// [[IsWithEnvironment]]
is_with_environment: bool,

/// [[OuterEnv]]
outer_env: ?Environment,

/// 9.1.1.2.1 HasBinding ( N )
/// https://tc39.es/ecma262/#sec-object-environment-records-hasbinding-n
pub fn hasBinding(self: Self, name: []const u8) !bool {
    const agent = self.binding_object.agent();

    // 1. Let bindingObject be envRec.[[BindingObject]].
    // 2. Let foundBinding be ? HasProperty(bindingObject, N).
    const found_binding = try self.binding_object.hasProperty(PropertyKey.from(name));

    // 3. If foundBinding is false, return false.
    if (!found_binding)
        return false;

    // 4. If envRec.[[IsWithEnvironment]] is false, return true.
    if (!self.is_with_environment)
        return true;

    // 5. Let unscopables be ? Get(bindingObject, @@unscopables).
    const unscopables = try self.binding_object.get(
        PropertyKey.from(agent.well_known_symbols.@"@@unscopables"),
    );

    // 6. If unscopables is an Object, then
    if (unscopables == .object) {
        // a. Let blocked be ToBoolean(? Get(unscopables, N)).
        const blocked = (try unscopables.object.get(PropertyKey.from(name))).toBoolean();

        // b. If blocked is true, return false.
        if (blocked)
            return false;
    }

    // 7. Return true.
    return true;
}

/// 9.1.1.2.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-object-environment-records-hasthisbinding
pub fn hasThisBinding(_: Self) bool {
    // 1. Return false.
    return false;
}
