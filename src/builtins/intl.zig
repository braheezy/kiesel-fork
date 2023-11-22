//! 8 The Intl Object
//! https://tc39.es/ecma402/#intl-object

const std = @import("std");

const Allocator = std.mem.Allocator;

const icu4zig = @import("icu4zig");

const abstract_operations = @import("intl/abstract_operations.zig");
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
const canonicalizeLocaleList = abstract_operations.canonicalizeLocaleList;
const createArrayFromListMapToValue = types.createArrayFromListMapToValue;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;

comptime {
    const build_options = @import("build-options");
    if (!build_options.enable_intl) @compileError("Intl is not enabled");
}

pub const Intl = struct {
    pub const Locale = @import("./intl/locale.zig").Locale;
    pub const LocaleConstructor = @import("./intl/locale.zig").LocaleConstructor;
    pub const LocalePrototype = @import("./intl/locale.zig").LocalePrototype;

    pub fn create(realm: *Realm) Allocator.Error!Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinFunction(object, "getCanonicalLocales", getCanonicalLocales, 1, realm);

        // 8.1.1 Intl[ @@toStringTag ]
        // https://tc39.es/ecma402/#sec-Intl-toStringTag
        try defineBuiltinProperty(object, "@@toStringTag", PropertyDescriptor{
            .value = Value.from("Intl"),
            .writable = false,
            .enumerable = false,
            .configurable = true,
        });

        // 8.2.5 Intl.Locale ( . . . )
        // https://tc39.es/ecma402/#sec-intl.locale-intro
        try defineBuiltinProperty(object, "Locale", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%Intl.Locale%"()),
            .writable = true,
            .enumerable = false,
            .configurable = true,
        });

        return object;
    }

    fn getCanonicalLocales(agent: *Agent, _: Value, arguments: ArgumentsList) Agent.Error!Value {
        const locales = arguments.get(0);

        // 1. Let ll be ? CanonicalizeLocaleList(locales).
        const locale_list = try canonicalizeLocaleList(agent, locales);
        defer locale_list.deinit();

        // 2. Return CreateArrayFromList(ll).
        return Value.from(
            try createArrayFromListMapToValue(agent, icu4zig.Locale, locale_list.items, struct {
                fn mapFn(agent_: *Agent, locale: icu4zig.Locale) Allocator.Error!Value {
                    return Value.from(try locale.toString(agent_.gc_allocator));
                }
            }.mapFn),
        );
    }
};
