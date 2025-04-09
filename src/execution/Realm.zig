//! 9.3 Realms
//! https://tc39.es/ecma262/#sec-code-realms

const std = @import("std");

const ast = @import("../language/ast.zig");
const builtins = @import("../builtins.zig");
const environments = @import("environments.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = @import("Agent.zig");
const ExecutionContext = @import("ExecutionContext.zig");
const GlobalEnvironment = environments.GlobalEnvironment;
const Module = language.Module;
const ModuleRequest = language.ModuleRequest;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const SafePointer = types.SafePointer;
const String = types.String;
const addRestrictedFunctionProperties = builtins.addRestrictedFunctionProperties;
const globalObjectProperties = builtins.globalObjectProperties;
const newGlobalEnvironment = environments.newGlobalEnvironment;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

pub const Intrinsics = @import("Realm/Intrinsics.zig");

const Realm = @This();

/// [[AgentSignifier]]
agent: *Agent,

/// [[Intrinsics]]
intrinsics: Intrinsics,

/// [[GlobalObject]]
global_object: *Object,

/// [[GlobalEnv]]
global_env: *GlobalEnvironment,

/// [[TemplateMap]]
template_map: std.AutoHashMapUnmanaged(*ast.TemplateLiteral, *Object),

/// [[LoadedModules]]
loaded_modules: ModuleRequest.HashMapUnmanaged(Module),

/// [[HostDefined]]
host_defined: SafePointer,

/// Non-standard, needed for `Math.random()`
rng: std.Random.DefaultPrng,

/// 9.3.1 InitializeHostDefinedRealm ( )
/// https://tc39.es/ecma262/#sec-initializehostdefinedrealm
pub fn initializeHostDefinedRealm(
    agent: *Agent,
    args: struct {
        global_object: ?*Object = null,
        this_value: ?*Object = null,
    },
) Agent.Error!void {
    // 1. Let realm be a new Realm Record.
    var realm = try agent.gc_allocator.create(Realm);

    // Set this early, it'll be accessed before the realm struct is fully initialized.
    realm.agent = agent;

    // 2. Perform CreateIntrinsics(realm).
    try realm.createIntrinsics();

    realm.* = .{
        .intrinsics = realm.intrinsics,
        .rng = .init(@intFromPtr(realm)),
        .global_object = undefined,
        .global_env = undefined,

        // 3. Set realm.[[AgentSignifier]] to AgentSignifier().
        .agent = realm.agent,

        // 4. Set realm.[[TemplateMap]] to a new empty List.
        .template_map = .empty,

        .loaded_modules = .empty,
        .host_defined = .null_pointer,
    };

    // 5. Let newContext be a new execution context.
    const new_context = try agent.gc_allocator.create(ExecutionContext);
    new_context.* = .{
        // 6. Set the Function of newContext to null.
        .origin = .realm,

        // 7. Set the Realm of newContext to realm.
        .realm = realm,

        // 8. Set the ScriptOrModule of newContext to null.
        .script_or_module = null,

        .ecmascript_code = undefined,
    };

    // 9. Push newContext onto the execution context stack; newContext is now the running
    //    execution context.
    try agent.execution_context_stack.append(agent.gc_allocator, new_context);

    // 10. If the host requires use of an exotic object to serve as realm's global object, then
    //     a. Let global be such an object created in a host-defined manner.
    // 11. Else,
    //     a. Let global be OrdinaryObjectCreate(realm.[[Intrinsics]].[[%Object.prototype%]]).
    const global = args.global_object orelse try ordinaryObjectCreate(
        agent,
        try realm.intrinsics.@"%Object.prototype%"(),
    );
    global.property_storage.shape = try global.property_storage.shape.makeUnique(agent.gc_allocator);

    // 12. If the host requires that the this binding in realm's global scope return an object
    //     other than the global object, then
    //     a. Let thisValue be such an object created in a host-defined manner.
    // 13. Else,
    //     a. Let thisValue be global.
    const this_value = args.this_value orelse global;

    // 14. Set realm.[[GlobalObject]] to global.
    realm.global_object = global;

    // 15. Set realm.[[GlobalEnv]] to NewGlobalEnvironment(global, thisValue).
    realm.global_env = try newGlobalEnvironment(agent.gc_allocator, global, this_value);

    // 16. Perform ? SetDefaultGlobalBindings(realm).
    try realm.setDefaultGlobalBindings();

    // 17. Create any host-defined global object properties on global.
    // 18. Return unused.
}

/// 9.3.2 CreateIntrinsics ( realmRec )
/// https://tc39.es/ecma262/#sec-createintrinsics
fn createIntrinsics(self: *Realm) std.mem.Allocator.Error!void {
    // 1. Set realmRec.[[Intrinsics]] to a new Record.
    self.intrinsics = .{ .realm = self };

    // Ensure %Object.prototype% exists before %Function.prototype% is created, otherwise the
    // latter will be created twice.
    _ = try self.intrinsics.@"%Object.prototype%"();

    // 2. Set fields of realmRec.[[Intrinsics]] with the values listed in Table 6. The field
    //    names are the names listed in column one of the table. The value of each field is a new
    //    object value fully and recursively populated with property values as defined by the
    //    specification of each object in clauses 19 through 28. All object property values are
    //    newly created object values. All values that are built-in function objects are created by
    //    performing CreateBuiltinFunction(steps, length, name, slots, realmRec, prototype) where
    //    steps is the definition of that function provided by this specification, name is the
    //    initial value of the function's "name" property, length is the initial value of the
    //    function's "length" property, slots is a list of the names, if any, of the function's
    //    specified internal slots, and prototype is the specified value of the function's
    //    [[Prototype]] internal slot. The creation of the intrinsics and their properties must be
    //    ordered to avoid any dependencies upon objects that have not yet been created.
    // NOTE: Intrinsics are lazily allocated, see the struct itself for details.

    // 3. Perform AddRestrictedFunctionProperties(realmRec.[[Intrinsics]].[[%Function.prototype%]], realmRec).
    try addRestrictedFunctionProperties(try self.intrinsics.@"%Function.prototype%"(), self);

    // 4. Return unused.
}

/// 9.3.3 SetDefaultGlobalBindings ( realmRec )
/// https://tc39.es/ecma262/#sec-setdefaultglobalbindings
fn setDefaultGlobalBindings(self: *Realm) Agent.Error!void {
    // 1. Let global be realmRec.[[GlobalObject]].
    const global = self.global_object;

    // Why export a constant when you can do reflection instead!
    const global_properties_count = @typeInfo(@typeInfo(@TypeOf(globalObjectProperties)).@"fn".return_type.?).array.len;
    const lazy_properties_count = global_properties_count - 4; // globalThis, Infinity, NaN, undefined
    try global.property_storage.values.ensureUnusedCapacity(self.agent.gc_allocator, global_properties_count);
    try global.property_storage.lazy_properties.ensureUnusedCapacity(self.agent.gc_allocator, lazy_properties_count);

    // 2. For each property of the Global Object specified in clause 19, do
    for (globalObjectProperties(self)) |property| {
        // a. Let name be the String value of the property name.
        const name = try String.fromAscii(self.agent, property[0]);
        const property_key = PropertyKey.from(name);

        // b. Let desc be the fully populated data Property Descriptor for the property, containing
        //    the specified attributes for the property. For properties listed in 19.2, 19.3, or
        //    19.4 the value of the [[Value]] attribute is the corresponding intrinsic object from
        //    realmRec.
        switch (property[1]) {
            .property_descriptor => |property_descriptor| {
                // c. Perform ? DefinePropertyOrThrow(global, name, desc).
                try global.definePropertyOrThrow(property_key, property_descriptor);
            },
            .lazy_property => |initializer| {
                // NOTE: There aren't any accessors on the global object so this only ever creates data properties.
                try global.definePropertyOrThrow(property_key, .{
                    .value = undefined,
                    .writable = true,
                    .enumerable = false,
                    .configurable = true,
                });
                global.property_storage.lazy_properties.putAssumeCapacity(property_key, .{
                    .realm = self,
                    .initializer = initializer,
                });
            },
        }
    }

    // 3. Return unused.
}
