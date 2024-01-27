//! 9.3 Realms
//! https://tc39.es/ecma262/#sec-code-realms

const std = @import("std");

const Allocator = std.mem.Allocator;
const Xoroshiro128 = std.rand.Xoroshiro128;

const builtins = @import("../builtins.zig");
const environments = @import("environments.zig");
const types = @import("../types.zig");

const Agent = @import("Agent.zig");
const ExecutionContext = @import("ExecutionContext.zig");
const GlobalEnvironment = environments.GlobalEnvironment;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const SafePointer = types.SafePointer;
const Value = types.Value;
const addRestrictedFunctionProperties = builtins.addRestrictedFunctionProperties;
const globalObjectProperties = builtins.globalObjectProperties;
const newGlobalEnvironment = environments.newGlobalEnvironment;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;

pub const Intrinsics = @import("Realm/Intrinsics.zig");

const Self = @This();

agent: *Agent,
rng: Xoroshiro128,

/// [[Intrinsics]]
intrinsics: Intrinsics,

/// [[GlobalObject]]
global_object: Object,

/// [[GlobalEnv]]
global_env: *GlobalEnvironment,

/// [[HostDefined]]
host_defined: SafePointer,

// TODO: [[TemplateMap]], [[LoadedModules]]

/// 9.3.1 CreateRealm ( )
/// https://tc39.es/ecma262/#sec-createrealm
pub fn create(agent: *Agent) Allocator.Error!*Self {
    // 1. Let realmRec be a new Realm Record.
    var realm = try agent.gc_allocator.create(Self);

    // Set this early, it'll be accessed before the realm struct is fully initialized.
    realm.agent = agent;

    // 2. Perform CreateIntrinsics(realmRec).
    try realm.createIntrinsics();

    realm.* = .{
        .agent = realm.agent,
        .intrinsics = realm.intrinsics,
        .rng = Xoroshiro128.init(@intFromPtr(realm)),

        // 3. Set realmRec.[[GlobalObject]] to undefined.
        .global_object = undefined,

        // 4. Set realmRec.[[GlobalEnv]] to undefined.
        .global_env = undefined,

        .host_defined = SafePointer.null_pointer,

        // TODO: 5. Set realmRec.[[TemplateMap]] to a new empty List.
    };

    // 6. Return realmRec.
    return realm;
}

/// 9.3.2 CreateIntrinsics ( realmRec )
/// https://tc39.es/ecma262/#sec-createintrinsics
fn createIntrinsics(self: *Self) Allocator.Error!void {
    // 1. Set realmRec.[[Intrinsics]] to a new Record.
    self.intrinsics = Intrinsics{ .realm = self };

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

/// 9.3.3 SetRealmGlobalObject ( realmRec, globalObj, thisValue )
/// https://tc39.es/ecma262/#sec-setrealmglobalobject
pub fn setRealmGlobalObject(
    self: *Self,
    maybe_global_object: ?Object,
    maybe_this_value: ?Object,
) Allocator.Error!void {
    // 1. If globalObj is undefined, then
    //     a. Let intrinsics be realmRec.[[Intrinsics]].
    //     b. Set globalObj to OrdinaryObjectCreate(intrinsics.[[%Object.prototype%]]).
    // 2. Assert: globalObj is an Object.
    const global_object = maybe_global_object orelse try ordinaryObjectCreate(
        self.agent,
        try self.intrinsics.@"%Object.prototype%"(),
    );

    // 3. If thisValue is undefined, set thisValue to globalObj.
    const this_value = maybe_this_value orelse global_object;

    // 4. Set realmRec.[[GlobalObject]] to globalObj.
    self.global_object = global_object;

    // 5. Let newGlobalEnv be NewGlobalEnvironment(globalObj, thisValue).
    const new_global_env = try newGlobalEnvironment(
        self.agent.gc_allocator,
        global_object,
        this_value,
    );

    // 6. Set realmRec.[[GlobalEnv]] to newGlobalEnv.
    self.global_env = new_global_env;

    // 7. Return unused.
}

/// 9.3.4 SetDefaultGlobalBindings ( realmRec )
/// https://tc39.es/ecma262/#sec-setdefaultglobalbindings
pub fn setDefaultGlobalBindings(self: *Self) Agent.Error!Object {
    // 1. Let global be realmRec.[[GlobalObject]].
    const global = self.global_object;

    // 2. For each property of the Global Object specified in clause 19, do
    for (try globalObjectProperties(self)) |property| {
        // a. Let name be the String value of the property name.
        const name = PropertyKey.from(property[0]);

        // b. Let desc be the fully populated data Property Descriptor for the property, containing
        //    the specified attributes for the property. For properties listed in 19.2, 19.3, or
        //    19.4 the value of the [[Value]] attribute is the corresponding intrinsic object from
        //    realmRec.
        const descriptor = property[1];

        // c. Perform ? DefinePropertyOrThrow(global, name, desc).
        try global.definePropertyOrThrow(name, descriptor);
    }

    // 3. Return global.
    return global;
}

/// 9.6 InitializeHostDefinedRealm ( )
/// https://tc39.es/ecma262/#sec-initializehostdefinedrealm
pub fn initializeHostDefinedRealm(
    agent: *Agent,
    args: struct {
        global_object: ?Object = null,
        this_value: ?Object = null,
    },
) Agent.Error!void {
    // 1. Let realm be CreateRealm().
    const realm = try create(agent);

    // 2. Let newContext be a new execution context.
    const new_context = ExecutionContext{
        // 3. Set the Function of newContext to null.
        .function = null,

        // 4. Set the Realm of newContext to realm.
        .realm = realm,

        // 5. Set the ScriptOrModule of newContext to null.
        .script_or_module = null,
    };

    // 6. Push newContext onto the execution context stack; newContext is now the running execution
    //    context.
    try agent.execution_context_stack.append(new_context);

    // 7. If the host requires use of an exotic object to serve as realm's global object, let
    //    global be such an object created in a host-defined manner. Otherwise, let global be
    //    undefined, indicating that an ordinary object should be created as the global object.
    const global = args.global_object;

    // 8. If the host requires that the this binding in realm's global scope return an object other
    //    than the global object, let thisValue be such an object created in a host-defined manner.
    //    Otherwise, let thisValue be undefined, indicating that realm's global this binding should
    //    be the global object.
    const this_value = args.this_value;

    // 9. Perform SetRealmGlobalObject(realm, global, thisValue).
    try realm.setRealmGlobalObject(global, this_value);

    // 10. Let globalObj be ? SetDefaultGlobalBindings(realm).
    _ = try realm.setDefaultGlobalBindings();

    // 11. Create any host-defined global object properties on globalObj.
    // 12. Return unused.
}
