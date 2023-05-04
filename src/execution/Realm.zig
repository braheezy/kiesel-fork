//! 9.3 Realms
//! https://tc39.es/ecma262/#sec-code-realms

const std = @import("std");

const Allocator = std.mem.Allocator;

const types = @import("../types.zig");

const Agent = @import("Agent.zig");
const ExecutionContext = @import("ExecutionContext.zig");
const Object = types.Object;

pub const Intrinsics = @import("Realm/Intrinsics.zig");

const Self = @This();

// Stubs, for now
const Environment = struct {};

agent: *Agent,

/// [[Intrinsics]]
intrinsics: Intrinsics,

/// [[GlobalObject]]
global_object: Object,

/// [[GlobalEnv]]
global_env: Environment,

/// [[HostDefined]]
host_defined: ?*anyopaque = null,

// TODO: [[TemplateMap]], [[LoadedModules]]

/// 9.3.1 CreateRealm ( )
/// https://tc39.es/ecma262/#sec-createrealm
pub fn create(agent: *Agent) !*Self {
    // 1. Let realmRec be a new Realm Record.
    var realm = try agent.allocator.create(Self);
    realm.* = .{
        .agent = agent,

        // 2. Perform CreateIntrinsics(realmRec).
        .intrinsics = try realm.createIntrinsics(),

        // 3. Set realmRec.[[GlobalObject]] to undefined.
        .global_object = undefined,

        // 4. Set realmRec.[[GlobalEnv]] to undefined.
        .global_env = undefined,

        // TODO: 5. Set realmRec.[[TemplateMap]] to a new empty List.
    };

    // 6. Return realmRec.
    return realm;
}

/// 9.3.2 CreateIntrinsics ( realmRec )
/// https://tc39.es/ecma262/#sec-createintrinsics
fn createIntrinsics(self: *Self) !Intrinsics {
    _ = self;
    // 1. Set realmRec.[[Intrinsics]] to a new Record.
    var intrinsics = Intrinsics{};

    // TODO: 2. Set fields of realmRec.[[Intrinsics]] with the values listed in Table 6. The field
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
    // TODO: 3. Perform AddRestrictedFunctionProperties(realmRec.[[Intrinsics]].[[%Function.prototype%]], realmRec).

    // 4. Return unused.
    return intrinsics;
}

/// 9.6 InitializeHostDefinedRealm ( )
/// https://tc39.es/ecma262/#sec-initializehostdefinedrealm
pub fn initializeHostDefinedRealm(agent: *Agent) !void {
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

    // 8. If the host requires that the this binding in realm's global scope return an object other
    //    than the global object, let thisValue be such an object created in a host-defined manner.
    //    Otherwise, let thisValue be undefined, indicating that realm's global this binding should
    //    be the global object.
    const thisValue = null;
    _ = thisValue;

    // TODO: 9. Perform SetRealmGlobalObject(realm, global, thisValue).
    // TODO: 10. Let globalObj be ? SetDefaultGlobalBindings(realm).

    // 11. Create any host-defined global object properties on globalObj.
    // 12. Return unused.
}
