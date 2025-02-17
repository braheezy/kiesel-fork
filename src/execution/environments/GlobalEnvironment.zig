//! 9.1.1.4 Global Environment Records
//! https://tc39.es/ecma262/#sec-global-environment-records

const environments = @import("../environments.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Agent = execution.Agent;
const DeclarativeEnvironment = environments.DeclarativeEnvironment;
const Environment = environments.Environment;
const Object = types.Object;
const ObjectEnvironment = environments.ObjectEnvironment;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const String = types.String;
const Value = types.Value;

const GlobalEnvironment = @This();

/// [[ObjectRecord]]
object_record: *ObjectEnvironment,

/// [[GlobalThisValue]]
global_this_value: *Object,

/// [[DeclarativeRecord]]
declarative_record: *DeclarativeEnvironment,

/// [[OuterEnv]]
outer_env: ?Environment,

/// 9.1.1.4.1 HasBinding ( N )
/// https://tc39.es/ecma262/#sec-global-environment-records-hasbinding-n
pub fn hasBinding(self: GlobalEnvironment, name: *const String) Agent.Error!bool {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, return true.
    if (self.declarative_record.hasBinding(name)) return true;

    // 3. Let ObjRec be envRec.[[ObjectRecord]].
    // 4. Return ? ObjRec.HasBinding(N).
    return self.object_record.hasBinding(name);
}

/// 9.1.1.4.2 CreateMutableBinding ( N, D )
/// https://tc39.es/ecma262/#sec-global-environment-records-createmutablebinding-n-d
pub fn createMutableBinding(
    self: *GlobalEnvironment,
    agent: *Agent,
    name: *const String,
    deletable: bool,
) Agent.Error!void {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, throw a TypeError exception.
    if (self.declarative_record.hasBinding(name)) {
        return agent.throwException(.type_error, "Binding for '{}' already exists", .{name});
    }

    // 3. Return ! DclRec.CreateMutableBinding(N, D).
    return self.declarative_record.createMutableBinding(agent, name, deletable);
}

/// 9.1.1.4.3 CreateImmutableBinding ( N, S )
/// https://tc39.es/ecma262/#sec-global-environment-records-createimmutablebinding-n-s
pub fn createImmutableBinding(
    self: *GlobalEnvironment,
    agent: *Agent,
    name: *const String,
    strict: bool,
) Agent.Error!void {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, throw a TypeError exception.
    if (self.declarative_record.hasBinding(name)) {
        return agent.throwException(.type_error, "Binding for '{}' already exists", .{name});
    }

    // 3. Return ! DclRec.CreateImmutableBinding(N, S).
    return self.declarative_record.createImmutableBinding(agent, name, strict);
}

/// 9.1.1.4.4 InitializeBinding ( N, V )
/// https://tc39.es/ecma262/#sec-global-environment-records-initializebinding-n-v
pub fn initializeBinding(
    self: GlobalEnvironment,
    agent: *Agent,
    name: *const String,
    value: Value,
) Agent.Error!void {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, then
    if (self.declarative_record.hasBinding(name)) {
        // a. Return ! DclRec.InitializeBinding(N, V).
        return self.declarative_record.initializeBinding(agent, name, value);
    }

    // 3. Assert: If the binding exists, it must be in the Object Environment Record.
    // 4. Let ObjRec be envRec.[[ObjectRecord]].
    // 5. Return ? ObjRec.InitializeBinding(N, V).
    return self.object_record.initializeBinding(agent, name, value);
}

/// 9.1.1.4.5 SetMutableBinding ( N, V, S )
/// https://tc39.es/ecma262/#sec-global-environment-records-setmutablebinding-n-v-s
pub fn setMutableBinding(
    self: GlobalEnvironment,
    agent: *Agent,
    name: *const String,
    value: Value,
    strict: bool,
) Agent.Error!void {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, then
    if (self.declarative_record.hasBinding(name)) {
        // a. Return ? DclRec.SetMutableBinding(N, V, S).
        return self.declarative_record.setMutableBinding(agent, name, value, strict);
    }

    // 3. Let ObjRec be envRec.[[ObjectRecord]].
    // 4. Return ? ObjRec.SetMutableBinding(N, V, S).
    return self.object_record.setMutableBinding(agent, name, value, strict);
}

/// 9.1.1.4.6 GetBindingValue ( N, S )
/// https://tc39.es/ecma262/#sec-global-environment-records-getbindingvalue-n-s
pub fn getBindingValue(
    self: GlobalEnvironment,
    agent: *Agent,
    name: *const String,
    strict: bool,
) Agent.Error!Value {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, then
    if (self.declarative_record.hasBinding(name)) {
        // a. Return ? DclRec.GetBindingValue(N, S).
        return self.declarative_record.getBindingValue(agent, name, strict);
    }

    // 3. Let ObjRec be envRec.[[ObjectRecord]].
    // 4. Return ? ObjRec.GetBindingValue(N, S).
    return self.object_record.getBindingValue(agent, name, strict);
}

/// 9.1.1.4.7 DeleteBinding ( N )
/// https://tc39.es/ecma262/#sec-global-environment-records-deletebinding-n
pub fn deleteBinding(self: *GlobalEnvironment, name: *const String) Agent.Error!bool {
    // 1. Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. If ! DclRec.HasBinding(N) is true, then
    if (self.declarative_record.hasBinding(name)) {
        // a. Return ! DclRec.DeleteBinding(N).
        return self.declarative_record.deleteBinding(name);
    }

    // 3. Let ObjRec be envRec.[[ObjectRecord]].
    // 4. Let globalObject be ObjRec.[[BindingObject]].
    const global_object = self.object_record.binding_object;

    // 5. Let existingProp be ? HasOwnProperty(globalObject, N).
    const existing_prop = try global_object.hasOwnProperty(PropertyKey.from(name));

    // 6. If existingProp is true, then
    if (existing_prop) {
        // a. Return ? ObjRec.DeleteBinding(N).
        return self.object_record.deleteBinding(name);
    }

    // 7. Return true.
    return true;
}

/// 9.1.1.4.8 HasThisBinding ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-hasthisbinding
pub fn hasThisBinding(_: GlobalEnvironment) bool {
    // 1. Return true.
    return true;
}

/// 9.1.1.4.9 HasSuperBinding ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-hassuperbinding
pub fn hasSuperBinding(_: GlobalEnvironment) bool {
    // 1. Return false.
    return false;
}

/// 9.1.1.4.10 WithBaseObject ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-withbaseobject
pub fn withBaseObject(_: GlobalEnvironment) ?*Object {
    // 1. Return undefined.
    return null;
}

/// 9.1.1.4.11 GetThisBinding ( )
/// https://tc39.es/ecma262/#sec-global-environment-records-getthisbinding
pub fn getThisBinding(self: GlobalEnvironment) *Object {
    // 1. Return envRec.[[GlobalThisValue]].
    return self.global_this_value;
}

/// 9.1.1.4.12 HasLexicalDeclaration ( N )
/// https://tc39.es/ecma262/#sec-haslexicaldeclaration
pub fn hasLexicalDeclaration(self: GlobalEnvironment, name: *const String) bool {
    // 1.Let DclRec be envRec.[[DeclarativeRecord]].
    // 2. Return ! DclRec.HasBinding(N).
    return self.declarative_record.hasBinding(name);
}

/// 9.1.1.4.13 HasRestrictedGlobalProperty ( N )
/// https://tc39.es/ecma262/#sec-hasrestrictedglobalproperty
pub fn hasRestrictedGlobalProperty(self: GlobalEnvironment, name: *const String) Agent.Error!bool {
    // 1. Let ObjRec be envRec.[[ObjectRecord]].
    // 2. Let globalObject be ObjRec.[[BindingObject]].
    const global_object = self.object_record.binding_object;

    // 3. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
    const existing_property = try global_object.internal_methods.getOwnProperty(
        global_object.agent,
        global_object,
        PropertyKey.from(name),
    ) orelse {
        // 4. If existingProp is undefined, return false.
        return false;
    };

    // 5. If existingProp.[[Configurable]] is true, return false.
    if (existing_property.configurable == true) return false;

    // 6. Return true.
    return true;
}

/// 9.1.1.4.14 CanDeclareGlobalVar ( N )
/// https://tc39.es/ecma262/#sec-candeclareglobalvar
pub fn canDeclareGlobalVar(self: *GlobalEnvironment, name: *const String) Agent.Error!bool {
    // 1. Let ObjRec be envRec.[[ObjectRecord]].
    // 2. Let globalObject be ObjRec.[[BindingObject]].
    const global_object = self.object_record.binding_object;

    // 3. Let hasProperty be ? HasOwnProperty(globalObject, N).
    const has_property = try global_object.hasOwnProperty(PropertyKey.from(name));

    // 4. If hasProperty is true, return true.
    if (has_property) return true;

    // 5. Return ? IsExtensible(globalObject).
    return global_object.isExtensible();
}

/// 9.1.1.4.15 CanDeclareGlobalFunction ( N )
/// https://tc39.es/ecma262/#sec-candeclareglobalfunction
pub fn canDeclareGlobalFunction(self: *GlobalEnvironment, name: *const String) Agent.Error!bool {
    // 1. Let ObjRec be envRec.[[ObjectRecord]].
    // 2. Let globalObject be ObjRec.[[BindingObject]].
    const global_object = self.object_record.binding_object;

    // 3. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
    const existing_prop = try global_object.internal_methods.getOwnProperty(
        global_object.agent,
        global_object,
        PropertyKey.from(name),
    ) orelse {
        // 4. If existingProp is undefined, return ? IsExtensible(globalObject).
        return global_object.isExtensible();
    };

    // 5. If existingProp.[[Configurable]] is true, return true.
    if (existing_prop.configurable == true) return true;

    // 6. If IsDataDescriptor(existingProp) is true and existingProp has attribute values
    //    { [[Writable]]: true, [[Enumerable]]: true }, return true.
    if (existing_prop.isDataDescriptor() and
        existing_prop.writable == true and
        existing_prop.enumerable == true) return true;

    // 7. Return false.
    return false;
}

/// 9.1.1.4.16 CreateGlobalVarBinding ( N, D )
/// https://tc39.es/ecma262/#sec-createglobalvarbinding
pub fn createGlobalVarBinding(
    self: *GlobalEnvironment,
    agent: *Agent,
    name: *const String,
    deletable: bool,
) Agent.Error!void {
    // 1. Let ObjRec be envRec.[[ObjectRecord]].
    // 2. Let globalObject be ObjRec.[[BindingObject]].
    const global_object = self.object_record.binding_object;

    // 3. Let hasProperty be ? HasOwnProperty(globalObject, N).
    const has_property = try global_object.hasOwnProperty(PropertyKey.from(name));

    // 4. Let extensible be ? IsExtensible(globalObject).
    const extensible = try global_object.isExtensible();

    // 5. If hasProperty is false and extensible is true, then
    if (!has_property and extensible) {
        // a. Perform ? ObjRec.CreateMutableBinding(N, D).
        try self.object_record.createMutableBinding(agent, name, deletable);

        // b. Perform ? ObjRec.InitializeBinding(N, undefined).
        try self.object_record.initializeBinding(agent, name, .undefined);
    }

    // 6. Return unused.
}

/// 9.1.1.4.17 CreateGlobalFunctionBinding ( N, V, D )
/// https://tc39.es/ecma262/#sec-createglobalfunctionbinding
pub fn createGlobalFunctionBinding(
    self: *GlobalEnvironment,
    agent: *Agent,
    name: *const String,
    value: Value,
    deletable: bool,
) Agent.Error!void {
    const property_key = PropertyKey.from(name);

    // 1. Let ObjRec be envRec.[[ObjectRecord]].
    // 2. Let globalObject be ObjRec.[[BindingObject]].
    const global_object = self.object_record.binding_object;

    // 3. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
    const existing_prop = try global_object.internal_methods.getOwnProperty(
        agent,
        global_object,
        property_key,
    );

    // 4. If existingProp is undefined or existingProp.[[Configurable]] is true, then
    const property_descriptor: PropertyDescriptor = if (existing_prop == null or existing_prop.?.configurable == true) blk: {
        // a. Let desc be the PropertyDescriptor {
        //      [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: D
        //    }.
        break :blk .{ .value = value, .writable = true, .enumerable = true, .configurable = deletable };
    }
    // 5. Else,
    else blk: {
        // a. Let desc be the PropertyDescriptor { [[Value]]: V }.
        break :blk .{ .value = value };
    };

    // 6. Perform ? DefinePropertyOrThrow(globalObject, N, desc).
    try global_object.definePropertyOrThrow(property_key, property_descriptor);

    // 7. Perform ? Set(globalObject, N, V, false).
    try global_object.set(property_key, value, .ignore);

    // 8. Return unused.
}
