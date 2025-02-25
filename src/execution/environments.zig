//! 9.1 Environment Records
//! https://tc39.es/ecma262/#sec-environment-records

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const ECMAScriptFunction = builtins.ECMAScriptFunction;
const Object = types.Object;
const PrivateName = types.PrivateName;
const Reference = types.Reference;
const String = types.String;
const Value = types.Value;

pub const DeclarativeEnvironment = @import("environments/DeclarativeEnvironment.zig");
pub const FunctionEnvironment = @import("environments/FunctionEnvironment.zig");
pub const GlobalEnvironment = @import("environments/GlobalEnvironment.zig");
pub const ModuleEnvironment = @import("environments/ModuleEnvironment.zig");
pub const ObjectEnvironment = @import("environments/ObjectEnvironment.zig");
pub const PrivateEnvironment = @import("environments/PrivateEnvironment.zig");

/// 9.1.1 The Environment Record Type Hierarchy
/// https://tc39.es/ecma262/#sec-the-environment-record-type-hierarchy
pub const Environment = union(enum) {
    declarative_environment: *DeclarativeEnvironment,
    object_environment: *ObjectEnvironment,
    function_environment: *FunctionEnvironment,
    global_environment: *GlobalEnvironment,
    module_environment: *ModuleEnvironment,

    pub const LookupCacheEntry = struct {
        distance: usize,
    };

    pub fn outerEnv(self: Environment) ?Environment {
        return switch (self) {
            inline .function_environment,
            .module_environment,
            => |env| env.declarative_environment.outer_env,
            inline else => |env| env.outer_env,
        };
    }

    pub fn hasBinding(self: Environment, name: *const String) Agent.Error!bool {
        return switch (self) {
            .function_environment => |env| env.declarative_environment.hasBinding(name),
            inline else => |env| env.hasBinding(name),
        };
    }

    pub fn createMutableBinding(
        self: Environment,
        agent: *Agent,
        name: *const String,
        deletable: bool,
    ) Agent.Error!void {
        return switch (self) {
            inline .function_environment,
            .module_environment,
            => |env| env.declarative_environment.createMutableBinding(agent, name, deletable),
            inline else => |env| env.createMutableBinding(agent, name, deletable),
        };
    }

    pub fn createImmutableBinding(
        self: Environment,
        agent: *Agent,
        name: *const String,
        strict: bool,
    ) Agent.Error!void {
        return switch (self) {
            inline .function_environment,
            .module_environment,
            => |env| env.declarative_environment.createImmutableBinding(agent, name, strict),
            inline else => |env| env.createImmutableBinding(agent, name, strict),
        };
    }

    pub fn initializeBinding(
        self: Environment,
        agent: *Agent,
        name: *const String,
        value: Value,
    ) Agent.Error!void {
        return switch (self) {
            inline .function_environment,
            .module_environment,
            => |env| env.declarative_environment.initializeBinding(agent, name, value),
            inline else => |env| env.initializeBinding(agent, name, value),
        };
    }

    pub fn setMutableBinding(
        self: Environment,
        agent: *Agent,
        name: *const String,
        value: Value,
        strict: bool,
    ) Agent.Error!void {
        return switch (self) {
            inline .function_environment,
            .module_environment,
            => |env| env.declarative_environment.setMutableBinding(agent, name, value, strict),
            inline else => |env| env.setMutableBinding(agent, name, value, strict),
        };
    }

    pub fn getBindingValue(
        self: Environment,
        agent: *Agent,
        name: *const String,
        strict: bool,
    ) Agent.Error!Value {
        return switch (self) {
            .function_environment => |env| env.declarative_environment.getBindingValue(agent, name, strict),
            inline else => |env| env.getBindingValue(agent, name, strict),
        };
    }

    pub fn deleteBinding(self: Environment, name: *const String) Agent.Error!bool {
        return switch (self) {
            .function_environment => |env| env.declarative_environment.deleteBinding(name),
            inline else => |env| env.deleteBinding(name),
        };
    }

    pub fn hasThisBinding(self: Environment) bool {
        return switch (self) {
            inline else => |env| env.hasThisBinding(),
        };
    }

    pub fn hasSuperBinding(self: Environment) bool {
        return switch (self) {
            .module_environment => |env| env.declarative_environment.hasSuperBinding(),
            inline else => |env| env.hasSuperBinding(),
        };
    }

    pub fn withBaseObject(self: Environment) ?*Object {
        return switch (self) {
            inline .function_environment,
            .module_environment,
            => |env| env.declarative_environment.withBaseObject(),
            inline else => |env| env.withBaseObject(),
        };
    }

    pub fn getThisBinding(self: Environment) error{ExceptionThrown}!Value {
        return switch (self) {
            .declarative_environment,
            .object_environment,
            => unreachable,
            .global_environment => |env| Value.from(env.getThisBinding()),
            inline else => |env| env.getThisBinding(),
        };
    }
};

/// 9.1.2.1 GetIdentifierReference ( env, name, strict )
/// https://tc39.es/ecma262/#sec-getidentifierreference
pub fn getIdentifierReference(
    start_env: Environment,
    name: *const String,
    strict: bool,
    maybe_lookup_cache_entry: ?*?Environment.LookupCacheEntry,
) Agent.Error!Reference {
    // 1. If env is null, then
    //     a. Return the Reference Record {
    //          [[Base]]: unresolvable, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty
    //        }.
    // 2. Let exists be ? env.HasBinding(name).
    // 3. If exists is true, then
    //     a. Return the Reference Record {
    //          [[Base]]: env, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty
    //        }.
    // 4. Else,
    //     a. Let outer be env.[[OuterEnv]].
    //     b. Return ? GetIdentifierReference(outer, name, strict).
    var env = start_env;
    if (maybe_lookup_cache_entry) |lookup_cache_entry| {
        if (lookup_cache_entry.*) |cache| {
            // In the case of an unresolvable reference we'll reach the last environment without an
            // outer env.
            for (0..cache.distance) |_| env = env.outerEnv() orelse {
                return .{
                    .base = .unresolvable,
                    .referenced_name = .{ .value = Value.from(name) },
                    .strict = strict,
                    .this_value = null,
                };
            };
        }
    }
    var distance: usize = 0;
    defer if (maybe_lookup_cache_entry) |lookup_cache_entry| {
        lookup_cache_entry.* = .{ .distance = distance };
    };
    while (!try env.hasBinding(name)) {
        distance += 1;
        env = env.outerEnv() orelse {
            return .{
                .base = .unresolvable,
                .referenced_name = .{ .value = Value.from(name) },
                .strict = strict,
                .this_value = null,
            };
        };
    }
    return .{
        .base = .{ .environment = env },
        .referenced_name = .{ .value = Value.from(name) },
        .strict = strict,
        .this_value = null,
    };
}

/// 9.1.2.2 NewDeclarativeEnvironment ( E )
/// https://tc39.es/ecma262/#sec-newdeclarativeenvironment
pub fn newDeclarativeEnvironment(
    allocator: std.mem.Allocator,
    outer_env: ?Environment,
) std.mem.Allocator.Error!*DeclarativeEnvironment {
    // 1. Let env be a new Declarative Environment Record containing no bindings.
    const env = try allocator.create(DeclarativeEnvironment);

    env.* = .{
        // 2. Set env.[[OuterEnv]] to E.
        .outer_env = outer_env,

        .bindings = .empty,
    };

    // 3. Return env.
    return env;
}

/// 9.1.2.3 NewObjectEnvironment ( O, W, E )
/// https://tc39.es/ecma262/#sec-newobjectenvironment
pub fn newObjectEnvironment(
    allocator: std.mem.Allocator,
    binding_object: *Object,
    is_with_environment: bool,
    outer_env: ?Environment,
) std.mem.Allocator.Error!*ObjectEnvironment {
    // 1. Let env be a new Object Environment Record.
    const env = try allocator.create(ObjectEnvironment);

    env.* = .{
        // 2. Set env.[[BindingObject]] to O.
        .binding_object = binding_object,

        // 3. Set env.[[IsWithEnvironment]] to W.
        .is_with_environment = is_with_environment,

        // 4. Set env.[[OuterEnv]] to E.
        .outer_env = outer_env,
    };

    // 5. Return env.
    return env;
}

/// 9.1.2.4 NewFunctionEnvironment ( F, newTarget )
/// https://tc39.es/ecma262/#sec-newfunctionenvironment
pub fn newFunctionEnvironment(
    allocator: std.mem.Allocator,
    function: *ECMAScriptFunction,
    new_target: ?*Object,
) std.mem.Allocator.Error!*FunctionEnvironment {
    // 1. Let env be a new Function Environment Record containing no bindings.
    const env = try allocator.create(FunctionEnvironment);
    env.* = .{
        // 2. Set env.[[FunctionObject]] to F.
        .function_object = function,

        // 3. If F.[[ThisMode]] is lexical, set env.[[ThisBindingStatus]] to lexical.
        // 4. Else, set env.[[ThisBindingStatus]] to uninitialized.
        .this_binding_status = if (function.fields.this_mode == .lexical)
            .lexical
        else
            .uninitialized,

        // 5. Set env.[[NewTarget]] to newTarget.
        .new_target = new_target,

        // 6. Set env.[[OuterEnv]] to F.[[Environment]].
        .declarative_environment = .{
            .outer_env = function.fields.environment,
            .bindings = .empty,
        },

        .this_value = .undefined,
    };

    // 7. Return env.
    return env;
}

/// 9.1.2.5 NewGlobalEnvironment ( G, thisValue )
/// https://tc39.es/ecma262/#sec-newglobalenvironment
pub fn newGlobalEnvironment(
    allocator: std.mem.Allocator,
    global_object: *Object,
    this_value: *Object,
) std.mem.Allocator.Error!*GlobalEnvironment {
    // 1. Let objRec be NewObjectEnvironment(G, false, null).
    const object_record = try newObjectEnvironment(allocator, global_object, false, null);

    // 2. Let dclRec be NewDeclarativeEnvironment(null).
    const declarative_record = try newDeclarativeEnvironment(allocator, null);

    // 3. Let env be a new Global Environment Record.
    const env = try allocator.create(GlobalEnvironment);
    env.* = .{
        // 4. Set env.[[ObjectRecord]] to objRec.
        .object_record = object_record,

        // 5. Set env.[[GlobalThisValue]] to thisValue.
        .global_this_value = this_value,

        // 6. Set env.[[DeclarativeRecord]] to dclRec.
        .declarative_record = declarative_record,

        // 7. Set env.[[OuterEnv]] to null.
        .outer_env = null,
    };

    // 8. Return env.
    return env;
}

/// 9.1.2.6 NewModuleEnvironment ( E )
/// https://tc39.es/ecma262/#sec-newmoduleenvironment
pub fn newModuleEnvironment(
    allocator: std.mem.Allocator,
    outer_env: Environment,
) std.mem.Allocator.Error!*ModuleEnvironment {
    // 1. Let env be a new Module Environment Record containing no bindings.
    const env = try allocator.create(ModuleEnvironment);
    env.* = .{
        // 2. Set env.[[OuterEnv]] to E.
        .declarative_environment = .{
            .outer_env = outer_env,
            .bindings = .empty,
        },

        .indirect_bindings = .empty,
    };

    // 3. Return env.
    return env;
}

/// 9.2.1.1 NewPrivateEnvironment ( outerPrivateEnv )
/// https://tc39.es/ecma262/#sec-newprivateenvironment
pub fn newPrivateEnvironment(
    allocator: std.mem.Allocator,
    outer_private_env: ?*PrivateEnvironment,
) std.mem.Allocator.Error!*PrivateEnvironment {
    // 1. Let names be a new empty List.
    const names: std.StringHashMapUnmanaged(PrivateName) = .empty;

    // 2. Return the PrivateEnvironment Record {
    //      [[OuterPrivateEnvironment]]: outerPrivateEnv, [[Names]]: names
    //    }.
    const env = try allocator.create(PrivateEnvironment);
    env.* = .{
        .outer_private_environment = outer_private_env,
        .names = names,
    };
    return env;
}
