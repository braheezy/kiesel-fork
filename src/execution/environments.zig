//! 9.1 Environment Records
//! https://tc39.es/ecma262/#sec-environment-records

const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const ECMAScriptFunction = builtins.ECMAScriptFunction;
const Object = types.Object;
const PrivateName = types.PrivateName;
const Reference = types.Reference;
const SourceTextModule = language.SourceTextModule;
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

    pub fn hasBinding(self: Environment, name: String) Agent.Error!bool {
        return switch (self) {
            .function_environment => |env| env.declarative_environment.hasBinding(name),
            inline else => |env| env.hasBinding(name),
        };
    }

    pub fn createMutableBinding(
        self: Environment,
        agent: *Agent,
        name: String,
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
        name: String,
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
        name: String,
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
        name: String,
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
        name: String,
        strict: bool,
    ) Agent.Error!Value {
        return switch (self) {
            .function_environment => |env| env.declarative_environment.getBindingValue(agent, name, strict),
            inline else => |env| env.getBindingValue(agent, name, strict),
        };
    }

    pub fn deleteBinding(self: Environment, name: String) Agent.Error!bool {
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

    pub fn withBaseObject(self: Environment) ?Object {
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

    pub fn bindThisValue(self: Environment, value: Value) error{ExceptionThrown}!Value {
        return switch (self) {
            .function_environment => |env| env.bindThisValue(value),
            else => unreachable,
        };
    }

    pub fn getSuperBase(self: Environment) std.mem.Allocator.Error!Value {
        return switch (self) {
            .function_environment => |env| env.getSuperBase(),
            else => unreachable,
        };
    }

    pub fn createImportBinding(
        self: Environment,
        name: String,
        module: *SourceTextModule,
        binding_name: String,
    ) std.mem.Allocator.Error!void {
        return switch (self) {
            .module_environment => |env| env.createImportBinding(name, module, binding_name),
            else => unreachable,
        };
    }
};

/// 9.1.2.1 GetIdentifierReference ( env, name, strict )
/// https://tc39.es/ecma262/#sec-getidentifierreference
pub fn getIdentifierReference(
    start_env: Environment,
    name: String,
    strict: bool,
    maybe_lookup_cache_entry: ?*?Environment.LookupCacheEntry,
) Agent.Error!Reference {
    var env: ?Environment = start_env;
    if (maybe_lookup_cache_entry) |lookup_cache_entry| {
        if (lookup_cache_entry.*) |cache| {
            // In the case of an unresolvable reference we'll reach the last environment without an
            // outer env.
            for (0..cache.distance) |_| env = env.?.outerEnv();
            return .{
                .base = if (env) |environment|
                    .{ .environment = environment }
                else
                    .unresolvable,
                .referenced_name = .{ .value = Value.from(name) },
                .strict = strict,
                .this_value = null,
            };
        }
        lookup_cache_entry.* = .{ .distance = 0 };
    }

    while (true) {
        // 2. Let exists be ? env.HasBinding(name).
        const exists = try env.?.hasBinding(name);

        // 3. If exists is true, then
        if (exists) {
            // a. Return the Reference Record {
            //      [[Base]]: env, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty
            //    }.
            return .{
                .base = .{ .environment = env.? },
                .referenced_name = .{ .value = Value.from(name) },
                .strict = strict,
                .this_value = null,
            };
        }
        // 4. Else,
        else {
            if (maybe_lookup_cache_entry) |lookup_cache_entry| {
                lookup_cache_entry.*.?.distance += 1;
            }

            // a. Let outer be env.[[OuterEnv]].
            env = env.?.outerEnv();

            // 1. If env is null, then
            if (env == null) {
                // a. Return the Reference Record {
                //      [[Base]]: unresolvable, [[ReferencedName]]: name, [[Strict]]: strict, [[ThisValue]]: empty
                //    }.
                return .{
                    .base = .unresolvable,
                    .referenced_name = .{ .value = Value.from(name) },
                    .strict = strict,
                    .this_value = null,
                };
            }

            // b. Return ? GetIdentifierReference(outer, name, strict).
        }
    }
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

        .bindings = .init(allocator),
    };

    // 3. Return env.
    return env;
}

/// 9.1.2.3 NewObjectEnvironment ( O, W, E )
/// https://tc39.es/ecma262/#sec-newobjectenvironment
pub fn newObjectEnvironment(
    allocator: std.mem.Allocator,
    binding_object: Object,
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
    new_target: ?Object,
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
        .declarative_environment = try newDeclarativeEnvironment(allocator, function.fields.environment),

        .this_value = .undefined,
    };

    // 7. Return env.
    return env;
}

/// 9.1.2.5 NewGlobalEnvironment ( G, thisValue )
/// https://tc39.es/ecma262/#sec-newglobalenvironment
pub fn newGlobalEnvironment(
    allocator: std.mem.Allocator,
    global_object: Object,
    this_value: Object,
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

        // 7. Set env.[[VarNames]] to a new empty List.
        .var_names = .init(allocator),

        // 8. Set env.[[OuterEnv]] to null.
        .outer_env = null,
    };

    // 9. Return env.
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
        .declarative_environment = try newDeclarativeEnvironment(allocator, outer_env),

        .indirect_bindings = .init(allocator),
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
    const names = std.StringHashMap(PrivateName).init(allocator);

    // 2. Return the PrivateEnvironment Record {
    //      [[OuterPrivateEnvironment]]: outerPrivateEnv, [[Names]]: names
    //    }.
    const env = try allocator.create(PrivateEnvironment);
    env.* = .{ .outer_private_environment = outer_private_env, .names = names };
    return env;
}
