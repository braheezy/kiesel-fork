//! 9.1 Environment Records
//! https://tc39.es/ecma262/#sec-environment-records

const std = @import("std");

const Allocator = std.mem.Allocator;

const types = @import("../types.zig");

const Object = types.Object;
const Value = types.Value;

pub const DeclarativeEnvironment = @import("environments/DeclarativeEnvironment.zig");
pub const GlobalEnvironment = @import("environments/GlobalEnvironment.zig");
pub const ObjectEnvironment = @import("environments/ObjectEnvironment.zig");
pub const PrivateEnvironment = @import("environments/PrivateEnvironment.zig");

/// 9.1.1 The Environment Record Type Hierarchy
/// https://tc39.es/ecma262/#sec-the-environment-record-type-hierarchy
pub const Environment = union(enum) {
    const Self = @This();

    declarative_environment: *DeclarativeEnvironment,
    object_environment: *ObjectEnvironment,
    global_environment: *GlobalEnvironment,

    pub fn outerEnv(self: Self) ?Self {
        return switch (self) {
            .declarative_environment => |env| env.outer_env,
            .object_environment => |env| env.outer_env,
            .global_environment => |env| env.outer_env,
        };
    }

    pub fn hasBinding(self: Self, name: []const u8) bool {
        _ = self;
        _ = name;
        @compileError("Not implemented");
    }

    pub fn createMutableBinding(self: Self, name: []const u8, deletable: bool) bool {
        _ = self;
        _ = name;
        _ = deletable;
        @compileError("Not implemented");
    }

    pub fn createImmutableBinding(self: Self, name: []const u8, strict: bool) bool {
        _ = self;
        _ = name;
        _ = strict;
        @compileError("Not implemented");
    }

    pub fn initializeBinding(self: Self, name: []const u8, value: Value) bool {
        _ = self;
        _ = name;
        _ = value;
        @compileError("Not implemented");
    }

    pub fn setMutableBinding(self: Self, name: []const u8, value: Value, strict: bool) bool {
        _ = self;
        _ = name;
        _ = value;
        _ = strict;
        @compileError("Not implemented");
    }

    pub fn getBindingValue(self: Self, name: []const u8, strict: bool) bool {
        _ = self;
        _ = name;
        _ = strict;
        @compileError("Not implemented");
    }

    pub fn deleteBinding(self: Self, name: []const u8) bool {
        _ = self;
        _ = name;
        @compileError("Not implemented");
    }

    pub fn hasThisBinding(self: Self) bool {
        _ = self;
        @compileError("Not implemented");
    }

    pub fn hasSuperBinding(self: Self) bool {
        _ = self;
        @compileError("Not implemented");
    }

    pub fn withBaseObject(self: Self) ?Object {
        _ = self;
        @compileError("Not implemented");
    }
};

/// 9.1.2.2 NewDeclarativeEnvironment ( E )
/// https://tc39.es/ecma262/#sec-newdeclarativeenvironment
pub fn newDeclarativeEnvironment(
    allocator: Allocator,
    outer_env: ?Environment,
) !*DeclarativeEnvironment {
    // 1. Let env be a new Declarative Environment Record containing no bindings.
    const env = try allocator.create(DeclarativeEnvironment);

    env.* = .{
        // 2. Set env.[[OuterEnv]] to E.
        .outer_env = outer_env,
    };

    // 3. Return env.
    return env;
}

/// 9.1.2.3 NewObjectEnvironment ( O, W, E )
/// https://tc39.es/ecma262/#sec-newobjectenvironment
pub fn newObjectEnvironment(
    allocator: Allocator,
    binding_object: Object,
    is_with_environment: bool,
    outer_env: ?Environment,
) !*ObjectEnvironment {
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

/// 9.1.2.5 NewGlobalEnvironment ( G, thisValue )
/// https://tc39.es/ecma262/#sec-newglobalenvironment
pub fn newGlobalEnvironment(
    allocator: Allocator,
    global_object: Object,
    this_value: Object,
) !*GlobalEnvironment {
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
        .var_names = std.ArrayList([]const u8).init(allocator),

        // 8. Set env.[[OuterEnv]] to null.
        .outer_env = null,
    };

    // 9. Return env.
    return env;
}

/// 9.2.1.1 NewPrivateEnvironment ( outerPrivEnv )
/// https://tc39.es/ecma262/#sec-newprivateenvironment
pub fn newPrivateEnvironment(
    allocator: Allocator,
    outer_private_environment: ?*PrivateEnvironment,
) !*PrivateEnvironment {
    // 1. Let names be a new empty List.
    const names = std.ArrayList(void).init(allocator);

    // 2. Return the PrivateEnvironment Record {
    //      [[OuterPrivateEnvironment]]: outerPrivEnv, [[Names]]: names
    //    }.
    const env = try allocator.create(PrivateEnvironment);
    env.* = .{
        .outer_private_environment = outer_private_environment,
        .names = names,
    };
    return env;
}
