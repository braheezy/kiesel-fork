//! 9.7 Agents
//! https://tc39.es/ecma262/#sec-agents

const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;

const builtins = @import("../builtins.zig");
const environments = @import("environments.zig");
const pretty_printing = @import("../pretty_printing.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const BigInt = types.BigInt;
const Environment = environments.Environment;
const ExecutionContext = @import("ExecutionContext.zig");
const HostHooks = @import("HostHooks.zig");
const Job = @import("job.zig").Job;
const Object = types.Object;
const Realm = @import("Realm.zig");
const Reference = types.Reference;
const String = types.String;
const StringHashMap = types.StringHashMap;
const Symbol = types.Symbol;
const Value = types.Value;
const WellKnownSymbols = @import("Agent/WellKnownSymbols.zig");
const getIdentifierReference = environments.getIdentifierReference;
const noexcept = utils.noexcept;

const Self = @This();

gc_allocator: Allocator,
options: Options,
pre_allocated: struct {
    zero: BigInt,
    one: BigInt,
},
exception: ?Value = null,
symbol_id: usize = 0,
well_known_symbols: WellKnownSymbols,
global_symbol_registry: StringHashMap(Symbol),
host_hooks: HostHooks,
execution_context_stack: std.ArrayList(ExecutionContext),
queued_jobs: std.ArrayList(QueuedJob),
platform: Platform,

/// [[LittleEndian]]
little_endian: bool = builtin.cpu.arch.endian() == .little,

pub const Platform = @import("Agent/Platform.zig");

pub const Options = struct {
    platform: ?Platform = null,
    debug: struct {
        print_ast: bool = false,
        print_bytecode: bool = false,
    } = .{},
};

pub const Error = Allocator.Error || error{ExceptionThrown};

pub const QueuedJob = struct {
    job: Job,
    realm: ?*Realm,
};

pub fn init(gc_allocator: Allocator, options: Options) Allocator.Error!Self {
    const platform = options.platform orelse Platform.default();
    pretty_printing.state.tty_config = platform.tty_config;
    var self: Self = .{
        .gc_allocator = gc_allocator,
        .options = options,
        .pre_allocated = undefined,
        .well_known_symbols = undefined,
        .global_symbol_registry = undefined,
        .host_hooks = .{},
        .execution_context_stack = undefined,
        .queued_jobs = undefined,
        .platform = platform,
    };
    self.pre_allocated = .{
        .zero = try BigInt.from(self.gc_allocator, 0),
        .one = try BigInt.from(self.gc_allocator, 1),
    };
    self.well_known_symbols = WellKnownSymbols.init(&self);
    self.global_symbol_registry = StringHashMap(Symbol).init(self.gc_allocator);
    self.execution_context_stack = std.ArrayList(ExecutionContext).init(self.gc_allocator);
    self.queued_jobs = std.ArrayList(QueuedJob).init(self.gc_allocator);
    return self;
}

pub fn deinit(self: *Self) void {
    self.pre_allocated.zero.value.deinit();
    self.pre_allocated.one.value.deinit();
    self.global_symbol_registry.deinit();
    self.execution_context_stack.deinit();
    self.queued_jobs.deinit();
    self.platform.deinit();
}

pub fn drainJobQueue(self: *Self) void {
    while (self.queued_jobs.items.len != 0) {
        const queued_job = self.queued_jobs.orderedRemove(0);
        const current_realm = self.runningExecutionContext().realm;
        if (queued_job.realm) |new_realm| {
            self.runningExecutionContext().realm = new_realm;
        }
        _ = queued_job.job.func(queued_job.job.captures) catch {};
        self.runningExecutionContext().realm = current_realm;
    }
}

pub fn checkStackOverflow(self: *Self) error{ExceptionThrown}!void {
    if (self.platform.stack_info) |stack_info| {
        const remaining_stack = @frameAddress() - stack_info.base;
        const required_stack = (if (builtin.mode == .Debug) 128 else 64) * 1024; // Arbitrary limit
        if (remaining_stack < required_stack) {
            return self.throwException(.internal_error, "Stack overflow", .{});
        }
    }
}

pub fn createSymbol(self: *Self, description: ?String) error{Overflow}!Symbol {
    const id = blk: {
        const next_symbol_id = try std.math.add(usize, self.symbol_id, 1);
        defer self.symbol_id = next_symbol_id;
        break :blk self.symbol_id;
    };
    return .{ .id = id, .description = description };
}

const ExceptionType = enum {
    // NativeError types
    aggregate_error,
    eval_error,
    range_error,
    reference_error,
    syntax_error,
    type_error,
    uri_error,

    // Non-standard internal error
    internal_error,

    pub fn typeName(self: @This()) []const u8 {
        return switch (self) {
            .aggregate_error => "AggregateError",
            .eval_error => "EvalError",
            .range_error => "RangeError",
            .reference_error => "ReferenceError",
            .syntax_error => "SyntaxError",
            .type_error => "TypeError",
            .uri_error => "URIError",
            .internal_error => "Error",
        };
    }
};

pub fn createException(
    self: Self,
    comptime exception_type: ExceptionType,
    comptime fmt: []const u8,
    args: anytype,
) Allocator.Error!Object {
    const realm = self.currentRealm();
    const constructor = try @field(
        Realm.Intrinsics,
        "%" ++ exception_type.typeName() ++ "%",
    )(&realm.intrinsics);
    const message = try std.fmt.allocPrint(self.gc_allocator, fmt, args);
    const error_object = constructor.construct(
        &.{Value.from(try String.fromUtf8(self.gc_allocator, message))},
        null,
    ) catch |err| try noexcept(err);
    if (exception_type == .internal_error) {
        // We don't have a dedicated type for this, but let's at least adjust the name
        error_object.as(builtins.Error).fields.error_data.name = String.fromLiteral("InternalError");
    }
    return error_object;
}

pub fn clearException(self: *Self) Value {
    defer self.exception = null;
    return self.exception.?;
}

/// 5.2.3.2 Throw an Exception
/// https://tc39.es/ecma262/#sec-throw-an-exception
pub fn throwException(
    self: *Self,
    comptime exception_type: ExceptionType,
    comptime fmt: []const u8,
    args: anytype,
) error{ExceptionThrown} {
    self.exception = if (self.createException(
        exception_type,
        fmt,
        args,
    )) |error_object|
        Value.from(error_object)
    else |_|
        Value.from("Out of memory");
    return error.ExceptionThrown;
}

/// https://tc39.es/ecma262/#running-execution-context
pub fn runningExecutionContext(self: Self) *ExecutionContext {
    // At any point in time, there is at most one execution context per agent that is actually
    // executing code. This is known as the agent's running execution context.
    // The running execution context is always the top element of this stack.
    std.debug.assert(self.execution_context_stack.items.len > 0);
    return &self.execution_context_stack.items[self.execution_context_stack.items.len - 1];
}

/// https://tc39.es/ecma262/#current-realm
pub fn currentRealm(self: Self) *Realm {
    // The value of the Realm component of the running execution context is also called the current
    // Realm Record.
    return self.runningExecutionContext().realm;
}

/// https://tc39.es/ecma262/#active-function-object
pub fn activeFunctionObject(self: Self) Object {
    // The value of the Function component of the running execution context is also called the
    // active function object.
    return self.runningExecutionContext().function.?;
}

/// 9.4.1 GetActiveScriptOrModule ( )
/// https://tc39.es/ecma262/#sec-getactivescriptormodule
pub fn getActiveScriptOrModule(self: Self) ?ExecutionContext.ScriptOrModule {
    // 1. If the execution context stack is empty, return null.
    if (self.execution_context_stack.items.len == 0) return null;

    // 2. Let ec be the topmost execution context on the execution context stack whose
    //    ScriptOrModule component is not null.
    // 3. If no such execution context exists, return null. Otherwise, return ec's ScriptOrModule.
    var it = std.mem.reverseIterator(self.execution_context_stack.items);
    while (it.nextPtr()) |execution_context| {
        if (execution_context.script_or_module) |script_or_module|
            return script_or_module;
    }
    return null;
}

/// 9.4.2 ResolveBinding ( name [ , env ] )
/// https://tc39.es/ecma262/#sec-resolvebinding
pub fn resolveBinding(
    self: *Self,
    name: []const u8,
    maybe_env: ?Environment,
    strict: bool,
    maybe_lookup_cache_entry: ?*?Environment.LookupCacheEntry,
) Error!Reference {
    // 1. If env is not present or env is undefined, then
    //     a. Set env to the running execution context's LexicalEnvironment.
    // 2. Assert: env is an Environment Record.
    const env = maybe_env orelse self.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 3. Let strict be IsStrict(the syntactic production that is being evaluated).
    // NOTE: This is part of the generated bytecode and passed through as an argument.

    // 4. Return ? GetIdentifierReference(env, name, strict).
    return getIdentifierReference(env, name, strict, maybe_lookup_cache_entry);
}

/// 9.4.3 GetThisEnvironment ( )
/// https://tc39.es/ecma262/#sec-getthisenvironment
pub fn getThisEnvironment(self: *Self) Environment {
    // 1. Let env be the running execution context's LexicalEnvironment.
    var env = self.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 2. Repeat,
    while (true) {
        // a. Let exists be env.HasThisBinding().
        const exists = env.hasThisBinding();

        // b. If exists is true, return env.
        if (exists) return env;

        // c. Let outer be env.[[OuterEnv]].
        const outer = env.outerEnv();

        // d. Assert: outer is not null.
        std.debug.assert(outer != null);

        // e. Set env to outer.
        env = outer.?;
    }

    // NOTE: The loop in step 2 will always terminate because the list of environments always ends
    //       with the global environment which has a this binding.
    unreachable;
}

/// 9.4.4 ResolveThisBinding ( )
/// https://tc39.es/ecma262/#sec-resolvethisbinding
pub fn resolveThisBinding(self: *Self) Error!Value {
    // 1. Let envRec be GetThisEnvironment().
    const env = self.getThisEnvironment();

    // 2. Return ? envRec.GetThisBinding().
    return env.getThisBinding();
}

/// 9.4.5 GetNewTarget ( )
/// https://tc39.es/ecma262/#sec-getnewtarget
pub fn getNewTarget(self: *Self) ?Object {
    // 1. Let envRec be GetThisEnvironment().
    const env = self.getThisEnvironment();

    // 2. Assert: envRec has a [[NewTarget]] field.
    std.debug.assert(env == .function_environment);

    // 3. Return envRec.[[NewTarget]].
    return env.function_environment.new_target;
}

/// 9.4.6 GetGlobalObject ( )
/// https://tc39.es/ecma262/#sec-getglobalobject
pub fn getGlobalObject(self: Self) Object {
    // 1. Let currentRealm be the current Realm Record.
    const current_realm = self.currentRealm();

    // 2. Return currentRealm.[[GlobalObject]].
    return current_realm.global_object;
}

test "well_known_symbols" {
    const gc = @import("../gc.zig");
    var agent = try init(gc.allocator(), .{});
    defer agent.deinit();
    const unscopables = agent.well_known_symbols.@"@@unscopables";
    try std.testing.expectEqual(unscopables.id, 12);
    try std.testing.expectEqualStrings(unscopables.description.?.ascii, "Symbol.unscopables");
}
