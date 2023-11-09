//! 9.7 Agents
//! https://tc39.es/ecma262/#sec-agents

const builtin = @import("builtin");
const std = @import("std");

const Allocator = std.mem.Allocator;

const SafePointer = @import("any-pointer").SafePointer;

const builtins = @import("../builtins.zig");
const default_host_hooks = @import("default_host_hooks.zig");
const environments = @import("environments.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const ArgumentsList = builtins.ArgumentsList;
const BigInt = types.BigInt;
const Environment = environments.Environment;
const ExecutionContext = @import("ExecutionContext.zig");
const ImportedModulePayload = language.ImportedModulePayload;
const ImportedModuleReferrer = language.ImportedModuleReferrer;
const Job = @import("job.zig").Job;
const JobCallback = @import("job.zig").JobCallback;
const Object = types.Object;
const PropertyKeyHashMap = Object.PropertyStorage.PropertyKeyHashMap;
const Realm = @import("Realm.zig");
const Reference = types.Reference;
const SourceTextModule = language.SourceTextModule;
const String = types.String;
const Symbol = types.Symbol;
const Value = types.Value;
const getIdentifierReference = environments.getIdentifierReference;

const Self = @This();

gc_allocator: Allocator,
options: Options,
pre_allocated: struct {
    zero: BigInt,
    one: BigInt,
    pow_2_63: BigInt,
    pow_2_64: BigInt,
},
exception: ?Value = null,
symbol_id: usize = 0,
well_known_symbols: WellKnownSymbols,
global_symbol_registry: std.StringArrayHashMap(Symbol),
host_hooks: HostHooks,
execution_context_stack: std.ArrayList(ExecutionContext),
queued_promise_jobs: std.ArrayList(QueuedPromiseJob),

/// [[LittleEndian]]
little_endian: bool = builtin.cpu.arch.endian() == .little,

pub const Options = struct {
    debug: struct {
        print_ast: bool = false,
        print_bytecode: bool = false,
    } = .{},
};

pub const Error = Allocator.Error || error{ExceptionThrown};

/// 6.1.5.1 Well-Known Symbols
/// https://tc39.es/ecma262/#sec-well-known-symbols
pub const WellKnownSymbols = struct {
    @"@@asyncIterator": Symbol,
    @"@@hasInstance": Symbol,
    @"@@isConcatSpreadable": Symbol,
    @"@@iterator": Symbol,
    @"@@match": Symbol,
    @"@@matchAll": Symbol,
    @"@@replace": Symbol,
    @"@@search": Symbol,
    @"@@species": Symbol,
    @"@@split": Symbol,
    @"@@toPrimitive": Symbol,
    @"@@toStringTag": Symbol,
    @"@@unscopables": Symbol,
};

pub const HostHooks = struct {
    pub const ImportMetaProperties = PropertyKeyHashMap(Value);

    pub const ResizeArrayBufferHandled = enum {
        handled,
        unhandled,
    };

    pub const PromiseRejectionTrackerOperation = enum {
        reject,
        handle,
    };

    hostMakeJobCallback: *const fn (callback: Object) JobCallback,
    hostCallJobCallback: *const fn (
        job_callback: JobCallback,
        this_value: Value,
        arguments_list: []const Value,
    ) Error!Value,
    hostEnqueuePromiseJob: *const fn (
        agent: *Self,
        job: Job,
        realm: ?*Realm,
    ) Allocator.Error!void,
    hostGetImportMetaProperties: *const fn (
        module: *SourceTextModule,
    ) Allocator.Error!ImportMetaProperties,
    hostFinalizeImportMeta: *const fn (import_meta: Object, module: *SourceTextModule) void,
    hostLoadImportedModule: *const fn (
        agent: *Self,
        referrer: ImportedModuleReferrer,
        specifier: String,
        host_defined: SafePointer,
        payload: ImportedModulePayload,
    ) Allocator.Error!void,
    hostEnsureCanCompileStrings: *const fn (callee_realm: *Realm) Error!void,
    hostHasSourceTextAvailable: *const fn (func: Object) bool,
    hostResizeArrayBuffer: *const fn (
        buffer: *builtins.ArrayBuffer,
        new_byte_length: u53,
    ) Error!ResizeArrayBufferHandled,
    hostPromiseRejectionTracker: *const fn (
        promise: *builtins.Promise,
        operation: PromiseRejectionTrackerOperation,
    ) void,
};

pub const QueuedPromiseJob = struct {
    job: Job,
    realm: ?*Realm,
};

pub fn init(gc_allocator: Allocator, options: Options) Allocator.Error!Self {
    var self = Self{
        .gc_allocator = gc_allocator,
        .options = options,
        .pre_allocated = undefined,
        .well_known_symbols = undefined,
        .global_symbol_registry = undefined,
        .host_hooks = undefined,
        .execution_context_stack = undefined,
        .queued_promise_jobs = undefined,
    };
    self.pre_allocated = .{
        .zero = try BigInt.from(self.gc_allocator, 0),
        .one = try BigInt.from(self.gc_allocator, 1),
        .pow_2_63 = try BigInt.from(self.gc_allocator, std.math.pow(u64, 2, 63)),
        .pow_2_64 = try BigInt.from(self.gc_allocator, std.math.pow(u128, 2, 64)),
    };
    self.well_known_symbols = .{
        .@"@@asyncIterator" = self.createSymbol(String.from("Symbol.asyncIterator")) catch unreachable,
        .@"@@hasInstance" = self.createSymbol(String.from("Symbol.hasInstance")) catch unreachable,
        .@"@@isConcatSpreadable" = self.createSymbol(String.from("Symbol.isConcatSpreadable")) catch unreachable,
        .@"@@iterator" = self.createSymbol(String.from("Symbol.iterator")) catch unreachable,
        .@"@@match" = self.createSymbol(String.from("Symbol.match")) catch unreachable,
        .@"@@matchAll" = self.createSymbol(String.from("Symbol.matchAll")) catch unreachable,
        .@"@@replace" = self.createSymbol(String.from("Symbol.replace")) catch unreachable,
        .@"@@search" = self.createSymbol(String.from("Symbol.search")) catch unreachable,
        .@"@@species" = self.createSymbol(String.from("Symbol.species")) catch unreachable,
        .@"@@split" = self.createSymbol(String.from("Symbol.split")) catch unreachable,
        .@"@@toPrimitive" = self.createSymbol(String.from("Symbol.toPrimitive")) catch unreachable,
        .@"@@toStringTag" = self.createSymbol(String.from("Symbol.toStringTag")) catch unreachable,
        .@"@@unscopables" = self.createSymbol(String.from("Symbol.unscopables")) catch unreachable,
    };
    self.global_symbol_registry = std.StringArrayHashMap(Symbol).init(self.gc_allocator);
    self.host_hooks = .{
        .hostMakeJobCallback = default_host_hooks.hostMakeJobCallback,
        .hostCallJobCallback = default_host_hooks.hostCallJobCallback,
        .hostEnqueuePromiseJob = default_host_hooks.hostEnqueuePromiseJob,
        .hostGetImportMetaProperties = default_host_hooks.hostGetImportMetaProperties,
        .hostFinalizeImportMeta = default_host_hooks.hostFinalizeImportMeta,
        .hostLoadImportedModule = default_host_hooks.hostLoadImportedModule,
        .hostEnsureCanCompileStrings = default_host_hooks.hostEnsureCanCompileStrings,
        .hostHasSourceTextAvailable = default_host_hooks.hostHasSourceTextAvailable,
        .hostResizeArrayBuffer = default_host_hooks.hostResizeArrayBuffer,
        .hostPromiseRejectionTracker = default_host_hooks.hostPromiseRejectionTracker,
    };
    self.execution_context_stack = std.ArrayList(ExecutionContext).init(self.gc_allocator);
    self.queued_promise_jobs = std.ArrayList(QueuedPromiseJob).init(self.gc_allocator);
    return self;
}

pub fn deinit(self: *Self) void {
    self.pre_allocated.zero.value.deinit();
    self.pre_allocated.one.value.deinit();
    self.pre_allocated.pow_2_63.value.deinit();
    self.pre_allocated.pow_2_64.value.deinit();
    self.global_symbol_registry.deinit();
    self.execution_context_stack.deinit();
    self.queued_promise_jobs.deinit();
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

/// 5.2.3.2 Throw an Exception
/// https://tc39.es/ecma262/#sec-throw-an-exception
pub fn throwException(
    self: *Self,
    comptime exception_type: ExceptionType,
    comptime fmt: []const u8,
    args: anytype,
) error{ExceptionThrown} {
    const realm = self.currentRealm();
    const constructor = @field(
        Realm.Intrinsics,
        "%" ++ exception_type.typeName() ++ "%",
    )(&realm.intrinsics) catch unreachable; // Already allocated for the global object
    self.exception = blk: {
        const message = std.fmt.allocPrint(
            self.gc_allocator,
            fmt,
            args,
        ) catch |err| switch (err) {
            error.OutOfMemory => break :blk Value.from("Out of memory"),
        };
        const error_object = constructor.construct(.{Value.from(message)}, null) catch |err| switch (err) {
            error.OutOfMemory => break :blk Value.from("Out of memory"),
            error.ExceptionThrown => unreachable,
        };
        if (exception_type == .internal_error) {
            // We don't have a dedicated type for this, but let's at least adjust the name
            error_object.as(builtins.Error).fields.error_data.name = String.from("InternalError");
        }
        break :blk Value.from(error_object);
    };
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
) Error!Reference {
    // 1. If env is not present or env is undefined, then
    //     a. Set env to the running execution context's LexicalEnvironment.
    // 2. Assert: env is an Environment Record.
    const env = maybe_env orelse self.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 3. If the source text matched by the syntactic production that is being evaluated is
    //    contained in strict mode code, let strict be true; else let strict be false.
    // NOTE: This is part of the generated bytecode and passed through as an argument.

    // 4. Return ? GetIdentifierReference(env, name, strict).
    return getIdentifierReference(env, name, strict);
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
    const gc = @import("gc");
    var agent = try init(gc.allocator(), .{});
    defer agent.deinit();
    const unscopables = agent.well_known_symbols.@"@@unscopables";
    try std.testing.expectEqual(unscopables.id, 12);
    try std.testing.expectEqualStrings(unscopables.description.?.utf8, "Symbol.unscopables");
}
