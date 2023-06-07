//! 9.7 Agents
//! https://tc39.es/ecma262/#sec-agents

const gc = @import("gc");
const std = @import("std");

const Allocator = std.mem.Allocator;

const environments = @import("environments.zig");
const types = @import("../types.zig");

const Environment = environments.Environment;
const BigInt = types.BigInt;
const ExecutionContext = @import("ExecutionContext.zig");
const Object = types.Object;
const Realm = @import("Realm.zig");
const Reference = types.Reference;
const Symbol = types.Symbol;
const Value = types.Value;
const getIdentifierReference = environments.getIdentifierReference;

const Self = @This();

gc_allocator: Allocator,
options: Options,
pre_allocated: struct {
    one: BigInt.Value,
    two: BigInt.Value,
    pow_2_63: BigInt.Value,
    pow_2_64: BigInt.Value,
},
exception: ?Value = null,
symbol_id: usize = 0,
well_known_symbols: WellKnownSymbols,
execution_context_stack: std.ArrayList(ExecutionContext),

pub const Options = struct {
    debug: struct {
        disable_gc: bool = false,
        print_ast: bool = false,
        print_bytecode: bool = false,
    } = .{},
};

pub const Error = error{
    ExceptionThrown,
    OutOfMemory,
};

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

pub fn init(options: Options) !Self {
    var self = Self{
        .gc_allocator = gc.allocator(),
        .options = options,
        .pre_allocated = undefined,
        .well_known_symbols = undefined,
        .execution_context_stack = undefined,
    };
    if (options.debug.disable_gc) gc.disable();
    self.pre_allocated = .{
        .one = try BigInt.Value.initSet(self.gc_allocator, 1),
        .two = try BigInt.Value.initSet(self.gc_allocator, 2),
        .pow_2_63 = try BigInt.Value.initSet(self.gc_allocator, std.math.pow(u64, 2, 63)),
        .pow_2_64 = try BigInt.Value.initSet(self.gc_allocator, std.math.pow(u128, 2, 64)),
    };
    self.well_known_symbols = WellKnownSymbols{
        .@"@@asyncIterator" = self.createSymbol("Symbol.asyncIterator") catch unreachable,
        .@"@@hasInstance" = self.createSymbol("Symbol.hasInstance") catch unreachable,
        .@"@@isConcatSpreadable" = self.createSymbol("Symbol.isConcatSpreadable") catch unreachable,
        .@"@@iterator" = self.createSymbol("Symbol.iterator") catch unreachable,
        .@"@@match" = self.createSymbol("Symbol.match") catch unreachable,
        .@"@@matchAll" = self.createSymbol("Symbol.matchAll") catch unreachable,
        .@"@@replace" = self.createSymbol("Symbol.replace") catch unreachable,
        .@"@@search" = self.createSymbol("Symbol.search") catch unreachable,
        .@"@@species" = self.createSymbol("Symbol.species") catch unreachable,
        .@"@@split" = self.createSymbol("Symbol.split") catch unreachable,
        .@"@@toPrimitive" = self.createSymbol("Symbol.toPrimitive") catch unreachable,
        .@"@@toStringTag" = self.createSymbol("Symbol.toStringTag") catch unreachable,
        .@"@@unscopables" = self.createSymbol("Symbol.unscopables") catch unreachable,
    };
    self.execution_context_stack = std.ArrayList(ExecutionContext).init(self.gc_allocator);
    return self;
}

pub fn deinit(self: *Self) void {
    self.pre_allocated.one.deinit();
    self.pre_allocated.two.deinit();
    self.pre_allocated.pow_2_63.deinit();
    self.pre_allocated.pow_2_64.deinit();
    self.execution_context_stack.deinit();
}

pub fn createSymbol(self: *Self, description: ?[]const u8) !Symbol {
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
            .internal_error => "InternalError",
        };
    }
};

/// 5.2.3.2 Throw an Exception
/// https://tc39.es/ecma262/#sec-throw-an-exception
pub fn throwException(
    self: *Self,
    exception_type: ExceptionType,
    message: []const u8,
) error{ExceptionThrown} {
    // TODO: Create an actual error object.
    self.exception = Value.from(
        std.fmt.allocPrint(
            self.gc_allocator,
            "{s}: {s}",
            .{ exception_type.typeName(), message },
        ) catch "InternalError: Out of memory",
    );
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
) !Reference {
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

test "well_known_symbols" {
    var agent = try init(.{});
    defer agent.deinit();
    const unscopables = agent.well_known_symbols.@"@@unscopables";
    try std.testing.expectEqual(unscopables.id, 12);
    try std.testing.expectEqualStrings(unscopables.description.?, "Symbol.unscopables");
}
