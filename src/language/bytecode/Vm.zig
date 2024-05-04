const std = @import("std");

const Allocator = std.mem.Allocator;

const build_options = @import("build-options");
const ast = @import("../ast.zig");
const builtins = @import("../../builtins.zig");
const bytecode = @import("../bytecode.zig");
const execution = @import("../../execution.zig");
const instructions_ = @import("instructions.zig");
const language = @import("../../language.zig");
const types = @import("../../types.zig");
const utils = @import("../../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const BigInt = types.BigInt;
const ClassConstructorFields = builtins.ClassConstructorFields;
const ClassFieldDefinition = types.ClassFieldDefinition;
const ClassStaticBlockDefinition = types.ClassStaticBlockDefinition;
const Completion = types.Completion;
const Environment = execution.Environment;
const Executable = @import("Executable.zig");
const ImportedModuleReferrer = language.ImportedModuleReferrer;
const Instruction = instructions_.Instruction;
const Iterator = types.Iterator;
const IteratorKind = types.IteratorKind;
const Object = types.Object;
const PrivateElement = types.PrivateElement;
const PrivateMethodDefinition = types.PrivateMethodDefinition;
const PrivateName = types.PrivateName;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const PropertyKeyOrPrivateName = types.PropertyKeyOrPrivateName;
const Reference = types.Reference;
const SafePointer = types.SafePointer;
const String = types.String;
const Value = types.Value;
const arrayCreate = builtins.arrayCreate;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createForInIterator = builtins.createForInIterator;
const defineMethodProperty = builtins.defineMethodProperty;
const generateAndRunBytecode = bytecode.generateAndRunBytecode;
const getArrayLength = @import("../../builtins/array.zig").getArrayLength;
const getIterator = types.getIterator;
const isLessThan = types.isLessThan;
const isLooselyEqual = types.isLooselyEqual;
const isStrictlyEqual = types.isStrictlyEqual;
const makeClassConstructor = builtins.makeClassConstructor;
const makeConstructor = builtins.makeConstructor;
const makeMethod = builtins.makeMethod;
const newDeclarativeEnvironment = execution.newDeclarativeEnvironment;
const newObjectEnvironment = execution.newObjectEnvironment;
const newPrivateEnvironment = execution.newPrivateEnvironment;
const newPromiseCapability = builtins.newPromiseCapability;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryFunctionCreate = builtins.ordinaryFunctionCreate;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const performEval = builtins.performEval;
const setFunctionName = builtins.setFunctionName;

const Self = @This();

agent: *Agent,
ip: usize,
stack: std.ArrayList(Value),
iterator_stack: std.ArrayList(Iterator),
lexical_environment_stack: std.ArrayList(Environment),
reference_stack: std.ArrayList(?Reference),
exception_jump_target_stack: std.ArrayList(usize),
function_arguments: std.ArrayList(Value),
environment_lookup_cache: std.ArrayList(?Environment.LookupCacheEntry),
result: ?Value = null,
exception: ?Value = null,
iterator: ?Iterator = null,
reference: ?Reference = null,

pub fn init(agent: *Agent) Allocator.Error!Self {
    const stack = try std.ArrayList(Value).initCapacity(agent.gc_allocator, 32);
    const iterator_stack = std.ArrayList(Iterator).init(agent.gc_allocator);
    const lexical_environment_stack = std.ArrayList(Environment).init(agent.gc_allocator);
    const reference_stack = std.ArrayList(?Reference).init(agent.gc_allocator);
    const exception_jump_target_stack = std.ArrayList(usize).init(agent.gc_allocator);
    const function_arguments = try std.ArrayList(Value).initCapacity(agent.gc_allocator, 8);
    const environment_lookup_cache = std.ArrayList(?Environment.LookupCacheEntry).init(agent.gc_allocator);
    return .{
        .agent = agent,
        .ip = 0,
        .stack = stack,
        .iterator_stack = iterator_stack,
        .lexical_environment_stack = lexical_environment_stack,
        .reference_stack = reference_stack,
        .exception_jump_target_stack = exception_jump_target_stack,
        .function_arguments = function_arguments,
        .environment_lookup_cache = environment_lookup_cache,
    };
}

pub fn deinit(self: Self) void {
    self.stack.deinit();
    self.iterator_stack.deinit();
    self.lexical_environment_stack.deinit();
    self.reference_stack.deinit();
    self.exception_jump_target_stack.deinit();
    self.function_arguments.deinit();
    self.environment_lookup_cache.deinit();
}

fn fetchInstruction(self: *Self, executable: Executable) ?Instruction {
    const instructions = executable.instructions.items;
    if (self.ip >= instructions.len) return null;
    defer self.ip += 1;
    return instructions[self.ip];
}

fn fetchConstant(self: *Self, executable: Executable) Value {
    const index = self.fetchIndex(executable);
    return executable.constants.unmanaged.entries.get(index).key;
}

fn fetchIdentifier(self: *Self, executable: Executable) []const u8 {
    const index = self.fetchIndex(executable);
    return executable.identifiers.unmanaged.entries.get(index).key;
}

fn fetchFunctionOrClass(self: *Self, executable: Executable) Executable.FunctionOrClass {
    const index = self.fetchIndex(executable);
    return executable.functions_and_classes.items[index];
}

fn fetchBlock(self: *Self, executable: Executable) Executable.StatementListOrCaseBlock {
    const index = self.fetchIndex(executable);
    return executable.blocks.items[index];
}

fn fetchIndex(self: *Self, executable: Executable) Executable.IndexType {
    const b1 = @intFromEnum(self.fetchInstruction(executable).?);
    const b2 = @intFromEnum(self.fetchInstruction(executable).?);
    return std.mem.bytesToValue(Executable.IndexType, &[_]u8{ b1, b2 });
}

/// 8.6.2.1 InitializeBoundName ( name, value, environment )
/// https://tc39.es/ecma262/#sec-initializeboundname
fn initializeBoundName(
    agent: *Agent,
    name: []const u8,
    value: Value,
    environment_or_strict: union(enum) {
        environment: Environment,
        strict: bool,
    },
) Agent.Error!void {
    switch (environment_or_strict) {
        // 1. If environment is not undefined, then
        .environment => |environment| {
            // a. Perform ! environment.InitializeBinding(name, value).
            environment.initializeBinding(agent, name, value) catch |err| try noexcept(err);

            // b. Return unused.
        },

        // 2. Else,
        .strict => |strict| {
            // a. Let lhs be ? ResolveBinding(name).
            const lhs = try agent.resolveBinding(name, null, strict, null);

            // b. Return ? PutValue(lhs, value).
            try lhs.putValue(agent, value);
        },
    }
}

/// 13.3.5.1.1 EvaluateNew ( constructExpr, arguments )
/// https://tc39.es/ecma262/#sec-evaluatenew
fn evaluateNew(agent: *Agent, constructor: Value, arguments: []const Value) Agent.Error!Value {
    // 1. Let ref be ? Evaluation of constructExpr.
    // 2. Let constructor be ? GetValue(ref).
    // 3. If arguments is empty, then
    //     a. Let argList be a new empty List.
    // 4. Else,
    //     a. Let argList be ? ArgumentListEvaluation of arguments.
    // NOTE: This is part of the generated bytecode.

    // 5. If IsConstructor(constructor) is false, throw a TypeError exception.
    if (!constructor.isConstructor()) {
        return agent.throwException(.type_error, "{} is not a constructor", .{constructor});
    }

    // 6. Return ? Construct(constructor, argList).
    return Value.from(try constructor.object.construct(arguments, null));
}

/// 13.3.6.2 EvaluateCall ( func, ref, arguments, tailPosition )
/// https://tc39.es/ecma262/#sec-evaluatecall
fn evaluateCallGetThisValue(maybe_reference: ?Reference) Value {
    // 1. If ref is a Reference Record, then
    if (maybe_reference) |reference| {
        // a. If IsPropertyReference(ref) is true, then
        if (reference.isPropertyReference()) {
            // i. Let thisValue be GetThisValue(ref).
            return reference.getThisValue();
        }
        // b. Else,
        else {
            // i. Let refEnv be ref.[[Base]].
            // ii. Assert: refEnv is an Environment Record.
            const reference_environment = reference.base.environment;

            // iii. Let thisValue be refEnv.WithBaseObject().
            return if (reference_environment.withBaseObject()) |object|
                Value.from(object)
            else
                .undefined;
        }
    }
    // 2. Else,
    else {
        // a. Let thisValue be undefined.
        return .undefined;
    }
}

/// 13.3.6.2 EvaluateCall ( func, ref, arguments, tailPosition )
/// https://tc39.es/ecma262/#sec-evaluatecall
fn evaluateCall(
    agent: *Agent,
    function: Value,
    this_value: Value,
    arguments: []const Value,
) Agent.Error!Value {
    // 1-2.
    // NOTE: These are done in the function above, which is called by the prepare_call instruction.

    // 3. Let argList be ? ArgumentListEvaluation of arguments.
    // NOTE: This is part of the generated bytecode.

    // 4. If func is not an Object, throw a TypeError exception.
    if (function != .object) {
        return agent.throwException(.type_error, "{} is not an Object", .{function});
    }

    // 5. If IsCallable(func) is false, throw a TypeError exception.
    if (!function.isCallable()) {
        return agent.throwException(.type_error, "{} is not callable", .{function});
    }

    // TODO: 6. If tailPosition is true, perform PrepareForTailCall().

    // 7. Return ? Call(func, thisValue, argList).
    return function.callAssumeCallable(this_value, arguments);
}

/// 13.3.7.2 GetSuperConstructor ( )
/// https://tc39.es/ecma262/#sec-getsuperconstructor
fn getSuperConstructor(agent: *Agent) Allocator.Error!Value {
    // 1. Let envRec be GetThisEnvironment().
    const env = agent.getThisEnvironment();

    // 2. Assert: envRec is a Function Environment Record.
    std.debug.assert(env == .function_environment);

    // 3. Let activeFunction be envRec.[[FunctionObject]].
    // 4. Assert: activeFunction is an ECMAScript function object.
    const active_function = env.function_environment.function_object.object();

    // 5. Let superConstructor be ! activeFunction.[[GetPrototypeOf]]().
    const super_constructor = active_function.internalMethods().getPrototypeOf(active_function) catch |err| try noexcept(err);

    // 6. Return superConstructor.
    return if (super_constructor) |object| Value.from(object) else .null;
}

/// CallExpression : CoverCallExpressionAndAsyncArrowHead
/// Step 6.a.
fn directEval(agent: *Agent, arguments: []const Value, strict: bool) Agent.Error!Value {
    // i. Let argList be ? ArgumentListEvaluation of arguments.
    // NOTE: This is part of the generated bytecode.

    // ii. If argList has no elements, return undefined.
    if (arguments.len == 0) return .undefined;

    // iii. Let evalArg be the first element of argList.
    const eval_arg = arguments[0];

    // iv. If IsStrict(this CallExpression) is true, let strictCaller be true. Otherwise let
    //     strictCaller be false.
    const strict_caller = strict;

    // v. Return ? PerformEval(evalArg, strictCaller, true).
    return performEval(agent, eval_arg, strict_caller, true);
}

/// 13.15.3 ApplyStringOrNumericBinaryOperator ( lval, opText, rval )
/// https://tc39.es/ecma262/#sec-applystringornumericbinaryoperator
fn applyStringOrNumericBinaryOperator(
    agent: *Agent,
    lval: Value,
    operator: ast.BinaryExpression.Operator,
    rval: Value,
) Agent.Error!Value {
    var final_lval = lval;
    var final_rval = rval;

    // 1. If opText is +, then
    if (operator == .@"+") {
        // a. Let lprim be ? ToPrimitive(lval).
        const lprim = try lval.toPrimitive(agent, null);

        // b. Let rprim be ? ToPrimitive(rval).
        const rprim = try rval.toPrimitive(agent, null);

        // c. If lprim is a String or rprim is a String, then
        if (lprim == .string or rprim == .string) {
            // i. Let lstr be ? ToString(lprim).
            const lstr = try lprim.toString(agent);

            // ii. Let rstr be ? ToString(rprim).
            const rstr = try rprim.toString(agent);

            // iii. Return the string-concatenation of lstr and rstr.
            // TODO: Implement rope strings
            return Value.from(
                try std.mem.concat(agent.gc_allocator, u8, &.{ lstr.utf8, rstr.utf8 }),
            );
        }

        // d. Set lval to lprim.
        final_lval = lprim;

        // e. Set rval to rprim.
        final_rval = rprim;
    }

    // 2. NOTE: At this point, it must be a numeric operation.

    // 3. Let lnum be ? ToNumeric(lval).
    const lnum = try final_lval.toNumeric(agent);

    // 4. Let rnum be ? ToNumeric(rval).
    const rnum = try final_rval.toNumeric(agent);

    // 5. If Type(lnum) is not Type(rnum), throw a TypeError exception.
    if (std.meta.activeTag(lnum) != std.meta.activeTag(rnum)) {
        return agent.throwException(
            .type_error,
            "Left-hand side and right-hand side of numeric binary expression must have the same type",
            .{},
        );
    }

    // 6. If lnum is a BigInt, then
    if (lnum == .big_int) switch (operator) {
        // a. If opText is **, return ? BigInt::exponentiate(lnum, rnum).
        .@"**" => return Value.from(try lnum.big_int.exponentiate(agent, rnum.big_int)),

        // b. If opText is /, return ? BigInt::divide(lnum, rnum).
        .@"/" => return Value.from(try lnum.big_int.divide(agent, rnum.big_int)),

        // c. If opText is %, return ? BigInt::remainder(lnum, rnum).
        .@"%" => return Value.from(try lnum.big_int.remainder(agent, rnum.big_int)),

        // d. If opText is >>>, return ? BigInt::unsignedRightShift(lnum, rnum).
        .@">>>" => return Value.from(try lnum.big_int.unsignedRightShift(agent, rnum.big_int)),

        else => {},
    };

    // 7. Let operation be the abstract operation associated with opText and Type(lnum) in the following table:
    // 8. Return operation(lnum, rnum).
    switch (operator) {
        .@"**" => return Value.from(lnum.number.exponentiate(rnum.number)),
        .@"*" => switch (lnum) {
            .number => return Value.from(lnum.number.multiply(rnum.number)),
            .big_int => return Value.from(try lnum.big_int.multiply(agent, rnum.big_int)),
        },
        .@"/" => return Value.from(lnum.number.divide(rnum.number)),
        .@"%" => return Value.from(lnum.number.remainder(rnum.number)),
        .@"+" => switch (lnum) {
            .number => return Value.from(lnum.number.add(rnum.number)),
            .big_int => return Value.from(try lnum.big_int.add(agent, rnum.big_int)),
        },
        .@"-" => switch (lnum) {
            .number => return Value.from(lnum.number.subtract(rnum.number)),
            .big_int => return Value.from(try lnum.big_int.subtract(agent, rnum.big_int)),
        },
        .@"<<" => switch (lnum) {
            .number => return Value.from(lnum.number.leftShift(rnum.number)),
            .big_int => return Value.from(try lnum.big_int.leftShift(agent, rnum.big_int)),
        },
        .@">>" => switch (lnum) {
            .number => return Value.from(lnum.number.signedRightShift(rnum.number)),
            .big_int => return Value.from(try lnum.big_int.signedRightShift(agent, rnum.big_int)),
        },
        .@">>>" => return Value.from(lnum.number.unsignedRightShift(rnum.number)),
        .@"&" => switch (lnum) {
            .number => return Value.from(lnum.number.bitwiseAND(rnum.number)),
            .big_int => return Value.from(try lnum.big_int.bitwiseAND(agent, rnum.big_int)),
        },
        .@"^" => switch (lnum) {
            .number => return Value.from(lnum.number.bitwiseXOR(rnum.number)),
            .big_int => return Value.from(try lnum.big_int.bitwiseXOR(agent, rnum.big_int)),
        },
        .@"|" => switch (lnum) {
            .number => return Value.from(lnum.number.bitwiseOR(rnum.number)),
            .big_int => return Value.from(try lnum.big_int.bitwiseOR(agent, rnum.big_int)),
        },
    }
}

/// 14.2.3 BlockDeclarationInstantiation ( code, env )
/// https://tc39.es/ecma262/#sec-blockdeclarationinstantiation
fn blockDeclarationInstantiation(
    agent: *Agent,
    code: Executable.StatementListOrCaseBlock,
    env: Environment,
    strict: bool,
) Allocator.Error!void {
    // NOTE: Keeping this wrapped in a generic `Environment` makes a bunch of stuff below easier.
    std.debug.assert(env == .declarative_environment);

    // 1. let declarations be the LexicallyScopedDeclarations of code.
    const declarations = switch (code) {
        inline else => |node| try node.lexicallyScopedDeclarations(agent.gc_allocator),
    };
    defer agent.gc_allocator.free(declarations);

    // 2. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

    // 3. For each element d of declarations, do
    for (declarations) |declaration| {
        const bound_names = try declaration.boundNames(agent.gc_allocator);
        defer agent.gc_allocator.free(bound_names);

        // a. For each element dn of the BoundNames of d, do
        for (bound_names) |name| {
            // i. If IsConstantDeclaration of d is true, then
            if (declaration.isConstantDeclaration()) {
                // 1. Perform ! env.CreateImmutableBinding(dn, true).
                env.createImmutableBinding(agent, name, true) catch |err| try noexcept(err);
            }
            // ii. Else,
            else {
                // 1. Perform ! env.CreateMutableBinding(dn, false). NOTE: This step is replaced in section B.3.2.6.
                env.createMutableBinding(agent, name, false) catch |err| try noexcept(err);
            }
        }

        // b. If d is either a FunctionDeclaration, a GeneratorDeclaration, an
        //    AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration, then
        if (declaration == .hoistable_declaration) {
            var hoistable_declaration = declaration.hoistable_declaration;

            switch (hoistable_declaration) {
                inline else => |*function_declaration| {
                    // Assign the function body's strictness, which is needed for the deferred bytecode generation.
                    // FIXME: This should ideally happen at parse time.
                    function_declaration.function_body.strict = strict or
                        function_declaration.function_body.functionBodyContainsUseStrict();
                },
            }

            // i. Let fn be the sole element of the BoundNames of d.
            const function_name = switch (hoistable_declaration) {
                inline else => |function_declaration| function_declaration.identifier,
            }.?;

            // ii. Let fo be InstantiateFunctionObject of d with arguments env and privateEnv.
            const function_object = try switch (hoistable_declaration) {
                .function_declaration => |function_declaration| function_declaration.instantiateOrdinaryFunctionObject(agent, env, private_env),
                .generator_declaration => |generator_declaration| generator_declaration.instantiateGeneratorFunctionObject(agent, env, private_env),
                .async_function_declaration => |async_function_declaration| async_function_declaration.instantiateAsyncFunctionObject(agent, env, private_env),
                .async_generator_declaration => |async_generator_declaration| async_generator_declaration.instantiateAsyncGeneratorFunctionObject(agent, env, private_env),
            };

            // iii. Perform ! env.InitializeBinding(fn, fo). NOTE: This step is replaced in section B.3.2.6.
            env.initializeBinding(
                agent,
                function_name,
                Value.from(function_object),
            ) catch |err| try noexcept(err);
        }
    }

    // 4. Return unused.
}

/// 15.2.5 Runtime Semantics: InstantiateOrdinaryFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateordinaryfunctionexpression
fn instantiateOrdinaryFunctionExpression(
    agent: *Agent,
    function_expression: ast.FunctionExpression,
    default_name: ?[]const u8,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // FunctionExpression : function BindingIdentifier ( FormalParameters ) { FunctionBody }
    if (function_expression.identifier) |identifier| {
        // 1. Assert: name is not present.
        std.debug.assert(default_name == null);

        // 2. Set name to StringValue of BindingIdentifier.
        const name = identifier;

        // 3. Let outerEnv be the running execution context's LexicalEnvironment.
        const outer_env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
        const func_env = try newDeclarativeEnvironment(agent.gc_allocator, outer_env);

        // 5. Perform ! funcEnv.CreateImmutableBinding(name, false).
        func_env.createImmutableBinding(agent, name, false) catch |err| try noexcept(err);

        // 6. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // 7. Let sourceText be the source text matched by FunctionExpression.
        const source_text = function_expression.source_text;

        // 8. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //    FormalParameters, FunctionBody, non-lexical-this, funcEnv, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            function_expression.formal_parameters,
            function_expression.function_body,
            .non_lexical_this,
            .{ .declarative_environment = func_env },
            private_env,
        );

        // 9. Perform SetFunctionName(closure, name).
        try setFunctionName(closure, PropertyKey.from(name), null);

        // 10. Perform MakeConstructor(closure).
        try makeConstructor(closure, .{});

        // 11. Perform ! funcEnv.InitializeBinding(name, closure).
        func_env.initializeBinding(agent, name, Value.from(closure));

        // 12. Return closure.
        return closure;
    }
    // FunctionExpression : function ( FormalParameters ) { FunctionBody }
    else {
        // 1. If name is not present, set name to "".
        const name = default_name orelse "";

        // 2. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 3. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // 4. Let sourceText be the source text matched by FunctionExpression.
        const source_text = function_expression.source_text;

        // 5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //    FormalParameters, FunctionBody, non-lexical-this, env, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            function_expression.formal_parameters,
            function_expression.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 6. Perform SetFunctionName(closure, name).
        try setFunctionName(closure, PropertyKey.from(name), null);

        // 7. Perform MakeConstructor(closure).
        try makeConstructor(closure, .{});

        // 8. Return closure.
        return closure;
    }
}

/// 15.3.4 Runtime Semantics: InstantiateArrowFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiatearrowfunctionexpression
fn instantiateArrowFunctionExpression(
    agent: *Agent,
    arrow_function: ast.ArrowFunction,
    default_name: ?[]const u8,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // 1. If name is not present, set name to "".
    const name = default_name orelse "";

    // 2. Let env be the LexicalEnvironment of the running execution context.
    const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 3. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

    // 4. Let sourceText be the source text matched by ArrowFunction.
    const source_text = arrow_function.source_text;

    // 5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText, ArrowParameters,
    //    ConciseBody, lexical-this, env, privateEnv).
    const closure = try ordinaryFunctionCreate(
        agent,
        try realm.intrinsics.@"%Function.prototype%"(),
        source_text,
        arrow_function.formal_parameters,
        arrow_function.function_body,
        .lexical_this,
        env,
        private_env,
    );

    // 6. Perform SetFunctionName(closure, name).
    try setFunctionName(closure, PropertyKey.from(name), null);

    // 7. Return closure.
    return closure;
}

/// 15.4.4 Runtime Semantics: DefineMethod
/// https://tc39.es/ecma262/#sec-runtime-semantics-definemethod
fn defineMethod(
    agent: *Agent,
    function_expression: ast.FunctionExpression,
    property_name: Value,
    object: Object,
    function_prototype: ?Object,
) Agent.Error!struct { key: PropertyKeyOrPrivateName, closure: Object } {
    const realm = agent.currentRealm();

    // 1. Let propKey be ? Evaluation of ClassElementName.
    const property_key: PropertyKeyOrPrivateName = if (property_name.toPrivateName()) |private_name|
        .{ .private_name = private_name }
    else if (property_name.toPropertyKey(agent)) |property_key|
        .{ .property_key = property_key }
    else |err|
        return err;

    // 2. Let env be the running execution context's LexicalEnvironment.
    const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 3. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

    // 4. If functionPrototype is present, then
    //     a. Let prototype be functionPrototype.
    // 5. Else,
    //     a. Let prototype be %Function.prototype%.
    const prototype = function_prototype orelse try realm.intrinsics.@"%Function.prototype%"();

    // 6. Let sourceText be the source text matched by MethodDefinition.
    const source_text = function_expression.source_text;

    // 7. Let closure be OrdinaryFunctionCreate(prototype, sourceText, UniqueFormalParameters,
    //    FunctionBody, non-lexical-this, env, privateEnv).
    const closure = try ordinaryFunctionCreate(
        agent,
        prototype,
        source_text,
        function_expression.formal_parameters,
        function_expression.function_body,
        .non_lexical_this,
        env,
        private_env,
    );

    // 8. Perform MakeMethod(closure, object).
    makeMethod(closure.as(builtins.ECMAScriptFunction), object);

    // 9. Return the Record { [[Key]]: propKey, [[Closure]]: closure }.
    return .{ .key = property_key, .closure = closure };
}

/// 15.4.5 Runtime Semantics: MethodDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-methoddefinitionevaluation
fn methodDefinitionEvaluation(
    agent: *Agent,
    // Like ast.MethodDefinition but with an evaluated ast.PropertyName
    method_definition: struct {
        property_name: Value,
        method: ast.MethodDefinition.Method,
    },
    object: Object,
    enumerable: bool,
) Agent.Error!?PrivateMethodDefinition {
    const realm = agent.currentRealm();
    switch (method_definition.method) {
        // MethodDefinition : ClassElementName ( UniqueFormalParameters ) { FunctionBody }
        .method => |function_expression| {
            // 1. Let methodDef be ? DefineMethod of MethodDefinition with argument object.
            const method_def = try defineMethod(
                agent,
                function_expression,
                method_definition.property_name,
                object,
                null,
            );

            // 2. Perform SetFunctionName(methodDef.[[Closure]], methodDef.[[Key]]).
            try setFunctionName(method_def.closure, method_def.key, null);

            // 3. Return ? DefineMethodProperty(object, methodDef.[[Key]], methodDef.[[Closure]], enumerable).
            return defineMethodProperty(object, method_def.key, method_def.closure, enumerable);
        },

        // MethodDefinition : get ClassElementName ( ) { FunctionBody }
        .get => |function_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the running execution context's LexicalEnvironment.
            const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

            // 4. Let sourceText be the source text matched by MethodDefinition.
            const source_text = function_expression.source_text;

            // 5. Let formalParameterList be an instance of the production FormalParameters : [empty] .
            const formal_parameter_list: ast.FormalParameters = .{ .items = &.{} };

            // 6. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
            //    formalParameterList, FunctionBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%Function.prototype%"(),
                source_text,
                formal_parameter_list,
                function_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 7. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 8. Perform SetFunctionName(closure, propKey, "get").
            try setFunctionName(closure, property_key_or_private_name, "get");

            switch (property_key_or_private_name) {
                // 9. If propKey is a Private Name, then
                .private_name => |private_name| {
                    // a. Return PrivateElement { [[Key]]: propKey, [[Kind]]: accessor, [[Get]]: closure, [[Set]]: undefined }.
                    const private_element: PrivateElement = .{
                        .accessor = .{ .get = closure, .set = null },
                    };
                    return .{ .private_name = private_name, .private_element = private_element };
                },
                // 10. Else,
                .property_key => |property_key| {
                    // a. Let desc be the PropertyDescriptor {
                    //      [[Get]]: closure, [[Enumerable]]: enumerable, [[Configurable]]: true
                    //    }.
                    const property_descriptor: PropertyDescriptor = .{
                        .get = closure,
                        .enumerable = enumerable,
                        .configurable = true,
                    };

                    // b. Perform ? DefinePropertyOrThrow(object, propKey, desc).
                    try object.definePropertyOrThrow(property_key, property_descriptor);

                    // c. Return unused.
                    return null;
                },
            }
        },

        // MethodDefinition : set ClassElementName ( PropertySetParameterList ) { FunctionBody }
        .set => |function_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the running execution context's LexicalEnvironment.
            const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

            // 4. Let sourceText be the source text matched by MethodDefinition.
            const source_text = function_expression.source_text;

            // 5. Let closure be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
            //    PropertySetParameterList, FunctionBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%Function.prototype%"(),
                source_text,
                function_expression.formal_parameters,
                function_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 6. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 7. Perform SetFunctionName(closure, propKey, "set").
            try setFunctionName(closure, property_key_or_private_name, "set");

            switch (property_key_or_private_name) {
                // 8. If propKey is a Private Name, then
                .private_name => |private_name| {
                    // a. Return PrivateElement { [[Key]]: propKey, [[Kind]]: accessor, [[Get]]: undefined, [[Set]]: closure }.
                    const private_element: PrivateElement = .{
                        .accessor = .{ .get = null, .set = closure },
                    };
                    return .{ .private_name = private_name, .private_element = private_element };
                },
                // 9. Else,
                .property_key => |property_key| {
                    // a. Let desc be the PropertyDescriptor {
                    //      [[Set]]: closure, [[Enumerable]]: enumerable, [[Configurable]]: true
                    //    }.
                    const property_descriptor: PropertyDescriptor = .{
                        .set = closure,
                        .enumerable = enumerable,
                        .configurable = true,
                    };

                    // b. Perform ? DefinePropertyOrThrow(object, propKey, desc).
                    try object.definePropertyOrThrow(property_key, property_descriptor);

                    // c. Return unused.
                    return null;
                },
            }
        },

        // GeneratorMethod : * ClassElementName ( UniqueFormalParameters ) { GeneratorBody }
        .generator => |generator_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the running execution context's LexicalEnvironment.
            const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

            // 4. Let sourceText be the source text matched by GeneratorMethod.
            const source_text = generator_expression.source_text;

            // 5. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
            //    UniqueFormalParameters, GeneratorBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
                source_text,
                generator_expression.formal_parameters,
                generator_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 6. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 7. Perform SetFunctionName(closure, propKey).
            try setFunctionName(closure, property_key_or_private_name, null);

            // 8. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
            const prototype = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%GeneratorPrototype%"(),
            );

            // 9. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            closure.definePropertyOrThrow(PropertyKey.from("prototype"), .{
                .value = Value.from(prototype),
                .writable = true,
                .enumerable = false,
                .configurable = false,
            }) catch |err| try noexcept(err);

            // 10. Return ? DefineMethodProperty(object, propKey, closure, enumerable).
            return defineMethodProperty(object, property_key_or_private_name, closure, enumerable);
        },

        // AsyncGeneratorMethod : async * ClassElementName ( UniqueFormalParameters ) { AsyncGeneratorBody }
        .async_generator => |async_generator_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the running execution context's LexicalEnvironment.
            const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

            // 4. Let sourceText be the source text matched by AsyncGeneratorMethod.
            const source_text = async_generator_expression.source_text;

            // 5. Let closure be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%,
            //    sourceText, UniqueFormalParameters, AsyncGeneratorBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%AsyncFunction.prototype%"(),
                source_text,
                async_generator_expression.formal_parameters,
                async_generator_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 6. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 7. Perform SetFunctionName(closure, propKey).
            try setFunctionName(closure, property_key_or_private_name, null);

            // 8. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorFunction.prototype.prototype%).
            const prototype = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
            );

            // 9. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            closure.definePropertyOrThrow(PropertyKey.from("prototype"), .{
                .value = Value.from(prototype),
                .writable = true,
                .enumerable = false,
                .configurable = false,
            }) catch |err| try noexcept(err);

            // 10. Return ? DefineMethodProperty(object, propKey, closure, enumerable).
            return defineMethodProperty(object, property_key_or_private_name, closure, enumerable);
        },

        // AsyncMethod : async ClassElementName ( UniqueFormalParameters ) { AsyncFunctionBody }
        .@"async" => |async_function_expression| {
            // 1. Let propKey be ? Evaluation of ClassElementName.
            const property_key_or_private_name: PropertyKeyOrPrivateName = if (method_definition.property_name.toPrivateName()) |private_name|
                .{ .private_name = private_name }
            else if (method_definition.property_name.toPropertyKey(agent)) |property_key|
                .{ .property_key = property_key }
            else |err|
                return err;

            // 2. Let env be the LexicalEnvironment of the running execution context.
            const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

            // 3. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

            // 4. Let sourceText be the source text matched by AsyncMethod.
            const source_text = async_function_expression.source_text;

            // 5. Let closure be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
            //    UniqueFormalParameters, AsyncFunctionBody, non-lexical-this, env, privateEnv).
            const closure = try ordinaryFunctionCreate(
                agent,
                try realm.intrinsics.@"%AsyncFunction.prototype%"(),
                source_text,
                async_function_expression.formal_parameters,
                async_function_expression.function_body,
                .non_lexical_this,
                env,
                private_env,
            );

            // 6. Perform MakeMethod(closure, object).
            makeMethod(closure.as(builtins.ECMAScriptFunction), object);

            // 7. Perform SetFunctionName(closure, propKey).
            try setFunctionName(closure, property_key_or_private_name, null);

            // 8. Return ? DefineMethodProperty(object, propKey, closure, enumerable).
            return defineMethodProperty(object, property_key_or_private_name, closure, enumerable);
        },
    }
}

/// 15.5.4 Runtime Semantics: InstantiateGeneratorFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiategeneratorfunctionexpression
fn instantiateGeneratorFunctionExpression(
    agent: *Agent,
    generator_expression: ast.GeneratorExpression,
    default_name: ?[]const u8,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // GeneratorExpression : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
    if (generator_expression.identifier) |identifier| {
        // 1. Assert: name is not present.
        std.debug.assert(default_name == null);

        // 2. Set name to StringValue of BindingIdentifier.
        const name = identifier;

        // 3. Let outerEnv be the running execution context's LexicalEnvironment.
        const outer_env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
        const func_env = try newDeclarativeEnvironment(agent.gc_allocator, outer_env);

        // 5. Perform ! funcEnv.CreateImmutableBinding(name, false).
        func_env.createImmutableBinding(agent, name, false) catch |err| try noexcept(err);

        // 6. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // 7. Let sourceText be the source text matched by GeneratorExpression.
        const source_text = generator_expression.source_text;

        // 8. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
        //    FormalParameters, GeneratorBody, non-lexical-this, funcEnv, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
            source_text,
            generator_expression.formal_parameters,
            generator_expression.function_body,
            .non_lexical_this,
            .{ .declarative_environment = func_env },
            private_env,
        );

        // 9. Perform SetFunctionName(closure, name).
        try setFunctionName(closure, PropertyKey.from(name), null);

        // 10. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%GeneratorPrototype%"(),
        );

        // 11. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
        //       [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //     }).
        closure.definePropertyOrThrow(PropertyKey.from("prototype"), .{
            .value = Value.from(prototype),
            .writable = true,
            .enumerable = false,
            .configurable = false,
        }) catch |err| try noexcept(err);

        // 12. Perform ! funcEnv.InitializeBinding(name, closure).
        func_env.initializeBinding(agent, name, Value.from(closure));

        // 13. Return closure.
        return closure;
    }
    // GeneratorExpression : function * ( FormalParameters ) { GeneratorBody }
    else {
        // 1. If name is not present, set name to "".
        const name = default_name orelse "";

        // 2. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 3. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // 4. Let sourceText be the source text matched by GeneratorExpression.
        const source_text = generator_expression.source_text;

        // 5. Let closure be OrdinaryFunctionCreate(%GeneratorFunction.prototype%, sourceText,
        //    FormalParameters, GeneratorBody, non-lexical-this, env, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%GeneratorFunction.prototype%"(),
            source_text,
            generator_expression.formal_parameters,
            generator_expression.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 6. Perform SetFunctionName(closure, name).
        try setFunctionName(closure, PropertyKey.from(name), null);

        // 7. Let prototype be OrdinaryObjectCreate(%GeneratorFunction.prototype.prototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%GeneratorPrototype%"(),
        );

        // 8. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
        //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //    }).
        closure.definePropertyOrThrow(PropertyKey.from("prototype"), .{
            .value = Value.from(prototype),
            .writable = true,
            .enumerable = false,
            .configurable = false,
        }) catch |err| try noexcept(err);

        // 9. Return closure.
        return closure;
    }
}

/// 15.6.4 Runtime Semantics: InstantiateAsyncGeneratorFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncgeneratorfunctionexpression
fn instantiateAsyncGeneratorFunctionExpression(
    agent: *Agent,
    async_generator_expression: ast.AsyncGeneratorExpression,
    default_name: ?[]const u8,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // GeneratorExpression : function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
    if (async_generator_expression.identifier) |identifier| {
        // 1. Assert: name is not present.
        std.debug.assert(default_name == null);

        // 2. Set name to StringValue of BindingIdentifier.
        const name = identifier;

        // 3. Let outerEnv be the running execution context's LexicalEnvironment.
        const outer_env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
        const func_env = try newDeclarativeEnvironment(agent.gc_allocator, outer_env);

        // 5. Perform ! funcEnv.CreateImmutableBinding(name, false).
        func_env.createImmutableBinding(agent, name, false) catch |err| try noexcept(err);

        // 6. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // 7. Let sourceText be the source text matched by AsyncGeneratorExpression.
        const source_text = async_generator_expression.source_text;

        // 8. Let closure be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%, sourceText,
        //    FormalParameters, AsyncGeneratorBody, non-lexical-this, funcEnv, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
            source_text,
            async_generator_expression.formal_parameters,
            async_generator_expression.function_body,
            .non_lexical_this,
            .{ .declarative_environment = func_env },
            private_env,
        );

        // 9. Perform SetFunctionName(closure, name).
        try setFunctionName(closure, PropertyKey.from(name), null);

        // 10. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorFunction.prototype.prototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
        );

        // 11. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
        //       [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //     }).
        closure.definePropertyOrThrow(PropertyKey.from("prototype"), .{
            .value = Value.from(prototype),
            .writable = true,
            .enumerable = false,
            .configurable = false,
        }) catch |err| try noexcept(err);

        // 12. Perform ! funcEnv.InitializeBinding(name, closure).
        func_env.initializeBinding(agent, name, Value.from(closure));

        // 13. Return closure.
        return closure;
    }
    // AsyncGeneratorExpression : async function * ( FormalParameters ) { AsyncGeneratorBody }
    else {
        // 1. If name is not present, set name to "".
        const name = default_name orelse "";

        // 2. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 3. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // 4. Let sourceText be the source text matched by AsyncGeneratorExpression.
        const source_text = async_generator_expression.source_text;

        // 5. Let closure be OrdinaryFunctionCreate(%AsyncGeneratorFunction.prototype%, sourceText,
        //    FormalParameters, AsyncGeneratorBody, non-lexical-this, env, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorFunction.prototype%"(),
            source_text,
            async_generator_expression.formal_parameters,
            async_generator_expression.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 6. Perform SetFunctionName(closure, name).
        try setFunctionName(closure, PropertyKey.from(name), null);

        // 7. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorFunction.prototype.prototype%).
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
        );

        // 8. Perform ! DefinePropertyOrThrow(closure, "prototype", PropertyDescriptor {
        //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
        //    }).
        closure.definePropertyOrThrow(PropertyKey.from("prototype"), .{
            .value = Value.from(prototype),
            .writable = true,
            .enumerable = false,
            .configurable = false,
        }) catch |err| try noexcept(err);

        // 9. Return closure.
        return closure;
    }
}

/// 15.7.10 Runtime Semantics: ClassFieldDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-classfielddefinitionevaluation
fn classFieldDefinitionEvaluation(
    agent: *Agent,
    field_definition: ast.FieldDefinition,
    home_object: Object,
) Agent.Error!ClassFieldDefinition {
    const realm = agent.currentRealm();

    // 1. Let name be ? Evaluation of ClassElementName.
    const name: PropertyKeyOrPrivateName = blk: {
        const value = (try generateAndRunBytecode(
            agent,
            field_definition.class_element_name,
            .{},
        )).value.?;
        if (value.toPrivateName()) |private_name| {
            break :blk .{ .private_name = private_name };
        } else if (value.toPropertyKey(agent)) |property_key| {
            break :blk .{ .property_key = property_key };
        } else |err| try noexcept(err);
    };

    // 2. If Initializer is present, then
    const initializer = if (field_definition.initializer) |initializer| blk: {
        // a. Let formalParameterList be an instance of the production FormalParameters : [empty] .
        const formal_parameter_list: ast.FormalParameters = .{ .items = &.{} };

        // b. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // c. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // d. Let sourceText be the empty sequence of Unicode code points.
        const source_text = "";

        // e. Let initializer be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
        //    formalParameterList, Initializer, non-lexical-this, env, privateEnv).
        const body: ast.FunctionBody = blk_body: {
            const statement = try agent.gc_allocator.create(ast.Statement);
            statement.* = .{ .return_statement = .{ .expression = initializer } };
            const items = try agent.gc_allocator.alloc(ast.StatementListItem, 1);
            items[0] = .{ .statement = statement };
            const statement_list: ast.StatementList = .{ .items = items };
            break :blk_body .{
                .type = .normal,
                .statement_list = statement_list,
                .strict = true,
            };
        };
        const initializer_function = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%Function.prototype%"(),
            source_text,
            formal_parameter_list,
            body,
            .non_lexical_this,
            env,
            private_env,
        );

        // f. Perform MakeMethod(initializer, homeObject).
        makeMethod(initializer_function.as(builtins.ECMAScriptFunction), home_object);

        // g. Set initializer.[[ClassFieldInitializerName]] to name.
        initializer_function.as(builtins.ECMAScriptFunction).fields.class_field_initializer_name = name;

        break :blk initializer_function;
    }
    // 3. Else,
    else blk: {
        // a. Let initializer be empty.
        break :blk null;
    };

    // 4. Return the ClassFieldDefinition Record { [[Name]]: name, [[Initializer]]: initializer }.
    return .{
        .name = name,
        .initializer = if (initializer) |i| i.as(builtins.ECMAScriptFunction) else null,
    };
}

/// 15.7.11 Runtime Semantics: ClassStaticBlockDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-classstaticblockdefinitionevaluation
fn classStaticBlockDefinitionEvaluation(
    agent: *Agent,
    class_static_block: ast.ClassStaticBlock,
    home_object: Object,
) Allocator.Error!ClassStaticBlockDefinition {
    const realm = agent.currentRealm();

    // 1. Let lex be the running execution context's LexicalEnvironment.
    const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 2. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

    // 3. Let sourceText be the empty sequence of Unicode code points.
    const source_text = "";

    // 4. Let formalParameters be an instance of the production FormalParameters : [empty] .
    const formal_parameter_list: ast.FormalParameters = .{ .items = &.{} };

    // 5. Let bodyFunction be OrdinaryFunctionCreate(%Function.prototype%, sourceText,
    //    formalParameters, ClassStaticBlockBody, non-lexical-this, lex, privateEnv).
    const body: ast.FunctionBody = blk_body: {
        // NOTE: This serves as a replacement for EvaluateClassStaticBlockBody, which invokes
        //       FunctionDeclarationInstantiation before evaluating the statement list.
        break :blk_body .{
            .type = .normal,
            .statement_list = class_static_block.statement_list,
            .strict = true,
        };
    };
    const body_function = try ordinaryFunctionCreate(
        agent,
        try realm.intrinsics.@"%Function.prototype%"(),
        source_text,
        formal_parameter_list,
        body,
        .non_lexical_this,
        env,
        private_env,
    );

    // 6. Perform MakeMethod(bodyFunction, homeObject).
    makeMethod(body_function.as(builtins.ECMAScriptFunction), home_object);

    // 7. Return the ClassStaticBlockDefinition Record { [[BodyFunction]]: bodyFunction }.
    return .{ .body_function = body_function.as(builtins.ECMAScriptFunction) };
}

/// 15.7.13 Runtime Semantics: ClassElementEvaluation
/// https://tc39.es/ecma262/#sec-static-semantics-classelementevaluation
fn classElementEvaluation(
    agent: *Agent,
    class_element: ast.ClassElement,
    object: Object,
) Agent.Error!?union(enum) {
    class_field_definition: ClassFieldDefinition,
    class_static_block_definition: ClassStaticBlockDefinition,
    private_method_definition: PrivateMethodDefinition,
} {
    switch (class_element) {
        // ClassElement :
        //     FieldDefinition ;
        //     static FieldDefinition ;
        .field_definition,
        .static_field_definition,
        => |field_definition| {
            // 1. Return ? ClassFieldDefinitionEvaluation of FieldDefinition with argument object.
            return .{
                .class_field_definition = try classFieldDefinitionEvaluation(
                    agent,
                    field_definition,
                    object,
                ),
            };
        },

        // ClassElement :
        //     MethodDefinition
        //     static MethodDefinition
        .method_definition,
        .static_method_definition,
        => |method_definition| {
            // 1. Return ? MethodDefinitionEvaluation of MethodDefinition with arguments object and false.
            const property_name = (try generateAndRunBytecode(
                agent,
                method_definition.class_element_name,
                .{},
            )).value.?;
            if (try methodDefinitionEvaluation(
                agent,
                .{ .property_name = property_name, .method = method_definition.method },
                object,
                false,
            )) |private_method_definition|
                return .{ .private_method_definition = private_method_definition }
            else
                return null;
        },

        // ClassElement : ClassStaticBlock
        .class_static_block => |class_static_block| {
            // 1. Return ClassStaticBlockDefinitionEvaluation of ClassStaticBlock with argument object.
            return .{
                .class_static_block_definition = try classStaticBlockDefinitionEvaluation(
                    agent,
                    class_static_block,
                    object,
                ),
            };
        },

        // ClassElement : ;
        .empty_statement => {
            // 1. Return unused.
            return null;
        },
    }
}

/// 15.7.14 Runtime Semantics: ClassDefinitionEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-classdefinitionevaluation
fn classDefinitionEvaluation(
    agent: *Agent,
    class_tail: ast.ClassTail,
    class_binding: ?[]const u8,
    class_name: []const u8,
) Agent.Error!Object {
    const realm = agent.currentRealm();

    // 1. Let env be the LexicalEnvironment of the running execution context.
    const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 2. Let classEnv be NewDeclarativeEnvironment(env).
    const class_env = try newDeclarativeEnvironment(agent.gc_allocator, env);

    // 3. If classBinding is not undefined, then
    if (class_binding != null) {
        // a. Perform ! classEnv.CreateImmutableBinding(classBinding, true).
        class_env.createImmutableBinding(
            agent,
            class_binding.?,
            true,
        ) catch |err| try noexcept(err);
    }

    // 4. Let outerPrivateEnvironment be the running execution context's PrivateEnvironment.
    const outer_private_environment = agent.runningExecutionContext().ecmascript_code.?.private_environment;

    // 5. Let classPrivateEnvironment be NewPrivateEnvironment(outerPrivateEnvironment).
    const class_private_environment = try newPrivateEnvironment(
        agent.gc_allocator,
        outer_private_environment,
    );

    // 6. If ClassBody is present, then
    if (class_tail.class_body.class_element_list.items.len != 0) {
        const private_bound_identifiers = try class_tail.class_body.privateBoundIdentifiers(agent.gc_allocator);
        defer agent.gc_allocator.free(private_bound_identifiers);

        // a. For each String dn of the PrivateBoundIdentifiers of ClassBody, do
        for (private_bound_identifiers) |declared_name| {
            // i. If classPrivateEnvironment.[[Names]] contains a Private Name pn such that pn.[[Description]] is dn, then
            if (class_private_environment.names.contains(declared_name)) {
                // 1. Assert: This is only possible for getter/setter pairs.
            }
            // ii. Else,
            else {
                var symbol = agent.createSymbol(String.from(declared_name)) catch |err| switch (err) {
                    error.Overflow => return agent.throwException(
                        .internal_error,
                        "Maximum number of symbols exceeded",
                        .{},
                    ),
                };
                symbol.private = true;

                // 1. Let name be a new Private Name whose [[Description]] is dn.
                const name: PrivateName = .{ .symbol = symbol };

                // 2. Append name to classPrivateEnvironment.[[Names]].
                try class_private_environment.names.putNoClobber(declared_name, name);
            }
        }
    }

    var prototype_parent: ?Object = undefined;
    var constructor_parent: Object = undefined;

    // 7. If ClassHeritage is not present, then
    if (class_tail.class_heritage == null) {
        // a. Let protoParent be %Object.prototype%.
        prototype_parent = try realm.intrinsics.@"%Object.prototype%"();

        // b. Let constructorParent be %Function.prototype%.
        constructor_parent = try realm.intrinsics.@"%Function.prototype%"();
    }
    // 8. Else,
    else {
        // a. Set the running execution context's LexicalEnvironment to classEnv.
        agent.runningExecutionContext().ecmascript_code.?.lexical_environment = .{ .declarative_environment = class_env };

        // b. NOTE: The running execution context's PrivateEnvironment is outerPrivateEnvironment
        //    when evaluating ClassHeritage.
        // c. Let superclassRef be Completion(Evaluation of ClassHeritage).
        const superclass_ref = generateAndRunBytecode(
            agent,
            ast.ExpressionStatement{ .expression = class_tail.class_heritage.?.* },
            .{},
        );

        // d. Set the running execution context's LexicalEnvironment to env.
        agent.runningExecutionContext().ecmascript_code.?.lexical_environment = env;

        // e. Let superclass be ? GetValue(? superclassRef).
        // NOTE: Wrapping the Expression node in a ExpressionStatement above ensures a get_value
        //       instruction is emitted for references.
        const superclass = (try superclass_ref).value.?;

        // f. If superclass is null, then
        if (superclass == .null) {
            // i. Let protoParent be null.
            prototype_parent = null;

            // ii. Let constructorParent be %Function.prototype%.
            constructor_parent = try realm.intrinsics.@"%Function.prototype%"();
        }
        // g. Else if IsConstructor(superclass) is false, then
        else if (!superclass.isConstructor()) {
            // i. Throw a TypeError exception.
            return agent.throwException(.type_error, "{} is not a constructor", .{superclass});
        }
        // h. Else,
        else {
            // i. Let protoParent be ? Get(superclass, "prototype").
            const prototype_parent_value = try superclass.object.get(PropertyKey.from("prototype"));

            // ii. If protoParent is not an Object and protoParent is not null, throw a TypeError
            //     exception.
            if (prototype_parent_value != .object and prototype_parent_value != .null) {
                return agent.throwException(
                    .type_error,
                    "{} is not an Object or null",
                    .{prototype_parent_value},
                );
            }

            prototype_parent = if (prototype_parent_value == .object)
                prototype_parent_value.object
            else
                null;

            // iii. Let constructorParent be superclass.
            constructor_parent = superclass.object;
        }
    }

    // 9. Let proto be OrdinaryObjectCreate(protoParent).
    const prototype = try ordinaryObjectCreate(agent, prototype_parent);

    // 10. If ClassBody is not present, let constructor be empty.
    // 11. Else, let constructor be ConstructorMethod of ClassBody.
    const constructor = class_tail.class_body.constructorMethod();

    // 12. Set the running execution context's LexicalEnvironment to classEnv.
    agent.runningExecutionContext().ecmascript_code.?.lexical_environment = .{ .declarative_environment = class_env };

    // 13. Set the running execution context's PrivateEnvironment to classPrivateEnvironment.
    agent.runningExecutionContext().ecmascript_code.?.private_environment = class_private_environment;

    // 14. If constructor is empty, then
    var function = if (constructor == null) blk: {
        // a. Let defaultConstructor be a new Abstract Closure with no parameters that captures
        //    nothing and performs the following steps when called:
        const default_constructor = struct {
            fn func(agent_: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
                // i. Let args be the List of arguments that was passed to this function by [[Call]]
                //    or [[Construct]].
                const args = arguments.values;

                // ii. If NewTarget is undefined, throw a TypeError exception.
                if (new_target == null) {
                    return agent_.throwException(.type_error, "Class must be constructed with 'new'", .{});
                }

                // iii. Let F be the active function object.
                const function = agent_.activeFunctionObject();
                const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);

                // iv. If F.[[ConstructorKind]] is derived, then
                var result = if (class_constructor_fields.constructor_kind == .derived) blk: {
                    // 1. NOTE: This branch behaves similarly to constructor(...args) { super(...args); }.
                    //    The most notable distinction is that while the aforementioned ECMAScript
                    //    source text observably calls the @@iterator method on %Array.prototype%,
                    //    this function does not.

                    // 2. Let func be ! F.[[GetPrototypeOf]]().
                    const prototype_function = function.internalMethods().getPrototypeOf(function) catch |err| try noexcept(err);

                    // 3. If IsConstructor(func) is false, throw a TypeError exception.
                    if (prototype_function == null or !Value.from(prototype_function.?).isConstructor()) {
                        return agent_.throwException(
                            .type_error,
                            "{} is not a constructor",
                            .{
                                if (prototype_function == null)
                                    .undefined
                                else
                                    Value.from(prototype_function.?),
                            },
                        );
                    }

                    // 4. Let result be ? Construct(func, args, NewTarget).
                    break :blk try prototype_function.?.construct(args, new_target.?);
                }
                // v. Else,
                else blk: {
                    // 1. NOTE: This branch behaves similarly to constructor() {}.
                    // 2. Let result be ? OrdinaryCreateFromConstructor(NewTarget, "%Object.prototype%").
                    break :blk try ordinaryCreateFromConstructor(
                        builtins.Object,
                        agent_,
                        new_target.?,
                        "%Object.prototype%",
                        {},
                    );
                };

                // vi. Perform ? InitializeInstanceElements(result, F).
                try result.initializeInstanceElements(function);

                // vii. Return result.
                return Value.from(result);
            }
        }.func;

        // b. Let F be CreateBuiltinFunction(defaultConstructor, 0, className, « [[ConstructorKind]],
        //    [[SourceText]] », the current Realm Record, constructorParent).
        const class_constructor_fields = try agent.gc_allocator.create(ClassConstructorFields);
        break :blk try createBuiltinFunction(agent, .{ .constructor = default_constructor }, .{
            .length = 0,
            .name = class_name,
            .realm = agent.currentRealm(),
            .prototype = constructor_parent,
            .additional_fields = SafePointer.make(*ClassConstructorFields, class_constructor_fields),
        });
    }
    // 15. Else,
    else blk: {
        // a. Let constructorInfo be ! DefineMethod of constructor with arguments proto and
        //    constructorParent.
        const constructor_info = defineMethod(
            agent,
            constructor.?.method.method,
            Value.from("constructor"),
            prototype,
            constructor_parent,
        ) catch |err| try noexcept(err);

        // b. Let F be constructorInfo.[[Closure]].
        const function = constructor_info.closure;

        // c. Perform MakeClassConstructor(F).
        makeClassConstructor(function.as(builtins.ECMAScriptFunction));

        // d. Perform SetFunctionName(F, className).
        try setFunctionName(function, PropertyKey.from(class_name), null);

        break :blk function;
    };

    // 16. Perform MakeConstructor(F, false, proto).
    try makeConstructor(function, .{ .writable_prototype = false, .prototype = prototype });

    // 17. If ClassHeritage is present, set F.[[ConstructorKind]] to derived.
    if (class_tail.class_heritage != null) {
        if (function.is(builtins.ECMAScriptFunction)) {
            function.as(builtins.ECMAScriptFunction).fields.constructor_kind = .derived;
        } else if (function.is(builtins.BuiltinFunction)) {
            const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
            class_constructor_fields.constructor_kind = .derived;
        } else unreachable;
    }

    // 18. Perform ! DefineMethodProperty(proto, "constructor", F, false).
    _ = defineMethodProperty(
        prototype,
        .{ .property_key = PropertyKey.from("constructor") },
        function,
        false,
    ) catch |err| try noexcept(err);

    // 19. If ClassBody[opt] is not present, let elements be a new empty List.
    // 20. Else, let elements be NonConstructorElements of ClassBody.
    const elements = try class_tail.class_body.nonConstructorElements(agent.gc_allocator);
    defer agent.gc_allocator.free(elements);

    // 21. Let instancePrivateMethods be a new empty List.
    var instance_private_methods = std.ArrayList(PrivateMethodDefinition).init(agent.gc_allocator);
    // Converted to owned slice, no `deinit()` needed

    // 22. Let staticPrivateMethods be a new empty List.
    var static_private_methods = std.ArrayList(PrivateMethodDefinition).init(agent.gc_allocator);
    defer static_private_methods.deinit();

    // 23. Let instanceFields be a new empty List.
    var instance_fields = std.ArrayList(ClassFieldDefinition).init(agent.gc_allocator);
    // Converted to owned slice, no `deinit()` needed

    // 24. Let staticElements be a new empty List.
    var static_elements = std.ArrayList(union(enum) {
        class_field_definition: ClassFieldDefinition,
        class_static_block_definition: ClassStaticBlockDefinition,
    }).init(agent.gc_allocator);
    defer static_elements.deinit();

    // 25. For each ClassElement e of elements, do
    for (elements) |class_element| {
        // a. If IsStatic of e is false, then
        const element_or_error = if (!class_element.isStatic()) blk: {
            // i. Let element be Completion(ClassElementEvaluation of e with argument proto).
            break :blk classElementEvaluation(agent, class_element, prototype);
        }
        // b. Else,
        else blk: {
            // i. Let element be Completion(ClassElementEvaluation of e with argument F).
            break :blk classElementEvaluation(agent, class_element, function);
        };

        // c. If element is an abrupt completion, then
        // d. Set element to ! element.
        const element = element_or_error catch |err| {
            // i. Set the running execution context's LexicalEnvironment to env.
            agent.runningExecutionContext().ecmascript_code.?.lexical_environment = env;

            // ii. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
            agent.runningExecutionContext().ecmascript_code.?.private_environment = outer_private_environment;

            // iii. Return ? element.
            return err;
        };

        if (element != null) switch (element.?) {
            // e. If element is a PrivateElement, then
            .private_method_definition => |private_method_definition| {
                // i. Assert: element.[[Kind]] is either method or accessor.
                std.debug.assert(
                    private_method_definition.private_element == .method or
                        private_method_definition.private_element == .accessor,
                );

                // ii. If IsStatic of e is false, let container be instancePrivateMethods.
                // iii. Else, let container be staticPrivateMethods.
                const container = if (!class_element.isStatic())
                    &instance_private_methods
                else
                    &static_private_methods;

                // iv. If container contains a PrivateElement pe such that pe.[[Key]] is element.[[Key]], then
                if (for (container.items, 0..) |p, index| {
                    if (p.private_name.eql(private_method_definition.private_name)) {
                        break .{ .private_element = p.private_element, .index = index };
                    }
                } else null) |found| {
                    // 1. Assert: element.[[Kind]] and pe.[[Kind]] are both accessor.
                    std.debug.assert(
                        private_method_definition.private_element == .accessor and
                            found.private_element == .accessor,
                    );

                    var combined = private_method_definition;

                    // 2. If element.[[Get]] is undefined, then
                    if (private_method_definition.private_element.accessor.get == null) {
                        // a. Let combined be PrivateElement {
                        //      [[Key]]: element.[[Key]], [[Kind]]: accessor, [[Get]]: pe.[[Get]], [[Set]]: element.[[Set]]
                        //    }.
                        combined.private_element.accessor.get = found.private_element.accessor.get;
                    }
                    // 3. Else,
                    else {
                        // a. Let combined be PrivateElement {
                        //      [[Key]]: element.[[Key]], [[Kind]]: accessor, [[Get]]: element.[[Get]], [[Set]]: pe.[[Set]]
                        //    }.
                        combined.private_element.accessor.set = found.private_element.accessor.set;
                    }

                    // 4. Replace pe in container with combined.
                    container.items[found.index] = combined;
                }
                // v. Else,
                else {
                    // 1. Append element to container.
                    try container.append(private_method_definition);
                }
            },

            // f. Else if element is a ClassFieldDefinition Record, then
            .class_field_definition => |class_field_definition| {
                // i. If IsStatic of e is false, append element to instanceFields.
                // ii. Else, append element to staticElements.
                if (!class_element.isStatic())
                    try instance_fields.append(class_field_definition)
                else
                    try static_elements.append(
                        .{ .class_field_definition = class_field_definition },
                    );
            },

            // g. Else if element is a ClassStaticBlockDefinition Record, then
            .class_static_block_definition => |class_static_block_definition| {
                // i. Append element to staticElements.
                try static_elements.append(
                    .{ .class_static_block_definition = class_static_block_definition },
                );
            },
        };
    }

    // 26. Set the running execution context's LexicalEnvironment to env.
    agent.runningExecutionContext().ecmascript_code.?.lexical_environment = env;

    // 27. If classBinding is not undefined, then
    if (class_binding != null) {
        // a. Perform ! classEnv.InitializeBinding(classBinding, F).
        class_env.initializeBinding(agent, class_binding.?, Value.from(function));
    }

    // 28. Set F.[[PrivateMethods]] to instancePrivateMethods.
    if (function.is(builtins.ECMAScriptFunction)) {
        function.as(builtins.ECMAScriptFunction).fields.private_methods = try instance_private_methods.toOwnedSlice();
    } else if (function.is(builtins.BuiltinFunction)) {
        const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
        class_constructor_fields.private_methods = try instance_private_methods.toOwnedSlice();
    } else unreachable;

    // 29. Set F.[[Fields]] to instanceFields.
    if (function.is(builtins.ECMAScriptFunction)) {
        function.as(builtins.ECMAScriptFunction).fields.fields = try instance_fields.toOwnedSlice();
    } else if (function.is(builtins.BuiltinFunction)) {
        const class_constructor_fields = function.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
        class_constructor_fields.fields = try instance_fields.toOwnedSlice();
    } else unreachable;

    // 30. For each PrivateElement method of staticPrivateMethods, do
    for (static_private_methods.items) |method| {
        // a. Perform ! PrivateMethodOrAccessorAdd(F, method).
        function.privateMethodOrAccessorAdd(
            method.private_name,
            method.private_element,
        ) catch |err| try noexcept(err);
    }

    // 31. For each element elementRecord of staticElements, do
    for (static_elements.items) |element| {
        const result = switch (element) {
            // a. If elementRecord is a ClassFieldDefinition Record, then
            .class_field_definition => |class_field_definition| blk: {
                // i. Let result be Completion(DefineField(F, elementRecord)).
                break :blk function.defineField(class_field_definition);
            },
            // b. Else,
            .class_static_block_definition => |class_static_block_definition| blk: {
                // i. Assert: elementRecord is a ClassStaticBlockDefinition Record.
                // ii. Let result be Completion(Call(elementRecord.[[BodyFunction]], F)).
                const body_function = class_static_block_definition.body_function.object();
                _ = Value.from(body_function).callAssumeCallableNoArgs(
                    Value.from(function),
                ) catch |err| break :blk err;
            },
        };

        // c. If result is an abrupt completion, then
        _ = result catch |err| {
            // i. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
            agent.runningExecutionContext().ecmascript_code.?.private_environment = outer_private_environment;

            // ii. Return ? result.
            return err;
        };
    }

    // 32. Set the running execution context's PrivateEnvironment to outerPrivateEnvironment.
    agent.runningExecutionContext().ecmascript_code.?.private_environment = outer_private_environment;

    // 33. Return F.
    return function;
}

/// 15.7.15 Runtime Semantics: BindingClassDeclarationEvaluation
/// https://tc39.es/ecma262/#sec-runtime-semantics-bindingclassdeclarationevaluation
fn bindingClassDeclarationEvaluation(
    agent: *Agent,
    class_declaration: ast.ClassDeclaration,
) Agent.Error!Object {
    // ClassDeclaration : class BindingIdentifier ClassTail
    if (class_declaration.identifier) |identifier| {
        // 1. Let className be StringValue of BindingIdentifier.
        const class_name = identifier;

        // 2. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments className and className.
        const value = try classDefinitionEvaluation(
            agent,
            class_declaration.class_tail,
            class_name,
            class_name,
        );

        // 3. Set value.[[SourceText]] to the source text matched by ClassDeclaration.
        if (value.is(builtins.ECMAScriptFunction)) {
            value.as(builtins.ECMAScriptFunction).fields.source_text = class_declaration.source_text;
        } else if (value.is(builtins.BuiltinFunction)) {
            const class_constructor_fields = value.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
            class_constructor_fields.source_text = class_declaration.source_text;
        } else unreachable;

        // 4. Let env be the running execution context's LexicalEnvironment.
        const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 5. Perform ? InitializeBoundName(className, value, env).
        try initializeBoundName(agent, class_name, Value.from(value), .{ .environment = env });

        // 6. Return value.
        return value;
    }
    // ClassDeclaration : class ClassTail
    else {
        // 1. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments undefined and
        //    "default".
        const value = try classDefinitionEvaluation(
            agent,
            class_declaration.class_tail,
            null,
            "default",
        );

        // 2. Set value.[[SourceText]] to the source text matched by ClassDeclaration.
        if (value.is(builtins.ECMAScriptFunction)) {
            value.as(builtins.ECMAScriptFunction).fields.source_text = class_declaration.source_text;
        } else if (value.is(builtins.BuiltinFunction)) {
            const class_constructor_fields = value.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
            class_constructor_fields.source_text = class_declaration.source_text;
        } else unreachable;

        // 3. Return value.
        return value;
    }
}

/// 15.8.3 Runtime Semantics: InstantiateAsyncFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncfunctionexpression
fn instantiateAsyncFunctionExpression(
    agent: *Agent,
    async_function_expression: ast.AsyncFunctionExpression,
    default_name: ?[]const u8,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // AsyncFunctionExpression : async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }
    if (async_function_expression.identifier) |identifier| {
        // 1. Assert: name is not present.
        std.debug.assert(default_name == null);

        // 2. Set name to StringValue of BindingIdentifier.
        const name = identifier;

        // 3. Let outerEnv be the running execution context's LexicalEnvironment.
        const outer_env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 4. Let funcEnv be NewDeclarativeEnvironment(outerEnv).
        const func_env = try newDeclarativeEnvironment(agent.gc_allocator, outer_env);

        // 5. Perform ! funcEnv.CreateImmutableBinding(name, false).
        func_env.createImmutableBinding(agent, name, false) catch |err| try noexcept(err);

        // 6. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // 7. Let sourceText be the source text matched by AsyncFunctionExpression.
        const source_text = async_function_expression.source_text;

        // 8. Let closure be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
        //    FormalParameters, AsyncFunctionBody, non-lexical-this, funcEnv, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncFunction.prototype%"(),
            source_text,
            async_function_expression.formal_parameters,
            async_function_expression.function_body,
            .non_lexical_this,
            .{ .declarative_environment = func_env },
            private_env,
        );

        // 9. Perform SetFunctionName(closure, name).
        try setFunctionName(closure, PropertyKey.from(name), null);

        // 10. Perform ! funcEnv.InitializeBinding(name, closure).
        func_env.initializeBinding(agent, name, Value.from(closure));

        // 11. Return closure.
        return closure;
    }
    // AsyncFunctionExpression : async function ( FormalParameters ) { AsyncFunctionBody }
    else {
        // 1. If name is not present, set name to "".
        const name = default_name orelse "";

        // 2. Let env be the LexicalEnvironment of the running execution context.
        const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

        // 3. Let privateEnv be the running execution context's PrivateEnvironment.
        const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

        // 4. Let sourceText be the source text matched by AsyncFunctionExpression.
        const source_text = async_function_expression.source_text;

        // 5. Let closure be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText,
        //    FormalParameters, AsyncFunctionBody, non-lexical-this, env, privateEnv).
        const closure = try ordinaryFunctionCreate(
            agent,
            try realm.intrinsics.@"%AsyncFunction.prototype%"(),
            source_text,
            async_function_expression.formal_parameters,
            async_function_expression.function_body,
            .non_lexical_this,
            env,
            private_env,
        );

        // 6. Perform SetFunctionName(closure, name).
        try setFunctionName(closure, PropertyKey.from(name), null);

        // 7. Return closure.
        return closure;
    }
}

/// 15.9.4 Runtime Semantics: InstantiateAsyncArrowFunctionExpression
/// https://tc39.es/ecma262/#sec-runtime-semantics-instantiateasyncarrowfunctionexpression
fn instantiateAsyncArrowFunctionExpression(
    agent: *Agent,
    async_arrow_function: ast.AsyncArrowFunction,
    default_name: ?[]const u8,
) Allocator.Error!Object {
    const realm = agent.currentRealm();

    // 1. If name is not present, set name to "".
    const name = default_name orelse "";

    // 2. Let env be the LexicalEnvironment of the running execution context.
    const env = agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

    // 3. Let privateEnv be the running execution context's PrivateEnvironment.
    const private_env = agent.runningExecutionContext().ecmascript_code.?.private_environment;

    // 4. Let sourceText be the source text matched by AsyncArrowFunction.
    const source_text = async_arrow_function.source_text;

    // 5. Let head be the AsyncArrowHead that is covered by CoverCallExpressionAndAsyncArrowHead.
    // 6. Let parameters be the ArrowFormalParameters of head.
    const parameters = async_arrow_function.formal_parameters;

    // 7. Let closure be OrdinaryFunctionCreate(%AsyncFunction.prototype%, sourceText, parameters,
    //    AsyncConciseBody, lexical-this, env, privateEnv).
    const closure = try ordinaryFunctionCreate(
        agent,
        try realm.intrinsics.@"%AsyncFunction.prototype%"(),
        source_text,
        parameters,
        async_arrow_function.function_body,
        .lexical_this,
        env,
        private_env,
    );

    // 8. Perform SetFunctionName(closure, name).
    try setFunctionName(closure, PropertyKey.from(name), null);

    // 9. Return closure.
    return closure;
}

pub fn executeInstruction(
    self: *Self,
    executable: Executable,
    instruction: Instruction,
) Agent.Error!void {
    switch (instruction) {
        .apply_string_or_numeric_binary_operator => {
            const operator_type = self.fetchIndex(executable);
            const operator: ast.BinaryExpression.Operator = @enumFromInt(operator_type);
            const rval = self.stack.pop();
            const lval = self.stack.pop();
            self.result = try applyStringOrNumericBinaryOperator(self.agent, lval, operator, rval);
        },
        .array_create => self.result = Value.from(try arrayCreate(self.agent, 0, null)),
        .array_push_value => {
            const init_value = self.stack.pop();
            const array = self.stack.pop().object;
            const index = getArrayLength(array);
            // From ArrayAccumulation:
            // 4. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(nextIndex)), initValue).
            array.createDataPropertyOrThrow(
                PropertyKey.from(@as(PropertyKey.IntegerIndex, index)),
                init_value,
            ) catch |err| try noexcept(err);
            self.result = Value.from(array);
        },
        .array_set_length => {
            const length = self.fetchIndex(executable);
            const array = self.result.?.object;
            // From ArrayAccumulation:
            // 2. Perform ? Set(array, "length", 𝔽(len), true).
            try array.set(PropertyKey.from("length"), Value.from(length), .throw);
        },
        .array_spread_value => {
            const spread_obj = self.stack.pop();
            const array = self.stack.pop().object;
            var next_index: u53 = @intCast(getArrayLength(array));

            // From ArrayAccumulation:
            // 3. Let iteratorRecord be ? GetIterator(spreadObj, sync).
            var iterator = try getIterator(self.agent, spread_obj, .sync);

            // 4. Repeat,
            //     a. Let next be ? IteratorStepValue(iteratorRecord).
            //     b. If next is done, return nextIndex.
            while (try iterator.stepValue()) |next| : (next_index += 1) {
                // c. Perform ! CreateDataPropertyOrThrow(array, ! ToString(𝔽(nextIndex)), next).
                array.createDataPropertyOrThrow(
                    PropertyKey.from(next_index),
                    next,
                ) catch |err| try noexcept(err);

                // d. Set nextIndex to nextIndex + 1.
            }
            self.result = Value.from(array);
        },
        .binding_class_declaration_evaluation => {
            const class_declaration = self.fetchFunctionOrClass(executable).class_declaration;
            self.result = Value.from(try bindingClassDeclarationEvaluation(self.agent, class_declaration));
        },
        .bitwise_not => {
            const value = self.result.?;
            self.result = switch (value) {
                .number => |number| Value.from(number.bitwiseNOT()),
                .big_int => |big_int| Value.from(try big_int.bitwiseNOT(self.agent)),
                else => unreachable,
            };
        },
        .block_declaration_instantiation => {
            const block = self.fetchBlock(executable);
            const old_env = self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment;
            const block_env = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);
            const strict = self.fetchIndex(executable) == 1;
            try blockDeclarationInstantiation(
                self.agent,
                block,
                .{ .declarative_environment = block_env },
                strict,
            );
            self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = .{
                .declarative_environment = block_env,
            };
        },
        .class_definition_evaluation => {
            const class_expression = self.fetchFunctionOrClass(executable).class_expression;

            // ClassExpression : class BindingIdentifier ClassTail
            if (class_expression.identifier) |identifier| {
                // 1. Let className be StringValue of BindingIdentifier.
                const class_name = identifier;

                // 2. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments className and className.
                const value = try classDefinitionEvaluation(
                    self.agent,
                    class_expression.class_tail,
                    class_name,
                    class_name,
                );

                // 3. Set value.[[SourceText]] to the source text matched by ClassExpression.
                if (value.is(builtins.ECMAScriptFunction)) {
                    value.as(builtins.ECMAScriptFunction).fields.source_text = class_expression.source_text;
                } else if (value.is(builtins.BuiltinFunction)) {
                    const class_constructor_fields = value.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
                    class_constructor_fields.source_text = class_expression.source_text;
                } else unreachable;

                // 4. Return value.
                self.result = Value.from(value);
            }
            // ClassExpression : class ClassTail
            else {
                // 1. Let value be ? ClassDefinitionEvaluation of ClassTail with arguments undefined and "".
                const value = try classDefinitionEvaluation(
                    self.agent,
                    class_expression.class_tail,
                    null,
                    "",
                );

                // 2. Set value.[[SourceText]] to the source text matched by ClassExpression.
                if (value.is(builtins.ECMAScriptFunction)) {
                    value.as(builtins.ECMAScriptFunction).fields.source_text = class_expression.source_text;
                } else if (value.is(builtins.BuiltinFunction)) {
                    const class_constructor_fields = value.as(builtins.BuiltinFunction).fields.additional_fields.cast(*ClassConstructorFields);
                    class_constructor_fields.source_text = class_expression.source_text;
                } else unreachable;

                // 3. Return value.
                self.result = Value.from(value);
            }
        },
        .create_catch_binding => {
            // TODO: This should create a new environment - for now we approximate this by creating
            //       a new binding and ignoring the error if one already exists.
            const name = self.fetchIdentifier(executable);
            const thrown_value = self.exception.?;
            self.exception = null;
            const running_context = self.agent.runningExecutionContext();
            const catch_env = running_context.ecmascript_code.?.lexical_environment;
            if (!try catch_env.hasBinding(name)) {
                try catch_env.createMutableBinding(self.agent, name, false);
                try catch_env.initializeBinding(self.agent, name, thrown_value);
            } else {
                catch_env.setMutableBinding(
                    self.agent,
                    name,
                    thrown_value,
                    false,
                ) catch unreachable;
            }
        },
        .create_object_property_iterator => {
            const value = self.result.?;

            // b. Let obj be ! ToObject(exprValue).
            const object = value.toObject(self.agent) catch |err| try noexcept(err);

            // c. Let iterator be EnumerateObjectProperties(obj).
            const iterator = try createForInIterator(self.agent, object);

            // d. Let nextMethod be ! GetV(iterator, "next").
            const next_method = iterator.get(PropertyKey.from("next")) catch |err| try noexcept(err);

            // e. Return the Iterator Record { [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]: false }.
            self.iterator = .{ .iterator = iterator, .next_method = next_method, .done = false };
        },
        .create_with_environment => {
            const object = self.result.?.object;
            const old_env = self.lexical_environment_stack.getLast();
            const new_env: Environment = .{
                .object_environment = try newObjectEnvironment(
                    self.agent.gc_allocator,
                    object,
                    true,
                    old_env,
                ),
            };
            self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = new_env;
        },
        .decrement => {
            const value = self.result.?;
            self.result = switch (value) {
                .number => |number| Value.from(number.subtract(.{ .i32 = 1 })),
                .big_int => |big_int| Value.from(
                    try big_int.subtract(self.agent, self.agent.pre_allocated.one),
                ),
                else => @panic("decrement instruction must only be used with numeric value"),
            };
        },
        .delete => {
            // NOTE: 1-2. are part of the generated bytecode.
            const reference = self.reference.?;

            // 3. If IsUnresolvableReference(ref) is true, then
            if (reference.isUnresolvableReference()) {
                // a. Assert: ref.[[Strict]] is false.
                std.debug.assert(!reference.strict);

                // b. Return true.
                self.result = Value.from(true);
                return;
            }

            // 4. If IsPropertyReference(ref) is true, then
            if (reference.isPropertyReference()) {
                // a. Assert: IsPrivateReference(ref) is false.
                std.debug.assert(!reference.isPrivateReference());

                // b. If IsSuperReference(ref) is true, throw a ReferenceError exception.
                if (reference.isSuperReference()) {
                    return self.agent.throwException(
                        .reference_error,
                        "Cannot delete super reference",
                        .{},
                    );
                }

                // c. Let baseObj be ? ToObject(ref.[[Base]]).
                const base_obj = try reference.base.value.toObject(self.agent);

                // d. Let deleteStatus be ? baseObj.[[Delete]](ref.[[ReferencedName]]).
                const referenced_name = switch (reference.referenced_name) {
                    .string => |string| PropertyKey.from(string),
                    .symbol => |symbol| PropertyKey.from(symbol),
                    .private_name => unreachable,
                };
                const delete_status = try base_obj.internalMethods().delete(base_obj, referenced_name);

                // e. If deleteStatus is false and ref.[[Strict]] is true, throw a TypeError exception.
                if (!delete_status and reference.strict) {
                    return self.agent.throwException(.type_error, "Could not delete property", .{});
                }

                // f. Return deleteStatus.
                self.result = Value.from(delete_status);
            }
            // 5. Else,
            else {
                // a. Let base be ref.[[Base]].
                // b. Assert: base is an Environment Record.
                const base = reference.base.environment;

                // c. Return ? base.DeleteBinding(ref.[[ReferencedName]]).
                self.result = Value.from(try base.deleteBinding(reference.referenced_name.string));
            }
        },
        .evaluate_call => {
            const maybe_reference = self.reference_stack.getLastOrNull() orelse null;
            const argument_count = self.fetchIndex(executable);
            const strict = self.fetchIndex(executable) == 1;
            self.function_arguments.clearRetainingCapacity();
            try self.function_arguments.ensureTotalCapacity(argument_count);
            for (0..argument_count) |_| {
                const argument = self.stack.pop();
                self.function_arguments.appendAssumeCapacity(argument);
            }
            std.mem.reverse(Value, self.function_arguments.items);
            const arguments = self.function_arguments.items;
            const this_value = self.stack.pop();
            const function = self.stack.pop();

            const realm = self.agent.currentRealm();
            const eval = try realm.intrinsics.@"%eval%"();

            // 6. If ref is a Reference Record, IsPropertyReference(ref) is false, and
            //    ref.[[ReferencedName]] is "eval", then
            if (maybe_reference) |reference| {
                if (!reference.isPropertyReference() and
                    reference.referenced_name == .string and
                    std.mem.eql(u8, reference.referenced_name.string, "eval") and

                    // a. If SameValue(func, %eval%) is true, then
                    function.object.sameValue(eval))
                {
                    self.result = try directEval(self.agent, arguments, strict);
                    return;
                }
            }

            self.result = try evaluateCall(
                self.agent,
                function,
                this_value,
                arguments,
            );
        },
        .evaluate_import_call => {
            const realm = self.agent.currentRealm();

            // 1. Let referrer be GetActiveScriptOrModule().
            // 2. If referrer is null, set referrer to the current Realm Record.
            const referrer: ImportedModuleReferrer = if (self.agent.getActiveScriptOrModule()) |script_or_module|
                switch (script_or_module) {
                    .script => |script| .{ .script = script },
                    .module => |module| .{ .module = module },
                }
            else
                .{ .realm = realm };

            // 3. Let argRef be ? Evaluation of AssignmentExpression.
            // 4. Let specifier be ? GetValue(argRef).
            const specifier = self.stack.pop();

            // 5. Let promiseCapability be ! NewPromiseCapability(%Promise%).
            const promise_capability = newPromiseCapability(
                self.agent,
                Value.from(try realm.intrinsics.@"%Promise%"()),
            ) catch |err| try noexcept(err);

            // 6. Let specifierString be Completion(ToString(specifier)).
            const specifier_string = specifier.toString(self.agent) catch |err| {
                // 7. IfAbruptRejectPromise(specifierString, promiseCapability).
                self.result = Value.from(try promise_capability.rejectPromise(self.agent, err));
                return;
            };

            // 8. Perform HostLoadImportedModule(referrer, specifierString, empty, promiseCapability).
            try self.agent.host_hooks.hostLoadImportedModule(
                self.agent,
                referrer,
                specifier_string,
                SafePointer.null_pointer,
                .{ .promise_capability = promise_capability },
            );

            // 9. Return promiseCapability.[[Promise]].
            self.result = Value.from(promise_capability.promise);
        },
        .evaluate_new => {
            const argument_count = self.fetchIndex(executable);
            self.function_arguments.clearRetainingCapacity();
            try self.function_arguments.ensureTotalCapacity(argument_count);
            for (0..argument_count) |_| {
                const argument = self.stack.pop();
                self.function_arguments.appendAssumeCapacity(argument);
            }
            std.mem.reverse(Value, self.function_arguments.items);
            const arguments = self.function_arguments.items;
            const constructor = self.stack.pop();
            self.result = try evaluateNew(self.agent, constructor, arguments);
        },
        // 13.3.3 EvaluatePropertyAccessWithExpressionKey ( baseValue, expression, strict )
        // https://tc39.es/ecma262/#sec-evaluate-property-access-with-expression-key
        .evaluate_property_access_with_expression_key => {
            // 1. Let propertyNameReference be ? Evaluation of expression.
            // 2. Let propertyNameValue be ? GetValue(propertyNameReference).
            const property_name_value = self.stack.pop();

            const strict = self.fetchIndex(executable) == 1;
            const base_value = self.stack.pop();

            // 3. Let propertyKey be ? ToPropertyKey(propertyNameValue).
            const property_key = try property_name_value.toPropertyKey(self.agent);

            // 4. Return the Reference Record {
            //      [[Base]]: baseValue,
            //      [[ReferencedName]]: propertyKey,
            //      [[Strict]]: strict,
            //      [[ThisValue]]: empty
            //    }.
            self.reference = .{
                .base = .{ .value = base_value },
                .referenced_name = switch (try property_key.toStringOrSymbol(self.agent)) {
                    .string => |string| .{ .string = string },
                    .symbol => |symbol| .{ .symbol = symbol },
                },
                .strict = strict,
                .this_value = null,
            };
        },
        // 13.3.4 EvaluatePropertyAccessWithIdentifierKey ( baseValue, identifierName, strict )
        // https://tc39.es/ecma262/#sec-evaluate-property-access-with-identifier-key
        .evaluate_property_access_with_identifier_key => {
            // 1. Let propertyNameString be StringValue of identifierName.
            const property_name_string = self.fetchIdentifier(executable);

            const strict = self.fetchIndex(executable) == 1;
            const base_value = self.stack.pop();

            // 2. Return the Reference Record {
            //      [[Base]]: baseValue,
            //      [[ReferencedName]]: propertyNameString,
            //      [[Strict]]: strict,
            //      [[ThisValue]]: empty
            //    }.
            self.reference = .{
                .base = .{ .value = base_value },
                .referenced_name = .{ .string = property_name_string },
                .strict = strict,
                .this_value = null,
            };
        },
        .evaluate_super_call => {
            const argument_count = self.fetchIndex(executable);

            // 1. Let newTarget be GetNewTarget().
            const new_target = self.agent.getNewTarget();

            // 2. Assert: newTarget is an Object.
            std.debug.assert(new_target != null);

            // 3. Let func be GetSuperConstructor().
            const function = try getSuperConstructor(self.agent);

            // 4. Let argList be ? ArgumentListEvaluation of Arguments.
            self.function_arguments.clearRetainingCapacity();
            try self.function_arguments.ensureTotalCapacity(argument_count);
            for (0..argument_count) |_| {
                const argument = self.stack.pop();
                self.function_arguments.appendAssumeCapacity(argument);
            }
            std.mem.reverse(Value, self.function_arguments.items);
            const arguments = self.function_arguments.items;

            // 5. If IsConstructor(func) is false, throw a TypeError exception.
            if (!function.isConstructor()) {
                return self.agent.throwException(
                    .type_error,
                    "{} is not a constructor",
                    .{function},
                );
            }

            // 6. Let result be ? Construct(func, argList, newTarget).
            var result = try function.object.construct(arguments, new_target);

            // 7. Let thisER be GetThisEnvironment().
            const this_environment = self.agent.getThisEnvironment();

            // 8. Perform ? thisER.BindThisValue(result).
            _ = try this_environment.bindThisValue(Value.from(result));

            // 9. Let F be thisER.[[FunctionObject]].
            // 10. Assert: F is an ECMAScript function object.
            const constructor = this_environment.function_environment.function_object.object();

            // 11. Perform ? InitializeInstanceElements(result, F).
            try result.initializeInstanceElements(constructor);

            // 12. Return result.
            self.result = Value.from(result);
        },
        .for_declaration_binding_instantiation => {
            const name = self.fetchIdentifier(executable);
            const is_constant_declaration = self.fetchIndex(executable) == 1;

            // iii. Let iterationEnv be NewDeclarativeEnvironment(oldEnv).
            const old_env = self.lexical_environment_stack.getLast();
            const environment = try newDeclarativeEnvironment(self.agent.gc_allocator, old_env);

            // iv. Perform ForDeclarationBindingInstantiation of lhs with argument iterationEnv.

            // 14.7.5.4 Runtime Semantics: ForDeclarationBindingInstantiation
            // https://tc39.es/ecma262/#sec-runtime-semantics-fordeclarationbindinginstantiation
            {
                // 1. For each element name of the BoundNames of ForBinding, do
                // a. If IsConstantDeclaration of LetOrConst is true, then
                if (is_constant_declaration) {
                    // i. Perform ! environment.CreateImmutableBinding(name, true).
                    environment.createImmutableBinding(
                        self.agent,
                        name,
                        true,
                    ) catch |err| try noexcept(err);
                }
                // b. Else,
                else {
                    // i. Perform ! environment.CreateMutableBinding(name, false).
                    environment.createMutableBinding(
                        self.agent,
                        name,
                        false,
                    ) catch |err| try noexcept(err);
                }

                // 2. Return unused.
            }

            // v. Set the running execution context's LexicalEnvironment to iterationEnv.
            self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = .{
                .declarative_environment = environment,
            };
        },
        .get_iterator => {
            const iterator_kind: IteratorKind = @enumFromInt((self.fetchIndex(executable)));
            self.iterator = try getIterator(self.agent, self.result.?, iterator_kind);
        },
        .get_new_target => {
            self.result = if (self.agent.getNewTarget()) |new_target|
                Value.from(new_target)
            else
                .undefined;
        },
        .get_or_create_import_meta => {
            // 1. Let module be GetActiveScriptOrModule().
            // 2. Assert: module is a Source Text Module Record.
            var module = self.agent.getActiveScriptOrModule().?.module;

            // 3. Let importMeta be module.[[ImportMeta]].
            // 4. If importMeta is empty, then
            if (module.import_meta == null) {
                // a. Set importMeta to OrdinaryObjectCreate(null).
                const import_meta = try ordinaryObjectCreate(self.agent, null);

                // b. Let importMetaValues be HostGetImportMetaProperties(module).
                var import_meta_values = try self.agent.host_hooks.hostGetImportMetaProperties(module);
                defer import_meta_values.deinit();

                // c. For each Record { [[Key]], [[Value]] } p of importMetaValues, do
                var it = import_meta_values.iterator();
                while (it.next()) |entry| {
                    // i. Perform ! CreateDataPropertyOrThrow(importMeta, p.[[Key]], p.[[Value]]).
                    import_meta.createDataPropertyOrThrow(
                        entry.key_ptr.*,
                        entry.value_ptr.*,
                    ) catch |err| try noexcept(err);
                }

                // d. Perform HostFinalizeImportMeta(importMeta, module).
                self.agent.host_hooks.hostFinalizeImportMeta(import_meta, module);

                // e. Set module.[[ImportMeta]] to importMeta.
                module.import_meta = import_meta;

                // f. Return importMeta.
                self.result = Value.from(import_meta);
            }
            // 5. Else,
            else {
                // a. Assert: importMeta is an Object.
                // b. Return importMeta.
                self.result = Value.from(module.import_meta.?);
            }
        },
        .get_value => {
            if (self.reference) |reference| self.result = try reference.getValue(self.agent);
            self.reference = null;
        },
        .greater_than => {
            const rval = self.stack.pop();
            const lval = self.stack.pop();

            // 5. Let r be ? IsLessThan(rval, lval, false).
            const result = try isLessThan(self.agent, rval, lval, .right_first);

            // 6. If r is undefined, return false. Otherwise, return r.
            self.result = Value.from(result orelse false);
        },
        .greater_than_equals => {
            const rval = self.stack.pop();
            const lval = self.stack.pop();

            // 5. Let r be ? IsLessThan(lval, rval, true).
            const result = try isLessThan(self.agent, lval, rval, .left_first);

            // 6. If r is either true or undefined, return false. Otherwise, return true.
            self.result = Value.from(!(result orelse true));
        },
        .has_private_element => {
            const private_identifier = self.fetchIdentifier(executable);
            const rval = self.stack.pop();

            // 4. If rval is not an Object, throw a TypeError exception.
            if (rval != .object) {
                return self.agent.throwException(
                    .type_error,
                    "Right-hand side of 'in' operator must be an object",
                    .{},
                );
            }

            // 5. Let privateEnv be the running execution context's PrivateEnvironment.
            const private_environment = self.agent.runningExecutionContext().ecmascript_code.?.private_environment.?;

            // 6. Let privateName be ResolvePrivateIdentifier(privateEnv, privateIdentifier).
            const private_name = private_environment.resolvePrivateIdentifier(private_identifier);

            // 7. If PrivateElementFind(rval, privateName) is not empty, return true.
            // 8. Return false.
            self.result = Value.from(rval.object.privateElementFind(private_name) != null);
        },
        .has_property => {
            const rval = self.stack.pop();
            const lval = self.stack.pop();

            // 5. If rval is not an Object, throw a TypeError exception.
            if (rval != .object) {
                return self.agent.throwException(
                    .type_error,
                    "Right-hand side of 'in' operator must be an object",
                    .{},
                );
            }

            // 6. Return ? HasProperty(rval, ? ToPropertyKey(lval)).
            self.result = Value.from(
                try rval.object.hasProperty(try lval.toPropertyKey(self.agent)),
            );
        },
        .increment => {
            const value = self.result.?;
            self.result = switch (value) {
                .number => |number| Value.from(number.add(.{ .i32 = 1 })),
                .big_int => |big_int| Value.from(
                    try big_int.add(self.agent, self.agent.pre_allocated.one),
                ),
                else => @panic("increment instruction must only be used with numeric value"),
            };
        },
        .initialize_default_export => {
            const value = self.result.?;

            // 3. Let env be the running execution context's LexicalEnvironment.
            const environment = self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment;

            // 4. Perform ? InitializeBoundName("*default*", value, env).
            try initializeBoundName(
                self.agent,
                "*default*",
                value,
                .{ .environment = environment },
            );
        },
        .initialize_referenced_binding => {
            const reference = self.reference_stack.getLast().?;
            const value = self.result.?;
            try reference.initializeReferencedBinding(self.agent, value);
            self.reference = null;
        },
        .instanceof_operator => {
            const rval = self.stack.pop();
            const lval = self.stack.pop();

            // 5. Return ? InstanceofOperator(lval, rval).
            self.result = Value.from(try lval.instanceofOperator(self.agent, rval));
        },
        .instantiate_arrow_function_expression => {
            const arrow_function = self.fetchFunctionOrClass(executable).arrow_function;
            const closure = try instantiateArrowFunctionExpression(
                self.agent,
                arrow_function,
                null,
            );
            self.result = Value.from(closure);
        },
        .instantiate_async_arrow_function_expression => {
            const async_arrow_function = self.fetchFunctionOrClass(executable).async_arrow_function;
            const closure = try instantiateAsyncArrowFunctionExpression(
                self.agent,
                async_arrow_function,
                null,
            );
            self.result = Value.from(closure);
        },
        .instantiate_async_function_expression => {
            const async_function_expression = self.fetchFunctionOrClass(executable).async_function_expression;
            const closure = try instantiateAsyncFunctionExpression(
                self.agent,
                async_function_expression,
                null,
            );
            self.result = Value.from(closure);
        },
        .instantiate_async_generator_function_expression => {
            const async_generator_expression = self.fetchFunctionOrClass(executable).async_generator_expression;
            const closure = try instantiateAsyncGeneratorFunctionExpression(
                self.agent,
                async_generator_expression,
                null,
            );
            self.result = Value.from(closure);
        },
        .instantiate_generator_function_expression => {
            const generator_expression = self.fetchFunctionOrClass(executable).generator_expression;
            const closure = try instantiateGeneratorFunctionExpression(
                self.agent,
                generator_expression,
                null,
            );
            self.result = Value.from(closure);
        },
        .instantiate_ordinary_function_expression => {
            const function_expression = self.fetchFunctionOrClass(executable).function_expression;
            const closure = try instantiateOrdinaryFunctionExpression(
                self.agent,
                function_expression,
                null,
            );
            self.result = Value.from(closure);
        },
        .is_loosely_equal => {
            const rval = self.stack.pop();
            const lval = self.stack.pop();

            // 5. Return IsLooselyEqual(rval, lval).
            self.result = Value.from(try isLooselyEqual(self.agent, rval, lval));
        },
        .is_strictly_equal => {
            const rval = self.stack.pop();
            const lval = self.stack.pop();

            // 5. Return IsStrictlyEqual(rval, lval).
            self.result = Value.from(isStrictlyEqual(rval, lval));
        },
        .jump => self.ip = self.fetchIndex(executable),
        .jump_conditional => {
            const ip_consequent = self.fetchIndex(executable);
            const ip_alternate = self.fetchIndex(executable);
            const value = self.result.?;
            self.ip = if (value.toBoolean()) ip_consequent else ip_alternate;
        },
        .less_than => {
            const rval = self.stack.pop();
            const lval = self.stack.pop();

            // 5. Let r be ? IsLessThan(lval, rval, true).
            const result = try isLessThan(self.agent, lval, rval, .left_first);

            // 6. If r is undefined, return false. Otherwise, return r.
            self.result = Value.from(result orelse false);
        },
        .less_than_equals => {
            const rval = self.stack.pop();
            const lval = self.stack.pop();

            // 5. Let r be ? IsLessThan(rval, lval, false).
            const result = try isLessThan(self.agent, rval, lval, .right_first);

            // 6. If r is either true or undefined, return false. Otherwise, return true.
            self.result = Value.from(!(result orelse true));
        },
        .load => {
            // Handle null value to allow load of 'empty' result at beginning of script
            if (self.result) |value| try self.stack.append(value);
        },
        .load_constant => {
            const value = self.fetchConstant(executable);
            try self.stack.append(value);
        },
        .load_iterator_next_args => {
            const iterator = self.iterator_stack.getLast();
            try self.stack.append(iterator.next_method);
            try self.stack.append(Value.from(iterator.iterator));
        },
        .load_this_value_for_evaluate_call => {
            const maybe_reference = self.reference_stack.getLast();
            const this_value = evaluateCallGetThisValue(maybe_reference);
            try self.stack.append(this_value);
        },
        .load_this_value_for_make_super_property_reference => {
            // 1. Let env be GetThisEnvironment().
            const env = self.agent.getThisEnvironment();

            // 2. Let actualThis be ? env.GetThisBinding().
            const actual_this = try env.getThisBinding();

            try self.stack.append(actual_this);
        },
        .logical_not => {
            const value = self.result.?;
            self.result = Value.from(!value.toBoolean());
        },
        // 6.2.5.9 MakePrivateReference ( baseValue, privateIdentifier )
        // https://tc39.es/ecma262/#sec-makeprivatereference
        .make_private_reference => {
            const private_identifier = self.fetchIdentifier(executable);
            const base_value = self.stack.pop();

            // 1. Let privEnv be the running execution context's PrivateEnvironment.
            // 2. Assert: privEnv is not null.
            const private_environment = self.agent.runningExecutionContext().ecmascript_code.?.private_environment.?;

            // 3. Let privateName be ResolvePrivateIdentifier(privEnv, privateIdentifier).
            const private_name = private_environment.resolvePrivateIdentifier(private_identifier);

            // 4. Return the Reference Record {
            //      [[Base]]: baseValue, [[ReferencedName]]: privateName, [[Strict]]: true, [[ThisValue]]: empty
            //    }.
            self.reference = .{
                .base = .{ .value = base_value },
                .referenced_name = .{ .private_name = private_name },
                .strict = true,
                .this_value = null,
            };
        },
        // 13.3.7.3 MakeSuperPropertyReference ( actualThis, propertyKey, strict )
        // https://tc39.es/ecma262/#sec-makesuperpropertyreference
        .make_super_property_reference => {
            const property_name_value = self.stack.pop();
            const strict = self.fetchIndex(executable) == 1;
            const actual_this = self.stack.pop();
            const property_key = try property_name_value.toPropertyKey(self.agent);

            // 1. Let env be GetThisEnvironment().
            const env = self.agent.getThisEnvironment();

            // 2. Assert: env.HasSuperBinding() is true.
            std.debug.assert(env.hasSuperBinding());

            // 3. Let baseValue be ? env.GetSuperBase().
            const base_value = try env.getSuperBase();

            // 4. Return the Reference Record {
            //      [[Base]]: baseValue, [[ReferencedName]]: propertyKey, [[Strict]]: strict, [[ThisValue]]: actualThis
            //    }.
            self.reference = .{
                .base = .{ .value = base_value },
                .referenced_name = switch (try property_key.toStringOrSymbol(self.agent)) {
                    .string => |string| .{ .string = string },
                    .symbol => |symbol| .{ .symbol = symbol },
                },
                .strict = strict,
                .this_value = actual_this,
            };
        },
        .object_create => {
            const object = try ordinaryObjectCreate(
                self.agent,
                try self.agent.currentRealm().intrinsics.@"%Object.prototype%"(),
            );
            self.result = Value.from(object);
        },
        .object_define_method => {
            const function_or_class = self.fetchFunctionOrClass(executable);
            const method_type: ast.MethodDefinition.Type = @enumFromInt((self.fetchIndex(executable)));
            const method = switch (method_type) {
                inline else => |@"type"| @unionInit(
                    ast.MethodDefinition.Method,
                    @tagName(@"type"),
                    @field(function_or_class, switch (@"type") {
                        .method, .get, .set => "function_expression",
                        .generator => "generator_expression",
                        .@"async" => "async_function_expression",
                        .async_generator => "async_generator_expression",
                    }),
                ),
            };
            const property_name = self.stack.pop();
            const object = self.stack.pop().object;
            _ = try methodDefinitionEvaluation(
                self.agent,
                .{ .property_name = property_name, .method = method },
                object,
                true,
            );
            self.result = Value.from(object);
        },
        .object_set_property => {
            const property_value = self.stack.pop();
            const property_name = try self.stack.pop().toPropertyKey(self.agent);
            const object = self.stack.pop().object;
            // From PropertyDefinitionEvaluation:
            // 5. Perform ! CreateDataPropertyOrThrow(object, propName, propValue).
            object.createDataPropertyOrThrow(property_name, property_value) catch |err| try noexcept(err);
            self.result = Value.from(object);
        },
        .object_spread_value => {
            const from_value = self.stack.pop();
            var object = self.stack.pop().object;
            const excluded_names: []const PropertyKey = &.{};
            // From PropertyDefinitionEvaluation:
            // 4. Perform ? CopyDataProperties(object, fromValue, excludedNames).
            try object.copyDataProperties(from_value, excluded_names);
            self.result = Value.from(object);
        },
        .pop_exception_jump_target => _ = self.exception_jump_target_stack.pop(),
        .pop_iterator => _ = self.iterator_stack.pop(),
        .pop_lexical_environment => _ = self.lexical_environment_stack.pop(),
        .pop_reference => _ = self.reference_stack.pop(),
        .push_lexical_environment => {
            const lexical_environment = self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment;
            try self.lexical_environment_stack.append(lexical_environment);
        },
        .push_exception_jump_target => {
            const jump_target = self.fetchIndex(executable);
            try self.exception_jump_target_stack.append(jump_target);
        },
        .push_iterator => try self.iterator_stack.append(self.iterator.?),
        .push_reference => try self.reference_stack.append(self.reference),
        .put_value => {
            const lref = self.reference_stack.getLast().?;
            const rval = self.result.?;
            try lref.putValue(self.agent, rval);
            self.reference = null;
        },
        .reg_exp_create => {
            const flags = self.stack.pop();
            const pattern = self.stack.pop();
            self.result = Value.from(try builtins.regExpCreate(self.agent, pattern, flags));
        },
        .resolve_binding => {
            const name = self.fetchIdentifier(executable);
            const strict = self.fetchIndex(executable) == 1;
            const environment_lookup_cache_index = self.fetchIndex(executable);
            const lookup_cache_entry = &self.environment_lookup_cache.items[
                environment_lookup_cache_index
            ];
            self.reference = try self.agent.resolveBinding(name, null, strict, lookup_cache_entry);
        },
        .resolve_private_identifier => {
            // 1. Let privateIdentifier be StringValue of PrivateIdentifier.
            const private_identifier = self.fetchIdentifier(executable);

            // 2. Let privateEnvRec be the running execution context's PrivateEnvironment.
            const private_environment = self.agent.runningExecutionContext().ecmascript_code.?.private_environment.?;

            // 3. Let names be privateEnvRec.[[Names]].
            // 4. Assert: Exactly one element of names is a Private Name whose [[Description]] is privateIdentifier.
            // 5. Let privateName be the Private Name in names whose [[Description]] is privateIdentifier.
            // 6. Return privateName.
            const private_name = private_environment.names.get(private_identifier).?;
            std.debug.assert(private_name.symbol.private);
            self.result = Value.from(private_name.symbol);
        },
        .resolve_this_binding => self.result = try self.agent.resolveThisBinding(),
        .restore_lexical_environment => {
            const lexical_environment = self.lexical_environment_stack.getLast();
            self.agent.runningExecutionContext().ecmascript_code.?.lexical_environment = lexical_environment;
        },
        .rethrow_exception_if_any => if (self.exception) |value| {
            self.agent.exception = value;
            return error.ExceptionThrown;
        },
        .@"return" => {}, // Handled in run()
        .store => {
            // Handle empty stack to allow restoring a null `.load`
            self.result = self.stack.popOrNull();
        },
        .store_constant => {
            const value = self.fetchConstant(executable);
            self.result = value;
        },
        .throw => {
            const value = self.result.?;
            self.agent.exception = value;
            return error.ExceptionThrown;
        },
        .to_number => {
            const value = self.result.?;
            self.result = Value.from(try value.toNumber(self.agent));
        },
        .to_numeric => {
            const value = self.result.?;
            const numeric = try value.toNumeric(self.agent);
            self.result = switch (numeric) {
                .number => |number| Value.from(number),
                .big_int => |big_int| Value.from(big_int),
            };
        },
        .to_object => {
            const value = self.result.?;
            self.result = Value.from(try value.toObject(self.agent));
        },
        .to_string => {
            const value = self.result.?;
            self.result = Value.from(try value.toString(self.agent));
        },
        .typeof => {
            // 1. Let val be ? Evaluation of UnaryExpression.
            // NOTE: This is part of the generated bytecode.

            // 2. If val is a Reference Record, then
            if (self.reference) |reference| {
                // a. If IsUnresolvableReference(val) is true, return "undefined".
                if (reference.isUnresolvableReference()) {
                    self.result = Value.from("undefined");
                    return;
                }
            }

            // 3. Set val to ? GetValue(val).
            const value = if (self.reference) |reference|
                try reference.getValue(self.agent)
            else
                self.result.?;

            self.result = switch (value) {
                // 4. If val is undefined, return "undefined".
                .undefined => Value.from("undefined"),

                // 5. If val is null, return "object".
                .null => Value.from("object"),

                // 6. If val is a String, return "string".
                .string => Value.from("string"),

                // 7. If val is a Symbol, return "symbol".
                .symbol => Value.from("symbol"),

                // 8. If val is a Boolean, return "boolean".
                .boolean => Value.from("boolean"),

                // 9. If val is a Number, return "number".
                .number => Value.from("number"),

                // 10. If val is a BigInt, return "bigint".
                .big_int => Value.from("bigint"),

                // 11. Assert: val is an Object.
                .object => |object| blk: {
                    // B.3.6.3 Changes to the typeof Operator
                    // https://tc39.es/ecma262/#sec-IsHTMLDDA-internal-slot-typeof
                    if (build_options.enable_annex_b) {
                        // 12. If val has an [[IsHTMLDDA]] internal slot, return "undefined".
                        if (object.isHTMLDDA()) break :blk Value.from("undefined");
                    } else {
                        // 12. NOTE: This step is replaced in section B.3.6.3.
                    }

                    // 13. If val has a [[Call]] internal slot, return "function".
                    if (object.internalMethods().call) |_| break :blk Value.from("function");

                    // 14. Return "object".
                    break :blk Value.from("object");
                },
            };
        },
        .unary_minus => {
            const value = self.result.?;
            self.result = switch (value) {
                .number => |number| Value.from(number.unaryMinus()),
                .big_int => |big_int| Value.from(try big_int.unaryMinus()),
                else => unreachable,
            };
        },
        _ => unreachable,
    }
}

pub fn run(self: *Self, executable: Executable) Agent.Error!Completion {
    try self.environment_lookup_cache.resize(executable.environment_lookup_cache_size);
    @memset(self.environment_lookup_cache.items, null);
    while (@call(
        .always_inline,
        Self.fetchInstruction,
        .{ self, executable },
    )) |instruction| {
        @call(
            .always_inline,
            Self.executeInstruction,
            .{ self, executable, instruction },
        ) catch |err| {
            if (self.exception_jump_target_stack.items.len != 0) {
                self.exception = self.agent.clearException();
                self.ip = self.exception_jump_target_stack.getLast();
            } else return err;
        };
        if (instruction == .@"return") {
            return .{ .type = .@"return", .value = self.result, .target = null };
        }
    }
    return Completion.normal(self.result);
}
