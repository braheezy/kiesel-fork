//! 20.2 Function Objects
//! https://tc39.es/ecma262/#sec-function-objects

const std = @import("std");

const ast = @import("../language/ast.zig");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const BuiltinFunction = builtins.BuiltinFunction;
const ClassConstructorFields = builtins.builtin_function.ClassConstructorFields;
const Diagnostics = language.Diagnostics;
const ECMAScriptFunction = builtins.ECMAScriptFunction;
const Object = types.Object;
const Parser = @import("../language/Parser.zig");
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const boundFunctionCreate = builtins.boundFunctionCreate;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinFunctionWithAttributes = utils.defineBuiltinFunctionWithAttributes;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const formatParseError = utils.formatParseError;
const makeConstructor = builtins.makeConstructor;
const noexcept = utils.noexcept;
const ordinaryFunctionCreate = builtins.ordinaryFunctionCreate;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const setFunctionLength = builtins.setFunctionLength;
const setFunctionName = builtins.setFunctionName;

fn GrammarSymbol(comptime T: type) type {
    return struct {
        type: type = T,
        acceptFn: fn (*Parser) Parser.AcceptError!T = undefined,
    };
}

/// 20.2.2 Properties of the Function Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-function-constructor
pub const constructor = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return createBuiltinFunction(realm.agent, .{ .constructor = impl }, .{
            .length = 1,
            .name = "Function",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
        // 20.2.2.1 Function.prototype
        // https://tc39.es/ecma262/#sec-function.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(realm.intrinsics.@"%Function.prototype%"() catch unreachable),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });
    }

    /// 20.2.1.1 Function ( ...parameterArgs, bodyArg )
    /// https://tc39.es/ecma262/#sec-function-p1-p2-pn-body
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?Object) Agent.Error!Value {
        const parameter_args = arguments.values[0..arguments.count() -| 1];
        const maybe_body_arg = arguments.getOrNull(arguments.count() -| 1);

        // 1. Let C be the active function object.
        const constructor_ = agent.activeFunctionObject();

        // 2. If bodyArg is not present, set bodyArg to the empty String.
        const body_arg = maybe_body_arg orelse Value.from("");

        // 3. Return ? CreateDynamicFunction(C, NewTarget, normal, parameterArgs, bodyArg).
        return Value.from(try createDynamicFunction(
            agent,
            constructor_,
            new_target,
            .normal,
            parameter_args,
            body_arg,
        ));
    }
};

/// 20.2.1.1.1 CreateDynamicFunction ( constructor, newTarget, kind, parameterArgs, bodyArg )
/// https://tc39.es/ecma262/#sec-createdynamicfunction
pub fn createDynamicFunction(
    agent: *Agent,
    constructor_: Object,
    maybe_new_target: ?Object,
    comptime kind: enum {
        normal,
        generator,
        @"async",
        async_generator,
    },
    parameter_args: []const Value,
    body_arg: Value,
) Agent.Error!Object {
    const realm = agent.currentRealm();

    // 1. If newTarget is undefined, set newTarget to constructor.
    const new_target = maybe_new_target orelse constructor_;

    comptime var prefix: []const u8 = undefined;
    comptime var fallback_prototype: []const u8 = undefined;
    comptime var expr_sym: GrammarSymbol(switch (kind) {
        .normal => ast.FunctionExpression,
        .generator => ast.GeneratorExpression,
        .@"async" => ast.AsyncFunctionExpression,
        .async_generator => ast.AsyncGeneratorExpression,
    }) = .{};
    comptime var body_sym: GrammarSymbol(ast.FunctionBody) = .{};
    comptime var parameter_sym: GrammarSymbol(ast.FormalParameters) = .{};

    switch (kind) {
        // 2. If kind is normal, then
        .normal => {
            // a. Let prefix be "function".
            prefix = "function";

            // b. Let exprSym be the grammar symbol FunctionExpression.
            expr_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FunctionExpression {
                    return parser.acceptFunctionExpression();
                }
            }.accept;

            // c. Let bodySym be the grammar symbol FunctionBody[~Yield, ~Await].
            body_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FunctionBody {
                    return parser.acceptFunctionBody(.normal);
                }
            }.accept;

            // d. Let parameterSym be the grammar symbol FormalParameters[~Yield, ~Await].
            parameter_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FormalParameters {
                    return parser.acceptFormalParameters();
                }
            }.accept;

            // e. Let fallbackProto be "%Function.prototype%".
            fallback_prototype = "%Function.prototype%";
        },

        // 3. Else if kind is generator, then
        .generator => {
            // a. Let prefix be "function*".
            prefix = "function*";

            // b. Let exprSym be the grammar symbol GeneratorExpression.
            expr_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.GeneratorExpression {
                    return parser.acceptGeneratorExpression();
                }
            }.accept;

            // c. Let bodySym be the grammar symbol GeneratorBody.
            body_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FunctionBody {
                    return parser.acceptFunctionBody(.generator);
                }
            }.accept;

            // d. Let parameterSym be the grammar symbol FormalParameters[+Yield, ~Await].
            parameter_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FormalParameters {
                    return parser.acceptFormalParameters();
                }
            }.accept;

            // e. Let fallbackProto be "%GeneratorFunction.prototype%".
            fallback_prototype = "%GeneratorFunction.prototype%";
        },

        // 4. Else if kind is async, then
        .@"async" => {
            // a. Let prefix be "async function".
            prefix = "async function";

            // b. Let exprSym be the grammar symbol AsyncFunctionExpression.
            expr_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.AsyncFunctionExpression {
                    return parser.acceptAsyncFunctionExpression();
                }
            }.accept;

            // c. Let bodySym be the grammar symbol AsyncFunctionBody.
            body_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FunctionBody {
                    return parser.acceptFunctionBody(.@"async");
                }
            }.accept;

            // d. Let parameterSym be the grammar symbol FormalParameters[~Yield, +Await].
            parameter_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FormalParameters {
                    return parser.acceptFormalParameters();
                }
            }.accept;

            // e. Let fallbackProto be "%AsyncFunction.prototype%".
            fallback_prototype = "%AsyncFunction.prototype%";
        },

        // 5. Else,
        .async_generator => {
            // a. Assert: kind is async-generator.

            // b. Let prefix be "async function*".
            prefix = "async function*";

            // c. Let exprSym be the grammar symbol AsyncGeneratorExpression.
            expr_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.AsyncGeneratorExpression {
                    return parser.acceptAsyncGeneratorExpression();
                }
            }.accept;

            // d. Let bodySym be the grammar symbol AsyncGeneratorBody.
            body_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FunctionBody {
                    return parser.acceptFunctionBody(.async_generator);
                }
            }.accept;

            // e. Let parameterSym be the grammar symbol FormalParameters[+Yield, +Await].
            parameter_sym.acceptFn = struct {
                fn accept(parser: *Parser) Parser.AcceptError!ast.FormalParameters {
                    return parser.acceptFormalParameters();
                }
            }.accept;

            // f. Let fallbackProto be "%AsyncGeneratorFunction.prototype%".
            fallback_prototype = "%AsyncGeneratorFunction.prototype%";
        },
    }

    // 6. Let argCount be the number of elements in parameterArgs.
    const arg_count = parameter_args.len;

    // 7. Let parameterStrings be a new empty List.
    var parameter_strings = try std.ArrayList(String).initCapacity(
        agent.gc_allocator,
        parameter_args.len,
    );
    defer parameter_strings.deinit();

    // 8. For each element arg of parameterArgs, do
    for (parameter_args) |arg| {
        // a. Append ? ToString(arg) to parameterStrings.
        parameter_strings.appendAssumeCapacity(try arg.toString(agent));
    }

    // 9. Let bodyString be ? ToString(bodyArg).
    const body_string = try body_arg.toString(agent);

    // 10. Let currentRealm be the current Realm Record.
    const current_realm = realm;

    // 11. Perform ? HostEnsureCanCompileStrings(currentRealm, parameterStrings, bodyString, false).
    try agent.host_hooks.hostEnsureCanCompileStrings(current_realm, parameter_strings.items, body_string, false);

    // 12. Let P be the empty String.
    var result = String.Builder.init(agent.gc_allocator);
    defer result.deinit();

    // 13. If argCount > 0, then
    if (arg_count > 0) {
        // a. Set P to parameterStrings[0].
        // b. Let k be 1.
        // c. Repeat, while k < argCount,
        //     i. Let nextArgString be parameterStrings[k].
        //     ii. Set P to the string-concatenation of P, "," (a comma), and nextArgString.
        //     iii. Set k to k + 1.
        for (parameter_strings.items, 0..) |next_arg_string, k| {
            if (k > 0) try result.appendChar(',');
            try result.appendString(next_arg_string);
        }
    }

    const parameters_string = try (try result.build()).toUtf8(agent.gc_allocator);

    // 14. Let bodyParseString be the string-concatenation of 0x000A (LINE FEED), bodyString, and
    //     0x000A (LINE FEED).
    const body_parse_string = try std.fmt.allocPrint(
        agent.gc_allocator,
        "\n{}\n",
        .{body_string},
    );

    // 15. Let sourceString be the string-concatenation of prefix, " anonymous(", P, 0x000A
    //     (LINE FEED), ") {", bodyParseString, and "}".
    const source_string = try std.fmt.allocPrint(
        agent.gc_allocator,
        "{[prefix]s} anonymous({[parameters_string]s}\n) {{{[body_parse_string]s}}}",
        .{ .prefix = prefix, .parameters_string = parameters_string, .body_parse_string = body_parse_string },
    );

    // 16. Let sourceText be StringToCodePoints(sourceString).
    const source_text = source_string;

    var diagnostics = Diagnostics.init(agent.gc_allocator);
    defer diagnostics.deinit();

    // 17. Let parameters be ParseText(P, parameterSym).
    const parameters = Parser.parseNode(parameter_sym.type, parameter_sym.acceptFn, agent.gc_allocator, parameters_string, .{
        .diagnostics = &diagnostics,
        .file_name = "Function",
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // 18. If parameters is a List of errors, throw a SyntaxError exception.
            const parse_error = diagnostics.errors.items[0];
            return agent.throwException(
                .syntax_error,
                "{s}",
                .{try formatParseError(agent.gc_allocator, parse_error)},
            );
        },
    };

    // 19. Let body be ParseText(bodyParseString, bodySym).
    const body = Parser.parseNode(body_sym.type, body_sym.acceptFn, agent.gc_allocator, body_parse_string, .{
        .diagnostics = &diagnostics,
        .file_name = "Function",
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // 20. If body is a List of errors, throw a SyntaxError exception.
            const parse_error = diagnostics.errors.items[0];
            return agent.throwException(
                .syntax_error,
                "{s}",
                .{try formatParseError(agent.gc_allocator, parse_error)},
            );
        },
    };

    // 21. NOTE: The parameters and body are parsed separately to ensure that each is valid alone.
    //           For example, new Function("/*", "*/ ) {") does not evaluate to a function.

    // 22. NOTE: If this step is reached, sourceText must have the syntax of exprSym (although the
    //           reverse implication does not hold). The purpose of the next two steps is to
    //           enforce any Early Error rules which apply to exprSym directly.

    // 23. Let expr be ParseText(sourceText, exprSym).
    _ = Parser.parseNode(expr_sym.type, expr_sym.acceptFn, agent.gc_allocator, source_text, .{
        .diagnostics = &diagnostics,
        .file_name = "Function",
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // 24. If expr is a List of errors, throw a SyntaxError exception.
            const parse_error = diagnostics.errors.items[0];
            return agent.throwException(
                .syntax_error,
                "{s}",
                .{try formatParseError(agent.gc_allocator, parse_error)},
            );
        },
    };

    // 25. Let proto be ? GetPrototypeFromConstructor(newTarget, fallbackProto).
    const proto = try getPrototypeFromConstructor(new_target, fallback_prototype);

    // 26. Let env be currentRealm.[[GlobalEnv]].
    const env = current_realm.global_env;

    // 27. Let privateEnv be null.
    const private_env = null;

    // 28. Let F be OrdinaryFunctionCreate(proto, sourceText, parameters, body, non-lexical-this, env, privateEnv).
    const function = try ordinaryFunctionCreate(
        agent,
        proto,
        source_text,
        parameters,
        body,
        .non_lexical_this,
        .{ .global_environment = env },
        private_env,
    );

    // 29. Perform SetFunctionName(F, "anonymous").
    try setFunctionName(function, PropertyKey.from("anonymous"), null);

    switch (kind) {
        // 30. If kind is generator, then
        .generator => {
            // a. Let prototype be OrdinaryObjectCreate(%GeneratorPrototype%).
            const prototype_ = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%GeneratorPrototype%"(),
            );

            // b. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            function.definePropertyOrThrow(PropertyKey.from("prototype"), .{
                .value = Value.from(prototype_),
                .writable = true,
                .enumerable = false,
                .configurable = false,
            }) catch |err| try noexcept(err);
        },

        // 31. Else if kind is async-generator, then
        .async_generator => {
            // a. Let prototype be OrdinaryObjectCreate(%AsyncGeneratorPrototype%).
            const prototype_ = try ordinaryObjectCreate(
                agent,
                try realm.intrinsics.@"%AsyncGeneratorPrototype%"(),
            );

            // b. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor {
            //      [[Value]]: prototype, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
            //    }).
            function.definePropertyOrThrow(PropertyKey.from("prototype"), .{
                .value = Value.from(prototype_),
                .writable = true,
                .enumerable = false,
                .configurable = false,
            }) catch |err| try noexcept(err);
        },

        // 32. Else if kind is normal, then
        .normal => {
            // a. Perform MakeConstructor(F).
            try makeConstructor(function, .{});
        },

        // 33. NOTE: Functions whose kind is async are not constructable and do not have a
        //           [[Construct]] internal method or a "prototype" property.
        .@"async" => {},
    }

    // 34. Return F.
    return function;
}

/// 20.2.3 Properties of the Function Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-function-prototype-object
pub const prototype = struct {
    pub fn create(realm: *Realm) std.mem.Allocator.Error!Object {
        return createBuiltinFunction(realm.agent, .{ .function = function }, .{
            .length = 0,
            .name = "",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) std.mem.Allocator.Error!void {
        try defineBuiltinFunction(object, "apply", apply, 2, realm);
        try defineBuiltinFunction(object, "bind", bind, 1, realm);
        try defineBuiltinFunction(object, "call", call, 1, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinFunctionWithAttributes(object, "%Symbol.hasInstance%", @"%Symbol.hasInstance%", 1, realm, .{
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.2.3.4 Function.prototype.constructor
        // https://tc39.es/ecma262/#sec-function.prototype.constructor
        try defineBuiltinProperty(
            object,
            "constructor",
            Value.from(try realm.intrinsics.@"%Function%"()),
        );
    }

    fn function(_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        return .undefined;
    }

    /// 20.2.3.1 Function.prototype.apply ( thisArg, argArray )
    /// https://tc39.es/ecma262/#sec-function.prototype.apply
    fn apply(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const this_arg = arguments.get(0);
        const arg_array = arguments.get(1);

        // 1. Let func be the this value.
        const func = this_value;

        // 2. If IsCallable(func) is false, throw a TypeError exception.
        if (!func.isCallable()) {
            return agent.throwException(.type_error, "{} is not a function", .{func});
        }

        // 3. If argArray is either undefined or null, then
        if (arg_array.isUndefined() or arg_array.isNull()) {
            // TODO: a. Perform PrepareForTailCall().

            // b. Return ? Call(func, thisArg).
            return func.callAssumeCallableNoArgs(this_arg);
        }

        // 4. Let argList be ? CreateListFromArrayLike(argArray).
        const arg_list = try arg_array.createListFromArrayLike(agent, null);

        // TODO: 5. Perform PrepareForTailCall().

        // 6. Return ? Call(func, thisArg, argList).
        return func.callAssumeCallable(this_arg, arg_list);
    }

    /// 20.2.3.2 Function.prototype.bind ( thisArg, ...args )
    /// https://tc39.es/ecma262/#sec-function.prototype.bind
    fn bind(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const this_arg = arguments.get(0);
        const args = if (arguments.count() <= 1) &[_]Value{} else arguments.values[1..];

        // 1. Let Target be the this value.
        const target = this_value;

        // 2. If IsCallable(Target) is false, throw a TypeError exception.
        if (!target.isCallable()) {
            return agent.throwException(.type_error, "{} is not a function", .{target});
        }

        // 3. Let F be ? BoundFunctionCreate(Target, thisArg, args).
        const function_ = try boundFunctionCreate(agent, target.asObject(), this_arg, args);

        // 4. Let L be 0.
        var length: f64 = 0;

        // 5. Let targetHasLength be ? HasOwnProperty(Target, "length").
        const target_has_length = try target.asObject().hasOwnProperty(PropertyKey.from("length"));

        // 6. If targetHasLength is true, then
        if (target_has_length) {
            // a. Let targetLen be ? Get(Target, "length").
            const target_length = try target.asObject().get(PropertyKey.from("length"));

            // b. If targetLen is a Number, then
            if (target_length.isNumber()) {
                // i. If targetLen is +∞𝔽, then
                if (target_length.asNumber().isPositiveInf()) {
                    // 1. Set L to +∞.
                    length = std.math.inf(f64);
                }
                // ii. Else if targetLen is -∞𝔽, then
                else if (target_length.asNumber().isNegativeInf()) {
                    // 1. Set L to 0.
                    length = 0;
                }
                // iii. Else,
                else {
                    // 1. Let targetLenAsInt be ! ToIntegerOrInfinity(targetLen).
                    const target_length_as_int = target_length.toIntegerOrInfinity(agent) catch unreachable;

                    // 2. Assert: targetLenAsInt is finite.
                    std.debug.assert(std.math.isFinite(target_length_as_int));

                    // 3. Let argCount be the number of elements in args.
                    const arg_count = args.len;

                    // 4. Set L to max(targetLenAsInt - argCount, 0).
                    length = @max(target_length_as_int - @as(f64, @floatFromInt(arg_count)), 0);
                }
            }
        }

        // 7. Perform SetFunctionLength(F, L).
        try setFunctionLength(function_, length);

        // 8. Let targetName be ? Get(Target, "name").
        var target_name = try target.asObject().get(PropertyKey.from("name"));

        // 9. If targetName is not a String, set targetName to the empty String.
        if (!target_name.isString()) target_name = Value.from("");

        // 10. Perform SetFunctionName(F, targetName, "bound").
        try setFunctionName(function_, PropertyKey.from(target_name.asString()), "bound");

        // 11. Return F.
        return Value.from(function_);
    }

    /// 20.2.3.3 Function.prototype.call ( thisArg, ...args )
    /// https://tc39.es/ecma262/#sec-function.prototype.call
    fn call(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const this_arg = arguments.get(0);
        const args = if (arguments.count() <= 1) &[_]Value{} else arguments.values[1..];

        // 1. Let func be the this value.
        const func = this_value;

        // 2. If IsCallable(func) is false, throw a TypeError exception.
        if (!func.isCallable()) {
            return agent.throwException(.type_error, "{} is not a function", .{func});
        }

        // TODO: 3. Perform PrepareForTailCall().

        // 4. Return ? Call(func, thisArg, args).
        return func.callAssumeCallable(this_arg, args);
    }

    /// 20.2.3.5 Function.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-function.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let func be the this value.
        const func = this_value;

        // 2. If func is an Object, func has a [[SourceText]] internal slot, func.[[SourceText]] is
        //    a sequence of Unicode code points, and HostHasSourceTextAvailable(func) is true, then
        //     a. Return CodePointsToString(func.[[SourceText]]).
        if (func.isObject()) {
            if (func.asObject().is(ECMAScriptFunction)) {
                const ecmascript_function = func.asObject().as(ECMAScriptFunction);
                return Value.from(try String.fromUtf8(agent.gc_allocator, ecmascript_function.fields.source_text));
            } else if (func.asObject().is(BuiltinFunction)) {
                const builtin_function = func.asObject().as(BuiltinFunction);
                if (builtin_function.fields.additional_fields.tryCast(*ClassConstructorFields)) |class_constructor_fields| {
                    return Value.from(try String.fromUtf8(agent.gc_allocator, class_constructor_fields.source_text));
                }
            }
        }

        // 3. If func is a built-in function object, return an implementation-defined String source
        //    code representation of func. The representation must have the syntax of a
        //    NativeFunction. Additionally, if func has an [[InitialName]] internal slot and
        //    func.[[InitialName]] is a String, the portion of the returned String that would be
        //    matched by NativeFunctionAccessoropt PropertyName must be the value of
        //    func.[[InitialName]].
        if (func.isObject() and func.asObject().is(BuiltinFunction)) {
            const builtin_function = func.asObject().as(BuiltinFunction);
            const name: String = builtin_function.fields.initial_name orelse .empty;
            const source_text = try std.fmt.allocPrint(
                agent.gc_allocator,
                "function {}() {{ [native code] }}",
                .{name},
            );
            return Value.from(try String.fromAscii(agent.gc_allocator, source_text));
        }

        // 4. If func is an Object and IsCallable(func) is true, return an implementation-defined
        //    String source code representation of func. The representation must have the syntax of
        //    a NativeFunction.
        if (func.isCallable()) return Value.from("function () { [native code] }");

        // 5. Throw a TypeError exception.
        return agent.throwException(.type_error, "{} is not a function", .{func});
    }

    /// 20.2.3.6 Function.prototype [ %Symbol.hasInstance% ] ( V )
    /// https://tc39.es/ecma262/#sec-function.prototype-%symbol.hasinstance%
    fn @"%Symbol.hasInstance%"(_: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const value = arguments.get(0);

        // 1. Let F be the this value.
        const func = this_value;

        // 2. Return ? OrdinaryHasInstance(F, V).
        return Value.from(try func.ordinaryHasInstance(value));
    }
};
