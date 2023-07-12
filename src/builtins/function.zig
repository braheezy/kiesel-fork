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
const ArgumentsList = builtins.ArgumentsList;
const BuiltinFunction = builtins.BuiltinFunction;
const Diagnostics = language.Diagnostics;
const ECMAScriptFunction = builtins.ECMAScriptFunction;
const Object = types.Object;
const Parser = @import("../language/Parser.zig");
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const getPrototypeFromConstructor = builtins.getPrototypeFromConstructor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const makeConstructor = builtins.makeConstructor;
const ordinaryFunctionCreate = builtins.ordinaryFunctionCreate;
const setFunctionName = builtins.setFunctionName;

/// 20.2.2 Properties of the Function Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-function-constructor
pub const FunctionConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            // 20.2.2.1 Function.length
            // https://tc39.es/ecma262/#sec-function.length
            .length = 1,
            .name = "Function",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 20.2.2.2 Function.prototype
        // https://tc39.es/ecma262/#sec-boolean.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(realm.intrinsics.@"%Function.prototype%"() catch unreachable),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 20.2.3.4 Function.prototype.constructor
        // https://tc39.es/ecma262/#sec-function.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%Function.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 20.2.1.1 Function ( ...parameterArgs, bodyArg )
    /// https://tc39.es/ecma262/#sec-function-p1-p2-pn-body
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const parameter_args = ArgumentsList.from(arguments.values[0..arguments.count() -| 1]);
        const maybe_body_arg = arguments.getOrNull(arguments.count() -| 1);

        // 1. Let C be the active function object.
        const constructor = agent.activeFunctionObject();

        // 2. If bodyArg is not present, set bodyArg to the empty String.
        const body_arg = maybe_body_arg orelse Value.from("");

        // 3. Return ? CreateDynamicFunction(C, NewTarget, normal, parameterArgs, bodyArg).
        return Value.from(try createDynamicFunction(
            agent,
            constructor,
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
    constructor: Object,
    maybe_new_target: ?Object,
    comptime kind: enum {
        normal,
        generator,
        @"async",
        async_generator,
    },
    parameter_args: ArgumentsList,
    body_arg: Value,
) !Object {
    const realm = agent.currentRealm();

    // 1. Let currentRealm be the current Realm Record.
    const current_realm = realm;

    // 2. Perform ? HostEnsureCanCompileStrings(currentRealm).
    try agent.host_hooks.hostEnsureCanCompileStrings(current_realm);

    // 3. If newTarget is undefined, set newTarget to constructor.
    const new_target = maybe_new_target orelse constructor;

    const GrammarSymbol = struct { type: type, function_name: []const u8 };

    comptime var prefix: []const u8 = undefined;
    comptime var fallback_prototype: []const u8 = undefined;
    comptime var expr_sym: GrammarSymbol = undefined;
    comptime var body_sym: GrammarSymbol = undefined;
    comptime var parameter_sym: GrammarSymbol = undefined;

    switch (kind) {
        // 4. If kind is normal, then
        .normal => {
            // a. Let prefix be "function".
            prefix = "function";

            // b. Let exprSym be the grammar symbol FunctionExpression.
            expr_sym = .{
                .type = ast.FunctionExpression,
                .function_name = "acceptFunctionExpression",
            };

            // c. Let bodySym be the grammar symbol FunctionBody[~Yield, ~Await].
            body_sym = .{
                .type = ast.FunctionBody,
                .function_name = "acceptFunctionBody",
            };

            // d. Let parameterSym be the grammar symbol FormalParameters[~Yield, ~Await].
            parameter_sym = .{
                .type = ast.FormalParameters,
                .function_name = "acceptFormalParameters",
            };

            // e. Let fallbackProto be "%Function.prototype%".
            fallback_prototype = "%Function.prototype%";
        },

        // TODO: 5. Else if kind is generator, then
        .generator => @compileError("Not implemented"),

        // TODO: 6. Else if kind is async, then
        .@"async" => @compileError("Not implemented"),

        // 7. Else,
        // TODO: a. Assert: kind is asyncGenerator.
        .async_generator => @compileError("Not implemented"),
    }

    // 8. Let argCount be the number of elements in parameterArgs.
    const arg_count = parameter_args.count();

    // 9. Let P be the empty String.
    var parameters_string: []const u8 = "";

    // 10. If argCount > 0, then
    if (arg_count > 0) {
        // a. Let firstArg be parameterArgs[0].
        // b. Set P to ? ToString(firstArg).
        // c. Let k be 1.
        // d. Repeat, while k < argCount,
        //     i. Let nextArg be parameterArgs[k].
        //     ii. Let nextArgString be ? ToString(nextArg).
        //     iii. Set P to the string-concatenation of P, "," (a comma), and nextArgString.
        //     iv. Set k to k + 1.
        var argument_strings = try std.ArrayList([]const u8).initCapacity(
            agent.gc_allocator,
            arg_count,
        );
        defer argument_strings.deinit();
        for (parameter_args.values) |arg| {
            argument_strings.appendAssumeCapacity(try arg.toString(agent));
        }
        parameters_string = try std.mem.join(agent.gc_allocator, ",", argument_strings.items);
    }

    // 11. Let bodyString be the string-concatenation of 0x000A (LINE FEED), ? ToString(bodyArg),
    //     and 0x000A (LINE FEED).
    const body_string = try std.fmt.allocPrint(
        agent.gc_allocator,
        "\n{s}\n",
        .{try body_arg.toString(agent)},
    );

    // 12. Let sourceString be the string-concatenation of prefix, " anonymous(", P, 0x000A
    //     (LINE FEED), ") {", bodyString, and "}".
    const source_string = try std.fmt.allocPrint(
        agent.gc_allocator,
        "{[prefix]s} anonymous({[parameters_string]s}\n) {{{[body_string]s}}}",
        .{ .prefix = prefix, .parameters_string = parameters_string, .body_string = body_string },
    );

    // 13. Let sourceText be StringToCodePoints(sourceString).
    const source_text = source_string;

    var diagnostics = Diagnostics.init(agent.gc_allocator);
    defer diagnostics.deinit();

    // 14. Let parameters be ParseText(StringToCodePoints(P), parameterSym).
    const parameters = Parser.parseNode(parameter_sym.type, parameter_sym.function_name, agent.gc_allocator, parameters_string, .{
        .diagnostics = &diagnostics,
        .file_name = "Function",
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // 15. If parameters is a List of errors, throw a SyntaxError exception.
            const parse_error = diagnostics.errors.items[0];
            return agent.throwException(
                .syntax_error,
                try agent.gc_allocator.dupe(u8, parse_error.message),
            );
        },
    };

    // 16. Let body be ParseText(StringToCodePoints(bodyString), bodySym).
    const body = Parser.parseNode(body_sym.type, body_sym.function_name, agent.gc_allocator, body_string, .{
        .diagnostics = &diagnostics,
        .file_name = "Function",
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // 17. If body is a List of errors, throw a SyntaxError exception.
            const parse_error = diagnostics.errors.items[0];
            return agent.throwException(
                .syntax_error,
                try agent.gc_allocator.dupe(u8, parse_error.message),
            );
        },
    };

    // 18. NOTE: The parameters and body are parsed separately to ensure that each is valid alone.
    //           For example, new Function("/*", "*/ ) {") does not evaluate to a function.

    // 19. NOTE: If this step is reached, sourceText must have the syntax of exprSym (although the
    //           reverse implication does not hold). The purpose of the next two steps is to
    //           enforce any Early Error rules which apply to exprSym directly.

    // 20. Let expr be ParseText(sourceText, exprSym).
    _ = Parser.parseNode(expr_sym.type, expr_sym.function_name, agent.gc_allocator, source_text, .{
        .diagnostics = &diagnostics,
        .file_name = "Function",
    }) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {
            // 21. If expr is a List of errors, throw a SyntaxError exception.
            const parse_error = diagnostics.errors.items[0];
            return agent.throwException(
                .syntax_error,
                try agent.gc_allocator.dupe(u8, parse_error.message),
            );
        },
    };

    // 22. Let proto be ? GetPrototypeFromConstructor(newTarget, fallbackProto).
    const prototype = try getPrototypeFromConstructor(new_target, fallback_prototype);

    // 23. Let realmF be the current Realm Record.
    // 24. Let env be realmF.[[GlobalEnv]].
    const env = realm.global_env;

    // 25. Let privateEnv be null.
    const private_env = null;

    // 26. Let F be OrdinaryFunctionCreate(proto, sourceText, parameters, body, non-lexical-this, env, privateEnv).
    const function = try ordinaryFunctionCreate(
        agent,
        prototype,
        source_text,
        parameters,
        body,
        .non_lexical_this,
        .{ .global_environment = env },
        private_env,
    );

    // 27. Perform SetFunctionName(F, "anonymous").
    try setFunctionName(function, PropertyKey.from("anonymous"), null);

    switch (kind) {
        // TODO: 28. If kind is generator, then
        .generator => @compileError("Not implemented"),

        // TODO: 29. Else if kind is asyncGenerator, then
        .async_generator => @compileError("Not implemented"),

        // 30. Else if kind is normal, then
        .normal => {
            // a. Perform MakeConstructor(F).
            try makeConstructor(function, .{});
        },

        // 31. NOTE: Functions whose kind is async are not constructible and do not have a
        //           [[Construct]] internal method or a "prototype" property.
        .@"async" => {},
    }

    // 32. Return F.
    return function;
}

/// 20.2.3 Properties of the Function Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-function-prototype-object
pub const FunctionPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createNoinit(realm);
        init(realm, object);
        return object;
    }

    pub fn createNoinit(realm: *Realm) !Object {
        return createBuiltinFunction(realm.agent, .{ .regular = behaviour }, .{
            .length = 0,
            .name = "",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });
    }

    pub fn init(realm: *Realm, object: Object) !void {
        try defineBuiltinFunction(object, "apply", apply, 2, realm);
        try defineBuiltinFunction(object, "call", call, 1, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
    }

    fn behaviour(_: *Agent, _: Value, _: ArgumentsList) !Value {
        return .undefined;
    }

    /// 20.2.3.1 Function.prototype.apply ( thisArg, argArray )
    /// https://tc39.es/ecma262/#sec-function.prototype.apply
    fn apply(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const this_arg = arguments.get(0);
        const arg_array = arguments.get(1);

        // 1. Let func be the this value.
        const func = this_value;

        // 2. If IsCallable(func) is false, throw a TypeError exception.
        if (!func.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not a function", .{func}),
            );
        }

        // 3. If argArray is either undefined or null, then
        if (arg_array == .undefined or arg_array == .null) {
            // TODO: a. Perform PrepareForTailCall().

            // b. Return ? Call(func, thisArg).
            return func.callAssumeCallableNoArgs(this_arg);
        }

        // 4. Let argList be ? CreateListFromArrayLike(argArray).
        const arg_list = try arg_array.createListFromArrayLike(agent, .{});

        // TODO: 5. Perform PrepareForTailCall().

        // 6. Return ? Call(func, thisArg, argList).
        return func.callAssumeCallable(this_arg, arg_list);
    }

    /// 20.2.3.3 Function.prototype.call ( thisArg, ...args )
    /// https://tc39.es/ecma262/#sec-function.prototype.call
    fn call(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const this_arg = arguments.get(0);
        const args = if (arguments.count() <= 1) &[_]Value{} else arguments.values[1..];

        // 1. Let func be the this value.
        const func = this_value;

        // 2. If IsCallable(func) is false, throw a TypeError exception.
        if (!func.isCallable()) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not a function", .{func}),
            );
        }

        // TODO: 3. Perform PrepareForTailCall().

        // 4. Return ? Call(func, thisArg, args).
        return func.callAssumeCallable(this_arg, args);
    }

    /// 20.2.3.5 Function.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-function.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let func be the this value.
        const func = this_value;

        // // 2. If func is an Object, func has a [[SourceText]] internal slot, func.[[SourceText]] is
        // //    a sequence of Unicode code points, and HostHasSourceTextAvailable(func) is true, then
        if (func == .object and func.object.is(ECMAScriptFunction)) {
            const ecmascript_function = func.object.as(ECMAScriptFunction);

            // a. Return CodePointsToString(func.[[SourceText]]).
            return Value.from(ecmascript_function.fields.source_text);
        }

        // 3. If func is a built-in function object, return an implementation-defined String source
        //    code representation of func. The representation must have the syntax of a
        //    NativeFunction. Additionally, if func has an [[InitialName]] internal slot and
        //    func.[[InitialName]] is a String, the portion of the returned String that would be
        //    matched by NativeFunctionAccessoropt PropertyName must be the value of
        //    func.[[InitialName]].
        if (func == .object and func.object.is(BuiltinFunction)) {
            const builtin_function = func.object.as(BuiltinFunction);
            const name = builtin_function.fields.initial_name orelse "";
            const source_text = try std.fmt.allocPrint(
                agent.gc_allocator,
                "function {s}() {{ [native code] }}",
                .{name},
            );
            return Value.from(source_text);
        }

        // 4. If func is an Object and IsCallable(func) is true, return an implementation-defined
        //    String source code representation of func. The representation must have the syntax of
        //    a NativeFunction.
        if (func.isCallable()) return Value.from("function () { [native code] }");

        // 5. Throw a TypeError exception.
        return agent.throwException(
            .type_error,
            try std.fmt.allocPrint(agent.gc_allocator, "{} is not a function", .{func}),
        );
    }
};
