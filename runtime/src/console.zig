//! Console
//! https://console.spec.whatwg.org/

const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const String = kiesel.types.String;
const Value = kiesel.types.Value;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;

/// 1. Namespace console
/// https://console.spec.whatwg.org/#console-namespace
pub const console = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        // For historical web-compatibility reasons, the namespace object for console must have as
        // its [[Prototype]] an empty object, created as if by ObjectCreate(%ObjectPrototype%),
        // instead of %ObjectPrototype%.
        const prototype = try ordinaryObjectCreate(
            agent,
            try realm.intrinsics.@"%Object.prototype%"(),
        );
        const object = try ordinaryObjectCreate(agent, prototype);

        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "%Symbol.toStringTag%",
            Value.from("console"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
        );

        try object.defineBuiltinFunction(agent, "assert", assert, 0, realm);
        try object.defineBuiltinFunction(agent, "debug", debug, 0, realm);
        try object.defineBuiltinFunction(agent, "error", @"error", 0, realm);
        try object.defineBuiltinFunction(agent, "info", info, 0, realm);
        try object.defineBuiltinFunction(agent, "log", log, 0, realm);
        try object.defineBuiltinFunction(agent, "warn", warn, 0, realm);

        return object;
    }

    /// 1.1.1. assert(condition, ...data)
    /// https://console.spec.whatwg.org/#assert
    fn assert(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const condition = arguments.get(0);
        var data: std.ArrayList(Value) = .empty;
        defer data.deinit(agent.gc_allocator);
        if (arguments.count() > 1) try data.appendSlice(agent.gc_allocator, arguments.values[1..]);

        // 1. If condition is true, return.
        if (condition.toBoolean()) return .undefined;

        // 2. Let message be a string without any formatting specifiers indicating generically an
        //    assertion failure (such as "Assertion failed").
        const message = String.fromLiteral("Assertion failed");

        // 3. If data is empty, append message to data.
        if (data.items.len == 0) {
            try data.append(agent.gc_allocator, Value.from(message));
        }
        // 4. Otherwise:
        else {
            // 1. Let first be data[0].
            const first = data.items[0];

            // 2. If Type(first) is not String, then prepend message to data.
            if (!first.isString()) {
                try data.insert(agent.gc_allocator, 0, Value.from(message));
            }
            // 3. Otherwise:
            else {
                // 1. Let concat be the concatenation of message, U+003A (:), U+0020 SPACE, and first.
                const concat = try String.concat(
                    agent,
                    &.{ message, String.fromLiteral(": "), first.asString() },
                );

                // 2. Set data[0] to concat.
                data.items[0] = Value.from(concat);
            }
        }

        // 5. Perform Logger("assert", data).
        try logger(agent, .assert, data.items);
        return .undefined;
    }

    /// 1.1.3. debug(...data)
    /// https://console.spec.whatwg.org/#debug
    fn debug(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Perform Logger("debug", data).
        try logger(agent, .debug, arguments.values);
        return .undefined;
    }

    /// 1.1.4. error(...data)
    /// https://console.spec.whatwg.org/#error
    fn @"error"(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Perform Logger("error", data).
        try logger(agent, .@"error", arguments.values);
        return .undefined;
    }

    /// 1.1.5. info(...data)
    /// https://console.spec.whatwg.org/#info
    fn info(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Perform Logger("info", data).
        try logger(agent, .info, arguments.values);
        return .undefined;
    }

    /// 1.1.6. log(...data)
    /// https://console.spec.whatwg.org/#log
    fn log(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Perform Logger("log", data).
        try logger(agent, .log, arguments.values);
        return .undefined;
    }

    /// 1.1.9. warn(...data)
    /// https://console.spec.whatwg.org/#log
    fn warn(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Perform Logger("warn", data).
        try logger(agent, .warn, arguments.values);
        return .undefined;
    }
};

const LogLevel = enum {
    assert,
    debug,
    @"error",
    info,
    log,
    warn,
};

/// 2.1. Logger(logLevel, args)
/// https://console.spec.whatwg.org/#logger
fn logger(agent: *Agent, log_level: LogLevel, args: []const Value) Agent.Error!void {
    // 1. If args is empty, return.
    if (args.len == 0) return;

    // 2. Let first be args[0].
    const first = args[0];

    // 3. Let rest be all elements following first in args.
    const rest = args[1..];

    // 4. If rest is empty, perform Printer(logLevel, « first ») and return.
    if (rest.len == 0) {
        printer(agent.platform.stdout, log_level, &.{first}) catch {};
    }
    // 5. Otherwise, perform Printer(logLevel, Formatter(args)).
    else {
        printer(agent.platform.stdout, log_level, try formatter(agent, args)) catch {};
    }

    // 6. Return undefined.
}

/// 2.2. Formatter(args)
/// https://console.spec.whatwg.org/#formatter
fn formatter(agent: *Agent, args: []const Value) Agent.Error![]const Value {
    std.debug.assert(args.len != 0);

    // 1. If args’s size is 1, return args.
    if (args.len == 1) return args;

    // 2. Let target be the first element of args.
    const target = args[0];

    // 3. Let current be the second element of args.
    const current = args[1];

    // TODO: 4. Find the first possible format specifier specifier, from the left to the right in
    //          target.
    _ = target;
    _ = current;
    _ = agent;

    // 5. If no format specifier was found, return args.
    return args;

    // TODO: 6-8.
}

/// 2.3. Printer(logLevel, args[, options])
/// https://console.spec.whatwg.org/#printer
fn printer(writer: *std.Io.Writer, log_level: LogLevel, args: []const Value) std.Io.Writer.Error!void {
    switch (log_level) {
        .assert => {},
        .debug => try writer.writeAll("[DEBUG] "),
        .@"error" => try writer.writeAll("[ERROR] "),
        .info => try writer.writeAll("[INFO] "),
        .log => {},
        .warn => try writer.writeAll("[WARNING] "),
    }
    for (args, 0..) |arg, i| {
        if (i > 0) try writer.writeByte(' ');
        if (arg.isString()) {
            try writer.print("{f}", .{arg.asString().fmtRaw()});
        } else {
            try writer.print("{f}", .{arg});
        }
    }
    try writer.writeByte('\n');
    try writer.flush();
}
