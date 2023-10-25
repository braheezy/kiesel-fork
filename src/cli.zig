const builtin = @import("builtin");
const std = @import("std");

const args = @import("args");
const gc = @import("gc");
const kiesel = @import("kiesel");

const Linenoise = @import("linenoise").Linenoise;
const SafePointer = @import("any-pointer").SafePointer;

const Allocator = std.mem.Allocator;

const Agent = kiesel.execution.Agent;
const ArgumentsList = kiesel.builtins.ArgumentsList;
const Diagnostics = kiesel.language.Diagnostics;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const ScriptOrModule = kiesel.execution.ScriptOrModule;
const SourceTextModule = kiesel.language.SourceTextModule;
const Value = kiesel.types.Value;
const coerceOptionsToObject = kiesel.types.coerceOptionsToObject;
const defineBuiltinFunction = kiesel.utils.defineBuiltinFunction;
const defineBuiltinProperty = kiesel.utils.defineBuiltinProperty;
const formatParseError = kiesel.utils.formatParseError;
const formatParseErrorHint = kiesel.utils.formatParseErrorHint;
const getOption = kiesel.types.getOption;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;

var tracked_promise_rejections: std.AutoArrayHashMap(
    *kiesel.builtins.Promise,
    Agent.HostHooks.PromiseRejectionTrackerOperation,
) = undefined;

const bdwgc_version_string = std.fmt.comptimePrint("{}.{}.{}", .{
    gc.c.GC_VERSION_MAJOR,
    gc.c.GC_VERSION_MINOR,
    gc.c.GC_VERSION_MICRO,
});

const version = std.fmt.comptimePrint(
    \\kiesel {[kiesel]s}
    \\bdwgc {[bdwgc]s}
    \\zig {[zig]s}
    \\
, .{
    .kiesel = kiesel.version_string,
    .bdwgc = bdwgc_version_string,
    .zig = builtin.zig_version_string,
});

pub const Kiesel = struct {
    pub fn create(realm: *Realm) !Object {
        const kiesel_object = try ordinaryObjectCreate(realm.agent, try realm.intrinsics.@"%Object.prototype%"());
        const gc_object = try ordinaryObjectCreate(realm.agent, try realm.intrinsics.@"%Object.prototype%"());
        try defineBuiltinFunction(gc_object, "collect", collect, 0, realm);
        try defineBuiltinFunction(kiesel_object, "createRealm", createRealm, 0, realm);
        try defineBuiltinFunction(kiesel_object, "detachArrayBuffer", detachArrayBuffer, 1, realm);
        try defineBuiltinFunction(kiesel_object, "evalScript", evalScript, 1, realm);
        try defineBuiltinProperty(kiesel_object, "gc", Value.from(gc_object));
        try defineBuiltinFunction(kiesel_object, "print", print, 1, realm);
        try defineBuiltinFunction(kiesel_object, "readStdin", readStdin, 0, realm);
        return kiesel_object;
    }

    fn collect(_: *Agent, _: Value, _: ArgumentsList) !Value {
        gc.collect();
        return .undefined;
    }

    fn createRealm(agent: *Agent, _: Value, _: ArgumentsList) !Value {
        const realm = try Realm.create(agent);
        try realm.setRealmGlobalObject(null, null);
        const global = try realm.setDefaultGlobalBindings();
        return Value.from(global);
    }

    fn detachArrayBuffer(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const array_buffer = arguments.get(0);
        if (array_buffer != .object or !array_buffer.object.is(kiesel.builtins.ArrayBuffer)) {
            return agent.throwException(.type_error, "Argument must be an ArrayBuffer");
        }
        try kiesel.builtins.detachArrayBuffer(
            agent,
            array_buffer.object.as(kiesel.builtins.ArrayBuffer),
            null,
        );
        return .undefined;
    }

    /// Algorithm from https://github.com/tc39/test262/blob/main/INTERPRETING.md
    fn evalScript(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const source_text = try arguments.get(0).toString(agent);

        // 1. Let hostDefined be any host-defined values for the provided sourceText (obtained in
        //    an implementation dependent manner)
        const host_defined = SafePointer.null_pointer;

        // 2. Let realm be the current Realm Record.
        const realm = agent.currentRealm();

        var diagnostics = Diagnostics.init(agent.gc_allocator);
        defer diagnostics.deinit();

        // 3. Let s be ParseScript(sourceText, realm, hostDefined).
        const script = Script.parse(source_text.utf8, realm, host_defined, .{
            .diagnostics = &diagnostics,
            .file_name = "evalScript",
        }) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            // 4. If s is a List of errors, then
            error.ParseError => {
                // a. Let error be the first element of s.
                const parse_error = diagnostics.errors.items[0];

                // b. Return Completion{[[Type]]: throw, [[Value]]: error, [[Target]]: empty}.
                return agent.throwException(
                    .syntax_error,
                    try formatParseError(agent.gc_allocator, parse_error),
                );
            },
        };

        // 5. Let status be ScriptEvaluation(s).
        const status = script.evaluate();

        // 6. Return Completion(status).
        return status;
    }

    fn print(agent: *Agent, _: Value, arguments: ArgumentsList) !Value {
        const stdout = std.io.getStdOut().writer();
        const value = arguments.get(0);
        const options = try coerceOptionsToObject(agent, arguments.get(1));
        const pretty = try getOption(options, "pretty", .boolean, null, false);
        if (pretty)
            stdout.print("{pretty}\n", .{value}) catch {}
        else
            stdout.print("{}\n", .{try value.toString(agent)}) catch {};
        return .undefined;
    }

    fn readStdin(agent: *Agent, _: Value, _: ArgumentsList) !Value {
        const bytes = std.io.getStdIn().readToEndAlloc(
            agent.gc_allocator,
            std.math.maxInt(usize),
        ) catch |err| {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(
                    agent.gc_allocator,
                    "Error while reading from stdin: {s}",
                    .{@errorName(err)},
                ),
            );
        };
        if (!std.unicode.utf8ValidateSlice(bytes)) {
            return agent.throwException(.type_error, "Invalid UTF-8");
        }
        return Value.from(bytes);
    }
};

fn run(allocator: Allocator, realm: *Realm, source_text: []const u8, options: struct {
    file_name: ?[]const u8 = null,
    module: bool = false,
}) !?Value {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const agent = realm.agent;

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    const parse_result: error{ ParseError, OutOfMemory }!ScriptOrModule = if (options.module) blk: {
        break :blk if (SourceTextModule.parse(source_text, realm, SafePointer.null_pointer, .{
            .diagnostics = &diagnostics,
            .file_name = options.file_name,
        })) |module| .{ .module = module } else |err| err;
    } else blk: {
        break :blk if (Script.parse(source_text, realm, SafePointer.null_pointer, .{
            .diagnostics = &diagnostics,
            .file_name = options.file_name,
        })) |script| .{ .script = script } else |err| err;
    };

    const script_or_module = parse_result catch |err| switch (err) {
        error.ParseError => {
            const parse_error = diagnostics.errors.items[0];
            const parse_error_hint = try formatParseErrorHint(allocator, parse_error, source_text);
            defer allocator.free(parse_error_hint);
            try stderr.print("{s}\n", .{parse_error_hint});
            agent.throwException(
                .syntax_error,
                try formatParseError(agent.gc_allocator, parse_error),
            ) catch {};
            try stderr.print("Uncaught exception: {pretty}\n", .{agent.exception.?});
            return null;
        },
        error.OutOfMemory => return error.OutOfMemory,
    };

    if (agent.options.debug.print_ast) {
        switch (script_or_module) {
            .script => |script| try script.ecmascript_code.print(stdout),
            .module => |module| try module.ecmascript_code.print(stdout),
        }
    }

    defer {
        // Run queued promise jobs
        while (agent.queued_promise_jobs.items.len != 0) {
            const queued_promise_job = agent.queued_promise_jobs.orderedRemove(0);
            const current_realm = agent.runningExecutionContext().realm;
            if (queued_promise_job.realm) |new_realm| {
                agent.runningExecutionContext().realm = new_realm;
            }
            _ = queued_promise_job.job.func(queued_promise_job.job.captures) catch {};
            agent.runningExecutionContext().realm = current_realm;
        }

        // Report tracked promise rejections
        var it = tracked_promise_rejections.iterator();
        while (it.next()) |entry| {
            const promise = entry.key_ptr.*;
            const operation = entry.value_ptr.*;
            switch (operation) {
                .reject => stderr.print(
                    "A promise was rejected without any handlers: {pretty}\n",
                    .{Value.from(promise.object())},
                ) catch {},
                .handle => stderr.print(
                    "A handler was added to an already rejected promise: {pretty}\n",
                    .{Value.from(promise.object())},
                ) catch {},
            }
        }
        tracked_promise_rejections.clearAndFree();
    }

    return switch (script_or_module) {
        .script => |script| script.evaluate(),
        .module => |module| blk: {
            module.initializeEnvironment() catch |err| break :blk err;
            module.executeModule(null) catch |err| break :blk err;
            return .undefined;
        },
    } catch |err| switch (err) {
        error.OutOfMemory => {
            try stderr.writeAll("Out of memory\n");
            return null;
        },
        error.ExceptionThrown => {
            try stderr.print("Uncaught exception: {pretty}\n", .{agent.exception.?});
            return null;
        },
    };
}

fn getHistoryPath(allocator: Allocator) ![]const u8 {
    const app_data_dir = try std.fs.getAppDataDir(allocator, "kiesel");
    defer allocator.free(app_data_dir);

    const history_path = try std.fs.path.join(allocator, &.{ app_data_dir, "history" });
    errdefer allocator.free(history_path);

    std.fs.makeDirAbsolute(app_data_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const file = try std.fs.createFileAbsolute(history_path, .{ .truncate = false });
    file.close();

    return history_path;
}

fn repl(allocator: Allocator, realm: *Realm, options: struct {
    module: bool = false,
}) !void {
    const stdout = std.io.getStdOut().writer();

    var linenoise = Linenoise.init(allocator);
    defer linenoise.deinit();

    const history_path = try getHistoryPath(allocator);
    defer allocator.free(history_path);

    linenoise.history.load(history_path) catch stdout.writeAll("Failed to load history\n") catch {};
    defer linenoise.history.save(history_path) catch stdout.writeAll("Failed to save history\n") catch {};

    while (true) {
        if (linenoise.linenoise("> ") catch |err| switch (err) {
            error.CtrlC => continue,
            else => return err,
        }) |source_text| {
            defer allocator.free(source_text);

            // Directly show another prompt when spamming enter, whitespace is evaluated
            // however (and will print 'undefined').
            if (source_text.len == 0) continue;

            try linenoise.history.add(source_text);

            if (try run(allocator, realm, source_text, .{
                .file_name = "repl",
                .module = options.module,
            })) |result| {
                try stdout.print("{pretty}\n", .{result});
            }
            // Handled exception & printed something, carry on
            else continue;
        }
        // ^D pressed, exit REPL
        else break;
    }
}

fn replBasic(allocator: Allocator, realm: *Realm, options: struct {
    module: bool = false,
}) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var array_list = std.ArrayList(u8).init(allocator);
        defer array_list.deinit();
        try stdout.writeAll("> ");
        stdin.streamUntilDelimiter(array_list.writer(), '\n', std.math.maxInt(usize)) catch |err| switch (err) {
            error.EndOfStream => {
                try stdout.writeAll("\n");
                break;
            },
            else => return err,
        };
        const source_text = try array_list.toOwnedSlice();
        defer allocator.free(source_text);

        // Directly show another prompt when spamming enter, whitespace is evaluated
        // however (and will print 'undefined').
        if (source_text.len == 0) continue;

        if (try run(allocator, realm, source_text, .{
            .file_name = "repl",
            .module = options.module,
        })) |result| {
            try stdout.print("{pretty}\n", .{result});
        }
        // Handled exception & printed something, carry on
        else continue;
    }
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut().writer();

    const Options = struct {
        command: ?[]const u8 = null,
        @"disable-gc": bool = false,
        module: bool = false,
        @"print-ast": bool = false,
        @"print-bytecode": bool = false,
        @"print-gc-warnings": bool = false,
        @"print-result": bool = false,
        version: bool = false,
        help: bool = false,

        pub const shorthands = .{
            .c = "command",
            .m = "module",
            .p = "print-result",
            .v = "version",
            .h = "help",
        };

        pub const meta = .{
            .usage_summary = "[options] [file]",
            .option_docs = .{
                .command = "Run the given code instead of reading from a file",
                .@"disable-gc" = "Disable garbage collection",
                .module = "Run code as a module instead of a script",
                .@"print-ast" = "Print the parsed AST",
                .@"print-bytecode" = "Print the generated bytecode",
                .@"print-gc-warnings" = "Print GC warnings, e.g. OOM",
                .@"print-result" = "Print the evaluated result",
                .version = "Print version information and exit",
                .help = "Print help text and exit",
            },
        };
    };
    const parsed_args = args.parseForCurrentProcess(Options, allocator, .print) catch return 1;
    defer parsed_args.deinit();

    if (parsed_args.options.version) {
        try stdout.writeAll(version);
        return 0;
    } else if (parsed_args.options.help) {
        try args.printHelp(Options, "kiesel", stdout);
        return 0;
    }

    if (parsed_args.options.@"disable-gc") gc.disable();
    if (!parsed_args.options.@"print-gc-warnings") {
        gc.c.GC_set_warn_proc(struct {
            fn func(_: [*c]u8, _: gc.c.GC_word) callconv(.C) void {}
        }.func);
    }
    var agent = try Agent.init(gc.allocator(), .{
        .debug = .{
            .print_ast = parsed_args.options.@"print-ast",
            .print_bytecode = parsed_args.options.@"print-bytecode",
        },
    });
    defer agent.deinit();

    tracked_promise_rejections = @TypeOf(tracked_promise_rejections).init(agent.gc_allocator);
    defer tracked_promise_rejections.deinit();

    agent.host_hooks.hostPromiseRejectionTracker = struct {
        fn func(
            promise: *kiesel.builtins.Promise,
            operation: Agent.HostHooks.PromiseRejectionTrackerOperation,
        ) void {
            if (tracked_promise_rejections.get(promise)) |previous_operation| {
                // Don't report `Promise.reject().catch(handler)` evaluated in a single script
                if (previous_operation == .reject and operation == .handle) {
                    _ = tracked_promise_rejections.orderedRemove(promise);
                    return;
                }
            }
            tracked_promise_rejections.put(promise, operation) catch {};
        }
    }.func;

    try Realm.initializeHostDefinedRealm(&agent, .{});
    const realm = agent.currentRealm();
    try defineBuiltinProperty(realm.global_object, "Kiesel", Value.from(try Kiesel.create(realm)));

    const path = if (parsed_args.positionals.len > 0) parsed_args.positionals[0] else null;
    if (path != null) {
        const file = try std.fs.cwd().openFile(path.?, .{});
        defer file.close();
        const file_name = std.fs.path.basename(path.?);
        const source_text = try file.reader().readAllAlloc(allocator, std.math.maxInt(usize));
        defer allocator.free(source_text);
        if (try run(allocator, realm, source_text, .{
            .file_name = file_name,
            .module = parsed_args.options.module,
        })) |result| {
            if (parsed_args.options.@"print-result")
                try stdout.print("{pretty}\n", .{result});
        } else return 1;
    } else if (parsed_args.options.command) |source_text| {
        if (try run(allocator, realm, source_text, .{
            .file_name = "command",
            .module = parsed_args.options.module,
        })) |result| {
            if (parsed_args.options.@"print-result")
                try stdout.print("{pretty}\n", .{result});
        } else return 1;
    } else if (builtin.os.tag != .wasi) {
        try repl(allocator, realm, .{
            .module = parsed_args.options.module,
        });
    } else {
        try replBasic(allocator, realm, .{
            .module = parsed_args.options.module,
        });
    }
    return 0;
}
