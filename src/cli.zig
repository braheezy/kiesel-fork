const builtin = @import("builtin");
const std = @import("std");

const args = @import("args");
const icu4zig = @import("icu4zig");
const kiesel = @import("kiesel");
const kiesel_runtime = @import("kiesel-runtime");

const Editor = @import("zigline").Editor;

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const Diagnostics = kiesel.language.Diagnostics;
const HostHooks = kiesel.execution.HostHooks;
const ImportedModulePayload = kiesel.language.ImportedModulePayload;
const ImportedModuleReferrer = kiesel.language.ImportedModuleReferrer;
const Module = kiesel.language.Module;
const ModuleRequest = kiesel.language.ModuleRequest;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const SafePointer = kiesel.types.SafePointer;
const Script = kiesel.language.Script;
const ScriptOrModule = kiesel.execution.ScriptOrModule;
const SourceTextModule = kiesel.language.SourceTextModule;
const String = kiesel.types.String;
const Value = kiesel.types.Value;
const finishLoadingImportedModule = kiesel.language.finishLoadingImportedModule;
const fmtParseError = kiesel.language.fmtParseError;
const fmtParseErrorHint = kiesel.language.fmtParseErrorHint;
const ordinaryObjectCreate = kiesel.builtins.ordinaryObjectCreate;
const parseJSONModule = kiesel.language.parseJSONModule;
const regExpCreate = kiesel.builtins.regExpCreate;
const regExpExec = kiesel.builtins.regExpExec;

var tracked_promise_rejections: std.AutoArrayHashMapUnmanaged(
    *kiesel.builtins.Promise,
    HostHooks.PromiseRejectionTrackerOperation,
) = .empty;

var module_cache: ModuleRequest.HashMapUnmanaged(Module) = .empty;

// Python REPL my beloved 🐍
const repl_preamble = std.fmt.comptimePrint(
    \\Kiesel {[kiesel]} [Zig {[zig]}] on {[os]s}
    \\Use {[eof]s} to exit.
    \\
, .{
    .kiesel = kiesel.version,
    .zig = builtin.zig_version,
    .os = @tagName(builtin.os.tag),
    .eof = if (builtin.os.tag == .windows) "Ctrl+Z followed by Enter" else "Ctrl+D",
});

const ScriptOrModuleHostDefined = struct {
    base_dir: []const u8,
};

fn resolveModulePath(
    allocator: std.mem.Allocator,
    script_or_module: ScriptOrModule,
    specifier: []const u8,
) std.mem.Allocator.Error![]const u8 {
    const host_defined = switch (script_or_module) {
        .script => |script| script.host_defined,
        .module => |module| switch (module) {
            .source_text_module => |m| m.host_defined,
            .synthetic_module => unreachable,
        },
    };
    const base_dir: []const u8 = host_defined.cast(*ScriptOrModuleHostDefined).base_dir;
    std.debug.assert(std.fs.path.isAbsolute(base_dir));
    const resolved_path = try std.fs.path.resolve(allocator, &.{ base_dir, specifier });
    std.debug.assert(std.fs.path.isAbsolute(resolved_path));
    return resolved_path;
}

fn initializeGlobalObject(agent: *Agent, realm: *Realm, global_object: *Object) Agent.Error!void {
    try global_object.defineBuiltinPropertyLazy(
        agent,
        "Kiesel",
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                return Value.from(try Kiesel.create(agent_, realm_));
            }
        }.initializer,
        realm,
        .builtin_default,
    );
    if (kiesel.build_options.enable_runtime) {
        try kiesel_runtime.addBindings(agent, realm, global_object);
    }
}

const Kiesel = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const kiesel_object = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());
        if (kiesel.build_options.enable_annex_b) {
            try kiesel_object.defineBuiltinFunction(agent, "createIsHTMLDDA", createIsHTMLDDA, 0, realm);
        }
        try kiesel_object.defineBuiltinFunction(agent, "createRealm", createRealm, 0, realm);
        try kiesel_object.defineBuiltinFunction(agent, "detachArrayBuffer", detachArrayBuffer, 1, realm);
        try kiesel_object.defineBuiltinFunction(agent, "evalScript", evalScript, 1, realm);
        try kiesel_object.defineBuiltinFunction(agent, "isMerlin", isMerlin, 1, realm);
        try kiesel_object.defineBuiltinFunction(agent, "print", print, 1, realm);
        try kiesel_object.defineBuiltinFunction(agent, "readFile", readFile_, 1, realm);
        try kiesel_object.defineBuiltinFunction(agent, "readLine", readLine, 0, realm);
        try kiesel_object.defineBuiltinFunction(agent, "readStdin", readStdin, 0, realm);
        try kiesel_object.defineBuiltinFunction(agent, "sleep", sleep, 1, realm);
        try kiesel_object.defineBuiltinFunction(agent, "writeFile", writeFile, 2, realm);
        if (kiesel.build_options.enable_libgc) {
            const gc_object = try ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());
            try gc_object.defineBuiltinFunction(agent, "collect", collect, 0, realm);
            try kiesel_object.defineBuiltinProperty(agent, "gc", Value.from(gc_object));
        }
        return kiesel_object;
    }

    fn collect(_: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        kiesel.gc.collect();
        return .undefined;
    }

    fn createIsHTMLDDA(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const is_htmldda = try kiesel.builtins.Object.create(agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
            .is_htmldda = true,
            .internal_methods = &.{
                .call = struct {
                    /// This is required by [test262](https://github.com/tc39/test262/blob/main/INTERPRETING.md#host-defined-functions).
                    ///
                    /// What should happen when the function is being called with something
                    /// other than no arguments or an empty string is unclear and different
                    /// test262 runtimes disagree on this:
                    ///
                    /// - JSC: [Returns null unconditionally](https://github.com/WebKit/WebKit/blob/b571ec5131dcca906981b9a477d7b71e9605b6a6/Source/JavaScriptCore/jsc.cpp#L2818-L2827)
                    /// - V8: [Returns null unconditionally](https://source.chromium.org/chromium/chromium/src/+/main:v8/src/runtime/runtime-test.cc;l=1038-1055;drc=ca3478a884cd4d1c5d7897ded9838773ca1c4fd3)
                    /// - QuickJS: [Returns null unconditionally](https://github.com/quickjs-ng/quickjs/blob/6868fb9e2516fde4a7a3fcef113a6bb1e5ecc957/run-test262.c#L753-L757)
                    /// - LibJS: [Returns undefined](https://github.com/SerenityOS/serenity/blob/9a207da36845e18dc4f747d8ecc98fbc0e11545c/Userland/Libraries/LibJS/Contrib/Test262/IsHTMLDDA.cpp#L20-L32)
                    /// - SpiderMonkey: [Throws](https://searchfox.org/mozilla-central/rev/c130c69b7b863d5e28ab9524b65c27c7a9507c48/js/src/shell/js.cpp#7071-7085)
                    ///
                    /// We pick the most common one :^)
                    fn call(_: *Agent, _: *Object, _: Value, _: Arguments) Agent.Error!Value {
                        return .null;
                    }
                }.call,
            },
        });
        return Value.from(is_htmldda);
    }

    fn createRealm(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        try Realm.initializeHostDefinedRealm(agent, .{});
        const realm = agent.currentRealm();
        _ = agent.execution_context_stack.pop().?;
        try initializeGlobalObject(agent, realm, realm.global_object);
        return Value.from(realm.global_object);
    }

    fn detachArrayBuffer(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const array_buffer = arguments.get(0);
        if (!array_buffer.isObject() or !array_buffer.asObject().is(kiesel.builtins.ArrayBuffer)) {
            return agent.throwException(.type_error, "Argument must be an ArrayBuffer", .{});
        }
        try kiesel.builtins.detachArrayBuffer(
            agent,
            array_buffer.asObject().as(kiesel.builtins.ArrayBuffer),
            null,
        );
        return .undefined;
    }

    /// Algorithm from https://github.com/tc39/test262/blob/main/INTERPRETING.md
    fn evalScript(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const source_text = try arguments.get(0).toString(agent);

        // 1. Let hostDefined be any host-defined values for the provided sourceText (obtained in
        //    an implementation dependent manner)
        const host_defined = switch (agent.getActiveScriptOrModule().?) {
            .script => |script| script.host_defined,
            .module => |module| switch (module) {
                .source_text_module => |m| m.host_defined,
                .synthetic_module => unreachable,
            },
        };

        // 2. Let realm be the current Realm Record.
        const realm = agent.currentRealm();

        var diagnostics = Diagnostics.init(agent.gc_allocator);
        defer diagnostics.deinit();

        // 3. Let s be ParseScript(sourceText, realm, hostDefined).
        const script = Script.parse(try source_text.toUtf8(agent.gc_allocator), realm, host_defined, .{
            .diagnostics = &diagnostics,
            .file_name = "evalScript",
        }) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            // 4. If s is a List of errors, then
            error.ParseError => {
                // a. Let error be the first element of s.
                const parse_error = diagnostics.errors.items[0];

                // b. Return Completion{[[Type]]: throw, [[Value]]: error, [[Target]]: empty}.
                return agent.throwException(.syntax_error, "{}", .{fmtParseError(parse_error)});
            },
        };

        // 5. Let status be ScriptEvaluation(s).
        const status = script.evaluate();

        // 6. Return Completion(status).
        return status;
    }

    fn isMerlin(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const value = arguments.get(0);
        const string = try value.toString(agent);
        // See https://github.com/DerMolly/is-merlin/blob/main/src/index.ts for the source of the regexp
        const regexp = try regExpCreate(
            agent,
            Value.from("[mM][eE][rR][lL][iI][nN]|[rR][uU][hH][rR][-_\\s]*[sS][cC][hH][oO][lL][zZ]"),
            .undefined,
        );
        const match = try regExpExec(agent, regexp, string);
        return Value.from(match != null);
    }

    fn print(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const stdout = agent.platform.stdout;
        const value = arguments.get(0);
        const options = try arguments.get(1).coerceOptionsToObject(agent);
        const newline = try options.getOption(agent, "newline", .boolean, null, true);
        const pretty = try options.getOption(agent, "pretty", .boolean, null, false);
        const end = if (newline) "\n" else "";
        if (pretty)
            stdout.print("{pretty}{s}", .{ value, end }) catch {}
        else
            stdout.print("{}{s}", .{ try value.toString(agent), end }) catch {};
        return .undefined;
    }

    fn readFile_(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const path = try arguments.get(0).toString(agent);
        const bytes = readFile(
            agent.gc_allocator,
            try path.toUtf8(agent.gc_allocator),
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return agent.throwException(
                .type_error,
                "Error while reading file: {s}",
                .{@errorName(err)},
            ),
        };
        if (!std.unicode.utf8ValidateSlice(bytes)) {
            return agent.throwException(.type_error, "Invalid UTF-8", .{});
        }
        return Value.from(try String.fromUtf8(agent, bytes));
    }

    fn readLine(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        const stdin = std.io.getStdIn().reader();
        const bytes = stdin.readUntilDelimiterOrEofAlloc(
            agent.gc_allocator,
            '\n',
            std.math.maxInt(usize),
        ) catch |err| {
            return agent.throwException(
                .type_error,
                "Error while reading from stdin: {s}",
                .{@errorName(err)},
            );
        } orelse "";
        if (!std.unicode.utf8ValidateSlice(bytes)) {
            return agent.throwException(.type_error, "Invalid UTF-8", .{});
        }
        return Value.from(try String.fromUtf8(agent, bytes));
    }

    fn readStdin(agent: *Agent, _: Value, _: Arguments) Agent.Error!Value {
        const stdin = std.io.getStdIn().reader();
        const bytes = stdin.readAllAlloc(
            agent.gc_allocator,
            std.math.maxInt(usize),
        ) catch |err| {
            return agent.throwException(
                .type_error,
                "Error while reading from stdin: {s}",
                .{@errorName(err)},
            );
        };
        if (!std.unicode.utf8ValidateSlice(bytes)) {
            return agent.throwException(.type_error, "Invalid UTF-8", .{});
        }
        return Value.from(try String.fromUtf8(agent, bytes));
    }

    fn sleep(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const milliseconds = try arguments.get(0).toNumber(agent);
        if (milliseconds.asFloat() < 0 or !milliseconds.isFinite()) {
            return agent.throwException(
                .range_error,
                "Sleep duration must be a positive finite number",
                .{},
            );
        }
        const nanoseconds = std.math.lossyCast(u64, milliseconds.asFloat() * 1_000_000);
        std.Thread.sleep(nanoseconds);
        return .undefined;
    }

    fn writeFile(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const path = try arguments.get(0).toString(agent);
        const contents = try arguments.get(1).toString(agent);

        const file = std.fs.cwd().createFile(
            try path.toUtf8(agent.gc_allocator),
            .{},
        ) catch |err| {
            return agent.throwException(
                .type_error,
                "Error while opening file: {s}",
                .{@errorName(err)},
            );
        };
        defer file.close();
        file.writeAll(try contents.toUtf8(agent.gc_allocator)) catch |err| {
            return agent.throwException(
                .type_error,
                "Error while writing file: {s}",
                .{@errorName(err)},
            );
        };
        return .undefined;
    }
};

fn run(allocator: std.mem.Allocator, realm: *Realm, source_text: []const u8, options: struct {
    base_dir: []const u8,
    origin: union(enum) {
        repl,
        command,
        path: []const u8,
    },
    module: bool = false,
    print_promise_rejection_warnings: bool = true,
}) !Value {
    const agent = realm.agent;
    const stdout = agent.platform.stdout;
    const stderr = agent.platform.stderr;

    const host_defined = SafePointer.make(*ScriptOrModuleHostDefined, blk: {
        const ptr = try agent.gc_allocator.create(ScriptOrModuleHostDefined);
        ptr.* = .{ .base_dir = options.base_dir };
        break :blk ptr;
    });

    var diagnostics = Diagnostics.init(allocator);
    defer diagnostics.deinit();

    const file_name: []const u8 = switch (options.origin) {
        .repl => "repl",
        .command => "command",
        .path => |path| path,
    };

    const parse_result: error{ ParseError, OutOfMemory }!ScriptOrModule = if (options.module) blk: {
        break :blk if (SourceTextModule.parse(source_text, realm, host_defined, .{
            .diagnostics = &diagnostics,
            .file_name = file_name,
        })) |source_text_module| .{
            .module = .{ .source_text_module = source_text_module },
        } else |err| err;
    } else blk: {
        break :blk if (Script.parse(source_text, realm, host_defined, .{
            .diagnostics = &diagnostics,
            .file_name = file_name,
        })) |script| .{ .script = script } else |err| err;
    };

    const script_or_module = parse_result catch |err| switch (err) {
        error.ParseError => {
            const parse_error = diagnostics.errors.items[0];
            try stderr.print("{}\n", .{fmtParseErrorHint(parse_error, source_text)});
            const syntax_error = try agent.createErrorObject(
                .syntax_error,
                "{}",
                .{fmtParseError(parse_error)},
            );
            const exception: Agent.Exception = .{
                .value = Value.from(syntax_error),
                .stack_trace = &.{},
            };
            try stderr.print("{pretty}\n", .{exception});
            return error.AlreadyReported;
        },
        error.OutOfMemory => return error.OutOfMemory,
    };

    if (agent.options.debug.print_ast) {
        switch (script_or_module) {
            .script => |script| try script.print(stdout),
            .module => |module| try module.source_text_module.print(stdout),
        }
    }

    defer {
        agent.drainJobQueue();

        // Report tracked promise rejections
        if (options.print_promise_rejection_warnings) {
            var it = tracked_promise_rejections.iterator();
            while (it.next()) |entry| {
                const promise = entry.key_ptr.*;
                const operation = entry.value_ptr.*;
                switch (operation) {
                    .reject => stderr.print(
                        "A promise was rejected without any handlers: {pretty}\n",
                        .{Value.from(&promise.object)},
                    ) catch {},
                    .handle => stderr.print(
                        "A handler was added to an already rejected promise: {pretty}\n",
                        .{Value.from(&promise.object)},
                    ) catch {},
                }
            }
        }
        tracked_promise_rejections.clearAndFree(agent.gc_allocator);
    }

    return switch (script_or_module) {
        .script => |script| script.evaluate(),
        .module => |module| blk: {
            const module_path = resolveModulePath(
                agent.gc_allocator,
                script_or_module,
                options.origin.path,
            ) catch |err| break :blk err;
            const cache_key: ModuleRequest = .{
                .specifier = try String.fromUtf8(agent, module_path),
                .attributes = &.{},
            };
            try module_cache.putNoClobber(agent.gc_allocator, cache_key, module);
            var promise = module.loadRequestedModules(agent, null) catch |err| break :blk err;
            std.debug.assert(agent.queued_jobs.items.len == 0);
            switch (promise.fields.promise_state) {
                .pending => unreachable,
                .rejected => {
                    tracked_promise_rejections.clearAndFree(agent.gc_allocator);
                    agent.exception = .{
                        .value = promise.fields.promise_result,
                        // TODO: Capture stack when rejecting a promise
                        .stack_trace = &.{},
                    };
                    break :blk error.ExceptionThrown;
                },
                .fulfilled => {
                    module.link(agent) catch |err| break :blk err;
                    promise = module.evaluate(agent) catch |err| break :blk err;
                    std.debug.assert(agent.queued_jobs.items.len == 0);
                    switch (promise.fields.promise_state) {
                        .pending => unreachable,
                        .rejected => {
                            tracked_promise_rejections.clearAndFree(agent.gc_allocator);
                            agent.exception = .{
                                .value = promise.fields.promise_result,
                                // TODO: Capture stack when rejecting a promise
                                .stack_trace = &.{},
                            };
                            break :blk error.ExceptionThrown;
                        },
                        .fulfilled => {
                            break :blk Value.undefined;
                        },
                    }
                },
            }
        },
    } catch |err| switch (err) {
        error.OutOfMemory => {
            try stderr.writeAll("Out of memory\n");
            return error.AlreadyReported;
        },
        error.ExceptionThrown => {
            const exception = agent.clearException();
            try stderr.print("{pretty}\n", .{exception});
            return error.AlreadyReported;
        },
    };
}

const ReadFileError = std.mem.Allocator.Error || std.fs.File.OpenError || std.fs.File.ReadError;

fn readFile(
    allocator: std.mem.Allocator,
    path: []const u8,
) ReadFileError![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    return file.readToEndAlloc(allocator, std.math.maxInt(usize));
}

const GetHistoryPathError =
    std.mem.Allocator.Error ||
    std.fs.GetAppDataDirError ||
    std.fs.Dir.MakeError ||
    std.fs.File.OpenError;

fn getHistoryPath(allocator: std.mem.Allocator) GetHistoryPathError![]const u8 {
    const app_data_dir = try std.fs.getAppDataDir(allocator, "kiesel");
    defer allocator.free(app_data_dir);

    const history_path = try std.fs.path.join(allocator, &.{ app_data_dir, "history" });
    errdefer allocator.free(history_path);

    std.fs.cwd().makePath(app_data_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const file = try std.fs.createFileAbsolute(history_path, .{ .truncate = false });
    file.close();

    return history_path;
}

fn printValueDebugInfo(
    writer: anytype,
    tty_config: std.io.tty.Config,
    value: Value,
) @TypeOf(writer).Error!void {
    // Porffor REPL my beloved 💜
    try tty_config.setColor(writer, .blue);
    switch (value.type()) {
        .number => try writer.print(" (type: {s})", .{@tagName(value.asNumber())}),
        .string => try writer.print(" (ptr: 0x{x}, type: {s})", .{
            @intFromPtr(value.asString()),
            @tagName(value.asString().slice),
        }),
        .symbol => try writer.print(" (ptr: 0x{x})", .{@intFromPtr(value.asSymbol())}),
        .object => try writer.print(" (ptr: 0x{x}, shape: 0x{x}, indexed: {s}, tag: {s})", .{
            @intFromPtr(value.asObject()),
            @intFromPtr(value.asObject().property_storage.shape),
            @tagName(value.asObject().property_storage.indexed_properties.storage),
            @tagName(value.asObject().tag),
        }),
        else => {},
    }
    try tty_config.setColor(writer, .reset);
}

fn repl(allocator: std.mem.Allocator, realm: *Realm, options: struct {
    base_dir: []const u8,
    debug: bool = false,
    module: bool = false,
    print_promise_rejection_warnings: bool = true,
}) !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.writeAll(repl_preamble);

    var editor = Editor.init(allocator, .{});
    defer editor.deinit();

    var handler: struct {
        editor: *Editor,

        pub fn display_refresh(self: *@This()) void {
            self.editor.stripStyles();

            const line = self.editor.getBufferedLine() catch return;
            defer self.editor.allocator.free(line);

            var tokenizer = kiesel.language.tokenizer.initValidateUtf8(line, null) catch return;
            const change = kiesel.utils.temporaryChange(
                &kiesel.language.tokenizer.state,
                .{
                    .tokenizer = &tokenizer,
                    // Workaround to ensure the tokenizer generates template literal middle and
                    // tail tokens even though there is no parser setting this.
                    .parsing_template_literal = true,
                },
            );
            defer change.restore();

            while (tokenizer.next() catch null) |token| {
                self.editor.stylize(
                    .{
                        .begin = tokenizer.offset - token.text.len,
                        .end = tokenizer.offset,
                    },
                    switch (token.type) {
                        .@"await",
                        .@"break",
                        .@"catch",
                        .class,
                        .@"const",
                        .@"continue",
                        .debugger,
                        .default,
                        .delete,
                        .@"enum",
                        .@"export",
                        .extends,
                        .function,
                        .hashbang_comment,
                        .import,
                        .in,
                        .instanceof,
                        .new,
                        .@"return",
                        .super,
                        .this,
                        .typeof,
                        .@"var",
                        .void,
                        => .{ .foreground = .{ .xterm = .Blue }, .bold = true },

                        .case,
                        .comment,
                        .do,
                        .@"else",
                        .finally,
                        .@"for",
                        .@"if",
                        .@"switch",
                        .throw,
                        .@"try",
                        .@"while",
                        .with,
                        .yield,
                        => .{ .foreground = .{ .xterm = .Cyan } },

                        .false, .true => .{ .foreground = .{ .xterm = .Blue } },
                        .null => .{ .foreground = .{ .xterm = .Yellow } },
                        .numeric => .{ .foreground = .{ .xterm = .Magenta } },
                        .regular_expression,
                        .string,
                        .template,
                        .template_head,
                        .template_middle,
                        .template_tail,
                        => .{ .foreground = .{ .xterm = .Green } },
                        else => .{},
                    },
                ) catch continue;
            }
        }
    } = .{ .editor = &editor };
    editor.setHandler(&handler);

    const history_path = if (builtin.os.tag != .wasi) try getHistoryPath(allocator) else "";
    defer if (builtin.os.tag != .wasi) allocator.free(history_path);

    if (builtin.os.tag != .wasi) editor.loadHistory(history_path) catch stdout.writeAll("Failed to load history\n") catch {};
    defer if (builtin.os.tag != .wasi) editor.saveHistory(history_path) catch stdout.writeAll("Failed to save history\n") catch {};

    while (true) {
        const source_text = editor.getLine("> ") catch |err| switch (err) {
            error.Eof => break,
            else => return err,
        };
        defer allocator.free(source_text);

        // Directly show another prompt when spamming enter, whitespace is evaluated
        // however (and will print 'undefined').
        if (source_text.len == 0) continue;

        try editor.addToHistory(source_text);

        const result = run(allocator, realm, source_text, .{
            .base_dir = options.base_dir,
            .origin = .repl,
            .module = options.module,
            .print_promise_rejection_warnings = options.print_promise_rejection_warnings,
        }) catch |err| switch (err) {
            // Handled exception & printed something, carry on
            error.AlreadyReported => continue,
            else => return err,
        };
        try stdout.print("{pretty}", .{result});
        if (options.debug) {
            const tty_config = realm.agent.platform.tty_config;
            try printValueDebugInfo(stdout, tty_config, result);
        }
        try stdout.writeAll("\n");
    }
}

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const Options = struct {
        command: ?[]const u8 = null,
        debug: bool = false,
        @"disable-gc": bool = false,
        module: bool = false,
        @"print-ast": bool = false,
        @"print-bytecode": bool = false,
        @"print-gc-warnings": bool = false,
        @"print-promise-rejection-warnings": bool = true,
        @"print-result": bool = false,
        version: bool = false,
        help: bool = false,

        pub const shorthands = .{
            .c = "command",
            .d = "debug",
            .m = "module",
            .p = "print-result",
            .v = "version",
            .h = "help",
        };

        pub const meta = .{
            .usage_summary = "[options] [file]",
            .option_docs = .{
                .command = "Run the given code instead of reading from a file",
                .debug = "Enable debug mode",
                .@"disable-gc" = "Disable garbage collection",
                .module = "Run code as a module instead of a script",
                .@"print-ast" = "Print the parsed AST",
                .@"print-bytecode" = "Print the generated bytecode",
                .@"print-gc-warnings" = "Print GC warnings, e.g. OOM",
                .@"print-promise-rejection-warnings" = "Print promise rejection warnings",
                .@"print-result" = "Print the evaluated result",
                .version = "Print version information and exit",
                .help = "Print help text and exit",
            },
        };
    };
    const parsed_args = args.parseForCurrentProcess(Options, allocator, .print) catch return 1;
    defer parsed_args.deinit();

    const path_arg = if (parsed_args.positionals.len > 0) parsed_args.positionals[0] else null;

    if (parsed_args.options.version) {
        try stdout.print("kiesel {}\n", .{kiesel.version});
        if (kiesel.build_options.enable_libgc) {
            try stdout.print("libgc {}\n", .{kiesel.gc.libgc_version});
        }
        try stdout.print("zig {}\n", .{builtin.zig_version});
        return 0;
    }
    if (parsed_args.options.help) {
        try args.printHelp(Options, "kiesel", stdout);
        return 0;
    }
    if (parsed_args.options.module) {
        if (path_arg == null) {
            try stderr.writeAll("-m/--module option must not be used in REPL mode\n");
            return 1;
        }
        if (parsed_args.options.command != null) {
            try stderr.writeAll("-m/--module option must not be used with -c/--command\n");
            return 1;
        }
    }

    if (kiesel.build_options.enable_libgc and parsed_args.options.@"disable-gc") {
        kiesel.gc.disable();
    }
    if (kiesel.build_options.enable_libgc and !parsed_args.options.@"print-gc-warnings") {
        kiesel.gc.disableWarnings();
    }
    var platform = Agent.Platform.default();
    defer platform.deinit();
    var agent = try Agent.init(&platform, .{
        .debug = .{
            .print_ast = parsed_args.options.@"print-ast",
            .print_bytecode = parsed_args.options.@"print-bytecode",
        },
    });
    defer agent.deinit();

    if (kiesel.build_options.enable_intl) {
        if (std.process.getEnvVarOwned(allocator, "LANG")) |lang| {
            defer allocator.free(lang);
            const lang_trimmed = if (std.mem.indexOf(u8, lang, ".UTF-8")) |index|
                lang[0..index]
            else
                lang;
            if (icu4zig.Locale.fromString(lang_trimmed)) |locale|
                platform.default_locale = locale
            else |_| {}
        } else |_| {}
    }

    defer tracked_promise_rejections.deinit(agent.gc_allocator);
    defer module_cache.deinit(agent.gc_allocator);

    agent.host_hooks.hostGetSupportedImportAttributes = struct {
        fn func(agent_: *Agent) std.mem.Allocator.Error!HostHooks.SupportedImportAttributes {
            var supported_import_attributes: HostHooks.SupportedImportAttributes = .empty;
            try supported_import_attributes.put(agent_.gc_allocator, String.fromLiteral("type"), {});
            return supported_import_attributes;
        }
    }.func;

    agent.host_hooks.hostLoadImportedModule = struct {
        fn func(
            agent_: *Agent,
            referrer: ImportedModuleReferrer,
            module_request: ModuleRequest,
            _: SafePointer,
            payload: ImportedModulePayload,
        ) std.mem.Allocator.Error!void {
            const result = blk: {
                const script_or_module: ScriptOrModule = switch (referrer) {
                    .script => |script| .{ .script = script },
                    .module => |source_text_module| .{
                        .module = .{ .source_text_module = source_text_module },
                    },
                    .realm => unreachable,
                };
                const specifier_utf8 = try module_request.specifier.toUtf8(agent_.gc_allocator);
                defer agent_.gc_allocator.free(specifier_utf8);
                const module_path = resolveModulePath(
                    agent_.gc_allocator,
                    script_or_module,
                    specifier_utf8,
                ) catch |err| break :blk err;
                // NOTE: The spec says that the same (referrer, moduleRequest) pair must resolve to
                // the same cached module, but also that the actual mapping is host-defined.
                // When a module is loaded via dynamic import the referrer is a script, which then
                // doesn't have a cache hit if the module imports itself (referrer is a module) and
                // causes infinite recursion.
                // I haven't checked any of the major engines but at least Boa, LibJS, and QuickJS
                // all use only the module name/path:
                // - https://github.com/boa-dev/boa/blob/fc2a6e09969772feba98eaa89aaf89ca4797e925/core/engine/src/module/loader.rs#L248C5-L248C15
                // - https://github.com/SerenityOS/serenity/blob/648b36f3c53bf3fd83a8dbf5fc788046abe10e29/Userland/Libraries/LibJS/Runtime/VM.cpp#L481-L487
                // - https://github.com/bellard/quickjs/blob/36911f0d3ab1a4c190a4d5cbe7c2db225a455389/quickjs.c#L27590-L27596
                const cache_key: ModuleRequest = .{
                    .specifier = try String.fromUtf8(agent_, module_path),
                    .attributes = module_request.attributes,
                };
                if (module_cache.get(cache_key)) |module| break :blk module;
                if (loadImportedModule(agent_, module_request, module_path)) |module| {
                    try module_cache.putNoClobber(agent_.gc_allocator, cache_key, module);
                    break :blk module;
                } else |err| break :blk err;
            };
            try finishLoadingImportedModule(agent_, referrer, module_request, payload, result);
        }

        fn loadImportedModule(
            agent_: *Agent,
            module_request: ModuleRequest,
            module_path: []const u8,
        ) Agent.Error!Module {
            const realm = agent_.currentRealm();
            const source_text = readFile(agent_.gc_allocator, module_path) catch |err| {
                return agent_.throwException(
                    .internal_error,
                    "Failed to import '{}': {s}",
                    .{ module_request.specifier, @errorName(err) },
                );
            };
            defer agent_.gc_allocator.free(source_text);

            for (module_request.attributes) |import_attribute| {
                if (import_attribute.key.eql(String.fromLiteral("type"))) {
                    if (import_attribute.value.eql(String.fromLiteral("json"))) {
                        const synthetic_module = try parseJSONModule(
                            agent_,
                            try String.fromUtf8(agent_, source_text),
                        );
                        return .{ .synthetic_module = synthetic_module };
                    }
                    return agent_.throwException(
                        .internal_error,
                        "Failed to import '{}' with unknown module type '{}'",
                        .{ module_request.specifier, import_attribute.value },
                    );
                }
            }

            const host_defined = SafePointer.make(*ScriptOrModuleHostDefined, blk: {
                const ptr = try agent_.gc_allocator.create(ScriptOrModuleHostDefined);
                ptr.* = .{ .base_dir = std.fs.path.dirname(module_path).? };
                break :blk ptr;
            });

            var diagnostics = Diagnostics.init(agent_.gc_allocator);
            defer diagnostics.deinit();

            const source_text_module = SourceTextModule.parse(source_text, realm, host_defined, .{
                .diagnostics = &diagnostics,
                .file_name = std.fs.path.basename(module_path),
            }) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.ParseError => {
                    const parse_error = diagnostics.errors.items[0];
                    return agent_.throwException(
                        .syntax_error,
                        "{}",
                        .{fmtParseError(parse_error)},
                    );
                },
            };
            return .{ .source_text_module = source_text_module };
        }
    }.func;

    agent.host_hooks.hostPromiseRejectionTracker = struct {
        fn func(
            agent_: *Agent,
            promise: *kiesel.builtins.Promise,
            operation: HostHooks.PromiseRejectionTrackerOperation,
        ) void {
            if (tracked_promise_rejections.get(promise)) |previous_operation| {
                // Don't report `Promise.reject().catch(handler)` evaluated in a single script
                if (previous_operation == .reject and operation == .handle) {
                    _ = tracked_promise_rejections.orderedRemove(promise);
                    return;
                }
            }
            tracked_promise_rejections.put(agent_.gc_allocator, promise, operation) catch {};
        }
    }.func;

    try Realm.initializeHostDefinedRealm(&agent, .{});
    const realm = agent.currentRealm();
    try initializeGlobalObject(&agent, realm, realm.global_object);

    const cwd = try std.process.getCwdAlloc(allocator);
    defer allocator.free(cwd);
    std.debug.assert(std.fs.path.isAbsolute(cwd));

    if (path_arg) |path| {
        const source_text = try readFile(allocator, path);
        defer allocator.free(source_text);
        const resolved_path = try std.fs.path.resolve(allocator, &.{ cwd, path });
        defer allocator.free(resolved_path);
        std.debug.assert(std.fs.path.isAbsolute(resolved_path));
        const result = run(allocator, realm, source_text, .{
            .base_dir = std.fs.path.dirname(resolved_path).?,
            .origin = .{ .path = path },
            .module = parsed_args.options.module,
            .print_promise_rejection_warnings = parsed_args.options.@"print-promise-rejection-warnings",
        }) catch |err| switch (err) {
            error.AlreadyReported => return 1,
            else => return err,
        };
        if (parsed_args.options.@"print-result")
            try stdout.print("{pretty}\n", .{result});
    } else if (parsed_args.options.command) |source_text| {
        const result = run(allocator, realm, source_text, .{
            .base_dir = cwd,
            .origin = .command,
            .module = parsed_args.options.module,
            .print_promise_rejection_warnings = parsed_args.options.@"print-promise-rejection-warnings",
        }) catch |err| switch (err) {
            error.AlreadyReported => return 1,
            else => return err,
        };
        if (parsed_args.options.@"print-result")
            try stdout.print("{pretty}\n", .{result});
    } else {
        try repl(allocator, realm, .{
            .base_dir = cwd,
            .debug = parsed_args.options.debug,
            .module = parsed_args.options.module,
            .print_promise_rejection_warnings = parsed_args.options.@"print-promise-rejection-warnings",
        });
    }
    return 0;
}
