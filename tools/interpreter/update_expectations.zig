const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Parser = kiesel.language.Parser;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;

const Edit = struct {
    span: std.zig.Ast.Span,
    replacement: []const u8,
};

const TestCase = struct {
    source: std.zig.Zoir.Node.Index,
    expected_result: std.zig.Zoir.Node.Index,
    expected_output: std.zig.Zoir.Node.Index,
};

pub fn main(init: std.process.Init) !u8 {
    const gpa = init.gpa;
    const io = init.io;

    const args = try init.minimal.args.toSlice(gpa);
    defer gpa.free(args);

    if (args.len != 2) {
        std.debug.print("Usage: {s} <path/to/test_cases.zon>\n", .{args[0]});
        return 1;
    }
    const zon_path = args[1];

    const zon_source = try std.Io.Dir.cwd().readFileAllocOptions(
        io,
        zon_path,
        gpa,
        .limited(8 * 1024 * 1024),
        .of(u8),
        0,
    );
    defer gpa.free(zon_source);

    var diag: std.zon.parse.Diagnostics = .{};
    defer diag.deinit(gpa);

    const test_cases = try std.zon.parse.fromSliceAlloc(
        []const TestCase,
        gpa,
        zon_source,
        &diag,
        .{},
    );
    defer std.zon.parse.free(gpa, test_cases);

    var edits: std.ArrayList(Edit) = .empty;
    defer {
        for (edits.items) |edit| gpa.free(edit.replacement);
        edits.deinit(gpa);
    }

    for (test_cases, 0..) |test_case, i| {
        const expected_output_ast_node = test_case.expected_output.getAstNode(diag.zoir);

        const script_source = try std.zon.parse.fromZoirNodeAlloc(
            []const u8,
            gpa,
            diag.ast,
            diag.zoir,
            test_case.source,
            null,
            .{},
        );
        defer gpa.free(script_source);

        const output = generateExpectedOutput(gpa, io, script_source) catch |err| {
            std.debug.print("Failed to generate expected output for case {d}: {t}\n", .{ i, err });
            return 1;
        };
        defer gpa.free(output);

        const span = nodeSpan(diag.ast, zon_source, expected_output_ast_node);
        const replacement = try formatMultilineLiteral(gpa, output);
        try edits.append(gpa, .{
            .span = span,
            .replacement = replacement,
        });
    }

    var rewritten: std.ArrayList(u8) = .empty;
    defer rewritten.deinit(gpa);
    try rewritten.appendSlice(gpa, zon_source);

    while (edits.pop()) |edit| {
        try rewritten.replaceRange(
            gpa,
            edit.span.start,
            edit.span.end - edit.span.start,
            edit.replacement,
        );
        gpa.free(edit.replacement);
    }

    if (!std.mem.eql(u8, zon_source, rewritten.items)) {
        var atomic_file = try std.Io.Dir.cwd().createFileAtomic(io, zon_path, .{ .replace = true });
        defer atomic_file.deinit(io);
        var write_buffer: [4096]u8 = undefined;
        var file_writer = atomic_file.file.writer(io, &write_buffer);
        try file_writer.interface.writeAll(rewritten.items);
        try file_writer.flush();
        try atomic_file.replace(io);
    }

    return 0;
}

fn generateExpectedOutput(gpa: std.mem.Allocator, io: std.Io, source: []const u8) ![]const u8 {
    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();

    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const script = try Parser.parse(kiesel.language.ast.Script, arena, source, .{});

    var environ_map = try std.process.Environ.createMap(.empty, gpa);
    defer environ_map.deinit();

    var platform: Agent.Platform = .default(io, &environ_map);
    defer platform.deinit();
    platform.stdout = &aw.writer;
    platform.stderr = &aw.writer;
    platform.terminal_mode = .no_color;

    var agent: Agent = try .init(gpa, io, &platform, .{
        .debug = .{
            .print_ir = true,
            .print_bytecode = true,
        },
    });
    defer agent.deinit();

    try Realm.initializeHostDefinedRealm(&agent, .{});

    const realm = agent.currentRealm();
    const script_record = try agent.gc_allocator.create(Script);
    script_record.* = .{
        .realm = realm,
        .ecmascript_code = script,
        .loaded_modules = .empty,
        .host_defined = null,
        .source = source,
    };

    const test_context = try agent.gc_allocator.create(kiesel.execution.ExecutionContext);
    test_context.* = .{
        .origin = .script,
        .realm = realm,
        .script_or_module = .{ .script = script_record },
        .ecmascript_code = .{
            .variable_environment = .{ .global_environment = realm.global_env },
            .lexical_environment = .{ .global_environment = realm.global_env },
            .private_environment = null,
        },
    };
    try agent.execution_context_stack.append(agent.gc_allocator, test_context);
    defer _ = agent.execution_context_stack.pop().?;

    try Script.globalDeclarationInstantiation(&agent, script, realm.global_env, source);

    _ = try kiesel.interpreter.compileAndRun(&agent, .{ .script = &script }, "test");
    return aw.toOwnedSlice();
}

fn formatMultilineLiteral(
    gpa: std.mem.Allocator,
    text: []const u8,
) std.mem.Allocator.Error![]const u8 {
    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();

    var it = std.mem.splitScalar(u8, text, '\n');
    aw.writer.print("\\\\{s}", .{it.first()}) catch |err| switch (err) {
        error.WriteFailed => return error.OutOfMemory,
    };
    while (it.next()) |line| {
        aw.writer.print("\n        \\\\{s}", .{line}) catch |err| switch (err) {
            error.WriteFailed => return error.OutOfMemory,
        };
    }
    aw.writer.writeAll("\n        ") catch |err| switch (err) {
        error.WriteFailed => return error.OutOfMemory,
    };

    return aw.toOwnedSlice();
}

fn nodeSpan(ast: std.zig.Ast, source: []const u8, node: std.zig.Ast.Node.Index) std.zig.Ast.Span {
    const first_token = ast.firstToken(node);
    const last_token = ast.lastToken(node);
    const main_token = ast.nodeMainToken(node);

    var end = ast.tokenStart(last_token) + @as(u32, @intCast(ast.tokenSlice(last_token).len));
    while (end < source.len and std.ascii.isWhitespace(source[end])) {
        end += 1;
    }

    return .{
        .start = ast.tokenStart(first_token),
        .end = end,
        .main = ast.tokenStart(main_token),
    };
}
