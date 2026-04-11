const std = @import("std");

const kiesel = @import("kiesel");

const Bytecode = kiesel.interpreter.Bytecode;
const Ir = kiesel.interpreter.Ir;
const Parser = kiesel.language.Parser;

const Edit = struct {
    span: std.zig.Ast.Span,
    replacement: []const u8,
};

const GeneratedExpectations = struct {
    ir: []const u8,
    bc: []const u8,
};

const TestCase = struct {
    source: std.zig.Zoir.Node.Index,
    expected_result: std.zig.Zoir.Node.Index,
    expected_ir: std.zig.Zoir.Node.Index,
    expected_bc: std.zig.Zoir.Node.Index,
};

pub fn main() !u8 {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer _ = debug_allocator.deinit();
    const gpa = debug_allocator.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len != 2) {
        std.debug.print("Usage: {s} <path/to/test_cases.zon>\n", .{args[0]});
        return 1;
    }
    const zon_path = args[1];

    const zon_source = try std.fs.cwd().readFileAllocOptions(
        gpa,
        zon_path,
        8 * 1024 * 1024,
        null,
        .of(u8),
        0,
    );
    defer gpa.free(zon_source);

    var diag: std.zon.parse.Diagnostics = .{};
    defer diag.deinit(gpa);

    const test_cases = try std.zon.parse.fromSlice(
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
        const expected_ir_ast_node = switch (test_case.expected_ir.get(diag.zoir)) {
            .null => null,
            else => test_case.expected_ir.getAstNode(diag.zoir),
        };
        const expected_bc_ast_node = switch (test_case.expected_bc.get(diag.zoir)) {
            .null => null,
            else => test_case.expected_bc.getAstNode(diag.zoir),
        };

        if (expected_bc_ast_node == null and expected_ir_ast_node == null) {
            continue;
        }

        const script_source = try std.zon.parse.fromZoirNode(
            []const u8,
            gpa,
            diag.ast,
            diag.zoir,
            test_case.source,
            null,
            .{},
        );
        defer gpa.free(script_source);

        const generated_expectations = generateExpectations(gpa, script_source) catch |err| {
            std.debug.print("Failed to generate expectations for case {d}: {t}\n", .{ i, err });
            return 1;
        };
        defer {
            gpa.free(generated_expectations.ir);
            gpa.free(generated_expectations.bc);
        }

        if (expected_ir_ast_node) |node| {
            const span = nodeSpan(diag.ast, zon_source, node);
            const replacement = try formatMultilineLiteral(gpa, generated_expectations.ir);
            try edits.append(gpa, .{
                .span = span,
                .replacement = replacement,
            });
        }

        if (expected_bc_ast_node) |ast_node| {
            const span = nodeSpan(diag.ast, zon_source, ast_node);
            const replacement = try formatMultilineLiteral(gpa, generated_expectations.bc);
            try edits.append(gpa, .{
                .span = span,
                .replacement = replacement,
            });
        }
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
        var write_buffer: [4096]u8 = undefined;
        var atomic_file = try std.fs.cwd().atomicFile(zon_path, .{ .write_buffer = &write_buffer });
        defer atomic_file.deinit();
        try atomic_file.file_writer.interface.writeAll(rewritten.items);
        try atomic_file.finish();
    }

    return 0;
}

fn generateExpectations(gpa: std.mem.Allocator, source: []const u8) !GeneratedExpectations {
    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const script = try Parser.parse(kiesel.language.ast.Script, arena, source, .{});

    var ir = ir: {
        var builder: Ir.Builder = .init(gpa, "test", .{ .script = &script });
        defer builder.deinit();
        break :ir try builder.build();
    };
    defer ir.deinit(gpa);

    var bc = bc: {
        var builder: Bytecode.Builder = try .init(gpa, &ir);
        defer builder.deinit();
        break :bc try builder.build();
    };
    defer bc.deinit(gpa);

    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();

    try ir.print(&aw.writer, .no_color);
    const ir_out = try aw.toOwnedSlice();

    try bc.print(&aw.writer, .no_color);
    const bc_out = try aw.toOwnedSlice();

    return .{ .ir = ir_out, .bc = bc_out };
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
