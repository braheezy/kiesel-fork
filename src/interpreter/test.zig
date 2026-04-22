const std = @import("std");

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const interpreter = @import("../interpreter.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Agent = execution.Agent;
const Bytecode = interpreter.Bytecode;
const Ir = interpreter.Ir;
const Parser = language.Parser;
const Realm = execution.Realm;
const Script = language.Script;
const Value = types.Value;
const Vm = interpreter.Vm;

const ExpectedResult = union(enum) {
    value: ?LiteralValue,
    promise: struct { fulfilled: LiteralValue },
    exception,
    ignore,

    const LiteralValue = union(enum) {
        undefined,
        number: i32,
        string: []const u8,

        fn toValue(comptime self: LiteralValue) Value {
            return switch (self) {
                .undefined => .undefined,
                .number => |number| Value.from(number),
                .string => |string| Value.from(string),
            };
        }
    };
};

const TestCase = struct {
    source: []const u8,
    expected_result: ExpectedResult,
    expected_ir: ?[]const u8,
    expected_bc: ?[]const u8,
};

const test_cases: []const TestCase = @import("test_cases.zon");

fn testInterpreter(
    gpa: std.mem.Allocator,
    io: std.Io,
    environ_map: *const std.process.Environ.Map,
    source: []const u8,
    comptime expected_result: ExpectedResult,
    expected_ir: ?[]const u8,
    expected_bc: ?[]const u8,
) !void {
    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const script = try Parser.parse(language.ast.Script, arena, source, .{});

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

    const platform: Agent.Platform = .default(io, environ_map);
    defer platform.deinit();
    var agent: Agent = try .init(gpa, io, &platform, .{});
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

    const test_context = try agent.gc_allocator.create(execution.ExecutionContext);
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

    var vm: Vm = try .init(&agent, &bc);
    defer vm.deinit();

    // Agent.Error!RunResult -> Agent.Error!?Value
    const unwrapped = if (vm.run(.{})) |result| switch (result) {
        .@"return" => |value| value,
        .yield => unreachable,
    } else |err| err;

    if (unwrapped) |result| switch (expected_result) {
        .value => |expected| if (expected != null) {
            if (result == null) return error.TestExpectedEqual;
            if (!expected.?.toValue().isStrictlyEqual(result.?)) return error.TestExpectedEqual;
        } else if (result != null) return error.TestExpectedEqual,
        .promise => |expected| {
            agent.drainJobQueue();
            const promise = (result orelse return error.TestExpectedEqual).asObject().as(builtins.Promise);
            try std.testing.expectEqual(.fulfilled, promise.fields.promise_state);
            if (!expected.fulfilled.toValue().isStrictlyEqual(promise.fields.promise_result)) return error.TestExpectedEqual;
        },
        .exception => return error.TestExpectedException,
        .ignore => {},
    } else |err| switch (expected_result) {
        .exception => try std.testing.expectEqual(error.ExceptionThrown, err),
        else => return err,
    }

    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();

    const terminal: std.Io.Terminal = .{
        .writer = &aw.writer,
        .mode = .no_color,
    };

    if (expected_ir) |expected| {
        try ir.print(terminal);
        try std.testing.expectEqualStrings(expected, aw.written());
        aw.clearRetainingCapacity();
    }

    if (expected_bc) |expected| {
        try bc.print(terminal);
        try std.testing.expectEqualStrings(expected, aw.written());
        aw.clearRetainingCapacity();
    }
}

test {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var environ_map = try std.process.Environ.createMap(.empty, gpa);
    defer environ_map.deinit();
    inline for (test_cases) |test_case| {
        try testInterpreter(
            gpa,
            io,
            &environ_map,
            test_case.source,
            test_case.expected_result,
            test_case.expected_ir,
            test_case.expected_bc,
        );
    }
}
