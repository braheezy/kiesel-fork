const builtin = @import("builtin");
const std = @import("std");

const kiesel = @import("kiesel");
const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const String = kiesel.types.String;
const Value = kiesel.types.Value;

const coverage = @import("./coverage.zig");

const reprl_ctrl = struct {
    const in: std.Io.File = .{ .handle = coverage.REPRL_CRFD, .flags = .{ .nonblocking = false } };
    const out: std.Io.File = .{ .handle = coverage.REPRL_CWFD, .flags = .{ .nonblocking = false } };
};
const reprl_data = struct {
    const in: std.Io.File = .{ .handle = coverage.REPRL_DRFD, .flags = .{ .nonblocking = false } };
    const out: std.Io.File = .{ .handle = coverage.REPRL_DWFD, .flags = .{ .nonblocking = false } };
    const max_size = coverage.REPRL_MAX_DATA_SIZE;
};

const __sanitizer_cov_reset_edgeguards = coverage.__sanitizer_cov_reset_edgeguards;

// Usually provided by libFuzzer: https://github.com/llvm/llvm-project/blob/909212feecc197e469384924554087125ef1b7ea/compiler-rt/lib/fuzzer/FuzzerTracePC.cpp#L28
// Defined via inline assembly to avoid LLVM renaming the symbol to `__sancov_lowest_stack.1`
// when the sanitizer coverage pass collides with a Zig `export` in the same module.
comptime {
    switch (builtin.target.ofmt) {
        .elf => asm (std.fmt.comptimePrint(
                \\.globl __sancov_lowest_stack
                \\.section .tbss.__sancov_lowest_stack,"awT",@nobits
                \\__sancov_lowest_stack:
                \\.zero {[size]d}
                \\.size __sancov_lowest_stack, {[size]d}
            , .{ .size = @sizeOf(usize) })),
        .macho => asm (std.fmt.comptimePrint(
                \\.tbss ___sancov_lowest_stack$tlv$init, {[size]d}, {[alignment]d}
                \\.section __DATA,__thread_vars,thread_local_variables
                \\.globl ___sancov_lowest_stack
                \\___sancov_lowest_stack:
                \\.quad __tlv_bootstrap
                \\.quad 0
                \\.quad ___sancov_lowest_stack$tlv$init
            , .{ .size = @sizeOf(usize), .alignment = @ctz(@as(usize, @alignOf(usize))) })),
        else => unreachable,
    }
}

// https://github.com/googleprojectzero/fuzzilli/tree/main/Targets#adding-custom-fuzzilli-javascript-builtin
fn fuzzilli(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    const gpa = agent.gpa;
    const io = agent.io;
    // Don't throw as the fuzzed code itself might call this function
    if (arguments.count() != 2) return .undefined;
    const operation = try arguments.get(0).toString(agent);
    if (operation.eql(String.fromLiteral("FUZZILLI_CRASH"))) {
        const num = try arguments.get(1).toInt32(agent);
        switch (num) {
            0 => std.debug.assert(false),
            1 => @panic("crash"),
            2 => @as(*align(1) i32, @ptrFromInt(0x41414141)).* = 0x1337,
            else => unreachable,
        }
    } else if (operation.eql(String.fromLiteral("FUZZILLI_PRINT"))) {
        const str = try arguments.get(1).toString(agent);
        const bytes = try str.toUtf8(gpa);
        defer gpa.free(bytes);
        reprl_data.out.writeStreamingAll(io, bytes) catch {};
    }
    return .undefined;
}

// https://github.com/googleprojectzero/fuzzilli/tree/main/Targets#reprl-psuedocode
fn reprl(gpa: std.mem.Allocator, io: std.Io) !u8 {
    var platform: Agent.Platform = .default(io);
    defer platform.deinit();

    var helo: [4]u8 = "HELO".*;
    try reprl_ctrl.out.writeStreamingAll(io, &helo);
    std.debug.assert(try reprl_ctrl.in.readStreaming(io, &.{&helo}) == 4);
    std.debug.assert(std.mem.eql(u8, &helo, "HELO"));

    const memory_map = try reprl_data.in.createMemoryMap(io, .{ .len = reprl_data.max_size });

    while (true) {
        var action: [4]u8 = undefined;
        std.debug.assert(try reprl_ctrl.in.readStreaming(io, &.{&action}) == 4);
        std.debug.assert(std.mem.eql(u8, &action, "exec"));

        var data_size_bytes: [8]u8 = undefined;
        std.debug.assert(try reprl_ctrl.in.readStreaming(io, &.{&data_size_bytes}) == 8);
        const data_size = std.mem.bytesToValue(u64, &data_size_bytes);
        std.debug.assert(data_size <= reprl_data.max_size);

        // No need to allocate the null byte, Zig doesn't need it
        const data = try gpa.alloc(u8, data_size);
        defer gpa.free(data);
        @memcpy(data, memory_map.memory[0..data_size]);

        const result: u32 = blk: {
            var agent = try Agent.init(gpa, io, &platform, .{});
            defer agent.deinit();

            try Realm.initializeHostDefinedRealm(&agent, .{});
            const realm = agent.currentRealm();

            try realm.global_object.defineBuiltinFunction(&agent, "fuzzilli", fuzzilli, 2, realm);

            const script = Script.parse(data, realm, null, .{}) catch break :blk 1;
            _ = script.evaluate("fuzzilli") catch break :blk 1;
            break :blk 0;
        };

        const status: u32 = (result & 0xff) << 8;
        try reprl_ctrl.out.writeStreamingAll(io, @ptrCast(&status));
        __sanitizer_cov_reset_edgeguards();
    }
}

pub fn main(init: std.process.Init) u8 {
    const gpa = init.gpa;
    const io = init.io;
    return reprl(gpa, io) catch 1;
}
