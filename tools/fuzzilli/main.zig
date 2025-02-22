const std = @import("std");

const kiesel = @import("kiesel");
const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const Realm = kiesel.execution.Realm;
const Script = kiesel.language.Script;
const String = kiesel.types.String;
const Value = kiesel.types.Value;
const defineBuiltinFunction = kiesel.utils.defineBuiltinFunction;

const REPRL_CRFD = 100;
const REPRL_CWFD = 101;
const REPRL_DRFD = 102;
const REPRL_DWFD = 103;
const REPRL_MAX_DATA_SIZE = 16 << 20;

extern fn __sanitizer_cov_reset_edgeguards() void;

// https://github.com/googleprojectzero/fuzzilli/tree/main/Targets#adding-custom-fuzzilli-javascript-builtin
fn fuzzilli(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
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
        const bytes = try str.toUtf8(agent.gc_allocator);
        defer agent.gc_allocator.free(bytes);
        const file: std.fs.File = .{ .handle = REPRL_DWFD };
        file.writer().print("{s}\n", .{bytes}) catch {};
    }
    return .undefined;
}

// https://github.com/googleprojectzero/fuzzilli/tree/main/Targets#reprl-psuedocode
fn reprl() !u8 {
    const gpa = std.heap.page_allocator;

    var helo: [4]u8 = "HELO".*;
    std.debug.assert(try std.posix.write(REPRL_CWFD, &helo) == 4);
    std.debug.assert(try std.posix.read(REPRL_CRFD, &helo) == 4);
    std.debug.assert(std.mem.eql(u8, &helo, "HELO"));

    const ptr = std.c.mmap(
        null,
        REPRL_MAX_DATA_SIZE,
        std.os.linux.PROT.READ | std.os.linux.PROT.WRITE,
        .{ .TYPE = .SHARED },
        REPRL_DRFD,
        0,
    );
    std.debug.assert(ptr != std.c.MAP_FAILED);

    while (true) {
        var action: [4]u8 = undefined;
        std.debug.assert(try std.posix.read(REPRL_CRFD, &action) == 4);
        std.debug.assert(std.mem.eql(u8, &action, "exec"));

        var data_size_bytes: [8]u8 = undefined;
        std.debug.assert(try std.posix.read(REPRL_CRFD, &data_size_bytes) == 8);
        const data_size = std.mem.bytesToValue(u64, &data_size_bytes);
        std.debug.assert(data_size <= REPRL_MAX_DATA_SIZE);

        // No need to allocate the null byte, Zig doesn't need it
        const data = try gpa.alloc(u8, data_size);
        defer gpa.free(data);
        @memcpy(data, @as([*]u8, @ptrCast(ptr))[0..data_size]);

        const result: u32 = blk: {
            var agent = try Agent.init(kiesel.gc.allocator(), .{});
            defer agent.deinit();

            try Realm.initializeHostDefinedRealm(&agent, .{});
            const realm = agent.currentRealm();

            try defineBuiltinFunction(realm.global_object, "fuzzilli", fuzzilli, 2, realm);

            const script = Script.parse(data, realm, null, .{}) catch break :blk 1;
            _ = script.evaluate() catch break :blk 1;
            break :blk 0;
        };

        const status: u32 = (result & 0xff) << 8;
        std.debug.assert(try std.posix.write(REPRL_CWFD, std.mem.asBytes(&status)) == 4);
        __sanitizer_cov_reset_edgeguards();
    }
}

pub fn main() u8 {
    return reprl() catch 1;
}
