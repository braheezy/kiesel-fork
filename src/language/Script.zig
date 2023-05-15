//! 16.1.4 Script Records
//! https://tc39.es/ecma262/#sec-script-records

const std = @import("std");

const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const bytecode = @import("bytecode.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");

const Executable = bytecode.Executable;
const Parser = @import("Parser.zig");
const Realm = execution.Realm;
const Value = types.Value;
const Vm = bytecode.Vm;

const Self = @This();

/// [[Realm]]
realm: *Realm,

/// [[ECMAScriptCode]]
ecmascript_code: ast.Script,

// TODO: [[LoadedModules]]

/// [[HostDefined]]
host_defined: ?*anyopaque = null,

/// 16.1.5 ParseScript ( sourceText, realm, hostDefined )
/// https://tc39.es/ecma262/#sec-parse-script
pub fn parse(
    source_text: []const u8,
    realm: *Realm,
    host_defined: ?*anyopaque,
    ctx: Parser.Context,
) !*Self {
    const agent = realm.agent;

    // 1. Let script be ParseText(sourceText, Script).
    // 2. If script is a List of errors, return script.
    const script = try Parser.parse(ast.Script, agent.allocator, source_text, ctx);

    // 3. Return Script Record {
    //      [[Realm]]: realm, [[ECMAScriptCode]]: script, [[LoadedModules]]: « », [[HostDefined]]: hostDefined
    //    }.
    var self = try agent.allocator.create(Self);
    self.* = .{
        .realm = realm,
        .ecmascript_code = script,
        .host_defined = host_defined,
    };
    return self;
}

/// 16.1.6 ScriptEvaluation ( scriptRecord )
/// https://tc39.es/ecma262/#sec-runtime-semantics-scriptevaluation
pub fn evaluate(self: Self) !Value {
    const agent = self.realm.agent;

    var executable = Executable.init(agent.allocator);
    defer executable.deinit();

    var vm = try Vm.init(agent);
    defer vm.deinit();

    try self.ecmascript_code.generateBytecode(&executable);

    if (agent.options.debug.print_bytecode) {
        const stdout = std.io.getStdOut().writer();
        try executable.print(stdout);
    }

    return vm.run(executable);
}
