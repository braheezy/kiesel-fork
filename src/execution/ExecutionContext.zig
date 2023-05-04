//! 9.4 Execution Contexts
//! https://tc39.es/ecma262/#sec-execution-contexts

const language = @import("../language.zig");
const types = @import("../types.zig");

const Object = types.Object;
const Realm = @import("Realm.zig");
const Script = language.Script;

const Module = struct {};

pub const ScriptOrModule = union(enum) {
    script: *Script,
    module: *Module,
};

/// Function
function: ?Object,

/// Realm
realm: *Realm,

/// ScriptOrModule
script_or_module: ?ScriptOrModule,
