//! 9.4 Execution Contexts
//! https://tc39.es/ecma262/#sec-execution-contexts

const environments = @import("environments.zig");
const language = @import("../language.zig");
const types = @import("../types.zig");

const Environment = environments.Environment;
const Object = types.Object;
const PrivateEnvironment = environments.PrivateEnvironment;
const Realm = @import("Realm.zig");
const Script = language.Script;
const SourceTextModule = language.SourceTextModule;

pub const ScriptOrModule = union(enum) {
    script: *Script,
    module: *SourceTextModule,
};

/// Function
function: ?Object,

/// Realm
realm: *Realm,

/// ScriptOrModule
script_or_module: ?ScriptOrModule,

/// ECMAScript code execution contexts have the additional state components listed in Table 26.
/// https://tc39.es/ecma262/#ecmascript-code-execution-context
ecmascript_code: ?struct {
    /// LexicalEnvironment
    lexical_environment: Environment,

    /// VariableEnvironment
    variable_environment: Environment,

    /// PrivateEnvironment
    private_environment: ?*PrivateEnvironment,
} = null,
