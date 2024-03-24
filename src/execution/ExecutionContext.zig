//! 9.4 Execution Contexts
//! https://tc39.es/ecma262/#sec-execution-contexts

const builtins = @import("../builtins.zig");
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

/// Table 26: Additional State Components for ECMAScript Code Execution Contexts
/// https://tc39.es/ecma262/#table-additional-state-components-for-ecmascript-code-execution-contexts
ecmascript_code: ?struct {
    /// LexicalEnvironment
    lexical_environment: Environment,

    /// VariableEnvironment
    variable_environment: Environment,

    /// PrivateEnvironment
    private_environment: ?*PrivateEnvironment,
} = null,

/// Table 27: Additional State Components for Generator Execution Contexts
/// https://tc39.es/ecma262/#table-additional-state-components-for-generator-execution-contexts
generator: ?union {
    generator: *builtins.Generator,
    async_generator: *builtins.AsyncGenerator,
} = null,
