const build_options = @import("build-options");
const execution = @import("../../../execution.zig");

const Agent = execution.Agent;
const InternalMethods = @import("InternalMethods.zig");
const Object = @import("../Object.zig");
const PropertyStorage = @import("PropertyStorage.zig");

/// [[Prototype]]
prototype: ?Object,

/// [[Extensible]]
extensible: bool,

// [[IsHTMLDDA]]
is_htmldda: if (build_options.enable_annex_b) bool else void,

agent: *Agent,
internal_methods: InternalMethods,
property_storage: PropertyStorage,
