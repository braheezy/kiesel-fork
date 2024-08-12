const build_options = @import("build-options");
const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Agent = execution.Agent;
const InternalMethods = @import("InternalMethods.zig");
const Object = @import("../Object.zig");
const PrivateElement = types.PrivateElement;
const PrivateNameArrayHashMap = types.PrivateNameArrayHashMap;
const PropertyStorage = @import("PropertyStorage.zig");

tag: ?Object.Tag,

/// [[Prototype]]
prototype: ?Object,

/// [[Extensible]]
extensible: bool,

/// [[PrivateElements]]
private_elements: PrivateNameArrayHashMap(PrivateElement),

// [[IsHTMLDDA]]
is_htmldda: if (build_options.enable_annex_b) bool else void,

agent: *Agent,
internal_methods: InternalMethods,
property_storage: PropertyStorage,
