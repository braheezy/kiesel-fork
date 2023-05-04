const execution = @import("../../../execution.zig");

const Agent = execution.Agent;
const InternalMethods = @import("InternalMethods.zig");
const Object = @import("../Object.zig");
const PropertyStorage = @import("PropertyStorage.zig");

/// [[Prototype]]
prototype: ?Object,

/// [[Extensible]]
extensible: bool,

agent: *Agent,
internal_methods: InternalMethods,
property_storage: PropertyStorage,
