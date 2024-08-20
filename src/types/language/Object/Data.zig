const builtin = @import("builtin");
const std = @import("std");

const build_options = @import("build-options");
const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Agent = execution.Agent;
const InternalMethods = @import("InternalMethods.zig");
const Object = @import("../Object.zig");
const PrivateElement = types.PrivateElement;
const PrivateNameArrayHashMap = types.PrivateNameArrayHashMap;
const PropertyStorage = @import("PropertyStorage.zig");

tag: Object.Tag,

/// [[Prototype]]
prototype: ?Object,

/// [[Extensible]]
extensible: bool,

/// [[PrivateElements]]
private_elements: PrivateNameArrayHashMap(PrivateElement),

// [[IsHTMLDDA]]
is_htmldda: if (build_options.enable_annex_b) bool else void,

agent: *Agent,
internal_methods: *const InternalMethods,
property_storage: PropertyStorage,

comptime {
    // Let's make sure the size doesn't quietly change
    switch (builtin.target.ptrBitWidth()) {
        // Only some 32-bit platforms have certain bitpacking optimizations applied
        32 => std.debug.assert(@sizeOf(@This()) == 100 or @sizeOf(@This()) == 104),
        64 => switch (builtin.mode) {
            .ReleaseFast => std.debug.assert(@sizeOf(@This()) == 168),
            else => std.debug.assert(@sizeOf(@This()) == 192),
        },
        else => unreachable,
    }
}
