const std = @import("std");

const AnyPointer = @import("any-pointer").AnyPointer;

const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Agent = execution.Agent;
const Data = @import("Data.zig");
const InternalMethods = @import("InternalMethods.zig");
const Object = types.Object;
const PrivateElement = types.PrivateElement;
const PrivateNameArrayHashMap = types.PrivateNameArrayHashMap;

pub fn MakeObject(
    comptime options: struct {
        Fields: type = void,
        tag: Object.Tag = .unset,
    },
) type {
    const has_fields = options.Fields != void;
    const has_is_htmldda = std.meta.fieldInfo(Data, .is_htmldda).type != void;

    // FIXME: Can we dedupe this?
    const Args = if (has_fields and has_is_htmldda) struct {
        fields: options.Fields,
        prototype: ?Object,
        extensible: bool = true,
        is_htmldda: bool = false,
        internal_methods: *const InternalMethods = &.{},
    } else if (has_fields) struct {
        fields: options.Fields,
        prototype: ?Object,
        extensible: bool = true,
        internal_methods: *const InternalMethods = &.{},
    } else if (has_is_htmldda) struct {
        prototype: ?Object,
        extensible: bool = true,
        is_htmldda: bool = false,
        internal_methods: *const InternalMethods = &.{},
    } else struct {
        prototype: ?Object,
        extensible: bool = true,
        internal_methods: *const InternalMethods = &.{},
    };

    return struct {
        pub const Fields = options.Fields;
        pub const tag = options.tag;

        fields: Fields,
        data: Data,

        pub fn create(agent: *Agent, args: Args) std.mem.Allocator.Error!Object {
            const self = try agent.gc_allocator.create(@This());
            self.* = .{
                .fields = if (has_fields) args.fields,
                .data = .{
                    .tag = options.tag,
                    .agent = agent,
                    .prototype = args.prototype,
                    .extensible = args.extensible,
                    .private_elements = PrivateNameArrayHashMap(PrivateElement).init(agent.gc_allocator),
                    .internal_methods = args.internal_methods,
                    .property_storage = Object.PropertyStorage.init(agent.gc_allocator),
                    .is_htmldda = if (has_is_htmldda) args.is_htmldda,
                },
            };
            return self.object();
        }

        pub fn object(self: *@This()) Object {
            return .{
                .data = &self.data,
            };
        }
    };
}
