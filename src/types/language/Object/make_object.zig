const std = @import("std");

const execution = @import("../../../execution.zig");
const types = @import("../../../types.zig");

const Agent = execution.Agent;
const InternalMethods = @import("InternalMethods.zig");
const Object = types.Object;

pub fn MakeObject(
    comptime options: struct {
        Fields: type = void,
        tag: Object.Tag = .unset,
    },
) type {
    const has_fields = options.Fields != void;
    const has_is_htmldda = @FieldType(Object.Shape, "is_htmldda") != void;

    // FIXME: Can we dedupe this?
    const Args = if (has_fields and has_is_htmldda) struct {
        fields: options.Fields,
        prototype: ?*Object,
        extensible: bool = true,
        is_htmldda: bool = false,
        internal_methods: *const InternalMethods = &.{},
    } else if (has_fields) struct {
        fields: options.Fields,
        prototype: ?*Object,
        extensible: bool = true,
        internal_methods: *const InternalMethods = &.{},
    } else if (has_is_htmldda) struct {
        prototype: ?*Object,
        extensible: bool = true,
        is_htmldda: bool = false,
        internal_methods: *const InternalMethods = &.{},
    } else struct {
        prototype: ?*Object,
        extensible: bool = true,
        internal_methods: *const InternalMethods = &.{},
    };

    return struct {
        pub const Fields = options.Fields;
        pub const tag = options.tag;

        fields: Fields,
        object: Object,

        pub fn create(agent: *Agent, args: Args) std.mem.Allocator.Error!*Object {
            const self = try agent.gc_allocator.create(@This());
            errdefer agent.gc_allocator.destroy(self);
            self.* = .{
                .fields = if (has_fields) args.fields,
                .object = .{
                    .tag = options.tag,
                    .agent = agent,
                    .internal_methods = args.internal_methods,
                    .property_storage = .{
                        .shape = agent.empty_shape,
                        .values = .empty,
                        .accessors = .empty,
                        .indexed_properties = .empty,
                        .lazy_properties = .empty,
                        .private_elements = .empty,
                    },
                },
            };
            if (args.prototype != null) {
                try self.object.setPrototype(args.prototype);
            }
            if (!args.extensible) {
                try self.object.setNonExtensible();
            }
            if (has_is_htmldda and args.is_htmldda) {
                try self.object.setIsHTMLDDA();
            }
            return &self.object;
        }
    };
}
