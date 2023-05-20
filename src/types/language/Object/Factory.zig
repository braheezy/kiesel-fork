const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../../../execution.zig");

const Agent = execution.Agent;
const Data = @import("Data.zig");
const InternalMethods = @import("InternalMethods.zig");
const Object = @import("../Object.zig");

pub fn Factory(
    comptime options: struct {
        Fields: type = void,
        tag: ?Object.Tag = null,
    },
) type {
    const has_fields = options.Fields != void;

    // FIXME: Can we dedupe this?
    const Args = if (has_fields) struct {
        fields: options.Fields,
        prototype: ?Object,
        extensible: bool = true,
        internal_methods: InternalMethods = .{},
    } else struct {
        prototype: ?Object,
        extensible: bool = true,
        internal_methods: InternalMethods = .{},
    };

    return struct {
        const Self = @This();

        pub const Fields = options.Fields;

        fields: Fields,
        data: Data,

        pub fn create(agent: *Agent, args: Args) !Object {
            const self = try agent.gc_allocator.create(Self);
            self.* = .{
                .fields = if (has_fields) args.fields,
                .data = .{
                    .agent = agent,
                    .prototype = args.prototype,
                    .extensible = args.extensible,
                    .internal_methods = args.internal_methods,
                    .property_storage = Object.PropertyStorage.init(agent.gc_allocator),
                },
            };
            return self.object();
        }

        pub fn object(self: *Self) Object {
            return .{
                .ptr = self,
                .data = &self.data,
                .tag = options.tag,
            };
        }
    };
}
