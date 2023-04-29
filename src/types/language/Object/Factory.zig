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
        // FIXME: `internal_methods: InternalMethods = .{}` would be nicer but causes a 'depends on itself' error
        internal_methods: ?InternalMethods = null,
    },
) type {
    const has_fields = options.Fields != void;

    const Args = if (has_fields) struct {
        fields: options.Fields,
        prototype: ?Object,
    } else struct {
        prototype: ?Object,
    };

    return struct {
        const Self = @This();

        fields: options.Fields,
        data: Data,

        pub fn create(agent: *Agent, args: Args) !*Self {
            const self = try agent.allocator.create(Self);
            self.* = .{
                .fields = if (has_fields) args.fields,
                .data = .{
                    .agent = agent,
                    .prototype = args.prototype,
                    .internal_methods = options.internal_methods orelse .{},
                    .property_storage = Object.PropertyStorage.init(agent.allocator),
                },
            };
            return self;
        }

        pub fn object(self: *Self) Object {
            return .{
                .ptr = self,
                .data = &self.data,
            };
        }
    };
}
