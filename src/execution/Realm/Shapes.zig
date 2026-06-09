//! Pre-configured shapes for common objects.

const std = @import("std");

const builtins = @import("../../builtins.zig");
const execution = @import("../../execution.zig");
const types = @import("../../types.zig");

const Realm = execution.Realm;
const Object = types.Object;
const PropertyKey = types.PropertyKey;

const Shapes = @This();

realm: *Realm,
lazy_shapes: struct {
    array: ?*Object.Shape = null,
    array_iterator: ?*Object.Shape = null,
    ordinary_object: ?*Object.Shape = null,
    ordinary_function: ?*Object.Shape = null,
    ordinary_function_prototype: ?*Object.Shape = null,
    unmapped_arguments_object: ?*Object.Shape = null,
    mapped_arguments_object: ?*Object.Shape = null,
    reg_exp_object: ?*Object.Shape = null,
    reg_exp_exec_object: ?*Object.Shape = null,
} = .{},

pub fn init(realm: *Realm) Shapes {
    return .{ .realm = realm };
}

pub fn array(self: *Shapes) std.mem.Allocator.Error!*Object.Shape {
    const shape = self.lazy_shapes.array orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setInternalMethodsWithoutTransition(builtins.array.internal_methods);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%Array.prototype%"());
        self.lazy_shapes.array = shape;
        break :blk shape;
    };
    return shape;
}

pub fn arrayIterator(self: *Shapes) std.mem.Allocator.Error!*Object.Shape {
    const shape = self.lazy_shapes.array_iterator orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%ArrayIteratorPrototype%"());
        self.lazy_shapes.array_iterator = shape;
        break :blk shape;
    };
    return shape;
}

pub fn ordinaryObject(self: *Shapes) std.mem.Allocator.Error!*Object.Shape {
    const shape = self.lazy_shapes.ordinary_object orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%Object.prototype%"());
        self.lazy_shapes.ordinary_object = shape;
        break :blk shape;
    };
    return shape;
}

pub const OrdinaryFunctionOffsets = struct {
    length: Object.Shape.PropertyOffset,
    name: Object.Shape.PropertyOffset,
    prototype: Object.Shape.PropertyOffset,
};

pub fn ordinaryFunction(self: *Shapes) std.mem.Allocator.Error!struct {
    *Object.Shape,
    OrdinaryFunctionOffsets,
} {
    const shape = self.lazy_shapes.ordinary_function orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setInternalMethodsWithoutTransition(builtins.ecmascript_function.internal_methods_constructor);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%Function.prototype%"());
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("length"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
            .value,
        );
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("name"),
            .{
                .writable = false,
                .enumerable = false,
                .configurable = true,
            },
            .value,
        );
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("prototype"),
            .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
            .value,
        );
        self.lazy_shapes.ordinary_function = shape;
        break :blk shape;
    };
    const offsets: OrdinaryFunctionOffsets = .{
        .length = @enumFromInt(0),
        .name = @enumFromInt(1),
        .prototype = @enumFromInt(2),
    };
    return .{ shape, offsets };
}

pub const OrdinaryFunctionPrototypeOffsets = struct {
    constructor: Object.Shape.PropertyOffset,
};

pub fn ordinaryFunctionPrototype(self: *Shapes) std.mem.Allocator.Error!struct {
    *Object.Shape,
    OrdinaryFunctionPrototypeOffsets,
} {
    const shape = self.lazy_shapes.ordinary_function_prototype orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%Object.prototype%"());
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("constructor"),
            .builtin_default,
            .value,
        );
        self.lazy_shapes.ordinary_function_prototype = shape;
        break :blk shape;
    };
    const offsets: OrdinaryFunctionPrototypeOffsets = .{
        .constructor = @enumFromInt(0),
    };
    return .{ shape, offsets };
}

pub const UnmappedArgumentsObjectOffsets = struct {
    length: Object.Shape.PropertyOffset,
    @"%Symbol.iterator%": Object.Shape.PropertyOffset,
    callee: Object.Shape.PropertyOffset,
};

pub fn unmappedArgumentsObject(self: *Shapes) std.mem.Allocator.Error!struct {
    *Object.Shape,
    UnmappedArgumentsObjectOffsets,
} {
    const shape = self.lazy_shapes.unmapped_arguments_object orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%Object.prototype%"());
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("length"),
            .builtin_default,
            .value,
        );
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
            .builtin_default,
            .value,
        );
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("callee"),
            .none,
            .accessor,
        );
        self.lazy_shapes.unmapped_arguments_object = shape;
        break :blk shape;
    };
    const offsets: UnmappedArgumentsObjectOffsets = .{
        .length = @enumFromInt(0),
        .@"%Symbol.iterator%" = @enumFromInt(1),
        .callee = @enumFromInt(2),
    };
    return .{ shape, offsets };
}

pub const MappedArgumentsObjectOffsets = struct {
    length: Object.Shape.PropertyOffset,
    @"%Symbol.iterator%": Object.Shape.PropertyOffset,
    callee: Object.Shape.PropertyOffset,
};

pub fn mappedArgumentsObject(self: *Shapes) std.mem.Allocator.Error!struct {
    *Object.Shape,
    MappedArgumentsObjectOffsets,
} {
    const shape = self.lazy_shapes.mapped_arguments_object orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setInternalMethodsWithoutTransition(builtins.arguments.internal_methods);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%Object.prototype%"());
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("length"),
            .builtin_default,
            .value,
        );
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from(agent.well_known_symbols.@"%Symbol.iterator%"),
            .builtin_default,
            .value,
        );
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("callee"),
            .builtin_default,
            .value,
        );
        self.lazy_shapes.mapped_arguments_object = shape;
        break :blk shape;
    };
    const offsets: MappedArgumentsObjectOffsets = .{
        .length = @enumFromInt(0),
        .@"%Symbol.iterator%" = @enumFromInt(1),
        .callee = @enumFromInt(2),
    };
    return .{ shape, offsets };
}

pub const RegExpObjectOffsets = struct {
    lastIndex: Object.Shape.PropertyOffset,
};

pub fn regExpObject(self: *Shapes) std.mem.Allocator.Error!struct {
    *Object.Shape,
    RegExpObjectOffsets,
} {
    const shape = self.lazy_shapes.reg_exp_object orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%RegExp.prototype%"());
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("lastIndex"),
            .{
                .writable = true,
                .enumerable = false,
                .configurable = false,
            },
            .value,
        );
        self.lazy_shapes.reg_exp_object = shape;
        break :blk shape;
    };
    const offsets: RegExpObjectOffsets = .{
        .lastIndex = @enumFromInt(0),
    };
    return .{ shape, offsets };
}

pub const RegExpExecObjectOffsets = struct {
    index: Object.Shape.PropertyOffset,
    input: Object.Shape.PropertyOffset,
    groups: Object.Shape.PropertyOffset,
};

pub fn regExpExecObject(self: *Shapes) std.mem.Allocator.Error!struct {
    *Object.Shape,
    RegExpExecObjectOffsets,
} {
    const shape = self.lazy_shapes.reg_exp_exec_object orelse blk: {
        const realm = self.realm;
        const agent = realm.agent;
        const shape = try Object.Shape.init(agent.gc_allocator);
        shape.setInternalMethodsWithoutTransition(builtins.array.internal_methods);
        shape.setPrototypeWithoutTransition(try realm.intrinsics.@"%Array.prototype%"());
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("index"),
            .all,
            .value,
        );
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("input"),
            .all,
            .value,
        );
        try shape.setPropertyWithoutTransition(
            agent.gc_allocator,
            PropertyKey.from("groups"),
            .all,
            .value,
        );
        self.lazy_shapes.reg_exp_exec_object = shape;
        break :blk shape;
    };
    const offsets: RegExpExecObjectOffsets = .{
        .index = @enumFromInt(0),
        .input = @enumFromInt(1),
        .groups = @enumFromInt(2),
    };
    return .{ shape, offsets };
}
