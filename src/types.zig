const std = @import("std");

const language = @import("types/language.zig");
const spec = @import("types/spec.zig");

pub const Arguments = spec.Arguments;
pub const BigInt = language.BigInt;
pub const ClassFieldDefinition = spec.ClassFieldDefinition;
pub const ClassStaticBlockDefinition = spec.ClassStaticBlockDefinition;
pub const Completion = spec.Completion;
pub const DataBlock = spec.DataBlock;
pub const Iterator = spec.Iterator;
pub const IteratorKind = spec.IteratorKind;
pub const MakeObject = language.MakeObject;
pub const Number = language.Number;
pub const Object = language.Object;
pub const PreferredType = Value.PreferredType;
pub const PrivateElement = spec.PrivateElement;
pub const PrivateMethodDefinition = spec.PrivateMethodDefinition;
pub const PrivateName = spec.PrivateName;
pub const PropertyDescriptor = spec.PropertyDescriptor;
pub const PropertyKey = Object.PropertyKey;
pub const PropertyKeyOrPrivateName = union(enum) { property_key: PropertyKey, private_name: PrivateName };
pub const Reference = spec.Reference;
pub const String = language.String;
pub const Symbol = language.Symbol;
pub const Value = language.Value;
pub const copyDataBlockBytes = spec.copyDataBlockBytes;
pub const createArrayFromList = language.createArrayFromList;
pub const createArrayFromListMapToValue = language.createArrayFromListMapToValue;
pub const createByteDataBlock = spec.createByteDataBlock;
pub const createIteratorResultObject = spec.createIteratorResultObject;
pub const createSharedByteDataBlock = spec.createSharedByteDataBlock;
pub const getIterator = spec.getIterator;
pub const getIteratorDirect = spec.getIteratorDirect;
pub const getIteratorFlattenable = spec.getIteratorFlattenable;
pub const getIteratorFromMethod = spec.getIteratorFromMethod;
pub const isLessThan = language.isLessThan;
pub const isLooselyEqual = language.isLooselyEqual;
pub const isStrictlyEqual = language.isStrictlyEqual;
pub const sameType = language.sameType;
pub const sameValue = language.sameValue;
pub const sameValueZero = language.sameValueZero;

pub const SafePointer = @import("any-pointer").SafePointer;

pub const ByteLength = enum(u53) {
    zero = 0,
    _,

    pub fn toAuto(self: ByteLength) AutoByteLength {
        const result: AutoByteLength = @enumFromInt(@intFromEnum(self));
        std.debug.assert(result != .auto);
        return result;
    }

    pub fn toDetached(self: ByteLength) DetachedByteLength {
        const result: DetachedByteLength = @enumFromInt(@intFromEnum(self));
        std.debug.assert(result != .detached);
        return result;
    }

    pub fn toOptional(self: ByteLength) OptionalByteLength {
        const result: OptionalByteLength = @enumFromInt(@intFromEnum(self));
        std.debug.assert(result != .none);
        return result;
    }
};

pub const AutoByteLength = enum(u54) {
    auto = std.math.maxInt(u53) + 1,
    zero = 0,
    _,

    pub fn unwrap(self: AutoByteLength) ?ByteLength {
        return if (self == .auto) null else @enumFromInt(@intFromEnum(self));
    }
};

pub const DetachedByteLength = enum(u54) {
    detached = std.math.maxInt(u53) + 1,
    zero = 0,
    _,

    pub fn unwrap(self: DetachedByteLength) ?ByteLength {
        return if (self == .detached) null else @enumFromInt(@intFromEnum(self));
    }
};

pub const OptionalByteLength = enum(u54) {
    none = std.math.maxInt(u53) + 1,
    zero = 0,
    _,

    pub fn unwrap(self: OptionalByteLength) ?ByteLength {
        return if (self == .none) null else @enumFromInt(@intFromEnum(self));
    }
};

pub const ArrayLength = enum(u53) {
    zero = 0,
    _,

    pub fn toAuto(self: ArrayLength) AutoArrayLength {
        const result: AutoArrayLength = @enumFromInt(@intFromEnum(self));
        std.debug.assert(result != .auto);
        return result;
    }

    pub fn toOptional(self: ArrayLength) OptionalArrayLength {
        const result: OptionalArrayLength = @enumFromInt(@intFromEnum(self));
        std.debug.assert(result != .none);
        return result;
    }
};

pub const AutoArrayLength = enum(u54) {
    auto = std.math.maxInt(u53) + 1,
    zero = 0,
    _,

    pub fn unwrap(self: AutoArrayLength) ?ArrayLength {
        return if (self == .auto) null else @enumFromInt(@intFromEnum(self));
    }
};

pub const OptionalArrayLength = enum(u54) {
    none = std.math.maxInt(u53) + 1,
    zero = 0,
    _,

    pub fn unwrap(self: OptionalArrayLength) ?ArrayLength {
        return if (self == .none) null else @enumFromInt(@intFromEnum(self));
    }
};

pub const ByteOffset = enum(u53) {
    zero = 0,
    _,
};

test {
    _ = language;
    _ = spec;
}
