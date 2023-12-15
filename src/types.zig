pub const language = @import("types/language.zig");
pub const spec = @import("types/spec.zig");

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
pub const PropertyDescriptor = spec.PropertyDescriptor;
pub const PropertyKey = Object.PropertyKey;
pub const Reference = spec.Reference;
pub const String = language.String;
pub const Symbol = language.Symbol;
pub const Value = language.Value;
pub const ValueHashMap = language.ValueHashMap;
pub const coerceOptionsToObject = language.coerceOptionsToObject;
pub const copyDataBlockBytes = spec.copyDataBlockBytes;
pub const createArrayFromList = language.createArrayFromList;
pub const createArrayFromListMapToValue = language.createArrayFromListMapToValue;
pub const createByteDataBlock = spec.createByteDataBlock;
pub const createIterResultObject = spec.createIterResultObject;
pub const getIterator = spec.getIterator;
pub const getIteratorFromMethod = spec.getIteratorFromMethod;
pub const getOption = language.getOption;
pub const isLessThan = language.isLessThan;
pub const isLooselyEqual = language.isLooselyEqual;
pub const isStrictlyEqual = language.isStrictlyEqual;
pub const sameValue = language.sameValue;
pub const sameValueZero = language.sameValueZero;

test {
    _ = language;
    _ = spec;
}
