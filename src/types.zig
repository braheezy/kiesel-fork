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
pub const PrivateNameArrayHashMap = spec.PrivateNameArrayHashMap;
pub const PropertyDescriptor = spec.PropertyDescriptor;
pub const PropertyKey = Object.PropertyKey;
pub const PropertyKeyOrPrivateName = union(enum) { property_key: PropertyKey, private_name: PrivateName };
pub const Reference = spec.Reference;
pub const String = language.String;
pub const StringArrayHashMap = language.StringArrayHashMap;
pub const StringHashMap = language.StringHashMap;
pub const Symbol = language.Symbol;
pub const Value = language.Value;
pub const ValueArrayHashMap = language.ValueArrayHashMap;
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
pub const getOption = language.getOption;
pub const isLessThan = language.isLessThan;
pub const isLooselyEqual = language.isLooselyEqual;
pub const isStrictlyEqual = language.isStrictlyEqual;
pub const sameType = language.sameType;
pub const sameValue = language.sameValue;
pub const sameValueZero = language.sameValueZero;
pub const data_block_max_byte_length = spec.data_block_max_byte_length;

pub const SafePointer = @import("any-pointer").SafePointer;

test {
    _ = language;
    _ = spec;
}
