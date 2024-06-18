const data_block = @import("spec/data_block.zig");
const iterator = @import("spec/iterator.zig");

pub const Arguments = @import("spec/Arguments.zig");
pub const ClassFieldDefinition = @import("spec/ClassFieldDefinition.zig");
pub const ClassStaticBlockDefinition = @import("spec/ClassStaticBlockDefinition.zig");
pub const Completion = @import("spec/Completion.zig");
pub const DataBlock = data_block.DataBlock;
pub const Iterator = iterator.Iterator;
pub const IteratorKind = iterator.IteratorKind;
pub const PrivateElement = @import("spec/private_element.zig").PrivateElement;
pub const PrivateMethodDefinition = @import("spec/PrivateMethodDefinition.zig");
pub const PrivateName = @import("spec/PrivateName.zig");
pub const PrivateNameArrayHashMap = PrivateName.PrivateNameArrayHashMap;
pub const PropertyDescriptor = @import("spec/PropertyDescriptor.zig");
pub const Reference = @import("spec/Reference.zig");
pub const copyDataBlockBytes = data_block.copyDataBlockBytes;
pub const createByteDataBlock = data_block.createByteDataBlock;
pub const createIterResultObject = iterator.createIterResultObject;
pub const createSharedByteDataBlock = data_block.createSharedByteDataBlock;
pub const getIterator = iterator.getIterator;
pub const getIteratorFromMethod = iterator.getIteratorFromMethod;
pub const data_block_max_byte_length = data_block.data_block_max_byte_length;

test {
    _ = data_block;
    _ = iterator;

    _ = Completion;
    _ = PropertyDescriptor;
    _ = Reference;
}
