const data_block = @import("spec/data_block.zig");
const iterator = @import("spec/iterator.zig");

pub const ClassFieldDefinition = @import("spec/ClassFieldDefinition.zig");
pub const Completion = @import("spec/Completion.zig");
pub const DataBlock = data_block.DataBlock;
pub const Iterator = iterator.Iterator;
pub const PropertyDescriptor = @import("spec/PropertyDescriptor.zig");
pub const Reference = @import("spec/Reference.zig");
pub const copyDataBlockBytes = data_block.copyDataBlockBytes;
pub const createByteDataBlock = data_block.createByteDataBlock;
pub const createIterResultObject = iterator.createIterResultObject;
pub const getIterator = iterator.getIterator;
pub const getIteratorFromMethod = iterator.getIteratorFromMethod;

test {
    _ = data_block;
    _ = iterator;

    _ = Completion;
    _ = PropertyDescriptor;
    _ = Reference;
}
