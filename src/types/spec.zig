const iterator = @import("spec/iterator.zig");

pub const Completion = @import("spec/Completion.zig");
pub const Iterator = iterator.Iterator;
pub const PropertyDescriptor = @import("spec/PropertyDescriptor.zig");
pub const Reference = @import("spec/Reference.zig");
pub const createIterResultObject = iterator.createIterResultObject;
pub const getIterator = iterator.getIterator;
pub const getIteratorFromMethod = iterator.getIteratorFromMethod;

test {
    _ = iterator;

    _ = Completion;
    _ = PropertyDescriptor;
    _ = Reference;
}
