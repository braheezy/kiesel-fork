//! 6.2.9 Data Blocks
//! https://tc39.es/ecma262/#sec-data-blocks

const std = @import("std");

const execution = @import("../../execution.zig");

const Agent = execution.Agent;

pub const DataBlock = std.ArrayList(u8);

/// 6.2.9.1 CreateByteDataBlock ( size )
/// https://tc39.es/ecma262/#sec-createbytedatablock
pub fn createByteDataBlock(agent: *Agent, size: u64) !DataBlock {
    // 1. If size > 2**53 - 1, throw a RangeError exception.
    if (size > std.math.maxInt(u53)) {
        return agent.throwException(.range_error, "Maximum buffer size exceeded");
    }

    const size_casted = std.math.cast(usize, size) orelse return agent.throwException(
        .range_error,
        try std.fmt.allocPrint(
            agent.gc_allocator,
            "Cannot allocate buffer of size {}",
            .{size},
        ),
    );

    // 2. Let db be a new Data Block value consisting of size bytes. If it is impossible to create
    //    such a Data Block, throw a RangeError exception.
    var data_block = DataBlock.initCapacity(agent.gc_allocator, size_casted) catch {
        return agent.throwException(
            .range_error,
            try std.fmt.allocPrint(
                agent.gc_allocator,
                "Cannot allocate buffer of size {}",
                .{size},
            ),
        );
    };

    // 3. Set all of the bytes of db to 0.
    var slice = data_block.addManyAsSliceAssumeCapacity(size_casted);
    @memset(slice, 0);

    // 4. Return db.
    return data_block;
}
