//! 6.2.9 Data Blocks
//! https://tc39.es/ecma262/#sec-data-blocks

const std = @import("std");

const execution = @import("../../execution.zig");

const Agent = execution.Agent;

pub const DataBlock = std.ArrayList(u8);

/// Arbitrary size limit (32 GiB)
pub const data_block_max_byte_length = 1024 * 1024 * 1024 * 32;

/// 6.2.9.1 CreateByteDataBlock ( size )
/// https://tc39.es/ecma262/#sec-createbytedatablock
pub fn createByteDataBlock(agent: *Agent, size: u64) Agent.Error!DataBlock {
    // 1. If size > 2**53 - 1, throw a RangeError exception.
    comptime std.debug.assert(data_block_max_byte_length <= std.math.maxInt(u53));

    // NOTE: Checking for a reasonable size below the theoretical limit is non-standard but also
    //       done in other engines (and tested by test262)
    if (size > data_block_max_byte_length) {
        return agent.throwException(.range_error, "Cannot allocate buffer of size {d}", .{size});
    }

    const size_casted = std.math.cast(usize, size) orelse {
        return agent.throwException(.range_error, "Cannot allocate buffer of size {d}", .{size});
    };

    // 2. Let db be a new Data Block value consisting of size bytes. If it is impossible to create
    //    such a Data Block, throw a RangeError exception.
    var data_block = DataBlock.initCapacity(agent.gc_allocator, size_casted) catch {
        return agent.throwException(.range_error, "Cannot allocate buffer of size {d}", .{size});
    };

    // 3. Set all of the bytes of db to 0.
    const slice = data_block.addManyAsSliceAssumeCapacity(size_casted);
    @memset(slice, 0);

    // 4. Return db.
    return data_block;
}

/// 6.2.9.2 CreateSharedByteDataBlock ( size )
/// https://tc39.es/ecma262/#sec-createsharedbytedatablock
pub fn createSharedByteDataBlock(agent: *Agent, size: u64) Agent.Error!DataBlock {
    // NOTE: Checking for a reasonable size below the theoretical limit is non-standard but also
    //       done in other engines (and tested by test262)
    if (size > data_block_max_byte_length) {
        return agent.throwException(.range_error, "Cannot allocate buffer of size {d}", .{size});
    }

    const size_casted = std.math.cast(usize, size) orelse {
        return agent.throwException(.range_error, "Cannot allocate buffer of size {d}", .{size});
    };

    // 1. Let db be a new Shared Data Block value consisting of size bytes. If it is impossible to
    //    create such a Shared Data Block, throw a RangeError exception.
    var data_block = DataBlock.initCapacity(agent.gc_allocator, size_casted) catch {
        return agent.throwException(.range_error, "Cannot allocate buffer of size {d}", .{size});
    };

    // 2. Let execution be the [[CandidateExecution]] field of the surrounding agent's Agent Record.
    // 3. Let eventsRecord be the Agent Events Record of execution.[[EventsRecords]] whose
    //    [[AgentSignifier]] is AgentSignifier().
    // 4. Let zero be « 0 ».
    // 5. For each index i of db, do
    //     a. Append WriteSharedMemory { [[Order]]: init, [[NoTear]]: true, [[Block]]: db,
    //        [[ByteIndex]]: i, [[ElementSize]]: 1, [[Payload]]: zero } to
    //        eventsRecord.[[EventList]].
    const slice = data_block.addManyAsSliceAssumeCapacity(size_casted);
    @memset(slice, 0);

    // 6. Return db.
    return data_block;
}

/// 6.2.9.3 CopyDataBlockBytes ( toBlock, toIndex, fromBlock, fromIndex, count )
/// https://tc39.es/ecma262/#sec-copydatablockbytes
pub fn copyDataBlockBytes(
    to_block: *DataBlock,
    to_index: u53,
    from_block: *const DataBlock,
    from_index: u53,
    count: u53,
) void {
    // 1. Assert: fromBlock and toBlock are distinct values.
    std.debug.assert(from_block != to_block);

    // 2. Let fromSize be the number of bytes in fromBlock.
    const from_size: u53 = @intCast(from_block.items.len);

    // 3. Assert: fromIndex + count ≤ fromSize.
    std.debug.assert(from_index + count <= from_size);

    // 4. Let toSize be the number of bytes in toBlock.
    const to_size: u53 = @intCast(to_block.items.len);

    // 5. Assert: toIndex + count ≤ toSize.
    std.debug.assert(to_index + count <= to_size);

    // 6. Repeat, while count > 0,
    //     TODO: a. If fromBlock is a Shared Data Block, then
    //         [...]
    //     b. Else,
    //         i. Assert: toBlock is not a Shared Data Block.
    //         ii. Set toBlock[toIndex] to fromBlock[fromIndex].
    //     c. Set toIndex to toIndex + 1.
    //     d. Set fromIndex to fromIndex + 1.
    //     e. Set count to count - 1.
    @memcpy(
        to_block.items[@intCast(to_index)..@intCast(to_index + count)],
        from_block.items[@intCast(from_index)..@intCast(from_index + count)],
    );

    // 7. Return unused.
}
