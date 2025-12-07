const std = @import("std");

const xev = @import("xev");
const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;

pub fn run(agent: *Agent) Agent.Error!void {
    var loop = xev.Loop.init(.{}) catch return error.OutOfMemory;
    defer loop.deinit();

    var timer = xev.Timer.init() catch return error.OutOfMemory;
    defer timer.deinit();

    var completion: xev.Completion = .{};
    var handles = try std.ArrayList(u64).initCapacity(agent.gc_allocator, 0);
    defer handles.deinit(agent.gc_allocator);

    while (true) {
        const now_ns: u128 = @intCast(agent.platform.currentTimeNs());
        handles.clearRetainingCapacity();
        try agent.collectDueTimerHandles(now_ns, &handles);
        if (handles.items.len != 0) {
            for (handles.items) |handle| {
                agent.runTimerCallback(handle, now_ns);
            }
            continue;
        }

        if (!agent.hasPendingTimers()) break;

        const next_fire = agent.nextTimerFireTime();
        const wait_ms: u64 = if (next_fire) |target| blk: {
            const delta = if (target > now_ns) target - now_ns else 0;
            const delta_ms = delta / 1_000_000;
            const ms: u64 = @intCast(delta_ms);
            break :blk ms;
        } else 0;

        // Prevent zero-length waits from bypassing the timer.
        completion = .{};
        timer.run(&loop, &completion, wait_ms, void, null, timerCallback);
        loop.run(.once) catch return error.OutOfMemory;
    }

    return;
}

fn timerCallback(
    _: ?*void,
    _: *xev.Loop,
    _: *xev.Completion,
    _: xev.Timer.RunError!void,
) xev.CallbackAction {
    return .disarm;
}
