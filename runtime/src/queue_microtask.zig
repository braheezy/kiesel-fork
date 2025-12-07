//! 8.7 Microtask queuing
//! https://html.spec.whatwg.org/#microtask-queuing

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const Job = kiesel.execution.Job;
const Object = kiesel.types.Object;
const SafePointer = kiesel.types.SafePointer;
const Value = kiesel.types.Value;

/// https://html.spec.whatwg.org/#dom-queuemicrotask
pub fn queueMicrotask(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    // The queueMicrotask(callback) method must queue a microtask to invoke callback, and if
    // callback throws an exception, report the exception.

    const callback = arguments.get(0);
    if (!callback.isCallable()) {
        return agent.throwException(.type_error, "{f} is not callable", .{callback});
    }
    const Captures = struct {
        agent: *Agent,
        callback: *Object,
    };
    const captures = try agent.gc_allocator.create(Captures);
    captures.* = .{
        .agent = agent,
        .callback = callback.asObject(),
    };
    const func = struct {
        fn func(captures_: SafePointer) Agent.Error!Value {
            const agent_ = captures_.cast(*Captures).agent;
            const callback_ = captures_.cast(*Captures).callback;
            const stderr = agent_.platform.stderr;

            _ = Value.from(callback_).callAssumeCallable(agent_, .undefined, &.{}) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.ExceptionThrown => {
                    const exception = agent_.clearException();
                    stderr.print("{f}\n", .{exception.fmtPretty()}) catch {};
                    stderr.flush() catch {};
                },
            };
            return .undefined;
        }
    }.func;
    const job: Job = .{ .func = func, .captures = .make(*Captures, captures) };
    const realm = agent.currentRealm();
    try agent.host_hooks.hostEnqueueGenericJob(agent, job, realm);
    return .undefined;
}
