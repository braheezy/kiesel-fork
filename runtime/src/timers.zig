const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Realm = kiesel.execution.Realm;
const Value = kiesel.types.Value;
const Arguments = kiesel.types.Arguments;
const Object = kiesel.types.Object;
const Number = kiesel.types.Number;

const builtins = kiesel.builtins;
const FunctionBuiltin = builtins.function;

const emptyParameters: []const Value = &.{};

pub fn addBindings(agent: *Agent, realm: *Realm, object: *Object) Agent.Error!void {
    try object.defineBuiltinFunction(agent, "setTimeout", setTimeout, 1, realm);
    try object.defineBuiltinFunction(agent, "setInterval", setInterval, 1, realm);
    try object.defineBuiltinFunction(agent, "clearTimeout", clearTimer, 1, realm);
    try object.defineBuiltinFunction(agent, "clearInterval", clearTimer, 1, realm);
}

fn setTimeout(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    return try scheduleTimer(agent, arguments, false);
}

fn setInterval(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    return try scheduleTimer(agent, arguments, true);
}

fn scheduleTimer(
    agent: *Agent,
    arguments: Arguments,
    is_interval: bool,
) Agent.Error!Value {
    const realm = agent.currentRealm();
    const callback_value = arguments.get(0);
    const callback = try resolveTimerCallback(agent, realm, callback_value);

    const delay_value = arguments.getOrNull(1) orelse Value.from(0);
    const delay_number = try delay_value.toNumber(agent);
    const delay_ns = agent.timerDelayNs(delay_number);

    const extra_args = if (arguments.count() > 2) arguments.values[2..arguments.count()] else &.{};

    const this_value = Value.from(realm.global_object);
    const interval_ns = if (is_interval) delay_ns else null;
    const handle = try agent.scheduleTimer(
        callback,
        this_value,
        realm,
        extra_args,
        delay_ns,
        interval_ns,
    );
    const handle_f64: f64 = @floatFromInt(handle);
    return Value.from(Number.from(handle_f64));
}

fn clearTimer(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    if (arguments.count() == 0) return .undefined;
    const handle_value = arguments.get(0);
    const handle_number = try handle_value.toNumber(agent);
    if (handle_number.isNan()) return .undefined;
    const handle: u64 = @intFromFloat(handle_number.asFloat());
    _ = agent.clearTimer(handle);
    return .undefined;
}

fn resolveTimerCallback(agent: *Agent, realm: *Realm, callback: Value) Agent.Error!Value {
    if (callback.isCallable()) return callback;
    if (callback.isString()) {
        const body_string = try callback.toString(agent);
        const constructor = try realm.intrinsics.@"%Function%"();
        const ecmascript_function = try FunctionBuiltin.createDynamicFunction(
            agent,
            constructor,
            null,
            .normal,
            emptyParameters,
            Value.from(body_string),
        );
        return Value.from(&ecmascript_function.object);
    }
    return agent.throwException(
        .type_error,
        "setTimeout/setInterval callback must be callable or string",
        .{},
    );
}
