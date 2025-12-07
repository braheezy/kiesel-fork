const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Object = kiesel.types.Object;
const Realm = kiesel.execution.Realm;
const Value = kiesel.types.Value;

const blob = @import("blob.zig");
const console = @import("console.zig").console;
const crypto = @import("crypto.zig");
const fetch = @import("fetch.zig");
const navigator = @import("navigator.zig");
const performance = @import("performance.zig");
const queue_microtask = @import("queue_microtask.zig");
const text_decoder = @import("text_decoder.zig");
const text_encoder = @import("text_encoder.zig");
const timers = @import("timers.zig");
const event_loop = @import("loop.zig");

const has_sockaddr = @hasDecl(std.posix.system, "sockaddr") and
    std.posix.system.sockaddr != void;

pub fn addBindings(agent: *Agent, realm: *Realm, object: *Object) Agent.Error!void {
    // Ensure Response.prototype exists, even though we don't expose the constructor for now.
    if (has_sockaddr) _ = try fetch.response.constructor.create(agent, realm);
    try object.defineBuiltinPropertyLazy(
        agent,
        "Blob",
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                return Value.from(try blob.constructor.create(agent_, realm_));
            }
        }.initializer,
        realm,
        .builtin_default,
    );
    try object.defineBuiltinPropertyLazy(
        agent,
        "console",
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                return Value.from(try console.create(agent_, realm_));
            }
        }.initializer,
        realm,
        .builtin_default,
    );
    try object.defineBuiltinPropertyLazy(
        agent,
        "crypto",
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                const instance = try crypto.create(agent_, realm_);
                return Value.from(&instance.object);
            }
        }.initializer,
        realm,
        .builtin_default,
    );
    if (has_sockaddr) try object.defineBuiltinFunctionLazy(
        agent,
        "fetch",
        fetch.fetch,
        1,
        realm,
        .builtin_default,
    );
    try object.defineBuiltinPropertyLazy(
        agent,
        "navigator",
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                const instance = try navigator.create(agent_, realm_);
                return Value.from(&instance.object);
            }
        }.initializer,
        realm,
        .builtin_default,
    );
    try object.defineBuiltinPropertyLazy(
        agent,
        "performance",
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                const instance = try performance.create(agent_, realm_);
                return Value.from(&instance.object);
            }
        }.initializer,
        realm,
        .builtin_default,
    );
    try object.defineBuiltinFunctionLazy(
        agent,
        "queueMicrotask",
        queue_microtask.queueMicrotask,
        1,
        realm,
        .builtin_default,
    );
    try object.defineBuiltinPropertyLazy(
        agent,
        "TextDecoder",
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                return Value.from(try text_decoder.constructor.create(agent_, realm_));
            }
        }.initializer,
        realm,
        .builtin_default,
    );
    try object.defineBuiltinPropertyLazy(
        agent,
        "TextEncoder",
        struct {
            fn initializer(agent_: *Agent, realm_: *Realm) std.mem.Allocator.Error!Value {
                return Value.from(try text_encoder.constructor.create(agent_, realm_));
            }
        }.initializer,
        realm,
        .builtin_default,
    );
    try timers.addBindings(agent, realm, object);
}

pub fn runEventLoop(agent: *Agent) Agent.Error!void {
    return event_loop.run(agent);
}
