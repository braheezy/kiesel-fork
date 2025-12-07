//! 5.6. Fetch method
//! https://fetch.spec.whatwg.org/#fetch-method

const std = @import("std");

const kiesel = @import("kiesel");

const Agent = kiesel.execution.Agent;
const Arguments = kiesel.types.Arguments;
const Value = kiesel.types.Value;
const newPromiseCapability = kiesel.builtins.newPromiseCapability;
const noexcept = kiesel.utils.noexcept;

pub const response = @import("fetch/response.zig");

/// https://fetch.spec.whatwg.org/#dom-global-fetch
pub fn fetch(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
    // Very basic completely non-compliant synchronous fetch. :3
    const realm = agent.currentRealm();
    const promise_capability = newPromiseCapability(
        agent,
        Value.from(try realm.intrinsics.@"%Promise%"()),
    ) catch |err| try noexcept(err);
    const url = try arguments.get(0).toString(agent);
    const url_utf8 = try url.toUtf8(agent.gc_allocator);
    defer agent.gc_allocator.free(url_utf8);

    const method: std.http.Method = .GET;
    const uri = std.Uri.parse(url_utf8) catch {
        const @"error" = try agent.createErrorObject(.type_error, "Invalid URL '{s}'", .{url_utf8});
        _ = try Value.from(promise_capability.reject).callAssumeCallable(
            agent,
            .undefined,
            &.{Value.from(&@"error".object)},
        );
        return Value.from(promise_capability.promise);
    };

    if (sendRequest(agent.gc_allocator, method, uri)) |result| {
        const response_object = try response.Response.create(agent, .{
            .prototype = response.response_prototype,
            .fields = .{
                .status = result.status,
                .reason = result.reason,
                .body = result.body,
            },
        });
        _ = try Value.from(promise_capability.resolve).callAssumeCallable(
            agent,
            .undefined,
            &.{Value.from(&response_object.object)},
        );
    } else |err| {
        const @"error" = try agent.createErrorObject(
            .type_error,
            "Could not fetch '{f}': {t}",
            .{ url, err },
        );
        _ = try Value.from(promise_capability.reject).callAssumeCallable(
            agent,
            .undefined,
            &.{Value.from(&@"error".object)},
        );
    }
    return Value.from(promise_capability.promise);
}

const SendRequestResult = struct {
    status: std.http.Status,
    reason: []const u8,
    body: []const u8,
};

fn sendRequest(allocator: std.mem.Allocator, method: std.http.Method, uri: std.Uri) !SendRequestResult {
    var arena_instance = std.heap.ArenaAllocator.init(allocator);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    var client: std.http.Client = .{ .allocator = allocator };
    defer client.deinit();
    try client.initDefaultProxies(arena);

    var req = try client.request(method, uri, .{
        .keep_alive = false,
    });
    defer req.deinit();
    try req.sendBodiless();

    var redirect_buffer: [1024]u8 = undefined;
    var resp = try req.receiveHead(&redirect_buffer);

    const status = resp.head.status;
    const reason = try allocator.dupe(u8, resp.head.reason);
    errdefer allocator.free(reason);

    var response_buffer: [1024]u8 = undefined;
    var decompress: std.http.Decompress = undefined;
    const decompress_buffer = try arena.alloc(u8, resp.head.content_encoding.minBufferCapacity());
    const reader = resp.readerDecompressing(&response_buffer, &decompress, decompress_buffer);
    const body = try reader.allocRemaining(allocator, .unlimited);

    return .{
        .status = status,
        .reason = reason,
        .body = body,
    };
}
