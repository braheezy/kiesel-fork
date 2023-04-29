//! 16.1.4 Script Records
//! https://tc39.es/ecma262/#sec-script-records

const std = @import("std");

const Allocator = std.mem.Allocator;

const execution = @import("../execution.zig");

const Realm = execution.Realm;

const Self = @This();

/// [[Realm]]
realm: *Realm,

/// [[HostDefined]]
host_defined: ?*anyopaque = null,

// TODO: [[ECMAScriptCode]], [[LoadedModules]]

/// 16.1.5 ParseScript ( sourceText, realm, hostDefined )
/// https://tc39.es/ecma262/#sec-parse-script
pub fn parse(allocator: Allocator, source_text: []const u8, realm: *Realm, host_defined: ?*anyopaque) !*Self {
    _ = source_text;

    // TODO: 1. Let script be ParseText(sourceText, Script).
    // TODO: 2. If script is a List of errors, return script.

    // 3. Return Script Record { [[Realm]]: realm, [[ECMAScriptCode]]: script, [[LoadedModules]]: « », [[HostDefined]]: hostDefined }.
    var script = try allocator.create(Self);
    script.realm = realm;
    script.host_defined = host_defined;
    return script;
}
