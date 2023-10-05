const SafePointer = @import("any-pointer").SafePointer;

const execution = @import("../execution.zig");
const types = @import("../types.zig");

const JobCallback = execution.JobCallback;
const Object = types.Object;
const Realm = execution.Realm;

/// 9.5.2 HostMakeJobCallback ( callback )
/// https://tc39.es/ecma262/#sec-hostmakejobcallback
pub fn hostMakeJobCallback(callback: Object) JobCallback {
    // 1. Return the JobCallback Record { [[Callback]]: callback, [[HostDefined]]: empty }.
    return .{ .callback = callback, .host_defined = SafePointer.null_pointer };
}

/// 19.2.1.2 HostEnsureCanCompileStrings ( calleeRealm )
/// https://tc39.es/ecma262/#sec-hostensurecancompilestrings
pub fn hostEnsureCanCompileStrings(_: *Realm) !void {
    // The default implementation of HostEnsureCanCompileStrings is to return NormalCompletion(unused).
}

/// 20.2.5 HostHasSourceTextAvailable ( func )
/// https://tc39.es/ecma262/#sec-hosthassourcetextavailable
pub fn hostHasSourceTextAvailable(_: Object) bool {
    // The default implementation of HostHasSourceTextAvailable is to return true.
    return true;
}
