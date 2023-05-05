const builtin_function = @import("builtins/builtin_function.zig");
const ecmascript_function = @import("builtins/ecmascript_function.zig");

pub const ordinary = @import("builtins/ordinary.zig");

pub const BuiltinFunction = builtin_function.BuiltinFunction;
pub const Object = @import("builtins/Object.zig").Object;
pub const createBuiltinFunction = builtin_function.createBuiltinFunction;
pub const setFunctionLength = ecmascript_function.setFunctionLength;
pub const setFunctionName = ecmascript_function.setFunctionName;

test {
    _ = builtin_function;
    _ = ecmascript_function;
    _ = ordinary;

    _ = Object;
}
