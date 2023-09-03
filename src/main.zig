pub const builtins = @import("builtins.zig");
pub const execution = @import("execution.zig");
pub const language = @import("language.zig");
pub const types = @import("types.zig");
pub const utils = @import("utils.zig");

pub const version_string = "0.1.0";

test {
    _ = builtins;
    _ = execution;
    _ = language;
    _ = types;
    _ = utils;
}
