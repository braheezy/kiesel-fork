pub const ordinary = @import("builtins/ordinary.zig");

pub const Object = @import("builtins/Object.zig").Object;

test {
    _ = ordinary;

    _ = Object;
}
