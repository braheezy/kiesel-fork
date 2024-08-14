const String = @import("../String.zig");

const Self = @This();

index: usize,
string: String,

pub fn next(self: *Self) ?u16 {
    if (self.index >= self.string.length()) return null;
    defer self.index += 1;
    return self.string.codeUnitAt(self.index);
}

pub fn previous(self: *Self) ?u16 {
    if (self.index == 0) return null;
    defer self.index -= 1;
    return self.string.codeUnitAt(self.index);
}
