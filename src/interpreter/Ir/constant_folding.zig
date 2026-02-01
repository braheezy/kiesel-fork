const std = @import("std");

const ast = @import("../../language/ast.zig");

pub const Constant = union(enum) {
    undefined,
    null,
    boolean: bool,
    number: f64,
    big_int: std.math.big.int.Const,
    string: []const u8,

    pub fn isTruthy(constant: Constant) bool {
        return switch (constant) {
            .undefined, .null => false,
            .boolean => |boolean| boolean,
            .number => |number| number != 0 and !std.math.isNan(number),
            .big_int => |big_int| !big_int.eqlZero(),
            .string => |string| string.len > 0,
        };
    }

    pub fn isNullish(constant: Constant) bool {
        return switch (constant) {
            .undefined, .null => true,
            else => false,
        };
    }

    pub fn deinit(constant: Constant, gpa: std.mem.Allocator) void {
        switch (constant) {
            .big_int => |big_int| gpa.free(big_int.limbs),
            .string => |string| gpa.free(string),
            else => {},
        }
    }
};

pub fn constantFold(
    gpa: std.mem.Allocator,
    expr: *const ast.Expression,
) std.mem.Allocator.Error!?Constant {
    // TODO: Implement constant folding for more complex expressions
    if (expr.* != .primary_expression or expr.primary_expression != .literal) {
        return null;
    }
    return switch (expr.primary_expression.literal) {
        .null => .null,
        .boolean => |boolean| .{ .boolean = boolean },
        .numeric => |numeric| blk: {
            const value = try numeric.numericValue(gpa);
            if (value.isNumber()) {
                const n = value.asNumber().asFloat();
                break :blk .{ .number = n };
            } else {
                const js_big_int = value.asBigInt();
                defer {
                    @constCast(js_big_int).managed.deinit();
                    gpa.destroy(js_big_int);
                }
                const big_int = js_big_int.managed.toConst();
                break :blk .{ .big_int = .{
                    .limbs = try gpa.dupe(std.math.big.Limb, big_int.limbs),
                    .positive = big_int.positive,
                } };
            }
        },
        .string => |s| .{ .string = try gpa.dupe(u8, s.text[1 .. s.text.len - 1]) },
    };
}
