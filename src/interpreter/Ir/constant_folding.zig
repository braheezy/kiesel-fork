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
    switch (expr.*) {
        .primary_expression => |prim_expr| switch (prim_expr) {
            .literal => |*literal| return try constantFoldLiteral(gpa, literal),
            else => {},
        },
        .binary_expression => |*bin_expr| return constantFoldBinaryExpression(gpa, bin_expr),
        else => {},
    }
    return null;
}

fn constantFoldLiteral(
    gpa: std.mem.Allocator,
    literal: *const ast.Literal,
) std.mem.Allocator.Error!Constant {
    return switch (literal.*) {
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

fn constantFoldBinaryExpression(
    gpa: std.mem.Allocator,
    bin_expr: *const ast.BinaryExpression,
) std.mem.Allocator.Error!?Constant {
    if (try constantFold(gpa, bin_expr.lhs_expression)) |lhs| {
        defer lhs.deinit(gpa);
        if (try constantFold(gpa, bin_expr.rhs_expression)) |rhs| {
            defer rhs.deinit(gpa);
            switch (bin_expr.operator) {
                .@"+" => {
                    if (lhs == .string or rhs == .string) {
                        const lhs_str = switch (lhs) {
                            .undefined => "undefined",
                            .null => "null",
                            .boolean => |b| if (b) "true" else "false",
                            // TODO: Implement Number.toString() without needing an agent
                            .number => return null,
                            .big_int => |b| try b.toStringAlloc(gpa, 10, .lower),
                            .string => |s| s,
                        };
                        defer if (lhs == .big_int) gpa.free(lhs_str);
                        const rhs_str = switch (rhs) {
                            .undefined => "undefined",
                            .null => "null",
                            .boolean => |b| if (b) "true" else "false",
                            // TODO: Implement Number.toString() without needing an agent
                            .number => return null,
                            .big_int => |b| try b.toStringAlloc(gpa, 10, .lower),
                            .string => |s| s,
                        };
                        defer if (rhs == .big_int) gpa.free(rhs_str);
                        const result = try std.mem.concat(gpa, u8, &.{ lhs_str, rhs_str });
                        return .{ .string = result };
                    } else if (lhs == .number and rhs == .number) {
                        const result = lhs.number + rhs.number;
                        return .{ .number = result };
                    } else if (lhs == .big_int and rhs == .big_int) {
                        var result_managed: std.math.big.int.Managed = try .init(gpa);
                        defer result_managed.deinit();
                        try result_managed.ensureAddCapacity(lhs.big_int, rhs.big_int);
                        var result_mutable = result_managed.toMutable();
                        result_mutable.add(lhs.big_int, rhs.big_int);
                        result_managed.setMetadata(result_mutable.positive, result_mutable.len);
                        const result: std.math.big.int.Const = .{
                            .limbs = try gpa.dupe(std.math.big.Limb, result_managed.toConst().limbs),
                            .positive = result_managed.toConst().positive,
                        };
                        return .{ .big_int = result };
                    } else if (lhs == .boolean and rhs == .boolean) {
                        // Y tho
                        const result: f64 = @floatFromInt(@intFromBool(lhs.boolean) + @intFromBool(rhs.boolean));
                        return .{ .number = result };
                    }
                },
                else => {},
            }
        }
    }
    return null;
}
