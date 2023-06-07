const std = @import("std");

const ast = @import("ast.zig");
const tokenizer = @import("tokenizer.zig");
const startsWithLineTerminator = tokenizer.startsWithLineTerminator;

/// 12.9.3 Numeric Literals
/// https://tc39.es/ecma262/#sec-literals-numeric-literals
pub fn parseNumericLiteral(
    str: []const u8,
    consume: enum { partial, complete },
) !ast.NumericLiteral {
    var state: enum {
        start,
        prefix,
        separator,
        integer_digit,
        fraction_period,
        fraction_digit,
        exponent_indicator,
        exponent_sign,
        exponent_digit,
        big_int_suffix,
    } = .start;
    var system: ast.NumericLiteral.System = .decimal;
    var production: ast.NumericLiteral.Production = .regular;
    var @"type": ast.NumericLiteral.Type = .number;
    var state_before_separator: ?@TypeOf(state) = null;
    for (str, 0..) |c, i| switch (c) {
        '0', '1' => {
            if (system == .decimal and production == .regular and state == .integer_digit and i > 0 and str[0] == '0') {
                system = .octal;
                production = .legacy_octal_integer_literal;
            }
            switch (state) {
                .fraction_period, .fraction_digit => state = .fraction_digit,
                .exponent_indicator, .exponent_sign, .exponent_digit => state = .exponent_digit,
                .separator => state = state_before_separator.?,
                else => state = .integer_digit,
            }
        },
        '2'...'7' => {
            if (system == .binary) return error.InvalidNumericLiteral;
            if (system == .decimal and production == .regular and state == .integer_digit and i > 0 and str[0] == '0') {
                system = .octal;
                production = .legacy_octal_integer_literal;
            }
            switch (state) {
                .fraction_period, .fraction_digit => state = .fraction_digit,
                .exponent_indicator, .exponent_sign, .exponent_digit => state = .exponent_digit,
                .separator => state = state_before_separator.?,
                else => state = .integer_digit,
            }
        },
        '8', '9' => {
            if (system == .binary) return error.InvalidNumericLiteral;
            if (system == .octal and
                std.mem.startsWith(u8, str, "0o") or std.mem.startsWith(u8, str, "0O"))
                return error.InvalidNumericLiteral;
            if ((system == .decimal or system == .octal) and state == .integer_digit and i > 0 and str[0] == '0') {
                system = .decimal;
                production = .non_octal_decimal_integer_literal;
            }
            switch (state) {
                .fraction_period, .fraction_digit => state = .fraction_digit,
                .exponent_indicator, .exponent_sign, .exponent_digit => state = .exponent_digit,
                .separator => state = state_before_separator.?,
                else => state = .integer_digit,
            }
        },
        'a'...'f', 'A'...'F' => switch (state) {
            .prefix, .separator => {
                if (system == .hexadecimal)
                    state = .integer_digit
                else
                    return error.InvalidNumericLiteral;
            },
            .integer_digit, .fraction_digit => {
                if ((c == 'b' or c == 'B') and i == 1 and str[0] == '0') {
                    state = .prefix;
                    system = .binary;
                } else if ((c == 'e' or c == 'E') and system == .decimal) {
                    state = .exponent_indicator;
                } else if (system == .hexadecimal) {
                    state = .integer_digit;
                } else {
                    return error.InvalidNumericLiteral;
                }
            },
            else => return error.InvalidNumericLiteral,
        },
        'x', 'X', 'o', 'O' => switch (state) {
            .integer_digit => if (i == 1 and str[0] == '0') {
                state = .prefix;
                system = switch (c) {
                    'x', 'X' => .hexadecimal,
                    'o', 'O' => .octal,
                    else => unreachable,
                };
            } else {
                return error.InvalidNumericLiteral;
            },
            else => return error.InvalidNumericLiteral,
        },
        '_' => switch (state) {
            .integer_digit, .fraction_digit, .exponent_digit => {
                if ((str[0] == '0' and i == 1) or production != .regular)
                    return error.InvalidNumericLiteral;
                state_before_separator = state;
                state = .separator;
            },
            else => return error.InvalidNumericLiteral,
        },
        '.' => switch (state) {
            .start, .integer_digit => {
                if (system != .decimal or production != .regular)
                    return error.InvalidNumericLiteral;
                state = .fraction_period;
            },
            else => return error.InvalidNumericLiteral,
        },
        '+', '-' => switch (state) {
            .exponent_indicator => state = .exponent_sign,
            else => switch (consume) {
                // If we've not encountered an exponent sign these are binary operators.
                .partial => return .{
                    .text = str[0..i],
                    .system = system,
                    .production = production,
                    .type = @"type",
                },
                .complete => return error.InvalidNumericLiteral,
            },
        },
        'n' => switch (state) {
            .integer_digit => {
                if (production != .regular) return error.InvalidNumericLiteral;
                state = .big_int_suffix;
                @"type" = .big_int;
            },
            else => return error.InvalidNumericLiteral,
        },
        else => switch (consume) {
            .partial => return .{
                .text = str[0..i],
                .system = system,
                .production = production,
                .type = @"type",
            },
            .complete => return error.InvalidNumericLiteral,
        },
    };

    // Special case: fraction_period is allowed as an end state, but not on its own
    if (state == .fraction_period and str.len == 1) return error.InvalidNumericLiteral;

    return switch (state) {
        // Valid end states after exhausting the input string
        .integer_digit, .fraction_period, .fraction_digit, .exponent_digit, .big_int_suffix => .{
            .text = str,
            .system = system,
            .production = production,
            .type = @"type",
        },
        else => error.InvalidNumericLiteral,
    };
}

/// 12.9.4 String Literals
/// https://tc39.es/ecma262/#sec-literals-string-literals
pub fn parseStringLiteral(
    str: []const u8,
    consume: enum { partial, complete },
) !ast.StringLiteral {
    var state: enum {
        start,
        opening_quote,
        closing_quote,
        character,
        backslash,
        line_continuation_start,
        line_continuation_middle,
        line_continuation_end,
    } = .start;
    var opening_quote: ?u8 = null;
    for (str, 0..) |c, i| switch (c) {
        // TODO: Implement remaining escape sequence types
        '"', '\'' => switch (state) {
            .start => {
                state = .opening_quote;
                opening_quote = c;
            },
            .opening_quote, .character, .line_continuation_end => {
                state = if (c == opening_quote) .closing_quote else .character;
            },
            .backslash => state = .character,
            else => return error.InvalidStringLiteral,
        },
        '\\' => switch (state) {
            .opening_quote, .character, .line_continuation_end => state = .backslash,
            .backslash => state = .character,
            else => return error.InvalidStringLiteral,
        },
        else => switch (state) {
            .opening_quote, .character, .line_continuation_end => {
                if (startsWithLineTerminator(str[i..])) return error.InvalidStringLiteral;
                state = .character;
            },
            .backslash => {
                if (startsWithLineTerminator(str[i..])) {
                    state = if (i < str.len - 1 and (c == '\n' or (c == '\r' and str[i + 1] != '\n')))
                        .line_continuation_end
                    else
                        .line_continuation_start;
                } else {
                    state = .character;
                }
            },
            .line_continuation_start => {
                switch (str[i - 1]) {
                    '\xe2' => {
                        if (c != '\x80') return error.InvalidStringLiteral;
                        state = .line_continuation_middle;
                    },
                    '\r' => {
                        state = if (c == '\n')
                            .line_continuation_end
                        else
                            .character;
                    },
                    else => state = .character,
                }
            },
            .line_continuation_middle => {
                std.debug.assert(str[i - 1] == '\x80');
                switch (c) {
                    '\xa8', '\xa9' => state = .line_continuation_end,
                    else => return error.InvalidStringLiteral,
                }
            },
            .closing_quote => switch (consume) {
                .partial => return .{ .text = str[0..i] },
                .complete => return error.InvalidStringLiteral,
            },
            else => return error.InvalidStringLiteral,
        },
    };

    return switch (state) {
        // Valid end states after exhausting the input string
        .closing_quote => .{ .text = str },
        else => error.InvalidStringLiteral,
    };
}

test "parseNumericLiteral" {
    for ([_]ast.NumericLiteral{
        .{ .text = "0", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1234567890", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1_23_456_7_890", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1e1", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1E1", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1e+1", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1e-1", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1234567890e1234567890", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "0b0", .system = .binary, .production = .regular, .type = .number },
        .{ .text = "0B0", .system = .binary, .production = .regular, .type = .number },
        .{ .text = "0b1", .system = .binary, .production = .regular, .type = .number },
        .{ .text = "0b1", .system = .binary, .production = .regular, .type = .number },
        .{ .text = "0o0", .system = .octal, .production = .regular, .type = .number },
        .{ .text = "0O0", .system = .octal, .production = .regular, .type = .number },
        .{ .text = "0o1", .system = .octal, .production = .regular, .type = .number },
        .{ .text = "0o1234567", .system = .octal, .production = .regular, .type = .number },
        .{ .text = "0x0", .system = .hexadecimal, .production = .regular, .type = .number },
        .{ .text = "0X0", .system = .hexadecimal, .production = .regular, .type = .number },
        .{ .text = "0x1", .system = .hexadecimal, .production = .regular, .type = .number },
        .{ .text = "0x123456789abcdefABCDEF", .system = .hexadecimal, .production = .regular, .type = .number },
        .{ .text = ".0", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = ".000", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = ".1", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = ".111", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "0.", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1.", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "0.0", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "0.000", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "0.0_0_0", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1.1", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1.111", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1.1e1", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1234567890.1234567890e+1234567890", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "1_23_456_7_890.1_23_456_7_890e+1_23_456_7_890", .system = .decimal, .production = .regular, .type = .number },
        .{ .text = "0n", .system = .decimal, .production = .regular, .type = .big_int },
        .{ .text = "1n", .system = .decimal, .production = .regular, .type = .big_int },
        .{ .text = "1234567890n", .system = .decimal, .production = .regular, .type = .big_int },
        .{ .text = "1_23_456_7_890n", .system = .decimal, .production = .regular, .type = .big_int },
        .{ .text = "0b1n", .system = .binary, .production = .regular, .type = .big_int },
        .{ .text = "0o1n", .system = .octal, .production = .regular, .type = .big_int },
        .{ .text = "0x1n", .system = .hexadecimal, .production = .regular, .type = .big_int },
        .{ .text = "00", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "01", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "02", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "03", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "04", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "05", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "06", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "07", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "01234567", .system = .octal, .production = .legacy_octal_integer_literal, .type = .number },
        .{ .text = "08", .system = .decimal, .production = .non_octal_decimal_integer_literal, .type = .number },
        .{ .text = "09", .system = .decimal, .production = .non_octal_decimal_integer_literal, .type = .number },
        .{ .text = "0123456789", .system = .decimal, .production = .non_octal_decimal_integer_literal, .type = .number },
    }) |input| {
        const parsed = parseNumericLiteral(input.text, .complete) catch unreachable;
        try std.testing.expectEqual(input.system, parsed.system);
        try std.testing.expectEqual(input.production, parsed.production);
        try std.testing.expectEqual(input.type, parsed.type);
        try std.testing.expectEqualStrings(input.text, parsed.text);
    }

    for ([_][]const u8
    // zig fmt: off
    {
        // Garbage input
        "", "foo", "0foo", "foo0",
        // Plain prefixes
        "0b", "0B",  "0o", "0O", "0x", "0X",
        // Invalid prefixes
        "0a1", "0z1",  "0bb1", "0oo1", "0xx1", "0X",
        // Invalid exponents
        "e", "E", "1e", "1E", "1+e", "1-e",
        // Invalid separators
        "_0", "0_", "_1", "1_", "0_1", "01234567_0", "0__1", "1__1", "._1", "_.1", "1._", "1_.1", "1._1",
        // Invalid binary digits
        "0b2", "0b12", "0b123456789", "0b123456789abcdef",
        // Invalid octal digits
        "0o8", "0o89", "0o123456789", "0o123456789abcdef",
        // Invalid hexadecimal digits
        "0xg", "0xgh", "0xghijklmno", "0xx",
        // Invalid bigints
        "1e1n", "00n", "01n",
        // Invalid fractions
        ".", "..", "..0", "0..", "0..0", "1.2.3", "0b1.1", "0o1.1", "0x1.1", "00.", "00.1", "01.", "01.1",
    }
    // zig fmt: on
    ) |input| {
        const parse_error = parseNumericLiteral(input, .complete);
        try std.testing.expectError(error.InvalidNumericLiteral, parse_error);
    }
}

test "parseStringLiteral" {
    for ([_]ast.StringLiteral{
        .{ .text = "''" },
        .{ .text = "\"\"" },
        .{ .text = "' '" },
        .{ .text = "\" \"" },
        .{ .text = "'foo'" },
        .{ .text = "\"foo\"" },
        .{ .text = "\" foo \"" },
        .{ .text = "\"Here's a \\\"string\\\"\"" },
        .{ .text = "'\\' \\\" \\\\ \\b \\f \\n \\r \\t \\v \\0'" },
        .{ .text = "'\\\n'" },
        .{ .text = "'\\\n \\\r \\\r\n \\\u{2028} \\\u{2029}'" },
        .{ .text = "'\\a \\z \\1 \\123'" },
    }) |input| {
        const parsed = parseStringLiteral(input.text, .complete) catch unreachable;
        try std.testing.expectEqualStrings(input.text, parsed.text);
    }

    for ([_][]const u8
    // zig fmt: off
    {
        // Garbage input
        "", "foo", "123",
        // Invalid quotes
        "'", "\"",  "'\"", "\"'", "''\"", "\"\"'", "“”",
        // Invalid escapes
        "'\\'", "'\\\\\\'",
    }
    // zig fmt: on
    ) |input| {
        const parse_error = parseStringLiteral(input, .complete);
        try std.testing.expectError(error.InvalidStringLiteral, parse_error);
    }
}
