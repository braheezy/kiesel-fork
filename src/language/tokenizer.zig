const ptk = @import("ptk");
const std = @import("std");

const TokenType = enum {
    @"{",
    @"}",
    comment,
    debugger,
    false,
    null,
    semicolon,
    this,
    true,
    whitespace,
};

const Pattern = ptk.Pattern(TokenType);

pub const Tokenizer = ptk.Tokenizer(TokenType, &[_]Pattern{
    Pattern.create(.@"{", ptk.matchers.literal("{")),
    Pattern.create(.@"}", ptk.matchers.literal("}")),
    Pattern.create(.comment, commentMatcher),
    Pattern.create(.debugger, ptk.matchers.literal("debugger")),
    Pattern.create(.false, ptk.matchers.literal("false")),
    Pattern.create(.null, ptk.matchers.literal("null")),
    Pattern.create(.semicolon, ptk.matchers.literal(";")),
    Pattern.create(.this, ptk.matchers.literal("this")),
    Pattern.create(.true, ptk.matchers.literal("true")),
    Pattern.create(.whitespace, whitespaceMatcher),
});

/// 12.2 White Space
/// https://tc39.es/ecma262/#sec-white-space
pub const whitespace = [_][]const u8{
    "\u{0009}", // <TAB>
    "\u{000B}", // <VT>
    "\u{000C}", // <FF>
    "\u{FEFF}", // <ZWNBSP>
    "\u{0020}", // <USP> (Unicode Space_Separator)
    "\u{00A0}",
    "\u{1680}",
    "\u{2000}",
    "\u{2001}",
    "\u{2002}",
    "\u{2003}",
    "\u{2004}",
    "\u{2005}",
    "\u{2006}",
    "\u{2007}",
    "\u{2008}",
    "\u{2009}",
    "\u{200A}",
    "\u{202F}",
    "\u{205F}",
    "\u{3000}",
};

/// 12.3 Line Terminators
/// https://tc39.es/ecma262/#sec-line-terminators
pub const line_terminators = [_][]const u8{
    "\u{000A}", // <LF>
    "\u{000D}", // <CR>
    "\u{2028}", // <LS>
    "\u{2029}", // <PS>
};

fn whitespaceMatcher(str: []const u8) ?usize {
    var rest = str;
    while (rest.len != 0) {
        // The definition of white space is the union of WhiteSpace and LineTerminator.
        for (whitespace ++ line_terminators) |needle| {
            if (std.mem.startsWith(u8, rest, needle)) {
                rest = rest[needle.len..];
                break;
            }
        } else {
            break;
        }
    }
    return str.len - rest.len;
}

fn commentMatcher(str: []const u8) ?usize {
    // TODO: Implement me :^)
    _ = str;
    return null;
}
