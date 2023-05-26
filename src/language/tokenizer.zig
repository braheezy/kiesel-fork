const ptk = @import("ptk");
const std = @import("std");

const literals = @import("literals.zig");
const parseNumericLiteral = literals.parseNumericLiteral;
const parseStringLiteral = literals.parseStringLiteral;

const TokenType = enum {
    @"-",
    @",",
    @";",
    @"!",
    @".",
    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",
    @"+",
    @"~",
    comment,
    debugger,
    do,
    @"else",
    false,
    identifier,
    @"if",
    null,
    numeric,
    string,
    this,
    throw,
    true,
    typeof,
    void,
    @"while",
    whitespace,
};

const Pattern = ptk.Pattern(TokenType);

pub const Tokenizer = ptk.Tokenizer(TokenType, &[_]Pattern{
    // NOTE: Needs to come first or strings such as 'ifelse' turn into two tokens
    Pattern.create(.identifier, identifierMatcher),
    Pattern.create(.@"-", ptk.matchers.literal("-")),
    Pattern.create(.@",", ptk.matchers.literal(",")),
    Pattern.create(.@";", ptk.matchers.literal(";")),
    Pattern.create(.@"!", ptk.matchers.literal("!")),
    Pattern.create(.@".", ptk.matchers.literal(".")),
    Pattern.create(.@"(", ptk.matchers.literal("(")),
    Pattern.create(.@")", ptk.matchers.literal(")")),
    Pattern.create(.@"[", ptk.matchers.literal("[")),
    Pattern.create(.@"]", ptk.matchers.literal("]")),
    Pattern.create(.@"{", ptk.matchers.literal("{")),
    Pattern.create(.@"}", ptk.matchers.literal("}")),
    Pattern.create(.@"+", ptk.matchers.literal("+")),
    Pattern.create(.@"~", ptk.matchers.literal("~")),
    Pattern.create(.comment, commentMatcher),
    Pattern.create(.debugger, ptk.matchers.literal("debugger")),
    Pattern.create(.do, ptk.matchers.literal("do")),
    Pattern.create(.@"else", ptk.matchers.literal("else")),
    Pattern.create(.false, ptk.matchers.literal("false")),
    Pattern.create(.@"if", ptk.matchers.literal("if")),
    Pattern.create(.null, ptk.matchers.literal("null")),
    Pattern.create(.numeric, numericMatcher),
    Pattern.create(.string, stringMatcher),
    Pattern.create(.this, ptk.matchers.literal("this")),
    Pattern.create(.throw, ptk.matchers.literal("throw")),
    Pattern.create(.true, ptk.matchers.literal("true")),
    Pattern.create(.typeof, ptk.matchers.literal("typeof")),
    Pattern.create(.void, ptk.matchers.literal("void")),
    Pattern.create(.@"while", ptk.matchers.literal("while")),
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

pub fn startsWithLineTerminator(str: []const u8) bool {
    for (line_terminators) |line_terminator| {
        if (std.mem.startsWith(u8, str, line_terminator)) return true;
    }
    return false;
}

pub fn containsLineTerminator(str: []const u8) bool {
    for (line_terminators) |line_terminator| {
        if (std.mem.indexOf(u8, str, line_terminator)) |_|
            return true;
    }
    return false;
}

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

/// 12.7 Names and Keywords
/// https://tc39.es/ecma262/#sec-names-and-keywords
fn identifierMatcher(str: []const u8) ?usize {
    // Identifier : IdentifierName but not ReservedWord
    const len = identifierNameMatcher(str) orelse return null;
    const identifier_name = str[0..len];
    for (reserved_words) |reserved_word| {
        if (std.mem.eql(u8, identifier_name, reserved_word)) return null;
    }
    return len;
}

fn identifierNameMatcher(str: []const u8) ?usize {
    // TODO: Handle UnicodeIDStart, UnicodeIDContinue, UnicodeEscapeSequence
    const start_chars = "$_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const part_chars = start_chars ++ "0123456789";
    for (str, 0..) |c, i| {
        if (std.mem.indexOfScalar(u8, if (i > 0) part_chars else start_chars, c) == null) {
            return i;
        }
    }
    return str.len;
}

/// 12.7.2 Keywords and Reserved Words
/// https://tc39.es/ecma262/#sec-keywords-and-reserved-words
pub const reserved_words = [_][]const u8{
    "await",   "break",  "case",     "catch",  "class",  "const",  "continue",   "debugger",
    "default", "delete", "do",       "else",   "enum",   "export", "extends",    "false",
    "finally", "for",    "function", "if",     "import", "in",     "instanceof", "new",
    "null",    "return", "super",    "switch", "this",   "throw",  "true",       "try",
    "typeof",  "var",    "void",     "while",  "with",   "yield",
};

fn numericMatcher(str: []const u8) ?usize {
    if (parseNumericLiteral(str, .partial)) |numeric_literal|
        return numeric_literal.text.len
    else |err| switch (err) {
        error.InvalidNumericLiteral => return null,
    }
}

fn stringMatcher(str: []const u8) ?usize {
    if (parseStringLiteral(str, .partial)) |string_literal|
        return string_literal.text.len
    else |err| switch (err) {
        error.InvalidStringLiteral => return null,
    }
}
