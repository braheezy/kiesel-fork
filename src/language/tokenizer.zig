const ptk = @import("ptk");
const std = @import("std");

const literals = @import("literals.zig");
const parseNumericLiteral = literals.parseNumericLiteral;
const parseRegularExpressionLiteral = literals.parseRegularExpressionLiteral;
const parseStringLiteral = literals.parseStringLiteral;
const parseTemplateLiteral = literals.parseTemplateLiteral;

const TokenType = enum {
    @"--",
    @"-",
    @"-=",
    @",",
    @";",
    @":",
    @"!",
    @"!=",
    @"!==",
    @"??",
    @"??=",
    @"?.",
    @"?",
    @".",
    @"...",
    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",
    @"*",
    @"**",
    @"**=",
    @"*=",
    @"/",
    @"/=",
    @"&",
    @"&&",
    @"&&=",
    @"&=",
    @"%",
    @"%=",
    @"^",
    @"^=",
    @"+",
    @"++",
    @"+=",
    @"<",
    @"<<",
    @"<<=",
    @"<=",
    @"=",
    @"==",
    @"===",
    @"=>",
    @">",
    @">=",
    @">>",
    @">>=",
    @">>>",
    @">>>=",
    @"|",
    @"|=",
    @"||",
    @"||=",
    @"~",
    @"await",
    @"break",
    case,
    @"catch",
    class,
    comment,
    @"const",
    @"continue",
    debugger,
    default,
    delete,
    do,
    @"else",
    @"enum",
    @"export",
    extends,
    false,
    finally,
    @"for",
    function,
    hashbang_comment,
    identifier,
    @"if",
    import,
    in,
    instanceof,
    new,
    null,
    numeric,
    regular_expression,
    @"return",
    string,
    super,
    @"switch",
    template,
    this,
    throw,
    true,
    @"try",
    typeof,
    @"var",
    void,
    @"while",
    whitespace,
    with,
    yield,
};

const Pattern = ptk.Pattern(TokenType);

const patterns = .{
    // NOTE: Needs to come first or identifiers such as 'ifelse' turn into two tokens
    Pattern.create(.identifier, identifierMatcher),
    // NOTE: A few of these need to come first to take priority over single-character tokens such
    //       as '/' (vs '// foo') or '.' (vs '.123'). For simplicity we group all the non-literal
    //       matchers here.
    Pattern.create(.comment, commentMatcher),
    Pattern.create(.hashbang_comment, hashbangCommentMatcher),
    Pattern.create(.numeric, numericMatcher),
    Pattern.create(.regular_expression, regularExpressionMatcher),
    Pattern.create(.string, stringMatcher),
    Pattern.create(.template, templateMatcher),
    Pattern.create(.whitespace, whitespaceMatcher),
    Pattern.create(.@"--", ptk.matchers.literal("--")),
    Pattern.create(.@"-=", ptk.matchers.literal("-=")),
    Pattern.create(.@"-", ptk.matchers.literal("-")),
    Pattern.create(.@",", ptk.matchers.literal(",")),
    Pattern.create(.@";", ptk.matchers.literal(";")),
    Pattern.create(.@":", ptk.matchers.literal(":")),
    Pattern.create(.@"!==", ptk.matchers.literal("!==")),
    Pattern.create(.@"!=", ptk.matchers.literal("!=")),
    Pattern.create(.@"!", ptk.matchers.literal("!")),
    Pattern.create(.@"??=", ptk.matchers.literal("??=")),
    Pattern.create(.@"??", ptk.matchers.literal("??")),
    Pattern.create(.@"?.", ptk.matchers.literal("?.")),
    Pattern.create(.@"?", ptk.matchers.literal("?")),
    Pattern.create(.@"...", ptk.matchers.literal("...")),
    Pattern.create(.@".", ptk.matchers.literal(".")),
    Pattern.create(.@"(", ptk.matchers.literal("(")),
    Pattern.create(.@")", ptk.matchers.literal(")")),
    Pattern.create(.@"[", ptk.matchers.literal("[")),
    Pattern.create(.@"]", ptk.matchers.literal("]")),
    Pattern.create(.@"{", ptk.matchers.literal("{")),
    Pattern.create(.@"}", ptk.matchers.literal("}")),
    Pattern.create(.@"**=", ptk.matchers.literal("**=")),
    Pattern.create(.@"**", ptk.matchers.literal("**")),
    Pattern.create(.@"*=", ptk.matchers.literal("*=")),
    Pattern.create(.@"*", ptk.matchers.literal("*")),
    Pattern.create(.@"/=", ptk.matchers.literal("/=")),
    Pattern.create(.@"/", ptk.matchers.literal("/")),
    Pattern.create(.@"&&=", ptk.matchers.literal("&&=")),
    Pattern.create(.@"&&", ptk.matchers.literal("&&")),
    Pattern.create(.@"&=", ptk.matchers.literal("&=")),
    Pattern.create(.@"&", ptk.matchers.literal("&")),
    Pattern.create(.@"%=", ptk.matchers.literal("%=")),
    Pattern.create(.@"%", ptk.matchers.literal("%")),
    Pattern.create(.@"^=", ptk.matchers.literal("^=")),
    Pattern.create(.@"^", ptk.matchers.literal("^")),
    Pattern.create(.@"++", ptk.matchers.literal("++")),
    Pattern.create(.@"+=", ptk.matchers.literal("+=")),
    Pattern.create(.@"+", ptk.matchers.literal("+")),
    Pattern.create(.@"<<=", ptk.matchers.literal("<<=")),
    Pattern.create(.@"<<", ptk.matchers.literal("<<")),
    Pattern.create(.@"<=", ptk.matchers.literal("<=")),
    Pattern.create(.@"<", ptk.matchers.literal("<")),
    Pattern.create(.@"===", ptk.matchers.literal("===")),
    Pattern.create(.@"==", ptk.matchers.literal("==")),
    Pattern.create(.@"=>", ptk.matchers.literal("=>")),
    Pattern.create(.@"=", ptk.matchers.literal("=")),
    Pattern.create(.@">>>=", ptk.matchers.literal(">>>=")),
    Pattern.create(.@">>>", ptk.matchers.literal(">>>")),
    Pattern.create(.@">>=", ptk.matchers.literal(">>=")),
    Pattern.create(.@">>", ptk.matchers.literal(">>")),
    Pattern.create(.@">=", ptk.matchers.literal(">=")),
    Pattern.create(.@">", ptk.matchers.literal(">")),
    Pattern.create(.@"||=", ptk.matchers.literal("||=")),
    Pattern.create(.@"||", ptk.matchers.literal("||")),
    Pattern.create(.@"|=", ptk.matchers.literal("|=")),
    Pattern.create(.@"|", ptk.matchers.literal("|")),
    Pattern.create(.@"~", ptk.matchers.literal("~")),
    Pattern.create(.@"await", ptk.matchers.literal("await")),
    Pattern.create(.@"break", ptk.matchers.literal("break")),
    Pattern.create(.case, ptk.matchers.literal("case")),
    Pattern.create(.@"catch", ptk.matchers.literal("catch")),
    Pattern.create(.class, ptk.matchers.literal("class")),
    Pattern.create(.@"const", ptk.matchers.literal("const")),
    Pattern.create(.@"continue", ptk.matchers.literal("continue")),
    Pattern.create(.debugger, ptk.matchers.literal("debugger")),
    Pattern.create(.default, ptk.matchers.literal("default")),
    Pattern.create(.delete, ptk.matchers.literal("delete")),
    Pattern.create(.do, ptk.matchers.literal("do")),
    Pattern.create(.@"else", ptk.matchers.literal("else")),
    Pattern.create(.@"enum", ptk.matchers.literal("enum")),
    Pattern.create(.@"export", ptk.matchers.literal("export")),
    Pattern.create(.extends, ptk.matchers.literal("extends")),
    Pattern.create(.false, ptk.matchers.literal("false")),
    Pattern.create(.finally, ptk.matchers.literal("finally")),
    Pattern.create(.@"for", ptk.matchers.literal("for")),
    Pattern.create(.function, ptk.matchers.literal("function")),
    Pattern.create(.@"if", ptk.matchers.literal("if")),
    Pattern.create(.import, ptk.matchers.literal("import")),
    Pattern.create(.instanceof, ptk.matchers.literal("instanceof")),
    Pattern.create(.in, ptk.matchers.literal("in")),
    Pattern.create(.new, ptk.matchers.literal("new")),
    Pattern.create(.null, ptk.matchers.literal("null")),
    Pattern.create(.@"return", ptk.matchers.literal("return")),
    Pattern.create(.super, ptk.matchers.literal("super")),
    Pattern.create(.@"switch", ptk.matchers.literal("switch")),
    Pattern.create(.this, ptk.matchers.literal("this")),
    Pattern.create(.throw, ptk.matchers.literal("throw")),
    Pattern.create(.true, ptk.matchers.literal("true")),
    Pattern.create(.@"try", ptk.matchers.literal("try")),
    Pattern.create(.typeof, ptk.matchers.literal("typeof")),
    Pattern.create(.@"var", ptk.matchers.literal("var")),
    Pattern.create(.void, ptk.matchers.literal("void")),
    Pattern.create(.@"while", ptk.matchers.literal("while")),
    Pattern.create(.with, ptk.matchers.literal("with")),
    Pattern.create(.yield, ptk.matchers.literal("yield")),
};

pub const Tokenizer = ptk.Tokenizer(TokenType, &patterns);

// FIXME: ptk should provide tokenizer state to matchers
pub var state: struct {
    tokenizer: *Tokenizer = undefined,
    parsing_regular_expression: bool = false,
} = .{};

comptime {
    @setEvalBranchQuota(10000);
    token_types: for (std.enums.values(TokenType)) |token_type| {
        for (patterns) |pattern| {
            if (pattern.type == token_type) continue :token_types;
        }
        @compileError(@ptrCast(std.fmt.comptimePrint(
            "No pattern found for TokenType.@\"{s}\"",
            .{@tagName(token_type)},
        )));
    }
}

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

/// 12.4 Comments
/// https://tc39.es/ecma262/#sec-comments
fn commentMatcher(str: []const u8) ?usize {
    if (std.mem.startsWith(u8, str, "//")) {
        for (line_terminators) |line_terminator| {
            if (std.mem.indexOf(u8, str, line_terminator)) |index|
                return index;
        }
        return str.len;
    }
    if (std.mem.startsWith(u8, str, "/*")) {
        if (std.mem.indexOf(u8, str, "*/")) |index|
            return index + 2;
    }
    return null;
}

/// 12.5 Hashbang Comments
/// https://tc39.es/ecma262/#sec-hashbang
fn hashbangCommentMatcher(str: []const u8) ?usize {
    if (state.tokenizer.offset > 0) return null;
    if (std.mem.startsWith(u8, str, "#!")) {
        for (line_terminators) |line_terminator| {
            if (std.mem.indexOf(u8, str, line_terminator)) |index|
                return index;
        }
        return str.len;
    }
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

fn regularExpressionMatcher(str: []const u8) ?usize {
    if (!state.parsing_regular_expression) return null;
    if (parseRegularExpressionLiteral(str, .partial)) |regular_expression_literal|
        return regular_expression_literal.pattern.len + regular_expression_literal.flags.len + 2
    else |err| switch (err) {
        error.InvalidRegularExpressionLiteral => return null,
    }
}

fn templateMatcher(str: []const u8) ?usize {
    if (parseTemplateLiteral(str, .partial)) |template_literal|
        return template_literal.text.len
    else |err| switch (err) {
        error.InvalidTemplateLiteral => return null,
    }
}

pub fn escapeSequenceMatcher(str: []const u8) ?usize {
    if (str.len < 2 or str[0] != '\\') return null;
    switch (str[1]) {
        'x' => {
            // \xXX
            if (str.len >= 4 and std.ascii.isHex(str[2]) and std.ascii.isHex(str[3]))
                return 4
            else
                return null;
        },
        'u' => {
            if (str.len < 3) return null;
            switch (str[2]) {
                // \u{X} - \u{XXXXXX}
                '{' => {
                    var i: usize = 3;
                    while (i < str.len) : (i += 1) {
                        if (str[i] == '}') break;
                        if (!std.ascii.isHex(str[i])) return null;
                    }
                    if (i < 4 or i > 9) return null;
                    const code_point = std.fmt.parseInt(u24, str[3..i], 16) catch unreachable;
                    if (code_point > 0x10FFFF) return null;
                    return i + 1;
                },
                // \uXXXX
                else => if (str.len >= 6 and
                    std.ascii.isHex(str[2]) and
                    std.ascii.isHex(str[3]) and
                    std.ascii.isHex(str[4]) and
                    std.ascii.isHex(str[5]))
                    return 6
                else
                    return null,
            }
        },
        else => return 2,
    }
}

test escapeSequenceMatcher {
    for ([_][]const u8
    // zig fmt: off
    {
        // Garbage input
        "", "\\", "foo",
        // Invalid \x escapes
        "\\x", "\\xxx", "\\xXX", "\\x{00}",
        // Invalid \u escapes
        "\\u", "\\uxxxx", "\\uXXXX", "\\u0", "\\u00", "\\u000",
        "\\u{}", "\\u{x}", "\\u{X}", "\\u{xxxx}", "\\u{XXXX}", "\\u{0000000}",
    }
    // zig fmt: on
    ) |input| {
        try std.testing.expect(escapeSequenceMatcher(input) == null);
    }

    for ([_][]const u8
    // zig fmt: off
    {
        // Valid no-op escapes
        "\\a", "\\z", "\\A", "\\Z", "\\1", "\\9",
        // Valid one-char escapes
        "\\0", "\\f", "\\n", "\\r", "\\t", "\\v", "\\\\",
        // Valid \x escapes
        "\\x00", "\\xff", "\\xFF", "\\x0f", "\\x12",
        // Valid \u escapes
        "\\u0000", "\\uffff", "\\uFFFF", "\\u1234", "\\u0000",
        "\\u{0}", "\\u{00}", "\\u{000}", "\\u{0000}", "\\u{00000}", "\\u{000000}",
    }
    // zig fmt: on
    ) |input| {
        try std.testing.expect(escapeSequenceMatcher(input).? == input.len);
    }
}
