//! 22.2 RegExp (Regular Expression) Objects
//! https://tc39.es/ecma262/#sec-regexp-regular-expression-objects

const std = @import("std");

const gc = @cImport({
    @cInclude("gc.h");
});
const libregexp = @cImport({
    @cInclude("libregexp.h");
});

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const createBuiltinFunction = builtins.createBuiltinFunction;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const sameValue = types.sameValue;

/// Copied from zig-libgc, unfortunately not a public API
fn getHeader(ptr: [*]u8) *[*]u8 {
    return @as(*[*]u8, @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize)));
}

/// Copied from zig-libgc, unfortunately not a public API
fn alignedAllocSize(ptr: [*]u8) usize {
    const unaligned_ptr = getHeader(ptr).*;
    const delta = @intFromPtr(ptr) - @intFromPtr(unaligned_ptr);
    return gc.GC_size(unaligned_ptr) - delta;
}

export fn lre_check_stack_overflow(_: ?*anyopaque, _: usize) c_int {
    // TODO: Implement stack overflow check
    return 0;
}

export fn lre_realloc(@"opaque": ?*anyopaque, maybe_ptr: ?*anyopaque, size: usize) ?*anyopaque {
    const agent = @as(*Agent, @alignCast(@ptrCast(@"opaque".?)));
    if (maybe_ptr) |ptr| {
        var old_mem: []u8 = @as(*[0]u8, @ptrCast(ptr));
        old_mem.len = alignedAllocSize(old_mem.ptr);
        return if (agent.gc_allocator.realloc(old_mem, size)) |slice| slice.ptr else |_| null;
    } else {
        return if (agent.gc_allocator.alloc(u8, size)) |slice| slice.ptr else |_| null;
    }
}

const FLAG_HAS_INDICES = @as(c_int, 1) << @as(c_int, 6);

/// 22.2.3.2 RegExpAlloc ( newTarget )
/// https://tc39.es/ecma262/#sec-regexpalloc
pub fn regExpAlloc(agent: *Agent, new_target: Object) !Object {
    // 1. Let obj be ? OrdinaryCreateFromConstructor(newTarget, "%RegExp.prototype%",
    //    « [[OriginalSource]], [[OriginalFlags]], [[RegExpRecord]], [[RegExpMatcher]] »).
    const object = try ordinaryCreateFromConstructor(
        RegExp,
        agent,
        new_target,
        "%RegExp.prototype%",
    );

    // 2. Perform ! DefinePropertyOrThrow(obj, "lastIndex", PropertyDescriptor {
    //      [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    object.definePropertyOrThrow(PropertyKey.from("lastIndex"), .{
        .writable = true,
        .enumerable = false,
        .configurable = false,
    }) catch |err| try noexcept(err);

    // 3. Return obj.
    return object;
}

/// 22.2.3.3 RegExpInitialize ( obj, pattern, flags )
/// https://tc39.es/ecma262/#sec-regexpinitialize
pub fn regExpInitialize(agent: *Agent, object: Object, pattern: Value, flags: Value) !Object {
    // 1. If pattern is undefined, let P be the empty String.
    // 2. Else, let P be ? ToString(pattern).
    const p = if (pattern == .undefined) String.from("") else try pattern.toString(agent);

    // 3. If flags is undefined, let F be the empty String.
    // 4. Else, let F be ? ToString(flags).
    const f = if (flags == .undefined) String.from("") else try flags.toString(agent);

    // 5. If F contains any code unit other than "d", "g", "i", "m", "s", "u", "v", or "y", or if F
    //    contains any code unit more than once, throw a SyntaxError exception.
    // 6. If F contains "i", let i be true; else let i be false.
    // 7. If F contains "m", let m be true; else let m be false.
    // 8. If F contains "s", let s be true; else let s be false.
    // 9. If F contains "u", let u be true; else let u be false.
    // 10. If F contains "v", let v be true; else let v be false.
    var re_flags: c_int = 0;
    for (f.utf8) |c| {
        const mask = switch (c) {
            // NOTE: "v" is not supported by libregexp
            'd' => FLAG_HAS_INDICES,
            'g' => libregexp.LRE_FLAG_GLOBAL,
            'i' => libregexp.LRE_FLAG_IGNORECASE,
            'm' => libregexp.LRE_FLAG_MULTILINE,
            's' => libregexp.LRE_FLAG_DOTALL,
            'u' => libregexp.LRE_FLAG_UTF16,
            'y' => libregexp.LRE_FLAG_STICKY,
            else => return agent.throwException(
                .syntax_error,
                try std.fmt.allocPrint(agent.gc_allocator, "Invalid RegExp flag '{c}'", .{c}),
            ),
        };
        if ((re_flags & mask) != 0) {
            return agent.throwException(
                .syntax_error,
                try std.fmt.allocPrint(agent.gc_allocator, "Duplicate RegExp flag '{c}'", .{c}),
            );
        }
        re_flags |= mask;
    }

    // TODO: 11. If u is true or v is true, then
    //     a. Let patternText be StringToCodePoints(P).
    // 12. Else,
    //     a. Let patternText be the result of interpreting each of P's 16-bit elements as a
    //        Unicode BMP code point. UTF-16 decoding is not applied to the elements.

    // 13. Let parseResult be ParsePattern(patternText, u, v).
    // 14. If parseResult is a non-empty List of SyntaxError objects, throw a SyntaxError exception.
    // 15. Assert: parseResult is a Pattern Parse Node.
    var re_bytecode_len: c_int = undefined;
    var error_msg: [64]u8 = undefined;
    const buf = try agent.gc_allocator.dupeZ(u8, p.utf8);
    defer agent.gc_allocator.free(buf);
    const re_bytecode = libregexp.lre_compile(
        &re_bytecode_len,
        &error_msg,
        error_msg.len,
        buf.ptr,
        buf.len,
        re_flags,
        agent,
    ) orelse {
        const str = std.mem.span(@as([*:0]const u8, @ptrCast(&error_msg)));
        if (std.mem.eql(u8, str, "out of memory")) return error.OutOfMemory;
        return agent.throwException(
            .syntax_error,
            try std.fmt.allocPrint(agent.gc_allocator, "Invalid RegExp pattern: {s}", .{str}),
        );
    };

    // 16. Set obj.[[OriginalSource]] to P.
    object.as(RegExp).fields.original_source = p;

    // 17. Set obj.[[OriginalFlags]] to F.
    object.as(RegExp).fields.original_flags = f;

    // 18. Let capturingGroupsCount be CountLeftCapturingParensWithin(parseResult).
    // 19. Let rer be the RegExp Record {
    //       [[IgnoreCase]]: i, [[Multiline]]: m, [[DotAll]]: s, [[Unicode]]: u, [[UnicodeSets]]: v,
    //       [[CapturingGroupsCount]]: capturingGroupsCount
    //     }.
    // 20. Set obj.[[RegExpRecord]] to rer.
    // 21. Set obj.[[RegExpMatcher]] to CompilePattern of parseResult with argument rer.
    var re_bytecode_slice: []u8 = @as(*[0]u8, @ptrCast(re_bytecode));
    re_bytecode_slice.len = @intCast(re_bytecode_len);
    object.as(RegExp).fields.re_bytecode = re_bytecode_slice;

    // 22. Perform ? Set(obj, "lastIndex", +0𝔽, true).
    try object.set(PropertyKey.from("lastIndex"), Value.from(0), .throw);

    // 23. Return obj.
    return object;
}

/// 22.2.5 Properties of the RegExp Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-regexp-constructor
pub const RegExpConstructor = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try createBuiltinFunction(realm.agent, .{ .constructor = behaviour }, .{
            .length = 2,
            .name = "RegExp",
            .realm = realm,
            .prototype = try realm.intrinsics.@"%Function.prototype%"(),
        });

        // 22.2.5.1 RegExp.prototype
        // https://tc39.es/ecma262/#sec-regexp.prototype
        try defineBuiltinProperty(object, "prototype", PropertyDescriptor{
            .value = Value.from(try realm.intrinsics.@"%RegExp.prototype%"()),
            .writable = false,
            .enumerable = false,
            .configurable = false,
        });

        // 22.2.5.2 get RegExp [ @@species ]
        // https://tc39.es/ecma262/#sec-get-regexp-@@species
        try defineBuiltinAccessor(object, "@@species", struct {
            fn getter(_: *Agent, this_value: Value, _: ArgumentsList) !Value {
                // 1. Return the this value.
                return this_value;
            }
        }.getter, null, realm);

        // 22.2.6.1 RegExp.prototype.constructor
        // https://tc39.es/ecma262/#sec-regexp.prototype.constructor
        try defineBuiltinProperty(
            realm.intrinsics.@"%RegExp.prototype%"() catch unreachable,
            "constructor",
            Value.from(object),
        );

        return object;
    }

    /// 22.2.4.1 RegExp ( pattern, flags )
    /// https://tc39.es/ecma262/#sec-regexp-pattern-flags
    fn behaviour(agent: *Agent, _: Value, arguments: ArgumentsList, new_target: ?Object) !Value {
        const pattern = arguments.get(0);
        const flags = arguments.get(1);

        // 1. Let patternIsRegExp be ? IsRegExp(pattern).
        const pattern_is_regexp = try pattern.isRegExp();

        var constructor: Object = undefined;

        // 2. If NewTarget is undefined, then
        if (new_target == null) {
            // a. Let newTarget be the active function object.
            constructor = agent.activeFunctionObject();

            // b. If patternIsRegExp is true and flags is undefined, then
            if (pattern_is_regexp and flags == .undefined) {
                // i. Let patternConstructor be ? Get(pattern, "constructor").
                const pattern_constructor = try pattern.object.get(PropertyKey.from("constructor"));

                // ii. If SameValue(newTarget, patternConstructor) is true, return pattern.
                if (sameValue(Value.from(constructor), pattern_constructor)) return pattern;
            }
        }
        // 3. Else,
        else {
            // a. Let newTarget be NewTarget.
            constructor = new_target.?;
        }

        var p: Value = undefined;
        var f: Value = undefined;

        // 4. If pattern is an Object and pattern has a [[RegExpMatcher]] internal slot, then
        if (pattern == .object and pattern.object.is(RegExp)) {
            // a. Let P be pattern.[[OriginalSource]].
            p = Value.from(pattern.object.as(RegExp).fields.original_source);

            // b. If flags is undefined, let F be pattern.[[OriginalFlags]].
            if (flags == .undefined) {
                f = Value.from(pattern.object.as(RegExp).fields.original_flags);
            }
            // c. Else, let F be flags.
            else {
                f = flags;
            }
        }
        // 5. Else if patternIsRegExp is true, then
        else if (pattern_is_regexp) {
            // a. Let P be ? Get(pattern, "source").
            p = try pattern.object.get(PropertyKey.from("source"));

            // b. If flags is undefined, then
            if (flags == .undefined) {
                // i. Let F be ? Get(pattern, "flags").
                f = try pattern.object.get(PropertyKey.from("flags"));
            }
            // c. Else,
            else {
                // i. Let F be flags.
                f = flags;
            }
        }
        // 6. Else,
        else {
            // a. Let P be pattern.
            p = pattern;

            // b. Let F be flags.
            f = flags;
        }

        // 7. Let O be ? RegExpAlloc(newTarget).
        const object = try regExpAlloc(agent, constructor);

        // 8. Return ? RegExpInitialize(O, P, F).
        return Value.from(try regExpInitialize(agent, object, p, f));
    }
};

/// 22.2.6 Properties of the RegExp Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-regexp-prototype-object
pub const RegExpPrototype = struct {
    pub fn create(realm: *Realm) !Object {
        const object = try builtins.Object.create(realm.agent, .{
            .prototype = try realm.intrinsics.@"%Object.prototype%"(),
        });

        try defineBuiltinAccessor(object, "dotAll", dotAll, null, realm);
        try defineBuiltinAccessor(object, "global", global, null, realm);
        try defineBuiltinAccessor(object, "hasIndices", hasIndices, null, realm);

        return object;
    }

    /// 22.2.6.3 get RegExp.prototype.dotAll
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.dotAll
    fn dotAll(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0073 (LATIN SMALL LETTER S).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.LRE_FLAG_DOTALL);
    }

    /// 22.2.6.4.1 RegExpHasFlag ( R, codeUnit )
    /// https://tc39.es/ecma262/#sec-regexphasflag
    fn regExpHasFlag(agent: *Agent, reg_exp: Value, flag: c_int) !Value {
        // 1. If R is not an Object, throw a TypeError exception.
        if (reg_exp != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{reg_exp}),
            );
        }

        // 2. If R does not have an [[OriginalFlags]] internal slot, then
        if (!reg_exp.object.is(RegExp)) {
            const realm = agent.currentRealm();

            // a. If SameValue(R, %RegExp.prototype%) is true, return undefined.
            if (reg_exp.object.sameValue(try realm.intrinsics.@"%RegExp.prototype%"())) {
                return .undefined;
            }

            // b. Otherwise, throw a TypeError exception.
            return agent.throwException(.type_error, "This value must be a RegExp object");
        }

        // 3. Let flags be R.[[OriginalFlags]].
        const re_bytecode = reg_exp.object.as(RegExp).fields.re_bytecode;
        const re_flags = libregexp.lre_get_flags(@ptrCast(re_bytecode));

        // 4. If flags contains codeUnit, return true.
        // 5. Return false.
        return Value.from((re_flags & flag) != 0);
    }

    /// 22.2.6.5 get RegExp.prototype.global
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.global
    fn global(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0067 (LATIN SMALL LETTER G).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.LRE_FLAG_GLOBAL);
    }

    /// 22.2.6.6 get RegExp.prototype.hasIndices
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.global
    fn hasIndices(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0064 (LATIN SMALL LETTER D).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, FLAG_HAS_INDICES);
    }
};

/// 22.2.8 Properties of RegExp Instances
/// https://tc39.es/ecma262/#sec-properties-of-regexp-instances
pub const RegExp = Object.Factory(.{
    .Fields = struct {
        /// [[OriginalSource]]
        original_source: String,

        /// [[OriginalFlags]]
        original_flags: String,

        /// [[RegExpRecord]]
        re_bytecode: []const u8,
    },
    .tag = .reg_exp,
});
