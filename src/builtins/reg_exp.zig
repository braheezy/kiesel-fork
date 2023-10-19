//! 22.2 RegExp (Regular Expression) Objects
//! https://tc39.es/ecma262/#sec-regexp-regular-expression-objects

const std = @import("std");

const Allocator = std.mem.Allocator;

const gc = @import("gc");

const libregexp = @cImport({
    @cInclude("libregexp.h");
});

const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const ArgumentsList = builtins.ArgumentsList;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyDescriptor = types.PropertyDescriptor;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const arrayCreate = builtins.arrayCreate;
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createRegExpStringIterator = builtins.createRegExpStringIterator;
const defineBuiltinAccessor = utils.defineBuiltinAccessor;
const defineBuiltinFunction = utils.defineBuiltinFunction;
const defineBuiltinProperty = utils.defineBuiltinProperty;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const sameValue = types.sameValue;

/// Copied from zig-libgc, unfortunately not a public API
fn getHeader(ptr: [*]u8) *[*]u8 {
    return @as(*[*]u8, @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize)));
}

/// Copied from zig-libgc, unfortunately not a public API
fn alignedAllocSize(ptr: [*]u8) usize {
    const unaligned_ptr = getHeader(ptr).*;
    const delta = @intFromPtr(ptr) - @intFromPtr(unaligned_ptr);
    return gc.c.GC_size(unaligned_ptr) - delta;
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

const FLAG_HAS_INDICES: c_int = 1 << 6;
const FLAG_UNICODE_SETS: c_int = 1 << 7;

pub const ParsedFlags = packed struct(u8) {
    const Self = @This();

    d: bool = false,
    g: bool = false,
    i: bool = false,
    m: bool = false,
    s: bool = false,
    u: bool = false,
    v: bool = false,
    y: bool = false,

    pub fn from(flags: []const u8) ?Self {
        var parsed_flags = Self{};
        for (flags) |flag| switch (flag) {
            inline 'd', 'g', 'i', 'm', 's', 'u', 'v', 'y' => |c| {
                if (@field(parsed_flags, &.{c})) return null;
                @field(parsed_flags, &.{c}) = true;
            },
            else => return null,
        };
        if (parsed_flags.u and parsed_flags.v) return null;
        return parsed_flags;
    }
};

/// 22.2.3.1 RegExpCreate ( P, F )
/// https://tc39.es/ecma262/#sec-regexpcreate
pub fn regExpCreate(agent: *Agent, pattern: Value, flags: Value) !Object {
    const realm = agent.currentRealm();

    // 1. Let obj be ! RegExpAlloc(%RegExp%).
    const object = regExpAlloc(
        agent,
        try realm.intrinsics.@"%RegExp%"(),
    ) catch |err| try noexcept(err);

    // 2. Return ? RegExpInitialize(obj, P, F).
    return regExpInitialize(agent, object, pattern, flags);
}

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
    const parsed_flags = ParsedFlags.from(f.utf8) orelse return agent.throwException(
        .syntax_error,
        try std.fmt.allocPrint(agent.gc_allocator, "Invalid RegExp flags '{s}'", .{f.utf8}),
    );

    // NOTE: "v" is not supported by libregexp, but we parse and store it regardless
    var re_flags: c_int = 0;
    if (parsed_flags.d) re_flags |= FLAG_HAS_INDICES;
    if (parsed_flags.g) re_flags |= libregexp.LRE_FLAG_GLOBAL;
    if (parsed_flags.i) re_flags |= libregexp.LRE_FLAG_IGNORECASE;
    if (parsed_flags.m) re_flags |= libregexp.LRE_FLAG_MULTILINE;
    if (parsed_flags.s) re_flags |= libregexp.LRE_FLAG_DOTALL;
    if (parsed_flags.u) re_flags |= libregexp.LRE_FLAG_UTF16;
    if (parsed_flags.v) re_flags |= FLAG_UNICODE_SETS;
    if (parsed_flags.y) re_flags |= libregexp.LRE_FLAG_STICKY;

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

/// 22.2.7.1 RegExpExec ( R, S )
/// https://tc39.es/ecma262/#sec-regexpexec
pub fn regExpExec(agent: *Agent, reg_exp: Object, string: String) !?Object {
    // 1. Let exec be ? Get(R, "exec").
    const exec = try reg_exp.get(PropertyKey.from("exec"));

    // 2. If IsCallable(exec) is true, then
    if (exec.isCallable()) {
        // a. Let result be ? Call(exec, R, « S »).
        const result = try exec.callAssumeCallable(Value.from(reg_exp), .{Value.from(string)});

        // b. If result is not an Object and result is not null, throw a TypeError exception.
        if (result != .object and result != .null) {
            return agent.throwException(
                .type_error,
                "RegExp exec function must return object or null",
            );
        }

        // c. Return result.
        return if (result == .object) result.object else null;
    }

    // 3. Perform ? RequireInternalSlot(R, [[RegExpMatcher]]).
    // 4. Return ? RegExpBuiltinExec(R, S).
    return regExpBuiltinExec(
        agent,
        try Value.from(reg_exp).requireInternalSlot(agent, RegExp),
        string,
    );
}

const CapturesList = [*c][*c]u8;

fn getMatch(captures_list: CapturesList, string: String, full_unicode: bool, i: usize) ?Match {
    if (captures_list[2 * i] == null or captures_list[2 * i + 1] == null) return null;
    const start_index = (@intFromPtr(captures_list[2 * i]) -
        @intFromPtr(string.utf8.ptr)) >> @intFromBool(full_unicode);
    const end_index = (@intFromPtr(captures_list[2 * i + 1]) -
        @intFromPtr(string.utf8.ptr)) >> @intFromBool(full_unicode);
    return .{ .start_index = start_index, .end_index = end_index };
}

/// 22.2.7.2 RegExpBuiltinExec ( R, S )
/// https://tc39.es/ecma262/#sec-regexpbuiltinexec
pub fn regExpBuiltinExec(agent: *Agent, reg_exp: *RegExp, string: String) !?Object {
    // 1. Let length be the length of S.
    const length = string.utf16Length();

    // 2. Let lastIndex be ℝ(? ToLength(? Get(R, "lastIndex"))).
    var last_index = std.math.lossyCast(
        usize,
        try (try reg_exp.object().get(PropertyKey.from("lastIndex"))).toLength(agent),
    );

    const re_bytecode = reg_exp.fields.re_bytecode;
    const capture_count: usize = @intCast(libregexp.lre_get_capture_count(@ptrCast(re_bytecode)));

    // libregexp's capture count includes the matched string
    std.debug.assert(capture_count >= 1);

    const captures_list: CapturesList = @ptrCast(
        try agent.gc_allocator.alloc([*c]u8, @intCast(@sizeOf([*c]u8) * capture_count * 2)),
    );

    // 3. Let flags be R.[[OriginalFlags]].
    const re_flags = libregexp.lre_get_flags(@ptrCast(re_bytecode));

    // 4. If flags contains "g", let global be true; else let global be false.
    // 5. If flags contains "y", let sticky be true; else let sticky be false.
    // 6. If flags contains "d", let hasIndices be true; else let hasIndices be false.

    // 7. If global is false and sticky is false, set lastIndex to 0.
    if ((re_flags & (libregexp.LRE_FLAG_GLOBAL | libregexp.LRE_FLAG_STICKY)) == 0) {
        last_index = 0;
    }

    // 8. Let matcher be R.[[RegExpMatcher]].

    // TODO: 9. If flags contains "u" or flags contains "v", let fullUnicode be true; else let fullUnicode be false.
    // NOTE: This is being used as a shift value, we must pass the right kind of string to libregexp for that to work.
    const full_unicode = false;

    // 10-13.
    const result = libregexp.lre_exec(
        captures_list,
        @ptrCast(re_bytecode),
        @ptrCast(string.utf8),
        @intCast(last_index),
        @intCast(string.utf8.len),
        @intFromBool(full_unicode),
        agent,
    );

    if (result < 0) return error.OutOfMemory;
    if (result == 0) {
        if (last_index > length or (re_flags & (libregexp.LRE_FLAG_GLOBAL | libregexp.LRE_FLAG_STICKY)) != 0) {
            try reg_exp.object().set(
                PropertyKey.from("lastIndex"),
                Value.from(0),
                .throw,
            );
        }
        return null;
    }
    var match = getMatch(captures_list, string, full_unicode, 0).?;
    last_index = match.start_index;

    // 14. Let e be r's endIndex value.
    // 15. If fullUnicode is true, set e to GetStringIndex(S, e).
    const end_index = match.end_index;

    // 16. If global is true or sticky is true, then
    if ((re_flags & (libregexp.LRE_FLAG_GLOBAL | libregexp.LRE_FLAG_STICKY)) != 0) {
        // a. Perform ? Set(R, "lastIndex", 𝔽(e), true).
        try reg_exp.object().set(PropertyKey.from("lastIndex"), Value.from(end_index), .throw);
    }

    // 17. Let n be the number of elements in r's captures List.
    const n = capture_count - 1;

    // 18. Assert: n = R.[[RegExpRecord]].[[CapturingGroupsCount]].
    // 19. Assert: n < 2**32 - 1.
    std.debug.assert(n < std.math.maxInt(u32));

    // 20. Let A be ! ArrayCreate(n + 1).
    // 21. Assert: The mathematical value of A's "length" property is n + 1.
    const array = arrayCreate(agent, n + 1, null) catch |err| try noexcept(err);

    // 22. Perform ! CreateDataPropertyOrThrow(A, "index", 𝔽(lastIndex)).
    array.createDataPropertyOrThrow(
        PropertyKey.from("index"),
        Value.from(last_index),
    ) catch |err| try noexcept(err);

    // 23. Perform ! CreateDataPropertyOrThrow(A, "input", S).
    array.createDataPropertyOrThrow(
        PropertyKey.from("input"),
        Value.from(string),
    ) catch |err| try noexcept(err);

    // 24. Let match be the Match Record { [[StartIndex]]: lastIndex, [[EndIndex]]: e }.
    match = Match{ .start_index = last_index, .end_index = end_index };

    // 25. Let indices be a new empty List.
    var indices = std.ArrayList(?Match).init(agent.gc_allocator);
    defer indices.deinit();

    // 26. Let groupNames be a new empty List.
    var group_names = std.ArrayList(?[]const u8).init(agent.gc_allocator);
    defer group_names.deinit();

    // 27. Append match to indices.
    try indices.append(match);

    // 28. Let matchedSubstr be GetMatchString(S, match).
    const matched_substr = Value.from(try getMatchString(agent, string, match));

    // 29. Perform ! CreateDataPropertyOrThrow(A, "0", matchedSubstr).
    array.createDataPropertyOrThrow(
        PropertyKey.from(0),
        matched_substr,
    ) catch |err| try noexcept(err);

    var group_name_ptr = libregexp.lre_get_groupnames(@ptrCast(re_bytecode));
    const has_groups = group_name_ptr != null;

    // 30. If R contains any GroupName, then
    const groups = if (has_groups) blk: {
        // a. Let groups be OrdinaryObjectCreate(null).
        break :blk Value.from(try ordinaryObjectCreate(agent, null));

        // b. Let hasGroups be true.
    }
    // 31. Else,
    else blk: {
        // a. Let groups be undefined.
        break :blk .undefined;

        // b. Let hasGroups be false.
    };

    // 32. Perform ! CreateDataPropertyOrThrow(A, "groups", groups).
    array.createDataPropertyOrThrow(
        PropertyKey.from("groups"),
        groups,
    ) catch |err| try noexcept(err);

    // 33. For each integer i such that 1 ≤ i ≤ n, in ascending order, do
    var i: usize = 1;
    while (i <= n) : (i += 1) {
        var captured_value: Value = undefined;

        // a. Let captureI be ith element of r's captures List.
        const capture_i = getMatch(captures_list, string, full_unicode, i);

        // b. If captureI is undefined, then
        if (capture_i == null) {
            // i. Let capturedValue be undefined.
            captured_value = .undefined;

            // ii. Append undefined to indices.
            try indices.append(null);
        }
        // c. Else,
        else {
            // i. Let captureStart be captureI.[[StartIndex]].
            // ii. Let captureEnd be captureI.[[EndIndex]].
            // iii. If fullUnicode is true, then
            //     1. Set captureStart to GetStringIndex(S, captureStart).
            //     2. Set captureEnd to GetStringIndex(S, captureEnd).
            // iv. Let capture be the Match Record { [[StartIndex]]: captureStart, [[EndIndex]]: captureEnd }.
            const capture = capture_i.?;

            // v. Let capturedValue be GetMatchString(S, capture).
            captured_value = Value.from(try getMatchString(agent, string, capture));

            // vi. Append capture to indices.
            try indices.append(capture);
        }

        // d. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(i)), capturedValue).
        array.createDataPropertyOrThrow(
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i))),
            captured_value,
        ) catch |err| try noexcept(err);

        // e. If the ith capture of R was defined with a GroupName, then
        if (group_name_ptr != null and group_name_ptr.* != 0) {
            // i. Let s be the CapturingGroupName of that GroupName.
            const group_name = std.mem.span(group_name_ptr);
            group_name_ptr += group_name.len + 1;

            // ii. Perform ! CreateDataPropertyOrThrow(groups, s, capturedValue).
            groups.object.createDataPropertyOrThrow(
                PropertyKey.from(group_name),
                captured_value,
            ) catch |err| try noexcept(err);

            // iii. Append s to groupNames.
            try group_names.append(group_name);
        }
        // f. Else,
        else {
            // i. Append undefined to groupNames.
            try group_names.append(null);
        }
    }

    // 34. If hasIndices is true, then
    if ((re_flags & FLAG_HAS_INDICES) != 0) {
        // a. Let indicesArray be MakeMatchIndicesIndexPairArray(S, indices, groupNames, hasGroups).
        const indices_array = try makeMatchIndicesIndexPairArray(
            agent,
            string,
            indices.items,
            group_names.items,
            has_groups,
        );

        // b. Perform ! CreateDataPropertyOrThrow(A, "indices", indicesArray).
        array.createDataPropertyOrThrow(
            PropertyKey.from("indices"),
            Value.from(indices_array),
        ) catch |err| try noexcept(err);
    }

    // 35. Return A.
    return array;
}

/// 22.2.7.3 AdvanceStringIndex ( S, index, unicode )
/// https://tc39.es/ecma262/#sec-advancestringindex
pub fn advanceStringIndex(string: String, index: u53, unicode: bool) u53 {
    // 1. Assert: index ≤ 2**53 - 1.

    // 2. If unicode is false, return index + 1.
    if (!unicode) return index + 1;

    // 3. Let length be the length of S.
    const length = string.utf16Length();

    // 4. If index + 1 ≥ length, return index + 1.
    if (index + 1 >= length) return index + 1;

    // 5. Let cp be CodePointAt(S, index).
    var it = std.unicode.Utf8View.initUnchecked(string.utf8).iterator();
    var i: u53 = 0;
    const code_point = while (it.nextCodepoint()) |code_point| : (i += 1) {
        if (i == index) break code_point;
    } else unreachable;

    // 6. Return index + cp.[[CodeUnitCount]].
    return index + (std.unicode.utf16CodepointSequenceLength(code_point) catch unreachable);
}

/// 22.2.7.5 Match Records
/// https://tc39.es/ecma262/#sec-match-records
const Match = struct {
    /// [[StartIndex]]
    start_index: usize,

    /// [[EndIndex]]
    end_index: usize,
};

/// 22.2.7.6 GetMatchString ( S, match )
/// https://tc39.es/ecma262/#sec-getmatchstring
fn getMatchString(agent: *Agent, string: String, match: Match) ![]const u8 {
    // 1. Assert: match.[[StartIndex]] ≤ match.[[EndIndex]] ≤ the length of S.
    std.debug.assert(match.start_index <= match.end_index);
    std.debug.assert(match.end_index <= string.utf16Length());

    // 2. Return the substring of S from match.[[StartIndex]] to match.[[EndIndex]].
    return string.substring(agent.gc_allocator, match.start_index, match.end_index);
}

/// 22.2.7.7 GetMatchIndexPair ( S, match )
/// https://tc39.es/ecma262/#sec-getmatchindexpair
fn getMatchIndexPair(agent: *Agent, string: String, match: Match) !Object {
    // 1. Assert: match.[[StartIndex]] ≤ match.[[EndIndex]] ≤ the length of S.
    std.debug.assert(match.start_index <= match.end_index);
    std.debug.assert(match.end_index <= string.utf16Length());

    // 2. Return CreateArrayFromList(« 𝔽(match.[[StartIndex]]), 𝔽(match.[[EndIndex]]) »).
    return createArrayFromList(
        agent,
        &.{ Value.from(match.start_index), Value.from(match.end_index) },
    );
}

/// 22.2.7.8 MakeMatchIndicesIndexPairArray ( S, indices, groupNames, hasGroups )
/// https://tc39.es/ecma262/#sec-makematchindicesindexpairarray
fn makeMatchIndicesIndexPairArray(agent: *Agent, string: String, indices: []const ?Match, group_names: []const ?[]const u8, has_groups: bool) !Object {
    // 1. Let n be the number of elements in indices.
    const n = indices.len;

    // 2. Assert: n < 2**32 - 1.
    std.debug.assert(n < std.math.maxInt(u32));

    // 3. Assert: groupNames has n - 1 elements.
    // 4. NOTE: The groupNames List contains elements aligned with the indices List starting at
    //    indices[1].
    std.debug.assert(group_names.len == n - 1);

    // 5. Let A be ! ArrayCreate(n).
    const array = arrayCreate(agent, 0, null) catch |err| try noexcept(err);

    // 6. If hasGroups is true, then
    const groups = if (has_groups) blk: {
        // a. Let groups be OrdinaryObjectCreate(null).
        break :blk Value.from(try ordinaryObjectCreate(agent, null));
    }
    // 7. Else,
    else blk: {
        // a. Let groups be undefined.
        break :blk .undefined;
    };

    // 8. Perform ! CreateDataPropertyOrThrow(A, "groups", groups).
    array.createDataPropertyOrThrow(
        PropertyKey.from("groups"),
        groups,
    ) catch |err| try noexcept(err);

    // 9. For each integer i such that 0 ≤ i < n, in ascending order, do
    var i: usize = 0;
    while (i < n) : (i += 1) {
        // a. Let matchIndices be indices[i].
        const match_indices = indices[i];

        // b. If matchIndices is not undefined,
        const match_index_pair = if (match_indices != null) blk: {
            // i. Let matchIndexPair be GetMatchIndexPair(S, matchIndices).
            break :blk Value.from(try getMatchIndexPair(agent, string, match_indices.?));
        }
        // c. Else,
        else blk: {
            // i. Let matchIndexPair be undefined.
            break :blk .undefined;
        };

        // d. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(i)), matchIndexPair).
        array.createDataPropertyOrThrow(
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i))),
            match_index_pair,
        ) catch |err| try noexcept(err);

        // e. If i > 0 and groupNames[i - 1] is not undefined, then
        if (i > 0 and group_names[i - 1] != null) {
            // i. Assert: groups is not undefined.
            std.debug.assert(groups != .undefined);

            // ii. Perform ! CreateDataPropertyOrThrow(groups, groupNames[i - 1], matchIndexPair).
            groups.object.createDataPropertyOrThrow(
                PropertyKey.from(try agent.gc_allocator.dupe(u8, group_names[i - 1].?)),
                match_index_pair,
            ) catch |err| try noexcept(err);
        }
    }

    // 10. Return A.
    return array;
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
        try defineBuiltinFunction(object, "exec", exec, 1, realm);
        try defineBuiltinAccessor(object, "flags", flags, null, realm);
        try defineBuiltinAccessor(object, "global", global, null, realm);
        try defineBuiltinAccessor(object, "hasIndices", hasIndices, null, realm);
        try defineBuiltinAccessor(object, "ignoreCase", ignoreCase, null, realm);
        try defineBuiltinFunction(object, "@@matchAll", @"@@matchAll", 1, realm);
        try defineBuiltinAccessor(object, "multiline", multiline, null, realm);
        try defineBuiltinFunction(object, "@@search", @"@@search", 1, realm);
        try defineBuiltinAccessor(object, "source", source, null, realm);
        try defineBuiltinAccessor(object, "sticky", sticky, null, realm);
        try defineBuiltinFunction(object, "test", @"test", 1, realm);
        try defineBuiltinFunction(object, "toString", toString, 0, realm);
        try defineBuiltinAccessor(object, "unicode", unicode, null, realm);
        try defineBuiltinAccessor(object, "unicodeSets", unicodeSets, null, realm);

        return object;
    }

    /// 22.2.6.2 RegExp.prototype.exec ( string )
    /// https://tc39.es/ecma262/#sec-regexp.prototype.exec
    fn exec(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Perform ? RequireInternalSlot(R, [[RegExpMatcher]]).
        const reg_exp = try this_value.requireInternalSlot(agent, RegExp);

        // 3. Let S be ? ToString(string).
        const string = try arguments.get(0).toString(agent);

        // 4. Return ? RegExpBuiltinExec(R, S).
        return if (try regExpBuiltinExec(agent, reg_exp, string)) |object|
            Value.from(object)
        else
            .null;
    }

    /// 22.2.6.3 get RegExp.prototype.dotAll
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.dotAll
    fn dotAll(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0073 (LATIN SMALL LETTER S).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.LRE_FLAG_DOTALL);
    }

    /// 22.2.6.4 get RegExp.prototype.flags
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.flags
    fn flags(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        const reg_exp = this_value;

        // 2. If R is not an Object, throw a TypeError exception.
        if (reg_exp != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{reg_exp}),
            );
        }

        // 3. Let codeUnits be a new empty List.
        var code_units = try std.ArrayList(u8).initCapacity(agent.gc_allocator, 8);

        // 4. Let hasIndices be ToBoolean(? Get(R, "hasIndices")).
        // 5. If hasIndices is true, append the code unit 0x0064 (LATIN SMALL LETTER D) to codeUnits.
        if ((try reg_exp.object.get(PropertyKey.from("hasIndices"))).toBoolean()) {
            code_units.appendAssumeCapacity('d');
        }

        // 6. Let global be ToBoolean(? Get(R, "global")).
        // 7. If global is true, append the code unit 0x0067 (LATIN SMALL LETTER G) to codeUnits.
        if ((try reg_exp.object.get(PropertyKey.from("global"))).toBoolean()) {
            code_units.appendAssumeCapacity('g');
        }

        // 8. Let ignoreCase be ToBoolean(? Get(R, "ignoreCase")).
        // 9. If ignoreCase is true, append the code unit 0x0069 (LATIN SMALL LETTER I) to codeUnits.
        if ((try reg_exp.object.get(PropertyKey.from("ignoreCase"))).toBoolean()) {
            code_units.appendAssumeCapacity('i');
        }

        // 10. Let multiline be ToBoolean(? Get(R, "multiline")).
        // 11. If multiline is true, append the code unit 0x006D (LATIN SMALL LETTER M) to codeUnits.
        if ((try reg_exp.object.get(PropertyKey.from("multiline"))).toBoolean()) {
            code_units.appendAssumeCapacity('m');
        }

        // 12. Let dotAll be ToBoolean(? Get(R, "dotAll")).
        // 13. If dotAll is true, append the code unit 0x0073 (LATIN SMALL LETTER S) to codeUnits.
        if ((try reg_exp.object.get(PropertyKey.from("dotAll"))).toBoolean()) {
            code_units.appendAssumeCapacity('s');
        }

        // 14. Let unicode be ToBoolean(? Get(R, "unicode")).
        // 15. If unicode is true, append the code unit 0x0075 (LATIN SMALL LETTER U) to codeUnits.
        if ((try reg_exp.object.get(PropertyKey.from("unicode"))).toBoolean()) {
            code_units.appendAssumeCapacity('u');
        }

        // 16. Let unicodeSets be ToBoolean(? Get(R, "unicodeSets")).
        // 17. If unicodeSets is true, append the code unit 0x0076 (LATIN SMALL LETTER V) to codeUnits.
        if ((try reg_exp.object.get(PropertyKey.from("unicodeSets"))).toBoolean()) {
            code_units.appendAssumeCapacity('v');
        }

        // 18. Let sticky be ToBoolean(? Get(R, "sticky")).
        // 19. If sticky is true, append the code unit 0x0079 (LATIN SMALL LETTER Y) to codeUnits.
        if ((try reg_exp.object.get(PropertyKey.from("sticky"))).toBoolean()) {
            code_units.appendAssumeCapacity('y');
        }

        // 20. Return the String value whose code units are the elements of the List codeUnits. If
        //     codeUnits has no elements, the empty String is returned.
        return Value.from(try code_units.toOwnedSlice());
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

    /// 22.2.6.7 get RegExp.prototype.ignoreCase
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.ignorecase
    fn ignoreCase(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0069 (LATIN SMALL LETTER I).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.LRE_FLAG_IGNORECASE);
    }

    /// 22.2.6.9 RegExp.prototype [ @@matchAll ] ( string )
    /// https://tc39.es/ecma262/#sec-regexp-prototype-matchall
    fn @"@@matchAll"(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const string_value = arguments.get(0);
        const realm = agent.currentRealm();

        // 1. Let R be the this value.
        const reg_exp = this_value;

        // 2. If R is not an Object, throw a TypeError exception.
        if (reg_exp != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{reg_exp}),
            );
        }

        // 3. Let S be ? ToString(string).
        const string = try string_value.toString(agent);

        // 4. Let C be ? SpeciesConstructor(R, %RegExp%).
        const constructor = try reg_exp.object.speciesConstructor(try realm.intrinsics.@"%RegExp%"());

        // 5. Let flags be ? ToString(? Get(R, "flags")).
        const flags_ = try (try reg_exp.object.get(PropertyKey.from("flags"))).toString(agent);

        // 6. Let matcher be ? Construct(C, « R, flags »).
        const matcher = try constructor.construct(.{ reg_exp, Value.from(flags_) }, null);

        // 7. Let lastIndex be ? ToLength(? Get(R, "lastIndex")).
        const last_index = try (try reg_exp.object.get(PropertyKey.from("lastIndex"))).toLength(agent);

        // 8. Perform ? Set(matcher, "lastIndex", lastIndex, true).
        try matcher.set(PropertyKey.from("lastIndex"), Value.from(last_index), .throw);

        // 9. If flags contains "g", let global be true.
        // 10. Else, let global be false.
        const global_ = std.mem.indexOfScalar(u8, flags_.utf8, 'g') != null;

        // 11. If flags contains "u" or flags contains "v", let fullUnicode be true.
        // 12. Else, let fullUnicode be false.
        const full_unicode = std.mem.indexOfScalar(u8, flags_.utf8, 'u') != null or
            std.mem.indexOfScalar(u8, flags_.utf8, 'v') != null;

        // 13. Return CreateRegExpStringIterator(matcher, S, global, fullUnicode).
        return Value.from(
            try createRegExpStringIterator(agent, matcher, string, global_, full_unicode),
        );
    }

    /// 22.2.6.10 get RegExp.prototype.multiline
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.multiline
    fn multiline(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x006D (LATIN SMALL LETTER M).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.LRE_FLAG_MULTILINE);
    }

    /// 22.2.6.12 RegExp.prototype [ @@search ] ( string )
    /// https://tc39.es/ecma262/#sec-regexp.prototype-@@search
    fn @"@@search"(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        const string_value = arguments.get(0);

        // 1. Let rx be the this value.
        const reg_exp = this_value;

        // 2. If rx is not an Object, throw a TypeError exception.
        if (reg_exp != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{reg_exp}),
            );
        }

        // 3. Let S be ? ToString(string).
        const string = try string_value.toString(agent);

        // 4. Let previousLastIndex be ? Get(rx, "lastIndex").
        const previous_last_index = try reg_exp.object.get(PropertyKey.from("lastIndex"));

        // 5. If SameValue(previousLastIndex, +0𝔽) is false, then
        if (!sameValue(previous_last_index, Value.from(0))) {
            // a. Perform ? Set(rx, "lastIndex", +0𝔽, true).
            try reg_exp.object.set(PropertyKey.from("lastIndex"), Value.from(0), .throw);
        }

        // 6. Let result be ? RegExpExec(rx, S).
        const result = try regExpExec(agent, reg_exp.object, string);

        // 7. Let currentLastIndex be ? Get(rx, "lastIndex").
        const current_last_index = try reg_exp.object.get(PropertyKey.from("lastIndex"));

        // 8. If SameValue(currentLastIndex, previousLastIndex) is false, then
        if (!sameValue(current_last_index, previous_last_index)) {
            // a. Perform ? Set(rx, "lastIndex", previousLastIndex, true).
            try reg_exp.object.set(PropertyKey.from("lastIndex"), previous_last_index, .throw);
        }

        // 9. If result is null, return -1𝔽.
        if (result == null) return Value.from(-1);

        // 10. Return ? Get(result, "index").
        return try result.?.get(PropertyKey.from("index"));
    }

    /// 22.2.6.13 get RegExp.prototype.source
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.source
    fn source(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // Let R be the this value.
        const reg_exp = this_value;

        // 2. If R is not an Object, throw a TypeError exception.
        if (reg_exp != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{reg_exp}),
            );
        }

        // 3. If R does not have an [[OriginalSource]] internal slot, then
        if (!reg_exp.object.is(RegExp)) {
            const realm = agent.currentRealm();

            // a. If SameValue(R, %RegExp.prototype%) is true, return "(?:)".
            if (reg_exp.object.sameValue(try realm.intrinsics.@"%RegExp.prototype%"())) {
                return Value.from("(?:)");
            }

            // b. Otherwise, throw a TypeError exception.
            return agent.throwException(.type_error, "This value must be a RegExp object");
        }

        // 4. Assert: R has an [[OriginalFlags]] internal slot.
        // 5. Let src be R.[[OriginalSource]].
        const src = reg_exp.object.as(RegExp).fields.original_source;

        // 6. Let flags be R.[[OriginalFlags]].
        const re_bytecode = reg_exp.object.as(RegExp).fields.re_bytecode;
        const re_flags = libregexp.lre_get_flags(@ptrCast(re_bytecode));

        // 7. Return EscapeRegExpPattern(src, flags).
        return Value.from(try escapeRegExpPattern(agent.gc_allocator, src, re_flags));
    }

    /// 22.2.6.13.1 EscapeRegExpPattern ( P, F )
    /// https://tc39.es/ecma262/#sec-escaperegexppattern
    fn escapeRegExpPattern(allocator: Allocator, pattern: String, _: c_int) !String {
        // TODO: 1-4.
        // 5. The code points / or any LineTerminator occurring in the pattern shall be escaped in
        //    S as necessary to ensure that the string-concatenation of "/", S, "/", and F can be
        //    parsed (in an appropriate lexical context) as a RegularExpressionLiteral that behaves
        //    identically to the constructed regular expression. For example, if P is "/", then S
        //    could be "\/" or "\u002F", among other possibilities, but not "/", because /// followed
        //    by F would be parsed as a SingleLineComment rather than a RegularExpressionLiteral.
        //    If P is the empty String, this specification can be met by letting S be "(?:)".
        // 6. Return S.
        if (pattern.utf16Length() == 0) return String.from("(?:)");
        var output = try allocator.alloc(
            u8,
            std.mem.replacementSize(u8, pattern.utf8, "/", "\\/"),
        );
        _ = std.mem.replace(u8, pattern.utf8, "/", "\\/", output);
        return String.from(output);
    }

    /// 22.2.6.15 get RegExp.prototype.sticky
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.sticky
    fn sticky(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0079 (LATIN SMALL LETTER Y).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.LRE_FLAG_STICKY);
    }

    /// 22.2.6.16 RegExp.prototype.test ( S )
    /// https://tc39.es/ecma262/#sec-regexp.prototype.test
    fn @"test"(agent: *Agent, this_value: Value, arguments: ArgumentsList) !Value {
        // 1. Let R be the this value.
        const reg_exp = this_value;

        // 2. If R is not an Object, throw a TypeError exception.
        if (reg_exp != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{reg_exp}),
            );
        }

        // 3. Let string be ? ToString(S).
        const string = try arguments.get(0).toString(agent);

        // 4. Let match be ? RegExpExec(R, string).
        const match = try regExpExec(agent, reg_exp.object, string);

        // 5. If match is not null, return true; else return false.
        return Value.from(match == null);
    }

    /// 22.2.6.17 RegExp.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-regexp.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        const reg_exp = this_value;

        // 2. If R is not an Object, throw a TypeError exception.
        if (reg_exp != .object) {
            return agent.throwException(
                .type_error,
                try std.fmt.allocPrint(agent.gc_allocator, "{} is not an Object", .{reg_exp}),
            );
        }

        // 3. Let pattern be ? ToString(? Get(R, "source")).
        const pattern = try (try reg_exp.object.get(PropertyKey.from("source"))).toString(agent);

        // 4. Let flags be ? ToString(? Get(R, "flags")).
        const flags_ = try (try reg_exp.object.get(PropertyKey.from("flags"))).toString(agent);

        // 5. Let result be the string-concatenation of "/", pattern, "/", and flags.
        const result = try std.mem.concat(
            agent.gc_allocator,
            u8,
            &.{ "/", pattern.utf8, "/", flags_.utf8 },
        );

        // 6. Return result.
        return Value.from(result);
    }

    /// 22.2.6.18 get RegExp.prototype.unicode
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.unicode
    fn unicode(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0075 (LATIN SMALL LETTER U).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.LRE_FLAG_UTF16);
    }

    /// 22.2.6.19 get RegExp.prototype.unicodeSets
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.unicodesets
    fn unicodeSets(agent: *Agent, this_value: Value, _: ArgumentsList) !Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0076 (LATIN SMALL LETTER V).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, FLAG_UNICODE_SETS);
    }
};

/// 22.2.8 Properties of RegExp Instances
/// https://tc39.es/ecma262/#sec-properties-of-regexp-instances
pub const RegExp = MakeObject(.{
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
