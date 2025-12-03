//! 22.2 RegExp (Regular Expression) Objects
//! https://tc39.es/ecma262/#sec-regexp-regular-expression-objects

const std = @import("std");

const libregexp = @import("../c/libregexp.zig");

const build_options = @import("build-options");
const builtins = @import("../builtins.zig");
const execution = @import("../execution.zig");
const gc = @import("../gc.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const Agent = execution.Agent;
const Arguments = types.Arguments;
const MakeObject = types.MakeObject;
const Object = types.Object;
const PropertyKey = types.PropertyKey;
const Realm = execution.Realm;
const String = types.String;
const Value = types.Value;
const arrayCreateFast = builtins.arrayCreateFast;
const arrayCreateFastWithShape = builtins.arrayCreateFastWithShape;
const createArrayFromList = types.createArrayFromList;
const createBuiltinFunction = builtins.createBuiltinFunction;
const createRegExpStringIterator = builtins.createRegExpStringIterator;
const getSubstitution = builtins.getSubstitution;
const noexcept = utils.noexcept;
const ordinaryCreateFromConstructor = builtins.ordinaryCreateFromConstructor;
const ordinaryObjectCreate = builtins.ordinaryObjectCreate;
const sameValue = types.sameValue;

pub const LreOpaque = struct {
    allocator: std.mem.Allocator,
};

var lre_alloc_sizes: if (!build_options.enable_libgc)
    std.AutoHashMapUnmanaged(*const anyopaque, usize)
else
    void = if (!build_options.enable_libgc) .empty;

export fn lre_check_stack_overflow(_: ?*anyopaque, _: usize) c_int {
    // TODO: Implement stack overflow check
    return 0;
}

export fn lre_check_timeout(_: ?*anyopaque) c_int {
    // This can be used to implement RegExp aborts e.g. on SIGINT.
    return 0;
}

export fn lre_realloc(@"opaque": ?*anyopaque, maybe_ptr: ?*anyopaque, size: usize) ?*anyopaque {
    const lre_opaque = @as(*LreOpaque, @ptrCast(@alignCast(@"opaque".?)));
    const old_mem: []u8 = if (maybe_ptr) |ptr| blk: {
        var old_mem: []u8 = @as(*[0]u8, @ptrCast(ptr));
        old_mem.len = if (build_options.enable_libgc)
            gc.GcAllocator.alignedAllocSize(old_mem.ptr)
        else
            lre_alloc_sizes.fetchRemove(ptr).?.value;
        break :blk old_mem;
    } else &.{};
    const new_mem = lre_opaque.allocator.realloc(old_mem, size) catch return null;
    if (!build_options.enable_libgc) {
        lre_alloc_sizes.put(lre_opaque.allocator, new_mem.ptr, new_mem.len) catch {
            lre_opaque.allocator.free(new_mem);
            return null;
        };
    }
    return new_mem.ptr;
}

pub const ParsedFlags = packed struct(u8) {
    d: bool = false,
    g: bool = false,
    i: bool = false,
    m: bool = false,
    s: bool = false,
    u: bool = false,
    v: bool = false,
    y: bool = false,

    pub fn from(flags: []const u8) ?ParsedFlags {
        var parsed_flags: ParsedFlags = .{};
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

    pub fn asLreFlags(self: ParsedFlags) c_int {
        var flags: c_int = 0;
        if (self.d) flags |= libregexp.c.LRE_FLAG_INDICES;
        if (self.g) flags |= libregexp.c.LRE_FLAG_GLOBAL;
        if (self.i) flags |= libregexp.c.LRE_FLAG_IGNORECASE;
        if (self.m) flags |= libregexp.c.LRE_FLAG_MULTILINE;
        if (self.s) flags |= libregexp.c.LRE_FLAG_DOTALL;
        if (self.u) flags |= libregexp.c.LRE_FLAG_UNICODE;
        if (self.v) flags |= libregexp.c.LRE_FLAG_UNICODE_SETS;
        if (self.y) flags |= libregexp.c.LRE_FLAG_STICKY;
        return flags;
    }
};

/// 22.2.3.1 RegExpCreate ( P, F )
/// https://tc39.es/ecma262/#sec-regexpcreate
pub fn regExpCreate(agent: *Agent, pattern: Value, flags: Value) Agent.Error!*RegExp {
    const realm = agent.currentRealm();

    // 1. Let obj be ! RegExpAlloc(%RegExp%).
    const reg_exp = regExpAlloc(
        agent,
        try realm.intrinsics.@"%RegExp%"(),
    ) catch |err| try noexcept(err);

    // 2. Return ? RegExpInitialize(obj, P, F).
    return regExpInitialize(agent, reg_exp, pattern, flags);
}

/// 22.2.3.2 RegExpAlloc ( newTarget )
/// https://tc39.es/ecma262/#sec-regexpalloc
pub fn regExpAlloc(agent: *Agent, new_target: *Object) Agent.Error!*RegExp {
    // 1. Let obj be ? OrdinaryCreateFromConstructor(newTarget, "%RegExp.prototype%",
    //    « [[OriginalSource]], [[OriginalFlags]], [[RegExpRecord]], [[RegExpMatcher]] »).
    const reg_exp = try ordinaryCreateFromConstructor(
        RegExp,
        agent,
        new_target,
        "%RegExp.prototype%",
        .{
            .original_source = undefined,
            .original_flags = undefined,
            .re_bytecode = undefined,
        },
    );

    // 2. Perform ! DefinePropertyOrThrow(obj, "lastIndex", PropertyDescriptor {
    //      [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false
    //    }).
    try reg_exp.object.definePropertyDirect(agent, PropertyKey.from("lastIndex"), .{
        .value_or_accessor = .{
            .value = .undefined,
        },
        .attributes = .{
            .writable = true,
            .enumerable = false,
            .configurable = false,
        },
    });

    // 3. Return obj.
    return reg_exp;
}

/// 22.2.3.3 RegExpInitialize ( obj, pattern, flags )
/// https://tc39.es/ecma262/#sec-regexpinitialize
pub fn regExpInitialize(
    agent: *Agent,
    reg_exp: *RegExp,
    pattern: Value,
    flags: Value,
) Agent.Error!*RegExp {
    if (!build_options.enable_libregexp) {
        return agent.throwException(.internal_error, "RegExp support is disabled", .{});
    }

    // 1. If pattern is undefined, let P be the empty String.
    // 2. Else, let P be ? ToString(pattern).
    const p: *const String = if (pattern.isUndefined()) .empty else try pattern.toString(agent);

    // 3. If flags is undefined, let F be the empty String.
    // 4. Else, let F be ? ToString(flags).
    const f: *const String = if (flags.isUndefined()) .empty else try flags.toString(agent);

    // 5. If F contains any code unit other than "d", "g", "i", "m", "s", "u", "v", or "y", or if F
    //    contains any code unit more than once, throw a SyntaxError exception.
    // 6. If F contains "i", let i be true; else let i be false.
    // 7. If F contains "m", let m be true; else let m be false.
    // 8. If F contains "s", let s be true; else let s be false.
    // 9. If F contains "u", let u be true; else let u be false.
    // 10. If F contains "v", let v be true; else let v be false.
    const parsed_flags = ParsedFlags.from(try f.toUtf8(agent.gc_allocator)) orelse {
        return agent.throwException(.syntax_error, "Invalid RegExp flags '{f}'", .{f.fmtEscaped()});
    };

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
    // NOTE: Despite passing in the buffer length below, this needs to be null-terminated.
    const buf = try agent.gc_allocator.dupeZ(u8, try p.toUtf8(agent.gc_allocator));
    defer agent.gc_allocator.free(buf);
    var @"opaque": LreOpaque = .{ .allocator = agent.gc_allocator };
    const re_bytecode = libregexp.c.lre_compile(
        &re_bytecode_len,
        &error_msg,
        error_msg.len,
        buf.ptr,
        buf.len,
        parsed_flags.asLreFlags(),
        &@"opaque",
    ) orelse {
        const str = std.mem.span(@as([*:0]const u8, @ptrCast(&error_msg)));
        if (std.mem.eql(u8, str, "out of memory")) return error.OutOfMemory;
        return agent.throwException(.syntax_error, "Invalid RegExp pattern: {s}", .{str});
    };

    // 16. Set obj.[[OriginalSource]] to P.
    reg_exp.fields.original_source = p;

    // 17. Set obj.[[OriginalFlags]] to F.
    reg_exp.fields.original_flags = f;

    // 18. Let capturingGroupsCount be CountLeftCapturingParensWithin(parseResult).
    // 19. Let rer be the RegExp Record {
    //       [[IgnoreCase]]: i, [[Multiline]]: m, [[DotAll]]: s, [[Unicode]]: u, [[UnicodeSets]]: v,
    //       [[CapturingGroupsCount]]: capturingGroupsCount
    //     }.
    // 20. Set obj.[[RegExpRecord]] to rer.
    // 21. Set obj.[[RegExpMatcher]] to CompilePattern of parseResult with argument rer.
    reg_exp.fields.re_bytecode = re_bytecode[0..@intCast(re_bytecode_len)];

    // 22. Perform ? Set(obj, "lastIndex", +0𝔽, true).
    try reg_exp.object.set(agent, PropertyKey.from("lastIndex"), Value.from(0), .throw);

    // 23. Return obj.
    return reg_exp;
}

/// 22.2.7.1 RegExpExec ( R, S )
/// https://tc39.es/ecma262/#sec-regexpexec
pub fn regExpExec(agent: *Agent, reg_exp: *Object, string: *const String) Agent.Error!?*Object {
    // 1. Let exec be ? Get(R, "exec").
    const exec = try reg_exp.get(agent, PropertyKey.from("exec"));

    // 2. If IsCallable(exec) is true, then
    if (exec.isCallable()) {
        // a. Let result be ? Call(exec, R, « S »).
        const result = try exec.callAssumeCallable(
            agent,
            Value.from(reg_exp),
            &.{Value.from(string)},
        );

        // b. If result is not an Object and result is not null, throw a TypeError exception.
        if (!result.isObject() and !result.isNull()) {
            return agent.throwException(
                .type_error,
                "RegExp exec function must return object or null",
                .{},
            );
        }

        // c. Return result.
        return if (result.isObject()) result.asObject() else null;
    }

    // 3. Perform ? RequireInternalSlot(R, [[RegExpMatcher]]).
    // 4. Return ? RegExpBuiltinExec(R, S).
    return regExpBuiltinExec(
        agent,
        try Value.from(reg_exp).requireInternalSlot(agent, RegExp),
        string,
    );
}

fn getMatch(captures_list: []?*u8, string: [*]const u8, shift: bool, i: usize) ?Match {
    const match = captures_list[2 * i ..][0..2].*;
    if (match[0] == null or match[1] == null) return null;
    const start_index = (@intFromPtr(match[0]) - @intFromPtr(string)) >> @intFromBool(shift);
    const end_index = (@intFromPtr(match[1]) - @intFromPtr(string)) >> @intFromBool(shift);
    return .{ .start_index = @intCast(start_index), .end_index = @intCast(end_index) };
}

/// 22.2.7.2 RegExpBuiltinExec ( R, S )
/// https://tc39.es/ecma262/#sec-regexpbuiltinexec
pub fn regExpBuiltinExec(agent: *Agent, reg_exp: *RegExp, string: *const String) Agent.Error!?*Object {
    if (!build_options.enable_libregexp) {
        return agent.throwException(.internal_error, "RegExp support is disabled", .{});
    }

    // 1. Let length be the length of S.
    const length = string.length;

    // 2. Let lastIndex be ℝ(? ToLength(! Get(R, "lastIndex"))).
    const last_index_value = reg_exp.object.getPropertyValueDirect(PropertyKey.from("lastIndex"));
    var last_index = std.math.lossyCast(u32, try last_index_value.toLength(agent));

    const re_bytecode = reg_exp.fields.re_bytecode;
    const capture_count: usize = @intCast(libregexp.c.lre_get_capture_count(@ptrCast(re_bytecode)));

    // libregexp's capture count includes the matched string
    std.debug.assert(capture_count >= 1);

    const captures_list = try agent.gc_allocator.alloc(?*u8, capture_count * 2);

    // 3. Let flags be R.[[OriginalFlags]].
    const re_flags = libregexp.c.lre_get_flags(@ptrCast(re_bytecode));

    // 4. If flags contains "g", let global be true; else let global be false.
    // 5. If flags contains "y", let sticky be true; else let sticky be false.
    // 6. If flags contains "d", let hasIndices be true; else let hasIndices be false.

    // 7. If global is false and sticky is false, set lastIndex to 0.
    if ((re_flags & (libregexp.c.LRE_FLAG_GLOBAL | libregexp.c.LRE_FLAG_STICKY)) == 0) {
        last_index = 0;
    }

    // 8-13.
    const shift = string.isUtf16();
    const buf: [*]const u8 = switch (string.asAsciiOrUtf16()) {
        .ascii => |ascii| ascii.ptr,
        .utf16 => |utf16| @ptrCast(utf16.ptr),
    };
    var @"opaque": LreOpaque = .{ .allocator = agent.gc_allocator };
    const ret = if (last_index > length) 0 else libregexp.c.lre_exec(
        @ptrCast(captures_list),
        @ptrCast(re_bytecode),
        buf,
        @intCast(last_index),
        @intCast(string.length),
        // 0 = 8 bit chars, 1 = 16 bit chars, 2 = 16 bit chars, UTF-16 (set internally via the u flag)
        switch (string.asAsciiOrUtf16()) {
            .ascii => 0,
            .utf16 => 1,
        },
        &@"opaque",
    );

    if (ret < 0) {
        const reason = switch (ret) {
            libregexp.c.LRE_RET_MEMORY_ERROR => "Out of memory",
            libregexp.c.LRE_RET_TIMEOUT => "Timeout exceeded",
            else => unreachable,
        };
        return agent.throwException(.internal_error, "Failed to execute RegExp: {s}", .{reason});
    }
    if (ret == 0) {
        if (last_index > length or (re_flags & (libregexp.c.LRE_FLAG_GLOBAL | libregexp.c.LRE_FLAG_STICKY)) != 0) {
            try reg_exp.object.set(
                agent,
                PropertyKey.from("lastIndex"),
                Value.from(0),
                .throw,
            );
        }
        return null;
    }
    var match = getMatch(captures_list, buf, shift, 0).?;
    last_index = match.start_index;

    // 14. Let e be r.[[EndIndex]].
    // 15. If fullUnicode is true, set e to GetStringIndex(S, e).
    const end_index = match.end_index;

    // 16. If global is true or sticky is true, then
    if ((re_flags & (libregexp.c.LRE_FLAG_GLOBAL | libregexp.c.LRE_FLAG_STICKY)) != 0) {
        // a. Perform ? Set(R, "lastIndex", 𝔽(e), true).
        try reg_exp.object.set(
            agent,
            PropertyKey.from("lastIndex"),
            Value.from(end_index),
            .throw,
        );
    }

    // 17. Let n be the number of elements in r.[[Captures]].
    const n = capture_count - 1;

    // 18. Assert: n = R.[[RegExpRecord]].[[CapturingGroupsCount]].
    // 19. Assert: n < 2**32 - 1.
    std.debug.assert(n < std.math.maxInt(u32));

    // 20. Let A be ! ArrayCreate(n + 1).
    // 21. Assert: The mathematical value of A's "length" property is n + 1.
    const realm = agent.currentRealm();
    const array_shape, const array_indices = try realm.shapes.regExpExecObject();
    const array = try arrayCreateFastWithShape(agent, @intCast(n + 1), array_shape);

    // 22. Perform ! CreateDataPropertyOrThrow(A, "index", 𝔽(lastIndex)).
    array.object.setValueAtPropertyIndex(array_indices.index, Value.from(last_index));

    // 23. Perform ! CreateDataPropertyOrThrow(A, "input", S).
    array.object.setValueAtPropertyIndex(array_indices.input, Value.from(string));

    // 24. Let match be the Match Record { [[StartIndex]]: lastIndex, [[EndIndex]]: e }.
    match = .{ .start_index = last_index, .end_index = end_index };

    // 25. Let indices be a new empty List.
    var indices: std.ArrayList(?Match) = .empty;
    defer indices.deinit(agent.gc_allocator);

    // 26. Let groupNames be a new empty List.
    var group_names: std.ArrayList(?[]const u8) = .empty;
    defer group_names.deinit(agent.gc_allocator);

    // 27. Append match to indices.
    try indices.append(agent.gc_allocator, match);

    // 28. Let matchedSubstr be GetMatchString(S, match).
    const matched_substr = try getMatchString(agent, string, match);

    // 29. Perform ! CreateDataPropertyOrThrow(A, "0", matchedSubstr).
    try array.object.createDataPropertyDirect(agent, PropertyKey.from(0), Value.from(matched_substr));

    var group_name_ptr = libregexp.c.lre_get_groupnames(@ptrCast(re_bytecode));
    const has_groups = group_name_ptr != null;

    // 30. If R contains any GroupName, then
    const groups: Value = if (has_groups) blk: {
        // a. Let groups be OrdinaryObjectCreate(null).
        break :blk Value.from(try ordinaryObjectCreate(agent, null));

        // b. Let hasGroups be true.
    } else blk: {
        // 31. Else,
        // a. Let groups be undefined.
        break :blk .undefined;

        // b. Let hasGroups be false.
    };

    // 32. Perform ! CreateDataPropertyOrThrow(A, "groups", groups).
    array.object.setValueAtPropertyIndex(array_indices.groups, groups);

    // 33. Let matchedGroupNames be a new empty List.
    var matched_group_names: std.StringHashMapUnmanaged(void) = .empty;
    defer matched_group_names.deinit(agent.gc_allocator);

    // 34. For each integer i such that 1 ≤ i ≤ n, in ascending order, do
    var i: usize = 1;
    while (i <= n) : (i += 1) {
        var captured_value: Value = undefined;

        // a. Let captureI be ith element of r.[[Captures]].
        const capture_i = getMatch(captures_list, buf, shift, i);

        // b. If captureI is undefined, then
        if (capture_i == null) {
            // i. Let capturedValue be undefined.
            captured_value = .undefined;

            // ii. Append undefined to indices.
            try indices.append(agent.gc_allocator, null);
        } else {
            // c. Else,
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
            try indices.append(agent.gc_allocator, capture);
        }

        // d. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(i)), capturedValue).
        // OPTIMIZATION: Because the array is created with the right length we can set indexed
        //               properties directly here.
        try array.object.property_storage.indexed_properties.set(
            agent.gc_allocator,
            @intCast(i),
            .{
                .value_or_accessor = .{
                    .value = captured_value,
                },
                .attributes = .all,
            },
        );

        // e. If the ith capture of R was defined with a GroupName, then
        if (group_name_ptr != null and group_name_ptr.* != 0) {
            // i. Let s be the CapturingGroupName of that GroupName.
            const group_name = std.mem.span(group_name_ptr);
            group_name_ptr += group_name.len + libregexp.c.LRE_GROUP_NAME_TRAILER_LEN;

            // ii. If matchedGroupNames contains s, then
            if (matched_group_names.contains(group_name)) {
                // 1. Assert: capturedValue is undefined.
                std.debug.assert(captured_value.isUndefined());

                // 2. Append undefined to groupNames.
                try group_names.append(agent.gc_allocator, null);
            } else {
                // iii. Else,
                // 1. If capturedValue is not undefined, append s to matchedGroupNames.
                if (!captured_value.isUndefined()) {
                    try matched_group_names.put(agent.gc_allocator, group_name, {});
                }

                // 2. NOTE: If there are multiple groups named s, groups may already have an s
                //    property at this point. However, because groups is an ordinary object whose
                //    properties are all writable data properties, the call to
                //    CreateDataPropertyOrThrow is nevertheless guaranteed to succeed.

                // 3. Perform ! CreateDataPropertyOrThrow(groups, s, capturedValue).
                const property_key = PropertyKey.from(
                    try String.fromUtf8(agent, group_name),
                );
                try groups.asObject().createDataPropertyDirect(agent, property_key, captured_value);

                // 4. Append s to groupNames.
                try group_names.append(agent.gc_allocator, group_name);
            }
        } else {
            // f. Else,
            // i. Append undefined to groupNames.
            try group_names.append(agent.gc_allocator, null);
        }
    }

    // 35. If hasIndices is true, then
    if ((re_flags & libregexp.c.LRE_FLAG_INDICES) != 0) {
        // a. Let indicesArray be MakeMatchIndicesIndexPairArray(S, indices, groupNames, hasGroups).
        const indices_array = try makeMatchIndicesIndexPairArray(
            agent,
            string,
            indices.items,
            group_names.items,
            has_groups,
        );

        // b. Perform ! CreateDataPropertyOrThrow(A, "indices", indicesArray).
        try array.object.createDataPropertyDirect(
            agent,
            PropertyKey.from("indices"),
            Value.from(&indices_array.object),
        );
    }

    // 36. Return A.
    return &array.object;
}

/// 22.2.7.3 AdvanceStringIndex ( S, index, unicode )
/// https://tc39.es/ecma262/#sec-advancestringindex
pub fn advanceStringIndex(string: *const String, index: u53, unicode: bool) u53 {
    // 1. Assert: index ≤ 2**53 - 1.

    // 2. If unicode is false, return index + 1.
    if (!unicode) return index + 1;

    // 3. Let length be the length of S.
    const length = string.length;

    // 4. If index + 1 ≥ length, return index + 1.
    if (index + 1 >= length) return index + 1;

    // 5. Let cp be CodePointAt(S, index).
    const code_point = string.codePointAt(@intCast(index));

    // 6. Return index + cp.[[CodeUnitCount]].
    return index + code_point.code_unit_count;
}

/// 22.2.7.5 Match Records
/// https://tc39.es/ecma262/#sec-match-records
const Match = struct {
    /// [[StartIndex]]
    start_index: u32,

    /// [[EndIndex]]
    end_index: u32,
};

/// 22.2.7.6 GetMatchString ( S, match )
/// https://tc39.es/ecma262/#sec-getmatchstring
fn getMatchString(agent: *Agent, string: *const String, match: Match) std.mem.Allocator.Error!*const String {
    // 1. Assert: match.[[StartIndex]] ≤ match.[[EndIndex]] ≤ the length of S.
    std.debug.assert(match.start_index <= match.end_index);
    std.debug.assert(match.end_index <= string.length);

    // 2. Return the substring of S from match.[[StartIndex]] to match.[[EndIndex]].
    return string.substring(agent, match.start_index, match.end_index);
}

/// 22.2.7.7 GetMatchIndexPair ( S, match )
/// https://tc39.es/ecma262/#sec-getmatchindexpair
fn getMatchIndexPair(agent: *Agent, string: *const String, match: Match) std.mem.Allocator.Error!*builtins.Array {
    // 1. Assert: match.[[StartIndex]] ≤ match.[[EndIndex]] ≤ the length of S.
    std.debug.assert(match.start_index <= match.end_index);
    std.debug.assert(match.end_index <= string.length);

    // 2. Return CreateArrayFromList(« 𝔽(match.[[StartIndex]]), 𝔽(match.[[EndIndex]]) »).
    return createArrayFromList(agent, &.{
        Value.from(match.start_index),
        Value.from(match.end_index),
    });
}

/// 22.2.7.8 MakeMatchIndicesIndexPairArray ( S, indices, groupNames, hasGroups )
/// https://tc39.es/ecma262/#sec-makematchindicesindexpairarray
fn makeMatchIndicesIndexPairArray(
    agent: *Agent,
    string: *const String,
    indices: []const ?Match,
    group_names: []const ?[]const u8,
    has_groups: bool,
) std.mem.Allocator.Error!*builtins.Array {
    // 1. Let n be the number of elements in indices.
    const n = indices.len;

    // 2. Assert: n < 2**32 - 1.
    std.debug.assert(n < std.math.maxInt(u32));

    // 3. Assert: groupNames has n - 1 elements.
    // 4. NOTE: The groupNames List contains elements aligned with the indices List starting at
    //    indices[1].
    std.debug.assert(group_names.len == n - 1);

    // 5. Let A be ! ArrayCreate(n).
    const array = try arrayCreateFast(agent, 0);

    // 6. If hasGroups is true, then
    const groups: Value = if (has_groups) blk: {
        // a. Let groups be OrdinaryObjectCreate(null).
        break :blk Value.from(try ordinaryObjectCreate(agent, null));
    } else blk: {
        // 7. Else,
        // a. Let groups be undefined.
        break :blk .undefined;
    };

    // 8. Perform ! CreateDataPropertyOrThrow(A, "groups", groups).
    try array.object.createDataPropertyDirect(agent, PropertyKey.from("groups"), groups);

    // 9. For each integer i such that 0 ≤ i < n, in ascending order, do
    var i: usize = 0;
    while (i < n) : (i += 1) {
        // a. Let matchIndices be indices[i].
        const match_indices = indices[i];

        // b. If matchIndices is not undefined,
        const match_index_pair: Value = if (match_indices != null) blk: {
            // i. Let matchIndexPair be GetMatchIndexPair(S, matchIndices).
            const match_index_pair = try getMatchIndexPair(agent, string, match_indices.?);
            break :blk Value.from(&match_index_pair.object);
        } else blk: {
            // c. Else,
            // i. Let matchIndexPair be undefined.
            break :blk .undefined;
        };

        // d. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(i)), matchIndexPair).
        try array.object.createDataPropertyDirect(
            agent,
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(i))),
            match_index_pair,
        );

        // e. If i > 0, then
        if (i > 0) {
            // i. Let s be groupNames[i - 1].
            // ii. If s is not undefined, then
            if (group_names[i - 1]) |group_name| {
                // 1. Assert: groups is not undefined.
                std.debug.assert(!groups.isUndefined());

                // 2. NOTE: If there are multiple groups named s, groups may already have an s
                //    property at this point. However, because groups is an ordinary object whose
                //    properties are all writable data properties, the call to
                //    CreateDataPropertyOrThrow is nevertheless guaranteed to succeed.

                // 3. Perform ! CreateDataPropertyOrThrow(groups, s, matchIndexPair).
                const property_key = PropertyKey.from(
                    try String.fromUtf8(agent, try agent.gc_allocator.dupe(u8, group_name)),
                );
                try groups.asObject().createDataPropertyDirect(
                    agent,
                    property_key,
                    match_index_pair,
                );
            }
        }
    }

    // 10. Return A.
    return array;
}

/// 22.2.5 Properties of the RegExp Constructor
/// https://tc39.es/ecma262/#sec-properties-of-the-regexp-constructor
pub const constructor = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        const builtin_function = try createBuiltinFunction(
            agent,
            .{ .constructor = impl },
            2,
            "RegExp",
            .{ .realm = realm, .prototype = try realm.intrinsics.@"%Function.prototype%"() },
        );
        return &builtin_function.object;
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        try object.defineBuiltinFunction(agent, "escape", escape, 1, realm);
        try object.defineBuiltinAccessor(agent, "%Symbol.species%", @"%Symbol.species%", null, realm);

        // 22.2.5.2 RegExp.prototype
        // https://tc39.es/ecma262/#sec-regexp.prototype
        try object.defineBuiltinPropertyWithAttributes(
            agent,
            "prototype",
            Value.from(try realm.intrinsics.@"%RegExp.prototype%"()),
            .none,
        );
    }

    /// 22.2.4.1 RegExp ( pattern, flags )
    /// https://tc39.es/ecma262/#sec-regexp-pattern-flags
    fn impl(agent: *Agent, arguments: Arguments, new_target: ?*Object) Agent.Error!Value {
        const pattern = arguments.get(0);
        const flags = arguments.get(1);

        // 1. Let patternIsRegExp be ? IsRegExp(pattern).
        const pattern_is_regexp = try pattern.isRegExp(agent);

        var constructor_: *Object = undefined;

        // 2. If NewTarget is undefined, then
        if (new_target == null) {
            // a. Let newTarget be the active function object.
            constructor_ = agent.activeFunctionObject();

            // b. If patternIsRegExp is true and flags is undefined, then
            if (pattern_is_regexp and flags.isUndefined()) {
                // i. Let patternConstructor be ? Get(pattern, "constructor").
                const pattern_constructor = try pattern.asObject().get(agent, PropertyKey.from("constructor"));

                // ii. If SameValue(newTarget, patternConstructor) is true, return pattern.
                if (sameValue(Value.from(constructor_), pattern_constructor)) return pattern;
            }
        } else {
            // 3. Else,
            // a. Let newTarget be NewTarget.
            constructor_ = new_target.?;
        }

        var p: Value = undefined;
        var f: Value = undefined;

        // 4. If pattern is an Object and pattern has a [[RegExpMatcher]] internal slot, then
        if (pattern.castObject(RegExp)) |reg_exp| {
            // a. Let P be pattern.[[OriginalSource]].
            p = Value.from(reg_exp.fields.original_source);

            // b. If flags is undefined, let F be pattern.[[OriginalFlags]].
            if (flags.isUndefined()) {
                f = Value.from(reg_exp.fields.original_flags);
            }
            // c. Else, let F be flags.
            else {
                f = flags;
            }
        }
        // 5. Else if patternIsRegExp is true, then
        else if (pattern_is_regexp) {
            // a. Let P be ? Get(pattern, "source").
            p = try pattern.asObject().get(agent, PropertyKey.from("source"));

            // b. If flags is undefined, then
            if (flags.isUndefined()) {
                // i. Let F be ? Get(pattern, "flags").
                f = try pattern.asObject().get(agent, PropertyKey.from("flags"));
            } else {
                // c. Else,
                // i. Let F be flags.
                f = flags;
            }
        } else {
            // 6. Else,
            // a. Let P be pattern.
            p = pattern;

            // b. Let F be flags.
            f = flags;
        }

        // 7. Let O be ? RegExpAlloc(newTarget).
        const reg_exp = try regExpAlloc(agent, constructor_);

        // 8. Return ? RegExpInitialize(O, P, F).
        _ = try regExpInitialize(agent, reg_exp, p, f);
        return Value.from(&reg_exp.object);
    }

    /// 22.2.5.1 RegExp.escape ( S )
    /// https://tc39.es/ecma262/#sec-regexp.escape
    fn escape(agent: *Agent, _: Value, arguments: Arguments) Agent.Error!Value {
        const string_value = arguments.get(0);

        // 1. If S is not a String, throw a TypeError exception.
        if (!string_value.isString()) {
            return agent.throwException(.type_error, "{f} is not a string", .{string_value});
        }
        const string = string_value.asString();

        // 2. Let escaped be the empty String.
        var escaped: String.Builder = .empty;
        defer escaped.deinit(agent.gc_allocator);

        // 3. Let cpList be StringToCodePoints(S).
        // 4. For each code point cp of cpList, do
        var position: u32 = 0;
        while (position < string.length) {
            const cp = string.codePointAt(position);
            defer position += cp.code_unit_count;

            // a. If escaped is the empty String and cp is matched by either DecimalDigit or
            //    AsciiLetter, then
            if (position == 0 and std.ascii.isAlphanumeric(std.math.cast(u8, cp.code_point) orelse 0)) {
                // i. NOTE: Escaping a leading digit ensures that output corresponds with pattern
                //    text which may be used after a \0 character escape or a DecimalEscape such as
                //    \1 and still match S rather than be interpreted as an extension of the
                //    preceding escape sequence. Escaping a leading ASCII letter does the same for
                //    the context after \c.
                // ii. Let numericValue be the numeric value of cp.
                // iii. Let hex be Number::toString(𝔽(numericValue), 16).
                // iv. Assert: The length of hex is 2.
                // v. Set escaped to the string-concatenation of the code unit 0x005C (REVERSE
                //    SOLIDUS), "x", and hex.
                try escaped.appendString(
                    agent.gc_allocator,
                    try String.fromAscii(
                        agent,
                        try std.fmt.allocPrint(agent.gc_allocator, "\\x{x}", .{cp.code_point}),
                    ),
                );
            } else {
                // b. Else,
                // i. Set escaped to the string-concatenation of escaped and EncodeForRegExpEscape(cp).
                try encodeForRegExpEscape(agent.gc_allocator, &escaped, cp);
            }
        }

        // 5. Return escaped.
        return Value.from(try escaped.build(agent));
    }

    /// 22.2.5.1.1 EncodeForRegExpEscape ( cp )
    /// https://tc39.es/ecma262/#sec-encodeforregexpescape
    fn encodeForRegExpEscape(
        allocator: std.mem.Allocator,
        escaped: *String.Builder,
        cp: String.CodePoint,
    ) std.mem.Allocator.Error!void {
        var hex_escape = false;
        switch (cp.code_point) {
            // 1. If cp is matched by SyntaxCharacter or cp is U+002F (SOLIDUS), then
            '^', '$', '\\', '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|', '/' => {
                // a. Return the string-concatenation of 0x005C (REVERSE SOLIDUS) and
                //    UTF16EncodeCodePoint(cp).
                try escaped.appendChar(allocator, '\\');
                try escaped.appendChar(allocator, @intCast(cp.code_point));
                return;
            },

            // 2. Else if cp is a code point listed in the “Code Point” column of Table 67, then
            '\t'...'\r' => {
                // a. Return the string-concatenation of 0x005C (REVERSE SOLIDUS) and the
                //    string in the “ControlEscape” column of the row whose “Code Point” column
                //    contains cp.
                try escaped.appendChar(allocator, '\\');
                try escaped.appendChar(
                    allocator,
                    ([5]u8{ 't', 'n', 'v', 'f', 'r' })[cp.code_point - 0x09],
                );
                return;
            },

            // 3. Let otherPunctuators be the string-concatenation of ",-=<>#&!%:;@~'`" and the
            //    code unit 0x0022 (QUOTATION MARK).
            // 4. Let toEscape be StringToCodePoints(otherPunctuators).
            ',', '-', '=', '<', '>', '#', '&', '!', '%', ':', ';', '@', '~', '\'', '`', '"' => {
                hex_escape = true;
            },

            else => {},
        }

        // 5. If toEscape contains cp, cp is matched by either WhiteSpace or LineTerminator, or cp
        //    has the same numeric value as a leading surrogate or trailing surrogate, then
        if (hex_escape or
            std.mem.indexOfScalar(u21, &String.whitespace_code_points, cp.code_point) != null or
            cp.is_unpaired_surrogate)
        {
            // a. Let cpNum be the numeric value of cp.
            // b. If cpNum ≤ 0xFF, then
            if (cp.code_point <= 0xff) {
                // i. Let hex be Number::toString(𝔽(cpNum), 16).
                // ii. Return the string-concatenation of the code unit 0x005C (REVERSE SOLIDUS),
                //     "x", and StringPad(hex, 2, "0", start).
                const str = try std.fmt.allocPrint(allocator, "\\x{x:0>2}", .{cp.code_point});
                // TODO: Support appending an ASCII slice to String.Builder
                for (str) |c| {
                    try escaped.appendChar(allocator, c);
                }
                return;
            }

            // c. Let escaped be the empty String.
            // d. Let codeUnits be UTF16EncodeCodePoint(cp).
            // e. For each code unit cu of codeUnits, do
            // i. Set escaped to the string-concatenation of escaped and UnicodeEscape(cu).
            // f. Return escaped.
            const str = try std.fmt.allocPrint(allocator, "\\u{x:0>4}", .{cp.code_point});
            // TODO: Support appending an ASCII slice to String.Builder
            for (str) |c| {
                try escaped.appendChar(allocator, c);
            }
            return;
        }

        // 6. Return UTF16EncodeCodePoint(cp).
        try escaped.appendCodePoint(allocator, cp.code_point);
    }

    /// 22.2.5.3 get RegExp [ %Symbol.species% ]
    /// https://tc39.es/ecma262/#sec-get-regexp-%symbol.species%
    fn @"%Symbol.species%"(_: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Return the this value.
        return this_value;
    }
};

/// 22.2.6 Properties of the RegExp Prototype Object
/// https://tc39.es/ecma262/#sec-properties-of-the-regexp-prototype-object
pub const prototype = struct {
    pub fn create(agent: *Agent, realm: *Realm) std.mem.Allocator.Error!*Object {
        return ordinaryObjectCreate(agent, try realm.intrinsics.@"%Object.prototype%"());
    }

    pub fn init(agent: *Agent, realm: *Realm, object: *Object) std.mem.Allocator.Error!void {
        if (!build_options.enable_libregexp) return;

        try object.defineBuiltinAccessor(agent, "dotAll", dotAll, null, realm);
        try object.defineBuiltinFunction(agent, "exec", exec, 1, realm);
        try object.defineBuiltinAccessor(agent, "flags", flags, null, realm);
        try object.defineBuiltinAccessor(agent, "global", global, null, realm);
        try object.defineBuiltinAccessor(agent, "hasIndices", hasIndices, null, realm);
        try object.defineBuiltinAccessor(agent, "ignoreCase", ignoreCase, null, realm);
        try object.defineBuiltinFunction(agent, "%Symbol.match%", @"%Symbol.match%", 1, realm);
        try object.defineBuiltinFunction(agent, "%Symbol.matchAll%", @"%Symbol.matchAll%", 1, realm);
        try object.defineBuiltinAccessor(agent, "multiline", multiline, null, realm);
        try object.defineBuiltinFunction(agent, "%Symbol.replace%", @"%Symbol.replace%", 2, realm);
        try object.defineBuiltinFunction(agent, "%Symbol.search%", @"%Symbol.search%", 1, realm);
        try object.defineBuiltinAccessor(agent, "source", source, null, realm);
        try object.defineBuiltinFunction(agent, "%Symbol.split%", @"%Symbol.split%", 2, realm);
        try object.defineBuiltinAccessor(agent, "sticky", sticky, null, realm);
        try object.defineBuiltinFunction(agent, "test", @"test", 1, realm);
        try object.defineBuiltinFunction(agent, "toString", toString, 0, realm);
        try object.defineBuiltinAccessor(agent, "unicode", unicode, null, realm);
        try object.defineBuiltinAccessor(agent, "unicodeSets", unicodeSets, null, realm);

        // 22.2.6.1 RegExp.prototype.constructor
        // https://tc39.es/ecma262/#sec-regexp.prototype.constructor
        try object.defineBuiltinProperty(
            agent,
            "constructor",
            Value.from(try realm.intrinsics.@"%RegExp%"()),
        );

        if (build_options.enable_annex_b) {
            try object.defineBuiltinFunction(agent, "compile", compile, 2, realm);
        }
    }

    /// 22.2.6.2 RegExp.prototype.exec ( string )
    /// https://tc39.es/ecma262/#sec-regexp.prototype.exec
    fn exec(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
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
    fn dotAll(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0073 (LATIN SMALL LETTER S).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.c.LRE_FLAG_DOTALL);
    }

    /// 22.2.6.4 get RegExp.prototype.flags
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.flags
    fn flags(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. If R is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const reg_exp = this_value.asObject();

        // 3. Let codeUnits be a new empty List.
        var code_units = try std.ArrayList(u8).initCapacity(agent.gc_allocator, 8);

        // 4. Let hasIndices be ToBoolean(? Get(R, "hasIndices")).
        // 5. If hasIndices is true, append the code unit 0x0064 (LATIN SMALL LETTER D) to codeUnits.
        if ((try reg_exp.get(agent, PropertyKey.from("hasIndices"))).toBoolean()) {
            code_units.appendAssumeCapacity('d');
        }

        // 6. Let global be ToBoolean(? Get(R, "global")).
        // 7. If global is true, append the code unit 0x0067 (LATIN SMALL LETTER G) to codeUnits.
        if ((try reg_exp.get(agent, PropertyKey.from("global"))).toBoolean()) {
            code_units.appendAssumeCapacity('g');
        }

        // 8. Let ignoreCase be ToBoolean(? Get(R, "ignoreCase")).
        // 9. If ignoreCase is true, append the code unit 0x0069 (LATIN SMALL LETTER I) to codeUnits.
        if ((try reg_exp.get(agent, PropertyKey.from("ignoreCase"))).toBoolean()) {
            code_units.appendAssumeCapacity('i');
        }

        // 10. Let multiline be ToBoolean(? Get(R, "multiline")).
        // 11. If multiline is true, append the code unit 0x006D (LATIN SMALL LETTER M) to codeUnits.
        if ((try reg_exp.get(agent, PropertyKey.from("multiline"))).toBoolean()) {
            code_units.appendAssumeCapacity('m');
        }

        // 12. Let dotAll be ToBoolean(? Get(R, "dotAll")).
        // 13. If dotAll is true, append the code unit 0x0073 (LATIN SMALL LETTER S) to codeUnits.
        if ((try reg_exp.get(agent, PropertyKey.from("dotAll"))).toBoolean()) {
            code_units.appendAssumeCapacity('s');
        }

        // 14. Let unicode be ToBoolean(? Get(R, "unicode")).
        // 15. If unicode is true, append the code unit 0x0075 (LATIN SMALL LETTER U) to codeUnits.
        if ((try reg_exp.get(agent, PropertyKey.from("unicode"))).toBoolean()) {
            code_units.appendAssumeCapacity('u');
        }

        // 16. Let unicodeSets be ToBoolean(? Get(R, "unicodeSets")).
        // 17. If unicodeSets is true, append the code unit 0x0076 (LATIN SMALL LETTER V) to codeUnits.
        if ((try reg_exp.get(agent, PropertyKey.from("unicodeSets"))).toBoolean()) {
            code_units.appendAssumeCapacity('v');
        }

        // 18. Let sticky be ToBoolean(? Get(R, "sticky")).
        // 19. If sticky is true, append the code unit 0x0079 (LATIN SMALL LETTER Y) to codeUnits.
        if ((try reg_exp.get(agent, PropertyKey.from("sticky"))).toBoolean()) {
            code_units.appendAssumeCapacity('y');
        }

        // 20. Return the String value whose code units are the elements of the List codeUnits. If
        //     codeUnits has no elements, the empty String is returned.
        return Value.from(
            try String.fromAscii(
                agent,
                try code_units.toOwnedSlice(agent.gc_allocator),
            ),
        );
    }

    /// 22.2.6.4.1 RegExpHasFlag ( R, codeUnit )
    /// https://tc39.es/ecma262/#sec-regexphasflag
    fn regExpHasFlag(agent: *Agent, reg_exp_value: Value, flag: c_int) Agent.Error!Value {
        // 1. If R is not an Object, throw a TypeError exception.
        if (!reg_exp_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{reg_exp_value});
        }

        // 2. If R does not have an [[OriginalFlags]] internal slot, then
        const reg_exp = reg_exp_value.asObject().cast(RegExp) orelse {
            const realm = agent.currentRealm();

            // a. If SameValue(R, %RegExp.prototype%) is true, return undefined.
            if (reg_exp_value.asObject() == try realm.intrinsics.@"%RegExp.prototype%"()) {
                return .undefined;
            }

            // b. Otherwise, throw a TypeError exception.
            return agent.throwException(.type_error, "This value must be a RegExp object", .{});
        };

        // 3. Let flags be R.[[OriginalFlags]].
        const re_bytecode = reg_exp.fields.re_bytecode;
        const re_flags = libregexp.c.lre_get_flags(@ptrCast(re_bytecode));

        // 4. If flags contains codeUnit, return true.
        // 5. Return false.
        return Value.from((re_flags & flag) != 0);
    }

    /// 22.2.6.5 get RegExp.prototype.global
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.global
    fn global(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0067 (LATIN SMALL LETTER G).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.c.LRE_FLAG_GLOBAL);
    }

    /// 22.2.6.6 get RegExp.prototype.hasIndices
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.global
    fn hasIndices(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0064 (LATIN SMALL LETTER D).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.c.LRE_FLAG_INDICES);
    }

    /// 22.2.6.7 get RegExp.prototype.ignoreCase
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.ignorecase
    fn ignoreCase(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0069 (LATIN SMALL LETTER I).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.c.LRE_FLAG_IGNORECASE);
    }

    /// 22.2.6.8 RegExp.prototype [ %Symbol.match% ] ( string )
    /// https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.match%
    fn @"%Symbol.match%"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const string_value = arguments.get(0);

        // 1. Let rx be the this value.
        // 2. If rx is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const reg_exp = this_value.asObject();

        // 3. Let S be ? ToString(string).
        const string = try string_value.toString(agent);

        // 4. Let flags be ? ToString(? Get(rx, "flags")).
        const flags_ = try (try reg_exp.get(agent, PropertyKey.from("flags"))).toString(agent);

        // 5. If flags does not contain "g", then
        if (flags_.indexOf(String.fromLiteral("g"), 0) == null) {
            // a. Return ? RegExpExec(rx, S).
            return if (try regExpExec(agent, reg_exp, string)) |object|
                Value.from(object)
            else
                .null;
        } else {
            // 6. Else,
            // a. If flags contains "u" or flags contains "v", let fullUnicode be true; otherwise
            //    let fullUnicode be false.
            const full_unicode = flags_.indexOf(String.fromLiteral("u"), 0) != null or
                flags_.indexOf(String.fromLiteral("v"), 0) != null;

            // b. Perform ? Set(rx, "lastIndex", +0𝔽, true).
            try reg_exp.set(agent, PropertyKey.from("lastIndex"), Value.from(0), .throw);

            // c. Let A be ! ArrayCreate(0).
            const array = try arrayCreateFast(agent, 0);

            // d. Let n be 0.
            var n: u53 = 0;

            // e. Repeat,
            while (true) : (n += 1) {
                // i. Let result be ? RegExpExec(rx, S).
                const result = try regExpExec(agent, reg_exp, string);

                // ii. If result is null, then
                if (result == null) {
                    // 1. If n = 0, return null.
                    if (n == 0) return .null;

                    // 2. Return A.
                    return Value.from(&array.object);
                } else {
                    // iii. Else,
                    // 1. Let matchStr be ? ToString(? Get(result, "0")).
                    const match_str = try (try result.?.get(agent, PropertyKey.from(0))).toString(agent);

                    // 2. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), matchStr).
                    try array.object.createDataPropertyDirect(
                        agent,
                        PropertyKey.from(n),
                        Value.from(match_str),
                    );

                    // 3. If matchStr is the empty String, then
                    if (match_str.isEmpty()) {
                        // a. Let thisIndex be ℝ(? ToLength(? Get(rx, "lastIndex"))).
                        const this_index = try (try reg_exp.get(
                            agent,
                            PropertyKey.from("lastIndex"),
                        )).toLength(agent);

                        // b. Let nextIndex be AdvanceStringIndex(S, thisIndex, fullUnicode).
                        const next_index = advanceStringIndex(string, this_index, full_unicode);

                        // c. Perform ? Set(rx, "lastIndex", 𝔽(nextIndex), true).
                        try reg_exp.set(agent, PropertyKey.from("lastIndex"), Value.from(next_index), .throw);
                    }

                    // 4. Set n to n + 1.
                }
            }
        }
    }

    /// 22.2.6.9 RegExp.prototype [ %Symbol.matchAll% ] ( string )
    /// https://tc39.es/ecma262/#sec-regexp-prototype-%symbol.matchall%
    fn @"%Symbol.matchAll%"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const string_value = arguments.get(0);
        const realm = agent.currentRealm();

        // 1. Let R be the this value.
        // 2. If R is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const reg_exp = this_value.asObject();

        // 3. Let S be ? ToString(string).
        const string = try string_value.toString(agent);

        // 4. Let C be ? SpeciesConstructor(R, %RegExp%).
        const constructor_ = try reg_exp.speciesConstructor(
            agent,
            try realm.intrinsics.@"%RegExp%"(),
        );

        // 5. Let flags be ? ToString(? Get(R, "flags")).
        const flags_ = try (try reg_exp.get(agent, PropertyKey.from("flags"))).toString(agent);

        // 6. Let matcher be ? Construct(C, « R, flags »).
        const matcher = try constructor_.construct(
            agent,
            &.{ Value.from(reg_exp), Value.from(flags_) },
            null,
        );

        // 7. Let lastIndex be ? ToLength(? Get(R, "lastIndex")).
        const last_index = try (try reg_exp.get(agent, PropertyKey.from("lastIndex"))).toLength(agent);

        // 8. Perform ? Set(matcher, "lastIndex", lastIndex, true).
        try matcher.set(agent, PropertyKey.from("lastIndex"), Value.from(last_index), .throw);

        // 9. If flags contains "g", let global be true.
        // 10. Else, let global be false.
        const global_ = flags_.indexOf(String.fromLiteral("g"), 0) != null;

        // 11. If flags contains "u" or flags contains "v", let fullUnicode be true.
        // 12. Else, let fullUnicode be false.
        const full_unicode = flags_.indexOf(String.fromLiteral("u"), 0) != null or
            flags_.indexOf(String.fromLiteral("v"), 0) != null;

        // 13. Return CreateRegExpStringIterator(matcher, S, global, fullUnicode).
        const reg_exp_string_iterator = try createRegExpStringIterator(
            agent,
            matcher,
            string,
            global_,
            full_unicode,
        );
        return Value.from(&reg_exp_string_iterator.object);
    }

    /// 22.2.6.10 get RegExp.prototype.multiline
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.multiline
    fn multiline(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x006D (LATIN SMALL LETTER M).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.c.LRE_FLAG_MULTILINE);
    }

    /// 22.2.6.11 RegExp.prototype [ %Symbol.replace% ] ( string, replaceValue )
    /// https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.replace%
    fn @"%Symbol.replace%"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const string_value = arguments.get(0);
        var replace_value = arguments.get(1);

        // 1. Let rx be the this value.
        // 2. If rx is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const reg_exp = this_value.asObject();

        // 3. Let S be ? ToString(string).
        const string = try string_value.toString(agent);

        // 4. Let lengthS be the length of S.
        const string_length = string.length;

        // 5. Let functionalReplace be IsCallable(replaceValue).
        const functional_replace = replace_value.isCallable();

        // 6. If functionalReplace is false, then
        if (!functional_replace) {
            // a. Set replaceValue to ? ToString(replaceValue).
            replace_value = Value.from(try replace_value.toString(agent));
        }

        // 7. Let flags be ? ToString(? Get(rx, "flags")).
        const flags_ = try (try reg_exp.get(agent, PropertyKey.from("flags"))).toString(agent);

        // 8. If flags contains "g", let global be true; otherwise let global be false.
        const global_ = flags_.indexOf(String.fromLiteral("g"), 0) != null;

        // 9. If global is true, then
        if (global_) {
            // a. Perform ? Set(rx, "lastIndex", +0𝔽, true).
            try reg_exp.set(agent, PropertyKey.from("lastIndex"), Value.from(0), .throw);
        }

        // 10. Let results be a new empty List.
        var results: std.ArrayList(*Object) = .empty;
        defer results.deinit(agent.gc_allocator);

        // 11. Let done be false.
        // 12. Repeat, while done is false,
        while (true) {
            // a. Let result be ? RegExpExec(rx, S).
            const result = try regExpExec(agent, reg_exp, string) orelse {
                // b. If result is null, then
                //     i. Set done to true.
                break;
            };

            // c. Else,
            // i. Append result to results.
            try results.append(agent.gc_allocator, result);

            // ii. If global is false, then
            if (!global_) {
                // 1. Set done to true.
                break;
            }

            // iii. Else,
            // 1. Let matchStr be ? ToString(? Get(result, "0")).
            const match_str = try (try result.get(agent, PropertyKey.from(0))).toString(agent);

            // 2. If matchStr is the empty String, then
            if (match_str.isEmpty()) {
                // a. Let thisIndex be ℝ(? ToLength(? Get(rx, "lastIndex"))).
                const this_index = try (try reg_exp.get(agent, PropertyKey.from("lastIndex"))).toLength(agent);

                // b. If flags contains "u" or flags contains "v", let fullUnicode be true;
                //    otherwise let fullUnicode be false.
                const full_unicode = flags_.indexOf(String.fromLiteral("u"), 0) != null or
                    flags_.indexOf(String.fromLiteral("v"), 0) != null;

                // c. Let nextIndex be AdvanceStringIndex(S, thisIndex, fullUnicode).
                const next_index = advanceStringIndex(string, this_index, full_unicode);

                // d. Perform ? Set(rx, "lastIndex", 𝔽(nextIndex), true).
                try reg_exp.set(agent, PropertyKey.from("lastIndex"), Value.from(next_index), .throw);
            }
        }

        // 13. Let accumulatedResult be the empty String.
        var accumulated_result: String.Builder = .empty;
        defer accumulated_result.deinit(agent.gc_allocator);

        // 14. Let nextSourcePosition be 0.
        var next_source_position: u32 = 0;

        // 15. For each element result of results, do
        for (results.items) |result| {
            // a. Let resultLength be ? LengthOfArrayLike(result).
            const result_length = try result.lengthOfArrayLike(agent);

            // b. Let nCaptures be max(resultLength - 1, 0).
            const n_captures = result_length -| 1;

            // c. Let matched be ? ToString(? Get(result, "0")).
            const matched = try (try result.get(agent, PropertyKey.from(0))).toString(agent);

            // d. Let matchLength be the length of matched.
            const matched_length = matched.length;

            // e. Let position be ? ToIntegerOrInfinity(? Get(result, "index")).
            const position_f64 = try (try result.get(agent, PropertyKey.from("index"))).toIntegerOrInfinity(agent);

            // f. Set position to the result of clamping position between 0 and lengthS.
            const position = std.math.clamp(
                std.math.lossyCast(u32, position_f64),
                0,
                string_length,
            );

            // g. Let captures be a new empty List.
            var captures = try std.ArrayList(?*const String).initCapacity(
                agent.gc_allocator,
                @intCast(n_captures),
            );
            defer captures.deinit(agent.gc_allocator);

            // h. Let n be 1.
            var n: u53 = 1;

            // i. Repeat, while n ≤ nCaptures,
            while (n <= n_captures) : (n += 1) {
                var capture_n_string: ?*const String = null;

                // i. Let capN be ? Get(result, ! ToString(𝔽(n))).
                var capture_n = try result.get(agent, PropertyKey.from(n));

                // ii. If capN is not undefined, then
                if (!capture_n.isUndefined()) {
                    // 1. Set capN to ? ToString(capN).
                    capture_n_string = try capture_n.toString(agent);
                }

                // iii. Append capN to captures.
                captures.appendAssumeCapacity(capture_n_string);

                // iv. NOTE: When n = 1, the preceding step puts the first element into captures
                //     (at index 0). More generally, the nth capture (the characters captured by
                //     the nth set of capturing parentheses) is at captures[n - 1].

                // v. Set n to n + 1.
            }

            // j. Let namedCaptures be ? Get(result, "groups").
            var named_captures = try result.get(agent, PropertyKey.from("groups"));

            // k. If functionalReplace is true, then
            const replacement_string = if (functional_replace) blk: {
                // i. Let replacerArgs be the list-concatenation of « matched », captures, and
                //    « 𝔽(position), S ».
                var replacer_args = try std.ArrayList(Value).initCapacity(
                    agent.gc_allocator,
                    captures.items.len + 3 + @intFromBool(!named_captures.isUndefined()),
                );
                replacer_args.appendAssumeCapacity(Value.from(matched));
                for (captures.items) |capture| replacer_args.appendAssumeCapacity(
                    if (capture) |s| Value.from(s) else .null,
                );
                replacer_args.appendAssumeCapacity(Value.from(position));
                replacer_args.appendAssumeCapacity(Value.from(string));

                // ii. If namedCaptures is not undefined, then
                if (!named_captures.isUndefined()) {
                    // 1. Append namedCaptures to replacerArgs.
                    replacer_args.appendAssumeCapacity(named_captures);
                }

                // iii. Let replacementValue be ? Call(replaceValue, undefined, replacerArgs).
                const replacement_value = try replace_value.callAssumeCallable(
                    agent,
                    .undefined,
                    replacer_args.items,
                );

                // iv. Let replacementString be ? ToString(replacementValue).
                break :blk try replacement_value.toString(agent);
            } else blk: {
                // l. Else,
                // i. If namedCaptures is not undefined, then
                const named_captures_object: ?*Object = if (!named_captures.isUndefined()) blk_obj: {
                    // 1. Set namedCaptures to ? ToObject(namedCaptures).
                    break :blk_obj try named_captures.toObject(agent);
                } else null;

                // ii. Let replacementString be ? GetSubstitution(matched, S, position, captures,
                //           namedCaptures, replaceValue).
                break :blk try getSubstitution(
                    agent,
                    matched,
                    string,
                    position,
                    captures.items,
                    named_captures_object,
                    replace_value.asString(),
                );
            };

            // m. If position ≥ nextSourcePosition, then
            if (position >= next_source_position) {
                // i. NOTE: position should not normally move backwards. If it does, it is an
                //    indication of an ill-behaving RegExp subclass or use of an access triggered
                //    side-effect to change the global flag or other characteristics of rx. In such
                //    cases, the corresponding substitution is ignored.

                // ii. Set accumulatedResult to the string-concatenation of accumulatedResult, the
                //     substring of S from nextSourcePosition to position, and replacementString.
                try accumulated_result.appendString(
                    agent.gc_allocator,
                    try string.substring(agent, next_source_position, position),
                );
                try accumulated_result.appendString(agent.gc_allocator, replacement_string);

                // iii. Set nextSourcePosition to position + matchLength.
                next_source_position = position + matched_length;
            }
        }

        // 16. If nextSourcePosition ≥ lengthS, return accumulatedResult.
        // 17. Return the string-concatenation of accumulatedResult and the substring of S from
        //     nextSourcePosition.
        if (next_source_position < string_length) {
            try accumulated_result.appendString(
                agent.gc_allocator,
                try string.substring(agent, next_source_position, null),
            );
        }
        return Value.from(try accumulated_result.build(agent));
    }

    /// 22.2.6.12 RegExp.prototype [ %Symbol.search% ] ( string )
    /// https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.search%
    fn @"%Symbol.search%"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const string_value = arguments.get(0);

        // 1. Let rx be the this value.
        // 2. If rx is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const reg_exp = this_value.asObject();

        // 3. Let S be ? ToString(string).
        const string = try string_value.toString(agent);

        // 4. Let previousLastIndex be ? Get(rx, "lastIndex").
        const previous_last_index = try reg_exp.get(agent, PropertyKey.from("lastIndex"));

        // 5. If previousLastIndex is not +0𝔽, then
        if (!sameValue(previous_last_index, Value.from(0))) {
            // a. Perform ? Set(rx, "lastIndex", +0𝔽, true).
            try reg_exp.set(agent, PropertyKey.from("lastIndex"), Value.from(0), .throw);
        }

        // 6. Let result be ? RegExpExec(rx, S).
        const result = try regExpExec(agent, reg_exp, string);

        // 7. Let currentLastIndex be ? Get(rx, "lastIndex").
        const current_last_index = try reg_exp.get(agent, PropertyKey.from("lastIndex"));

        // 8. If SameValue(currentLastIndex, previousLastIndex) is false, then
        if (!sameValue(current_last_index, previous_last_index)) {
            // a. Perform ? Set(rx, "lastIndex", previousLastIndex, true).
            try reg_exp.set(agent, PropertyKey.from("lastIndex"), previous_last_index, .throw);
        }

        // 9. If result is null, return -1𝔽.
        if (result == null) return Value.from(-1);

        // 10. Return ? Get(result, "index").
        return try result.?.get(agent, PropertyKey.from("index"));
    }

    /// 22.2.6.13 get RegExp.prototype.source
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.source
    fn source(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // Let R be the this value.
        // 2. If R is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        // 3. If R does not have an [[OriginalSource]] internal slot, then
        const reg_exp = this_value.asObject().cast(RegExp) orelse {
            const realm = agent.currentRealm();

            // a. If SameValue(R, %RegExp.prototype%) is true, return "(?:)".
            if (this_value.asObject() == try realm.intrinsics.@"%RegExp.prototype%"()) {
                return Value.from("(?:)");
            }

            // b. Otherwise, throw a TypeError exception.
            return agent.throwException(.type_error, "This value must be a RegExp object", .{});
        };

        // 4. Assert: R has an [[OriginalFlags]] internal slot.
        // 5. Let src be R.[[OriginalSource]].
        const src = reg_exp.fields.original_source;

        // 6. Let flags be R.[[OriginalFlags]].
        const re_bytecode = reg_exp.fields.re_bytecode;
        const re_flags = libregexp.c.lre_get_flags(@ptrCast(re_bytecode));

        // 7. Return EscapeRegExpPattern(src, flags).
        return Value.from(try escapeRegExpPattern(agent, src, re_flags));
    }

    /// 22.2.6.13.1 EscapeRegExpPattern ( P, F )
    /// https://tc39.es/ecma262/#sec-escaperegexppattern
    fn escapeRegExpPattern(
        agent: *Agent,
        pattern: *const String,
        _: c_int,
    ) std.mem.Allocator.Error!*const String {
        // TODO: 1-4.
        // 5. The code points / or any LineTerminator occurring in the pattern shall be escaped in
        //    S as necessary to ensure that the string-concatenation of "/", S, "/", and F can be
        //    parsed (in an appropriate lexical context) as a RegularExpressionLiteral that behaves
        //    identically to the constructed regular expression. For example, if P is "/", then S
        //    could be "\/" or "\u002F", among other possibilities, but not "/", because /// followed
        //    by F would be parsed as a SingleLineComment rather than a RegularExpressionLiteral.
        //    If P is the empty String, this specification can be met by letting S be "(?:)".
        // 6. Return S.
        if (pattern.isEmpty()) return String.fromLiteral("(?:)");
        var escaped = pattern;
        escaped = try escaped.replace(agent, "/", "\\/");
        escaped = try escaped.replace(agent, "\r", "\\r");
        escaped = try escaped.replace(agent, "\n", "\\n");
        // TODO: Handle LS and PS line terminators
        return escaped;
    }

    /// 22.2.6.14 RegExp.prototype [ %Symbol.split% ] ( string, limit )
    /// https://tc39.es/ecma262/#sec-regexp.prototype-%symbol.split%
    fn @"%Symbol.split%"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const realm = agent.currentRealm();
        const string_value = arguments.get(0);
        const limit_value = arguments.get(1);

        // 1. Let rx be the this value.
        // 2. If rx is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const reg_exp = this_value.asObject();

        // 3. Let S be ? ToString(string).
        const string = try string_value.toString(agent);

        // 4. Let C be ? SpeciesConstructor(rx, %RegExp%).
        const constructor_ = try reg_exp.speciesConstructor(
            agent,
            try realm.intrinsics.@"%RegExp%"(),
        );

        // 5. Let flags be ? ToString(? Get(rx, "flags")).
        const flags_ = try (try reg_exp.get(agent, PropertyKey.from("flags"))).toString(agent);

        // 6. If flags contains "u" or flags contains "v", let unicodeMatching be true.
        // 7. Else, let unicodeMatching be false.
        const unicode_matching = flags_.indexOf(String.fromLiteral("u"), 0) != null or
            flags_.indexOf(String.fromLiteral("v"), 0) != null;

        // 8. If flags contains "y", let newFlags be flags.
        // 9. Else, let newFlags be the string-concatenation of flags and "y".
        const new_flags = if (flags_.indexOf(String.fromLiteral("y"), 0) != null)
            flags_
        else
            try String.concat(agent, &.{ flags_, String.fromLiteral("y") });

        // 10. Let splitter be ? Construct(C, « rx, newFlags »).
        const splitter = try constructor_.construct(
            agent,
            &.{ Value.from(reg_exp), Value.from(new_flags) },
            null,
        );

        // 11. Let A be ! ArrayCreate(0).
        const array = try arrayCreateFast(agent, 0);

        // 12. Let lengthA be 0.
        var length_array: u32 = 0;

        // 13. If limit is undefined, let lim be 2**32 - 1; else let lim be ℝ(? ToUint32(limit)).
        const limit = if (limit_value.isUndefined())
            std.math.maxInt(u32)
        else
            try limit_value.toUint32(agent);

        // 14. If lim = 0, return A.
        if (limit == 0) return Value.from(&array.object);

        // 15. If S is the empty String, then
        if (string.isEmpty()) {
            // a. Let z be ? RegExpExec(splitter, S).
            const z = try regExpExec(agent, splitter, string);

            // b. If z is not null, return A.
            if (z != null) return Value.from(&array.object);

            // c. Perform ! CreateDataPropertyOrThrow(A, "0", S).
            try array.object.createDataPropertyDirect(agent, PropertyKey.from(0), Value.from(string));

            // d. Return A.
            return Value.from(&array.object);
        }

        // 16. Let size be the length of S.
        const size = string.length;

        // 17. Let p be 0.
        var p: u53 = 0;

        // 18. Let q be p.
        var q: u53 = p;

        // 19. Repeat, while q < size,
        while (q < size) {
            // a. Perform ? Set(splitter, "lastIndex", 𝔽(q), true).
            try splitter.set(agent, PropertyKey.from("lastIndex"), Value.from(q), .throw);

            // b. Let z be ? RegExpExec(splitter, S).
            const z = try regExpExec(agent, splitter, string);

            // c. If z is null, then
            if (z == null) {
                // i. Set q to AdvanceStringIndex(S, q, unicodeMatching).
                q = advanceStringIndex(string, q, unicode_matching);
            } else {
                // d. Else,
                // i. Let e be ℝ(? ToLength(? Get(splitter, "lastIndex"))).
                var e = try (try splitter.get(agent, PropertyKey.from("lastIndex"))).toLength(agent);

                // ii. Set e to min(e, size).
                e = @min(e, size);

                // iii. If e = p, then
                if (e == p) {
                    // 1. Set q to AdvanceStringIndex(S, q, unicodeMatching).
                    q = advanceStringIndex(string, q, unicode_matching);
                } else {
                    // iv. Else,
                    // 1. Let T be the substring of S from p to q.
                    const tail = try string.substring(agent, @intCast(p), @intCast(q));

                    // 2. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(lengthA)), T).
                    try array.object.createDataPropertyDirect(
                        agent,
                        PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(length_array))),
                        Value.from(tail),
                    );

                    // 3. Set lengthA to lengthA + 1.
                    length_array += 1;

                    // 4. If lengthA = lim, return A.
                    if (length_array == limit) return Value.from(&array.object);

                    // 5. Set p to e.
                    p = e;

                    // 6. Let numberOfCaptures be ? LengthOfArrayLike(z).
                    var number_of_captures = try z.?.lengthOfArrayLike(agent);

                    // 7. Set numberOfCaptures to max(numberOfCaptures - 1, 0).
                    if (number_of_captures > 0) number_of_captures -= 1;

                    // 8. Let i be 1.
                    var i: u53 = 1;

                    // 9. Repeat, while i ≤ numberOfCaptures,
                    while (i <= number_of_captures) : (i += 1) {
                        // a. Let nextCapture be ? Get(z, ! ToString(𝔽(i))).
                        const next_capture = try z.?.get(agent, PropertyKey.from(i));

                        // b. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(lengthA)), nextCapture).
                        try array.object.createDataPropertyDirect(
                            agent,
                            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(length_array))),
                            next_capture,
                        );

                        // c. Set i to i + 1.

                        // d. Set lengthA to lengthA + 1.
                        length_array += 1;

                        // e. If lengthA = lim, return A.
                        if (length_array == limit) return Value.from(&array.object);
                    }

                    // 10. Set q to p.
                    q = p;
                }
            }
        }

        // 20. Let T be the substring of S from p to size.
        const tail = try string.substring(agent, @intCast(p), size);

        // 21. Perform ! CreateDataPropertyOrThrow(A, ! ToString(𝔽(lengthA)), T).
        try array.object.createDataPropertyDirect(
            agent,
            PropertyKey.from(@as(PropertyKey.IntegerIndex, @intCast(length_array))),
            Value.from(tail),
        );

        // 22. Return A.
        return Value.from(&array.object);
    }

    /// 22.2.6.15 get RegExp.prototype.sticky
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.sticky
    fn sticky(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0079 (LATIN SMALL LETTER Y).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.c.LRE_FLAG_STICKY);
    }

    /// 22.2.6.16 RegExp.prototype.test ( S )
    /// https://tc39.es/ecma262/#sec-regexp.prototype.test
    fn @"test"(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. If R is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const reg_exp = this_value.asObject();

        // 3. Let string be ? ToString(S).
        const string = try arguments.get(0).toString(agent);

        // 4. Let match be ? RegExpExec(R, string).
        const match = try regExpExec(agent, reg_exp, string);

        // 5. If match is not null, return true; else return false.
        return Value.from(match != null);
    }

    /// 22.2.6.17 RegExp.prototype.toString ( )
    /// https://tc39.es/ecma262/#sec-regexp.prototype.tostring
    fn toString(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. If R is not an Object, throw a TypeError exception.
        if (!this_value.isObject()) {
            return agent.throwException(.type_error, "{f} is not an Object", .{this_value});
        }
        const reg_exp = this_value.asObject();

        // 3. Let pattern be ? ToString(? Get(R, "source")).
        const pattern = try (try reg_exp.get(agent, PropertyKey.from("source"))).toString(agent);

        // 4. Let flags be ? ToString(? Get(R, "flags")).
        const flags_ = try (try reg_exp.get(agent, PropertyKey.from("flags"))).toString(agent);

        // 5. Let result be the string-concatenation of "/", pattern, "/", and flags.
        const result = try String.concat(agent, &.{
            String.fromLiteral("/"),
            pattern,
            String.fromLiteral("/"),
            flags_,
        });

        // 6. Return result.
        return Value.from(result);
    }

    /// 22.2.6.18 get RegExp.prototype.unicode
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.unicode
    fn unicode(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0075 (LATIN SMALL LETTER U).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.c.LRE_FLAG_UNICODE);
    }

    /// 22.2.6.19 get RegExp.prototype.unicodeSets
    /// https://tc39.es/ecma262/#sec-get-regexp.prototype.unicodesets
    fn unicodeSets(agent: *Agent, this_value: Value, _: Arguments) Agent.Error!Value {
        // 1. Let R be the this value.
        // 2. Let cu be the code unit 0x0076 (LATIN SMALL LETTER V).
        // 3. Return ? RegExpHasFlag(R, cu).
        return regExpHasFlag(agent, this_value, libregexp.c.LRE_FLAG_UNICODE_SETS);
    }

    /// B.2.4.1 RegExp.prototype.compile ( pattern, flags )
    /// https://tc39.es/ecma262/#sec-regexp.prototype.compile
    fn compile(agent: *Agent, this_value: Value, arguments: Arguments) Agent.Error!Value {
        const pattern = arguments.get(0);
        const flags_ = arguments.get(1);

        // 1. Let O be the this value.
        // 2. Perform ? RequireInternalSlot(O, [[RegExpMatcher]]).
        const reg_exp = try this_value.requireInternalSlot(agent, RegExp);

        var p: Value = undefined;
        var f: Value = undefined;

        // 3. If pattern is an Object and pattern has a [[RegExpMatcher]] internal slot, then
        if (pattern.castObject(RegExp)) |pattern_reg_exp| {
            // a. If flags is not undefined, throw a TypeError exception.
            if (!flags_.isUndefined()) {
                return agent.throwException(
                    .type_error,
                    "Flags must be undefined when pattern is a RegExp object, got {f}",
                    .{flags_},
                );
            }

            // b. Let P be pattern.[[OriginalSource]].
            p = Value.from(pattern_reg_exp.fields.original_source);

            // c. Let F be pattern.[[OriginalFlags]].
            f = Value.from(pattern_reg_exp.fields.original_flags);
        } else {
            // 4. Else,
            // a. Let P be pattern.
            p = pattern;

            // b. Let F be flags.
            f = flags_;
        }

        // 5. Return ? RegExpInitialize(O, P, F).
        _ = try regExpInitialize(agent, reg_exp, p, f);
        return Value.from(&reg_exp.object);
    }
};

/// 22.2.8 Properties of RegExp Instances
/// https://tc39.es/ecma262/#sec-properties-of-regexp-instances
pub const RegExp = MakeObject(.{
    .Fields = struct {
        /// [[OriginalSource]]
        original_source: *const String,

        /// [[OriginalFlags]]
        original_flags: *const String,

        /// [[RegExpRecord]]
        re_bytecode: []const u8,
    },
    .tag = .reg_exp,
    .display_name = "RegExp",
});
