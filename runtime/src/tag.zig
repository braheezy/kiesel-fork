const std = @import("std");

const kiesel = @import("kiesel");

const Object = kiesel.types.Object;

pub const Tag = enum(u32) {
    const builtin_tag_max = std.meta.fields(Object.Tag).len;

    blob = builtin_tag_max + 1,
    crypto,
    navigator,
    performance,
    response,
    text_decoder,
    text_encoder,
};
