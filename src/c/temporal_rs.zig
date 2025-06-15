const build_options = @import("build-options");

pub const c = @cImport({
    if (!build_options.enable_temporal) {
        @compileError("temporal not enabled");
    }
    @cInclude("ErrorKind.h");
});

// https://github.com/boa-dev/temporal/blob/main/temporal_capi/src/error.rs
pub const TemporalError = error{
    GenericError,
    TypeError,
    RangeError,
    SyntaxError,
};

/// Convert a Rust `Result<TemporalError, T>` to a Zig `TemporalError!T`.
pub fn temporalErrorResult(result: anytype) TemporalError!Success(@TypeOf(result)) {
    if (success(result)) |x| return x;
    return switch (result.unnamed_0.err.kind) {
        c.ErrorKind_Generic => error.GenericError,
        c.ErrorKind_Type => error.TypeError,
        c.ErrorKind_Range => error.RangeError,
        c.ErrorKind_Syntax => error.SyntaxError,
        c.ErrorKind_Assert => @panic("temporal_rs assertion reached"),
        else => unreachable,
    };
}

/// Converts a "result" value to its "success" type, or returns `null` if the value is an error.
/// This is `inline` to prevent binary bloat, because each instantiation is expected to be called
/// only once.
inline fn success(result: anytype) ?Success(@TypeOf(result)) {
    if (!result.is_ok) return null;
    if (Success(@TypeOf(result)) == void) return;
    return result.unnamed_0.ok;
}

/// Given the C API representation of a `Result<T, E>`, returns the type 'T'.
fn Success(comptime Result: type) type {
    const Union = @FieldType(Result, "unnamed_0");
    if (!@hasField(Union, "ok")) return void;
    return @FieldType(Union, "ok");
}
