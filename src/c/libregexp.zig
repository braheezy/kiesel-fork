pub const libregexp = @cImport({
    @cInclude("cutils.h"); // For the BOOL typedef
    @cInclude("libregexp.h");
});
