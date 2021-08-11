const build_options = @import("build_options");
const std = @import("std");

pub const DEFAULT_MAX_DEPTH = 1024;
pub const step_size = build_options.step_size;
pub const u8xstep_size = std.meta.Vector(step_size, u8);
pub const STREAMING = false;
pub const SIMDJSON_PADDING = 32;

// pub const log_level: std.log.Level = .debug;
pub const log_level: std.log.Level = .err;
pub var debug = log_level == .debug;
pub fn println(comptime fmt: []const u8, args: anytype) void {
    print(fmt ++ "\n", args);
}
pub fn print(comptime fmt: []const u8, args: anytype) void {
    if (debug)
        std.debug.print(fmt, args);
    // std.log.debug(fmt, args);
}
pub fn print_vec(name: []const u8, vec: anytype) void {
    println("{s}: {any}", .{ name, @as([@sizeOf(@TypeOf(vec))]u8, vec) });
}

pub inline fn ROUNDUP_N(a: anytype, n: @TypeOf(a)) @TypeOf(a) {
    return (a + (n - 1)) & ~(n - 1);
}

pub const FileError = std.fs.File.OpenError || std.fs.File.ReadError || std.fs.File.SeekError;
pub const Error = std.mem.Allocator.Error || std.os.WriteError || FileError || error{ EndOfStream, Overflow } || JsonError;
pub const JsonError = error{
    /// This parser can't support a document that big
    CAPACITY,
    /// Error allocating memory, most likely out of memory
    MEMALLOC,
    /// Something went wrong while writing to the tape (stage 2), this is a generic error
    TAPE_ERROR,
    /// Your document exceeds the user-specified depth limitation
    DEPTH_ERROR,
    /// Problem while parsing a string
    STRING_ERROR,
    /// Problem while parsing an atom starting with the letter 't'
    T_ATOM_ERROR,
    /// Problem while parsing an atom starting with the letter 'f'
    F_ATOM_ERROR,
    /// Problem while parsing an atom starting with the letter 'n'
    N_ATOM_ERROR,
    /// Problem while parsing a number
    NUMBER_ERROR,
    /// the input is not valid UTF-8
    UTF8_ERROR,
    /// unknown error, or uninitialized document
    UNINITIALIZED,
    /// no structural element found
    EMPTY,
    /// found unescaped characters in a string.
    UNESCAPED_CHARS,
    /// missing quote at the end
    UNCLOSED_STRING,
    /// unsupported architecture
    UNSUPPORTED_ARCHITECTURE,
    /// JSON element has a different type than user expected
    INCORRECT_TYPE,
    /// JSON number does not fit in 64 bits
    NUMBER_OUT_OF_RANGE,
    /// JSON array index too large
    INDEX_OUT_OF_BOUNDS,
    /// JSON field not found in object
    NO_SUCH_FIELD,
    /// Error reading a file
    IO_ERROR,
    /// Invalid JSON pointer reference
    INVALID_JSON_POINTER,
    /// Invalid URI fragment
    INVALID_URI_FRAGMENT,
    /// indicative of a bug in simdjson
    UNEXPECTED_ERROR,
    /// parser is already in use.
    PARSER_IN_USE,
    /// tried to iterate an array or object out of order
    OUT_OF_ORDER_ITERATION,
    /// The JSON doesn't have enough padding for simdjson to safely parse it.
    INSUFFICIENT_PADDING,
};
