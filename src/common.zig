const std = @import("std");
const builtin = @import("builtin");
const root = @import("root");
const c = @import("c_intrinsics.zig");
const v = @import("vector_types.zig");

pub const STEP_SIZE = if (@hasDecl(root, "step_size")) root.step_size else 64;
comptime {
    if (!(STEP_SIZE == 64 or STEP_SIZE == 128)) @compileError("step-size must be either 64 or 128");
}
pub const DEFAULT_MAX_DEPTH = 1024;
pub const u8xstep_size = std.meta.Vector(STEP_SIZE, u8);
pub const STREAMING = false;
pub const SIMDJSON_PADDING = 32;

pub const log_level: std.log.Level = std.enums.nameCast(std.log.Level, if (@hasDecl(root, "build_options"))
    root.build_options.log_level
else
    @import("build_options").log_level);
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

pub inline fn ptr_diff(comptime T: type, p1: anytype, p2: anytype) !T {
    const U = std.meta.Child(@TypeOf(p1));
    const V = std.meta.Child(@TypeOf(p2));
    if (@sizeOf(U) != @sizeOf(V)) @compileError("ptr_diff: mismatched child sizes");
    const diff = @intFromPtr(p1) - @intFromPtr(p2);
    return std.math.cast(T, diff / (@sizeOf(U))) orelse return error.Overflow;
}

pub const FileError =
    std.fs.File.OpenError ||
    std.fs.File.ReadError ||
    std.fs.File.SeekError;

pub const Error =
    std.mem.Allocator.Error ||
    FileError ||
    error{ EndOfStream, Overflow, UserDefined, AllocatorRequired } ||
    JsonError;

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

pub const has_sse2 = std.Target.x86.featureSetHas(builtin.cpu.features, .sse2);
pub const has_avx = std.Target.x86.featureSetHas(builtin.cpu.features, .avx);
pub const has_pclmul = std.Target.x86.featureSetHas(builtin.cpu.features, .pclmul);
pub const has_armaes = std.Target.aarch64.featureSetHas(builtin.cpu.features, .aes);
pub const is_arm64 = builtin.cpu.arch == .aarch64;
pub const is_x86_64 = builtin.cpu.arch == .x86_64;

pub const Chunk = if (is_x86_64) v.u8x32 else if (is_arm64) v.u8x16 else v.u8x8;
pub const IChunk = if (is_x86_64) v.i8x32 else if (is_arm64) v.i8x16 else v.i8x8;
pub const ChunkArr = [chunk_len]u8;
pub const chunk_len = @sizeOf(Chunk);

pub const GetOptions = struct {
    allocator: ?std.mem.Allocator = null,
};

pub const TraitFn = fn (type) bool;

pub fn isContainer(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .@"struct", .@"union", .@"enum", .@"opaque" => true,
        else => false,
    };
}

pub fn hasFn(comptime name: []const u8) TraitFn {
    const Closure = struct {
        pub fn trait(comptime T: type) bool {
            if (!comptime isContainer(T)) return false;
            if (!comptime @hasDecl(T, name)) return false;
            const DeclType = @TypeOf(@field(T, name));
            return @typeInfo(DeclType) == .@"fn";
        }
    };
    return Closure.trait;
}
