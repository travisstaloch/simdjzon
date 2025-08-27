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
    std.fs.File.SeekError ||
    std.fs.File.Reader.SizeError;

pub const Error =
    std.mem.Allocator.Error ||
    FileError ||
    error{ EndOfStream, Overflow, UserDefined, AllocatorRequired, ReadFailed } ||
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
pub const chunk_align: std.mem.Alignment = .of(Chunk);

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

const mem = std.mem;
const Allocator = mem.Allocator;
const Writer = std.Io.Writer;
const assert = std.debug.assert;
const File = std.fs.File;
const Limit = std.Io.Limit;

/// Maintains `Writer` state such that it writes to the unused capacity of an
/// array list, filling it up completely before making a call through the
/// vtable, causing a resize. Consequently, the same, optimized, non-generic
/// machine code that uses `std.Io.Reader`, such as formatted printing, takes
/// the hot paths when using this API.
///
/// When using this API, it is not necessary to call `flush`.
// TODO upstream this and make `Io.Writer.Allocating = Io.Writer.AlignedAllocating(.fromByteUnits(1))`
pub fn AlignedAllocating(comptime alignment: mem.Alignment) type {
    return struct {
        allocator: Allocator,
        writer: Writer,

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
                .writer = .{
                    .buffer = &.{},
                    .vtable = &vtable,
                },
            };
        }

        pub fn initCapacity(allocator: Allocator, capacity: usize) error{OutOfMemory}!Self {
            return .{
                .allocator = allocator,
                .writer = .{
                    .buffer = try allocator.alloc(u8, capacity),
                    .vtable = &vtable,
                },
            };
        }

        pub fn initOwnedSlice(allocator: Allocator, slice: []align(alignment.toByteUnits()) u8) Self {
            return .{
                .allocator = allocator,
                .writer = .{
                    .buffer = slice,
                    .vtable = &vtable,
                },
            };
        }

        /// Replaces `array_list` with empty, taking ownership of the memory.
        pub fn fromArrayList(allocator: Allocator, array_list: *std.ArrayListAlignedUnmanaged(u8, alignment)) Self {
            defer array_list.* = .empty;
            return .{
                .allocator = allocator,
                .writer = .{
                    .vtable = &vtable,
                    .buffer = array_list.allocatedSlice(),
                    .end = array_list.items.len,
                },
            };
        }

        const vtable: Writer.VTable = .{
            .drain = Self.drain,
            .sendFile = Self.sendFile,
            .flush = Writer.noopFlush,
            .rebase = growingRebase,
        };

        pub fn deinit(a: *Self) void {
            a.allocator.free(a.writer.buffer);
            a.* = undefined;
        }

        /// Returns an array list that takes ownership of the allocated memory.
        /// Resets the `Allocating` to an empty state.
        pub fn toArrayList(a: *Self) std.ArrayListAlignedUnmanaged(u8, alignment) {
            const w = &a.writer;
            const result: std.ArrayListAlignedUnmanaged(u8, alignment) = .{
                .items = @alignCast(w.buffer[0..w.end]),
                .capacity = w.buffer.len,
            };
            w.buffer = &.{};
            w.end = 0;
            return result;
        }

        pub fn ensureUnusedCapacity(a: *Self, additional_count: usize) Allocator.Error!void {
            var list = a.toArrayList();
            defer a.setArrayList(list);
            return list.ensureUnusedCapacity(a.allocator, additional_count);
        }

        pub fn ensureTotalCapacity(a: *Self, new_capacity: usize) Allocator.Error!void {
            var list = a.toArrayList();
            defer a.setArrayList(list);
            return list.ensureTotalCapacity(a.allocator, new_capacity);
        }

        pub fn toOwnedSlice(a: *Self) error{OutOfMemory}![]align(alignment.toByteUnits()) u8 {
            var list = a.toArrayList();
            defer a.setArrayList(list);
            return list.toOwnedSlice(a.allocator);
        }

        pub fn toOwnedSliceSentinel(a: *Self, comptime sentinel: u8) error{OutOfMemory}![:sentinel]align(alignment.toByteUnits()) u8 {
            const gpa = a.allocator;
            var list = toArrayList(a);
            defer a.setArrayList(list);
            return @alignCast(try list.toOwnedSliceSentinel(gpa, sentinel));
        }

        pub fn written(a: *Self) []align(alignment.toByteUnits()) u8 {
            return @alignCast(a.writer.buffered());
        }

        pub fn shrinkRetainingCapacity(a: *Self, new_len: usize) void {
            a.writer.end = new_len;
        }

        pub fn clearRetainingCapacity(a: *Self) void {
            a.shrinkRetainingCapacity(0);
        }

        fn drain(w: *Writer, data: []const []const u8, splat: usize) Writer.Error!usize {
            const a: *Self = @fieldParentPtr("writer", w);
            const gpa = a.allocator;
            const pattern = data[data.len - 1];
            const splat_len = pattern.len * splat;
            var list = a.toArrayList();
            defer setArrayList(a, list);
            const start_len = list.items.len;
            assert(data.len != 0);
            for (data) |bytes| {
                list.ensureUnusedCapacity(gpa, bytes.len + splat_len + 1) catch return error.WriteFailed;
                list.appendSliceAssumeCapacity(bytes);
            }
            if (splat == 0) {
                list.items.len -= pattern.len;
            } else switch (pattern.len) {
                0 => {},
                1 => list.appendNTimesAssumeCapacity(pattern[0], splat - 1),
                else => for (0..splat - 1) |_| list.appendSliceAssumeCapacity(pattern),
            }
            return list.items.len - start_len;
        }

        fn sendFile(w: *Writer, file_reader: *File.Reader, limit: Limit) Writer.FileError!usize {
            if (File.Handle == void) return error.Unimplemented;
            if (limit == .nothing) return 0;
            const a: *Self = @fieldParentPtr("writer", w);
            const gpa = a.allocator;
            var list = a.toArrayList();
            defer setArrayList(a, list);
            const pos = file_reader.logicalPos();
            const additional = if (file_reader.getSize()) |size| size - pos else |_| std.atomic.cache_line;
            if (additional == 0) return error.EndOfStream;
            list.ensureUnusedCapacity(gpa, limit.minInt64(additional)) catch return error.WriteFailed;
            const dest = limit.slice(list.unusedCapacitySlice());
            const n = try file_reader.read(dest);
            list.items.len += n;
            return n;
        }

        fn growingRebase(w: *Writer, preserve: usize, minimum_len: usize) Writer.Error!void {
            const a: *Self = @fieldParentPtr("writer", w);
            const gpa = a.allocator;
            var list = a.toArrayList();
            defer setArrayList(a, list);
            const total = std.math.add(usize, preserve, minimum_len) catch return error.WriteFailed;
            list.ensureTotalCapacity(gpa, total) catch return error.WriteFailed;
            list.ensureUnusedCapacity(gpa, minimum_len) catch return error.WriteFailed;
        }

        fn setArrayList(a: *Self, list: std.ArrayListAlignedUnmanaged(u8, alignment)) void {
            a.writer.buffer = list.allocatedSlice();
            a.writer.end = list.items.len;
        }

        const testing = std.testing;
        test Self {
            var a: Self = .init(testing.allocator);
            defer a.deinit();
            const w = &a.writer;

            const x: i32 = 42;
            const y: i32 = 1234;
            try w.print("x: {}\ny: {}\n", .{ x, y });

            try testing.expectEqualSlices(u8, "x: 42\ny: 1234\n", a.written());
        }
    };
}
