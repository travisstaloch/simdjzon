const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const os = std.os;
const assert = std.debug.assert;
usingnamespace @import("vector_types.zig");
usingnamespace @import("llvm_intrinsics.zig");
usingnamespace @import("c_intrinsics.zig");
const StringParsing = @import("StringParsing.zig");
const NumberParsing = @import("NumberParsing.zig");
const AtomParsing = @import("AtomParsing.zig");

// pub const log_level: std.log.Level = .debug;
pub const log_level: std.log.Level = .alert;
var debug = log_level == .debug;
pub fn println(comptime fmt: []const u8, args: anytype) void {
    print(fmt ++ "\n", args);
}
pub fn print(comptime fmt: []const u8, args: anytype) void {
    if (debug)
        std.debug.print(fmt, args);
    // std.log.debug(fmt, args);
}
fn print_vec(name: []const u8, vec: u8x32) void {
    println("{s}: {any}", .{ name, @as([32]u8, vec) });
}

inline fn SIMDJSON_ROUNDUP_N(a: anytype, n: @TypeOf(a)) @TypeOf(a) {
    return (a + (n - 1)) & ~(n - 1);
}
const Document = struct {
    tape: std.ArrayListUnmanaged(u64),
    string_buf: []u8,
    string_buf_cap: u32,
    pub fn init() Document {
        return .{
            .tape = std.ArrayListUnmanaged(u64){},
            .string_buf = &[_]u8{},
            .string_buf_cap = 0,
        };
    }

    pub fn allocate(document: *Document, allocator: *mem.Allocator, capacity: u32) !void {
        if (capacity == 0) return;

        // a pathological input like "[[[[..." would generate capacity tape elements, so
        // need a capacity of at least capacity + 1, but it is also possible to do
        // worse with "[7,7,7,7,6,7,7,7,6,7,7,6,[7,7,7,7,6,7,7,7,6,7,7,6,7,7,7,7,7,7,6"
        //where capacity + 1 tape elements are
        // generated, see issue https://github.com/simdjson/simdjson/issues/345
        const tape_capacity = SIMDJSON_ROUNDUP_N(capacity + 3, 64);
        // a document with only zero-length strings... could have capacity/3 string
        // and we would need capacity/3 * 5 bytes on the string buffer
        document.string_buf_cap = SIMDJSON_ROUNDUP_N(5 * capacity / 3 + SIMDJSON_PADDING, 64);
        //   string_buf.reset( new (std::nothrow) uint8_t[string_capacity]);
        //   tape.reset(new (std::nothrow) uint64_t[tape_capacity]);
        errdefer {
            allocator.free(document.string_buf);
            document.tape.deinit(allocator);
        }
        try document.tape.ensureTotalCapacity(allocator, tape_capacity);
        document.string_buf = try allocator.alloc(u8, document.string_buf_cap);
        document.string_buf.len = 0;
    }

    pub fn deinit(doc: *Document, allocator: *mem.Allocator) void {
        doc.tape.deinit(allocator);
        doc.string_buf.len = doc.string_buf_cap;
        allocator.free(doc.string_buf[0..doc.string_buf_cap]);
    }
};

const BitIndexer = struct {
    // tail: []u32,
    // tail_pos: usize,
    tail: std.ArrayListUnmanaged(u32),

    // flatten out values in 'bits' assuming that they are are to have values of idx
    // plus their position in the bitvector, and store these indexes at
    // base_ptr[base] incrementing base as we go
    // will potentially store extra values beyond end of valid bits, so base_ptr
    // needs to be large enough to handle this
    inline fn write(indexer: *BitIndexer, reader_pos_: u64, bits_: u64, allocator: *mem.Allocator) !void {
        var bits = bits_;
        // In some instances, the next branch is expensive because it is mispredicted.
        // Unfortunately, in other cases,
        // it helps tremendously.
        print("{b:0>64} | bits", .{@bitReverse(u64, bits_)});
        if (bits == 0) {
            println("", .{});
            return;
        }
        const reader_pos = @intCast(i32, reader_pos_ - 64); //  this function is always passed last bits so reader_pos will be ahead by 64
        const cnt = @popCount(u64, bits);
        println(", reader_pos {}", .{reader_pos});
        const start_count = indexer.tail.items.len;

        // Do the first 8 all together
        // for (int i=0; i<8; i++) {
        {
            var new_items = try indexer.tail.addManyAsArray(allocator, 8);
            for (new_items) |*ptr| {
                //   this->tail[i] = idx + trailing_zeroes(bits);
                //   bits = clear_lowest_bit(bits);
                ptr.* = @intCast(u32, reader_pos + @ctz(u64, bits));
                bits = (bits -% 1) & bits;
                // std.log.debug("bits {}", .{bits});
            }
        }

        // Do the next 8 all together (we hope in most cases it won't happen at all
        // and the branch is easily predicted).
        // if (simdjson_unlikely(cnt > 8)) {
        if (cnt > 8) {
            //   for (int i=8; i<16; i++) {
            //     this->tail[i] = idx + trailing_zeroes(bits);
            //     bits = clear_lowest_bit(bits);
            //   }
            var new_items = try indexer.tail.addManyAsArray(allocator, 8);
            for (new_items) |*ptr| {
                //   this->tail[i] = idx + trailing_zeroes(bits);
                //   bits = clear_lowest_bit(bits);
                ptr.* = @intCast(u32, reader_pos + @ctz(u64, bits));
                bits = (bits -% 1) & bits;
            }
        }

        // Most files don't have 16+ structurals per block, so we take several basically guaranteed
        // branch mispredictions here. 16+ structurals per block means either punctuation ({} [] , // :)
        // or the start of a value ("abc" true 123) every four characters.
        //   if (simdjson_unlikely(cnt > 16)) {
        //     int i = 16;
        //     do {
        //       this->tail[i] = idx + trailing_zeroes(bits);
        //       bits = clear_lowest_bit(bits);
        //       i++;
        //     } while (i < cnt);
        //   }
        if (cnt > 16) {
            var i: usize = 16;
            while (true) {
                // indexer.tail[indexer.tail_pos + 16 ..][i] = @intCast(u32, idx + @ctz(u64, bits));
                // var new_item = try indexer.tail.addOne(allocator);
                // new_item.* = @intCast(u32, idx + @ctz(u64, bits));
                try indexer.tail.append(allocator, @intCast(u32, reader_pos + @ctz(u64, bits)));
                bits = (bits -% 1) & bits;
                i += 1;
                if (i >= cnt) break;
            }
        }

        // std.log.debug("tail.items.len {d} start_count + cnt {d}", .{ indexer.tail.items.len, start_count + cnt });
        // indexer.tail_pos += cnt;
        indexer.tail.shrinkRetainingCapacity(start_count + cnt);
    }
};

pub const FileError = std.fs.File.OpenError || std.fs.File.ReadError || std.fs.File.SeekError;
pub const Error = std.mem.Allocator.Error || std.os.WriteError || FileError || error{ EndOfStream, Overflow } || JsonError;
const JsonError = error{
    ///< This parser can't support a document that big
    CAPACITY,
    ///< Error allocating memory, most likely out of memory
    MEMALLOC,
    ///< Something went wrong while writing to the tape (stage 2), this is a generic error
    TAPE_ERROR,
    ///< Your document exceeds the user-specified depth limitation
    DEPTH_ERROR,
    ///< Problem while parsing a string
    STRING_ERROR,
    ///< Problem while parsing an atom starting with the letter 't'
    T_ATOM_ERROR,
    ///< Problem while parsing an atom starting with the letter 'f'
    F_ATOM_ERROR,
    ///< Problem while parsing an atom starting with the letter 'n'
    N_ATOM_ERROR,
    ///< Problem while parsing a number
    NUMBER_ERROR,
    ///< the input is not valid UTF-8
    UTF8_ERROR,
    ///< unknown error, or uninitialized document
    UNINITIALIZED,
    ///< no structural element found
    EMPTY,
    ///< found unescaped characters in a string.
    UNESCAPED_CHARS,
    ///< missing quote at the end
    UNCLOSED_STRING,
    ///< unsupported architecture
    UNSUPPORTED_ARCHITECTURE,
    ///< JSON element has a different type than user expected
    INCORRECT_TYPE,
    ///< JSON number does not fit in 64 bits
    NUMBER_OUT_OF_RANGE,
    ///< JSON array index too large
    INDEX_OUT_OF_BOUNDS,
    ///< JSON field not found in object
    NO_SUCH_FIELD,
    ///< Error reading a file
    IO_ERROR,
    ///< Invalid JSON pointer reference
    INVALID_JSON_POINTER,
    ///< Invalid URI fragment
    INVALID_URI_FRAGMENT,
    ///< indicative of a bug in simdjson
    UNEXPECTED_ERROR,
    ///< parser is already in use.
    PARSER_IN_USE,
    ///< tried to iterate an array or object out of order
    OUT_OF_ORDER_ITERATION,
    ///< The JSON doesn't have enough padding for simdjson to safely parse it.
    INSUFFICIENT_PADDING,
};

const Utf8Checker = struct {
    err: u8x32 = [1]u8{0} ** 32,
    prev_input_block: u8x32 = [1]u8{0} ** 32,
    prev_incomplete: u8x32 = [1]u8{0} ** 32,

    fn prev(comptime N: u8, chunk: u8x32, prev_chunk: u8x32) u8x32 {
        return switch (N) {
            1 => _prev1(chunk, prev_chunk),
            2 => _prev2(chunk, prev_chunk),
            3 => _prev3(chunk, prev_chunk),
            else => unreachable,
        };
    }

    inline fn check_special_cases(input: u8x32, prev1: u8x32) u8x32 {
        // Bit 0 = Too Short (lead byte/ASCII followed by lead byte/ASCII)
        // Bit 1 = Too Long (ASCII followed by continuation)
        // Bit 2 = Overlong 3-byte
        // Bit 4 = Surrogate
        // Bit 5 = Overlong 2-byte
        // Bit 7 = Two Continuations
        const TOO_SHORT: u8 = 1 << 0; // 11______ 0_______
        // 11______ 11______
        const TOO_LONG: u8 = 1 << 1; // 0_______ 10______
        const OVERLONG_3: u8 = 1 << 2; // 11100000 100_____
        const SURROGATE: u8 = 1 << 4; // 11101101 101_____
        const OVERLONG_2: u8 = 1 << 5; // 1100000_ 10______
        const TWO_CONTS: u8 = 1 << 7; // 10______ 10______
        const TOO_LARGE: u8 = 1 << 3; // 11110100 1001____
        // 11110100 101_____
        // 11110101 1001____
        // 11110101 101_____
        // 1111011_ 1001____
        // 1111011_ 101_____
        // 11111___ 1001____
        // 11111___ 101_____
        const TOO_LARGE_1000: u8 = 1 << 6;
        // 11110101 1000____
        // 1111011_ 1000____
        // 11111___ 1000____
        const OVERLONG_4: u8 = 1 << 6; // 11110000 1000____

        // const byte_1_high = prev1.shr<4>().lookup_16<uint8_t>(
        const byte_1_high_0 = prev1 >> @splat(32, @as(u3, 4));
        const tbl1 = [16]u8{
            // 0_______ ________ <ASCII in byte 1>
            TOO_LONG,               TOO_LONG,  TOO_LONG,                           TOO_LONG,
            TOO_LONG,               TOO_LONG,  TOO_LONG,                           TOO_LONG,
            // 10______ ________ <continuation in byte 1>
            TWO_CONTS,              TWO_CONTS, TWO_CONTS,                          TWO_CONTS,
            // 1100____ ________ <two byte lead in byte 1>
            TOO_SHORT | OVERLONG_2,
            // 1101____ ________ <two byte lead in byte 1>
            TOO_SHORT,
            // 1110____ ________ <three byte lead in byte 1>
            TOO_SHORT | OVERLONG_3 | SURROGATE,
            // 1111____ ________ <four+ byte lead in byte 1>
            TOO_SHORT | TOO_LARGE | TOO_LARGE_1000 | OVERLONG_4,
        } ** 2;
        const byte_1_high = shuffleEpi8(tbl1, byte_1_high_0);
        const CARRY: u8 = TOO_SHORT | TOO_LONG | TWO_CONTS; // These all have ____ in byte 1 .
        const byte_1_low0 = prev1 & @splat(32, @as(u8, 0x0F));
        // const simd8<uint8_t> byte_1_low = (prev1 & 0x0F).lookup_16<uint8_t>(
        const tbl2 = [16]u8{
            // ____0000 ________
            CARRY | OVERLONG_3 | OVERLONG_2 | OVERLONG_4,
            // ____0001 ________
            CARRY | OVERLONG_2,
            // ____001_ ________
            CARRY,
            CARRY,

            // ____0100 ________
            CARRY | TOO_LARGE,
            // ____0101 ________
            CARRY | TOO_LARGE | TOO_LARGE_1000,
            // ____011_ ________
            CARRY | TOO_LARGE | TOO_LARGE_1000,
            CARRY | TOO_LARGE | TOO_LARGE_1000,

            // ____1___ ________
            CARRY | TOO_LARGE | TOO_LARGE_1000,
            CARRY | TOO_LARGE | TOO_LARGE_1000,
            CARRY | TOO_LARGE | TOO_LARGE_1000,
            CARRY | TOO_LARGE | TOO_LARGE_1000,
            CARRY | TOO_LARGE | TOO_LARGE_1000,
            // ____1101 ________
            CARRY | TOO_LARGE | TOO_LARGE_1000 | SURROGATE,
            CARRY | TOO_LARGE | TOO_LARGE_1000,
            CARRY | TOO_LARGE | TOO_LARGE_1000,
        } ** 2;
        const byte_1_low = shuffleEpi8(tbl2, byte_1_low0);

        // const simd8<uint8_t> byte_2_high = input.shr<4>().lookup_16<uint8_t>(
        const byte_2_high_0 = input >> @splat(32, @as(u3, 4));
        const tbl3 = [16]u8{
            // ________ 0_______ <ASCII in byte 2>
            TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,
            TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,

            // ________ 1000____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4,
            // ________ 1001____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE,
            // ________ 101_____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE, TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE,

            // ________ 11______
            TOO_SHORT,                                                                    TOO_SHORT,                                                  TOO_SHORT,                                                 TOO_SHORT,
        } ** 2;
        const byte_2_high = shuffleEpi8(tbl3, byte_2_high_0);
        return (byte_1_high & byte_1_low & byte_2_high);
    }
    // zig fmt: on

    fn check_multibyte_lengths(input: u8x32, prev_input: u8x32, sc: u8x32) u8x32 {
        const prev2 = @bitCast(i8x32, prev(2, input, prev_input));
        const prev3 = @bitCast(i8x32, prev(3, input, prev_input));
        const must23 = must_be_2_3_continuation(prev2, prev3);
        // std.log.debug("input {s} prev_input {s} must23 {}", .{ @as([32]u8, input), @as([32]u8, prev_input), must23 });
        const must23_80 = must23 & @splat(32, @as(u8, 0x80));
        return must23_80 ^ sc;
    }

    fn must_be_2_3_continuation(prev2: i8x32, prev3: i8x32) u8x32 {
        const is_third_byte = _mm512_subs_epu8(prev2, @bitCast(i8x32, @splat(32, @as(u8, 0b11100000 - 1)))); // Only 111_____ will be > 0
        const is_fourth_byte = _mm512_subs_epu8(prev3, @bitCast(i8x32, @splat(32, @as(u8, 0b11110000 - 1)))); // Only 1111____ will be > 0
        // Caller requires a bool (all 1's). All values resulting from the subtraction will be <= 64, so signed comparison is fine.
        const result = @bitCast(i1x32, (is_third_byte | is_fourth_byte) > @splat(32, @as(i8, 0)));
        return @bitCast(u8x32, @as(i8x32, result));
    }

    //
    // Check whether the current bytes are valid UTF-8.
    //
    inline fn check_utf8_bytes(checker: *Utf8Checker, input: u8x32, prev_input: u8x32) void {
        // Flip prev1...prev3 so we can easily determine if they are 2+, 3+ or 4+ lead bytes
        // (2, 3, 4-byte leads become large positive numbers instead of small negative numbers)
        const prev1 = prev(1, input, prev_input);
        const sc = check_special_cases(input, prev1);
        checker.err |= check_multibyte_lengths(input, prev_input, sc);
    }

    // The only problem that can happen at EOF is that a multibyte character is too short
    // or a byte value too large in the last bytes: check_special_cases only checks for bytes
    // too large in the first of two bytes.
    inline fn check_eof(checker: *Utf8Checker) void {
        // If the previous block had incomplete UTF-8 characters at the end, an ASCII block can't
        // possibly finish them.
        checker.err |= checker.prev_incomplete;
    }

    inline fn is_ascii(input: u8x64) bool {
        const bytes: [64]u8 = input;
        const a: u8x32 = bytes[0..32].*;
        const b: u8x32 = bytes[32..64].*;

        return _mm256_movemask_epi8(a | b) == 0;
    }

    inline fn check_next_input(checker: *Utf8Checker, input: u8x64) void {
        const NUM_CHUNKS = STEP_SIZE / 32;
        const chunks = @bitCast([NUM_CHUNKS][32]u8, input);
        if (is_ascii(input)) {
            checker.err |= checker.prev_incomplete;
        } else {
            // you might think that a for-loop would work, but under Visual Studio, it is not good enough.
            // static_assert((simd8x64<uint8_t>::NUM_CHUNKS == 2) || (simd8x64<uint8_t>::NUM_CHUNKS == 4),
            // "We support either two or four chunks per 64-byte block.");
            if (NUM_CHUNKS == 2) {
                checker.check_utf8_bytes(chunks[0], checker.prev_input_block);
                checker.check_utf8_bytes(chunks[1], chunks[0]);
            } else if (NUM_CHUNKS == 4) {
                checker.check_utf8_bytes(chunks[0], checker.prev_input_block);
                checker.check_utf8_bytes(chunks[1], chunks[0]);
                checker.check_utf8_bytes(chunk[2], chunks[1]);
                checker.check_utf8_bytes(chunk[3], chunk[2]);
            } else unreachable;
            // checker.prev_incomplete = is_incomplete(input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1]);
            // checker.prev_input_block = input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1];
            checker.prev_incomplete = is_incomplete(chunks[NUM_CHUNKS - 1]);
            checker.prev_input_block = chunks[NUM_CHUNKS - 1];
        }
    }
    // do not forget to call check_eof!
    inline fn errors(checker: Utf8Checker) JsonError!void {
        const err = @reduce(.Or, checker.err);
        if (err != 0) return error.UTF8_ERROR;
    }

    //
    // Return nonzero if there are incomplete multibyte characters at the end of the block:
    // e.g. if there is a 4-byte character, but it's 3 bytes from the end.
    //
    inline fn is_incomplete(input: u8x32) u8x32 {
        // // If the previous input's last 3 bytes match this, they're too short (they ended at EOF):
        // // ... 1111____ 111_____ 11______
        const max_array: [32]u8 = .{
            255, 255, 255, 255, 255, 255,            255,            255,
            255, 255, 255, 255, 255, 255,            255,            255,
            255, 255, 255, 255, 255, 255,            255,            255,
            255, 255, 255, 255, 255, 0b11110000 - 1, 0b11100000 - 1, 0b11000000 - 1,
        };
        const max_value = @splat(32, @bitCast(i8, max_array[@as(i8, @sizeOf(@TypeOf(max_array)) - @sizeOf(u8x32))]));
        return @bitCast(u8x32, _mm512_subs_epu8(@bitCast(i8x32, input), max_value));
    }
};

const StringBlock = struct {
    backslash: u64,
    escaped: u64,
    quote: u64,
    in_string: u64,

    inline fn string_tail(sb: StringBlock) u64 {
        return sb.in_string ^ sb.quote;
    }

    inline fn non_quote_inside_string(sb: StringBlock, mask: u64) u64 {
        return mask & sb.in_string;
    }
};

const CharacterBlock = struct {
    whitespace: u64,
    op: u64,
    pub fn classify(input_vec: u8x64) CharacterBlock {
        // These lookups rely on the fact that anything < 127 will match the lower 4 bits, which is why
        // we can't use the generic lookup_16.
        const whitespace_table: u8x32 = [16]u8{ ' ', 100, 100, 100, 17, 100, 113, 2, 100, '\t', '\n', 112, 100, '\r', 100, 100 } ** 2;

        // The 6 operators (:,[]{}) have these values:
        //
        // , 2C
        // : 3A
        // [ 5B
        // { 7B
        // ] 5D
        // } 7D
        //
        // If you use | 0x20 to turn [ and ] into { and }, the lower 4 bits of each character is unique.
        // We exploit this, using a simd 4-bit lookup to tell us which character match against, and then
        // match it (against | 0x20).
        //
        // To prevent recognizing other characters, everything else gets compared with 0, which cannot
        // match due to the | 0x20.
        //
        // NOTE: Due to the | 0x20, this ALSO treats <FF> and <SUB> (control characters 0C and 1A) like ,
        // and :. This gets caught in stage 2, which checks the actual character to ensure the right
        // operators are in the right places.
        const op_table: u8x32 = [16]u8{
            0, 0, 0, 0,
            0, 0, 0, 0,
            0,   0,   ':', '{', // : = 3A, [ = 5B, { = 7B
            ',', '}', 0,
            0, // , = 2C, ] = 5D, } = 7D
        } ** 2;

        // We compute whitespace and op separately. If later code only uses one or the
        // other, given the fact that all functions are aggressively inlined, we can
        // hope that useless computations will be omitted. This is namely case when
        // minifying (we only need whitespace).

        //   const uint64_t whitespace = in.eq({
        //     _mm256_shuffle_epi8(whitespace_table, in.chunks[0]),
        //     _mm256_shuffle_epi8(whitespace_table, in.chunks[1])
        //   });
        const in = @bitCast([STEP_SIZE]u8, input_vec);
        // const in_vec = input_vec.*;
        const chunk0: u8x32 = in[0..32].*;
        const chunk1: u8x32 = in[32..64].*;
        const wss: [2]u8x32 = .{
            shuffleEpi8(whitespace_table, chunk0),
            shuffleEpi8(whitespace_table, chunk1),
        };
        const whitespace = input_vec == @bitCast(u8x64, wss);
        // Turn [ and ] into { and }
        //   const simd8x64<uint8_t> curlified{
        //     in.chunks[0] | 0x20,
        //     in.chunks[1] | 0x20
        //   };
        const curlified = input_vec | @splat(64, @as(u8, 0x20));
        //   const uint64_t op = curlified.eq({
        //     _mm256_shuffle_epi8(op_table, in.chunks[0]),
        //     _mm256_shuffle_epi8(op_table, in.chunks[1])
        //   });
        const ops: [2]u8x32 = .{
            shuffleEpi8(op_table, chunk0),
            shuffleEpi8(op_table, chunk1),
        };
        const op = curlified == @bitCast(u8x64, ops);

        return .{ .whitespace = @ptrCast(*const u64, &whitespace).*, .op = @ptrCast(*const u64, &op).* };
    }

    pub inline fn scalar(cb: CharacterBlock) u64 {
        return ~(cb.op | cb.whitespace);
    }
};

const Block = struct {
    string: StringBlock,
    characters: CharacterBlock,
    follows_nonquote_scalar: u64,

    inline fn structural_start(block: Block) u64 {
        return block.potential_structural_start() & ~block.string.string_tail();
    }
    inline fn potential_structural_start(block: Block) u64 {
        return block.characters.op | block.potential_scalar_start();
    }
    inline fn potential_scalar_start(block: Block) u64 {
        // The term "scalar" refers to anything except structural characters and white space
        // (so letters, numbers, quotes).
        // Whenever it is preceded by something that is not a structural element ({,},[,],:, ") nor a white-space
        // then we know that it is irrelevant structurally.
        return block.characters.scalar() & ~block.follows_nonquote_scalar;
    }
    //   inline fn follows_potential_scalar(block: Block) u64 {
    //     // _follows_potential_nonquote_scalar: is defined as marking any character that follows a character
    //     // that is not a structural element ({,},[,],:, comma) nor a quote (") and that is not a
    //     // white space.
    //     // It is understood that within quoted region, anything at all could be marked (irrelevant).
    //     return _follows_potential_nonquote_scalar;
    //   }

    inline fn non_quote_inside_string(block: Block, mask: u64) u64 {
        return block.string.non_quote_inside_string(mask);
    }
};

const StructuralIndexer = struct {
    prev_structurals: u64 = 0,
    unescaped_chars_error: u64 = 0,
    bit_indexer: BitIndexer,
    checker: Utf8Checker,

    pub fn init(allocator: *mem.Allocator, tail_len: usize) !StructuralIndexer {
        var tail = std.ArrayListUnmanaged(u32){};
        try tail.ensureTotalCapacity(allocator, tail_len);
        return StructuralIndexer{
            .bit_indexer = .{ .tail = tail },
            .checker = .{},
        };
    }

    inline fn follows(match: u64, overflow: *u64) u64 {
        const result = match << 1 | overflow.*;
        overflow.* = match >> 63;
        return result;
    }

    fn nextBlock(parser: *Parser, input_vec: u8x64) Block {
        const string = parser.nextStringBlock(input_vec);
        // identifies the white-space and the structurat characters
        const characters = CharacterBlock.classify(input_vec);

        // The term "scalar" refers to anything except structural characters and white space
        // (so letters, numbers, quotes).
        // We want  follows_scalar to mark anything that follows a non-quote scalar (so letters and numbers).
        //
        // A terminal quote should either be followed by a structural character (comma, brace, bracket, colon)
        // or nothing. However, we still want ' "a string"true ' to mark the 't' of 'true' as a potential
        // pseudo-structural character just like we would if we had  ' "a string" true '; otherwise we
        // may need to add an extra check when parsing strings.
        //
        // Performance: there are many ways to skin this cat.
        const nonquote_scalar = characters.scalar() & ~string.quote;
        const follows_nonquote_scalar = follows(nonquote_scalar, &parser.prev_scalar);
        // We are returning a function-local object so either we get a move constructor
        // or we get copy elision.
        // if (unescaped & strings.in_string != 0) return error.UnescapedCharacters;
        return Block{
            .string = string, // strings is a function-local object so either it moves or the copy is elided.
            .characters = characters,
            .follows_nonquote_scalar = follows_nonquote_scalar,
        };
    }

    fn step(si: *StructuralIndexer, read_buf: [64]u8, parser: *Parser, reader_pos: u64) !void {
        if (STEP_SIZE == 64) {
            const block_1 = nextBlock(parser, read_buf);
            // println("{b:0>64} | characters.op", .{@bitReverse(u64, block_1.characters.op)});
            try si.next(read_buf, block_1, reader_pos, parser.allocator);
            // TODO: better allocation strategy
            // std.log.debug("stream pos {}", .{try stream.getPos()});
        } else {
            return error.NotImplemented;
            //   simd::simd8x64<uint8_t> in_2(block+64);
            //   json_block block_1 = scanner.next(in_1);
            //   json_block block_2 = scanner.next(in_2);
            //   this->next(in_1, block_1, reader.block_index());
            //   this->next(in_2, block_2, reader.block_index()+64);
            //   reader.advance();
        }
    }

    fn finish(si: *StructuralIndexer, parser: *Parser, idx: usize, len: usize, partial: bool) !void {
        _ = partial;
        // println("finish idx {}, len {}", .{ idx, len });
        try si.bit_indexer.write(idx, si.prev_structurals, parser.allocator);

        // TODO:
        //   error_code error = scanner.finish();
        if (parser.prev_in_string != 0) return error.UNCLOSED_STRING;
        //   // We deliberately break down the next expression so that it is
        //   // human readable.
        //   const bool should_we_exit =  partial ?
        //     ((error != SUCCESS) && (error != UNCLOSED_STRING)) // when partial we tolerate UNCLOSED_STRING
        //     : (error != SUCCESS); // if partial is false, we must have SUCCESS
        //   const bool have_unclosed_string = (error == UNCLOSED_STRING);
        //   if (simdjson_unlikely(should_we_exit)) { return error; }

        if (si.unescaped_chars_error != 0) {
            return error.UNESCAPED_CHARS;
        }

        //   parser.n_structural_indexes = uint32_t(indexer.tail - parser.structural_indexes.get());
        parser.n_structural_indexes = try std.math.cast(u32, si.bit_indexer.tail.items.len);
        // ***
        // * This is related to https://github.com/simdjson/simdjson/issues/906
        // * Basically, we want to make sure that if the parsing continues beyond the last (valid)
        // * structural character, it quickly stops.
        // * Only three structural characters can be repeated without triggering an error in JSON:  [,] and }.
        // * We repeat the padding character (at 'len'). We don't know what it is, but if the parsing
        // * continues, then it must be [,] or }.
        // * Suppose it is ] or }. We backtrack to the first character, what could it be that would
        // * not trigger an error? It could be ] or } but no, because you can't start a document that way.
        // * It can't be a comma, a colon or any simple value. So the only way we could continue is
        // * if the repeated character is [. But if so, the document must start with [. But if the document
        // * starts with [, it should end with ]. If we enforce that rule, then we would get
        // * ][[ which is invalid.
        // **/
        var new_inds = try parser.indexer.bit_indexer.tail.addManyAsArray(parser.allocator, 3);
        new_inds[0] = @intCast(u32, len);
        new_inds[1] = @intCast(u32, len);
        new_inds[2] = 0;
        parser.next_structural_index = 0;
        // a valid JSON file cannot have zero structural indexes - we should have found something
        if (parser.indexer.bit_indexer.tail.items.len == 0) {
            return error.EMPTY;
        }
        if (parser.indexer.bit_indexer.tail.items[parser.indexer.bit_indexer.tail.items.len - 1] > len) {
            return error.UNEXPECTED_ERROR;
        }
        // TODO
        //   if (partial) {
        //     // If we have an unclosed string, then the last structural
        //     // will be the quote and we want to make sure to omit it.
        //     if(have_unclosed_string) {
        //       parser.n_structural_indexes--;
        //       // a valid JSON file cannot have zero structural indexes - we should have found something
        //       if (simdjson_unlikely(parser.n_structural_indexes == 0u)) { return CAPACITY; }
        //     }
        //     auto new_structural_indexes = find_next_document_index(parser);
        //     if (new_structural_indexes == 0 && parser.n_structural_indexes > 0) {
        //       return CAPACITY; // If the buffer is partial but the document is incomplete, it's too big to parse.
        //     }
        //     parser.n_structural_indexes = new_structural_indexes;
        //   }
        si.checker.check_eof();
        return si.checker.errors();
    }

    fn lteq(comptime T: type, chunks: [2]u8x32, m: T) u64 {
        //   const simd8<T> mask = simd8<T>::splat(m);
        //   return  simd8x64<bool>(
        //     this->chunks[0] <= mask,
        //     this->chunks[1] <= mask
        //   ).to_bitmask();
        const mask = @splat(32, m);
        const a = chunks[0] <= mask;
        const b = chunks[1] <= mask;
        const aint = @as(u64, @ptrCast(*const u32, &a).*);
        const bint = @as(u64, @ptrCast(*const u32, &b).*) << 32;
        return aint | bint;
    }

    fn next(si: *StructuralIndexer, input_vec: u8x64, block: Block, reader_pos: u64, allocator: *mem.Allocator) !void {
        const chunks = @bitCast([2]u8x32, input_vec);
        const unescaped = lteq(u8, chunks, 0x1F);
        if (debug) {
            var input: [64]u8 = undefined;
            std.mem.copy(u8, &input, &@as([64]u8, input_vec));
            for (input) |*c| {
                if (c.* == '\n') c.* = '-';
            }
            println("{s}", .{input});
        }
        // println("{b:0>64} | block.characters.op", .{@bitReverse(u64, block.characters.op)});
        // println("{b:0>64} | block.characters.whitespace", .{@bitReverse(u64, block.characters.whitespace)});
        // println("{b:0>64} | block.string.in_string", .{@bitReverse(u64, block.string.in_string)});
        // println("{b:0>64} | block.string.backslash", .{@bitReverse(u64, block.string.backslash)});
        // println("{b:0>64} | block.string.escaped", .{@bitReverse(u64, block.string.escaped)});
        // println("{b:0>64} | block.string.quote", .{@bitReverse(u64, block.string.quote)});
        // println("{b:0>64} | unscaped", .{ @bitReverse(u64, unescaped) });
        si.checker.check_next_input(input_vec);
        try si.bit_indexer.write(reader_pos, si.prev_structurals, allocator); // Output *last* iteration's structurals to the parser
        si.prev_structurals = block.structural_start();
        si.unescaped_chars_error |= block.non_quote_inside_string(unescaped);
    }
};

const OpenContainer = struct {
    tape_index: u32, // where, on the tape, does the scope ([,{) begin
    count: u32, // how many elements in the scope
};

const OpenContainerInfo = struct {
    open_container: OpenContainer,
    is_array: bool,
};

pub const Iterator = struct {
    parser: *Parser,
    next_structural: [*]u32,
    depth: u32 = 0,
    log_depth: u8 = 0,

    pub fn init(parser: *Parser, start_structural_index: usize) Iterator {
        return .{
            .parser = parser,
            .next_structural = parser.indexer.bit_indexer.tail.items[start_structural_index..].ptr,
        };
    }

    inline fn advance(iter: *Iterator) [*]const u8 {
        defer iter.next_structural += 1;
        // std.log.debug("advance() next_structural idx {} peek() '{c}'", .{ (@ptrToInt(iter.next_structural) - @ptrToInt(iter.parser.indexer.bit_indexer.tail.items.ptr)) / 4, iter.peek() });
        return iter.peek();
    }

    inline fn peek(iter: *Iterator) [*]const u8 {
        return iter.parser.bytes.ptr + iter.next_structural[0];
    }

    inline fn at_beginning(iter: *Iterator) bool {
        // std.log.debug("at-beginning {*}: {}", .{ iter.next_structural, iter.next_structural[0] });
        return iter.next_structural == iter.parser.indexer.bit_indexer.tail.items.ptr;
    }
    inline fn at_eof(iter: *Iterator) bool {
        // std.log.debug("at-beginning {*}: {}", .{ iter.next_structural, iter.next_structural[0] });
        return @ptrToInt(iter.next_structural) == @ptrToInt(iter.parser.indexer.bit_indexer.tail.items.ptr + iter.parser.n_structural_indexes);
    }

    const LOG_EVENT_LEN = 20;
    const LOG_BUFFER_LEN = 30;
    const LOG_SMALL_BUFFER_LEN = 10;
    const LOG_INDEX_LEN = 5;

    fn pad_with(comptime s: []const u8, comptime pad_byte: u8, comptime len: u8) [len]u8 {
        var buf = [1]u8{pad_byte} ** len;
        buf[0..s.len].* = s[0..s.len].*;
        return buf;
    }

    fn pad_with_alloc(s: []const u8, pad_byte: u8, len: u8, allocator: *mem.Allocator) []const u8 {
        var buf = allocator.alloc(u8, len) catch unreachable;
        std.mem.set(u8, buf, pad_byte);
        std.mem.copy(u8, buf, s[0..std.math.min(s.len, buf.len)]);
        return buf;
    }

    fn log_start(iter: *Iterator) void {
        if (debug) {
            iter.log_depth = 0;
            const event_txt = pad_with("Event", ' ', LOG_EVENT_LEN);
            const buffer_txt = pad_with("Buffer", ' ', LOG_BUFFER_LEN);
            const next_txt = pad_with("Next", ' ', LOG_SMALL_BUFFER_LEN);
            println("", .{});
            println("| {s} | {s} | {s} | Next# | Detail |", .{ event_txt, buffer_txt, next_txt });
            println("|{s}|{s}|{s}|-------|--------|", .{
                pad_with("", '-', LOG_EVENT_LEN + 2),
                pad_with("", '-', LOG_BUFFER_LEN + 2),
                pad_with("", '-', LOG_SMALL_BUFFER_LEN + 2),
            });
        }
    }

    fn printable_char(c: u8) u8 {
        return if (c >= 0x20) c else ' ';
    }

    fn log_line_fmt(iter: *Iterator, title_prefix: []const u8, title: []const u8, comptime detail_fmt: []const u8, detail_args: anytype) void {
        var buf: [0x100]u8 = undefined;
        iter.log_line(title_prefix, title, std.fmt.bufPrint(&buf, detail_fmt, detail_args) catch unreachable);
    }

    fn log_line(iter: *Iterator, title_prefix: []const u8, title: []const u8, detail: []const u8) void {
        var log_buf: [0x100]u8 = undefined;
        var log_buf2: [LOG_BUFFER_LEN]u8 = undefined;
        if (!debug) return;

        var log_fba = std.heap.FixedBufferAllocator.init(&log_buf);
        const depth_padding = pad_with_alloc("", ' ', @intCast(u8, if (iter.log_depth < 0x0f) iter.log_depth * 2 else 0xff), &log_fba.allocator);
        const titles = std.fmt.allocPrint(&log_fba.allocator, "{s}{s}{s}", .{ depth_padding, title_prefix, title }) catch unreachable;
        const p1 = pad_with_alloc(titles, ' ', LOG_EVENT_LEN, &log_fba.allocator);
        print("| {s} ", .{p1});
        const current_index = if (iter.at_beginning()) null else iter.next_structural - 1;
        const next_index = iter.next_structural;

        const content = blk: {
            if (current_index) |ci| {
                for (log_buf2) |*c, i| {
                    c.* = printable_char(iter.parser.bytes[ci[0] + i]);
                }
                break :blk pad_with_alloc(&log_buf2, ' ', LOG_BUFFER_LEN, &log_fba.allocator);
            } else {
                break :blk &pad_with("", ' ', LOG_BUFFER_LEN);
            }
        };
        print("| {s} ", .{content});
        const next_content = blk: {
            for (log_buf2) |*c, i| {
                if (next_index[0] + i >= iter.parser.bytes.len) break;
                // std.log.debug("bytes.len {} next_index[0] {} i {}", .{ iter.parser.bytes.len, next_index[0], i });
                c.* = printable_char(iter.parser.bytes[next_index[0] + i]);
            }
            break :blk pad_with_alloc(&log_buf2, ' ', LOG_SMALL_BUFFER_LEN, &log_fba.allocator);
        };
        print("| {s} ", .{next_content});

        if (current_index) |ci| {
            print("| {s} ", .{
                pad_with_alloc(
                    std.fmt.bufPrint(&log_buf2, "{}", .{ci[0]}) catch unreachable,
                    ' ',
                    LOG_INDEX_LEN,
                    &log_fba.allocator,
                ),
            });
        } else {
            print("| {s} ", .{&pad_with("", ' ', LOG_INDEX_LEN)});
        }
        // printf("| %*u ", LOG_INDEX_LEN, structurals.next_tape_index());
        println("| {s} ", .{detail});
    }

    fn log_value(iter: *Iterator, typ: []const u8) void {
        iter.log_line("", typ, "");
    }

    fn log_start_value(iter: *Iterator, typ: []const u8) void {
        iter.log_line("+", typ, "");
        if (debug) iter.log_depth = sat_add_u8(iter.log_depth, 1);
    }
    fn log_end_value(iter: *Iterator, typ: []const u8) void {
        if (debug) iter.log_depth = sat_sub_u8(iter.log_depth, 1);
        iter.log_line("-", typ, "");
    }

    fn log_error(iter: *Iterator, err: []const u8) void {
        _ = iter;
        if (debug) std.log.err("{s}", .{err});
    }
    fn log_error_fmt(iter: *Iterator, comptime fmt: []const u8, args: anytype) void {
        _ = iter;
        if (debug) std.log.err(fmt, args);
    }

    const State = enum {
        object_begin,
        object_field,
        object_continue,
        scope_end,
        array_begin,
        array_value,
        array_continue,
        document_end,
    };

    pub fn walk_document(iter: *Iterator, visitor: *TapeBuilder) !void {
        iter.log_start();
        if (iter.at_eof()) return error.EMPTY;
        iter.log_start_value("document");
        try visitor.visit_document_start(iter);
        if (iter.parser.bytes.len == 0) return iter.document_end(visitor);

        const value = iter.advance();
        var state: State = blk: {
            switch (value[0]) {
                '{' => {
                    if (iter.peek()[0] == '}') {
                        _ = iter.advance();
                        iter.log_value("empty object");
                        try visitor.visit_empty_object(iter);
                    } else break :blk .object_begin;
                },
                '[' => {
                    if (iter.peek()[0] == ']') {
                        _ = iter.advance();
                        iter.log_value("empty array");
                        try visitor.visit_empty_array(iter);
                    } else break :blk .array_begin;
                },
                else => try visitor.visit_root_primitive(iter, value),
            }
            break :blk .document_end;
        };

        while (true) {
            state = switch (state) {
                .object_begin => try iter.object_begin(visitor),
                .object_field => try iter.object_field(visitor),
                .object_continue => try iter.object_continue(visitor),
                .scope_end => try iter.scope_end(visitor),
                .array_begin => try iter.array_begin(visitor),
                .array_value => try iter.array_value(visitor),
                .array_continue => try iter.array_continue(visitor),
                .document_end => return try iter.document_end(visitor),
            };
        }
    }

    inline fn object_begin(iter: *Iterator, visitor: *TapeBuilder) Error!State {
        iter.log_start_value("object");
        iter.depth += 1;
        // iter.log_line_fmt("", "depth", "{d}/{d}", .{ iter.depth, iter.parser.max_depth });

        if (iter.depth >= iter.parser.max_depth) {
            iter.log_error("Exceeded max depth!");
            return error.DEPTH_ERROR;
        }
        try visitor.visit_object_start(iter);

        const key = iter.advance();
        if (key[0] != '"') {
            iter.log_error("Object does not start with a key");
            return error.TAPE_ERROR;
        }
        iter.increment_count();
        try visitor.visit_key(iter, key);
        return .object_field;
    }

    inline fn object_field(iter: *Iterator, visitor: *TapeBuilder) Error!State {
        if (iter.advance()[0] != ':') {
            iter.log_error("Missing colon after key in object");
            return error.TAPE_ERROR;
        }

        const value = iter.advance();
        switch (value[0]) {
            '{' => if (iter.peek()[0] == '}') {
                _ = iter.advance();
                iter.log_value("empty object");
                try visitor.visit_empty_object(iter);
            } else return .object_begin,
            '[' => if (iter.peek()[0] == ']') {
                _ = iter.advance();
                iter.log_value("empty array");
                try visitor.visit_empty_array(iter);
            } else return .array_begin,
            else => try visitor.visit_primitive(iter, value),
        }
        return .object_continue;
    }

    inline fn object_continue(iter: *Iterator, visitor: *TapeBuilder) Error!State {
        const value = iter.advance();
        // std.log.debug("object_continue() value '{c}'", .{value});
        switch (value[0]) {
            ',' => {
                iter.increment_count();
                const key = iter.advance();
                // println("key '{c}'", .{key});
                if (key[0] != '"') {
                    iter.log_error("Key string missing at beginning of field in object");
                    return error.TAPE_ERROR;
                }
                try visitor.visit_key(iter, key);
                return .object_field;
            },
            '}' => {
                iter.log_end_value("object");
                _ = try visitor.visit_object_end(iter);
                return .scope_end;
            },
            else => {
                iter.log_error("No comma between object fields");
                return error.TAPE_ERROR;
            },
        }
        unreachable;
    }

    inline fn scope_end(iter: *Iterator, _: *TapeBuilder) Error!State {
        // std.log.debug("scope_end iter.depth {}", .{iter.depth});
        iter.depth -= 1;
        if (iter.depth == 0) return .document_end;
        const is_array = iter.current_container().is_array;
        if (is_array)
            return .array_continue;

        return .object_continue;
    }

    inline fn array_begin(iter: *Iterator, visitor: *TapeBuilder) Error!State {
        iter.log_start_value("array");
        iter.depth += 1;
        if (iter.depth >= iter.parser.max_depth) {
            iter.log_error("Exceeded max depth!");
            return error.DEPTH_ERROR;
        }
        _ = try visitor.visit_array_start(iter);
        iter.increment_count();
        return .array_value;
    }

    inline fn array_value(iter: *Iterator, visitor: *TapeBuilder) Error!State {
        const value = iter.advance();
        switch (value[0]) {
            '{' => {
                if (iter.peek()[0] == '}') {
                    _ = iter.advance();
                    iter.log_value("empty object");
                    try visitor.visit_empty_object(iter);
                } else return .object_begin;
            },
            '[' => {
                if (iter.peek()[0] == ']') {
                    _ = iter.advance();
                    iter.log_value("empty array");
                    try visitor.visit_empty_array(iter);
                } else return .array_begin;
            },
            else => try visitor.visit_primitive(iter, value),
        }
        return .array_continue;
    }

    inline fn array_continue(iter: *Iterator, visitor: *TapeBuilder) Error!State {
        switch (iter.advance()[0]) {
            ',' => {
                iter.increment_count();
                return .array_value;
            },
            ']' => {
                iter.log_end_value("array");
                try visitor.visit_array_end(iter);
                return .scope_end;
            },
            else => {
                iter.log_error("Missing comma between array values");
                return error.TAPE_ERROR;
            },
        }
        unreachable;
    }

    inline fn ptr_diff(comptime T: type, p1: anytype, p2: anytype) !T {
        const U = std.meta.Child(@TypeOf(p1));
        const V = std.meta.Child(@TypeOf(p2));
        if (@sizeOf(U) != @sizeOf(V)) @compileError("ptr_diff: mismatched child sizes");
        const diff = @ptrToInt(p1) - @ptrToInt(p2);
        return std.math.cast(T, diff / (@sizeOf(U)));
    }

    inline fn document_end(iter: *Iterator, visitor: *TapeBuilder) Error!void {
        iter.log_end_value("document");
        try visitor.visit_document_end(iter);
        iter.parser.next_structural_index = try ptr_diff(
            u32,
            iter.next_structural,
            iter.parser.indexer.bit_indexer.tail.items.ptr,
        );

        // If we didn't make it to the end, it's an error
        // std.log.debug("next_structural_index {} n_structural_indexes {}", .{ iter.parser.next_structural_index, iter.parser.n_structural_indexes });
        // have to add because there are 3 additional items added to tail in finish()
        if (!STREAMING and iter.parser.next_structural_index != iter.parser.n_structural_indexes) {
            iter.log_error("More than one JSON value at the root of the document, or extra characters at the end of the JSON!");
            return error.TAPE_ERROR;
        }
    }

    inline fn current_container(iter: *Iterator) *OpenContainerInfo {
        // std.log.debug("current_container iter.parser.open_containers.len {} iter.depth {}", .{ iter.parser.open_containers.items.len, iter.depth });
        return &iter.parser.open_containers.items[iter.depth];
    }
    inline fn increment_count(iter: *Iterator) void {
        // we have a key value pair in the object at parser.dom_parser.depth - 1
        iter.current_container().open_container.count += 1;
    }
};

pub const TapeType = enum(u8) {
    ROOT = 'r',
    START_ARRAY = '[',
    START_OBJECT = '{',
    END_ARRAY = ']',
    END_OBJECT = '}',
    STRING = '"',
    INT64 = 'l',
    UINT64 = 'u',
    DOUBLE = 'd',
    TRUE = 't',
    FALSE = 'f',
    NULL = 'n',
    INVALID = 'i',
    pub inline fn as_u64(tt: TapeType) u64 {
        return @as(u64, @enumToInt(tt)) << 56;
    }
    pub inline fn from_u64(x: u64) TapeType {
        return std.meta.intToEnum(TapeType, (x & 0xff00000000000000) >> 56) catch .INVALID;
    }
    pub inline fn encode_value(tt: TapeType, value: u64) u64 {
        assert(value <= std.math.maxInt(u56));
        return @as(u64, @enumToInt(tt)) << 56 | value;
    }
    pub inline fn extract_value(item: u64) u64 {
        return item & value_mask;
    }
    pub const value_mask = 0x00ffffffffffffff;
    pub const count_mask = 0xffffff;
};

pub const TapeBuilder = struct {
    tape: *std.ArrayListUnmanaged(u64),
    current_string_buf_loc: [*]u8,

    pub fn init(doc: *Document) TapeBuilder {
        return .{
            .tape = &doc.tape,
            .current_string_buf_loc = doc.string_buf.ptr,
        };
    }

    pub inline fn append(tb: *TapeBuilder, iter: *Iterator, val: u64, tt: TapeType) !void {
        _ = iter;
        // iter.log_line_fmt("", "append", "val {} tt {}", .{ val, tt });
        try tb.tape.append(iter.parser.allocator, val | tt.as_u64());
    }

    pub inline fn append_double(tb: *TapeBuilder, iter: *Iterator, val: f64) !void {
        return tb.append2(iter, 0, @bitCast(u64, val), .DOUBLE);
    }

    pub inline fn append_i64(tb: *TapeBuilder, iter: *Iterator, val: u64) !void {
        return tb.append2(iter, 0, val, .INT64);
    }

    pub inline fn append_u64(tb: *TapeBuilder, iter: *Iterator, val: u64) !void {
        return tb.append2(iter, 0, val, .UINT64);
    }

    pub inline fn write(tb: *TapeBuilder, iter: *Iterator, idx: usize, val: u64, tt: TapeType) void {
        _ = iter;
        // iter.log_line_fmt("", "write", "val {} tt {} idx {}", .{ val, tt, idx });
        assert(idx < tb.tape.items.len);
        tb.tape.items[idx] = val | tt.as_u64();
    }

    pub inline fn next_tape_index(tb: TapeBuilder) u32 {
        return @intCast(u32, tb.tape.items.len);
    }

    pub inline fn skip(tb: TapeBuilder, allocator: *mem.Allocator) !void {
        _ = try tb.tape.addOne(allocator);
    }

    pub inline fn skip_double(tb: TapeBuilder, allocator: *mem.Allocator) !void {
        _ = tb;
        _ = allocator;
        // _ = try tb.tape.addManyAsArray(allocator, 2);
    }

    pub inline fn empty_container(tb: *TapeBuilder, iter: *Iterator, start: TapeType, end: TapeType) Error!void {
        const start_index = tb.next_tape_index();
        try tb.append(iter, start_index + 2, start);
        try tb.append(iter, start_index, end);
    }

    pub inline fn start_container(
        tb: TapeBuilder,
        open_containers: *std.ArrayListUnmanaged(OpenContainerInfo),
        allocator: *mem.Allocator,
        is_array: bool,
        count: u32,
    ) !void {
        // try open_containers.ensureUnusedCapacity(allocator, 1);
        // const cidx = open_containers.addOneAssumeCapacity();
        const tape_idx = tb.next_tape_index();
        try open_containers.append(allocator, .{
            .is_array = is_array,
            .open_container = .{
                .tape_index = @intCast(u32, tape_idx),
                .count = count,
            },
        });
        return tb.skip(allocator);
    }

    pub inline fn end_container(tb: *TapeBuilder, iter: *Iterator, start: TapeType, end: TapeType) Error!void {
        // Write the ending tape element, pointing at the start location
        // const container = iter.parser.open_containers.get(iter.depth).open_container;
        const container = iter.parser.open_containers.items[iter.depth].open_container;
        defer iter.parser.open_containers.shrinkRetainingCapacity(iter.depth);
        const start_tape_index = container.tape_index;
        try tb.append(iter, start_tape_index, end);
        // Write the start tape element, pointing at the end location (and including count)
        // count can overflow if it exceeds 24 bits... so we saturate
        // the convention being that a cnt of 0xffffff or more is undetermined in value (>=  0xffffff).
        const cntsat: u32 = std.math.min(@intCast(u32, container.count), 0xFFFFFF);
        //   tape_writer::write(iter.dom_parser.doc->tape[start_tape_index], next_tape_index(iter) | (uint64_t(cntsat) << 32), start);

        iter.log_line_fmt("", "end_container", "next_tape_index {}", .{tb.next_tape_index()});
        tb.write(iter, start_tape_index, tb.next_tape_index() | (@as(u64, cntsat) << 32), start);
        // tb.write(iter, start_tape_index, tb.next_tape_index() | cntsat, start);
    }

    inline fn on_start_string(tb: *TapeBuilder, iter: *Iterator) ![*]u8 {
        // iter.log_line_fmt("", "start_string", "iter.parser.doc.string_buf.len {}", .{iter.parser.doc.string_buf.len});
        try tb.append(iter, @ptrToInt(tb.current_string_buf_loc) - @ptrToInt(iter.parser.doc.string_buf.ptr), .STRING);
        return tb.current_string_buf_loc + @sizeOf(u32);
    }
    inline fn on_end_string(tb: *TapeBuilder, iter: *Iterator, dst: [*]u8) !void {
        const str_len = (try std.math.cast(
            u32,
            @ptrToInt(dst - @ptrToInt(tb.current_string_buf_loc + @sizeOf(u32))),
        ));
        // println("str_len {} str '{s}'", .{ str_len, (tb.current_string_buf_loc + 4)[0..str_len] });

        // TODO check for overflow in case someone has a crazy string (>=4GB?)
        // But only add the overflow check when the document itself exceeds 4GB
        // Currently unneeded because we refuse to parse docs larger or equal to 4GB.

        // NULL termination is still handy if you expect all your strings to
        // be NULL terminated? It comes at a small cost

        // iter.log_line_fmt("", "on_string_end", "{s}", .{str_start[0..str_len]});
        @memcpy(tb.current_string_buf_loc, mem.asBytes(&str_len), @sizeOf(u32));
        dst[0] = 0;
        iter.parser.doc.string_buf.len += str_len + 1 + @sizeOf(u32);
        // println("buf.len {} buf.cap {}", .{ iter.parser.doc.string_buf.len, iter.parser.doc.string_buf_cap });
        assert(iter.parser.doc.string_buf.len <= iter.parser.doc.string_buf_cap);
        tb.current_string_buf_loc += str_len + 1 + @sizeOf(u32);
    }

    pub inline fn append2(tb: *TapeBuilder, iter: *Iterator, val: u64, val2: anytype, tt: TapeType) !void {
        try tb.append(iter, val, tt);
        assert(@sizeOf(@TypeOf(val2)) == 8);
        try tb.tape.append(iter.parser.allocator, val2);
    }

    inline fn visit_root_primitive(visitor: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return switch (value[0]) {
            '"' => visitor.visit_string(iter, value, false),
            't' => visitor.visit_true_atom(iter, value),
            'f' => visitor.visit_false_atom(iter, value),
            'n' => visitor.visit_null_atom(iter, value),
            '-', '0'...'9' => visitor.visit_number(iter, value),
            else => blk: {
                iter.log_error("Document starts with a non-value character");
                break :blk error.TAPE_ERROR;
            },
        };
    }

    inline fn visit_number(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log_value("number");
        try NumberParsing.parse_number(value, iter, tb);
        // TODO: support more number types

    }
    inline fn visit_true_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log_value("true");
        assert(value[0] == 't');
        if (!try AtomParsing.is_valid_true_atom(value + 1)) return error.T_ATOM_ERROR;
        try tb.append(iter, 0, TapeType.TRUE);
    }
    inline fn visit_false_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log_value("false");
        assert(value[0] == 'f');
        if (!try AtomParsing.is_valid_false_atom(value + 1)) return error.T_ATOM_ERROR;
        try tb.append(iter, 0, TapeType.FALSE);
    }
    inline fn visit_null_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log_value("null");
        assert(value[0] == 'n');
        if (!try AtomParsing.is_valid_null_atom(value + 1)) return error.T_ATOM_ERROR;
        try tb.append(iter, 0, TapeType.NULL);
    }

    inline fn visit_primitive(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return switch (value[0]) {
            '"' => tb.visit_string(iter, value, false),
            't' => tb.visit_true_atom(iter, value),
            'f' => tb.visit_false_atom(iter, value),
            'n' => tb.visit_null_atom(iter, value),
            '-', '0'...'9' => tb.visit_number(iter, value),
            else => |c| blk: {
                iter.log_error_fmt("Non-value found when value was expected.  Value: '{c}' - (0x{x}:{})", .{ c, c, c });
                break :blk error.TAPE_ERROR;
            },
        };
    }

    inline fn visit_string(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8, key: bool) Error!void {
        iter.log_value(if (key) "key" else "string");
        var dst = try tb.on_start_string(iter);
        dst = StringParsing.parse_string(value + 1, dst) orelse {
            iter.log_error("Invalid escape in string");
            return error.STRING_ERROR;
        };

        try tb.on_end_string(iter, dst);
    }

    pub inline fn visit_key(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return tb.visit_string(iter, value, true);
    }

    pub inline fn visit_empty_object(tb: *TapeBuilder, iter: *Iterator) Error!void {
        return tb.empty_container(iter, .START_OBJECT, .END_OBJECT);
    }

    pub inline fn visit_empty_array(tb: *TapeBuilder, iter: *Iterator) Error!void {
        return tb.empty_container(iter, .START_ARRAY, .END_ARRAY);
    }

    pub inline fn visit_array_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, iter.parser.allocator, true, 0);
    }

    pub inline fn visit_array_end(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.end_container(iter, .START_ARRAY, .END_ARRAY);
    }

    pub inline fn visit_object_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, iter.parser.allocator, false, 0);
    }
    pub inline fn visit_object_end(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.end_container(iter, .START_OBJECT, .END_OBJECT);
    }
    pub inline fn visit_document_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, iter.parser.allocator, false, 0);
    }
    pub inline fn visit_document_end(tb: *TapeBuilder, iter: *Iterator) !void {
        // try tb.tape.append(0, .ROOT);
        // tape_writer::write(iter.dom_parser.doc->tape[start_tape_index], next_tape_index(iter), internal::tape_type::ROOT);
        tb.write(iter, 0, tb.next_tape_index(), .ROOT);
        iter.log_line_fmt("?", "document_end", "open_containers.len {} tape.len {}", .{ iter.parser.open_containers.items.len, tb.tape.items.len });
        return tb.append(iter, 0, .ROOT);
    }
};

const DEFAULT_MAX_DEPTH = 1024;
const STEP_SIZE = 64;
const STREAMING = false;
const SIMDJSON_PADDING = 32;

pub const Parser = struct {
    filename: []const u8,
    allocator: *mem.Allocator,
    prev_escaped: u64 = 0,
    prev_in_string: u64 = 0,
    prev_scalar: u64 = 0,
    next_structural_index: u32 = 0,
    doc: Document,
    indexer: StructuralIndexer,
    open_containers: std.ArrayListUnmanaged(OpenContainerInfo), //std.MultiArrayList(OpenContainerInfo),
    max_depth: u16,
    n_structural_indexes: u32 = 0,
    bytes: []u8 = &[_]u8{},
    input_len: u32 = 0,

    const Options = struct {
        max_depth: u16 = DEFAULT_MAX_DEPTH,
    };

    pub fn initFile(allocator: *mem.Allocator, filename: []const u8, options: Options) !Parser {
        var parser = Parser{
            .filename = filename,
            .allocator = allocator,
            .doc = Document.init(),
            .indexer = try StructuralIndexer.init(allocator, std.mem.page_size),
            .open_containers = std.ArrayListUnmanaged(OpenContainerInfo){},
            .max_depth = options.max_depth,
        };
        parser.input_len = try parser.read_file(filename);
        try parser.doc.allocate(allocator, parser.input_len);
        return parser;
    }

    const ascii_space = 0x20;

    pub fn initFixedBuffer(allocator: *mem.Allocator, input: []const u8, options: Options) !Parser {
        var parser = Parser{
            .filename = "<fixed buffer>",
            .allocator = allocator,
            .doc = Document.init(),
            .indexer = try StructuralIndexer.init(allocator, std.mem.page_size),
            .bytes = &[_]u8{},
            .open_containers = std.ArrayListUnmanaged(OpenContainerInfo){}, // std.MultiArrayList(OpenContainerInfo){},
            .max_depth = options.max_depth,
        };
        parser.input_len = try std.math.cast(u32, input.len);
        const paddedlen = try std.math.add(u32, parser.input_len, SIMDJSON_PADDING);
        parser.bytes = try parser.allocator.alloc(u8, paddedlen);
        mem.copy(u8, parser.bytes, input);

        // We write zeroes in the padded region to avoid having uninitized
        // garbage. If nothing else, garbage getting read might trigger a
        // warning in a memory checking.
        std.mem.set(u8, parser.bytes[parser.input_len..], ascii_space);
        try parser.doc.allocate(allocator, parser.input_len);
        return parser;
    }

    fn read_file(parser: *Parser, filename: []const u8) !u32 {
        var f = try std.fs.cwd().openFile(filename, .{ .read = true });
        defer f.close();
        const len = try std.math.cast(u32, try f.getEndPos());
        if (parser.bytes.len < len) {
            const paddedlen = try std.math.add(u32, len, SIMDJSON_PADDING);
            parser.bytes = try parser.allocator.realloc(parser.bytes, paddedlen);
            const nbytes = try f.read(parser.bytes);
            if (nbytes < len) return error.IO_ERROR;
            // We write zeroes in the padded region to avoid having uninitized
            // garbage. If nothing else, garbage getting read might trigger a
            // warning in a memory checking.
            std.mem.set(u8, parser.bytes[len..], ascii_space);
        }
        return len;
    }

    pub fn deinit(parser: *Parser) void {
        parser.indexer.bit_indexer.tail.deinit(parser.allocator);
        parser.open_containers.deinit(parser.allocator);
        parser.doc.deinit(parser.allocator);
        parser.allocator.free(parser.bytes);
    }

    inline fn find_escaped(parser: *Parser, backslash_: u64) u64 {
        // If there was overflow, pretend the first character isn't a backslash
        var backslash = backslash_ & ~parser.prev_escaped;
        const follows_escape = backslash << 1 | parser.prev_escaped;

        // Get sequences starting on even bits by clearing out the odd series using +
        const even_bits: u64 = 0x5555555555555555;
        const odd_sequence_starts = backslash & ~even_bits & ~follows_escape;
        var sequences_starting_on_even_bits: u64 = undefined;
        // println("{b:0>64} | prev_escaped a", .{@bitReverse(u64, parser.prev_escaped)});
        parser.prev_escaped = @boolToInt(@addWithOverflow(u64, odd_sequence_starts, backslash, &sequences_starting_on_even_bits));
        // println("{b:0>64} | prev_escaped b", .{@bitReverse(u64, parser.prev_escaped)});
        const invert_mask = sequences_starting_on_even_bits << 1; // The mask we want to return is the *escaped* bits, not escapes.

        // Mask every other backslashed character as an escaped character
        // Flip the mask for sequences that start on even bits, to correct them
        return (even_bits ^ invert_mask) & follows_escape;
    }

    inline fn nextStringBlock(parser: *Parser, input_vec: u8x64) StringBlock {
        // const input_vec = input_vec.*;
        const backslash_vec = input_vec == @splat(64, @as(u8, '\\'));
        const backslash = @bitCast(u64, backslash_vec);
        const escaped = parser.find_escaped(backslash);
        const quote_vec = input_vec == @splat(64, @as(u8, '"'));
        const quote = @bitCast(u64, quote_vec) & ~escaped;

        //
        // prefix_xor flips on bits inside the string (and flips off the end quote).
        //
        // Then we xor with prev_in_string: if we were in a string already, its effect is flipped
        // (characters inside strings are outside, and characters outside strings are inside).
        //
        const ones: u64x2 = [1]u64{std.math.maxInt(u64)} ** 2;
        var in_string = carrylessMul(.{ quote, 0 }, ones)[0];
        // println("{b:0>64} | quote a", .{@bitReverse(u64, quote)});
        // println("{b:0>64} | ones[0]", .{@bitReverse(u64, ones[0])});
        // println("{b:0>64} | in_string a", .{@bitReverse(u64, in_string)});
        // println("{b:0>64} | prev_in_string a", .{@bitReverse(u64, parser.prev_in_string)});
        in_string ^= parser.prev_in_string;
        // println("{b:0>64} | in_string b", .{@bitReverse(u64, in_string)});

        //
        // Check if we're still in a string at the end of the box so the next block will know
        //
        // right shift of a signed value expected to be well-defined and standard
        // compliant as of C++20, John Regher from Utah U. says this is fine code
        //
        // println("{b:0>64} | prev_in_string a", .{@bitReverse(u64, parser.prev_in_string)});
        // println("{b:0>64} | @bitCast(i64, in_string) ", .{@bitReverse(i64, @bitCast(i64, in_string))});
        // println("{b:0>64} | @bitCast(i64, in_string) >> 63 ", .{@bitReverse(i64, @bitCast(i64, in_string) >> 63)});
        // println("{b:0>64} | @bitCast(u64, @bitCast(i64, in_string) >> 63) ", .{@bitReverse(u64, @bitCast(u64, @bitCast(i64, in_string) >> 63))});
        parser.prev_in_string = @bitCast(u64, @bitCast(i64, in_string) >> 63);
        // parser.prev_in_string =  in_string >> 63;

        // Use ^ to turn the beginning quote off, and the end quote on.

        // We are returning a function-local object so either we get a move constructor
        // or we get copy elision.
        return StringBlock{
            .backslash = backslash,
            .escaped = escaped,
            .quote = quote,
            .in_string = in_string,
        };
    }

    fn stage1(parser: *Parser) !void {
        const end_pos = parser.input_len;
        const end_pos_minus_step = if (end_pos > STEP_SIZE) end_pos - STEP_SIZE else 0;

        var pos: u32 = 0;
        while (pos < end_pos_minus_step) : (pos += STEP_SIZE) {
            // println("i {} pos {}", .{ i, pos });
            const read_buf = parser.bytes[pos..][0..STEP_SIZE];
            try parser.indexer.step(read_buf.*, parser, pos);
            // for (blocks) |block| {
            //     println("{b:0>64} | characters.whitespace", .{@bitReverse(u64, block.characters.whitespace)});
            //     println("{b:0>64} | characters.op", .{@bitReverse(u64, block.characters.op)});
            //     println("{b:0>64} | in_string", .{@bitReverse(u64, block.strings.in_string)});
            // }
        }
        var read_buf = [1]u8{0x20} ** STEP_SIZE;
        std.mem.copy(u8, &read_buf, parser.bytes[pos..end_pos]);
        // std.log.debug("read_buf {d}", .{read_buf});
        try parser.indexer.step(read_buf, parser, pos);
        try parser.indexer.finish(parser, pos + 64, end_pos, STREAMING);

        // if (parser.indexer.bit_indexer.tail.items.len >= 4) {
        //     // TODO: verify all input was processed
        //     println("final pos {} end_pos {} parser.input_len {} tail.last {}", .{ pos, end_pos, parser.input_len, parser.indexer.bit_indexer.tail.items[parser.indexer.bit_indexer.tail.items.len - 4] });
        //     println("tail.items {d}", .{parser.indexer.bit_indexer.tail.items});
        //     if (parser.input_len != parser.indexer.bit_indexer.tail.items[parser.indexer.bit_indexer.tail.items.len - 4]) return error.TAPE_ERROR;
        // }
    }

    fn stage2(parser: *Parser) !void {
        var iter = Iterator.init(parser, 0);
        var tb = TapeBuilder.init(&parser.doc);
        try iter.walk_document(&tb);
    }

    pub fn parse(parser: *Parser) !void {
        if (parser.stage1()) |_| {} else |err| switch (err) {
            else => return err,
        }
        return parser.stage2();
    }

    pub fn element(parser: Parser) Element {
        return .{
            .tape = .{ .doc = &parser.doc, .idx = 1 },
        };
    }
};

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    var parser: Parser = undefined;
    // debug = true;

    if (os.argv.len == 1) {
        var stdin = std.io.getStdIn().reader();
        const input = try stdin.readAllAlloc(allocator, std.math.maxInt(u32));
        parser = try Parser.initFixedBuffer(allocator, input, .{});
    } else if (os.argv.len == 2) {
        const filename = std.mem.span(os.argv[1]);
        parser = try Parser.initFile(allocator, filename, .{});
    } else {
        std.log.err("Too many arguments.  Please provide input via filename or stdin", .{});
        return 1;
    }

    defer parser.deinit();
    try parser.parse();
    // catch |err| switch (err) {
    //     // error.EndOfStream => {},
    //     else => {
    //         std.log.err("parse failed. error {}", .{err});
    //         return 1;
    //     },
    // };
    std.log.debug("parse valid", .{});
    return 0;
}

const ElementType = enum(u8) {
    ///< dom::array
    ARRAY = '[',
    ///< dom::object
    OBJECT = '{',
    ///< int64_t
    INT64 = 'l',
    ///< uint64_t: any integer that fits in uint64_t but *not* int64_t
    UINT64 = 'u',
    ///< double: Any number with a "." or "e" that fits in double.
    DOUBLE = 'd',
    ///< std::string_view
    STRING = '"',
    ///< bool
    BOOL = 't',
    ///< null
    NULL = 'n',
};

const Array = struct {
    tape: TapeRef,
    pub fn at(a: Array, idx: usize) ?Element {
        var it = TapeRefIterator.init(.{ .doc = a.tape.doc, .idx = a.tape.idx + 1 }, a.tape.after_element() - 1);
        const target_idx = idx + it.tape.idx + idx;
        while (true) {
            if (it.tape.idx == target_idx)
                return Element{ .tape = .{ .doc = it.tape.doc, .idx = it.tape.idx } };
            _ = it.next() orelse break;
        }
        return null;
    }
};
const Object = struct {
    tape: TapeRef,
    pub fn at_key(o: Object, key: []const u8) ?Element {
        var it = TapeRefIterator.init(.{ .doc = o.tape.doc, .idx = o.tape.idx + 1 }, o.tape.after_element() - 1);
        while (true) {
            if (it.tape.key_equals(key))
                return Element{ .tape = .{ .doc = it.tape.doc, .idx = it.tape.idx + 1 } };
            _ = it.next() orelse break;
        }
        return null;
    }
};
const Value = union(ElementType) {
    NULL,
    BOOL: bool,
    INT64: i64,
    UINT64: u64,
    DOUBLE: f64,
    STRING: []const u8,
    ARRAY: Array,
    OBJECT: Object,
};
const TapeRef = struct {
    doc: *const Document,
    idx: usize,

    pub inline fn is(tr: TapeRef, tt: TapeType) bool {
        return tr.tape_ref_type() == tt;
    }
    pub inline fn tape_ref_type(tr: TapeRef) TapeType {
        return TapeType.from_u64(tr.current_raw());
    }
    pub inline fn value(tr: TapeRef) u64 {
        return TapeType.extract_value(tr.current_raw());
    }
    pub inline fn after_element(tr: TapeRef) u64 {
        return switch (tr.tape_ref_type()) {
            .START_ARRAY, .START_OBJECT => tr.matching_brace_idx(),
            .UINT64, .INT64, .DOUBLE => tr.idx + 2,
            else => tr.idx + 1,
        };
    }
    pub inline fn matching_brace_idx(tr: TapeRef) u32 {
        const result = @truncate(u32, tr.current_raw());
        std.log.debug("TapeRef matching_brace_idx() for {} {}", .{ tr.tape_ref_type(), result });
        return result;
        // return tr.value() + tr.scope_count();

    }
    pub inline fn current_raw(tr: TapeRef) u64 {
        std.log.debug("TapeRef current() idx {} len {}", .{ tr.idx, tr.doc.tape.items.len });
        return tr.doc.tape.items[tr.idx];
    }
    pub inline fn scope_count(tr: TapeRef) u32 {
        return @truncate(u32, (tr.current_raw() >> 32) & TapeType.count_mask);
    }

    pub fn get_string_length(tr: TapeRef) u32 {
        const string_buf_index = tr.value();
        return mem.readIntLittle(u32, (tr.doc.string_buf.ptr + string_buf_index)[0..@sizeOf(u32)]);
    }

    pub fn get_c_str(tr: TapeRef) [*:0]const u8 {
        return @ptrCast([*:0]const u8, tr.doc.string_buf.ptr + tr.value() + @sizeOf(u32));
    }

    pub fn get_as_type(tr: TapeRef, comptime T: type) T {
        comptime assert(@sizeOf(T) == @sizeOf(u64));
        return @bitCast(T, tr.current_raw());
    }

    pub fn get_next_as_type(tr: TapeRef, comptime T: type) T {
        comptime assert(@sizeOf(T) == @sizeOf(u64));
        return @bitCast(T, tr.doc.tape.items[tr.idx + 1]);
    }

    pub fn get_string(tr: TapeRef) []const u8 {
        return tr.get_c_str()[0..tr.get_string_length()];
    }

    pub fn key_equals(tr: TapeRef, string: []const u8) bool {
        // We use the fact that the key length can be computed quickly
        // without access to the string buffer.
        const len = tr.get_string_length();
        if (string.len == len) {
            // TODO: We avoid construction of a temporary string_view instance.
            return mem.eql(u8, string, tr.get_c_str()[0..len]);
        }
        return false;
    }
};

const TapeRefIterator = struct {
    tape: TapeRef,
    end_idx: u64,

    pub fn init(tape: TapeRef, end_idx: u64) TapeRefIterator {
        return .{
            .tape = tape,
            .end_idx = end_idx,
        };
    }
    pub fn next(tri: *TapeRefIterator) ?TapeRef {
        tri.tape.idx += 1;
        tri.tape.idx = tri.tape.after_element();
        return if (tri.tape.idx >= tri.end_idx) null else tri.tape;
    }
};
const Element = struct {
    tape: TapeRef,

    pub inline fn at_pointer(ele: Element, json_pointer: []const u8) ?Element {
        return switch (ele.tape.tape_ref_type()) {
            .START_OBJECT => ele.object(tape).at_pointer(json_pointer),
            .START_ARRAY => ele.array(tape).at_pointer(json_pointer),
            else => if (!json_pointer.len == 0) // a non-empty string is invalid on an atom
                INVALID_JSON_POINTER
            else
                ele,
            // an empty string means that we return the current node
            //   dom::element copy(*this);
            //   return simdjson_result<element>(std::move(copy));

        };
    }

    pub fn at_key(ele: Element, key: []const u8) ?Element {
        return if (ele.get(.OBJECT)) |o| o.OBJECT.at_key(key) else |_| null;
    }

    pub fn at(ele: Element, idx: usize) ?Element {
        return if (ele.get(.ARRAY)) |a| a.ARRAY.at(idx) else |_| null;
    }
    const E = error{INCORRECT_TYPE};
    pub fn get(ele: Element, ele_type: ElementType) E!Value {
        return switch (ele_type) {
            .OBJECT => Value{ .OBJECT = try ele.get_object() },
            .ARRAY => Value{ .ARRAY = try ele.get_array() },
            .INT64 => Value{ .INT64 = try ele.get_int64() },
            .UINT64 => Value{ .UINT64 = try ele.get_uint64() },
            .DOUBLE => Value{ .DOUBLE = try ele.get_double() },
            .STRING => Value{ .STRING = try ele.get_string() },
            .BOOL => Value{ .BOOL = try ele.get_bool() },
            .NULL => if (ele.tape.is(.NULL)) Value{ .NULL = {} } else error.INCORRECT_TYPE,
        };
    }

    pub fn get_array(ele: Element) !Array {
        return (try ele.get_tape_type(.START_ARRAY)).ARRAY;
    }
    pub fn get_object(ele: Element) !Object {
        return (try ele.get_tape_type(.START_OBJECT)).OBJECT;
    }
    pub fn get_int64(ele: Element) !i64 {
        return (try ele.get_tape_type(.INT64)).INT64;
    }
    pub fn get_uint64(ele: Element) !u64 {
        return (try ele.get_tape_type(.UINT64)).UINT64;
    }
    pub fn get_double(ele: Element) !f64 {
        return (try ele.get_tape_type(.DOUBLE)).DOUBLE;
    }
    pub fn get_string(ele: Element) ![]const u8 {
        return (try ele.get_tape_type(.STRING)).STRING;
    }
    pub fn get_bool(ele: Element) !bool {
        return switch (ele.tape.tape_ref_type()) {
            .TRUE => true,
            .FALSE => false,
            else => error.INCORRECT_TYPE,
        };
    }

    pub fn get_tape_type(ele: Element, comptime tape_type: TapeType) !Value {
        return switch (ele.tape.tape_ref_type()) {
            tape_type => ele.as_tape_type(tape_type),
            else => error.INCORRECT_TYPE,
        };
    }
    pub fn next_tape_value(ele: Element, comptime T: type) T {
        comptime assert(@sizeOf(T) == @sizeOf(u64));
        return mem.readIntLittle(T, @ptrCast([*]const u8, ele.tape.doc.tape.items.ptr + ele.tape.idx + 1)[0..8]);
    }
    pub fn as_tape_type(ele: Element, comptime tape_type: TapeType) !Value {
        return switch (tape_type) {
            .ROOT,
            .END_ARRAY,
            .END_OBJECT,
            .INVALID,
            .NULL,
            => error.INCORRECT_TYPE,
            .START_ARRAY => Value{ .ARRAY = .{ .tape = ele.tape } },
            .START_OBJECT => Value{ .OBJECT = .{ .tape = ele.tape } },
            .STRING => Value{ .STRING = ele.tape.get_string() },
            .INT64 => Value{ .INT64 = ele.tape.get_next_as_type(i64) },
            .UINT64 => Value{ .UINT64 = ele.tape.get_next_as_type(u64) },
            .DOUBLE => Value{ .DOUBLE = ele.tape.get_next_as_type(f64) },
            .TRUE => Value{ .BOOL = true },
            .FALSE => Value{ .BOOL = false },
        };
    }

    pub fn is(ele: Element, ele_type: ElementType) bool {
        return if (ele.get(ele_type)) true else |_| false;
    }
};

const allr = testing.allocator;
test "tape build" {
    const input = @embedFile("../test/test.json");
    const expecteds = [_]u64{
        TapeType.ROOT.encode_value(37), //  pointing  to 37 (rightafter  last  node) :0
        TapeType.START_OBJECT.encode_value(37 | (8 << 32)), // pointing to 37, length 8  :1
        TapeType.STRING.encode_value(6), // "Width" :2
        TapeType.INT64.as_u64(), 800, // :3
        TapeType.STRING.encode_value(6), // "Height" :5
        TapeType.INT64.as_u64(), 600, // :6
        TapeType.STRING.encode_value(5), // "Title" :8
        TapeType.STRING.encode_value(4), // "View  from my room" :9
        TapeType.STRING.encode_value(3), // "Url" :10
        TapeType.STRING.encode_value(21), // "http://ex.com/img.png" :11
        TapeType.STRING.encode_value(7), // "Private" :12
        TapeType.FALSE.as_u64(), // :13
        TapeType.STRING.encode_value(9), // "Thumbnail" :14
        TapeType.START_OBJECT.encode_value(25 | (3 << 32)), // 25, length 3 :15
        TapeType.STRING.encode_value(3), // "Url" :16
        TapeType.STRING.encode_value(21), // "http://ex.com/th.png". :17
        TapeType.STRING.encode_value(6), // "Height" :18
        TapeType.INT64.as_u64(), 125, // :19
        TapeType.STRING.encode_value(6), // "Width" :21
        TapeType.INT64.as_u64(), 100, // :22
        TapeType.END_OBJECT.encode_value(15), //  pointing  to 15 :24
        TapeType.STRING.encode_value(5), // "array" :25
        TapeType.START_ARRAY.encode_value(34 | (3 << 32)), //  pointing  to 34 :26
        TapeType.INT64.as_u64(), 116, // :27
        TapeType.INT64.as_u64(), 943, // :29
        TapeType.INT64.as_u64(), 234, // :31
        TapeType.END_ARRAY.encode_value(26), //  pointing  to  26 :33
        TapeType.STRING.encode_value(5), // "Owner" :34
        TapeType.NULL.as_u64(), // :35
        TapeType.END_OBJECT.encode_value(1), //  pointing  to  1 :36
        TapeType.ROOT.encode_value(0), //  pointing  to 0 :37
    };

    var parser = try Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();

    // verify doc.string_buf
    var p = @ptrCast([*:0]u8, parser.doc.string_buf.ptr);
    var j: u8 = 0;
    const expected_strings: []const []const u8 = &.{
        "Width",   "Height",    "Title", "View from my room",    "Url",    "http://ex.com/img.png",
        "Private", "Thumbnail", "Url",   "http://ex.com/th.png", "Height", "Width",
        "array",   "Owner",
    };
    while (@ptrToInt(p) < @ptrToInt(parser.doc.string_buf.ptr + parser.doc.string_buf.len)) : (j += 1) {
        const len = mem.bytesAsValue(u32, p[0..4]).*;
        p += 4;
        const str = std.mem.span(p);
        println("{}:{}-'{s}' '{s}'", .{ j, len, str, p[0..10] });
        try testing.expectEqual(@intCast(u32, str.len), len);
        try testing.expectEqualStrings(expected_strings[j], str);
        p += str.len + 1;
    }

    var i: usize = 0;
    while (i < expecteds.len) : (i += 1) {
        const expected = expecteds[i];
        const expected_type = TapeType.from_u64(expected);
        const expected_val = TapeType.extract_value(expected);
        // println("{} : expected {s}:{}-{x}", .{ i, @tagName(expected_type), expected_val, expected });
        const tape_item = parser.doc.tape.items[i];
        const tape_type = TapeType.from_u64(tape_item);
        const tape_val = TapeType.extract_value(tape_item);
        // println("{} : actual   {s}:{}-{x}", .{ i, @tagName(tape_type), tape_val, tape_item });
        // println("actual {}:{}", .{expected_type, expected_val});
        // println("expected 0x{x} tape_item 0x{x}", .{ expected, tape_item });
        try testing.expectEqual(expected_type, tape_type);

        // println("expected_val {} tape_val {}", .{ expected_val, tape_val });
        if (expected_type != .STRING)
            try testing.expectEqual(expected_val, tape_val);

        if (expected_type == .INT64) {
            i += 1;
            println("{s} {}", .{ @tagName(expected_type), parser.doc.tape.items[i] });
            try testing.expectEqual(expecteds[i], parser.doc.tape.items[i]);
        }
    }
}

test "float" {
    {
        var parser = try Parser.initFixedBuffer(allr, "123.456", .{});
        defer parser.deinit();
        try parser.parse();
        try testing.expectEqual(
            TapeType.DOUBLE.encode_value(0),
            parser.doc.tape.items[1],
        );
        try testing.expectApproxEqAbs(
            @as(f64, 123.456),
            @bitCast(f64, parser.doc.tape.items[2]),
            0.000000001,
        );
        const ele = parser.element();
        const d = try ele.get_double();
        try testing.expectApproxEqAbs(@as(f64, 123.456), d, 0.000000001);
    }
    {
        var parser = try Parser.initFixedBuffer(allr, "[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]", .{});
        defer parser.deinit();
        try parser.parse();
        // for (parser.doc.tape.items) |tape_item, i|
        //     println("{}:{s} {}", .{ i, @tagName(TapeType.from_u64(tape_item)), TapeType.extract_value(tape_item) });
        try testing.expectEqual(
            TapeType.DOUBLE.encode_value(0),
            parser.doc.tape.items[2],
        );
        try testing.expectApproxEqAbs(
            @as(f64, 0.000000000000000000000000000000000000000000000000000000000000000000000000000001),
            @bitCast(f64, parser.doc.tape.items[3]),
            std.math.f64_epsilon,
        );
    }
}

test "search tape" {
    try test_search_tape();
}

fn test_search_tape() !void {
    var parser = try Parser.initFile(allr, "test/test.json", .{});
    defer parser.deinit();
    try parser.parse();
    const ele = parser.element();
    const thumb = ele.at_key("Thumbnail") orelse return testing.expect(false);
    try testing.expectEqual(thumb.tape.scope_count(), 3);
    try testing.expect(thumb.is(.OBJECT));
    try testing.expectEqual(thumb.tape.idx, 15);

    const url = thumb.at_key("Url") orelse return testing.expect(false);
    try testing.expectEqual(url.tape.idx, 17);
    try testing.expectEqualStrings("http://ex.com/th.png", try url.get_string());

    const ht = thumb.at_key("Height") orelse return testing.expect(false);
    try testing.expectEqual(@as(i64, 125), try ht.get_int64());

    const priv = ele.at_key("Private") orelse return testing.expect(false);
    try testing.expectEqual(false, try priv.get_bool());

    const owner = ele.at_key("Owner") orelse return testing.expect(false);
    try testing.expectEqual(true, owner.is(.NULL));

    const array = ele.at_key("array") orelse return testing.expect(false);
    try testing.expectEqual(array.tape.idx, 26);
    try testing.expectEqual(true, array.is(.ARRAY));

    const array1 = array.at(0) orelse return testing.expect(false);
    try testing.expectEqual(true, array1.is(.INT64));
    try testing.expectEqual(@as(i64, 116), try array1.get_int64());
}

pub fn main2() !u8 {
    // try test_search_tape();
    var parser = try Parser.initFile(allr, "../../c/simdjson/jsonexamples/twitter.json", .{});
    defer parser.deinit();
    try parser.parse();
    const count = try parser.element().at_key("search_metadata").?.at_key("count").?.get_int64();
    try testing.expectEqual(@as(i64, 100), count);
    return 0;
}
