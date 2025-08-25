const std = @import("std");
const mem = std.mem;
const os = std.os;
const assert = std.debug.assert;
const builtin = @import("builtin");

const atom_parsing = @import("atom_parsing.zig");
const c = @import("c_intrinsics.zig");
const cmn = @import("common.zig");
const Chunk = cmn.Chunk;
const IChunk = cmn.IChunk;
const ChunkArr = cmn.ChunkArr;
const chunk_len = cmn.chunk_len;
const Logger = @import("Logger.zig");
const number_parsing = @import("number_parsing.zig");
const string_parsing = @import("string_parsing.zig");
const v = @import("vector_types.zig");

pub const Document = struct {
    tape: std.ArrayListUnmanaged(u64) = .{},
    string_buf: std.ArrayListUnmanaged(u8) = .{},

    pub fn allocate(document: *Document, allocator: mem.Allocator, capacity: u32) !void {
        if (capacity == 0) return;

        // a pathological input like "[[[[..." would generate capacity tape elements, so
        // need a capacity of at least capacity + 1, but it is also possible to do
        // worse with "[7,7,7,7,6,7,7,7,6,7,7,6,[7,7,7,7,6,7,7,7,6,7,7,6,7,7,7,7,7,7,6"
        //where capacity + 1 tape elements are
        // generated, see issue https://github.com/simdjson/simdjson/issues/345
        // a document with only zero-length strings... could have capacity/3 string
        // and we would need capacity/3 * 5 bytes on the string buffer
        const tape_cap = cmn.ROUNDUP_N(capacity + 3, 64);
        try document.tape.ensureTotalCapacity(allocator, tape_cap);
        errdefer document.tape.deinit(allocator);

        const str_cap = cmn.ROUNDUP_N(5 * capacity / 3 + cmn.SIMDJSON_PADDING, 64);
        try document.string_buf.ensureTotalCapacity(allocator, str_cap);
        document.string_buf.items.len = 0;
    }

    pub fn deinit(doc: *Document, allocator: mem.Allocator) void {
        doc.tape.deinit(allocator);
        doc.string_buf.deinit(allocator);
    }
};

const BitIndexer = struct {
    tail: std.ArrayListUnmanaged(u32) = .{},

    // flatten out values in 'bits' assuming that they are are to have values of idx
    // plus their position in the bitvector, and store these indexes at
    // base_ptr[base] incrementing base as we go
    // will potentially store extra values beyond end of valid bits, so base_ptr
    // needs to be large enough to handle this
    fn write(indexer: *BitIndexer, reader_pos_: u64, bits_: u64) void {
        var bits = bits_;
        // In some instances, the next branch is expensive because it is mispredicted.
        // Unfortunately, in other cases,
        // it helps tremendously.
        // cmn.println("{b:0>64} | write() bits reader_pos_={}", .{ @bitReverse(bits_), reader_pos_ });
        if (bits == 0) {
            // cmn.println("", .{});
            return;
        }
        const reader_pos: i32 = @intCast(reader_pos_ - 64); //  this function is always passed last bits so reader_pos will be ahead by 64
        const cnt = @popCount(bits);
        // cmn.println(", reader_pos {}", .{reader_pos});
        const start_count = indexer.tail.items.len;

        // Do the first 8 all together
        {
            const new_items = indexer.tail.addManyAsArrayAssumeCapacity(8);
            for (new_items) |*ptr| {
                ptr.* = @intCast(reader_pos + @ctz(bits));
                bits = (bits -% 1) & bits;
                // std.log.debug("bits {}", .{bits});
            }
        }

        // Do the next 8 all together (we hope in most cases it won't happen at all
        // and the branch is easily predicted).
        if (cnt > 8) {
            const new_items = indexer.tail.addManyAsArrayAssumeCapacity(8);
            for (new_items) |*ptr| {
                ptr.* = @intCast(reader_pos + @ctz(bits));
                bits = (bits -% 1) & bits;
            }
        }

        // Most files don't have 16+ structurals per block, so we take several basically guaranteed
        // branch mispredictions here. 16+ structurals per block means either punctuation ({} [] , // :)
        // or the start of a value ("abc" true 123) every four characters.
        if (cnt > 16) {
            var i: usize = 16;
            while (true) {
                indexer.tail.appendAssumeCapacity(@intCast(reader_pos + @ctz(bits)));
                bits = (bits -% 1) & bits;
                i += 1;
                if (i >= cnt) break;
            }
        }

        // cmn.println("tail.items.len {d} start_count + cnt {d}", .{ indexer.tail.items.len, start_count + cnt });
        indexer.tail.shrinkRetainingCapacity(start_count + cnt);
    }
};

const Utf8Checker = struct {
    err: Chunk = zeros,
    prev_input_block: Chunk = zeros,
    prev_incomplete: Chunk = zeros,

    const zeros: ChunkArr = [1]u8{0} ** chunk_len;

    fn prev(comptime N: comptime_int, chunk: Chunk, prev_chunk: Chunk) Chunk {
        comptime assert(0 < N and N < chunk_len);
        return std.simd.mergeShift(prev_chunk, chunk, chunk_len - N);
    }
    const check_special_cases = if (cmn.is_arm64) check_special_cases_arm64 else check_special_cases_x86;
    // zig fmt: off
    fn check_special_cases_x86(input: Chunk, prev1: Chunk) Chunk {
        // Bit 0 = Too Short (lead byte/ASCII followed by lead byte/ASCII)
        // Bit 1 = Too Long (ASCII followed by continuation)
        // Bit 2 = Overlong 3-byte
        // Bit 4 = Surrogate
        // Bit 5 = Overlong 2-byte
        // Bit 7 = Two Continuations
        const TOO_SHORT: u8 = 1 << 0;   // 11______ 0_______
                                        // 11______ 11______
        const TOO_LONG: u8 = 1 << 1;    // 0_______ 10______
        const OVERLONG_3: u8 = 1 << 2;  // 11100000 100_____
        const SURROGATE: u8 = 1 << 4;   // 11101101 101_____
        const OVERLONG_2: u8 = 1 << 5;  // 1100000_ 10______
        const TWO_CONTS: u8 = 1 << 7;   // 10______ 10______
        const TOO_LARGE: u8 = 1 << 3;   // 11110100 1001____
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
        const OVERLONG_4: u8 = 1 << 6;  // 11110000 1000____
        const u3x32 = @Vector(32, u3);
        const byte_1_high_0 = prev1 >> @as(u3x32,@splat(4));
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
        const byte_1_high = c.mm256_shuffle_epi8(tbl1, byte_1_high_0);
        const CARRY: u8 = TOO_SHORT | TOO_LONG | TWO_CONTS; // These all have ____ in byte 1 .
        const byte_1_low0 = prev1 & @as(v.u8x32, @splat(0x0F));
        
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
        const byte_1_low = c.mm256_shuffle_epi8(tbl2, byte_1_low0);

        const byte_2_high_0 = input >> @as(u3x32, @splat(4));
        const tbl3 = [16]u8{
            // ________ 0_______ <ASCII in byte 2>
            TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
            TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
            // ________ 1000____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4,
            // ________ 1001____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE,
            // ________ 101_____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE, TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE,
            // ________ 11______
            TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
        } ** 2;
        const byte_2_high = c.mm256_shuffle_epi8(tbl3, byte_2_high_0);
        return (byte_1_high & byte_1_low & byte_2_high);
    }
    
    fn check_special_cases_arm64(input: Chunk, prev1: Chunk) Chunk {
        // Bit 0 = Too Short (lead byte/ASCII followed by lead byte/ASCII)
        // Bit 1 = Too Long (ASCII followed by continuation)
        // Bit 2 = Overlong 3-byte
        // Bit 4 = Surrogate
        // Bit 5 = Overlong 2-byte
        // Bit 7 = Two Continuations
     
        const TOO_SHORT: u8 = 1 << 0;   // 11______ 0_______
                                        // 11______ 11______
        const TOO_LONG: u8 = 1 << 1;    // 0_______ 10______
        const OVERLONG_3: u8 = 1 << 2;  // 11100000 100_____
        const SURROGATE: u8 = 1 << 4;   // 11101101 101_____
        const OVERLONG_2: u8 = 1 << 5;  // 1100000_ 10______
        const TWO_CONTS: u8 = 1 << 7;   // 10______ 10______
        const TOO_LARGE: u8 = 1 << 3;   // 11110100 1001____
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
        const OVERLONG_4: u8 = 1 << 6;  // 11110000 1000____

        const u3xchunk_len = @Vector(chunk_len, u3);
        const byte_1_high_0 = prev1 >> @as(u3xchunk_len, @splat(4));
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
        };

        const byte_1_high = c.lookup_16_aarch64(byte_1_high_0, tbl1); 
        const CARRY: u8 = TOO_SHORT | TOO_LONG | TWO_CONTS; // These all have ____ in byte 1 .
        const byte_1_low0 = prev1 & @as(Chunk, @splat(0x0F));
        
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
        };
        // const byte_1_low = c.mm256_shuffle_epi8(tbl2, byte_1_low0);
        const byte_1_low = c.lookup_16_aarch64(byte_1_low0, tbl2);

        const byte_2_high_0 = input >> @as(u3xchunk_len, @splat(4));
        const tbl3 = [16]u8{
            // ________ 0_______ <ASCII in byte 2>
            TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
            TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
            // ________ 1000____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4,
            // ________ 1001____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE,
            // ________ 101_____
            TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE, TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE | TOO_LARGE,
            // ________ 11______
            TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
        };
        const byte_2_high = c.lookup_16_aarch64(byte_2_high_0, tbl3);
        return (byte_1_high & byte_1_low & byte_2_high);
    }
    // zig fmt: on

    fn check_multibyte_lengths(input: Chunk, prev_input: Chunk, sc: Chunk) Chunk {
        const prev2 = prev(2, input, prev_input);
        const prev3 = prev(3, input, prev_input);
        const must23 = must_be_2_3_continuation(prev2, prev3);
        // cmn.println("\nprev2 {}\nprev3 {}\nmust23 {}", .{ prev2, prev3, must23 });
        const must23_80 = must23 & @as(Chunk, @splat(0x80));
        return must23_80 ^ sc;
    }

    fn must_be_2_3_continuation(prev2: Chunk, prev3: Chunk) Chunk {
        const is_third_byte = prev2 -| @as(Chunk, @splat(0b11100000 - 0x80)); // Only 111_____ will be >= 0x80
        const is_fourth_byte = prev3 -| @as(Chunk, @splat(0b11110000 - 0x80)); // Only 1111____ will be >= 0x80
        return is_third_byte | is_fourth_byte;
    }

    //
    // Check whether the current bytes are valid UTF-8.
    //
    fn check_utf8_bytes(checker: *Utf8Checker, input: Chunk, prev_input: Chunk) void {
        // Flip prev1...prev3 so we can easily determine if they are 2+, 3+ or 4+ lead bytes
        // (2, 3, 4-byte leads become large positive numbers instead of small negative numbers)
        const prev1 = prev(1, input, prev_input);
        const sc = check_special_cases(input, prev1);
        checker.err |= check_multibyte_lengths(input, prev_input, sc);
    }

    // The only problem that can happen at EOF is that a multibyte character is too short
    // or a byte value too large in the last bytes: check_special_cases only checks for bytes
    // too large in the first of two bytes.
    fn check_eof(checker: *Utf8Checker) void {
        // If the previous block had incomplete UTF-8 characters at the end, an ASCII block can't
        // possibly finish them.
        checker.err |= checker.prev_incomplete;
    }

    fn is_ascii(input: v.u8x64) bool {
        return 0 == @as(u64, @bitCast(input >= @as(v.u8x64, @splat(0x80))));
    }

    fn check_next_input(checker: *Utf8Checker, input: v.u8x64) void {
        // const NUM_CHUNKS = cmn.STEP_SIZE / 32;

        if (is_ascii(input)) {
            // cmn.println("is_ascii checker.prev_incomplete {}", .{checker.prev_incomplete});
            checker.err |= checker.prev_incomplete;
        } else {
            // cmn.println("!is_ascii", .{});
            // you might think that a for-loop would work, but under Visual Studio, it is not good enough.
            // static_assert((simd8x64<uint8_t>::NUM_CHUNKS == 2) || (simd8x64<uint8_t>::NUM_CHUNKS == 4),
            // "We support either two or four chunks per 64-byte block.");
            if (cmn.is_x86_64) {
                const NUM_CHUNKS = 2;
                const chunks = @as([NUM_CHUNKS][32]u8, @bitCast(input));
                checker.check_utf8_bytes(chunks[0], checker.prev_input_block);
                checker.check_utf8_bytes(chunks[1], chunks[0]);
                checker.prev_incomplete = is_incomplete(chunks[NUM_CHUNKS - 1]);
                checker.prev_input_block = chunks[NUM_CHUNKS - 1];
            } else if (cmn.is_arm64) {
                const NUM_CHUNKS = 4;
                const chunks = @as([NUM_CHUNKS][16]u8, @bitCast(input));
                checker.check_utf8_bytes(chunks[0], checker.prev_input_block);
                checker.check_utf8_bytes(chunks[1], chunks[0]);
                checker.check_utf8_bytes(chunks[2], chunks[1]);
                checker.check_utf8_bytes(chunks[3], chunks[2]);
                checker.prev_incomplete = is_incomplete(chunks[NUM_CHUNKS - 1]);
                checker.prev_input_block = chunks[NUM_CHUNKS - 1];
            } else unreachable;
        }
    }
    // do not forget to call check_eof!
    fn errors(checker: Utf8Checker) cmn.JsonError!void {
        const err = @reduce(.Or, checker.err);
        if (err != 0) return error.UTF8_ERROR;
    }

    //
    // Return nonzero if there are incomplete multibyte characters at the end of the block:
    // e.g. if there is a 4-byte character, but it's 3 bytes from the end.
    //
    fn is_incomplete(input: Chunk) Chunk {
        // If the previous input's last 3 bytes match this, they're too short (they ended at EOF):
        // ... 1111____ 111_____ 11______
        const max_value = comptime max_value: {
            var max_array: Chunk = @splat(255);
            max_array[chunk_len - 3] = 0b11110000 - 1;
            max_array[chunk_len - 2] = 0b11100000 - 1;
            max_array[chunk_len - 1] = 0b11000000 - 1;
            break :max_value max_array;
        };
        return input -| max_value;
    }
};

const StringBlock = struct {
    backslash: u64,
    escaped: u64,
    quote: u64,
    in_string: u64,

    fn string_tail(sb: StringBlock) u64 {
        return sb.in_string ^ sb.quote;
    }

    fn non_quote_inside_string(sb: StringBlock, mask: u64) u64 {
        return mask & sb.in_string;
    }
};

const CharacterBlock = struct {
    whitespace: u64,
    op: u64,
    pub fn classify(input_vec: v.u8x64) CharacterBlock {
        // These lookups rely on the fact that anything < 127 will match the lower 4 bits, which is why
        // we can't use the generic lookup_16.
        const whitespace_table: v.u8x32 = [16]u8{ ' ', 100, 100, 100, 17, 100, 113, 2, 100, '\t', '\n', 112, 100, '\r', 100, 100 } ** 2;

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
        const op_table: v.u8x32 = [16]u8{
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

        const in = @as([64]u8, @bitCast(input_vec));
        const chunk0: v.u8x32 = in[0..32].*;
        const chunk1: v.u8x32 = in[32..64].*;
        const wss: [2]v.u8x32 = .{
            c.mm256_shuffle_epi8(whitespace_table, chunk0),
            c.mm256_shuffle_epi8(whitespace_table, chunk1),
        };
        const ws = input_vec == @as(v.u8x64, @bitCast(wss));
        // Turn [ and ] into { and }
        const curlified = input_vec | @as(v.u8x64, @splat(0x20));
        const ops: [2]v.u8x32 = .{
            c.mm256_shuffle_epi8(op_table, chunk0),
            c.mm256_shuffle_epi8(op_table, chunk1),
        };
        // if (cmn.debug) {
        //     cmn.println("", .{});
        //     var s: [64]u8 = @bitCast([64]u8, ops);
        //     for (s) |*sc| {
        //         if (!std.ascii.isPrint(sc.*)) sc.* = '-';
        //     }
        //     cmn.println("{s} | CharacterBlock.classify() ops", .{s});
        // }
        const whitespace = @as(u64, @bitCast(ws));
        const op = @as(u64, @bitCast(curlified == @as(v.u8x64, @bitCast(ops))));

        // cmn.println("{b:0>64} | whitespace", .{@bitReverse(whitespace)});
        // cmn.println("{b:0>64} | op", .{@bitReverse(op)});

        return .{ .whitespace = whitespace, .op = op };
    }

    pub fn classify_arm64(input_vec: v.u8x64) CharacterBlock {
        // Functional programming causes trouble with Visual Studio.
        // Keeping this version in comments since it is much nicer:
        // auto v = in.map<uint8_t>([&](simd8<uint8_t> chunk) {
        //  auto nib_lo = chunk & 0xf;
        //  auto nib_hi = chunk.shr<4>();
        //  auto shuf_lo = nib_lo.lookup_16<uint8_t>(16, 0, 0, 0, 0, 0, 0, 0, 0, 8, 12, 1, 2, 9, 0, 0);
        //  auto shuf_hi = nib_hi.lookup_16<uint8_t>(8, 0, 18, 4, 0, 1, 0, 1, 0, 0, 0, 3, 2, 1, 0, 0);
        //  return shuf_lo & shuf_hi;
        // });
        const in = @as([64]u8, @bitCast(input_vec));
        const chunk0: v.u8x16 = in[0..16].*;
        const chunk1: v.u8x16 = in[16..32].*;
        const chunk2: v.u8x16 = in[32..48].*;
        const chunk3: v.u8x16 = in[48..].*;
        const lo = @as(v.u8x16, @splat(0xf));
        const fours = @as(@Vector(16, u3), @splat(4));

        const tables: [2]v.u8x16 = .{
            .{ 16, 0, 0, 0, 0, 0, 0, 0, 0, 8, 12, 1, 2, 9, 0, 0 },
            .{ 8, 0, 18, 4, 0, 1, 0, 1, 0, 0, 0, 3, 2, 1, 0, 0 },
        };
        const table1 = tables[0];
        const table2 = tables[1];

        const vv: v.u8x64 =
            @as([16]u8, c.lookup_16_aarch64(chunk0 & lo, table1) & c.lookup_16_aarch64(chunk0 >> fours, table2)) ++
            @as([16]u8, c.lookup_16_aarch64(chunk1 & lo, table1) & c.lookup_16_aarch64(chunk1 >> fours, table2)) ++
            @as([16]u8, c.lookup_16_aarch64(chunk2 & lo, table1) & c.lookup_16_aarch64(chunk2 >> fours, table2)) ++
            @as([16]u8, c.lookup_16_aarch64(chunk3 & lo, table1) & c.lookup_16_aarch64(chunk3 >> fours, table2));

        // cmn.println("vv {}", .{vv});
        // We compute whitespace and op separately. If the code later only use one or the
        // other, given the fact that all functions are aggressively inlined, we can
        // hope that useless computations will be omitted. This is namely case when
        // minifying (we only need whitespace). *However* if we only need spaces,
        // it is likely that we will still compute 'v' above with two lookup_16: one
        // could do it a bit cheaper. This is in contrast with the x64 implementations
        // where we can, efficiently, do the white space and structural matching
        // separately. One reason for this difference is that on ARM NEON, the table
        // lookups either zero or leave unchanged the characters exceeding 0xF whereas
        // on x64, the equivalent instruction (pshufb) automatically applies a mask,
        // ignoring the 4 most significant bits. Thus the x64 implementation is
        // optimized differently. This being said, if you use this code strictly
        // just for minification (or just to identify the structural characters),
        // there is a small untaken optimization opportunity here. We deliberately
        // do not pick it up.

        const vchunks = @as([64]u8, @bitCast(vv));
        const vchunk0: v.u8x16 = vchunks[0..16].*;
        const vchunk1: v.u8x16 = vchunks[16..32].*;
        const vchunk2: v.u8x16 = vchunks[32..48].*;
        const vchunk3: v.u8x16 = vchunks[48..].*;
        const zeros = @as(v.u8x16, @splat(0));
        const sevens = @as(v.u8x16, @splat(0x7));
        const ops: [4]u16 = .{
            @as(u16, @bitCast(c.any_bits_set_aarch64(vchunk0, sevens) != zeros)),
            @as(u16, @bitCast(c.any_bits_set_aarch64(vchunk1, sevens) != zeros)),
            @as(u16, @bitCast(c.any_bits_set_aarch64(vchunk2, sevens) != zeros)),
            @as(u16, @bitCast(c.any_bits_set_aarch64(vchunk3, sevens) != zeros)),
        };

        const ws = @as(v.u8x16, @splat(0x18));
        const wss: [4]u16 = .{
            @as(u16, @bitCast(c.any_bits_set_aarch64(vchunk0, ws) != zeros)),
            @as(u16, @bitCast(c.any_bits_set_aarch64(vchunk1, ws) != zeros)),
            @as(u16, @bitCast(c.any_bits_set_aarch64(vchunk2, ws) != zeros)),
            @as(u16, @bitCast(c.any_bits_set_aarch64(vchunk3, ws) != zeros)),
        };
        const whitespace = @as(u64, @bitCast(wss));
        const op = @as(u64, @bitCast(ops));
        // cmn.println("{b:0>64} | whitespace", .{@bitReverse(whitespace)});
        // cmn.println("{b:0>64} | op", .{@bitReverse(op)});

        return .{ .whitespace = whitespace, .op = op };
    }

    pub fn scalar(cb: CharacterBlock) u64 {
        return ~(cb.op | cb.whitespace);
    }
};

const Block = struct {
    string: StringBlock,
    characters: CharacterBlock,
    follows_nonquote_scalar: u64,

    fn structural_start(block: Block) u64 {
        return block.potential_structural_start() & ~block.string.string_tail();
    }
    fn potential_structural_start(block: Block) u64 {
        return block.characters.op | block.potential_scalar_start();
    }
    fn potential_scalar_start(block: Block) u64 {
        // The term "scalar" refers to anything except structural characters and white space
        // (so letters, numbers, quotes).
        // Whenever it is preceded by something that is not a structural element ({,},[,],:, ") nor a white-space
        // then we know that it is irrelevant structurally.
        return block.characters.scalar() & ~block.follows_nonquote_scalar;
    }
    fn non_quote_inside_string(block: Block, mask: u64) u64 {
        return block.string.non_quote_inside_string(mask);
    }
};

pub const StructuralIndexer = struct {
    prev_structurals: u64 = 0,
    unescaped_chars_error: u64 = 0,
    bit_indexer: BitIndexer = .{},
    checker: Utf8Checker = .{},

    fn follows(match: u64, overflow: *u64) u64 {
        const result = match << 1 | overflow.*;
        overflow.* = match >> 63;
        return result;
    }

    fn nextBlock(parser: *Parser, input_vec: v.u8x64) Block {
        const string = parser.nextStringBlock(input_vec);
        // identifies the white-space and the structurat characters
        // missing cpu feature set: x86_64+sse2. please provide -mcpu=x86_64+sse2

        const characters = if (cmn.is_x86_64 and cmn.has_avx)
            CharacterBlock.classify(input_vec)
        else if (cmn.is_arm64)
            CharacterBlock.classify_arm64(input_vec)
        else
            @compileError(std.fmt.comptimePrint(
                "TODO provide fallback CharacterBlock.classify() for arch {s} with has_avx={}",
                .{ @tagName(builtin.cpu.arch), cmn.has_avx },
            ));

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

    pub fn step(si: *StructuralIndexer, read_buf: [cmn.STEP_SIZE]u8, parser: *Parser, reader_pos: u64) !void {
        if (cmn.STEP_SIZE == 64) {
            const block_1 = nextBlock(parser, read_buf);
            try si.next(read_buf, block_1, reader_pos);
            // std.log.debug("stream pos {}", .{try stream.getPos()});
        } else {
            const block_1 = nextBlock(parser, read_buf[0..64].*);
            const block_2 = nextBlock(parser, read_buf[64..128].*);
            try si.next(read_buf[0..64].*, block_1, reader_pos);
            try si.next(read_buf[64..128].*, block_2, reader_pos + 64);
        }
        try si.checker.errors();
    }

    pub fn finish(si: *StructuralIndexer, parser: *Parser, idx: usize, len: usize, partial: bool) !void {
        _ = partial;
        si.bit_indexer.write(idx, si.prev_structurals);

        // TODO partial:
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
        parser.n_structural_indexes = std.math.cast(u32, si.bit_indexer.tail.items.len) orelse return error.Overflow;
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
        var new_inds = parser.indexer.bit_indexer.tail.addManyAsArrayAssumeCapacity(3);
        new_inds[0] = @as(u32, @intCast(len));
        new_inds[1] = @as(u32, @intCast(len));
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

    fn lteq(comptime T: type, chunks: [2]v.u8x32, m: T) u64 {
        const mask: @Vector(32, T) = @splat(m);
        const a = chunks[0] <= mask;
        const b = chunks[1] <= mask;
        const aint = @as(u64, @as(*const u32, @ptrCast(&a)).*);
        const bint = @as(u64, @as(*const u32, @ptrCast(&b)).*) << 32;
        return aint | bint;
    }

    fn logStructurals(si: StructuralIndexer, input_vec: v.u8x64, block: Block, reader_pos: u64) void {
        _ = block;
        cmn.println("{b:0>64} | prev_structurals, reader_pos {}", .{ @bitReverse(si.prev_structurals), reader_pos });
        if (cmn.debug) {
            var input: [cmn.STEP_SIZE]u8 = undefined;
            @memcpy(input[0..64], &@as([64]u8, input_vec));
            for (&input) |*ch| {
                if (ch.* == '\n') ch.* = '-';
            }
            cmn.println("{s}", .{input});
        }
    }

    fn next(si: *StructuralIndexer, input_vec: v.u8x64, block: Block, reader_pos: u64) !void {
        si.logStructurals(input_vec, block, reader_pos);
        const chunks = @as([2]v.u8x32, @bitCast(input_vec));
        const unescaped = lteq(u8, chunks, 0x1F);
        si.checker.check_next_input(input_vec);
        si.bit_indexer.write(reader_pos, si.prev_structurals); // Output *last* iteration's structurals to the parser
        si.prev_structurals = block.structural_start();
        si.unescaped_chars_error |= block.non_quote_inside_string(unescaped);
    }
};

pub const OpenContainer = struct {
    tape_index: u32, // where, on the tape, does the scope ([,{) begin
    count: u32, // how many elements in the scope
};

pub const OpenContainerInfo = struct {
    open_container: OpenContainer,
    is_array: bool,
};

pub const Iterator = struct {
    parser: *Parser,
    _next_structural: [*]u32,
    depth: u32 = 0,
    log: Logger = .{ .depth = 0 },

    pub fn init(parser: *Parser, start_structural_index: usize) Iterator {
        return .{
            .parser = parser,
            ._next_structural = parser.indexer.bit_indexer.tail.items[start_structural_index..].ptr,
        };
    }

    fn advance(iter: *Iterator) [*]const u8 {
        defer iter._next_structural += 1;
        // std.log.debug("advance() next_structural idx {} peek() '{c}'", .{ (@ptrToInt(iter.next_structural) - @ptrToInt(iter.parser.indexer.bit_indexer.tail.items.ptr)) / 4, iter.peek() });
        return iter.peek();
    }

    fn peek(iter: *Iterator) [*]const u8 {
        return iter.parser.bytes.items.ptr + iter._next_structural[0];
    }

    pub fn at_beginning(iter: *Iterator) bool {
        // std.log.debug("at-beginning {*}: {}", .{ iter.next_structural, iter.next_structural[0] });
        return iter.next_structural() == iter.parser.indexer.bit_indexer.tail.items.ptr;
    }

    pub fn next_structural(iter: Iterator) [*]u32 {
        return iter._next_structural;
    }

    fn at_eof(iter: *Iterator) bool {
        // std.log.debug("at-beginning {*}: {}", .{ iter.next_structural, iter.next_structural[0] });
        return @intFromPtr(iter._next_structural) == @intFromPtr(iter.parser.indexer.bit_indexer.tail.items.ptr + iter.parser.n_structural_indexes);
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
        iter.log.start(iter);
        if (iter.at_eof()) return error.EMPTY;
        iter.log.start_value(iter, "document");
        try visitor.visit_document_start(iter);
        if (iter.parser.bytes.items.len == 0) return iter.document_end(visitor);

        const value = iter.advance();
        var state: State = blk: {
            switch (value[0]) {
                '{' => {
                    if (iter.peek()[0] == '}') {
                        _ = iter.advance();
                        iter.log.value(iter, "empty object");
                        try visitor.visit_empty_object();
                    } else break :blk .object_begin;
                },
                '[' => {
                    if (iter.peek()[0] == ']') {
                        _ = iter.advance();
                        iter.log.value(iter, "empty array");
                        try visitor.visit_empty_array();
                    } else break :blk .array_begin;
                },
                else => try visitor.visit_root_primitive(iter, value),
            }
            break :blk .document_end;
        };

        while (true) {
            // iter.log.event(iter, "state ", @tagName(state), 0, 0);
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

    fn object_begin(iter: *Iterator, visitor: *TapeBuilder) cmn.Error!State {
        iter.log.start_value(iter, "object");
        iter.depth += 1;
        // iter.log.line_fmt(iter, "", "depth", "{d}/{d}", .{ iter.depth, iter.parser.max_depth });

        if (iter.depth >= iter.parser.max_depth) {
            iter.log.err(iter, "Exceeded max depth!");
            return error.DEPTH_ERROR;
        }
        try visitor.visit_object_start(iter);

        const key = iter.advance();
        // iter.log.line(iter, "", "", key[0..10]);
        if (key[0] != '"') {
            iter.log.err(iter, "Object does not start with a key");
            return error.TAPE_ERROR;
        }
        iter.increment_count();
        try visitor.visit_key(iter, key);
        return .object_field;
    }

    fn object_field(iter: *Iterator, visitor: *TapeBuilder) cmn.Error!State {
        if (iter.advance()[0] != ':') {
            iter.log.err(iter, "Missing colon after key in object");
            return error.TAPE_ERROR;
        }

        const value = iter.advance();
        switch (value[0]) {
            '{' => if (iter.peek()[0] == '}') {
                _ = iter.advance();
                iter.log.value(iter, "empty object");
                try visitor.visit_empty_object();
            } else return .object_begin,
            '[' => if (iter.peek()[0] == ']') {
                _ = iter.advance();
                iter.log.value(iter, "empty array");
                try visitor.visit_empty_array();
            } else return .array_begin,
            else => try visitor.visit_primitive(iter, value),
        }
        return .object_continue;
    }

    fn object_continue(iter: *Iterator, visitor: *TapeBuilder) cmn.Error!State {
        const value = iter.advance();
        // std.log.debug("object_continue() value '{c}'", .{value});
        switch (value[0]) {
            ',' => {
                iter.increment_count();
                const key = iter.advance();
                // cmn.println("key '{c}'", .{key});
                if (key[0] != '"') {
                    iter.log.err(iter, "Key string missing at beginning of field in object");
                    return error.TAPE_ERROR;
                }
                try visitor.visit_key(iter, key);
                return .object_field;
            },
            '}' => {
                iter.log.end_value(iter, "object");
                _ = try visitor.visit_object_end(iter);
                return .scope_end;
            },
            else => {
                iter.log.err(iter, "No comma between object fields");
                return error.TAPE_ERROR;
            },
        }
        unreachable;
    }

    fn scope_end(iter: *Iterator, _: *TapeBuilder) cmn.Error!State {
        // std.log.debug("scope_end iter.depth {}", .{iter.depth});
        iter.depth -= 1;
        if (iter.depth == 0) return .document_end;
        const is_array = iter.parser.open_containers.items(.is_array)[iter.depth];
        if (is_array)
            return .array_continue;

        return .object_continue;
    }

    fn array_begin(iter: *Iterator, visitor: *TapeBuilder) cmn.Error!State {
        iter.log.start_value(iter, "array");
        iter.depth += 1;
        if (iter.depth >= iter.parser.max_depth) {
            iter.log.err(iter, "Exceeded max depth!");
            return error.DEPTH_ERROR;
        }
        _ = try visitor.visit_array_start(iter);
        iter.increment_count();
        return .array_value;
    }

    fn array_value(iter: *Iterator, visitor: *TapeBuilder) cmn.Error!State {
        const value = iter.advance();
        switch (value[0]) {
            '{' => {
                if (iter.peek()[0] == '}') {
                    _ = iter.advance();
                    iter.log.value(iter, "empty object");
                    try visitor.visit_empty_object();
                } else return .object_begin;
            },
            '[' => {
                if (iter.peek()[0] == ']') {
                    _ = iter.advance();
                    iter.log.value(iter, "empty array");
                    try visitor.visit_empty_array();
                } else return .array_begin;
            },
            else => try visitor.visit_primitive(iter, value),
        }
        return .array_continue;
    }

    fn array_continue(iter: *Iterator, visitor: *TapeBuilder) cmn.Error!State {
        switch (iter.advance()[0]) {
            ',' => {
                iter.increment_count();
                return .array_value;
            },
            ']' => {
                iter.log.end_value(iter, "array");
                try visitor.visit_array_end(iter);
                return .scope_end;
            },
            else => {
                iter.log.err(iter, "Missing comma between array values");
                return error.TAPE_ERROR;
            },
        }
        unreachable;
    }

    fn document_end(iter: *Iterator, visitor: *TapeBuilder) cmn.Error!void {
        iter.log.end_value(iter, "document");
        try visitor.visit_document_end();
        iter.parser.next_structural_index = try cmn.ptr_diff(
            u32,
            iter._next_structural,
            iter.parser.indexer.bit_indexer.tail.items.ptr,
        );

        // If we didn't make it to the end, it's an error
        // std.log.debug("next_structural_index {} n_structural_indexes {}", .{ iter.parser.next_structural_index, iter.parser.n_structural_indexes });
        // have to add because there are 3 additional items added to tail in finish()
        if (!cmn.STREAMING and iter.parser.next_structural_index != iter.parser.n_structural_indexes) {
            iter.log.err(iter, "More than one JSON value at the root of the document, or extra characters at the end of the JSON!");
            return error.TAPE_ERROR;
        }
    }

    fn current_container(iter: *Iterator) *OpenContainer {
        // std.log.debug("current_container iter.parser.open_containers.len {} iter.depth {}", .{ iter.parser.open_containers.items.len, iter.depth });
        return &iter.parser.open_containers.items(.open_container)[iter.depth];
    }
    fn increment_count(iter: *Iterator) void {
        // we have a key value pair in the object at parser.dom_parser.depth - 1
        iter.current_container().count += 1;
    }

    fn root_checkpoint(iter: *Iterator) [*]u32 {
        return iter.parser.indexer.bit_indexer.tail.items.ptr;
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
    pub fn as_u64(tt: TapeType) u64 {
        return @as(u64, @intFromEnum(tt)) << 56;
    }
    pub fn from_u64(x: u64) TapeType {
        return std.meta.intToEnum(TapeType, (x & 0xff00000000000000) >> 56) catch .INVALID;
    }
    pub fn encode_value(tt: TapeType, value: u64) u64 {
        assert(value <= std.math.maxInt(u56));
        return @as(u64, @intFromEnum(tt)) << 56 | value;
    }
    pub fn extract_value(item: u64) u64 {
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
            .current_string_buf_loc = doc.string_buf.items.ptr,
        };
    }

    pub fn append(tb: *TapeBuilder, val: u64, tt: TapeType) void {
        // iter.log.line_fmt(iter, "", "append", "val {} tt {}", .{ val, tt });
        tb.tape.appendAssumeCapacity(val | tt.as_u64());
    }

    pub fn append2(tb: *TapeBuilder, val: u64, val2: anytype, tt: TapeType) void {
        tb.append(val, tt);
        assert(@sizeOf(@TypeOf(val2)) == 8);
        tb.tape.appendAssumeCapacity(val2);
    }

    pub fn append_double(tb: *TapeBuilder, val: f64) void {
        tb.append2(0, @as(u64, @bitCast(val)), .DOUBLE);
    }

    pub fn append_i64(tb: *TapeBuilder, val: u64) void {
        tb.append2(0, val, .INT64);
    }

    pub fn append_u64(tb: *TapeBuilder, val: u64) void {
        tb.append2(0, val, .UINT64);
    }

    pub fn write(tb: *TapeBuilder, idx: usize, val: u64, tt: TapeType) void {
        // iter.log.line_fmt(iter, "", "write", "val {} tt {} idx {}", .{ val, tt, idx });
        assert(idx < tb.tape.items.len);
        tb.tape.items[idx] = val | tt.as_u64();
    }

    pub fn next_tape_index(tb: TapeBuilder) u32 {
        return @as(u32, @intCast(tb.tape.items.len));
    }

    pub fn skip(tb: *TapeBuilder) void {
        _ = tb.tape.addOneAssumeCapacity();
    }

    pub fn empty_container(tb: *TapeBuilder, start: TapeType, end: TapeType) void {
        const start_index = tb.next_tape_index();
        tb.append(start_index + 2, start);
        tb.append(start_index, end);
    }

    pub fn start_container(
        tb: *TapeBuilder,
        open_containers: *std.MultiArrayList(OpenContainerInfo),
        is_array: bool,
        count: u32,
    ) void {
        const tape_idx = tb.next_tape_index();
        open_containers.appendAssumeCapacity(.{
            .is_array = is_array,
            .open_container = .{
                .tape_index = @as(u32, @intCast(tape_idx)),
                .count = count,
            },
        });
        tb.skip();
    }

    pub fn end_container(tb: *TapeBuilder, iter: *Iterator, start: TapeType, end: TapeType) void {
        // Write the ending tape element, pointing at the start location
        const container = iter.parser.open_containers.items(.open_container)[iter.depth];
        defer iter.parser.open_containers.shrinkRetainingCapacity(iter.depth);
        const start_tape_index = container.tape_index;
        tb.append(start_tape_index, end);
        // Write the start tape element, pointing at the end location (and including count)
        // count can overflow if it exceeds 24 bits... so we saturate
        // the convention being that a cnt of 0xffffff or more is undetermined in value (>=  0xffffff).
        const cntsat: u32 = @min(@as(u32, @intCast(container.count)), 0xFFFFFF);

        // iter.log.line_fmt(iter, "", "end_container", "next_tape_index {}", .{tb.next_tape_index()});
        tb.write(start_tape_index, tb.next_tape_index() | (@as(u64, cntsat) << 32), start);
    }

    fn on_start_string(tb: *TapeBuilder, iter: *Iterator) ![*]u8 {
        // iter.log.line_fmt(iter, "", "start_string", "iter.parser.doc.string_buf.len {}", .{iter.parser.doc.string_buf.len});
        tb.append(cmn.ptr_diff(u64, tb.current_string_buf_loc, iter.parser.doc.string_buf.items.ptr) catch unreachable, .STRING);
        return tb.current_string_buf_loc + @sizeOf(u32);
    }
    fn on_end_string(tb: *TapeBuilder, iter: *Iterator, dst: [*]u8) !void {
        const str_len = try cmn.ptr_diff(u32, dst, tb.current_string_buf_loc + @sizeOf(u32));
        // cmn.println("str_len {} str '{s}'", .{ str_len, (tb.current_string_buf_loc + 4)[0..str_len] });

        // TODO check for overflow in case someone has a crazy string (>=4GB?)
        // But only add the overflow check when the document itself exceeds 4GB
        // Currently unneeded because we refuse to parse docs larger or equal to 4GB.

        // NULL termination is still handy if you expect all your strings to
        // be NULL terminated? It comes at a small cost

        // iter.log.line_fmt(iter, "", "on_string_end", "{s}", .{str_start[0..str_len]});
        @memcpy(tb.current_string_buf_loc[0..@sizeOf(u32)], mem.asBytes(&str_len)[0..@sizeOf(u32)]);
        dst[0] = 0;
        iter.parser.doc.string_buf.items.len += str_len + 1 + @sizeOf(u32);
        // cmn.println("buf.len {} buf.cap {}", .{ iter.parser.doc.string_buf.len, iter.parser.doc.string_buf_cap });
        tb.current_string_buf_loc += str_len + 1 + @sizeOf(u32);
    }

    fn visit_root_primitive(visitor: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return switch (value[0]) {
            '"' => visitor.visit_string(iter, value, false),
            't' => visitor.visit_true_atom(iter, value),
            'f' => visitor.visit_false_atom(iter, value),
            'n' => visitor.visit_null_atom(iter, value),
            '-', '0'...'9' => visitor.visit_number(iter, value),
            else => blk: {
                iter.log.err(iter, "Document starts with a non-value character");
                break :blk error.TAPE_ERROR;
            },
        };
    }

    fn visit_number(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) cmn.Error!void {
        iter.log.value(iter, "number");
        try number_parsing.parse_number(value, tb);
    }
    fn visit_true_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) cmn.Error!void {
        iter.log.value(iter, "true");
        assert(value[0] == 't');
        if (!atom_parsing.is_valid_rue_atom(value + 1)) return error.T_ATOM_ERROR;
        tb.append(0, TapeType.TRUE);
    }
    fn visit_false_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) cmn.Error!void {
        iter.log.value(iter, "false");
        assert(value[0] == 'f');
        if (!atom_parsing.is_valid_alse_atom(value + 1)) return error.T_ATOM_ERROR;
        tb.append(0, TapeType.FALSE);
    }
    fn visit_null_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) cmn.Error!void {
        iter.log.value(iter, "null");
        assert(value[0] == 'n');
        if (!atom_parsing.is_valid_ull_atom(value + 1)) return error.T_ATOM_ERROR;
        tb.append(0, TapeType.NULL);
    }

    fn visit_primitive(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return switch (value[0]) {
            '"' => tb.visit_string(iter, value, false),
            't' => tb.visit_true_atom(iter, value),
            'f' => tb.visit_false_atom(iter, value),
            'n' => tb.visit_null_atom(iter, value),
            '-', '0'...'9' => tb.visit_number(iter, value),
            else => |ch| blk: {
                iter.log.err_fmt(iter, "Non-value found when value was expected.  Value: '{c}' - (0x{x}:{})", .{ ch, ch, ch });
                break :blk error.TAPE_ERROR;
            },
        };
    }

    fn visit_string(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8, key: bool) cmn.Error!void {
        iter.log.value(iter, if (key) "key" else "string");
        var dst = try tb.on_start_string(iter);
        dst = string_parsing.parse_string(value + 1, dst) orelse {
            iter.log.err(iter, "Invalid escape in string");
            return error.STRING_ERROR;
        };

        try tb.on_end_string(iter, dst);
    }

    pub fn visit_key(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return tb.visit_string(iter, value, true);
    }

    pub fn visit_empty_object(tb: *TapeBuilder) cmn.Error!void {
        return tb.empty_container(.START_OBJECT, .END_OBJECT);
    }

    pub fn visit_empty_array(tb: *TapeBuilder) cmn.Error!void {
        return tb.empty_container(.START_ARRAY, .END_ARRAY);
    }

    pub fn visit_array_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, true, 0);
    }

    pub fn visit_array_end(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.end_container(iter, .START_ARRAY, .END_ARRAY);
    }

    pub fn visit_object_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, false, 0);
    }
    pub fn visit_object_end(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.end_container(iter, .START_OBJECT, .END_OBJECT);
    }
    pub fn visit_document_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, false, 0);
    }
    pub fn visit_document_end(tb: *TapeBuilder) !void {
        tb.write(0, tb.next_tape_index(), .ROOT);
        // iter.log.line_fmt(iter, "?", "document_end", "open_containers.len {} tape.len {}", .{ iter.parser.open_containers.items.len, tb.tape.items.len });
        return tb.append(0, .ROOT);
    }
};

pub const Parser = struct {
    filename: []const u8,
    allocator: mem.Allocator,
    prev_escaped: u64 = 0,
    prev_in_string: u64 = 0,
    prev_scalar: u64 = 0,
    next_structural_index: u32 = 0,
    doc: Document,
    indexer: StructuralIndexer,
    open_containers: std.MultiArrayList(OpenContainerInfo),
    max_depth: u16,
    n_structural_indexes: u32 = 0,
    bytes: std.ArrayListAlignedUnmanaged(u8, cmn.chunk_align) = .{},
    input_len: u32 = 0,

    pub const Options = struct {
        max_depth: u16 = cmn.DEFAULT_MAX_DEPTH,
    };

    pub fn initFile(allocator: mem.Allocator, filename: []const u8, options: Options) !Parser {
        var parser = Parser{
            .filename = filename,
            .allocator = allocator,
            .doc = .{},
            .indexer = .{},
            .open_containers = .{},
            .max_depth = options.max_depth,
        };
        parser.input_len = try parser.read_file(filename);

        try parser.finishInit(options);
        return parser;
    }

    const ascii_space = 0x20;

    pub fn initFixedBuffer(allocator: mem.Allocator, input: []const u8, options: Options) !Parser {
        var parser = Parser{
            .filename = "<fixed buffer>",
            .allocator = allocator,
            .doc = .{},
            .indexer = .{},
            .bytes = .{},
            .open_containers = .{},
            .max_depth = options.max_depth,
        };
        try parser.initExisting(input, options);
        return parser;
    }

    pub fn initFromReader(allocator: mem.Allocator, reader: *std.io.Reader, options: Options) !Parser {
        // TODO bench with Reader.expandTotalCapacity or Reader.fillMore
        var parser = Parser{
            .filename = "<reader>",
            .allocator = allocator,
            .doc = .{},
            .indexer = .{},
            .bytes = .{},
            .open_containers = .{},
            .max_depth = options.max_depth,
        };
        errdefer parser.deinit();
        try parser.initExistingFromReader(reader, options);
        return parser;
    }

    /// re-initialize an existing parser which was previously initialized
    pub fn initExisting(parser: *Parser, input: []const u8, options: Options) !void {
        parser.clearRetainingCapacity();
        parser.input_len = std.math.cast(u32, input.len) orelse return error.Overflow;
        const paddedlen = try std.math.add(u32, parser.input_len, cmn.SIMDJSON_PADDING);
        try parser.bytes.ensureTotalCapacity(parser.allocator, paddedlen);
        parser.bytes.items.len = paddedlen;
        @memcpy(parser.bytes.items[0..input.len], input);
        try parser.finishInit(options);
    }

    // 10/11/2023 TS - i would like to remove this in favor of
    // initExistingFromReader() but i'm leaving it as this method is slightly
    // more performant.  this is because it doesn't need to realloc for the
    // padding bytes as the total length is known ahead of time.
    fn read_file(parser: *Parser, filename: []const u8) !u32 {
        var f = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
        defer f.close();
        const len = std.math.cast(u32, try f.getEndPos()) orelse return error.Overflow;
        if (parser.bytes.items.len < len) {
            const paddedlen = try std.math.add(u32, len, cmn.SIMDJSON_PADDING);
            try parser.bytes.ensureTotalCapacity(parser.allocator, paddedlen);
            parser.bytes.items.len = paddedlen;
            const nbytes = try f.read(parser.bytes.items);
            if (nbytes < len) return error.IO_ERROR;
        }
        return len;
    }

    /// re-initialize an existing parser which was previously initialized
    ///
    /// performance notes:
    /// read input from 'reader' into a pre-initialized parser's 'bytes' field.
    /// this is done in two steps.  first the input is read from 'reader' into
    /// 'bytes'.  then 32 bytes of padding is added after the input.  this is
    /// necessary to support all reader types, including non-seekable ones
    /// where the total length can't be calculated ahead of time.  thus, there
    /// may be a performance penalty when adding the padding causes the input to
    /// be moved.  this penalty may be avoided by pre-setting the capacity such
    /// as: 'parser.bytes.ensureTotalCapacity(N)' where N is >= input.len + 32.
    pub fn initExistingFromReader(parser: *Parser, reader: *std.io.Reader, options: Options) !void {
        parser.clearRetainingCapacity();
        var bytes_unaligned: std.ArrayList(u8) = .{ .items = parser.bytes.items, .capacity = parser.bytes.capacity };
        try reader.appendRemaining(parser.allocator, &bytes_unaligned, .unlimited);
        parser.bytes.deinit(parser.allocator);
        parser.bytes = .{ .items = @alignCast(bytes_unaligned.items), .capacity = bytes_unaligned.capacity };
        parser.input_len = std.math.cast(u32, parser.bytes.items.len) orelse
            return error.Overflow;

        const paddedlen = try std.math.add(u32, parser.input_len, cmn.SIMDJSON_PADDING);
        try parser.bytes.ensureTotalCapacity(parser.allocator, paddedlen);
        parser.bytes.items.len = paddedlen;
        try parser.finishInit(options);
    }

    fn finishInit(parser: *Parser, options: Options) !void {
        // We write spaces in the padded region to avoid having uninitized
        // garbage. If nothing else, garbage getting read might trigger a
        // warning in a memory checking.
        @memset(parser.bytes.items[parser.input_len..], ascii_space);
        try parser.doc.allocate(parser.allocator, parser.input_len);
        const max_structures = cmn.ROUNDUP_N(parser.input_len, 64) + 2 + 7;
        try parser.indexer.bit_indexer.tail.ensureTotalCapacity(parser.allocator, max_structures);
        try parser.open_containers.ensureTotalCapacity(parser.allocator, options.max_depth);
    }

    pub fn deinit(parser: *Parser) void {
        parser.indexer.bit_indexer.tail.deinit(parser.allocator);
        parser.open_containers.deinit(parser.allocator);
        parser.doc.deinit(parser.allocator);
        parser.bytes.deinit(parser.allocator);
    }

    // reset the parser so that it may be re-used.  called by initExisting() and
    // initExistingFromReader()
    fn clearRetainingCapacity(parser: *Parser) void {
        parser.* = .{
            .filename = parser.filename,
            .allocator = parser.allocator,
            .doc = parser.doc,
            .indexer = .{ .bit_indexer = .{ .tail = parser.indexer.bit_indexer.tail } },
            .open_containers = parser.open_containers,
            .bytes = parser.bytes,
            .max_depth = parser.max_depth,
        };
        parser.indexer.bit_indexer.tail.clearRetainingCapacity();
        parser.open_containers.shrinkRetainingCapacity(0);
        parser.doc.tape.clearRetainingCapacity();
        parser.doc.string_buf.clearRetainingCapacity();
        parser.bytes.clearRetainingCapacity();
    }

    fn find_escaped(parser: *Parser, backslash_: u64) u64 {
        // If there was overflow, pretend the first character isn't a backslash
        const backslash = backslash_ & ~parser.prev_escaped;
        const follows_escape = backslash << 1 | parser.prev_escaped;

        // Get sequences starting on even bits by clearing out the odd series using +
        const even_bits: u64 = 0x5555555555555555;
        const odd_sequence_starts = backslash & ~even_bits & ~follows_escape;
        // cmn.println("{b:0>64} | prev_escaped a", .{@bitReverse(parser.prev_escaped)});
        const x = @addWithOverflow(odd_sequence_starts, backslash);
        const sequences_starting_on_even_bits: u64 = x[0];
        parser.prev_escaped = x[1];
        // cmn.println("{b:0>64} | prev_escaped b", .{@bitReverse(parser.prev_escaped)});
        const invert_mask = sequences_starting_on_even_bits << 1; // The mask we want to return is the *escaped* bits, not escapes.

        // Mask every other backslashed character as an escaped character
        // Flip the mask for sequences that start on even bits, to correct them
        return (even_bits ^ invert_mask) & follows_escape;
    }

    fn nextStringBlock(parser: *Parser, input_vec: v.u8x64) StringBlock {
        const backslash_vec = input_vec == @as(v.u8x64, @splat('\\'));
        const backslash = @as(u64, @bitCast(backslash_vec));
        const escaped = parser.find_escaped(backslash);
        const quote_vec = input_vec == @as(v.u8x64, @splat('"'));
        const quote = @as(u64, @bitCast(quote_vec)) & ~escaped;

        //
        // prefix_xor flips on bits inside the string (and flips off the end quote).
        //
        // Then we xor with prev_in_string: if we were in a string already, its effect is flipped
        // (characters inside strings are outside, and characters outside strings are inside).
        //
        const x = c.prefix_xor(quote);
        const in_string = x ^ parser.prev_in_string;
        // cmn.println("{b:0>64} | quote a", .{@bitReverse(quote)});
        // cmn.println("{b:0>64} | ones[0]", .{@bitReverse(ones[0])});
        // cmn.println("{b:0>64} | in_string a", .{@bitReverse(in_string)});
        // cmn.println("{b:0>64} | prev_in_string a", .{@bitReverse(parser.prev_in_string)});
        // cmn.println("{b:0>64} | x", .{@bitReverse(x)});
        // cmn.println("{b:0>64} | in_string b", .{@bitReverse(in_string)});

        //
        // Check if we're still in a string at the end of the box so the next block will know
        //
        // right shift of a signed value expected to be well-defined and standard
        // compliant as of C++20, John Regher from Utah U. says this is fine code
        //
        // cmn.println("{b:0>64} | prev_in_string a", .{@bitReverse(parser.prev_in_string)});
        // cmn.println("{b:0>64} | @bitCast(i64, in_string) ", .{@bitReverse(i64, @bitCast(i64, in_string))});
        // cmn.println("{b:0>64} | @bitCast(i64, in_string) >> 63 ", .{@bitReverse(i64, @bitCast(i64, in_string) >> 63)});
        // cmn.println("{b:0>64} | @bitCast(u64, @bitCast(i64, in_string) >> 63) ", .{@bitReverse(@bitCast(u64, @bitCast(i64, in_string) >> 63))});
        parser.prev_in_string = @as(u64, @bitCast(@as(i64, @bitCast(in_string)) >> 63));

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
        const end_pos_minus_step = if (end_pos > cmn.STEP_SIZE) end_pos - cmn.STEP_SIZE else 0;

        var pos: u32 = 0;
        while (pos < end_pos_minus_step) : (pos += cmn.STEP_SIZE) {
            // cmn.println("pos {}", .{pos});
            const read_buf = parser.bytes.items[pos..][0..cmn.STEP_SIZE];
            try parser.indexer.step(read_buf.*, parser, pos);
            // for (blocks) |block| {
            //     cmn.println("{b:0>64} | characters.whitespace", .{@bitReverse(block.characters.whitespace)});
            //     cmn.println("{b:0>64} | characters.op", .{@bitReverse(block.characters.op)});
            //     cmn.println("{b:0>64} | in_string", .{@bitReverse(block.strings.in_string)});
            // }
        }
        var read_buf = [1]u8{0x20} ** cmn.STEP_SIZE;
        @memcpy(read_buf[0 .. end_pos - pos], parser.bytes.items[pos..end_pos]);
        // std.log.debug("read_buf {d}", .{read_buf});
        try parser.indexer.step(read_buf, parser, pos);
        try parser.indexer.finish(parser, pos + cmn.STEP_SIZE, end_pos, cmn.STREAMING);
    }

    fn stage2(parser: *Parser) !void {
        var iter = Iterator.init(parser, 0);
        var tb = TapeBuilder.init(&parser.doc);
        try iter.walk_document(&tb);
    }

    pub fn parse(parser: *Parser) !void {
        try parser.stage1();
        return parser.stage2();
    }

    pub inline fn element(parser: *const Parser) Element {
        return .{ .tape = .{ .doc = &parser.doc, .idx = 1 } };
    }
};

pub const Array = struct {
    tape: TapeRef,
    pub fn at(a: Array, idx: usize) ?Element {
        var i: usize = 0;
        const end = a.tape.after_element() - 1;
        var arr = Array{ .tape = .{ .idx = a.tape.idx + 1, .doc = a.tape.doc } };
        while (arr.tape.idx < end) {
            if (i == idx) return .{ .tape = arr.tape };
            arr.tape.idx = arr.tape.after_element();
            i += 1;
        }
        return null;
    }

    pub fn at_pointer(arr: Array, _json_pointer: []const u8) cmn.Error!Element {
        if (_json_pointer.len == 0)
            return Element{ .tape = arr.tape }
        else if (_json_pointer[0] != '/')
            return error.INVALID_JSON_POINTER;
        var json_pointer = _json_pointer[1..];
        // - means "the append position" or "the element after the end of the array"
        // We don't support this, because we're returning a real element, not a position.
        if (json_pointer.len == 1 and json_pointer[0] == '-')
            return error.INDEX_OUT_OF_BOUNDS;

        // Read the array index
        var array_index: usize = 0;
        var i: usize = 0;
        while (i < json_pointer.len and json_pointer[i] != '/') : (i += 1) {
            const digit = json_pointer[i] -% '0';
            // Check for non-digit in array index. If it's there, we're trying to get a field in an object
            if (digit > 9) return error.INCORRECT_TYPE;

            array_index = array_index * 10 + digit;
        }
        // 0 followed by other digits is invalid
        if (i > 1 and json_pointer[0] == '0') {
            return error.INVALID_JSON_POINTER;
        } // "JSON pointer array index has other characters after 0"

        // Empty string is invalid; so is a "/" with no digits before it
        if (i == 0)
            return error.INVALID_JSON_POINTER;
        // "Empty string in JSON pointer array index"

        // Get the child
        var child = arr.at(array_index) orelse return error.INVALID_JSON_POINTER;
        // If there is a /, we're not done yet, call recursively.
        if (i < json_pointer.len) {
            child = try child.at_pointer(json_pointer[i..]);
        }
        return child;
    }
};
const Object = struct {
    tape: TapeRef,
    pub fn at_key(o: Object, key: []const u8) ?Element {
        var it = TapeRefIterator.init(o);
        while (true) {
            if (it.tape.key_equals(key))
                return Element{ .tape = .{ .doc = it.tape.doc, .idx = it.tape.idx + 1 } };
            _ = it.next() orelse break;
        }
        return null;
    }

    pub fn at_pointer(o: Object, _json_pointer: []const u8) cmn.Error!Element {
        if (_json_pointer.len == 0) // an empty string means that we return the current node
            return Element{ .tape = o.tape } // copy the current node
        else if (_json_pointer[0] != '/') // otherwise there is an error
            return error.INVALID_JSON_POINTER;

        var json_pointer = _json_pointer[1..];
        const slash = mem.indexOfScalar(u8, json_pointer, '/');
        const key = json_pointer[0 .. slash orelse json_pointer.len];
        // Find the child with the given key
        var child: Element = undefined;

        // TODO escapes
        // // If there is an escape character in the key, unescape it and then get the child.
        // var escape = mem.indexOfScalar(u8, key, '~');

        // if (escape != null) {
        //     // Unescape the key
        //     var unescaped: [0x100]u8 = undefined;
        //     @memcpy(&unescaped, key);
        //     while (true) {
        //         switch (unescaped[escape.? + 1]) {
        //             '0' => unescaped.replace(escape, 2, "~"),
        //             '1' => unescaped.replace(escape, 2, "/"),
        //             else => return error.INVALID_JSON_POINTER, // "Unexpected ~ escape character in JSON pointer");
        //         }
        //         //   escape = unescaped.find('~', escape+1);
        //         escape = mem.indexOfScalar(u8, unescaped[escape.? + 1], '~');
        //         if (escape != null) break;
        //     }
        //     child = o.at_key(unescaped) catch return child;
        // } else {
        //     child = o.at_key(key) catch return child; // we do not continue if there was an error
        // }
        child = o.at_key(key) orelse return error.INVALID_JSON_POINTER;

        // If there is a /, we have to recurse and look up more of the path
        if (slash != null)
            child = try child.at_pointer(json_pointer[slash.?..]);

        return child;
    }

    pub const KeyIterator = struct {
        tape_it: TapeRefIterator,

        pub fn next(it: *KeyIterator) ?[]const u8 {
            const s = it.tape_it.element().get_string() catch return null;
            _ = it.tape_it.next();
            return s;
        }
    };
    pub fn key_iterator(o: Object) KeyIterator {
        return .{ .tape_it = TapeRefIterator.init(o) };
    }
};

// TODO rename these
const ElementType = enum(u8) {
    /// Array
    ARRAY = '[',
    /// Object
    OBJECT = '{',
    /// i64
    INT64 = 'l',
    /// u64: any integer that fits in u64 but *not* i64
    UINT64 = 'u',
    /// double: Any number with a "." or "e" that fits in double.
    DOUBLE = 'd',
    /// []const u8
    STRING = '"',
    /// bool
    BOOL = 't',
    /// null
    NULL = 'n',
};

const Value = union(ElementType) {
    ARRAY: Array,
    OBJECT: Object,
    INT64: i64,
    UINT64: u64,
    DOUBLE: f64,
    STRING: []u8,
    BOOL: bool,
    NULL,
};
const TapeRef = struct {
    doc: *const Document,
    idx: usize,

    pub fn is(tr: TapeRef, tt: TapeType) bool {
        return tr.tape_ref_type() == tt;
    }
    pub fn tape_ref_type(tr: TapeRef) TapeType {
        return TapeType.from_u64(tr.current());
    }
    pub fn value(tr: TapeRef) u64 {
        return TapeType.extract_value(tr.current());
    }
    pub fn next_value(tr: TapeRef) u64 {
        return TapeType.extract_value(tr.doc.tape.items[tr.idx + 1]);
    }
    pub fn after_element(tr: TapeRef) u64 {
        return switch (tr.tape_ref_type()) {
            .START_ARRAY, .START_OBJECT => tr.matching_brace_idx(),
            .UINT64, .INT64, .DOUBLE => tr.idx + 2,
            else => tr.idx + 1,
        };
    }
    pub fn matching_brace_idx(tr: TapeRef) u32 {
        const result = @as(u32, @truncate(tr.current()));
        // std.log.debug("TapeRef matching_brace_idx() for {} {}", .{ tr.tape_ref_type(), result });
        return result;
    }
    pub fn current(tr: TapeRef) u64 {
        // std.log.warn("TapeRef current() idx {} len/cap {}/{}", .{ tr.idx, tr.doc.tape.items.len, tr.doc.tape.capacity });
        return tr.doc.tape.items[tr.idx];
    }
    pub fn scope_count(tr: TapeRef) u32 {
        return @as(u32, @truncate((tr.current() >> 32) & TapeType.count_mask));
    }

    pub fn get_string_length(tr: TapeRef) u32 {
        const string_buf_index = tr.value();
        // std.debug.print("get_string_length string_buf_index={}\n", .{string_buf_index});
        return mem.readInt(u32, (tr.doc.string_buf.items.ptr + string_buf_index)[0..@sizeOf(u32)], .little);
    }

    pub fn get_c_str(tr: TapeRef) [*:0]u8 {
        return @as([*:0]u8, @ptrCast(tr.doc.string_buf.items.ptr + tr.value() + @sizeOf(u32)));
    }

    pub fn get_as_type(tr: TapeRef, comptime T: type) T {
        comptime assert(@sizeOf(T) == @sizeOf(u64));
        return @as(T, @bitCast(tr.current()));
    }

    pub fn get_next_as_type(tr: TapeRef, comptime T: type) T {
        comptime assert(@sizeOf(T) == @sizeOf(u64));
        return @as(T, @bitCast(tr.doc.tape.items[tr.idx + 1]));
    }

    pub fn get_string(tr: TapeRef) []u8 {
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

    pub fn element(tr: TapeRef) Element {
        return .{ .tape = tr };
    }
};

const TapeRefIterator = struct {
    tape: TapeRef,
    end_idx: u64,

    pub fn init(iter: anytype) TapeRefIterator {
        const tape = iter.tape;
        return .{
            .tape = .{ .doc = tape.doc, .idx = tape.idx + 1 },
            .end_idx = tape.after_element() - 1,
        };
    }
    pub fn next(tri: *TapeRefIterator) ?Element {
        tri.tape.idx += 1;
        tri.tape.idx = tri.tape.after_element();
        return if (tri.tape.idx >= tri.end_idx) null else tri.element();
    }

    pub fn element(tri: TapeRefIterator) Element {
        return .{ .tape = tri.tape };
    }
};

/// this is public to allow for custom jsonParse() methods
pub const Element = struct {
    tape: TapeRef,

    pub fn at_pointer(ele: Element, json_pointer: []const u8) cmn.Error!Element {
        return switch (ele.tape.tape_ref_type()) {
            .START_OBJECT => (try ele.get_object()).at_pointer(json_pointer),
            .START_ARRAY => (try ele.get_array()).at_pointer(json_pointer),
            else => if (json_pointer.len != 0) error.INVALID_JSON_POINTER else ele,
        };
    }

    pub fn at_key(ele: Element, key: []const u8) ?Element {
        return if (ele.get_as_type(.OBJECT)) |o| o.OBJECT.at_key(key) else |_| null;
    }

    pub fn at(ele: Element, idx: usize) ?Element {
        return if (ele.get_as_type(.ARRAY)) |a| a.ARRAY.at(idx) else |_| null;
    }

    /// parse from 'ele' into 'out' pointer
    pub fn get(ele: Element, out: anytype) cmn.Error!void {
        return ele.jsonParse(out, .{});
    }

    /// parse from 'ele' into 'out' pointer.
    /// to be used inplace of get() when 'out' contains non-string slice types.
    pub fn get_alloc(ele: Element, allocator: mem.Allocator, out: anytype) cmn.Error!void {
        return ele.jsonParse(out, .{ .allocator = allocator });
    }

    fn readSlice(
        ele: Element,
        out: anytype,
        options: cmn.GetOptions,
        comptime child_info: std.builtin.Type,
    ) cmn.Error!void {
        const Child = child_info.pointer.child;
        const allocator = options.allocator orelse return error.AllocatorRequired;
        var elems: std.ArrayList(Child) = .empty;
        defer elems.deinit(allocator);

        const arr = ele.get_array() catch unreachable;
        var it = TapeRefIterator.init(arr);
        // for (it.tape.doc.tape.items, 0..) |item, i| {
        //     std.debug.print("{}/{}={}\n", .{ i, it.end_idx, TapeType.from_u64(item) });
        // }

        while (true) {
            const arr_ele = Element{ .tape = it.tape };
            var out_ele: Child = undefined;
            const idx = it.tape.after_element();
            // std.debug.print("next_element={}:{}\n", .{ idx, TapeType.from_u64(it.tape.doc.tape.items[idx]) });
            try arr_ele.jsonParse(&out_ele, options);
            try elems.append(allocator, out_ele);
            it.tape.idx = idx;
            if (it.tape.idx >= it.end_idx) break;
        }
        out.* = try elems.toOwnedSlice(allocator);
    }

    // TODO support user error sets
    /// this being public allows for custom jsonParse() methods to call back into this method.
    /// out: pointer to be assigned
    pub fn jsonParse(ele: Element, out: anytype, options: cmn.GetOptions) cmn.Error!void {
        const T = @TypeOf(out);
        const info = @typeInfo(T);
        switch (info) {
            .pointer => {
                const C = std.meta.Child(T);
                if (comptime cmn.hasFn("jsonParse")(C))
                    return C.jsonParse(ele, out, options);

                const child_info = @typeInfo(C);
                switch (info.pointer.size) {
                    .one => {
                        switch (child_info) {
                            .int => out.* = std.math.cast(C, try if (child_info.int.signedness == .signed)
                                ele.get_int64()
                            else
                                ele.get_uint64()) orelse return error.Overflow,
                            .float => out.* = @as(C, @floatCast(try ele.get_double())),
                            .bool => out.* = try ele.get_bool(),
                            .optional => out.* = if (ele.is(.NULL))
                                null
                            else blk: {
                                var x: std.meta.Child(C) = undefined;
                                try ele.jsonParse(&x, options);
                                break :blk x;
                            },
                            .array => try ele.jsonParse(@as([]std.meta.Child(C), out), options),
                            .@"struct" => {
                                switch (ele.tape.tape_ref_type()) {
                                    .START_OBJECT => {
                                        var obj = ele.get_object() catch unreachable;
                                        inline for (std.meta.fields(C)) |field| {
                                            if (obj.at_key(field.name)) |obj_ele|
                                                try obj_ele.jsonParse(&@field(out, field.name), options);
                                        }
                                    },
                                    else => return error.INCORRECT_TYPE,
                                }
                            },
                            .pointer => if (child_info.pointer.size == .slice) {
                                if (child_info.pointer.child == u8) {
                                    // std.debug.print("ele.tape.tape_ref_type()={}", .{ele.tape.tape_ref_type()});
                                    switch (ele.tape.tape_ref_type()) {
                                        .STRING => out.* = try ele.get_string(),
                                        .START_ARRAY => try readSlice(ele, out, options, child_info),
                                        else => return error.INCORRECT_TYPE,
                                    }
                                } else {
                                    switch (ele.tape.tape_ref_type()) {
                                        .START_ARRAY => try readSlice(ele, out, options, child_info),
                                        else => return error.INCORRECT_TYPE,
                                    }
                                }
                            } else @compileError("unsupported type: " ++ @typeName(T) ++
                                ". expecting slice"),
                            else => @compileError("unsupported type: " ++ @typeName(T) ++
                                ". int, float, bool or optional type."),
                        }
                    },
                    .slice => {
                        switch (ele.tape.tape_ref_type()) {
                            .STRING => {
                                const string = ele.get_string() catch unreachable;
                                const len = @min(string.len, out.len * @sizeOf(C));
                                @memcpy(
                                    @as([*]u8, @ptrCast(out.ptr))[0..len],
                                    string.ptr[0..len],
                                );
                            },
                            .START_ARRAY => {
                                const arr = ele.get_array() catch unreachable;
                                var it = TapeRefIterator.init(arr);
                                for (out) |*out_ele| {
                                    const arr_ele = Element{ .tape = it.tape };
                                    try arr_ele.jsonParse(out_ele, options);
                                    _ = it.next() orelse break;
                                }
                            },
                            else => return error.INCORRECT_TYPE,
                        }
                    },

                    else => @compileError("unsupported pointer type: " ++ @typeName(T) ++
                        ". expecting slice or single item pointer."),
                }
            },
            else => @compileError("unsupported type: " ++ @typeName(T) ++ ". expecting pointer type."),
        }
    }

    pub fn get_as_type(ele: Element, ele_type: ElementType) cmn.Error!Value {
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
    pub fn get_int64(ele: Element) cmn.Error!i64 {
        return if (!ele.is(.INT64))
            if (ele.is(.UINT64)) blk: {
                const result = ele.next_tape_value(u64);
                break :blk if (result > std.math.maxInt(i64))
                    error.NUMBER_OUT_OF_RANGE
                else
                    @as(i64, @bitCast(result));
            } else error.INCORRECT_TYPE
        else
            ele.next_tape_value(i64);
    }
    pub fn get_uint64(ele: Element) cmn.Error!u64 {
        return if (!ele.is(.UINT64))
            if (ele.is(.INT64)) blk: {
                const result = ele.next_tape_value(i64);
                break :blk if (result < 0)
                    error.NUMBER_OUT_OF_RANGE
                else
                    @as(u64, @bitCast(result));
            } else error.INCORRECT_TYPE
        else
            ele.next_tape_value(u64);
    }
    pub fn get_double(ele: Element) !f64 {
        const val = try ele.get_tape_type(.DOUBLE);
        return val.DOUBLE;
    }
    pub fn get_string(ele: Element) ![]u8 {
        return (try ele.get_tape_type(.STRING)).STRING;
    }
    /// parse a string as an i64
    pub fn get_string_int64(ele: Element) !i64 {
        const string = (try ele.get_tape_type(.STRING)).STRING;
        return std.math.cast(i64, try number_parsing.parse_integer(
            string.ptr,
            .{ .mode = .from_string },
        )) orelse error.Overflow;
    }
    /// parse a string as an u64
    pub fn get_string_uint64(ele: Element) !u64 {
        const string = (try ele.get_tape_type(.STRING)).STRING;
        return number_parsing.parse_integer(
            string.ptr,
            .{ .mode = .from_string },
        );
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
        return mem.readInt(T, @as([*]const u8, @ptrCast(ele.tape.doc.tape.items.ptr + ele.tape.idx + 1))[0..8], .little);
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
        return switch (ele_type) {
            .OBJECT => ele.tape.is(.START_OBJECT),
            .ARRAY => ele.tape.is(.START_ARRAY),
            .STRING => ele.tape.is(.STRING),
            .INT64 => ele.tape.is(.INT64),
            .UINT64 => ele.tape.is(.UINT64),
            .DOUBLE => ele.tape.is(.DOUBLE),
            .BOOL => ele.tape.is(.TRUE) or ele.tape.is(.FALSE),
            .NULL => ele.tape.is(.NULL),
        };
    }

    pub fn get_type(ele: Element) TapeType {
        return ele.tape.tape_ref_type();
    }
};
