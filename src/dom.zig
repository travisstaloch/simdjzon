const std = @import("std");

const mem = std.mem;
const os = std.os;
const assert = std.debug.assert;
usingnamespace @import("vector_types.zig");
usingnamespace @import("llvm_intrinsics.zig");
usingnamespace @import("c_intrinsics.zig");
const string_parsing = @import("string_parsing.zig");
const number_parsing = @import("number_parsing.zig");
const atom_parsing = @import("atom_parsing.zig");
const Logger = @import("Logger.zig");
usingnamespace @import("common.zig");

pub const Document = struct {
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
        const tape_capacity = ROUNDUP_N(capacity + 3, 64);
        // a document with only zero-length strings... could have capacity/3 string
        // and we would need capacity/3 * 5 bytes on the string buffer
        document.string_buf_cap = ROUNDUP_N(5 * capacity / 3 + SIMDJSON_PADDING, 64);
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
    tail: std.ArrayListUnmanaged(u32),

    // flatten out values in 'bits' assuming that they are are to have values of idx
    // plus their position in the bitvector, and store these indexes at
    // base_ptr[base] incrementing base as we go
    // will potentially store extra values beyond end of valid bits, so base_ptr
    // needs to be large enough to handle this
    inline fn write(indexer: *BitIndexer, reader_pos_: u64, bits_: u64) void {
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
        {
            var new_items = indexer.tail.addManyAsArrayAssumeCapacity(8);
            for (new_items) |*ptr| {
                ptr.* = @intCast(u32, reader_pos + @ctz(u64, bits));
                bits = (bits -% 1) & bits;
                // std.log.debug("bits {}", .{bits});
            }
        }

        // Do the next 8 all together (we hope in most cases it won't happen at all
        // and the branch is easily predicted).
        if (cnt > 8) {
            var new_items = indexer.tail.addManyAsArrayAssumeCapacity(8);
            for (new_items) |*ptr| {
                ptr.* = @intCast(u32, reader_pos + @ctz(u64, bits));
                bits = (bits -% 1) & bits;
            }
        }

        // Most files don't have 16+ structurals per block, so we take several basically guaranteed
        // branch mispredictions here. 16+ structurals per block means either punctuation ({} [] , // :)
        // or the start of a value ("abc" true 123) every four characters.
        if (cnt > 16) {
            var i: usize = 16;
            while (true) {
                indexer.tail.appendAssumeCapacity(@intCast(u32, reader_pos + @ctz(u64, bits)));
                bits = (bits -% 1) & bits;
                i += 1;
                if (i >= cnt) break;
            }
        }

        // std.log.debug("tail.items.len {d} start_count + cnt {d}", .{ indexer.tail.items.len, start_count + cnt });
        indexer.tail.shrinkRetainingCapacity(start_count + cnt);
    }
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
    // zig fmt: off
    inline fn check_special_cases(input: u8x32, prev1: u8x32) u8x32 {
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

        const byte_2_high_0 = input >> @splat(32, @as(u3, 4));
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
        // const NUM_CHUNKS = step_size / 32;
        const NUM_CHUNKS = 2;
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
            }
            // TODO: NUM_CHUNKS = 4
            // else if (NUM_CHUNKS == 4) {
            //     checker.check_utf8_bytes(chunks[0], checker.prev_input_block);
            //     checker.check_utf8_bytes(chunks[1], chunks[0]);
            //     checker.check_utf8_bytes(chunks[2], chunks[1]);
            //     checker.check_utf8_bytes(chunks[3], chunks[2]);
            // }
            else unreachable;
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
        // If the previous input's last 3 bytes match this, they're too short (they ended at EOF):
        // ... 1111____ 111_____ 11______
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

        const in = @bitCast([64]u8, input_vec);
        const chunk0: u8x32 = in[0..32].*;
        const chunk1: u8x32 = in[32..64].*;
        const wss: [2]u8x32 = .{
            shuffleEpi8(whitespace_table, chunk0),
            shuffleEpi8(whitespace_table, chunk1),
        };
        const whitespace = input_vec == @bitCast(u8x64, wss);
        // Turn [ and ] into { and }
        const curlified = input_vec | @splat(64, @as(u8, 0x20));
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
    inline fn non_quote_inside_string(block: Block, mask: u64) u64 {
        return block.string.non_quote_inside_string(mask);
    }
};

pub const StructuralIndexer = struct {
    prev_structurals: u64 = 0,
    unescaped_chars_error: u64 = 0,
    bit_indexer: BitIndexer,
    checker: Utf8Checker,

    pub fn init() !StructuralIndexer {
        return StructuralIndexer{
            .bit_indexer = .{ .tail = std.ArrayListUnmanaged(u32){} },
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

    pub fn step(si: *StructuralIndexer, read_buf: [step_size]u8, parser: *Parser, reader_pos: u64) !void {
        if (step_size == 64) {
            const block_1 = nextBlock(parser, read_buf);
            // println("{b:0>64} | characters.op", .{@bitReverse(u64, block_1.characters.op)});
            try si.next(read_buf, block_1, reader_pos);
            // std.log.debug("stream pos {}", .{try stream.getPos()});
        } else {
            const block_1 = nextBlock(parser, read_buf[0..64].*);
            const block_2 = nextBlock(parser, read_buf[64..128].*);
            try si.next(read_buf[0..64].*, block_1, reader_pos);
            try si.next(read_buf[64..128].*, block_2, reader_pos + 64);
        }
    }

    pub fn finish(si: *StructuralIndexer, parser: *Parser, idx: usize, len: usize, partial: bool) !void {
        _ = partial;
        // println("finish idx {}, len {}", .{ idx, len });
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
        var new_inds = parser.indexer.bit_indexer.tail.addManyAsArrayAssumeCapacity(3);
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
        const mask = @splat(32, m);
        const a = chunks[0] <= mask;
        const b = chunks[1] <= mask;
        const aint = @as(u64, @ptrCast(*const u32, &a).*);
        const bint = @as(u64, @ptrCast(*const u32, &b).*) << 32;
        return aint | bint;
    }

    fn next(si: *StructuralIndexer, input_vec: u8x64, block: Block, reader_pos: u64) !void {
        const chunks = @bitCast([2]u8x32, input_vec);
        const unescaped = lteq(u8, chunks, 0x1F);
        if (debug) {
            var input: [step_size]u8 = undefined;
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

    inline fn advance(iter: *Iterator) [*]const u8 {
        defer iter._next_structural += 1;
        // std.log.debug("advance() next_structural idx {} peek() '{c}'", .{ (@ptrToInt(iter.next_structural) - @ptrToInt(iter.parser.indexer.bit_indexer.tail.items.ptr)) / 4, iter.peek() });
        return iter.peek();
    }

    inline fn peek(iter: *Iterator) [*]const u8 {
        return iter.parser.bytes.ptr + iter._next_structural[0];
    }

    pub inline fn at_beginning(iter: *Iterator) bool {
        // std.log.debug("at-beginning {*}: {}", .{ iter.next_structural, iter.next_structural[0] });
        return iter.next_structural() == iter.parser.indexer.bit_indexer.tail.items.ptr;
    }

    pub inline fn next_structural(iter: Iterator) [*]u32 {
        return iter._next_structural;
    }

    inline fn at_eof(iter: *Iterator) bool {
        // std.log.debug("at-beginning {*}: {}", .{ iter.next_structural, iter.next_structural[0] });
        return @ptrToInt(iter._next_structural) == @ptrToInt(iter.parser.indexer.bit_indexer.tail.items.ptr + iter.parser.n_structural_indexes);
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
        if (iter.parser.bytes.len == 0) return iter.document_end(visitor);

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
        iter.log.start_value(iter, "object");
        iter.depth += 1;
        // iter.log.line_fmt(iter, "", "depth", "{d}/{d}", .{ iter.depth, iter.parser.max_depth });

        if (iter.depth >= iter.parser.max_depth) {
            iter.log.err(iter, "Exceeded max depth!");
            return error.DEPTH_ERROR;
        }
        try visitor.visit_object_start(iter);

        const key = iter.advance();
        if (key[0] != '"') {
            iter.log.err(iter, "Object does not start with a key");
            return error.TAPE_ERROR;
        }
        iter.increment_count();
        try visitor.visit_key(iter, key);
        return .object_field;
    }

    inline fn object_field(iter: *Iterator, visitor: *TapeBuilder) Error!State {
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

    inline fn object_continue(iter: *Iterator, visitor: *TapeBuilder) Error!State {
        const value = iter.advance();
        // std.log.debug("object_continue() value '{c}'", .{value});
        switch (value[0]) {
            ',' => {
                iter.increment_count();
                const key = iter.advance();
                // println("key '{c}'", .{key});
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

    inline fn scope_end(iter: *Iterator, _: *TapeBuilder) Error!State {
        // std.log.debug("scope_end iter.depth {}", .{iter.depth});
        iter.depth -= 1;
        if (iter.depth == 0) return .document_end;
        const is_array = iter.parser.open_containers.items(.is_array)[iter.depth];
        if (is_array)
            return .array_continue;

        return .object_continue;
    }

    inline fn array_begin(iter: *Iterator, visitor: *TapeBuilder) Error!State {
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

    inline fn array_value(iter: *Iterator, visitor: *TapeBuilder) Error!State {
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

    inline fn array_continue(iter: *Iterator, visitor: *TapeBuilder) Error!State {
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

    inline fn document_end(iter: *Iterator, visitor: *TapeBuilder) Error!void {
        iter.log.end_value(iter, "document");
        try visitor.visit_document_end();
        iter.parser.next_structural_index = try ptr_diff(
            u32,
            iter._next_structural,
            iter.parser.indexer.bit_indexer.tail.items.ptr,
        );

        // If we didn't make it to the end, it's an error
        // std.log.debug("next_structural_index {} n_structural_indexes {}", .{ iter.parser.next_structural_index, iter.parser.n_structural_indexes });
        // have to add because there are 3 additional items added to tail in finish()
        if (!STREAMING and iter.parser.next_structural_index != iter.parser.n_structural_indexes) {
            iter.log.err(iter, "More than one JSON value at the root of the document, or extra characters at the end of the JSON!");
            return error.TAPE_ERROR;
        }
    }

    inline fn current_container(iter: *Iterator) *OpenContainer {
        // std.log.debug("current_container iter.parser.open_containers.len {} iter.depth {}", .{ iter.parser.open_containers.items.len, iter.depth });
        return &iter.parser.open_containers.items(.open_container)[iter.depth];
    }
    inline fn increment_count(iter: *Iterator) void {
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

    pub inline fn append(tb: *TapeBuilder, val: u64, tt: TapeType) void {
        // iter.log.line_fmt(iter, "", "append", "val {} tt {}", .{ val, tt });
        tb.tape.appendAssumeCapacity(val | tt.as_u64());
    }

    pub inline fn append2(tb: *TapeBuilder, val: u64, val2: anytype, tt: TapeType) void {
        tb.append(val, tt);
        assert(@sizeOf(@TypeOf(val2)) == 8);
        tb.tape.appendAssumeCapacity(val2);
    }

    pub inline fn append_double(tb: *TapeBuilder, val: f64) void {
        tb.append2(0, @bitCast(u64, val), .DOUBLE);
    }

    pub inline fn append_i64(tb: *TapeBuilder, val: u64) void {
        tb.append2(0, val, .INT64);
    }

    pub inline fn append_u64(tb: *TapeBuilder, val: u64) void {
        tb.append2(0, val, .UINT64);
    }

    pub inline fn write(tb: *TapeBuilder, idx: usize, val: u64, tt: TapeType) void {
        // iter.log.line_fmt(iter, "", "write", "val {} tt {} idx {}", .{ val, tt, idx });
        assert(idx < tb.tape.items.len);
        tb.tape.items[idx] = val | tt.as_u64();
    }

    pub inline fn next_tape_index(tb: TapeBuilder) u32 {
        return @intCast(u32, tb.tape.items.len);
    }

    pub inline fn skip(tb: TapeBuilder) void {
        _ = tb.tape.addOneAssumeCapacity();
    }

    pub inline fn empty_container(tb: *TapeBuilder, start: TapeType, end: TapeType) void {
        const start_index = tb.next_tape_index();
        tb.append(start_index + 2, start);
        tb.append(start_index, end);
    }

    pub inline fn start_container(
        tb: TapeBuilder,
        open_containers: *std.MultiArrayList(OpenContainerInfo),
        is_array: bool,
        count: u32,
    ) void {
        const tape_idx = tb.next_tape_index();
        open_containers.appendAssumeCapacity(.{
            .is_array = is_array,
            .open_container = .{
                .tape_index = @intCast(u32, tape_idx),
                .count = count,
            },
        });
        tb.skip();
    }

    pub inline fn end_container(tb: *TapeBuilder, iter: *Iterator, start: TapeType, end: TapeType) void {
        // Write the ending tape element, pointing at the start location
        const container = iter.parser.open_containers.items(.open_container)[iter.depth];
        defer iter.parser.open_containers.shrinkRetainingCapacity(iter.depth);
        const start_tape_index = container.tape_index;
        tb.append(start_tape_index, end);
        // Write the start tape element, pointing at the end location (and including count)
        // count can overflow if it exceeds 24 bits... so we saturate
        // the convention being that a cnt of 0xffffff or more is undetermined in value (>=  0xffffff).
        const cntsat: u32 = std.math.min(@intCast(u32, container.count), 0xFFFFFF);

        // iter.log.line_fmt(iter, "", "end_container", "next_tape_index {}", .{tb.next_tape_index()});
        tb.write(start_tape_index, tb.next_tape_index() | (@as(u64, cntsat) << 32), start);
    }

    inline fn on_start_string(tb: *TapeBuilder, iter: *Iterator) ![*]u8 {
        // iter.log.line_fmt(iter, "", "start_string", "iter.parser.doc.string_buf.len {}", .{iter.parser.doc.string_buf.len});
        tb.append(ptr_diff(u64, tb.current_string_buf_loc, iter.parser.doc.string_buf.ptr) catch unreachable, .STRING);
        return tb.current_string_buf_loc + @sizeOf(u32);
    }
    inline fn on_end_string(tb: *TapeBuilder, iter: *Iterator, dst: [*]u8) !void {
        const str_len = try ptr_diff(u32, dst, tb.current_string_buf_loc + @sizeOf(u32));
        // println("str_len {} str '{s}'", .{ str_len, (tb.current_string_buf_loc + 4)[0..str_len] });

        // TODO check for overflow in case someone has a crazy string (>=4GB?)
        // But only add the overflow check when the document itself exceeds 4GB
        // Currently unneeded because we refuse to parse docs larger or equal to 4GB.

        // NULL termination is still handy if you expect all your strings to
        // be NULL terminated? It comes at a small cost

        // iter.log.line_fmt(iter, "", "on_string_end", "{s}", .{str_start[0..str_len]});
        @memcpy(tb.current_string_buf_loc, mem.asBytes(&str_len), @sizeOf(u32));
        dst[0] = 0;
        iter.parser.doc.string_buf.len += str_len + 1 + @sizeOf(u32);
        // println("buf.len {} buf.cap {}", .{ iter.parser.doc.string_buf.len, iter.parser.doc.string_buf_cap });
        assert(iter.parser.doc.string_buf.len <= iter.parser.doc.string_buf_cap);
        tb.current_string_buf_loc += str_len + 1 + @sizeOf(u32);
    }

    inline fn visit_root_primitive(visitor: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
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

    inline fn visit_number(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log.value(iter, "number");
        try number_parsing.parse_number(value, tb);
    }
    inline fn visit_true_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log.value(iter, "true");
        assert(value[0] == 't');
        if (!atom_parsing.is_valid_rue_atom(value + 1)) return error.T_ATOM_ERROR;
        tb.append(0, TapeType.TRUE);
    }
    inline fn visit_false_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log.value(iter, "false");
        assert(value[0] == 'f');
        if (!atom_parsing.is_valid_alse_atom(value + 1)) return error.T_ATOM_ERROR;
        tb.append(0, TapeType.FALSE);
    }
    inline fn visit_null_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log.value(iter, "null");
        assert(value[0] == 'n');
        if (!atom_parsing.is_valid_ull_atom(value + 1)) return error.T_ATOM_ERROR;
        tb.append(0, TapeType.NULL);
    }

    inline fn visit_primitive(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return switch (value[0]) {
            '"' => tb.visit_string(iter, value, false),
            't' => tb.visit_true_atom(iter, value),
            'f' => tb.visit_false_atom(iter, value),
            'n' => tb.visit_null_atom(iter, value),
            '-', '0'...'9' => tb.visit_number(iter, value),
            else => |c| blk: {
                iter.log.err_fmt(iter, "Non-value found when value was expected.  Value: '{c}' - (0x{x}:{})", .{ c, c, c });
                break :blk error.TAPE_ERROR;
            },
        };
    }

    inline fn visit_string(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8, key: bool) Error!void {
        iter.log.value(iter, if (key) "key" else "string");
        var dst = try tb.on_start_string(iter);
        dst = string_parsing.parse_string(value + 1, dst) orelse {
            iter.log.err(iter, "Invalid escape in string");
            return error.STRING_ERROR;
        };

        try tb.on_end_string(iter, dst);
    }

    pub inline fn visit_key(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return tb.visit_string(iter, value, true);
    }

    pub inline fn visit_empty_object(tb: *TapeBuilder) Error!void {
        return tb.empty_container(.START_OBJECT, .END_OBJECT);
    }

    pub inline fn visit_empty_array(tb: *TapeBuilder) Error!void {
        return tb.empty_container(.START_ARRAY, .END_ARRAY);
    }

    pub inline fn visit_array_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, true, 0);
    }

    pub inline fn visit_array_end(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.end_container(iter, .START_ARRAY, .END_ARRAY);
    }

    pub inline fn visit_object_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, false, 0);
    }
    pub inline fn visit_object_end(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.end_container(iter, .START_OBJECT, .END_OBJECT);
    }
    pub inline fn visit_document_start(tb: *TapeBuilder, iter: *Iterator) !void {
        return tb.start_container(&iter.parser.open_containers, false, 0);
    }
    pub inline fn visit_document_end(tb: *TapeBuilder) !void {
        tb.write(0, tb.next_tape_index(), .ROOT);
        // iter.log.line_fmt(iter, "?", "document_end", "open_containers.len {} tape.len {}", .{ iter.parser.open_containers.items.len, tb.tape.items.len });
        return tb.append(0, .ROOT);
    }
};

pub const Parser = struct {
    filename: []const u8,
    allocator: *mem.Allocator,
    prev_escaped: u64 = 0,
    prev_in_string: u64 = 0,
    prev_scalar: u64 = 0,
    next_structural_index: u32 = 0,
    doc: Document,
    indexer: StructuralIndexer,
    open_containers: std.MultiArrayList(OpenContainerInfo),
    max_depth: u16,
    n_structural_indexes: u32 = 0,
    bytes: []u8 = &[_]u8{},
    input_len: u32 = 0,

    pub const Options = struct {
        max_depth: u16 = DEFAULT_MAX_DEPTH,
    };

    pub fn initFile(allocator: *mem.Allocator, filename: []const u8, options: Options) !Parser {
        var parser = Parser{
            .filename = filename,
            .allocator = allocator,
            .doc = Document.init(),
            .indexer = try StructuralIndexer.init(),
            .open_containers = std.MultiArrayList(OpenContainerInfo){},
            .max_depth = options.max_depth,
        };
        parser.input_len = try parser.read_file(filename);
        const capacity = parser.input_len;
        try parser.doc.allocate(allocator, capacity);
        const max_structures = ROUNDUP_N(capacity, 64) + 2 + 7;
        try parser.indexer.bit_indexer.tail.ensureTotalCapacity(allocator, max_structures);
        try parser.open_containers.ensureTotalCapacity(allocator, options.max_depth);
        return parser;
    }

    const ascii_space = 0x20;

    pub fn initFixedBuffer(allocator: *mem.Allocator, input: []const u8, options: Options) !Parser {
        var parser = Parser{
            .filename = "<fixed buffer>",
            .allocator = allocator,
            .doc = Document.init(),
            .indexer = try StructuralIndexer.init(),
            .bytes = &[_]u8{},
            .open_containers = std.MultiArrayList(OpenContainerInfo){},
            .max_depth = options.max_depth,
        };
        parser.input_len = try std.math.cast(u32, input.len);
        const capacity = parser.input_len;
        const max_structures = ROUNDUP_N(capacity, 64) + 2 + 7;
        const paddedlen = try std.math.add(u32, capacity, SIMDJSON_PADDING);
        parser.bytes = try parser.allocator.alloc(u8, paddedlen);
        mem.copy(u8, parser.bytes, input);

        // We write spaces in the padded region to avoid having uninitized
        // garbage. If nothing else, garbage getting read might trigger a
        // warning in a memory checking.
        std.mem.set(u8, parser.bytes[capacity..], ascii_space);
        try parser.doc.allocate(allocator, capacity);
        try parser.indexer.bit_indexer.tail.ensureTotalCapacity(allocator, max_structures);
        try parser.open_containers.ensureTotalCapacity(allocator, options.max_depth);
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
            // We write spaces in the padded region to avoid having uninitized
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
        const end_pos_minus_step = if (end_pos > step_size) end_pos - step_size else 0;

        var pos: u32 = 0;
        while (pos < end_pos_minus_step) : (pos += step_size) {
            // println("i {} pos {}", .{ i, pos });
            const read_buf = parser.bytes[pos..][0..step_size];
            try parser.indexer.step(read_buf.*, parser, pos);
            // for (blocks) |block| {
            //     println("{b:0>64} | characters.whitespace", .{@bitReverse(u64, block.characters.whitespace)});
            //     println("{b:0>64} | characters.op", .{@bitReverse(u64, block.characters.op)});
            //     println("{b:0>64} | in_string", .{@bitReverse(u64, block.strings.in_string)});
            // }
        }
        var read_buf = [1]u8{0x20} ** step_size;
        std.mem.copy(u8, &read_buf, parser.bytes[pos..end_pos]);
        // std.log.debug("read_buf {d}", .{read_buf});
        try parser.indexer.step(read_buf, parser, pos);
        try parser.indexer.finish(parser, pos + step_size, end_pos, STREAMING);
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

    pub fn element(parser: Parser) Element {
        return .{
            .tape = .{ .doc = &parser.doc, .idx = 1 },
        };
    }
};

const Array = struct {
    tape: TapeRef,
    pub fn at(a: Array, idx: usize) ?Element {
        var it = TapeRefIterator.init(a);
        const target_idx = idx + it.tape.idx + idx;
        while (true) {
            if (it.tape.idx == target_idx)
                return Element{ .tape = .{ .doc = it.tape.doc, .idx = it.tape.idx } };
            _ = it.next() orelse break;
        }
        return null;
    }

    pub inline fn at_pointer(arr: Array, _json_pointer: []const u8) Error!Element {
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

    pub fn at_pointer(o: Object, _json_pointer: []const u8) Error!Element {
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
        //     mem.copy(u8, &unescaped, key);
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
        return TapeType.from_u64(tr.current());
    }
    pub inline fn value(tr: TapeRef) u64 {
        return TapeType.extract_value(tr.current());
    }
    pub inline fn next_value(tr: TapeRef) u64 {
        return TapeType.extract_value(tr.doc.tape.items[tr.idx + 1]);
    }
    pub inline fn after_element(tr: TapeRef) u64 {
        return switch (tr.tape_ref_type()) {
            .START_ARRAY, .START_OBJECT => tr.matching_brace_idx(),
            .UINT64, .INT64, .DOUBLE => tr.idx + 2,
            else => tr.idx + 1,
        };
    }
    pub inline fn matching_brace_idx(tr: TapeRef) u32 {
        const result = @truncate(u32, tr.current());
        // std.log.debug("TapeRef matching_brace_idx() for {} {}", .{ tr.tape_ref_type(), result });
        return result;
    }
    pub inline fn current(tr: TapeRef) u64 {
        // std.log.debug("TapeRef current() idx {} len {}", .{ tr.idx, tr.doc.tape.items.len });
        return tr.doc.tape.items[tr.idx];
    }
    pub inline fn scope_count(tr: TapeRef) u32 {
        return @truncate(u32, (tr.current() >> 32) & TapeType.count_mask);
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
        return @bitCast(T, tr.current());
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

    pub fn init(iter: anytype) TapeRefIterator {
        const tape = iter.tape;
        return .{
            .tape = .{ .doc = tape.doc, .idx = tape.idx + 1 },
            .end_idx = tape.after_element() - 1,
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

    pub inline fn at_pointer(ele: Element, json_pointer: []const u8) Error!Element {
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

    pub fn get(ele: Element, out: anytype) Error!void {
        const T = @TypeOf(out);
        const info = @typeInfo(T);
        switch (info) {
            .Pointer => {
                const C = std.meta.Child(T);
                const child_info = @typeInfo(C);
                switch (info.Pointer.size) {
                    .One => {
                        switch (child_info) {
                            .Int => out.* = try std.math.cast(C, try if (child_info.Int.signedness == .signed)
                                ele.get_int64()
                            else
                                ele.get_uint64()),
                            .Float => out.* = @floatCast(C, try ele.get_double()),
                            .Bool => out.* = try ele.get_bool(),
                            .Optional => out.* = if (ele.is(.NULL))
                                null
                            else blk: {
                                var x: std.meta.Child(C) = undefined;
                                try ele.get(&x);
                                break :blk x;
                            },
                            .Array => try ele.get(@as([]std.meta.Child(C), out)),
                            .Struct => {
                                switch (ele.tape.tape_ref_type()) {
                                    .START_OBJECT => {
                                        var obj = ele.get_object() catch unreachable;
                                        inline for (std.meta.fields(C)) |field| {
                                            if (obj.at_key(field.name)) |obj_ele|
                                                try obj_ele.get(&@field(out, field.name));
                                        }
                                    },
                                    else => return error.INCORRECT_TYPE,
                                }
                            },
                            else => @compileError("unsupported type: " ++ @typeName(T) ++
                                ". int, float, bool or optional type."),
                        }
                    },
                    .Slice => {
                        switch (ele.tape.tape_ref_type()) {
                            .STRING => {
                                const string = ele.get_string() catch unreachable;
                                @memcpy(
                                    @ptrCast([*]u8, out.ptr),
                                    string.ptr,
                                    std.math.min(string.len, out.len * @sizeOf(C)),
                                );
                            },
                            .START_ARRAY => {
                                var arr = ele.get_array() catch unreachable;
                                var it = TapeRefIterator.init(arr);
                                for (out) |*out_ele| {
                                    const arr_ele = Element{ .tape = it.tape };
                                    try arr_ele.get(out_ele);
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

    pub fn get_as_type(ele: Element, ele_type: ElementType) Error!Value {
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
    pub fn get_int64(ele: Element) Error!i64 {
        return if (!ele.is(.INT64))
            if (ele.is(.UINT64)) blk: {
                const result = ele.next_tape_value(u64);
                break :blk if (result > std.math.maxInt(i64))
                    error.NUMBER_OUT_OF_RANGE
                else
                    @bitCast(i64, result);
            } else error.INCORRECT_TYPE
        else
            ele.next_tape_value(i64);
    }
    pub fn get_uint64(ele: Element) Error!u64 {
        return if (!ele.is(.UINT64))
            if (ele.is(.INT64)) blk: {
                const result = ele.next_tape_value(i64);
                break :blk if (result < 0)
                    error.NUMBER_OUT_OF_RANGE
                else
                    @bitCast(u64, result);
            } else error.INCORRECT_TYPE
        else
            ele.next_tape_value(u64);
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
};
