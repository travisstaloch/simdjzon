const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const os = std.os;
const assert = std.debug.assert;
usingnamespace @import("vector_types.zig");
usingnamespace @import("llvm_intrinsics.zig");
const StringParsing = @import("StringParsing.zig");
const NumberParsing = @import("NumberParsing.zig");
const AtomParsing = @import("AtomParsing.zig");

const show_log = true;
const debug = true;
pub fn println(comptime fmt: []const u8, args: anytype) void {
    print(fmt ++ "\n", args);
}
pub fn print(comptime fmt: []const u8, args: anytype) void {
    if (show_log)
        std.debug.print(fmt, args);
    // std.log.debug(fmt, args);
}
inline fn SIMDJSON_ROUNDUP_N(a: usize, n: usize) usize {
    return (a + (n - 1)) & ~(n - 1);
}
const Document = struct {
    tape: std.ArrayListUnmanaged(u64),
    string_buf: std.ArrayListUnmanaged(u8),
    pub fn init() Document {
        return .{
            .tape = std.ArrayListUnmanaged(u64){},
            .string_buf = std.ArrayListUnmanaged(u8){},
        };
    }

    pub fn allocate(document: *Document, allocator: *mem.Allocator, capacity: usize) !void {
        if (capacity == 0) return;

        // a pathological input like "[[[[..." would generate capacity tape elements, so
        // need a capacity of at least capacity + 1, but it is also possible to do
        // worse with "[7,7,7,7,6,7,7,7,6,7,7,6,[7,7,7,7,6,7,7,7,6,7,7,6,7,7,7,7,7,7,6"
        //where capacity + 1 tape elements are
        // generated, see issue https://github.com/simdjson/simdjson/issues/345
        const tape_capacity = SIMDJSON_ROUNDUP_N(capacity + 3, 64);
        // a document with only zero-length strings... could have capacity/3 string
        // and we would need capacity/3 * 5 bytes on the string buffer
        const string_capacity = SIMDJSON_ROUNDUP_N(5 * capacity / 3 + SIMDJSON_PADDING, 64);
        //   string_buf.reset( new (std::nothrow) uint8_t[string_capacity]);
        //   tape.reset(new (std::nothrow) uint64_t[tape_capacity]);
        errdefer {
            document.string_buf.deinit(allocator);
            document.tape.deinit(allocator);
        }
        try document.tape.ensureTotalCapacity(allocator, tape_capacity);
        try document.string_buf.ensureTotalCapacity(allocator, string_capacity);
    }

    pub fn deinit(doc: *Document, allocator: *mem.Allocator) void {
        doc.tape.deinit(allocator);
        doc.string_buf.deinit(allocator);
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
        if (debug) print("{b:0>64} | bits", .{@bitReverse(u64, bits_)});
        if (bits == 0)
            return;
        const reader_pos = @intCast(i32, reader_pos_ - 64); //  this function is always passed last bits so reader_pos will be ahead by 64
        const cnt = @popCount(u64, bits);
        if (debug) println(", reader_pos {}", .{reader_pos});
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

        // std.log.debug("tail.items.len {d} tail {d}", .{ indexer.tail.items.len, indexer.tail.items[indexer.tail.items.len - cnt ..] });
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
    err: u8x8 = [1]u8{0} ** 8,
    prev_input_block: u8x8 = [1]u8{0} ** 8,
    prev_incomplete: u8x8 = [1]u8{0} ** 8,

    //
    // Check whether the current bytes are valid UTF-8.
    //
    inline fn check_utf8_bytes(checker: *Utf8Checker, input: u8x8, prev_input: u8x8) void {
        // Flip prev1...prev3 so we can easily determine if they are 2+, 3+ or 4+ lead bytes
        // (2, 3, 4-byte leads become large positive numbers instead of small negative numbers)
        const prev1 = input.prev(1, prev_input);
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
        //   if(simdjson_likely(is_ascii(input))) {
        if (is_ascii(input)) {
            checker.err |= checker.prev_incomplete;
        } else {
            const NUM_CHUNKS = STEP_SIZE / 8;
            // you might think that a for-loop would work, but under Visual Studio, it is not good enough.
            // static_assert((simd8x64<uint8_t>::NUM_CHUNKS == 2) || (simd8x64<uint8_t>::NUM_CHUNKS == 4),
            //     "We support either two or four chunks per 64-byte block.");
            // if(simd8x64<uint8_t>::NUM_CHUNKS == 2) {
            if (NUM_CHUNKS == 2) {
                const bytes: [64]u8 = input;
                const chunk0: u8x8 = bytes[0..8].*;
                const chunk1: u8x8 = bytes[8..16].*;
                checker.check_utf8_bytes(chunk0, checker.prev_input_block);
                checker.check_utf8_bytes(chunk1, chunk0);
                // } else if(simd8x64<uint8_t>::NUM_CHUNKS == 4) {
            } else if (NUM_CHUNKS == 4) {
                const bytes: [128]u8 = input;
                const chunk0: u8x8 = bytes[0..8].*;
                const chunk1: u8x8 = bytes[8..16].*;
                const chunk2: u8x8 = bytes[16..32].*;
                const chunk3: u8x8 = bytes[32..40].*;
                checker.check_utf8_bytes(chunk0, checker.prev_input_block);
                checker.check_utf8_bytes(chunk1, chunk0);
                checker.check_utf8_bytes(chunk2, chunk1);
                checker.check_utf8_bytes(chunk3, chunk2);
            }
            // checker.prev_incomplete = is_incomplete(input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1]);
            // checker.prev_input_block = input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1];
            const chunks = @bitCast([if (STEP_SIZE == 64) 8 else 16][8]u8, input);
            checker.prev_incomplete = is_incomplete(chunks[NUM_CHUNKS - 1]);
            checker.prev_input_block = chunks[NUM_CHUNKS - 1];
        }
    }
    // do not forget to call check_eof!
    inline fn errors(checker: Utf8Checker) JsonError!void {
        const err = @bitCast(u64, checker.err);
        if (err != 0) return error.UTF8_ERROR;
    }
    // const error_code = enum {
    //     UTF8_ERROR,
    //     SUCCESS,
    // };

    //
    // Return nonzero if there are incomplete multibyte characters at the end of the block:
    // e.g. if there is a 4-byte character, but it's 3 bytes from the end.
    //
    inline fn is_incomplete(input: u8x8) u8x8 {
        // If the previous input's last 3 bytes match this, they're too short (they ended at EOF):
        // ... 1111____ 111_____ 11______
        const max_array = [32]u8{ 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0b11110000 - 1, 0b11100000 - 1, 0b11000000 - 1 };
        // const max_value(&max_array[sizeof(max_array)-sizeof(u8x8)]);
        const max_value: u8x8 = max_array[@sizeOf(@TypeOf(max_array)) - @sizeOf(u8x8) ..][0..8].*;
        // return input.gt_bits(max_value);
        return _mm256_subs_epu8(input, max_value);
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
            // std.log.debug("{s}", .{input});
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
        // parser.n_structural_indexes = si.bit_indexer.tail.items.len;
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

    fn next(si: *StructuralIndexer, input_vec: u8x64, block: Block, reader_pos: u64, allocator: *mem.Allocator) !void {
        const chunks = @bitCast([2]u8x32, input_vec);
        const unescaped = lteq(u8, chunks, 0x1F);
        var input: [64]u8 = undefined;
        std.mem.copy(u8, &input, &@as([64]u8, input_vec));
        for (input) |*c| {
            if (c.* == '\n') c.* = '-';
        }
        if (debug) println("{s}", .{input});
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

    fn step_(si: *StructuralIndexer, reader: anytype, step_idx: u32, parser: *Parser) ![STEP_SIZE / 64]Block {
        comptime var n: u8 = 0;
        var result: [STEP_SIZE / 64]Block = undefined;
        inline while (n < (STEP_SIZE / 64)) : (n += 1) {
            const bytes_read = try reader.read(&read_buf);
            if (bytes_read == 0)
                break
            else if (bytes_read != 64) std.debug.panic("TODO: bytes_read ({}) != 64 ({})", .{ bytes_read, STEP_SIZE });
            const input_vec: u8x64 = read_buf[0..64].*;
            const chunks = @bitCast([2]u8x32, input_vec);
            println("{s} | input data : len {}", .{ read_buf, bytes_read });

            // TODO: if STEP_SIZE == 128, repeat
            const unescaped = lteq(u8, chunks, 0x1F);
            // println("{b:0>64} | unscaped", .{@bitReverse(u64, unescaped)});
            si.checker.check_next_input(input_vec);

            si.bit_indexer.write(step_idx, si.prev_structurals); // Output *last* iteration's structurals to the parser
            si.prev_structurals = block.structural_start();
            const strings = parser.nextStringBlock(input_vec);
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
            const nonquote_scalar = characters.scalar() & ~strings.quote;
            const follows_nonquote_scalar = follows(nonquote_scalar, &parser.prev_scalar);
            // We are returning a function-local object so either we get a move constructor
            // or we get copy elision.
            if (unescaped & strings.in_string != 0) return error.UnescapedCharacters;
            result[n] = Block{
                .strings = strings, // strings is a function-local object so either it moves or the copy is elided.
                .characters = characters,
                .follows_nonquote_scalar = follows_nonquote_scalar,
            };
        }
        return result;
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
        if (!debug) return;
        var buf: [0x1000]u8 = undefined;
        var fba = std.heap.FixedBufferAllocator.init(&buf);

        const depth_padding = pad_with_alloc("", ' ', @intCast(u8, if (iter.log_depth < 0x0f) iter.log_depth * 2 else 0xff), &fba.allocator);
        const titles = std.fmt.allocPrint(&fba.allocator, "{s}{s}{s}", .{ depth_padding, title_prefix, title }) catch unreachable;
        const p1 = pad_with_alloc(titles, ' ', LOG_EVENT_LEN, &fba.allocator);
        print("| {s} ", .{p1});
        const current_index = if (iter.at_beginning()) null else iter.next_structural - 1;
        const next_index = iter.next_structural;
        var buf2: [LOG_BUFFER_LEN]u8 = undefined;
        const content = blk: {
            if (current_index) |ci| {
                for (buf2) |*c, i| {
                    c.* = printable_char(iter.parser.bytes[ci[0] + i]);
                }
                break :blk pad_with_alloc(&buf2, ' ', LOG_BUFFER_LEN, &fba.allocator);
            } else {
                break :blk &pad_with("", ' ', LOG_BUFFER_LEN);
            }
        };
        print("| {s} ", .{content});
        const next_content = blk: {
            for (buf2) |*c, i| {
                if (next_index[0] + i >= iter.parser.bytes.len) break;
                // std.log.debug("bytes.len {} next_index[0] {} i {}", .{ iter.parser.bytes.len, next_index[0], i });
                c.* = printable_char(iter.parser.bytes[next_index[0] + i]);
            }
            break :blk pad_with_alloc(&buf2, ' ', LOG_SMALL_BUFFER_LEN, &fba.allocator);
        };
        print("| {s} ", .{next_content});

        if (current_index) |ci| {
            print("| {s} ", .{
                pad_with_alloc(
                    std.fmt.bufPrint(&buf2, "{}", .{ci[0]}) catch unreachable,
                    ' ',
                    LOG_INDEX_LEN,
                    &fba.allocator,
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

    pub fn object_begin(iter: *Iterator, visitor: *TapeBuilder) !void {
        iter.log_start_value("object");
        iter.depth += 1;

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
        return iter.object_field(visitor);
    }

    inline fn object_field(iter: *Iterator, visitor: *TapeBuilder) Error!void {
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
            } else return iter.object_begin(visitor),
            '[' => if (iter.peek()[0] == ']') {
                _ = iter.advance();
                iter.log_value("empty array");
                try visitor.visit_empty_array(iter);
            } else return iter.array_begin(visitor),
            else => try visitor.visit_primitive(iter, value),
        }
        return iter.object_continue(visitor);
    }

    inline fn object_continue(iter: *Iterator, visitor: *TapeBuilder) Error!void {
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
                return iter.object_field(visitor);
            },
            '}' => {
                iter.log_end_value("object");
                _ = try visitor.visit_object_end(iter);
                return iter.scope_end(visitor);
            },
            else => {
                iter.log_error("No comma between object fields");
                return error.TAPE_ERROR;
            },
        }
        unreachable;
    }

    inline fn scope_end(iter: *Iterator, visitor: *TapeBuilder) Error!void {
        // std.log.debug("scope_end iter.depth {}", .{iter.depth});
        iter.depth -= 1;
        if (iter.depth == 0) return iter.document_end(visitor);
        const is_array = iter.current_container().is_array;
        if (is_array)
            return iter.array_continue(visitor);

        return iter.object_continue(visitor);
    }

    inline fn array_begin(iter: *Iterator, visitor: *TapeBuilder) Error!void {
        iter.log_start_value("array");
        iter.depth += 1;
        if (iter.depth >= iter.parser.max_depth) {
            iter.log_error("Exceeded max depth!");
            return error.DEPTH_ERROR;
        }
        _ = try visitor.visit_array_start(iter);
        iter.increment_count();
        return iter.array_value(visitor);
    }

    inline fn array_value(iter: *Iterator, visitor: *TapeBuilder) Error!void {
        const value = iter.advance();
        switch (value[0]) {
            '{' => {
                if (iter.peek()[0] == '}') {
                    _ = iter.advance();
                    iter.log_value("empty object");
                    try visitor.visit_empty_object(iter);
                }
                return iter.object_begin(visitor);
            },
            '[' => {
                if (iter.peek()[0] == ']') {
                    _ = iter.advance();
                    iter.log_value("empty array");
                    try visitor.visit_empty_array(iter);
                }
                return iter.array_begin(visitor);
            },
            else => try visitor.visit_primitive(iter, value),
        }
        return iter.array_continue(visitor);
    }

    inline fn array_continue(iter: *Iterator, visitor: *TapeBuilder) Error!void {
        switch (iter.advance()[0]) {
            ',' => {
                iter.increment_count();
                return iter.array_value(visitor);
            },
            ']' => {
                iter.log_end_value("array");
                try visitor.visit_array_end(iter);
                return iter.scope_end(visitor);
            },
            else => {
                iter.log_error("Missing comma between array values");
                return error.TAPE_ERROR;
            },
        }
        unreachable;
    }

    inline fn document_end(iter: *Iterator, visitor: *TapeBuilder) Error!void {
        iter.log_end_value("document");
        try visitor.visit_document_end(iter);

        //   dom_parser.next_structural_index = uint32_t(next_structural - &dom_parser.structural_indexes[0]);

        // TODO
        // If we didn't make it to the end, it's an error
        if (!STREAMING and iter.parser.next_structural_index != iter.parser.indexer.bit_indexer.tail.items.len) {
            iter.log_error("More than one JSON value at the root of the document, or extra characters at the end of the JSON!");
            return error.TAPE_ERROR;
        }
    }

    inline fn current_container(iter: *Iterator) *OpenContainerInfo {
        // std.log.debug("current_container iter.parser.open_containers.len {} iter.depth {}", .{ iter.parser.open_containers.items.len, iter.depth });
        // return @ptrCast(
        //     *OpenContainerInfo,
        //     // iter.parser.open_containers.items(.open_container)[iter.depth..].ptr,
        //     iter.parser.open_containers.items[iter.depth..].ptr,
        // );
        return &iter.parser.open_containers.items[iter.depth];
        // return iter.parser.open_containers.set(iter.parser.open_containers.len - 1);
    }
    inline fn increment_count(iter: *Iterator) void {
        // we have a key value pair in the object at parser.dom_parser.depth - 1
        iter.current_container().open_container.count += 1;
    }

    pub fn walk_document(iter: *Iterator, visitor: *TapeBuilder) !void {
        iter.log_start();
        iter.log_start_value("document");
        try visitor.visit_document_start(iter);
        if (iter.parser.bytes.len == 0) return iter.document_end(visitor);

        const value = iter.advance();
        switch (value[0]) {
            '{' => {
                if (iter.peek()[0] == '}') {
                    _ = iter.advance();
                    iter.log_value("empty object");
                    try visitor.visit_empty_object(iter);
                } else return try iter.object_begin(visitor);
            },
            '[' => {
                if (iter.peek()[0] == ']') {
                    _ = iter.advance();
                    iter.log_value("empty array");
                    try visitor.visit_empty_array(iter);
                } else return try iter.array_begin(visitor);
            },
            else => try visitor.visit_root_primitive(iter, value),
        }
        return iter.document_end(visitor);
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
    TRUE_VALUE = 't',
    FALSE_VALUE = 'f',
    NULL_VALUE = 'n',
    INVALID = 'i',
    pub fn as_u64(tt: TapeType) u64 {
        return @as(u64, @enumToInt(tt)) << 56;
    }
    pub fn from_u64(x: u64) TapeType {
        return std.meta.intToEnum(TapeType, (x & 0xff00000000000000) >> 56) catch .INVALID;
    }
    pub fn as_u64_value(tt: TapeType, value: u64) u64 {
        assert(value <= std.math.maxInt(u56));
        return @as(u64, @enumToInt(tt)) << 56 | value;
    }
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

    pub inline fn append(tb: *TapeBuilder, iter: *Iterator, val: u64, tt: TapeType) !void {
        _ = iter;
        // iter.log_line_fmt("", "append", "val {} tt {}", .{ val, tt });
        try tb.tape.append(iter.parser.allocator, val | tt.as_u64());
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
        // iter.log_line_fmt("", "start_string", "iter.parser.doc.string_buf.items.len {}", .{iter.parser.doc.string_buf.items.len});
        try tb.append(iter, @ptrToInt(tb.current_string_buf_loc) - @ptrToInt(iter.parser.doc.string_buf.items.ptr), .STRING);
        return tb.current_string_buf_loc + @sizeOf(u32);
    }
    inline fn on_end_string(tb: *TapeBuilder, iter: *Iterator, dst: [*]const u8) !void {
        const str_length = try std.math.cast(
            u32,
            @ptrToInt(dst - @ptrToInt(tb.current_string_buf_loc + @sizeOf(u32))) / 8,
        );
        // TODO check for overflow in case someone has a crazy string (>=4GB?)
        // But only add the overflow check when the document itself exceeds 4GB
        // Currently unneeded because we refuse to parse docs larger or equal to 4GB.

        // @memcpy(current_string_buf_loc, &str_length, @sizeOf(u32));
        // // NULL termination is still handy if you expect all your strings to
        // // be NULL terminated? It comes at a small cost
        // dst.* = 0;
        // tb.current_string_buf_loc = dst + 1;
        // iter.log_line_fmt("", "end_string", "str {s}", .{str});
        try iter.parser.doc.string_buf.appendSlice(iter.parser.allocator, dst[0..str_length]);
    }

    pub inline fn append2(tb: *TapeBuilder, iter: *Iterator, val: u64, val2: anytype, tt: TapeType) !void {
        try tb.append(iter, val, tt);
        assert(@sizeOf(@TypeOf(val2)) == 8);
        //   static_assert(sizeof(val2) == sizeof(*next_tape_loc), "Type is not 64 bits!");

        //   memcpy(next_tape_loc, &val2, sizeof(val2));
        //   tb.append(iter, val2, t);
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
        if (!try AtomParsing.is_valid_true_atom(value)) return error.T_ATOM_ERROR;
        try tb.append(iter, 0, TapeType.TRUE_VALUE);
    }
    inline fn visit_false_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log_value("false");
        assert(value[0] == 'f');
        if (!try AtomParsing.is_valid_false_atom(value)) return error.T_ATOM_ERROR;
        try tb.append(iter, 0, TapeType.FALSE_VALUE);
    }
    inline fn visit_null_atom(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) Error!void {
        iter.log_value("null");
        assert(value[0] == 'n');
        if (!try AtomParsing.is_valid_null_atom(value)) return error.T_ATOM_ERROR;
        try tb.append(iter, 0, TapeType.NULL_VALUE);
    }

    inline fn visit_primitive(tb: *TapeBuilder, iter: *Iterator, value: [*]const u8) !void {
        return switch (value[0]) {
            '"' => tb.visit_string(iter, value, false),
            't' => tb.visit_true_atom(iter, value),
            'f' => tb.visit_false_atom(iter, value),
            'n' => tb.visit_null_atom(iter, value),
            '-', '0'...'9' => tb.visit_number(iter, value),
            else => blk: {
                iter.log_error("Non-value found when value was expected!");
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
    // n_structural_indexes: usize = 0,
    bytes: []u8 = &[_]u8{},

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
        const len = try parser.read_file(filename);
        try parser.doc.allocate(allocator, len);
        return parser;
    }

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
        const len = try std.math.cast(u32, input.len);
        const paddedlen = try std.math.add(u32, len, SIMDJSON_PADDING);
        parser.bytes = try parser.allocator.alloc(u8, paddedlen);
        mem.copy(u8, parser.bytes, input);

        // We write zeroes in the padded region to avoid having uninitized
        // garbage. If nothing else, garbage getting read might trigger a
        // warning in a memory checking.
        std.mem.set(u8, parser.bytes[len..], 0);
        try parser.doc.allocate(allocator, input.len);
        return parser;
    }

    fn read_file(parser: *Parser, filename: []const u8) !usize {
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
            std.mem.set(u8, parser.bytes[len..], 0);
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
        if (debug) println("{b:0>64} | prev_escaped a", .{@bitReverse(u64, parser.prev_escaped)});
        parser.prev_escaped = @boolToInt(@addWithOverflow(u64, odd_sequence_starts, backslash, &sequences_starting_on_even_bits));
        if (debug) println("{b:0>64} | prev_escaped b", .{@bitReverse(u64, parser.prev_escaped)});
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
        // if (debug) println("{b:0>64} | quote a", .{@bitReverse(u64, quote)});
        // if (debug) println("{b:0>64} | ones[0]", .{@bitReverse(u64, ones[0])});
        // if (debug) println("{b:0>64} | in_string a", .{@bitReverse(u64, in_string)});
        // if (debug) println("{b:0>64} | prev_in_string a", .{@bitReverse(u64, parser.prev_in_string)});
        in_string ^= parser.prev_in_string;
        // if (debug) println("{b:0>64} | in_string b", .{@bitReverse(u64, in_string)});

        //
        // Check if we're still in a string at the end of the box so the next block will know
        //
        // right shift of a signed value expected to be well-defined and standard
        // compliant as of C++20, John Regher from Utah U. says this is fine code
        //
        if (debug) println("{b:0>64} | prev_in_string a", .{@bitReverse(u64, parser.prev_in_string)});
        // if(debug) println("{b:0>64} | @bitCast(i64, in_string) ", .{@bitReverse(i64, @bitCast(i64, in_string))});
        // if(debug) println("{b:0>64} | @bitCast(i64, in_string) >> 63 ", .{@bitReverse(i64, @bitCast(i64, in_string) >> 63)});
        // if(debug) println("{b:0>64} | @bitCast(u64, @bitCast(i64, in_string) >> 63) ", .{@bitReverse(u64, @bitCast(u64, @bitCast(i64, in_string) >> 63))});
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
        const end_pos = parser.bytes.len;
        const end_pos_minus_step = if (end_pos > STEP_SIZE) end_pos - STEP_SIZE else 0;

        var pos: u32 = 0;
        while (pos < end_pos_minus_step) : (pos += STEP_SIZE) {
            // println("i {} pos {}", .{ i, pos });
            // _ = try parser.bytes.read(&read_buf);
            const read_buf = parser.bytes[pos..][0..STEP_SIZE];
            try parser.indexer.step(read_buf.*, parser, pos);
            // for (blocks) |block| {
            //     println("{b:0>64} | characters.whitespace", .{@bitReverse(u64, block.characters.whitespace)});
            //     println("{b:0>64} | characters.op", .{@bitReverse(u64, block.characters.op)});
            //     println("{b:0>64} | in_string", .{@bitReverse(u64, block.strings.in_string)});
            // }
        }
        var read_buf: [STEP_SIZE]u8 = undefined;
        std.mem.set(u8, &read_buf, 0x20);
        std.mem.copy(u8, &read_buf, parser.bytes[pos..]);
        println("final pos {} ", .{pos});
        try parser.indexer.step(read_buf, parser, pos);
        try parser.indexer.finish(parser, pos + 64, end_pos, STREAMING);
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
};

pub const log_level: std.log.Level = .debug;
const allr = testing.allocator;
pub fn main() !u8 {
    var parser: Parser = undefined;

    if (os.argv.len == 1) {
        var stdin = std.io.getStdIn().reader();
        const input = try stdin.readAllAlloc(allr, std.math.maxInt(u32));
        parser = try Parser.initFixedBuffer(allr, input, .{});
    } else if (os.argv.len == 2) {
        const filename = std.mem.span(os.argv[1]);
        parser = try Parser.initFile(allr, filename, .{});
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

test "tape build" {
    const input = @embedFile("../test/test.json");
    const expecteds = [_]u64{
        TapeType.ROOT.as_u64_value(37), //  pointing  to 37 (rightafter  last  node) :0
        TapeType.START_OBJECT.as_u64_value(37 | (8 << 32)), //  pointing  to next  tapelocation  37 (first  node  after  thescope) :1
        TapeType.STRING.as_u64(), // "Width" :2
        TapeType.INT64.as_u64(), 800, // :3
        TapeType.STRING.as_u64(), // "Height" :5
        TapeType.INT64.as_u64(), 600, // :6
        TapeType.STRING.as_u64(), // "Title" :8
        TapeType.STRING.as_u64(), // "View  from my room" :9
        TapeType.STRING.as_u64(), // "Url" :10
        TapeType.STRING.as_u64(), // "http://,ex.com/img.png" :11
        TapeType.STRING.as_u64(), // "Private" :12
        TapeType.FALSE_VALUE.as_u64(), // :13
        TapeType.STRING.as_u64(), // "Thumbnail" :14
        TapeType.START_OBJECT.as_u64_value(25 | (3 << 32)), //  pointing  to next  tapelocation  25 (first  node  after  thescope) :15
        TapeType.STRING.as_u64(), // "Url" :16
        TapeType.STRING.as_u64(), // "http://,ex.com/th.png". :17
        TapeType.STRING.as_u64(), // "Height" :18
        TapeType.INT64.as_u64(), 125, // :19
        TapeType.STRING.as_u64(), // "Width" :21
        TapeType.INT64.as_u64(), 100, // :22
        TapeType.END_OBJECT.as_u64_value(15), //  pointing  to  previous  tapelocation  15 (start  of the  scope) :24
        TapeType.STRING.as_u64(), // "array" :25
        TapeType.START_ARRAY.as_u64_value(34 | (3 << 32)), //  pointing  to next  tapelocation  34 (first  node  after  thescope) :26
        TapeType.INT64.as_u64(), 116, // :27
        TapeType.INT64.as_u64(), 943, // :29
        TapeType.INT64.as_u64(), 234, // :31
        TapeType.END_ARRAY.as_u64_value(26), //  pointing  to  previous  tapelocation  26 (start  of the  scope) :33
        TapeType.STRING.as_u64(), // "Owner" :34
        TapeType.NULL_VALUE.as_u64(), // :35
        TapeType.END_OBJECT.as_u64_value(1), //  pointing  to  previous  tapelocation 1 (start  of the  scope) :36
        TapeType.ROOT.as_u64(), //  pointing  to 0 (start  root) :37
    };
    testing.log_level = .debug;
    var parser = try Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();

    var i: usize = 0;
    while (i < expecteds.len) : (i += 1) {
        const expected = expecteds[i];
        const expected_type = TapeType.from_u64(expected);
        const expected_val = expected & 0x00ffffffffffffff;
        std.log.debug("{} : expected {s}:{}-{x}", .{ i, @tagName(expected_type), expected_val, expected });
        const tape_item = parser.doc.tape.items[i];
        const tape_type = TapeType.from_u64(tape_item);
        const tape_val = tape_item & 0x00ffffffffffffff;
        std.log.debug("{} : actual   {s}:{}-{x}", .{ i, @tagName(tape_type), tape_val, tape_item });
        // std.log.debug("actual {}:{}", .{expected_type, expected_val});
        // std.log.debug("expected 0x{x} tape_item 0x{x}", .{ expected, tape_item });
        try testing.expectEqual(expected_type, tape_type);

        // std.log.debug("expected_val {} tape_val {}", .{ expected_val, tape_val });
        if (expected_type != .STRING)
            try testing.expectEqual(expected_val, tape_val);

        if (expected_type == .INT64) {
            i += 1;
            try testing.expectEqual(expecteds[i], parser.doc.tape.items[i]);
        }
    }
}
