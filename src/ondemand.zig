const std = @import("std");
const mem = std.mem;
const os = std.os;
const assert = std.debug.assert;

usingnamespace @import("common.zig");
const dom = @import("dom.zig");
const Logger = @import("Logger.zig");
const number_parsing = @import("number_parsing.zig");
const string_parsing = @import("string_parsing.zig");
const atom_parsing = @import("atom_parsing.zig");
const CharUtils = string_parsing.CharUtils;

pub const Value = struct {
    iter: ValueIterator,
    pub fn find_field(v: *Value, key: []const u8) !Value {
        return (try v.start_or_resume_object()).find_field(key);
    }
    pub fn find_field_unordered(v: *Value, key: []const u8) !Value {
        return (try v.start_or_resume_object()).find_field_unordered(key);
    }
    pub fn get_object(v: *Value) !Object {
        return Object.start(&v.iter);
    }
    pub fn get_array(v: *Value) !Array {
        return Array.start(&v.iter);
    }
    fn start_or_resume_object(v: *Value) !Object {
        return if (v.iter.at_start())
            try v.get_object()
        else
            Object.resume_(&v.iter);
    }
    pub fn get_int(v: *Value, comptime T: type) !T {
        return v.iter.get_int(T);
    }
    pub fn get_string(v: *Value, buf: []u8) ![]const u8 {
        return v.iter.get_string(buf);
    }
    pub fn get_double(v: *Value) !f64 {
        return v.iter.get_double();
    }
    pub fn get_bool(v: *Value) !bool {
        return v.iter.get_bool();
    }
    pub fn is_null(v: *Value) !bool {
        return v.iter.is_null();
    }
};

pub const Field = struct {
    key: [*]const u8, // TODO: make this a []const u8
    value: Value,
};

pub const ObjectIterator = struct {
    iter: ValueIterator,
    fn init(iter: ValueIterator) ObjectIterator {
        return ObjectIterator{ .iter = iter };
    }
    /// if there is a next field, copies the unescaped key into key_buf
    /// and returns a new iterator at the key's value
    pub fn next(oi: *ObjectIterator, key_buf: []u8) !?Field {
        errdefer oi.iter.abandon();
        const has_value = if (oi.iter.at_first_field())
            true
        else if (!oi.iter.is_open())
            false
        else blk: {
            try oi.iter.skip_child();
            break :blk try oi.iter.has_next_field();
        };
        if (has_value) {
            ValueIterator.copy_key_without_quotes(key_buf, try oi.iter.field_key(key_buf.len), key_buf.len);
            try oi.iter.field_value();
            return Field{ .key = @ptrCast([*]const u8, &key_buf), .value = .{ .iter = oi.iter.child() } };
        }
        return null;
    }

    pub fn get_int(oi: *ObjectIterator, comptime T: type) !T {
        return oi.iter.get_int(T);
    }
};
pub const Object = struct {
    iter: ValueIterator,
    pub fn iterator(o: Object) ObjectIterator {
        return ObjectIterator.init(o.iter);
    }
    fn start_root(iter: *ValueIterator) !Object {
        _ = try iter.start_root_object();
        return Object{ .iter = iter.* };
    }
    fn start(iter: *ValueIterator) !Object {
        _ = try iter.start_object();
        return Object{ .iter = iter.* };
    }
    fn resume_(iter: *ValueIterator) Object {
        return Object{ .iter = iter.* };
    }

    pub fn find_field(o: *Object, key: []const u8) !Value {
        const has_value = try o.iter.find_field_raw(key);
        if (!has_value) return error.NO_SUCH_FIELD;
        return Value{ .iter = o.iter.child() };
    }

    pub fn find_field_unordered(o: *Object, key: []const u8) !Value {
        const has_value = try o.iter.find_field_unordered_raw(key);
        if (!has_value) return error.NO_SUCH_FIELD;
        return Value{ .iter = o.iter.child() };
    }
};

const ArrayIterator = struct {
    iter: ValueIterator,
    fn init(iter: ValueIterator) ArrayIterator {
        return ArrayIterator{ .iter = iter };
    }

    pub fn next(ai: *ArrayIterator) !?Value {
        errdefer ai.iter.abandon();
        const has_value = if (ai.iter.at_first_field())
            true
        else if (!ai.iter.is_open())
            false
        else blk: {
            try ai.iter.skip_child();
            break :blk try ai.iter.has_next_element();
        };
        if (has_value) {
            defer ai.iter.skip_child() catch {};
            // std.debug.print("ai.iter.iter.depth {} ai.iter.depth {}\n", .{ ai.iter.iter.depth, ai.iter.depth });
            // return ArrayIterator{ .iter = ai.iter.child() };
            return Value{ .iter = ai.iter.child() };
        }
        return null;
    }
    pub fn get_int(ai: *ArrayIterator, comptime T: type) !T {
        return ai.iter.get_int(T);
    }
};
const Array = struct {
    iter: ValueIterator,
    fn start_root(iter: *ValueIterator) !Array {
        _ = try iter.start_root_array();
        return Array{ .iter = iter.* };
    }
    fn start(iter: *ValueIterator) !Array {
        _ = try iter.start_array();
        return Array{ .iter = iter.* };
    }
    pub fn iterator(o: Array) ArrayIterator {
        return ArrayIterator.init(o.iter);
    }
};
const TokenIterator = struct {
    src: std.io.StreamSource,
    buf: [mem.page_size]u8 = undefined,
    /// result of src.read() - number of bytes read from src
    buf_len: u16 = 0,
    /// src index of the start of the buffer.  set to index[0] each src.read()
    buf_start_pos: u32,
    /// src index
    index: [*]const u32,

    pub fn peek_delta(ti: *TokenIterator, delta: i32, len: u16) ![*]const u8 {
        return ti.peek(
            if (delta < 0)
                ti.index - @intCast(u32, -delta)
            else
                ti.index + @intCast(u32, delta),
            len,
        );
    }

    /// position: pointer to a src position
    /// len_hint: requested length to be read from src, starting at position
    pub fn peek(ti: *TokenIterator, position: [*]const u32, len_hint: u16) ![*]const u8 {
        if (len_hint > ti.buf.len) return error.CAPACITY;
        const start = position[0];
        // const end = start + len;
        // println("\nTokenIterator: {} <= start {} end {} < ti.buf_start_pos + len {}", .{ ti.buf_start_pos, start, end, ti.buf_start_pos + ti.buf_len });
        if (ti.buf_start_pos <= start and start < ti.buf_start_pos + ti.buf_len)
            return @ptrCast([*]const u8, &ti.buf) + (start - ti.buf_start_pos);
        // println("TokenIterator: seek and read()", .{});
        try ti.src.seekTo(start);
        ti.buf_start_pos = start;
        ti.buf_len = @truncate(u16, try ti.src.read(&ti.buf));
        return &ti.buf;
    }
};
pub const Iterator = struct {
    token: TokenIterator,
    parser: *Parser,
    err: Error!void = {},
    depth: u32,
    log: Logger = .{ .depth = 0 },

    pub fn init(parser: *Parser, src: std.io.StreamSource) Iterator {
        return .{
            .token = .{ .src = src, .index = parser.structural_indices().ptr, .buf_start_pos = 0 },
            .parser = parser,
            .depth = 1,
        };
    }

    pub fn advance(iter: *Iterator, len: u16) ![*]const u8 {
        defer iter.token.index += 1;
        // print("advance '{s}'\n", .{(try iter.token.peek(iter.token.index, len))[0..len]});
        return iter.token.peek(iter.token.index, len);
    }
    pub fn peek(iter: *Iterator, index: [*]const u32, len: u16) ![*]const u8 {
        return iter.token.peek(index, len);
    }
    pub fn peek_delta(iter: *Iterator, delta: i32, len: u16) ![*]const u8 {
        return iter.token.peek_delta(delta, len);
    }
    pub fn last_document_position(iter: Iterator) [*]const u32 {
        // The following line fails under some compilers...
        // SIMDJSON_ASSUME(parser.implementation.n_structural_indexes > 0);
        // since it has side-effects.
        const n_structural_indexes = iter.parser.parser.n_structural_indexes;
        assert(n_structural_indexes > 0);
        const result = iter.parser.structural_indices().ptr + (n_structural_indexes - 1);
        // println("tail.items {any} n_structural_indexes {} result {}\n", .{ iter.parser.structural_indices(), n_structural_indexes, result[0] });
        return result;
    }

    pub fn peek_last(iter: *Iterator) ![*]const u8 {
        return iter.token.peek(iter.last_document_position(), 1);
    }
    pub fn root_checkpoint(iter: Iterator) [*]const u32 {
        return iter.parser.structural_indices().ptr;
        //   return iter.parser.structural_indexes.get();

    }
    pub fn skip_child(iter: *Iterator, parent_depth: u32) !void {
        if (iter.depth <= parent_depth) return;

        switch ((try iter.advance(1))[0]) {
            // TODO consider whether matching braces is a requirement: if non-matching braces indicates
            // *missing* braces, then future lookups are not in the object/arrays they think they are,
            // violating the rule "validate enough structure that the user can be confident they are
            // looking at the right values."
            // PERF TODO we can eliminate the switch here with a lookup of how much to add to depth

            // For the first open array/object in a value, we've already incremented depth, so keep it the same
            // We never stop at colon, but if we did, it wouldn't affect depth
            '[', '{', ':' => {
                iter.log.start_value(iter, "skip");
            },
            // If there is a comma, we have just finished a value in an array/object, and need to get back in
            ',' => {
                iter.log.value(iter, "skip");
            },
            // ] or } means we just finished a value and need to jump out of the array/object
            ']', '}' => {
                iter.log.end_value(iter, "skip");
                iter.depth -= 1;
                if (iter.depth <= parent_depth) return;
            },
            // Anything else must be a scalar value
            else => {
                // For the first scalar, we will have incremented depth already, so we decrement it here.
                iter.log.value(iter, "skip");
                iter.depth -= 1;
                if (iter.depth <= parent_depth) return;
            },
        }

        // Now that we've considered the first value, we only increment/decrement for arrays/objects
        // const end = @ptrToInt(iter.parser.structural_indices().ptr + iter.parser.parser.n_structural_indexes);
        const end_idx = iter.parser.structural_indices()[iter.parser.parser.n_structural_indexes];
        while (iter.token.index[0] <= end_idx) {
            switch ((try iter.advance(1))[0]) {
                '[', '{' => {
                    iter.log.start_value(iter, "skip");
                    iter.depth += 1;
                },
                // TODO consider whether matching braces is a requirement: if non-matching braces indicates
                // *missing* braces, then future lookups are not in the object/arrays they think they are,
                // violating the rule "validate enough structure that the user can be confident they are
                // looking at the right values."
                // PERF TODO we can eliminate the switch here with a lookup of how much to add to depth
                ']', '}' => {
                    iter.log.end_value(iter, "skip");
                    iter.depth -= 1;
                    if (iter.depth <= parent_depth) return;
                },
                else => {
                    iter.log.value(iter, "skip");
                },
            }
        }

        return iter.report_error(error.TAPE_ERROR, "not enough close braces");
    }

    fn report_error(iter: *Iterator, err: Error, message: []const u8) Error {
        _ = message;
        _ = iter;
        assert(err != error.UNINITIALIZED and err != error.INCORRECT_TYPE and err != error.NO_SUCH_FIELD);
        iter.log.err(iter, message);

        return err;
    }

    fn ascend_to(iter: *Iterator, parent_depth: u32) void {
        assert(parent_depth >= 0 and parent_depth < std.math.maxInt(i32) - 1);
        assert(iter.depth == parent_depth + 1);
        iter.depth = parent_depth;
    }
    fn descend_to(iter: *Iterator, child_depth: u32) void {
        assert(child_depth >= 1 and child_depth < std.math.maxInt(i32));
        assert(iter.depth == child_depth - 1);
        iter.depth = child_depth;
    }

    fn abandon(iter: *Iterator) void {
        // iter.parser.deinit();
        // iter.parser = undefined;
        iter.depth = 0;
    }

    pub fn at_beginning(iter: *Iterator) bool {
        // std.log.debug("at-beginning {*}: {}", .{ iter.next_structural, iter.next_structural[0] });
        return iter.token.index == iter.parser.structural_indices().ptr;
    }

    pub inline fn next_structural(iter: *Iterator) [*]u32 {
        // std.log.debug("at-beginning {*}: {}", .{ iter.next_structural, iter.next_structural[0] });
        return @intToPtr([*]u32, @ptrToInt(iter.token.index));
    }

    fn reenter_child(iter: *Iterator, position: [*]const u32, child_depth: u32) void {
        assert(child_depth >= 1 and child_depth < std.math.maxInt(i32));
        assert(iter.depth == child_depth - 1);
        // assert(@ptrToInt(position) >= @ptrToInt(iter.parser.start_positions[child_depth]));
        iter.token.index = position;
        iter.depth = child_depth;
    }

    pub fn string_buf_loc(iter: Iterator) [*]const u8 {
        return @ptrCast([*]const u8, &iter.token.buf) + iter.token.index[0] - iter.token.buf_start_pos;
    }
};
const ValueIterator = struct {
    iter: Iterator, // this needs to be a value, not a pointer
    depth: u32,
    start_position: [*]const u32,

    pub fn init(iter: Iterator, depth: u32, start_position: [*]const u32) ValueIterator {
        return .{
            .iter = iter,
            .depth = depth,
            .start_position = start_position,
        };
    }
    // pub fn at_key(vi: ValueIterator, key: []const u8) Value {
    //     return doc.resume_value_iterator().at_key(key);
    // }
    fn parse_null(json: [*]const u8) bool {
        return !atom_parsing.is_valid_atom(json, 4, atom_parsing.atom_null) and CharUtils.is_structural_or_whitespace(json[4]);
    }
    fn parse_bool(vi: *ValueIterator, json: [*]const u8) !bool {
        const not_true = !(atom_parsing.is_valid_atom(json, 4, atom_parsing.atom_true));
        const not_false = !(atom_parsing.is_valid_atom(json, 4, atom_parsing.atom_fals) and json[4] == 'e');
        const err = (not_true and not_false) or
            CharUtils.is_not_structural_or_whitespace(json[if (not_true) 5 else 4]);
        if (err) return vi.incorrect_type_error("Not a boolean");
        return !not_true;
    }

    pub fn is_null(vi: *ValueIterator) !bool {
        return parse_null(try vi.advance_non_root_scalar("null", 4));
    }
    fn is_at_start(vi: ValueIterator) bool {
        return vi.iter.token.index == vi.start_position;
    }
    fn peek_start(vi: *ValueIterator, len: u16) ![*]const u8 {
        return try vi.iter.peek(vi.start_position, len);
    }
    fn assert_at_start(vi: ValueIterator) void {
        assert(vi.iter.token.index == vi.start_position);
        assert(vi.iter.depth == vi.depth);
        assert(vi.depth > 0);
    }
    fn assert_at_child(vi: ValueIterator) void {
        assert(@ptrToInt(vi.iter.token.index) > @ptrToInt(vi.start_position));
        assert(vi.iter.depth == vi.depth + 1);
        assert(vi.depth > 0);
    }
    fn assert_at_non_root_start(vi: ValueIterator) void {
        vi.assert_at_start();
        assert(vi.depth > 1);
    }
    fn ascend_to(vi: *ValueIterator, parent_depth: u32) void {
        assert(parent_depth >= 0 and parent_depth < std.math.maxInt(i32) - 1);
        assert(vi.depth == parent_depth + 1);
        vi.depth = parent_depth;
    }
    fn advance_non_root_scalar(vi: *ValueIterator, typ: []const u8, peek_len: u16) ![*]const u8 {
        vi.iter.log.value2(&vi.iter, typ, "", vi.start_position[0], vi.depth);
        if (!vi.is_at_start())
            return vi.peek_start(peek_len);

        vi.assert_at_non_root_start();
        const result = try vi.iter.advance(peek_len);
        vi.ascend_to(vi.depth - 1);
        return result;
    }

    fn is_open(vi: ValueIterator) bool {
        return vi.iter.depth >= vi.depth;
    }
    fn skip_child(vi: *ValueIterator) !void {
        assert(@ptrToInt(vi.iter.token.index) > @ptrToInt(vi.start_position));
        assert(vi.iter.depth >= vi.depth);

        return vi.iter.skip_child(vi.depth);
    }

    fn has_next_field(vi: *ValueIterator) !bool {
        vi.assert_at_next();

        switch ((try vi.iter.advance(1))[0]) {
            '}' => {
                vi.iter.log.end_value(&vi.iter, "object");
                vi.iter.ascend_to(vi.depth - 1);
                return false;
            },
            ',' => return true,
            else => return vi.iter.report_error(error.TAPE_ERROR, "Missing comma between object fields"),
        }
    }

    fn has_next_element(vi: *ValueIterator) !bool {
        vi.assert_at_next();

        switch ((try vi.iter.advance(1))[0]) {
            ']' => {
                vi.iter.log.end_value(&vi.iter, "array");
                vi.iter.ascend_to(vi.depth - 1);
                return false;
            },
            ',' => {
                vi.iter.descend_to(vi.depth + 1);
                return true;
            },
            else => return vi.iter.report_error(error.TAPE_ERROR, "Missing comma between array elements"),
        }
    }
    fn start_root_object(vi: *ValueIterator) !bool {
        var result = try vi.start_object();
        const last_char = (try vi.iter.peek_last())[0];
        if (last_char != '}')
            return vi.iter.report_error(error.TAPE_ERROR, "object invalid: { at beginning of document unmatched by } at end of document");
        return result;
    }
    fn start_root_array(vi: *ValueIterator) !bool {
        var result = try vi.start_array();
        const last_char = (try vi.iter.peek_last())[0];
        if (last_char != ']')
            return vi.iter.report_error(error.TAPE_ERROR, "array invalid: [ at beginning of document unmatched by ] at end of document");
        return result;
    }
    fn advance_container_start(vi: *ValueIterator, typ: []const u8, peek_len: u16) ![*]const u8 {
        vi.iter.log.line_fmt(&vi.iter, "", typ, "start pos {} depth {}", .{ vi.start_position[0], vi.depth });

        // If we're not at the position anymore, we don't want to advance the cursor.
        if (!vi.is_at_start()) {
            // #ifdef SIMDJSON_DEVELOPMENT_CHECKS
            //     if (!vi.is_at_iterator_start()) { return OUT_OF_ORDER_ITERATION; }
            // #endif
            return vi.peek_start(peek_len);
        }

        // Get the JSON and advance the cursor, decreasing depth to signify that we have retrieved the value.
        vi.assert_at_start();
        return vi.iter.advance(peek_len);
    }
    fn incorrect_type_error(vi: *ValueIterator, message: []const u8) Error {
        _ = message;
        vi.iter.log.err_fmt(&vi.iter, "{s}.  start_position {} depth {}", .{ message, vi.start_position[0], vi.depth });
        return error.INCORRECT_TYPE;
    }
    fn assert_at_container_start(vi: ValueIterator) void {
        assert(vi.iter.token.index == vi.start_position + 1);
        assert(vi.iter.depth == vi.depth);
        assert(vi.depth > 0);
    }

    fn assert_at_next(vi: ValueIterator) void {
        assert(@ptrToInt(vi.iter.token.index) > @ptrToInt(vi.start_position));
        assert(vi.iter.depth == vi.depth);
        assert(vi.depth > 0);
    }

    fn started_object(vi: *ValueIterator) !bool {
        vi.assert_at_container_start();
        if ((try vi.iter.peek_delta(0, 1))[0] == '}') {
            vi.iter.log.value(&vi.iter, "empty object");
            _ = try vi.iter.advance(0);
            vi.iter.ascend_to(vi.depth - 1);
            return false;
        }
        vi.iter.log.start_value(&vi.iter, "object");
        // #ifdef SIMDJSON_DEVELOPMENT_CHECKS
        //   vi.iter.set_start_position(_depth, _start_position);
        // #endif
        return true;
    }

    fn started_array(vi: *ValueIterator) !bool {
        vi.assert_at_container_start();
        if ((try vi.iter.peek_delta(0, 1))[0] == ']') {
            vi.iter.log.value(&vi.iter, "empty array");
            _ = try vi.iter.advance(0);
            vi.iter.ascend_to(vi.depth - 1);
            return false;
        }
        vi.iter.log.start_value(&vi.iter, "array");

        vi.iter.descend_to(vi.depth + 1);
        // #ifdef SIMDJSON_DEVELOPMENT_CHECKS
        // _json_iter->set_start_position(_depth, _start_position);
        // #endif
        return true;
    }

    fn start_object(vi: *ValueIterator) !bool {
        var json: [*]const u8 = try vi.advance_container_start("object", 1);
        if (json[0] != '{')
            return vi.incorrect_type_error("Not an object");
        return vi.started_object();
    }

    fn start_array(vi: *ValueIterator) !bool {
        var json: [*]const u8 = try vi.advance_container_start("array", 1);
        if (json[0] != '[')
            return vi.incorrect_type_error("Not an array");
        return vi.started_array();
    }

    fn field_key(vi: *ValueIterator, key_len: usize) ![*]const u8 {
        vi.assert_at_next();
        var key = try vi.iter.advance(try std.math.cast(u16, key_len));
        if (key[0] != '"')
            return vi.iter.report_error(error.TAPE_ERROR, "Object key is not a string");
        return key;
    }
    fn field_value(vi: *ValueIterator) !void {
        vi.assert_at_next();

        if ((try vi.iter.advance(1))[0] != ':')
            return vi.iter.report_error(error.TAPE_ERROR, "Missing colon in object field");
        vi.iter.descend_to(vi.depth + 1);
    }
    pub fn find_field(vi: ValueIterator, key: []const u8) Value {
        return vi.start_or_resume_object().find_field(key);
    }

    fn at_start(vi: ValueIterator) bool {
        return vi.iter.token.index == vi.start_position;
    }
    fn at_first_field(vi: ValueIterator) bool {
        assert(@ptrToInt(vi.iter.token.index) > @ptrToInt(vi.start_position));
        return vi.iter.token.index == vi.start_position + 1;
    }

    fn abandon(vi: *ValueIterator) void {
        vi.iter.abandon();
    }
    pub fn copy_key_with_quotes(key_buf: []u8, key: [*]const u8, key_len: usize) void {
        mem.copy(u8, key_buf, key[0..std.math.min(key_len + 2, key_buf.len)]);
        // return key_buf[0 .. key_len + 2];
    }
    fn copy_key_without_quotes(key_buf: []u8, key: [*]const u8, key_len: usize) void {
        mem.copy(u8, key_buf, key[1..std.math.min(key_len, key_buf.len)]);
        // const end = string_parsing.parse_string(key + 1, key_buf.ptr);
        // const len = try ptr_diff(u8, end.?, key_buf.ptr);
        // return key_buf[0..len];
    }
    fn find_field_raw(vi: *ValueIterator, key: []const u8) !bool {
        // println("find_field_raw vi.depth {} vi.iter.depth {}", .{ vi.depth, vi.iter.depth });
        var has_value = false;
        // TODO add build option for this key buffer length
        var key_buf: [1024]u8 = undefined;
        errdefer vi.abandon();
        //
        // Initially, the object can be in one of a few different places:
        //
        // 1. The start of the object, at the first field:
        //
        //    ```
        //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
        //      ^ (depth 2, index 1)
        //    ```
        //
        if (vi.at_first_field()) {
            has_value = true;

            //
            // 2. When a previous search did not yield a value or the object is empty:
            //
            //    ```
            //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
            //                                     ^ (depth 0)
            //    { }
            //        ^ (depth 0, index 2)
            //    ```
            //
        } else if (!vi.is_open()) {
            // #ifdef SIMDJSON_DEVELOPMENT_CHECKS
            //     // If we're past the end of the object, we're being iterated out of order.
            //     // Note: this isn't perfect detection. It's possible the user is inside some other object; if so,
            //     // this object iterator will blithely scan that object for fields.
            //     if (vi.iter.depth() < depth() - 1) { return OUT_OF_ORDER_ITERATION; }
            // #endif
            has_value = false;

            // 3. When a previous search found a field or an iterator yielded a value:
            //
            //    ```
            //    // When a field was not fully consumed (or not even touched at all)
            //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
            //           ^ (depth 2)
            //    // When a field was fully consumed
            //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
            //                   ^ (depth 1)
            //    // When the last field was fully consumed
            //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
            //                                   ^ (depth 1)
            //    ```
            //
        } else {
            try vi.skip_child();
            has_value = try vi.has_next_field();
            // #ifdef SIMDJSON_DEVELOPMENT_CHECKS
            //     if (vi.iter.start_position(_depth) != _start_position) { return OUT_OF_ORDER_ITERATION; }
            // #endif
        }

        while (has_value) {
            // Get the key and colon, stopping at the value.

            if (key.len + 2 > key_buf.len) return error.STRING_ERROR;
            copy_key_with_quotes(&key_buf, try vi.field_key(try std.math.cast(u16, key.len)), key.len);

            println("actual_key '{s}'", .{key_buf[0..key.len]});

            // size_t max_key_length = vi.iter.peek_length() - 2; // -2 for the two quotes
            // if ((error = field_key().get(actual_key) )) { abandon(); return error; };

            try vi.field_value();
            // If it matches, stop and return
            // We could do it this way if we wanted to allow arbitrary
            // key content (including escaped quotes).
            //if (actual_key.unsafe_is_equal(max_key_length, key)) {
            // Instead we do the following which may trigger buffer overruns if the
            // user provides an adversarial key (containing a well placed unescaped quote
            // character and being longer than the number of bytes remaining in the JSON
            // input).
            if (unsafe_is_equal(&key_buf, key)) {
                vi.iter.log.event(&vi.iter, "match ", key, -2, 0);
                return true;
            }

            // No match: skip the value and see if , or } is next
            vi.iter.log.event(&vi.iter, "no match ", key, -2, 0);
            try vi.skip_child(); // Skip the value entirely
            has_value = try vi.has_next_field();
        }

        // If the loop ended, we're out of fields to look at.
        return false;
    }

    fn find_field_unordered_raw(vi: *ValueIterator, key: []const u8) !bool {
        errdefer vi.abandon();
        var has_value: bool = undefined;
        var key_buf: [1024]u8 = undefined;
        //
        // Initially, the object can be in one of a few different places:
        //
        // 1. The start of the object, at the first field:
        //
        //    ```
        //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
        //      ^ (depth 2, index 1)
        //    ```
        //
        if (vi.at_first_field()) {
            // If we're at the beginning of the object, we definitely have a field
            has_value = true;

            // 2. When a previous search did not yield a value or the object is empty:
            //
            //    ```
            //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
            //                                     ^ (depth 0)
            //    { }
            //        ^ (depth 0, index 2)
            //    ```
            //
        } else if (!vi.is_open()) {
            // #ifdef SIMDJSON_DEVELOPMENT_CHECKS
            //     // If we're past the end of the object, we're being iterated out of order.
            //     // Note: this isn't perfect detection. It's possible the user is inside some other object; if so,
            //     // this object iterator will blithely scan that object for fields.
            //     if (vi.iter.depth() < depth() - 1) { return OUT_OF_ORDER_ITERATION; }
            // #endif
            has_value = false;

            // 3. When a previous search found a field or an iterator yielded a value:
            //
            //    ```
            //    // When a field was not fully consumed (or not even touched at all)
            //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
            //           ^ (depth 2)
            //    // When a field was fully consumed
            //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
            //                   ^ (depth 1)
            //    // When the last field was fully consumed
            //    { "a": [ 1, 2 ], "b": [ 3, 4 ] }
            //                                   ^ (depth 1)
            //    ```
            //
        } else {
            // Finish the previous value and see if , or } is next
            try vi.skip_child();
            has_value = try vi.has_next_field();
            // #ifdef SIMDJSON_DEVELOPMENT_CHECKS
            //     if (vi.iter.start_position(_depth) != _start_position) { return OUT_OF_ORDER_ITERATION; }
            // #endif
        }

        // After initial processing, we will be in one of two states:
        //
        // ```
        // // At the beginning of a field
        // { "a": [ 1, 2 ], "b": [ 3, 4 ] }
        //   ^ (depth 1)
        // { "a": [ 1, 2 ], "b": [ 3, 4 ] }
        //                  ^ (depth 1)
        // // At the end of the object
        // { "a": [ 1, 2 ], "b": [ 3, 4 ] }
        //                                  ^ (depth 0)
        // ```
        //

        // First, we scan from that point to the end.
        // If we don't find a match, we loop back around, and scan from the beginning to that point.
        const search_start = vi.iter.token.index;

        // Next, we find a match starting from the current position.
        while (has_value) {
            assert(vi.iter.depth == vi.depth); // We must be at the start of a field

            // Get the key and colon, stopping at the value.
            // size_t max_key_length = vi.iter.peek_length() - 2; // -2 for the two quotes

            if (key.len + 2 > key_buf.len) return error.STRING_ERROR;
            copy_key_with_quotes(&key_buf, try vi.field_key(), key.len);

            try vi.field_value();

            // If it matches, stop and return
            // We could do it this way if we wanted to allow arbitrary
            // key content (including escaped quotes).
            // if (actual_key.unsafe_is_equal(max_key_length, key)) {
            // Instead we do the following which may trigger buffer overruns if the
            // user provides an adversarial key (containing a well placed unescaped quote
            // character and being longer than the number of bytes remaining in the JSON
            // input).
            if (unsafe_is_equal(&key_buf, key)) {
                vi.iter.log.event(&vi.iter, "match ", key, -2, 0);
                return true;
            }

            // No match: skip the value and see if , or } is next
            vi.iter.log.event(&vi.iter, "no match ", key, -2, 0);
            try vi.skip_child();
            has_value = try vi.has_next_field();
        }

        // If we reach the end without finding a match, search the rest of the fields starting at the
        // beginning of the object.
        // (We have already run through the object before, so we've already validated its structure. We
        // don't check errors in this bit.)
        vi.iter.reenter_child(vi.start_position + 1, vi.depth);

        has_value = try vi.started_object();
        while (@ptrToInt(vi.iter.token.index) < @ptrToInt(search_start)) {
            assert(has_value); // we should reach search_start before ever reaching the end of the object
            assert(vi.iter.depth == vi.depth); // We must be at the start of a field

            // Get the key and colon, stopping at the value.
            // size_t max_key_length = vi.iter.peek_length() - 2; // -2 for the two quotes

            if (key.len + 2 > key_buf.len) return error.STRING_ERROR;
            copy_key_with_quotes(&key_buf, try vi.field_key(), key.len);

            vi.field_value() catch unreachable;

            // If it matches, stop and return
            // We could do it this way if we wanted to allow arbitrary
            // key content (including escaped quotes).
            // if (actual_key.unsafe_is_equal(max_key_length, key)) {
            // Instead we do the following which may trigger buffer overruns if the
            // user provides an adversarial key (containing a well placed unescaped quote
            // character and being longer than the number of bytes remaining in the JSON
            // input).
            if (unsafe_is_equal(&key_buf, key)) {
                vi.iter.log.event(&vi.iter, "match ", key, -2, 0);
                return true;
            }

            // No match: skip the value and see if , or } is next
            vi.iter.log.event(&vi.iter, "no match ", key, -2, 0);
            try vi.skip_child();
            has_value = try vi.has_next_field();
        }

        // If the loop ended, we're out of fields to look at.
        return false;
    }

    fn child(vi: ValueIterator) ValueIterator {
        vi.assert_at_child();
        return ValueIterator.init(vi.iter, vi.depth + 1, vi.iter.token.index);
    }

    pub fn get_int(vi: *ValueIterator, comptime T: type) !T {
        const peek_len = comptime try std.math.cast(u16, std.math.log10(@as(usize, std.math.maxInt(T))));
        const u64int = try number_parsing.parse_integer(
            try vi.advance_non_root_scalar(@typeName(T), peek_len),
        );
        return std.math.cast(T, if (@typeInfo(T).Int.signedness == .signed)
            @bitCast(i64, u64int)
        else
            u64int);
    }

    pub fn unescape(src: [*]const u8, dst: [*]u8) ![]const u8 {
        const end = string_parsing.parse_string(src, dst) orelse return error.STRING_ERROR;
        const len = try ptr_diff(u32, end, dst);
        return dst[0..len];
    }

    pub fn get_string(vi: *ValueIterator, dest: []u8) ![]const u8 {
        return unescape(try vi.get_raw_json_string(try std.math.cast(u16, dest.len)), dest.ptr);
    }

    fn advance_start(vi: *ValueIterator, typ: []const u8, buf_len: u16) ![*]const u8 {
        vi.iter.log.value2(&vi.iter, typ, "", vi.start_position[0], vi.depth);
        // If we're not at the position anymore, we don't want to advance the cursor.
        if (!vi.is_at_start()) return vi.peek_start(buf_len);

        // Get the JSON and advance the cursor, decreasing depth to signify that we have retrieved the value.
        vi.assert_at_start();
        const result = try vi.iter.advance(buf_len);
        vi.iter.ascend_to(vi.depth - 1);
        return result;
    }

    pub fn get_raw_json_string(vi: *ValueIterator, buf_len: u16) ![*]const u8 {
        const json = try vi.advance_start("string", buf_len);
        if (json[0] != '"')
            return vi.incorrect_type_error("Not a string");
        return json + 1;
    }

    pub fn get_double(vi: *ValueIterator) !f64 {
        return number_parsing.parse_double(try vi.advance_non_root_scalar("double", 40));
    }
    pub fn get_bool(vi: *ValueIterator) !bool {
        return vi.parse_bool(try vi.advance_non_root_scalar("bool", 5));
    }
};

fn unsafe_is_equal(a: [*]const u8, target: []const u8) bool {
    // Assumptions: does not contain unescaped quote characters, and
    // the raw content is quote terminated within a valid JSON string.
    if (target.len <= SIMDJSON_PADDING)
        return (a[target.len + 1] == '"') and mem.eql(u8, a[1 .. target.len + 1], target);

    var r = a;
    var pos: usize = 0;
    while (pos < target.len) : (pos += 1) {
        if (r[pos] != target[pos]) {
            return false;
        }
    }
    if (r[pos] != '"') {
        return false;
    }
    return true;
}

pub const Document = struct {
    iter: Iterator,
    pub const DOCUMENT_DEPTH = 0;

    inline fn get_root_value_iterator(doc: *Document) ValueIterator {
        return doc.resume_value_iterator();
    }
    inline fn resume_value_iterator(doc: *Document) ValueIterator {
        return ValueIterator.init(doc.iter, 1, doc.iter.root_checkpoint());
    }
    pub fn at_key(doc: *Document, key: []const u8) Value {
        return doc.resume_value_iterator().at_key(key);
    }
    pub fn get_object(doc: *Document) !Object {
        var value = doc.get_root_value_iterator();
        return try Object.start_root(&value);
    }
    pub fn get_array(doc: *Document) !Array {
        var value = doc.get_root_value_iterator();
        return Array.start_root(&value);
    }
    fn resume_value(doc: *Document) Value {
        return Value{ .iter = doc.resume_value_iterator() };
    }
    pub fn find_field(doc: *Document, key: []const u8) !Value {
        return doc.resume_value().find_field(key);
    }

    pub fn find_field_unordered(doc: *Document, key: []const u8) !Value {
        return doc.resume_value().find_field_unordered(key);
    }
};

pub const Parser = struct {
    parser: dom.Parser,
    src: *std.io.StreamSource,
    iter: Iterator,
    end_pos: u32,

    pub fn init(src: *std.io.StreamSource, allocator: *mem.Allocator, filename: []const u8, options: dom.Parser.Options) !Parser {
        var result = Parser{
            .parser = .{
                .allocator = allocator,
                .filename = filename,
                .doc = dom.Document.init(),
                .indexer = try dom.StructuralIndexer.init(),
                .open_containers = std.MultiArrayList(dom.OpenContainerInfo){},
                .max_depth = options.max_depth,
            },
            .src = src,
            .iter = undefined,
            .end_pos = try std.math.cast(u32, try src.getEndPos()),
        };
        const capacity = result.end_pos;
        const max_structures = ROUNDUP_N(capacity, 64) + 2 + 7;
        try result.parser.indexer.bit_indexer.tail.ensureTotalCapacity(allocator, max_structures);
        try result.parser.open_containers.ensureTotalCapacity(allocator, options.max_depth);
        return result;
    }

    pub fn deinit(p: *Parser) void {
        p.parser.deinit();
    }

    pub fn stage1(p: *Parser) !void {
        var pos: u32 = 0;
        var read_buf: [step_size]u8 = undefined;
        var bytes_read: u32 = undefined;
        println("", .{});
        while (true) : (pos += step_size) {
            // println("i {} pos {}", .{ i, pos });
            bytes_read = @intCast(u32, try p.src.read(&read_buf));
            if (bytes_read < step_size) break;

            try p.parser.indexer.step(read_buf, &p.parser, pos);
            // for (blocks) |block| {
            //     println("{b:0>64} | characters.whitespace", .{@bitReverse(u64, block.characters.whitespace)});
            //     println("{b:0>64} | characters.op", .{@bitReverse(u64, block.characters.op)});
            //     println("{b:0>64} | in_string", .{@bitReverse(u64, block.strings.in_string)});
            // }
        }
        std.mem.set(u8, read_buf[bytes_read..], 0x20);
        // std.log.debug("read_buf {d}", .{read_buf});
        try p.parser.indexer.step(read_buf, &p.parser, pos);
        try p.parser.indexer.finish(&p.parser, pos + step_size, pos + bytes_read, STREAMING);
    }

    pub fn iterate(p: *Parser) !Document {
        try p.stage1();
        return Document{
            .iter = Iterator.init(p, p.src.*),
        };
    }

    inline fn structural_indices(parser: Parser) []u32 {
        return parser.parser.indexer.bit_indexer.tail.items;
    }
};
