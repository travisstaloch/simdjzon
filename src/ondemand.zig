const std = @import("std");
const mem = std.mem;
const os = std.os;
const assert = std.debug.assert;

const cmn = @import("common.zig");
const dom = @import("dom.zig");
const Logger = @import("Logger.zig");
const number_parsing = @import("number_parsing.zig");
const string_parsing = @import("string_parsing.zig");
const atom_parsing = @import("atom_parsing.zig");
const CharUtils = string_parsing.CharUtils;
const root = @import("root");
const builtin = @import("builtin");
// TODO: document that this is configurable in root
pub const READ_BUF_CAP = if (@hasDecl(root, "read_buf_cap"))
    root.read_buf_cap
else if (builtin.is_test)
    mem.page_size
else
    unreachable;

const GetOptions = struct {
    allocator: ?mem.Allocator = null,
};

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
    pub fn get_string(v: *Value, comptime T: type, buf: []u8) !T {
        return v.iter.get_string(T, buf);
    }
    pub fn get_string_alloc(v: *Value, comptime T: type, allocator: mem.Allocator) !T {
        return v.iter.get_string_alloc(T, allocator);
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

    pub fn at_pointer(v: *Value, json_pointer: []const u8) !Value {
        return switch (try v.iter.get_type()) {
            .array => (try v.get_array()).at_pointer(json_pointer),
            .object => (try v.get_object()).at_pointer(json_pointer),
            else => error.INVALID_JSON_POINTER,
        };
    }

    pub fn get(val: *Value, out: anytype, options: GetOptions) cmn.Error!void {
        const T = @TypeOf(out);
        const info = @typeInfo(T);
        switch (info) {
            .Pointer => {
                const C = std.meta.Child(T);
                const child_info = @typeInfo(C);
                switch (info.Pointer.size) {
                    .One => {
                        switch (child_info) {
                            .Int => out.* = try val.get_int(C),
                            .Float => out.* = @floatCast(C, try val.get_double()),
                            .Bool => out.* = try val.get_bool(),
                            .Optional => out.* = if (try val.is_null())
                                null
                            else blk: {
                                var x: std.meta.Child(C) = undefined;
                                try val.get(&x, options);
                                break :blk x;
                            },
                            .Array => {
                                var arr = try val.get_array();
                                var iter = arr.iterator();
                                for (out) |*out_ele| {
                                    var arr_ele = (try iter.next()) orelse break;
                                    try arr_ele.get(out_ele, options);
                                }
                            },
                            .Pointer => {
                                if (child_info.Pointer.size == .Slice) {
                                    if (options.allocator) |allocator| {
                                        switch (try val.get_type()) {
                                            .array => {
                                                var list = std.ArrayListUnmanaged(std.meta.Child(C)){};
                                                var arr = try val.get_array();
                                                var iter = arr.iterator();
                                                while (try iter.next()) |*arr_ele| {
                                                    try arr_ele.get(try list.addOne(allocator), options);
                                                }
                                                out.* = list.toOwnedSlice(allocator);
                                            },
                                            .string => out.* = try val.get_string_alloc(C, allocator),
                                            else => return error.INCORRECT_TYPE,
                                        }
                                    } else {
                                        val.iter.iter.parser.log.err(&val.iter, "slice requires an options.allocator be provided: get(..., .{.allocator = allocator})");
                                        return error.MEMALLOC;
                                    }
                                } else @compileError("unsupported type: " ++ @typeName(T) ++
                                    ". expecting slice");
                            },
                            .Struct => {
                                switch (try val.iter.get_type()) {
                                    .object => {
                                        var obj = try val.get_object();
                                        inline for (std.meta.fields(C)) |field| {
                                            const field_info = @typeInfo(field.field_type);
                                            if (obj.find_field_unordered(field.name)) |*obj_val|
                                                if (field_info != .Pointer)
                                                    try obj_val.get(&@field(out, field.name), options)
                                                else {
                                                    if (options.allocator != null)
                                                        try obj_val.get(&@field(out, field.name), options)
                                                    else
                                                        try obj_val.get(@field(out, field.name), options);
                                                }
                                            else |_| {}
                                        }
                                    },
                                    else => return error.INCORRECT_TYPE,
                                }
                            },
                            else => @compileError("unsupported type: " ++ @typeName(T) ++
                                ". expecting int, float, bool or optional type."),
                        }
                    },
                    .Slice => {
                        switch (try val.iter.get_type()) {
                            .string => _ = try if (options.allocator) |allocator|
                                val.get_string_alloc(T, allocator)
                            else
                                return error.INCORRECT_TYPE,
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

    pub fn get_type(val: *Value) !ValueType {
        return val.iter.get_type();
    }

    pub fn raw_json_token(val: *Value) ![]const u8 {
        const len = std.math.cast(u16, val.iter.peek_start_length()) orelse return error.Overflow;
        const ptr = try val.iter.peek_start(len);
        return ptr[0..len];
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
            ValueIterator.copy_key_without_quotes(key_buf, try oi.iter.field_key(), key_buf.len);
            try oi.iter.field_value();
            return Field{ .key = key_buf.ptr, .value = .{ .iter = oi.iter.child() } };
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
    // TODO: move these to ValueIterator
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
    pub fn resume_value(o: Object) Value {
        return Value{ .iter = o.iter };
    }

    pub fn find_field(o: *Object, key: []const u8) !Value {
        return if (try o.iter.find_field_raw(key))
            Value{ .iter = o.iter.child() }
        else
            error.NO_SUCH_FIELD;
    }

    pub fn find_field_unordered(o: *Object, key: []const u8) !Value {
        return if (try o.iter.find_field_unordered_raw(key))
            Value{ .iter = o.iter.child() }
        else
            error.NO_SUCH_FIELD;
    }

    pub fn at_pointer(o: *Object, json_pointer_: []const u8) !Value {
        if (json_pointer_[0] != '/') return error.INVALID_JSON_POINTER;
        var json_pointer = json_pointer_[1..];
        const slash = mem.indexOfScalar(u8, json_pointer, '/');
        const key = json_pointer[0 .. slash orelse json_pointer.len];
        // Find the child with the given key
        var child: Value = undefined;

        // TODO: escapes
        // If there is an escape character in the key, unescape it and then get the child.
        //   const escape = mem.indexOfScalar(u8, key, '~');
        //   if (escape != null) {
        //     // Unescape the key
        //     std::string unescaped(key);
        //     do {
        //       switch (unescaped[escape+1]) {
        //         case '0':
        //           unescaped.replace(escape, 2, "~");
        //           break;
        //         case '1':
        //           unescaped.replace(escape, 2, "/");
        //           break;
        //         default:
        //           return error.INVALID_JSON_POINTER; // "Unexpected ~ escape character in JSON pointer");
        //       }
        //       escape = unescaped.find('~', escape+1);
        //     } while (escape != std::string::npos);
        //     child = find_field(unescaped);  // Take note find_field does not unescape keys when matching
        //   } else {
        child = try o.find_field(key);
        child.iter.iter.err catch return child; // we do not continue if there was an error

        // If there is a /, we have to recurse and look up more of the path
        if (slash != null)
            child = try child.at_pointer(json_pointer[slash.?..]);
        return child;
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
            const result = Value{ .iter = ai.iter.child() };
            try ai.iter.skip_child();
            return result;
        }
        return null;
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

    pub fn at(a: *Array, index: usize) !?Value {
        var it = a.iterator();
        var i: usize = 0;
        while (try it.next()) |e| : (i += 1)
            if (i == index)
                return e;
        return null;
    }
    pub fn at_pointer(a: *Array, json_pointer_: []const u8) !Value {
        if (json_pointer_[0] != '/') return error.INVALID_JSON_POINTER;
        var json_pointer = json_pointer_[1..];
        // - means "the append position" or "the element after the end of the array"
        // We don't support this, because we're returning a real element, not a position.
        if (json_pointer.len == 1 and json_pointer[0] == '-') return error.INDEX_OUT_OF_BOUNDS;

        // Read the array index
        var array_index: usize = 0;
        var i: usize = 0;
        while (i < json_pointer.len and json_pointer[i] != '/') : (i += 1) {
            const digit = json_pointer[i] - '0';
            // Check for non-digit in array index. If it's there, we're trying to get a field in an object
            if (digit > 9) return error.INCORRECT_TYPE;
            array_index = array_index * 10 + digit;
        }

        // 0 followed by other digits is invalid
        if (i > 1 and json_pointer[0] == '0') return error.INVALID_JSON_POINTER; // "JSON pointer array index has other characters after 0"

        // Empty string is invalid; so is a "/" with no digits before it
        if (i == 0) return error.INVALID_JSON_POINTER; // "Empty string in JSON pointer array index"
        // Get the child
        var child = (try a.at(array_index)) orelse return error.INVALID_JSON_POINTER;
        // If there is an error, it ends here
        child.iter.iter.err catch return child;

        // If there is a /, we're not done yet, call recursively.
        if (i < json_pointer.len)
            child = try child.at_pointer(json_pointer[i..]);

        return child;
    }
};
const TokenIterator = struct {
    /// src index
    index: [*]const u32,

    pub fn peek_delta(ti: *TokenIterator, parser: *Parser, delta: i32, len: u16) ![*]const u8 {
        return parser.peek(
            if (delta < 0)
                ti.index - @intCast(u32, -delta)
            else
                ti.index + @intCast(u32, delta),
            len,
        );
    }

    pub fn peek_length(position: [*]const u32) u32 {
        return (position + 1)[0] - position[0];
    }
};
pub const Iterator = struct {
    token: TokenIterator,
    parser: *Parser,
    err: cmn.Error!void = {},
    depth: u32,

    pub fn init(parser: *Parser) Iterator {
        return Iterator{
            .token = .{ .index = parser.structural_indices().ptr },
            .parser = parser,
            .depth = 1,
        };
    }

    pub fn advance(iter: *Iterator, peek_len: u16) ![*]const u8 {
        defer iter.token.index += 1;
        // cmn.print("advance '{s}'\n", .{(try iter.parser.peek(iter.token.index, peek_len))[0..peek_len]});
        return iter.parser.peek(iter.token.index, peek_len);
    }
    pub fn peek(iter: *Iterator, index: [*]const u32, len: u16) ![*]const u8 {
        return iter.parser.peek(index, len);
    }
    pub fn peek_last(iter: *Iterator) ![*]const u8 {
        return iter.parser.peek(iter.last_document_position(), 1);
    }
    pub fn peek_delta(iter: *Iterator, delta: i32, len: u16) ![*]const u8 {
        return iter.token.peek_delta(iter.parser, delta, len);
    }
    pub fn peek_length(position: [*]const u32) u32 {
        return TokenIterator.peek_length(position);
    }
    pub fn last_document_position(iter: Iterator) [*]const u32 {
        // The following line fails under some compilers...
        // SIMDJSON_ASSUME(parser.implementation.n_structural_indexes > 0);
        // since it has side-effects.
        const n_structural_indexes = iter.parser.parser.n_structural_indexes;
        assert(n_structural_indexes > 0);
        const result = iter.parser.structural_indices().ptr + (n_structural_indexes - 1);
        // cmn.println("tail.items {any} n_structural_indexes {} result {}\n", .{ iter.parser.structural_indices(), n_structural_indexes, result[0] });
        return result;
    }
    pub fn root_checkpoint(iter: Iterator) [*]const u32 {
        return iter.parser.structural_indices().ptr;
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
                iter.parser.log.start_value(iter, "skip");
            },
            // If there is a comma, we have just finished a value in an array/object, and need to get back in
            ',' => {
                iter.parser.log.value(iter, "skip");
            },
            // ] or } means we just finished a value and need to jump out of the array/object
            ']', '}' => {
                iter.parser.log.end_value(iter, "skip");
                iter.depth -= 1;
                if (iter.depth <= parent_depth) return;
            },
            // Anything else must be a scalar value
            else => {
                // For the first scalar, we will have incremented depth already, so we decrement it here.
                iter.parser.log.value(iter, "skip");
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
                    iter.parser.log.start_value(iter, "skip");
                    iter.depth += 1;
                },
                // TODO consider whether matching braces is a requirement: if non-matching braces indicates
                // *missing* braces, then future lookups are not in the object/arrays they think they are,
                // violating the rule "validate enough structure that the user can be confident they are
                // looking at the right values."
                // PERF TODO we can eliminate the switch here with a lookup of how much to add to depth
                ']', '}' => {
                    iter.parser.log.end_value(iter, "skip");
                    iter.depth -= 1;
                    if (iter.depth <= parent_depth) return;
                },
                else => {
                    iter.parser.log.value(iter, "skip");
                },
            }
        }

        return iter.report_error(error.TAPE_ERROR, "not enough close braces");
    }

    fn report_error(iter: *Iterator, err: cmn.Error, message: []const u8) cmn.Error {
        assert(err != error.UNINITIALIZED and err != error.INCORRECT_TYPE and err != error.NO_SUCH_FIELD);
        iter.parser.log.err(iter, message);

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

    fn copy_to_buffer(json: [*]const u8, max_len_: u32, comptime N: u16, tmpbuf: *[N]u8) bool {
        // Truncate whitespace to fit the buffer.
        var max_len = max_len_;
        if (max_len > N - 1) {
            if (CharUtils.is_not_structural_or_whitespace(json[N - 1])) return false;
            max_len = N - 1;
        }

        // Copy to the buffer.
        @memcpy(tmpbuf, json, max_len);
        tmpbuf[max_len] = ' ';
        return true;
    }

    pub fn rewind(iter: *Iterator) !void {
        iter.token.index = iter.parser.structural_indices().ptr;
        iter.parser.log.start(iter); // We start again
        iter.depth = 1;
    }

    pub fn at_eof(iter: Iterator) bool {
        return iter.token.index == iter.last_document_position();
    }
};

pub const ValueIterator = struct {
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
    fn parse_null(json: [*]const u8) bool {
        return atom_parsing.is_valid_atom(json, 4, atom_parsing.atom_null) and
            CharUtils.is_structural_or_whitespace(json[4]);
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
        return parse_null(try vi.advance_non_root_scalar("null", 5));
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
    fn assert_at_root(vi: ValueIterator) void {
        vi.assert_at_start();
        assert(vi.depth == 1);
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
        vi.iter.parser.log.value2(&vi.iter, typ, "", vi.start_position[0], vi.depth);
        if (!vi.is_at_start())
            return vi.peek_start(peek_len);

        vi.assert_at_non_root_start();
        const result = try vi.iter.advance(peek_len);
        vi.ascend_to(vi.depth - 1);
        return result;
    }
    fn advance_root_scalar(vi: *ValueIterator, typ: []const u8, peek_len: u16) ![*]const u8 {
        vi.iter.parser.log.value2(&vi.iter, typ, "", vi.start_position[0], vi.depth);
        if (!vi.is_at_start())
            return vi.peek_start(peek_len);
        vi.assert_at_root();
        defer vi.iter.ascend_to(vi.depth - 1);
        return vi.iter.advance(peek_len);
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
                vi.iter.parser.log.end_value(&vi.iter, "object");
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
                vi.iter.parser.log.end_value(&vi.iter, "array");
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
        vi.iter.parser.log.line_fmt(&vi.iter, "", typ, "start pos {} depth {}", .{ vi.start_position[0], vi.depth });

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
    fn incorrect_type_error(vi: *ValueIterator, message: []const u8) cmn.Error {
        vi.iter.parser.log.err_fmt(&vi.iter, "{s}.  start_position {} depth {}", .{ message, vi.start_position[0], vi.depth });
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
            vi.iter.parser.log.value(&vi.iter, "empty object");
            _ = try vi.iter.advance(0);
            vi.iter.ascend_to(vi.depth - 1);
            return false;
        }
        vi.iter.parser.log.start_value(&vi.iter, "object");
        // #ifdef SIMDJSON_DEVELOPMENT_CHECKS
        //   vi.iter.set_start_position(_depth, _start_position);
        // #endif
        return true;
    }

    fn started_array(vi: *ValueIterator) !bool {
        vi.assert_at_container_start();
        if ((try vi.iter.peek_delta(0, 1))[0] == ']') {
            vi.iter.parser.log.value(&vi.iter, "empty array");
            _ = try vi.iter.advance(1);
            vi.iter.ascend_to(vi.depth - 1);
            return false;
        }
        vi.iter.parser.log.start_value(&vi.iter, "array");

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

    fn field_key(vi: *ValueIterator) ![*]const u8 {
        vi.assert_at_next();
        var key = try vi.iter.advance(std.math.cast(u16, std.math.min(
            READ_BUF_CAP,
            vi.peek_start_length(),
        )) orelse return error.Overflow);
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
        // const len = try cmn.ptr_diff(u8, end.?, key_buf.ptr);
        // return key_buf[0..len];
    }
    fn find_field_raw(vi: *ValueIterator, key: []const u8) !bool {
        // cmn.println("find_field_raw vi.depth {} vi.iter.depth {}", .{ vi.depth, vi.iter.depth });
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
            copy_key_with_quotes(&key_buf, try vi.field_key(), key.len);

            cmn.println("actual_key '{s}'", .{key_buf[0..key.len]});

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
                vi.iter.parser.log.event(&vi.iter, "match ", key, -2, 0);
                return true;
            }

            // No match: skip the value and see if , or } is next
            vi.iter.parser.log.event(&vi.iter, "no match ", key, -2, 0);
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
                vi.iter.parser.log.event(&vi.iter, "match ", key, -2, 0);
                return true;
            }

            // No match: skip the value and see if , or } is next
            vi.iter.parser.log.event(&vi.iter, "no match ", key, -2, 0);
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
                vi.iter.parser.log.event(&vi.iter, "match ", key, -2, 0);
                return true;
            }

            // No match: skip the value and see if , or } is next
            vi.iter.parser.log.event(&vi.iter, "no match ", key, -2, 0);
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
        const peek_len = comptime std.math.cast(u16, std.math.log10(@as(usize, std.math.maxInt(T)))) orelse return error.Overflow;
        const u64int = try number_parsing.parse_integer(
            try vi.advance_non_root_scalar(@typeName(T), peek_len),
        );
        return std.math.cast(T, if (@typeInfo(T).Int.signedness == .signed)
            @bitCast(i64, u64int)
        else
            u64int) orelse return error.Overflow;
    }

    pub fn unescape(comptime T: type, src: [*]const u8, dst: [*]u8) !T {
        const end = string_parsing.parse_string(src, dst) orelse return error.STRING_ERROR;
        const len = try cmn.ptr_diff(u32, end, dst);
        return @as(T, dst[0..len]);
    }

    pub fn get_string(vi: *ValueIterator, comptime T: type, dest: []u8) !T {
        if (dest.len + 2 < vi.peek_start_length()) return error.CAPACITY;
        const peek_len = std.math.cast(u16, std.math.min(READ_BUF_CAP, dest.len)) orelse return error.Overflow;
        return unescape(T, try vi.get_raw_json_string(peek_len), dest.ptr);
    }
    pub fn get_string_alloc(vi: *ValueIterator, comptime T: type, allocator: mem.Allocator) !T {
        const str_len = std.math.cast(u16, vi.peek_start_length()) orelse return error.Overflow;
        if (str_len + 2 > READ_BUF_CAP) return error.CAPACITY;
        const start = try vi.get_raw_json_string(str_len);
        return string_parsing.parse_string_alloc(T, start, allocator, str_len);
    }

    fn advance_start(vi: *ValueIterator, typ: []const u8, peek_len: u16) ![*]const u8 {
        vi.iter.parser.log.value2(&vi.iter, typ, "", vi.start_position[0], vi.depth);
        // If we're not at the position anymore, we don't want to advance the cursor.
        if (!vi.is_at_start()) return vi.peek_start(peek_len);

        // Get the JSON and advance the cursor, decreasing depth to signify that we have retrieved the value.
        vi.assert_at_start();
        const result = try vi.iter.advance(peek_len);
        vi.iter.ascend_to(vi.depth - 1);
        return result;
    }

    pub fn get_raw_json_string(vi: *ValueIterator, peek_len: u16) ![*]const u8 {
        const json = try vi.advance_start("string", peek_len);
        if (json[0] != '"')
            return vi.incorrect_type_error("Not a string");
        return json + 1;
    }

    pub fn get_double(vi: *ValueIterator) !f64 {
        return number_parsing.parse_double(try vi.advance_non_root_scalar("double", 40));
    }
    pub fn get_bool(vi: *ValueIterator) !bool {
        return vi.parse_bool(try vi.advance_non_root_scalar("bool", 6));
    }
    fn peek_start_length(vi: *ValueIterator) u32 {
        return Iterator.peek_length(vi.start_position);
    }
    fn get_root_int(vi: *ValueIterator, comptime T: type) !T {
        const max_len = vi.peek_start_length();
        const json = try vi.advance_root_scalar("int64", std.math.cast(u16, max_len) orelse return error.Overflow);
        var tmpbuf: [20 + 1]u8 = undefined; // -<19 digits> is the longest possible integer
        if (!Iterator.copy_to_buffer(json, max_len, tmpbuf.len, &tmpbuf)) {
            vi.iter.parser.log.err_fmt(&vi.iter, "Root number more than 20 characters. start_position {} depth {}", .{ vi.start_position[0], vi.depth });
            return error.NUMBER_ERROR;
        }
        return std.math.cast(T, try number_parsing.parse_integer(&tmpbuf)) orelse return error.Overflow;
    }
    fn get_root_double(vi: *ValueIterator) !f64 {
        const max_len = vi.peek_start_length();
        const N = comptime std.math.min(1074 + 8 + 1, READ_BUF_CAP);
        const json = try vi.advance_root_scalar("double", N);
        // Per https://www.exploringbinary.com/maximum-number-of-decimal-digits-in-binary-floating-point-numbers/, 1074 is the maximum number of significant fractional digits. Add 8 more digits for the biggest number: -0.<fraction>e-308.
        var tmpbuf: [N]u8 = undefined;
        if (!Iterator.copy_to_buffer(json, max_len, N, &tmpbuf)) {
            vi.iter.parser.log.err_fmt(&vi.iter, "Root number more than 1082 characters. start_position {} depth {}", .{ vi.start_position[0], vi.depth });
            return error.NUMBER_ERROR;
        }
        return number_parsing.parse_double(&tmpbuf);
    }
    fn get_root_string(vi: *ValueIterator, comptime T: type, dest: []u8) !T {
        return vi.get_string(T, dest);
    }
    fn get_root_string_alloc(vi: *ValueIterator, comptime T: type, allocator: mem.Allocator) !T {
        return vi.get_string_alloc(T, allocator);
    }
    fn get_root_bool(vi: *ValueIterator) !bool {
        const max_len = vi.peek_start_length();
        const json = try vi.advance_root_scalar("bool", 5);
        var tmpbuf: [5 + 1]u8 = undefined;
        if (!Iterator.copy_to_buffer(json, max_len, tmpbuf.len, &tmpbuf))
            return vi.incorrect_type_error("Not a boolean");
        return vi.parse_bool(&tmpbuf);
    }
    fn is_root_null(vi: *ValueIterator) !bool {
        const max_len = vi.peek_start_length();
        const json = try vi.advance_root_scalar("null", 4);
        return max_len >= 4 and !atom_parsing.is_valid_atom(json[0..4], 4, atom_parsing.atom_null) or
            (max_len == 4 or CharUtils.is_structural_or_whitespace(json[5]));
    }

    pub fn get_type(vi: *ValueIterator) !ValueType {
        const start = try vi.peek_start(1);
        return switch (start[0]) {
            '{' => .object,
            '[' => .array,
            '"' => .string,
            'n' => .nul,
            't', 'f' => .bool,
            '-', '0'...'9' => .number,
            else => error.TAPE_ERROR,
        };
    }
};

fn unsafe_is_equal(a: [*]const u8, target: []const u8) bool {
    // Assumptions: does not contain unescaped quote characters, and
    // the raw content is quote terminated within a valid JSON string.
    if (target.len <= cmn.SIMDJSON_PADDING)
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

pub const ValueType = enum(u3) {
    // Start at 1 to catch uninitialized / default values more easily
    /// A JSON array   ( [ 1, 2, 3 ... ] )
    array = 1,
    /// A JSON object  ( { "a": 1, "b" 2, ... } )
    object,
    /// A JSON number  ( 1 or -2.3 or 4.5e6 ...)
    number,
    /// A JSON string  ( "a" or "hello world\n" ...)
    string,
    /// A JSON boolean (true or false)
    bool,
    /// A JSON null    (null)
    nul,
};

pub const Document = struct {
    iter: Iterator,
    pub const DOCUMENT_DEPTH = 0;

    inline fn get_root_value_iterator(doc: *Document) ValueIterator {
        return doc.resume_value_iterator();
    }
    inline fn resume_value_iterator(doc: *Document) ValueIterator {
        return ValueIterator.init(doc.iter, 1, doc.iter.root_checkpoint());
    }
    pub fn get_object(doc: *Document) !Object {
        var val = doc.get_root_value_iterator();
        return try Object.start_root(&val);
    }
    pub fn get_array(doc: *Document) !Array {
        var val = doc.get_root_value_iterator();
        return Array.start_root(&val);
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
    pub fn get_int(doc: *Document, comptime T: type) !T {
        return doc.get_root_value_iterator().get_root_int(T);
    }
    pub fn get_string(doc: *Document, comptime T: type, buf: []u8) !T {
        return doc.get_root_value_iterator().get_root_string(T, buf);
    }
    pub fn get_string_alloc(doc: *Document, comptime T: type, allocator: mem.Allocator) !T {
        return doc.get_root_value_iterator().get_root_string_alloc(T, allocator);
    }
    pub fn get_double(doc: *Document) !f64 {
        return doc.get_root_value_iterator().get_root_double();
    }
    pub fn get_bool(doc: *Document) !bool {
        return doc.get_root_value_iterator().get_root_bool();
    }
    pub fn is_null(doc: *Document) !bool {
        return doc.get_root_value_iterator().is_root_null();
    }
    pub fn rewind(doc: *Document) !void {
        return doc.iter.rewind();
    }
    pub fn get_type(doc: *Document) !ValueType {
        return doc.get_root_value_iterator().get_type();
    }
    pub fn at_pointer(doc: *Document, json_pointer: []const u8) !Value {
        try doc.rewind(); // Rewind the document each time at_pointer is called
        if (json_pointer.len == 0)
            return doc.resume_value();
        return switch (try doc.get_type()) {
            .array => (try doc.get_array()).at_pointer(json_pointer),
            .object => (try doc.get_object()).at_pointer(json_pointer),
            else => error.INVALID_JSON_POINTER,
        };
    }
    pub fn get(doc: *Document, out: anytype, options: GetOptions) !void {
        return doc.value().get(out, options);
    }
    pub fn value(doc: *Document) Value {
        return Value{ .iter = doc.get_root_value_iterator() };
    }
    pub fn raw_json_token(doc: *Document) ![]const u8 {
        var iter = doc.get_root_value_iterator();
        const len = std.math.cast(u16, iter.peek_start_length()) orelse return error.Overflow;
        const ptr = try iter.peek_start(len);
        return ptr[0..len];
    }
};

pub const Parser = struct {
    parser: dom.Parser,
    src: *std.io.StreamSource,
    end_pos: u32,
    read_buf: [READ_BUF_CAP]u8 = undefined,
    /// result of src.read() - number of bytes read from src
    read_buf_len: u16 = 0,
    /// src index of the start of the buffer.  set to index[0] each src.read()
    read_buf_start_pos: u32,
    log: Logger = .{ .depth = 0 },

    pub fn init(src: *std.io.StreamSource, allocator: mem.Allocator, filename: []const u8, options: dom.Parser.Options) !Parser {
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
            .end_pos = std.math.cast(u32, try src.getEndPos()) orelse return error.Overflow,
            .read_buf_start_pos = 0,
            .read_buf_len = 0,
        };
        const capacity = result.end_pos;
        const max_structures = cmn.ROUNDUP_N(capacity, 64) + 2 + 7;
        try result.parser.indexer.bit_indexer.tail.ensureTotalCapacity(allocator, max_structures);
        try result.parser.open_containers.ensureTotalCapacity(allocator, options.max_depth);
        return result;
    }

    pub fn deinit(p: *Parser) void {
        p.parser.deinit();
    }

    pub fn stage1(p: *Parser) !void {
        var pos: u32 = 0;
        var read_buf: [cmn.STEP_SIZE]u8 = undefined;
        var bytes_read: u32 = undefined;
        cmn.println("", .{});
        while (true) : (pos += cmn.STEP_SIZE) {
            // cmn.println("i {} pos {}", .{ i, pos });
            bytes_read = @intCast(u32, try p.src.read(&read_buf));
            if (bytes_read < cmn.STEP_SIZE) break;

            try p.parser.indexer.step(read_buf, &p.parser, pos);
            // for (blocks) |block| {
            //     cmn.println("{b:0>64} | characters.whitespace", .{@bitReverse(u64, block.characters.whitespace)});
            //     cmn.println("{b:0>64} | characters.op", .{@bitReverse(u64, block.characters.op)});
            //     cmn.println("{b:0>64} | in_string", .{@bitReverse(u64, block.strings.in_string)});
            // }
        }
        std.mem.set(u8, read_buf[bytes_read..], 0x20);
        // std.log.debug("read_buf {d}", .{read_buf});
        try p.parser.indexer.step(read_buf, &p.parser, pos);
        try p.parser.indexer.finish(&p.parser, pos + cmn.STEP_SIZE, pos + bytes_read, cmn.STREAMING);
    }

    pub fn iterate(p: *Parser) !Document {
        try p.stage1();
        return Document{ .iter = Iterator.init(p) };
    }

    inline fn structural_indices(parser: Parser) []u32 {
        return parser.parser.indexer.bit_indexer.tail.items;
    }

    /// position: pointer to a src position
    /// len_hint: requested length to be read from src, starting at position
    pub fn peek(
        parser: *Parser,
        position: [*]const u32,
        len_hint: u16,
    ) ![*]const u8 {
        // cmn.println("peek() len_hint {} READ_BUF_CAP {}", .{ len_hint, READ_BUF_CAP });
        if (len_hint > READ_BUF_CAP) return error.CAPACITY;
        // cmn.println("\nTokenIterator: {} <= start {} end {} < read_buf_start_pos + len {}", .{ parser.read_buf_start_pos, start, end, read_buf_start_pos + read_buf_len });
        cmn.println("parser {*} position {*} len_hint {}", .{parser, position, len_hint});
        const start_pos = position[0];
        if (parser.read_buf_start_pos <= start_pos and start_pos < parser.read_buf_start_pos + parser.read_buf_len) blk: {
            const offset = start_pos - parser.read_buf_start_pos;
            if (offset + len_hint > READ_BUF_CAP) break :blk;
            return @ptrCast([*]const u8, &parser.read_buf) + offset;
        }
        // cmn.println("TokenIterator: seek and read()", .{});
        try parser.src.seekTo(start_pos);
        parser.read_buf_start_pos = start_pos;
        // not sure that 0xaa is the best value here but it does prevent false positives like [nul]
        @memset(&parser.read_buf, 0xaa, READ_BUF_CAP);
        parser.read_buf_len = @truncate(u16, try parser.src.read(&parser.read_buf));
        return &parser.read_buf;
    }
};
