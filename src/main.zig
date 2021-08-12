const std = @import("std");
const build_options = @import("build_options");

const testing = std.testing;
const mem = std.mem;
const os = std.os;
const assert = std.debug.assert;
usingnamespace @import("common.zig");
const dom = @import("dom.zig");
const ondemand = @import("ondemand.zig");

comptime {
    assert(step_size == 64 or step_size == 128);
}

pub fn main() !u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;
    var parser: dom.Parser = undefined;
    // debug = true;

    if (os.argv.len == 1) {
        var stdin = std.io.getStdIn().reader();
        const input = try stdin.readAllAlloc(allocator, std.math.maxInt(u32));
        parser = try dom.Parser.initFixedBuffer(allocator, input, .{});
    } else if (os.argv.len == 2) {
        const filename = std.mem.span(os.argv[1]);
        parser = try dom.Parser.initFile(allocator, filename, .{});
    } else {
        std.log.err("Too many arguments.  Please provide input via filename or stdin", .{});
        return 1;
    }

    defer parser.deinit();
    parser.parse() catch |err| switch (err) {
        // error.EndOfStream => {},
        else => {
            std.log.err("parse failed. {s}", .{@errorName(err)});
            return 1;
        },
    };
    std.log.debug("parse valid", .{});
    return 0;
}

const TapeType = dom.TapeType;

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

    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
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
        var parser = try dom.Parser.initFixedBuffer(allr, "123.456", .{});
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
        var parser = try dom.Parser.initFixedBuffer(allr, "[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]", .{});
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
    var parser = try dom.Parser.initFile(allr, "test/test.json", .{});
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

test "json pointer" {
    try test_json_pointer();
}
fn test_json_pointer() !void {
    const input =
        \\{"a": {"b": [1,2,3], "c": 3.1415, "d": true, "e": "e-string", "f": null}}
    ;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    const b0 = try parser.element().at_pointer("/a/b/0");
    try testing.expectEqual(@as(i64, 1), try b0.get_int64());
    const b2 = try parser.element().at_pointer("/a/b/2");
    try testing.expectEqual(@as(i64, 3), try b2.get_int64());
    try testing.expectError(error.INVALID_JSON_POINTER, parser.element().at_pointer("/a/b/3"));
    try testing.expectError(error.INVALID_JSON_POINTER, parser.element().at_pointer("/c/b"));
    var i: i64 = undefined;
    try (try parser.element().at_pointer("/a/b/1")).get(&i);
    try testing.expectEqual(@as(i64, 2), i);
    var f: f32 = undefined;
    try (try parser.element().at_pointer("/a/c")).get(&f);
    try testing.expectApproxEqAbs(@as(f32, 3.1415), f, 0.00000001);
    var b = false;
    try (try parser.element().at_pointer("/a/d")).get(&b);
    try testing.expectEqual(true, b);
    var u: u64 = undefined;
    try (try parser.element().at_pointer("/a/b/2")).get(&u);
    try testing.expectEqual(@as(u64, 3), u);
    {
        var s = [1]u8{'x'} ** 8;
        try (try parser.element().at_pointer("/a/e")).get(&s);
        try testing.expectEqualStrings("e-string", &s);
    }
    const expected: [8]u8 = "e-stxxxx".*;
    {
        var s = [1]u8{'x'} ** 8;
        try (try parser.element().at_pointer("/a/e")).get(s[0..4]);
        try testing.expectEqualStrings(&expected, &s);
    }
    {
        // this contrived example shows you can read string data into slice types other than u8
        var s = [1]u16{mem.readIntLittle(u16, "xx")} ** 4;
        try (try parser.element().at_pointer("/a/e")).get(s[0..2]);
        const expected_u16s = @bitCast([4]u16, expected);
        try testing.expectEqualSlices(u16, &expected_u16s, &s);
    }
    var opt: ?u8 = undefined;
    try (try parser.element().at_pointer("/a/f")).get(&opt);
    try testing.expectEqual(@as(?u8, null), opt);
    try (try parser.element().at_pointer("/a/b/2")).get(&opt);
    try testing.expectEqual(@as(?u8, 3), opt);
}

test "get with slice/array" {
    const input =
        \\[1,2,3,4]
    ;
    var s: [4]u8 = undefined;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    try parser.element().get(&s);
    try testing.expectEqualSlices(u8, &.{ 1, 2, 3, 4 }, &s);
}

// ------------
// README tests
// ------------

// const dom = @import("dom.zig");
test "get with struct" {
    const S = struct { a: u8, b: u8, c: struct { d: u8 } };
    const input =
        \\{"a": 42, "b": 84, "c": {"d": 126}}
    ;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    var s: S = undefined;
    try parser.element().get(&s);
    try testing.expectEqual(@as(u8, 42), s.a);
    try testing.expectEqual(@as(u8, 84), s.b);
    try testing.expectEqual(@as(u8, 126), s.c.d);
}

test "at_pointer" {
    const input =
        \\{"a": {"b": [1,2,3]}}
    ;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    const b0 = try parser.element().at_pointer("/a/b/0");
    try testing.expectEqual(@as(i64, 1), try b0.get_int64());
}

// ------------
// end README tests
// ------------

test "ondemand" {
    try test_ondemand();
}

const E = Error || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedApproxEqAbs };
fn test_ondemand_doc(input: []const u8, expected: fn (doc: *ondemand.Document) E!void) !void {
    // std.debug.print("\n0123456789012345678901234567890123456789\n{s}\n", .{input});
    var src = std.io.StreamSource{ .const_buffer = std.io.fixedBufferStream(input) };
    var parser = try ondemand.Parser.init(&src, allr, "<fba>", .{});
    defer parser.deinit();
    var doc = try parser.iterate(src);
    try expected(&doc);
}

pub fn test_ondemand() !void {
    try test_ondemand_doc(
        \\ {"x": 1, "y": 2, "z": {"a": 33}}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            try testing.expectEqual(@as(u64, 2), try (try doc.find_field("y")).get_int(u64));
            try testing.expectEqual(@as(u64, 1), try (try doc.find_field("x")).get_int(u64));
            try testing.expectEqual(@as(u8, 33), try (try (try doc.find_field("z")).find_field("a")).get_int(u8));
        }
    }.func);

    try test_ondemand_doc(
        \\ {"x": 1, "y": 2}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var obj = try doc.get_object();
            var objit = obj.iterator();
            var buf: [10]u8 = undefined;

            var f1 = (try objit.next(&buf)) orelse return testing.expect(false);
            try testing.expectEqualStrings("x", buf[0..1]);
            try testing.expectEqual(@as(u64, 1), try f1.get_int(u64));

            var f2 = (try objit.next(&buf)) orelse return testing.expect(false);
            try testing.expectEqualStrings("y", buf[0..1]);
            try testing.expectEqual(@as(u64, 2), try f2.get_int(u64));

            try testing.expect((try objit.next(&buf)) == null);
        }
    }.func);

    try test_ondemand_doc(
        \\ {"str": "strval", "f": 1.23, "t": true, "not": false, "n": null, "neg": -42 }
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var obj = try doc.get_object();
            var objit = obj.iterator();
            var buf: [10]u8 = undefined;
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("str", buf[0..3]);
                try testing.expectEqualStrings("strval", try f.iter.get_string(&buf));
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("f", buf[0..1]);
                try testing.expectApproxEqAbs(@as(f64, 1.23), try f.iter.get_double(), std.math.f64_epsilon);
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("t", buf[0..1]);
                try testing.expectEqual(true, try f.iter.get_bool());
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("not", buf[0..3]);
                try testing.expectEqual(false, try f.iter.get_bool());
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("n", buf[0..1]);
                try testing.expectEqual(false, try f.iter.is_null());
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("neg", buf[0..3]);
                try testing.expectEqual(@as(i8, -42), try f.iter.get_int(i8));
            }

            try testing.expect((try objit.next(&buf)) == null);
        }
    }.func);

    try test_ondemand_doc(
        \\ [1,2]
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var arr = try doc.get_array();
            var arrit = arr.iterator();

            {
                var e = (try arrit.next()) orelse return testing.expect(false);
                try testing.expectEqual(@as(u64, 1), try e.get_int(u64));
            }
            {
                var e = (try arrit.next()) orelse return testing.expect(false);
                try testing.expectEqual(@as(u64, 2), try e.get_int(u64));
            }

            try testing.expect((try arrit.next()) == null);
        }
    }.func);
}
