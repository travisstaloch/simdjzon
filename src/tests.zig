const std = @import("std");
const testing = std.testing;
const mem = std.mem;
const allr = testing.allocator;

const simdjzon = @import("simdjzon");
const dom = simdjzon.dom;
const ondemand = simdjzon.ondemand;
const cmn = simdjzon.common;
const TapeType = dom.TapeType;

const builtin = @import("builtin");
const is_wine = @import("build_options").is_wine;
const is_win_release_small = builtin.mode == .ReleaseSmall and
    (builtin.target.os.tag == .windows or is_wine);

test "tape build 1" {
    const f = try std.fs.cwd().openFile("test/test.json", .{});
    defer f.close();
    var buf: [1024]u8 = undefined;
    var freader = f.reader(&buf);
    const input = try freader.interface.allocRemaining(allr, .limited(std.math.maxInt(u32)));
    defer allr.free(input);
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
    var p: [*:0]u8 = @ptrCast(parser.doc.string_buf.items.ptr);
    var j: u8 = 0;
    const expected_strings: []const []const u8 = &.{
        "Width",   "Height",    "Title", "View from my room",    "Url",    "http://ex.com/img.png",
        "Private", "Thumbnail", "Url",   "http://ex.com/th.png", "Height", "Width",
        "array",   "Owner",
    };
    while (@intFromPtr(p) < @intFromPtr(parser.doc.string_buf.items.ptr + parser.doc.string_buf.items.len)) : (j += 1) {
        const len = mem.bytesAsValue(u32, p[0..4]).*;
        p += 4;
        const str = std.mem.span(p);
        cmn.println("{}:{}-'{s}' '{s}'", .{ j, len, str, p[0..10] });
        try testing.expectEqual(@as(u32, @intCast(str.len)), len);
        try testing.expectEqualStrings(expected_strings[j], str);
        p += str.len + 1;
    }

    var i: usize = 0;
    while (i < expecteds.len) : (i += 1) {
        const expected = expecteds[i];
        const expected_type = TapeType.from_u64(expected);
        const expected_val = TapeType.extract_value(expected);
        // cmn.println("{} : expected {s}:{}-{x}", .{ i, @tagName(expected_type), expected_val, expected });
        const tape_item = parser.doc.tape.items[i];
        const tape_type = TapeType.from_u64(tape_item);
        const tape_val = TapeType.extract_value(tape_item);
        // cmn.println("{} : actual   {s}:{}-{x}", .{ i, @tagName(tape_type), tape_val, tape_item });
        // cmn.println("actual {}:{}", .{expected_type, expected_val});
        // cmn.println("expected 0x{x} tape_item 0x{x}", .{ expected, tape_item });
        try testing.expectEqual(expected_type, tape_type);

        // cmn.println("expected_val {} tape_val {}", .{ expected_val, tape_val });
        if (expected_type != .STRING)
            try testing.expectEqual(expected_val, tape_val);

        if (expected_type == .INT64) {
            i += 1;
            cmn.println("{s} {}", .{ @tagName(expected_type), parser.doc.tape.items[i] });
            try testing.expectEqual(expecteds[i], parser.doc.tape.items[i]);
        }
    }
}

test "tape build 2" {
    const input =
        \\{ "\\\"Nam[{": [ 116,"\\\\" , 234, "true", false ], "t":"\\\""}
    ;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
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
            @as(f64, @bitCast(parser.doc.tape.items[2])),
            0.000000001,
        );
        const ele = parser.element();
        const d = try ele.get_double();
        try testing.expectApproxEqAbs(@as(f64, 123.456), d, 0.000000001);
    }
    {
        var parser = try dom.Parser.initFixedBuffer(allr, "[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001,0.1,0.01]", .{});
        defer parser.deinit();
        try parser.parse();
        // for (parser.doc.tape.items) |tape_item, i|
        //     cmn.println("{}:{s} {}", .{ i, @tagName(TapeType.from_u64(tape_item)), TapeType.extract_value(tape_item) });
        try testing.expectEqual(
            TapeType.DOUBLE.encode_value(0),
            parser.doc.tape.items[2],
        );
        try testing.expectApproxEqAbs(
            @as(f64, 0.000000000000000000000000000000000000000000000000000000000000000000000000000001),
            @as(f64, @bitCast(parser.doc.tape.items[3])),
            std.math.floatEps(f64),
        );
        try testing.expectEqual(
            TapeType.DOUBLE.encode_value(0),
            parser.doc.tape.items[4],
        );
        try testing.expectApproxEqAbs(
            @as(f64, 0.1),
            @as(f64, @bitCast(parser.doc.tape.items[5])),
            std.math.floatEps(f64),
        );
        try testing.expectEqual(
            TapeType.DOUBLE.encode_value(0),
            parser.doc.tape.items[6],
        );
        try testing.expectApproxEqAbs(
            @as(f64, 0.01),
            @as(f64, @bitCast(parser.doc.tape.items[7])),
            std.math.floatEps(f64),
        );
    }
}

test "search tape" {
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

test "array of objects" {
    var parser = try dom.Parser.initFixedBuffer(allr,
        \\[{"a": 1}, {"b": 2}]
    , .{});
    defer parser.deinit();
    try parser.parse();
    const ele = parser.element();
    try testing.expect(ele.is(.ARRAY));
    const array = try ele.get_array();
    const array0 = array.at(0) orelse unreachable;
    try testing.expect(array0.is(.OBJECT));
    const array0_afield = array0.at_key("a") orelse unreachable;
    try testing.expect(array0_afield.is(.INT64));
    try testing.expectEqual(@as(i64, 1), try array0_afield.get_int64());
}

test "array of objects 2" {
    var parser = try dom.Parser.initFixedBuffer(allr,
        \\[{"foo": null, "params": [1,2,4],  "id": 1},
        \\ {"foo": null, "params": [1,2,10], "id": 2}]
    , .{});
    defer parser.deinit();
    try parser.parse();
    const ele = parser.element();
    try testing.expect(ele.is(.ARRAY));
    const array = try ele.get_array();

    const array0 = array.at(0) orelse unreachable;
    try testing.expect(array0.is(.OBJECT));
    const params0 = array0.at_key("params") orelse unreachable;
    try testing.expect(params0.is(.ARRAY));
    const params0_3 = params0.at(2) orelse unreachable;
    try testing.expectEqual(@as(i64, 4), try params0_3.get_int64());
    const id0 = array0.at_key("id") orelse unreachable;
    try testing.expectEqual(@as(i64, 1), try id0.get_int64());

    const array1 = array.at(1) orelse unreachable;
    try testing.expect(array1.is(.OBJECT));
    const params1 = array1.at_key("params") orelse unreachable;
    try testing.expect(params1.is(.ARRAY));
    const params1_3 = params1.at(2) orelse unreachable;
    try testing.expectEqual(@as(i64, 10), try params1_3.get_int64());
    const id1 = array1.at_key("id") orelse unreachable;
    try testing.expectEqual(@as(i64, 2), try id1.get_int64());
}

test "json pointer" {
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
        var s = [1]u16{mem.readInt(u16, "xx", .little)} ** 4;
        try (try parser.element().at_pointer("/a/e")).get(s[0..2]);
        const expected_u16s: [4]u16 = @bitCast(expected);
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

// const dom = @import("simdjzon").dom;
test "get with struct - readme" {
    const S = struct { a: u8, b: []const u8, c: struct { d: u8 } };
    const input =
        \\{"a": 42, "b": "b-string", "c": {"d": 126}}
    ;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    var s: S = undefined;
    try parser.element().get(&s);
    try testing.expectEqual(@as(u8, 42), s.a);
    try testing.expectEqualStrings("b-string", s.b);
    try testing.expectEqual(@as(u8, 126), s.c.d);
}

test "at_pointer - readme" {
    const input =
        \\{"a": {"b": [1,2,3]}}
    ;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    const b0 = try parser.element().at_pointer("/a/b/0");
    try testing.expectEqual(@as(i64, 1), try b0.get_int64());
}

// const ondemand = @import("simdjzon").ondemand;
// ondemand api users must specify `pub const read_buf_cap = N;` in their
// root source file.  In tests, this defaults to `std.mem.page_size`.
test "ondemand get with struct - readme" {
    const S = struct { a: struct { b: []const u8 } };
    const input =
        \\{"a": {"b": "b-string"}}
    ;
    // ondemand api requires a seekable file.
    // --- begin boilerplate ceremony.  this can usually be replaced with just opening a file
    const file_name = "ondemand_get_with_struct_readme";
    var tdir = testing.tmpDir(.{});
    defer tdir.cleanup();
    const tfile = try tdir.dir.createFile(file_name, .{ .read = true });
    defer tfile.close();
    try tfile.writeAll(input);
    try tfile.seekTo(0);
    // --- end boilerplate ceremony.
    var read_buf: [READ_BUF_CAP]u8 = undefined;
    var src = tfile.reader(&read_buf);
    var parser = try ondemand.Parser.init(&src, allr, file_name, .{});
    defer parser.deinit();
    var doc = try parser.iterate();

    var s: S = undefined;
    try doc.get(&s, .{ .allocator = allr });
    defer allr.free(s.a.b);
    try testing.expectEqualStrings("b-string", s.a.b);
}

test "ondemand at_pointer - readme" {
    const input =
        \\{"a": {"b": [1,2,3]}}
    ;
    // ondemand api requires a seekable file.
    // --- begin boilerplate ceremony.  this can usually be replaced with just opening a file
    const file_name = "ondemand_at_pointer_readme";
    var tdir = testing.tmpDir(.{});
    defer tdir.cleanup();
    const tfile = try tdir.dir.createFile(file_name, .{ .read = true });
    defer tfile.close();
    try tfile.writeAll(input);
    try tfile.seekTo(0);
    // --- end boilerplate ceremony.
    var read_buf: [READ_BUF_CAP]u8 = undefined;
    var src = tfile.reader(&read_buf);
    var parser = try ondemand.Parser.init(&src, allr, file_name, .{});
    defer parser.deinit();
    var doc = try parser.iterate();
    var b0 = try doc.at_pointer("/a/b/0");
    try testing.expectEqual(@as(u8, 1), try b0.get_int(u8));
}

// ------------
// end README tests
// ------------

test "get_alloc integer slice" {
    const T = []u8;
    const input =
        \\[0,1,2]
    ;

    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    var s: T = undefined;
    defer allr.free(s);
    try parser.element().get_alloc(allr, &s);

    try std.testing.expectEqual(@as(usize, 3), s.len);
    for (s, 0..) |item, i| {
        try std.testing.expectEqual(@as(u8, @intCast(i)), item);
    }
}

test "get_alloc struct field slice" {
    const T = struct { xs: []struct { a: u8 } };
    const input =
        \\{ "xs": [{"a": 42}, {"a": 42}]}
    ;

    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    var s: T = undefined;
    defer allr.free(s.xs);
    try parser.element().get_alloc(allr, &s);

    try std.testing.expectEqual(@as(usize, 2), s.xs.len);
    for (s.xs) |item| {
        try std.testing.expectEqual(@as(u8, 42), item.a);
    }
}

test "get_alloc struct field slice more field types" {
    const S = struct { a: u8, b: []const u8, c: struct { d: u8 } };
    const T = struct { xs: []S };
    const input =
        \\{ "xs": [
        \\{"a": 42, "b": "b-string", "c": {"d": 126}},
        \\{"a": 42, "b": "b-string", "c": {"d": 126}}
        \\]}
    ;

    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    var s: T = undefined;
    defer allr.free(s.xs);
    try parser.element().get_alloc(allr, &s);

    try std.testing.expectEqual(@as(usize, 2), s.xs.len);
    for (s.xs) |item| {
        try std.testing.expectEqual(@as(u8, 42), item.a);
        try std.testing.expectEqualStrings("b-string", item.b);
        try std.testing.expectEqual(@as(u8, 126), item.c.d);
    }
}

test "dom user defined jsonParse()" {
    const input =
        \\{ "foo": "bar"}
    ;
    const T = struct {
        foo: []const u8,
        pub fn jsonParse(_: dom.Element, out: anytype, _: cmn.GetOptions) !void {
            out.foo = "foo";
        }
    };
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    var s: T = undefined;
    try parser.element().get(&s);
    try testing.expectEqualStrings("foo", s.foo);
}

test "get_alloc slice of struct" {
    const S = struct { a: u8, b: []const u8 };
    const T = []S;
    const input =
        \\[
        \\{"a": 42, "b": "b-string" },
        \\{"a": 42, "b": "b-string" }
        \\]
    ;

    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    var s: T = undefined;
    defer allr.free(s);
    try parser.element().get_alloc(allr, &s);

    try std.testing.expectEqual(@as(usize, 2), s.len);
    for (s) |item| {
        try std.testing.expectEqual(@as(u8, 42), item.a);
        try std.testing.expectEqualStrings("b-string", item.b);
    }
}

test "get_string_int64/uint64" {
    const input =
        \\"1234"
    ;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    {
        const int = try parser.element().get_string_int64();
        try testing.expectEqual(@as(i64, 1234), int);
    }
    {
        const int = try parser.element().get_string_uint64();
        try testing.expectEqual(@as(u64, 1234), int);
    }
}

test "dom object key iterator" {
    const input =
        \\{"a": {"c": null, "d": 3}, "b": 2}
    ;
    var parser = try dom.Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    const ele = parser.element();
    const o = try ele.get_object();
    {
        var keyit = o.key_iterator();
        for ([_][]const u8{ "a", "b" }) |exkey| {
            const key = keyit.next() orelse return error.TestUnexpectedResult;
            try testing.expectEqualStrings(key, exkey);
        }
        try testing.expect(keyit.next() == null);
    }
    {
        const ele2 = o.at_key("a") orelse return error.TestUnexpectedResult;
        const o2 = try ele2.get_object();
        var keyit = o2.key_iterator();
        for ([_][]const u8{ "c", "d" }) |exkey| {
            const key = keyit.next() orelse return error.TestUnexpectedResult;
            try testing.expectEqualStrings(key, exkey);
        }
        try testing.expect(keyit.next() == null);
    }
}

const READ_BUF_CAP = 4096;

// ondemand api users must specify `pub const read_buf_cap = N;` in their
// root source file.  In tests, this defaults to `std.mem.page_size`.
test "ondemand get with struct" {
    const S = struct { a: struct { b: []const u8 } };
    const input =
        \\{"a": {"b": "b-string"}}
    ;
    const file_name = "ondemand_get_with_struct";
    var tdir = testing.tmpDir(.{});
    defer tdir.cleanup();
    const tfile = try tdir.dir.createFile(file_name, .{ .read = true });
    defer tfile.close();
    try tfile.writeAll(input);
    try tfile.seekTo(0);
    var read_buf: [READ_BUF_CAP]u8 = undefined;
    var freader = tfile.reader(&read_buf);
    var parser = try ondemand.Parser.init(&freader, allr, file_name, .{});
    defer parser.deinit();
    var doc = try parser.iterate();

    var arena = std.heap.ArenaAllocator.init(allr);
    defer arena.deinit();
    const a = arena.allocator();
    var s: S = undefined;
    try doc.get(&s, .{ .allocator = a });
    try testing.expectEqualStrings("b-string", s.a.b);
}

test "ondemand at_pointer" {
    const input =
        \\{"a": {"b": [1,2,3]}}
    ;
    var tdir = testing.tmpDir(.{});
    defer tdir.cleanup();
    const file_name = "ondemand_at_pointer";
    const tfile = try tdir.dir.createFile(file_name, .{ .read = true });
    defer tfile.close();
    try tfile.writeAll(input);
    try tfile.seekTo(0);
    var read_buf: [READ_BUF_CAP]u8 = undefined;
    var freader = tfile.reader(&read_buf);
    var parser = try ondemand.Parser.init(&freader, allr, file_name, .{});
    defer parser.deinit();
    var doc = try parser.iterate();
    var b0 = try doc.at_pointer("/a/b/0");
    try testing.expectEqual(@as(u8, 1), try b0.get_int(u8));
}

// ------------
// end README tests
// ------------

const E = cmn.Error ||
    error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedApproxEqAbs, TestUnexpectedError, TestExpectedError } ||
    std.fs.File.WriteError;
fn test_ondemand_doc(input: []const u8, expected: *const fn (doc: *ondemand.Document) E!void) E!void {
    // std.debug.print("\n0123456789012345678901234567890123456789\n{s}\n", .{input});
    var tdir = testing.tmpDir(.{});
    defer tdir.cleanup();
    // create file_name with 10 random chars 'test_ondemand_doc_XXXXXXXXXX'.
    // this seems to prevent a bug where the same file is used in multiple tests.
    var seed: u64 = undefined;
    try std.posix.getrandom(std.mem.asBytes(&seed));
    var prng = std.Random.DefaultPrng.init(seed);
    const random = prng.random();
    const file_name_prefix = "test_ondemand_doc_";
    var file_name: [28]u8 = file_name_prefix.* ++ @as([10]u8, @splat(undefined));
    const alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
    for (file_name[file_name_prefix.len..]) |*c| {
        c.* = alphabet[random.intRangeLessThan(u8, 0, alphabet.len)];
    }
    const tfile = try tdir.dir.createFile(&file_name, .{ .read = true });
    defer tfile.close();
    std.debug.assert(input.len == try tfile.write(input));
    try tfile.seekTo(0);
    var read_buf: [READ_BUF_CAP]u8 = undefined;
    var freader = tfile.reader(&read_buf);
    var parser = try ondemand.Parser.init(&freader, allr, &file_name, .{});
    defer parser.deinit();
    var doc = try parser.iterate();
    try expected(&doc);
}

test "ondemand struct find_field" {
    try test_ondemand_doc(
        \\ {"x": 1, "y": 2, "z": {"a": 33}}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var y = try doc.find_field("y");
            try testing.expectEqual(@as(u64, 2), try y.get_int(u64));
            var x = try doc.find_field("x");
            try testing.expectEqual(@as(u64, 1), try x.get_int(u64));
            var z = try doc.find_field("z");
            var a = try z.find_field("a");
            try testing.expectEqual(@as(u8, 33), try a.get_int(u8));
        }
    }.func);
}
test "ondemand struct iteration" {
    try test_ondemand_doc(
        \\ {"x": 1, "y": 2}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var obj = try doc.get_object();
            var objit = obj.iterator();
            var buf: [10]u8 = undefined;
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("x", buf[0..1]);
                try testing.expectEqual(@as(u64, 1), try f.value.get_int(u64));
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("y", buf[0..1]);
                try testing.expectEqual(@as(u64, 2), try f.value.get_int(u64));
            }

            try testing.expect((try objit.next(&buf)) == null);
        }
    }.func);
}

test "ondemand struct iteration types" {
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
                if ((builtin.mode == .ReleaseSmall and builtin.target.os.tag == .linux) or
                    is_win_release_small)
                {
                    std.log.warn("TODO fix get_string() in release small", .{});
                } else {
                    try testing.expectEqualStrings("strval", try f.value.get_string(&buf));
                }
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("f", buf[0..1]);
                try testing.expectApproxEqAbs(@as(f64, 1.23), try f.value.get_double(), std.math.floatEps(f64));
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("t", buf[0..1]);
                try testing.expectEqual(true, try f.value.get_bool());
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("not", buf[0..3]);
                try testing.expectEqual(false, try f.value.get_bool());
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("n", buf[0..1]);
                try testing.expectEqual(true, try f.value.is_null());
            }
            {
                var f = (try objit.next(&buf)) orelse return testing.expect(false);
                try testing.expectEqualStrings("neg", buf[0..3]);
                try testing.expectEqual(@as(i8, -42), try f.value.get_int(i8));
            }

            try testing.expect((try objit.next(&buf)) == null);
        }
    }.func);
}

test "ondemand array iteration" {
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

test "ondemand array iteration nested 1" {
    try test_ondemand_doc(
        \\[
        \\  { "xs": [ 1.0, 2.0, 3.0, 4.0 ] },
        \\  { "xs": [ 1.0, 2.0, 3.0, 4.0 ] },
        \\  { "xs": [ 1.0, 2.0, 3.0, 4.0 ] }
        \\]
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var arr = try doc.get_array();
            var arrit = arr.iterator();

            while (try arrit.next()) |e_| {
                var e = e_;
                var obj = try e.get_object();
                var xs = try obj.find_field("xs");
                var arr2 = try xs.get_array();
                var it = arr2.iterator();
                var i: u8 = 0;
                while (try it.next()) |e2_| {
                    var e2 = e2_;
                    try testing.expectApproxEqAbs(
                        @as(f64, @floatFromInt(i + 1)),
                        try e2.get_double(),
                        std.math.floatEps(f64),
                    );
                    i += 1;
                }
                try testing.expectEqual(@as(u8, 4), i);
            }

            try testing.expect((try arrit.next()) == null);
        }
    }.func);
}

test "ondemand root types" {
    try test_ondemand_doc(
        \\1
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            try testing.expectEqual(@as(u8, 1), try doc.get_int(u8));
        }
    }.func);
    try test_ondemand_doc(
        \\1.0
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            try testing.expectApproxEqAbs(@as(f64, 1.0), try doc.get_double(), std.math.floatEps(f64));
        }
    }.func);

    try test_ondemand_doc(
        \\"string"
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            {
                const s = try doc.get_string_alloc(allr);
                defer allr.free(s);
                try testing.expectEqualStrings("string", s);
            }
            if (is_win_release_small) {
                std.log.warn("TODO fix get_string() in release small", .{});
            } else {
                var buf: [6]u8 = undefined;
                const s = try doc.get_string(&buf);
                try testing.expectEqualStrings("string", s);
            }
        }
    }.func);

    try test_ondemand_doc(
        \\true
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            try testing.expectEqual(true, try doc.get_bool());
        }
    }.func);
    try test_ondemand_doc(
        \\false
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            try testing.expectEqual(false, try doc.get_bool());
        }
    }.func);
    try test_ondemand_doc(
        \\null
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            try testing.expect(try doc.is_null());
        }
    }.func);
}

test "ondemand atpointer" {
    try test_ondemand_doc(
        \\{"a": {"b": 1, "c": [1,2]}}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var b = try doc.at_pointer("/a/b");
            try testing.expectEqual(@as(u8, 1), try b.get_int(u8));
            var c1 = try doc.at_pointer("/a/c/1");
            try testing.expectEqual(@as(u8, 2), try c1.get_int(u8));
            try testing.expectError(error.NO_SUCH_FIELD, doc.at_pointer("/b"));
        }
    }.func);
}

test "ondemand at, array at_pointer" {
    try test_ondemand_doc(
        \\[1,2]
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var arr = try doc.get_array();
            var e0 = try arr.at(0);
            try testing.expectEqual(@as(u8, 1), try e0.?.get_int(u8));
            var e1 = try arr.at(1);
            try testing.expectEqual(@as(u8, 2), try e1.?.get_int(u8));
            try testing.expectEqual(@as(?ondemand.Value, null), try arr.at(2));
            var a0 = try arr.at_pointer("/0");
            try testing.expectEqual(@as(u8, 1), try a0.get_int(u8));
            var a1 = try arr.at_pointer("/1");
            try testing.expectEqual(@as(u8, 2), try a1.get_int(u8));
            try testing.expectError(error.INVALID_JSON_POINTER, arr.at_pointer("/2"));
        }
    }.func);
}

test "ondemand get struct" {
    try test_ondemand_doc(
        \\{"a": 1, "b": 2, "c": {"d": 3}}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            const S = struct { a: u8, b: u8, c: struct { d: u8 } };
            var s: S = undefined;
            try doc.get(&s, .{});
            try testing.expectEqual(@as(u8, 1), s.a);
            try testing.expectEqual(@as(u8, 2), s.b);
            try testing.expectEqual(@as(u8, 3), s.c.d);
        }
    }.func);
    try test_ondemand_doc(
        \\{"a": {"b": [1,2,3], "c": 3.1415, "d": true, "e": "e-string", "f": null, "g": [4,5,6]}}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            {
                const S = struct { a: struct { b: [3]u8, c: f32, d: bool, f: ?u8 } };
                var s: S = undefined;
                try doc.get(&s, .{});
                try testing.expectApproxEqAbs(@as(f32, 3.1416), s.a.c, std.math.floatEps(f16));
                try testing.expectEqual(true, s.a.d);
                try testing.expectEqual(@as(?u8, null), s.a.f);
                try testing.expectEqualSlices(u8, &.{ 1, 2, 3 }, &s.a.b);
            }
            {
                // parse strings or arrays in slices using an allocator
                const S = struct { a: struct { g: []u8, e: []const u8 } };
                var s: S = undefined;
                try doc.get(&s, .{ .allocator = allr });
                defer allr.free(s.a.g);
                defer allr.free(s.a.e);
                try testing.expectEqualSlices(u8, &.{ 4, 5, 6 }, s.a.g);
                try testing.expectEqualStrings("e-string", s.a.e);
            }
        }
    }.func);
}

test "ondemand get_string_alloc" {
    try test_ondemand_doc(
        \\"asdf"
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var arena = std.heap.ArenaAllocator.init(allr);
            defer arena.deinit();
            const str = try doc.get_string_alloc(arena.allocator());
            try testing.expectEqualStrings("asdf", str);
        }
    }.func);
    const s = "asdf";
    const reps = ondemand.READ_BUF_CAP / s.len + 100;
    const overlong_str = "\"" ++ s ** reps ++ "\"";
    try test_ondemand_doc(overlong_str, struct {
        fn func(doc: *ondemand.Document) E!void {
            const str = doc.get_string_alloc(allr);
            try testing.expectError(error.CAPACITY, str);
        }
    }.func);
}

test "ondemand array iteration nested 2" {
    try test_ondemand_doc(
        \\{"a": [{"b": 1, "c": 2, "d": 3}, {"b": 1, "c": 2, "d": 3}] }
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var buf: [0x10]u8 = undefined;
            const obj1 = try doc.get_object();
            var it1 = obj1.iterator();
            var field1 = try it1.next(&buf) orelse return testing.expect(false);
            var arr1 = try field1.value.get_array();
            var it = arr1.iterator();
            var i: u8 = 0;
            while (try it.next()) |_ao| : (i += 1) {
                var ao = _ao;
                try testing.expectEqual(ondemand.ValueType.object, try ao.get_type());
                var d = try ao.find_field_unordered("d");
                try testing.expectEqual(ondemand.ValueType.number, try d.get_type());
                try testing.expectEqual(@as(i64, 3), try d.get_int(i64));
                var c = try ao.find_field_unordered("c");
                try testing.expectEqual(ondemand.ValueType.number, try c.get_type());
                try testing.expectEqual(@as(i64, 2), try c.get_int(i64));
                var b = try ao.find_field_unordered("b");
                try testing.expectEqual(ondemand.ValueType.number, try b.get_type());
                try testing.expectEqual(@as(i64, 1), try b.get_int(i64));
            }
            try testing.expectEqual(@as(u8, 2), i);
        }
    }.func);
}

test "ondemand edge cases" {
    // This is a sneaky case where TokenIterator.buf could have 'null' when read only moves ahead by 1
    try test_ondemand_doc(
        \\[nul]
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            const arr = try doc.get_array();
            var it = arr.iterator();
            var val = (try it.next()) orelse return testing.expect(false);
            try testing.expectEqual(false, try val.is_null());
        }
    }.func);
}

test "ondemand raw_json_token" {
    try test_ondemand_doc(
        \\12321323213213213213213213213211223
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            const tok = try doc.raw_json_token();
            try testing.expectEqualStrings("12321323213213213213213213213211223", tok);
        }
    }.func);
    try test_ondemand_doc(
        \\{"value":12321323213213213213213213213211223}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var obj = try doc.get_object();
            var val = try obj.find_field("value");
            const tok = try val.raw_json_token();
            try testing.expectEqualStrings("12321323213213213213213213213211223", tok);
        }
    }.func);
}

test "ondemand user defined jsonParse()" {
    const T = struct {
        foo: []const u8,
        pub fn jsonParse(_: *ondemand.Value, args: anytype, _: cmn.GetOptions) !void {
            args.foo = "foo";
        }
    };

    try test_ondemand_doc(
        \\{ "foo": "bar"}
    , struct {
        fn func(doc: *ondemand.Document) E!void {
            var s: T = undefined;
            try doc.get(&s, .{});
            try testing.expectEqualStrings("foo", s.foo);
        }
    }.func);
}

fn domCheckTweets(parser: anytype) !void {
    var tweets = parser.element();
    const count = try (try tweets.at_pointer("/search_metadata/count")).get_uint64();
    try testing.expectEqual(@as(u64, 100), count);
}

test "twitter" {
    const output_filename: []const u8 = if (builtin.os.tag == .windows)
        "test\\twitter.json"
    else
        "test/twitter.json";
    {
        var parser = try dom.Parser.initFile(allr, output_filename, .{});
        defer parser.deinit();
        try parser.parse();
        try domCheckTweets(&parser);
    }
    { // initFromReader() / initExistingFromReader()
        var file = try std.fs.cwd().openFile(output_filename, .{});
        defer file.close();

        var buf: [4096]u8 = undefined;
        var parser = parser: {
            var freader = file.reader(&buf);
            // parser has an invalid reference to freader. but that ok since we re-init below.
            var parser = try dom.Parser.initFromReader(allr, &freader.interface, .{});
            try parser.parse();
            try domCheckTweets(&parser);
            break :parser parser;
        };
        defer parser.deinit();

        for (0..5) |_| {
            file.close();
            file = try std.fs.cwd().openFile(output_filename, .{});
            var freader = file.reader(&buf);
            try parser.initExistingFromReader(&freader.interface, .{});
            try testing.expectEqual(@as(usize, 0), parser.indexer.bit_indexer.tail.items.len);
            try parser.parse();
            try domCheckTweets(&parser);
        }
    }
    {
        var file = try std.fs.cwd().openFile(output_filename, .{});
        defer file.close();
        var read_buf: [READ_BUF_CAP]u8 = undefined;
        var freader = file.reader(&read_buf);
        var parser = try ondemand.Parser.init(&freader, allr, output_filename, .{});
        defer parser.deinit();
        var tweets = try parser.iterate();
        var x = try tweets.at_pointer("/search_metadata/count");
        const count = try x.get_int(u8);
        try testing.expectEqual(@as(u8, 100), count);
    }
}

test "Writer.AlignedAllocating" {
    const input =
        \\[{"foo": null, "params": [1,2,4],  "id": 1},
        \\ {"foo": null, "params": [1,2,10], "id": 2},
        \\ {"foo": null, "params": [1,2,11], "id": 3},
        \\ {"foo": null, "params": [1,2,12], "id": 4}]
    ;
    var reader = std.Io.Reader.fixed(input);
    var bytes: std.ArrayListAlignedUnmanaged(u8, .fromByteUnits(32)) = .{};
    try bytes.ensureTotalCapacity(allr, 100); // simulate existing allocation
    defer bytes.deinit(allr);
    var aligned_w: cmn.AlignedAllocating(.fromByteUnits(32)) = .initOwnedSlice(allr, bytes.allocatedSlice());
    _ = try reader.streamRemaining(&aligned_w.writer);
    bytes = aligned_w.toArrayList();
    try std.testing.expect(std.mem.Alignment.fromByteUnits(32).check(@intFromPtr(bytes.items.ptr)));
}
