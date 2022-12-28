const std = @import("std");
const mem = std.mem;
const common = @import("common.zig");
const Ondemand = @import("ondemand.zig");
const println = common.println;
const print = common.print;

depth: u8 = 0,

const Logger = @This();

pub const MAX_DEPTH = 30;
const LOG_EVENT_LEN = 20;
const LOG_BUFFER_LEN = 30;
const LOG_SMALL_BUFFER_LEN = 10;
const LOG_INDEX_LEN = 5;

fn pad_with(comptime s: []const u8, comptime pad_byte: u8, comptime len: u8) [len]u8 {
    var buf = [1]u8{pad_byte} ** len;
    buf[0..s.len].* = s[0..s.len].*;
    return buf;
}

fn pad_with_alloc(s: []const u8, pad_byte: u8, len: u8, allocator: mem.Allocator) []const u8 {
    var buf = allocator.alloc(u8, len) catch return s;
    std.mem.set(u8, buf, pad_byte);
    std.mem.copy(u8, buf, s[0..std.math.min(s.len, buf.len)]);
    return buf;
}

pub fn start(log: *Logger, iter: anytype) void {
    _ = iter;
    if (common.debug) {
        log.depth = 0;
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
    return if (c >= 0x20 and c < 128) c else ' ';
}

pub fn line_fmt(log: *Logger, iter: anytype, title_prefix: []const u8, title: []const u8, comptime detail_fmt: []const u8, detail_args: anytype) void {
    var buf: [0x100]u8 = undefined;
    log.line(iter, title_prefix, title, std.fmt.bufPrint(&buf, detail_fmt, detail_args) catch return);
}

pub fn line(log: *Logger, iter: anytype, title_prefix: []const u8, title: []const u8, detail: []const u8) void {
    if (iter.depth >= Logger.MAX_DEPTH) return;
    var log_buf: [0x100]u8 = undefined;
    var log_buf2: [LOG_BUFFER_LEN]u8 = undefined;
    if (!common.debug) return;

    var log_fba = std.heap.FixedBufferAllocator.init(&log_buf);
    const depth_padding = pad_with_alloc("", ' ', @intCast(u8, if (log.depth < 0x0f) log.depth * 2 else 0xff), log_fba.allocator());
    const titles = std.fmt.allocPrint(
        log_fba.allocator(),
        "{s}{s}{s}",
        .{ depth_padding, title_prefix, title },
    ) catch return;
    const p1 = pad_with_alloc(titles, ' ', LOG_EVENT_LEN, log_fba.allocator());
    print("| {s} ", .{p1});
    const current_index = if (iter.at_beginning()) null else iter.next_structural() - 1;
    const next_index = iter.next_structural();

    const is_ondemand = @TypeOf(iter) == *Ondemand.Iterator;
    const content = blk: {
        if (current_index) |ci| {
            if (is_ondemand) {
                var len = std.math.min(iter.parser.read_buf_len, log_buf2.len);
                const ptr = iter.peek(ci, len) catch break :blk null;
                mem.copy(u8, &log_buf2, ptr[0..len]);
            }

            for (log_buf2) |*c, i| {
                if (!is_ondemand)
                    c.* = printable_char(iter.parser.bytes[ci[0] + i])
                else
                    c.* = printable_char(c.*);
            }
            break :blk pad_with_alloc(&log_buf2, ' ', LOG_BUFFER_LEN, log_fba.allocator());
        } else {
            break :blk &pad_with("", ' ', LOG_BUFFER_LEN);
        }
    };
    print("| {?s} ", .{content});
    const next_content = blk: {
        if (is_ondemand) {
            const len = std.math.min(iter.parser.read_buf_len, log_buf2.len);
            const ptr = iter.peek(next_index, len) catch break :blk null;
            mem.copy(u8, &log_buf2, ptr[0..len]);
        }

        const end_pos = if (!is_ondemand) iter.parser.bytes.len else iter.parser.end_pos;
        for (log_buf2) |*c, i| {
            if (next_index[0] + i >= end_pos) break;
            // std.log.debug("bytes.len {} next_index[0] {} i {}", .{ iter.parser.bytes.len, next_index[0], i });
            if (!is_ondemand)
                c.* = printable_char(iter.parser.bytes[next_index[0] + i])
            else
                c.* = printable_char(c.*);
        }
        break :blk pad_with_alloc(&log_buf2, ' ', LOG_SMALL_BUFFER_LEN, log_fba.allocator());
    };
    print("| {?s} ", .{next_content});

    if (current_index) |ci| {
        print("| {s} ", .{
            pad_with_alloc(
                std.fmt.bufPrint(&log_buf2, "{}", .{ci[0]}) catch return,
                ' ',
                LOG_INDEX_LEN,
                log_fba.allocator(),
            ),
        });
    } else {
        print("| {s} ", .{&pad_with("", ' ', LOG_INDEX_LEN)});
    }
    // printf("| %*u ", LOG_INDEX_LEN, structurals.next_tape_index());
    println("| {s} ", .{detail});
}

pub fn value(log: *Logger, iter: anytype, typ: []const u8) void {
    log.line(iter, "", typ, "");
}
pub fn value2(log: *Logger, iter: anytype, typ: []const u8, detail: []const u8, delta: u32, depth_delta: u32) void {
    // log.line(iter, "", typ, "");
    log.line_fmt(iter, typ, detail, "start pos {} depth {}", .{ delta, depth_delta });
}

pub fn start_value(log: *Logger, iter: anytype, typ: []const u8) void {
    log.line(iter, "+", typ, "");
    if (common.debug) log.depth = log.depth +| 1;
}
pub fn end_value(log: *Logger, iter: anytype, typ: []const u8) void {
    if (common.debug) log.depth = log.depth -| 1;
    log.line(iter, "-", typ, "");
}

pub fn err(log: *Logger, iter: anytype, err_msg: []const u8) void {
    _ = iter;
    _ = log;
    if (common.debug) std.log.err("{s}", .{err_msg});
}
pub fn err_fmt(log: *Logger, iter: anytype, comptime fmt: []const u8, args: anytype) void {
    _ = iter;
    _ = log;
    if (common.debug) std.log.err(fmt, args);
}

pub fn event(log: *Logger, iter: anytype, typ: []const u8, detail: []const u8, delta: i32, depth_delta: i32) void {
    log.line_fmt(iter, typ, detail, "{} {}", .{ delta, depth_delta });
}
