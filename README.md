:warning: **Under heavy development.  Expect bugs and/or missing features** :warning:

# simdjzon
This is a port of [simdjson](https://github.com/simdjson/simdjson), a high performance JSON parser developed by Daniel Lemire and Geoff Langdale to [zig](https://ziglang.org/).  


# requirements
A CPU with both AVX2 and CLMUL is required (Haswell from 2013 onwards should do for Intel, for AMD a Ryzen/EPYC CPU (Q1 2017) should be sufficient).  

No fallback for unsupported CPUs is provided (yet).

# usage
```console
# json validation
$ git clone https://github.com/travisstaloch/simdjzon
$ cd simdjzon
$ zig build -Drelease-fast # uses the dom api by default
$ zig-out/bin/simdjzon test/test.json
$ echo $? # 0 on success
0
$ zig build -Drelease-fast -Dondemand # use the ondemand api
$ zig-out/bin/simdjzon test/test.json
$ echo $? # 0 on success
0
$ zig build test
All 19 tests passed.
```

```zig
const dom = @import("dom.zig");
test "get with struct" {
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

const ondemand = @import("ondemand.zig");
test "ondemand get with struct" {
    const S = struct { a: struct { b: []const u8 } };
    const input =
        \\{"a": {"b": "b-string"}}
    ;
    var src = std.io.StreamSource{ .const_buffer = std.io.fixedBufferStream(input) };
    var parser = try ondemand.Parser.init(&src, allr, "<fba>", .{});
    defer parser.deinit();
    var doc = try parser.iterate();

    var s: S = undefined;
    try doc.get(&s, .{ .allocator = allr });
    defer allr.free(s.a.b);
    try testing.expectEqualStrings("b-string", s.a.b);
}

test "ondemand at_pointer" {
    const input =
        \\{"a": {"b": [1,2,3]}}
    ;
    var src = std.io.StreamSource{ .const_buffer = std.io.fixedBufferStream(input) };
    var parser = try ondemand.Parser.init(&src, allr, "<fba>", .{});
    defer parser.deinit();
    var doc = try parser.iterate();
    var b0 = try doc.at_pointer("/a/b/0");
    try testing.expectEqual(@as(u8, 1), try b0.get_int(u8));
}
```

## gyro package manager
### add dependency
```console
$ gyro add --src github travisstaloch/simdjzon
```

### modify
```zig
// build.zig
const deps = @import("deps.zig");
...
    deps.pkgs.addAllTo(exe);
    exe.linkLibC();
    // add c files / include dir
    const gyro_src_dir = deps.base_dirs.simdjzon ++ "/src/";
    exe.addCSourceFile(gyro_src_dir ++ "utils.c", &.{});
    exe.addIncludeDir(gyro_src_dir);
```
### build
```console
gyro build
```
# performance
## parsing/validating twitter.json (630Kb)
### simdjson

```console
$ wget https://raw.githubusercontent.com/simdjson/simdjson/master/singleheader/simdjson.h https://raw.githubusercontent.com/simdjson/simdjson/master/singleheader/simdjson.cpp https://raw.githubusercontent.com/simdjson/simdjson/master/jsonexamples/twitter.json

$ cat main.cpp
#include "simdjson.h"
using namespace simdjson;
int main(int argc, char** argv) {
    if(argc != 2) {
        std::cout << "USAGE: ./simdjson <file.json>" << std::endl;
        exit(1);
    }
    dom::parser parser; 
    try
    {
        const dom::element doc = parser.load(argv[1]);
    }
    catch(const std::exception& e)
    {
        std::cerr << e.what() << '\n';
        return 1;
    }
    return 0;
}

$ g++ main.cpp simdjson.cpp -o simdjson -O3 -march=native
$ time ./simdjson twitter.json

real	0m0.003s
user	0m0.002s
sys	0m0.001s

$ echo $?
0
```

### simdjzon
```console
$ time zig-out/bin/simdjzon twitter.json 

real	0m0.002s
user	0m0.000s
sys	0m0.002s

$ echo $?
0

```

### timed against simdjson, go, nim, zig std lib
The simdjson binary was compiled as shown above.  Go and nim binaries created with sources from JSONTestSuite. [zig std lib driver](bench/src/zig_json.zig).
Validation times for several large json files.  Created with [benchmark_and_plot.jl](bench/benchmark_and_plot.jl)
![results](https://github.com/travisstaloch/simdjson-zig/blob/media/bench/validation_grouped.png)

# JSONTestSuite

Results of running simdjson and simdjzon through [JSONTestSuite](https://github.com/nst/JSONTestSuite).  Results are equal as of 8/7/21

![results](https://github.com/travisstaloch/simdjson-zig/blob/media/JSONTestSuiteResults.png)
