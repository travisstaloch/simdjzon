:warning: **Heavily under development.  Expect bugs and/or missing features** :warning:

# simdjzon
This is a port of [simdjson](https://github.com/simdjson/simdjson), a high performance JSON parser developed by Daniel Lemire and Geoff Langdale to [zig](https://ziglang.org/).  


# requirements
A CPU with both AVX2 and CLMUL is required (Haswell from 2013 onwards should do for Intel, for AMD a Ryzen/EPYC CPU (Q1 2017) should be sufficient).

No fallback for unsupported CPUs is provided.

# usage
```console
# json validation
git clone https://github.com/travisstaloch/simdjzon
zig build -Drelease-fast
zig-out/bin/simdjzon test/test.json
echo $? # 0 on success
0
```

```zig
test "read into a struct" {
    const S = struct { a: u8, b: u8 };
    const input =
        \\{"a": 42, "b": 84}
    ;
    var parser = try Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    var s: S = undefined;
    try parser.element().get(&s);
    try testing.expectEqual(@as(u8, 42), s.a);
    try testing.expectEqual(@as(u8, 84), s.b);
}

test "at_pointer" {
    const input =
        \\{"a": {"b": [1,2,3]}}
    ;
    var parser = try Parser.initFixedBuffer(allr, input, .{});
    defer parser.deinit();
    try parser.parse();
    const b0 = try parser.element().at_pointer("/a/b/0");
    try testing.expectEqual(@as(i64, 1), try b0.get_int64());
}

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
