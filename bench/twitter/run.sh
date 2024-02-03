#!/bin/bash
set -xe

rm -f simdjson.cpp simdjson.h twitter.json simdjson-twitterbench

# build simdjzon
zig version
zig build -Doptimize=ReleaseFast

# build simdjson
g++ --version
cat bench/twitter/main.cpp
wget https://github.com/simdjson/simdjson/raw/master/singleheader/simdjson.cpp -O bench/twitter/simdjson.cpp
wget https://github.com/simdjson/simdjson/raw/master/singleheader/simdjson.h -O bench/twitter/simdjson.h
wget https://github.com/simdjson/simdjson/raw/master/jsonexamples/twitter.json -O bench/twitter/twitter.json
g++ bench/twitter/main.cpp bench/twitter/simdjson.cpp -Ibench/twitter -o bench/twitter/simdjson-twitterbench -O3 -march=native

# bench
poop "./bench/twitter/simdjson-twitterbench bench/twitter/twitter.json" "zig-out/bin/twitterbench bench/twitter/twitter.json"
