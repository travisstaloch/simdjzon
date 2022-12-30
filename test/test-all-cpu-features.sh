set -x

#
# test against different cpus
#
zig build test -Dtarget=x86_64-linux -Dcpu=x86_64+avx # no pclmul - clmulSoft
zig build test -Dtarget=x86_64-linux -Dcpu=x86_64+avx+pclmul
zig build test -Dtarget=x86_64-linux # no avx - fallback
zig build test -Dcpu=x86_64_v2 # no avx - fallback
zig build test -Dtarget=aarch64-linux -Dcpu=apple_latest-aes -fqemu # no aes - clmulSoft
zig build test -Dtarget=aarch64-linux -fqemu
# zig build test -Dtarget=powerpc-linux -fqemu # doesn't work - no classify() + 32bit errors
# zig build test -Dtarget=powerpc64-linux -fqemu # doesn't work - no classify()
zig build test -Dcpu=baseline # fallback

#
# check that main works with different cpus
#
zig build run -- test/twitter.json -v
zig build run -Dondemand -- test/twitter.json -v
zig build -Dtarget=aarch64-linux && qemu-aarch64 zig-out/bin/simdjzon test/twitter.json -v
zig build -Dtarget=aarch64-linux -Dondemand && qemu-aarch64 zig-out/bin/simdjzon test/twitter.json -v
zig build run -Dcpu=baseline -- test/twitter.json -v
zig build run -Dcpu=baseline -Dondemand -- test/twitter.json -v
