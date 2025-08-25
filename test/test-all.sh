# run tests and make sure

set -xe
# wget https://raw.githubusercontent.com/simdjson/simdjson/master/jsonexamples/twitter.json -O test/twitter.json
# curl -fsS -o test/twitter.json https://raw.githubusercontent.com/simdjson/simdjson/master/jsonexamples/twitter.json

args="-freference-trace"
declare -a options=("-Doptimize=Debug" "-Doptimize=ReleaseSmall" "-Doptimize=ReleaseSafe" "-Doptimize=ReleaseFast")
declare -a targets=("native" "aarch64-linux" "native-windows -fwine")

for option in "${options[@]}"; do
  for target in "${targets[@]}"; do
    args2="$args $option"
    targs="-Dtarget=$target -fqemu"
    zig build test --summary all $args2 $targs
    zig build test --summary all $args2 $targs -Dstep-128 

    zig build $args2 $targs
    zig build -Dondemand $args2 $targs
    zig build -Dstep-128 $args2 $targs
    zig build -Dondemand -Dstep-128 $args2 $targs
  done
done


