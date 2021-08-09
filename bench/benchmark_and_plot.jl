using Plots.Dates
using StatsPlots

json_files_names = [
    ["../../../nim/godot_test/_godotapi/api.json","godot_api.json"],
    ["../../../nim/godot-nim-stub/_godotapi/api.json","godot_api2.json"],
    ["../../../impala/anydsl/llvm_build/compile_commands.json","llvm_compile_commands.json"],
    ["../../../web/vuegl-pool/package-lock.json","vuegl_pkg_lock.json"],
    ["../../../c/simdjson/jsonexamples/twitter.json","simdjson_twitter.json"],
]
json_files = map(first, json_files_names)
json_names = map(last, json_files_names)

parsers_names = [
    ["bin/zig_json", "zig std lib"],
    ["../../JSONTestSuite/simdjson/simdjson", "c++ simdjson"],
    ["../zig-out/bin/simdjzon", "zig simdjzon"],
    ["../../JSONTestSuite/parsers/test_nim/test_json", "nim json"],
    ["../../JSONTestSuite/parsers/test_go/test_json", "go json"],
]
parsers = map(first, parsers_names)
parser_names = map(last, parsers_names)
simdjzon_parser_idx = findfirst(x -> x == "zig simdjzon", parser_names)

# begin
fileslen = length(json_files)
parserslen = length(parsers)
bench_results = Dict(parser => [[] for _ in 1:fileslen] for parser in parsers)

function bench(parser, json_file, i, j; dry_run=false, debug=false)
    out = Pipe()
    proc = run(pipeline(`./time.sh $parser $json_file`, stdout=out), wait=true)
    close(out.in)
    output = String(read(out))
    
    debug && println("$(parser) $json_file exitcode $(proc.exitcode) stdout $output")
    if !dry_run
        @assert(proc.exitcode == 0, "parse validation failure")
        push!(bench_results[parser][i], parse(Float16, output))
    end
end

# dry run to warm up cache
for parser in parsers, json_file in json_files
    bench(parser, json_file, 1, 1; dry_run=true)
end
num_runs = 10
# gather data
for j in 1:num_runs, (i, json_file) in enumerate(json_files)
    for parser in parsers
        bench(parser, json_file, i, j; debug=false)
    end
end

# average the results and 
# append simdjzon results to csv file
_mn = [[] for _ in parsers]
parser_idx = 1
open("benchmark_results.csv"; append=true) do results_file
    write(results_file, join(["Date", json_names...], ", "), "\n")
    now = Dates.now()
    write(results_file, Dates.format(now, "Y-m-d"), ", ")
    for parser in parsers
        parser_results = bench_results[parser]
        for result_set in parser_results
            avg = sum(result_set) / length(result_set)
            if parser_idx == simdjzon_parser_idx
                write(results_file, "$avg, ")
            end
            push!(_mn[parser_idx], avg)
        end
        global parser_idx += 1
    end
    write(results_file, "\n")
end

# prepare for plotting
lenp = length(parsers)
lenf = length(json_files)
json_file_sizes_kb = map(x -> filesize(x) ÷ 1000, json_files)
json_names_lens = map(x -> json_names[x] * " ($(json_file_sizes_kb[x])Kb)", 1:lenf)
nam = repeat(json_names_lens, inner=lenp)
sx = repeat(parser_names, outer=lenf)

# reshape, flipping rows/cols
dim1 = length(_mn)
dim2 = length(_mn[1])
matrix = Array{eltype(_mn[1]),2}(undef, dim1, dim2)
for i in 1:dim1, j in 1:dim2
    matrix[i,j] = _mn[i][j]
end
mn = collect(Iterators.flatten(reshape(matrix, dim2, dim1)))

# plot
l = @layout [a{0.01h}; grid(1, 1)]
p = fill(plot(), 2, 1)
p[1] = plot(title="JSON Validation Times", framestyle=nothing, showaxis=false, xticks=false, yticks=false, margin=0Plots.mm)
p[2] = groupedbar(nam, mn, group=sx, ylabel="milliseconds", 
            title="average of $num_runs runs", xrotation=-7, titlefontsize=8,
            margin=0Plots.mm, framestyle=nothing, margin_right=10Plots.mm
    )
plot(p..., layout=l)
savefig("validation_grouped.png")

# old line plot
if false
    using Plots
    x = 1:fileslen
    p = plot(x, _mn[1], title="validation time", 
                label=parser_names[1], lw=3, 
                ylabel="milliseconds", yrotation=0,
                xlabel="file", xticks=(x, json_names_lens), xrotation=-7
        )
    for i in 2:length(parsers)
        line = _mn[i]
        plot!(p, x, line, label=parser_names[i], lw=3)
    end
    savefig("validation_line.png")
end
