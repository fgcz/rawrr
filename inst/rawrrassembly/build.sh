#!/bin/bash
set -euxo pipefail

usage() {
    cat <<EOF
Usage: $0 <repo_dir> <dist_dir>

Builds rawrr for all supported runtimes and copies the published
binaries into <dist_dir>.

Arguments:
  repo_dir   Path to the rawrr repository checkout (contains rawrr.csproj)
  dist_dir   Directory to copy the published binaries into (created if missing)

Options:
  -h, --help   Show this help message and exit

Example:
  $0 ~/src/rawrr-temp ./dist
EOF
}

if [ "$#" -eq 1 ] && { [ "$1" == "-h" ] || [ "$1" == "--help" ]; }; then
    usage
    exit 0
fi

if [ "$#" -ne 2 ]; then
    usage
    exit 1
fi
repo_dir="$1"
dist_dir="$2"

cd "$repo_dir"

# RawFileReader is already vendored in most checkouts; only fetch it if missing.
if [ ! -d "RawFileReader/Libs/NetCore/Net8" ]; then
    git clone --depth=1 https://github.com/thermofisherlsms/RawFileReader.git
fi

# Pass the local feed via --source instead of `dotnet nuget add source`: the
# latter writes to the user's global nuget.config, and leftover entries from
# past runs (pointing at deleted checkouts) make every future restore fail
# with NU1301. --source only affects this restore/publish invocation.
nuget_source="$PWD/RawFileReader/Libs/NetCore/Net8/"

# Build in place instead of copying the project (+ vendored RawFileReader) into
# a /tmp work folder first: that copy can be hundreds of MB per runtime and
# easily fills a small/dedicated /tmp volume.
runtimes="linux-x64 osx-x64 win-x64 linux-arm64 osx-arm64 win-arm64"
for runtime in $runtimes; do
    dotnet publish rawrr.csproj --runtime "$runtime" -c Release \
        --source "https://api.nuget.org/v3/index.json" \
        --source "$nuget_source"
done

mkdir -p "$dist_dir"

for runtime in $runtimes; do
    source_path="bin/Release/net8.0/$runtime/publish"
    suffix=""
    if [[ "$runtime" == win-* ]]; then
        suffix=".exe"
    fi
    cp "$source_path/rawrr$suffix" "$dist_dir/rawrr-$runtime$suffix"
done
