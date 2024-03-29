#!/usr/bin/env bash
#
# Dependencies:
#
# - Nix — https://nixos.org/download.html
# - Stack — https://docs.haskellstack.org/
# - Docker — https://docs.docker.com/get-docker/
# - jq — https://stedolan.github.io/jq/
# - Pandoc — https://pandoc.org/
# - gzip — https://www.gnu.org/software/gzip/
# - GNU tar — https://www.gnu.org/software/tar/
# - fakeroot — https://wiki.debian.org/FakeRoot

set -eo pipefail

if (( BASH_VERSINFO[0] > 4 || ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] >= 4 ) ))
then
    # Treat unset variables and parameters as an error when expanding.  This
    # wasn't very reliable in older Bash versions, hence the version check.
    set -u

    # Command substitutions will inherits the value of the `set -e`, i.e.
    # `set -o errexit`.
    #
    # Available since Bash 4.4, this is actually something Bash does in POSIX
    # mode without it needed to be told.
    shopt -s inherit_errexit
fi

if [[ -n "${DEBUGGING_MODE:-}"  ]]; then
    # This will cause Bash to print commands before executing them.
    set -x
fi

declare -r target='socks-server'
declare root=
root="$(dirname "$(realpath "${0}")")"

declare -a flags=(
    --flag="${target}:static"
)

# Usage:
#
#   getDockerImageViaNix DEBUGGING_MODE ATTRIBUTE
function getDockerImageViaNix() {
    local -r debuggingMode="$1"; shift
    local -r root="$1"; shift
    local -r attribute="$1"; shift

    # GHC version should correspond to LTS version in 'stack.yaml'.
    DEBUGGING_MODE="${debuggingMode}" nix-build --no-link \
        -A "${attribute}" \
        "${root}/nix/ghc8104-musl-docker-image.nix"
}

# Usage:
#
#   main
function main() {
    # At this point Nix will fetch cache the image in the Nix store
    # ("/nix/store"). Subsequent calls will not refetch it unless the entry in
    # Nix store was garbage collected or if the Nix derivation for the docker
    # image has changed.
    local loadDockerImageBashScript=
    loadDockerImageBashScript="$(
        getDockerImageViaNix "${DEBUGGING_MODE:-0}" "${root}" \
            loadDockerImageBashScript
    )"

    # Script can tell us what's the docker image name after it's loaded into
    # Docker.
    local dockerImage=
    dockerImage="$(
        "${loadDockerImageBashScript}" --full-image-name
    )"

    # Don't load the image if it's already present. If the image has changed,
    # but if the dockerImage (name and tag) has not then you may want to
    # override the check or remove the old image for the load to kick off.
    local -a images=()
    mapfile -t images < <(
        docker images "${dockerImage}" --format='{{.ID}}'
    )
    if (( ${#images[@]} == 0 )); then
        "${loadDockerImageBashScript}"
    fi

    # Directory structure:
    #
    # ${root}/out/
    # ├── ${target}/
    # │   ├── bin/*
    # │   ├── etc/*
    # │   └── share/man/man1/*.1.gz
    # └── ${target}-${version}-${arch}.tar.xz

    local -r out="${root}/out"
    local -r dest="${out}/${target}"

    mkdir -p "${dest}/bin"
    stack \
        --local-bin-path="${dest}/bin" \
        --docker --docker-image="${dockerImage}" \
        install \
        "${flags[@]}" \
        "${target}"

    mkdir -p "${dest}/etc"
    cp "${root}/config.dhall" "${dest}/etc/${target}.dhall"

    mkdir -p "${dest}/share/man/man1"
    local -r -a manPages=("${target}.1")
    for manPage in "${manPages[@]}"; do
        local src="${root}/man/${manPage}.md"
        local dst="${dest}/share/man/man1/${manPage}"
        pandoc --standalone --to=man --output="${dst}" "${src}"
        gzip --force --best "${dst}"
    done

    local version=
    version="$(
        jq --raw-output . "${root}/version.json"
    )"

    local arch=
    arch="$(
        stack --docker --docker-image="${dockerImage}" \
            ghc -- --print-build-platform \
        | sed 's/-unknown-/-/'
    )"

    fakeroot -- bash <<EOF
        chown -R root:root "${dest}"
        find "${dest}"        -type d -print0 | xargs -0 chmod 755
        find "${dest}/bin"    -type f -print0 | xargs -0 chmod 755
        find "${dest}/share"  -type f -print0 | xargs -0 chmod 644
        find "${dest}/etc"    -type f -print0 | xargs -0 chmod 644

        tar --directory="${out}" \
            --create --xz --file="${out}/${target}-${version}-${arch}.tar.xz" \
            "${target}/"
EOF
}

main "$@"
