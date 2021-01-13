{ pkgs ? import <nixpkgs> { } }:

let
  # https://hub.docker.com/r/utdemir/ghc-musl
  imageName = "utdemir/ghc-musl";
  imageTag = "v16-ghc884";

  # Values gotten from the output of:
  #
  #   nix-prefetch-docker utdemir/ghc-musl v16-ghc884
  dockerImage = pkgs.dockerTools.pullImage {
    inherit imageName;
    imageDigest =
      "sha256:b4b85da8d5c4a0241a0fe86a6d33ac8650fe0b0fe6e3253552f56c6039d85ff3";
    sha256 = "1vvh1q91lh3ksrrfn44sn5zjicq8y32nf3v6gh5fmmzd2ljmyz0l";
    finalImageName = imageName;
    finalImageTag = imageTag;

    os = "linux";
    arch = "x86_64";
  };

in {
  inherit dockerImage;

  loadDockerImageBashScript = pkgs.writeScript "load-docker-image.bash" ''
    #!/usr/bin/env bash

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

    if [[ -n "''${DEBUGGING_MODE:-}"  ]]; then
        # This will cause Bash to print commands before executing them.
        set -x
    fi

    # Usage:
    #
    #   main [--full-image-name]
    function main() {
        case "$1" in
            --full-image-name)
                echo "${imageName}:${imageTag}"
                ;;

            *)
                echo "Loading docker image: ${dockerImage}"
                docker load --input "${dockerImage}"
                ;;
        esac
    }

    main "$@"
  '';
}
