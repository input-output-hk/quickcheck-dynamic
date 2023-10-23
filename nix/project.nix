{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    name = "quickcheck-dynamic";

    src = ../.;

    compiler-nix-name = lib.mkDefault "ghc962";

    flake.variants.ghc8107.compiler-nix-name = "ghc8107";
    flake.variants.ghc928.compiler-nix-name = "ghc928";

    shell.withHoogle = false;

    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
    };
  });

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project