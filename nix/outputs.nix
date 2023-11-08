{ repoRoot, inputs, pkgs, lib, system }:

let 

  project = lib.iogx.mkHaskellProject {

    cabalProject = pkgs.haskell-nix.cabalProject' {
      name = "quickcheck-dynamic";
      src = ../.;
      compiler-nix-name = lib.mkDefault "ghc962";
      flake.variants.ghc8107.compiler-nix-name = "ghc8107";
      flake.variants.ghc928.compiler-nix-name = "ghc928";
      shell.withHoogle = false;
      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
      };
    };

    shellArgs = _cabalProject: {
      name = "quickcheck-dynamic";
      preCommit = {
        cabal-fmt.enable = true;
        fourmolu.enable = true;
        fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
      };
    };
  };

in 

[
  (
    project.flake
  )
]
