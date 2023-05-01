{ inputs # Desystemized merged inputs 
, systemized-inputs # Non-desystemized merged inputs
, flakeopts # iogx config passed to mkFlake
, pkgs # Desystemized nixpkgs (NEVER use systemized-inputs.nixpkgs.legacyPackages!)
, ghc # Current compiler
, deferPluginErrors # For Haddock generation
, enableProfiling
}:
pkgs.haskell-nix.cabalProject' (_: {

  compiler-nix-name = ghc;

  src = flakeopts.repoRoot;
})
