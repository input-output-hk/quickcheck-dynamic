# The haskell.nix project

{
  # Desystemized merged inputs.
  # All the inputs from iogx (e.g. CHaP, haskell-nix, etc..) unioned with the 
  # inputs defined in your flake. You will also find the `self` attribute here.
  # These inputs have been desystemized against the current system.
  inputs

  # Non-desystemized merged inputs.
  # All the inputs from iogx (e.g. CHaP, haskell-nix, etc..) unioned with the 
  # inputs defined in your flake. You will also find the `self` argument here. 
  # These inputs have not been desystemized, they are the original `inputs` from
  # iogx and your `flake.nix`.
, systemized-inputs

  # The very attrset passed to `inputs.iogx.mkFlake` in your `flake.nix`.
, flakeopts

  # Desystemized legacy nix packages configured against `haskell.nix`.
  # NEVER use the `nixpkgs` coming from `inputs` or `systemized-inputs`!
, pkgs

  # The current compiler against which to build the haskell.nix project.
  # You want to set this as the value of `compiler-nix-name`.
, ghc

  # This flag will be set to false when building for CI, and will be set to true
  # when the build has to include haddock.
, deferPluginErrors

  # Whether to enable haskell profiling. 
, enableProfiling
}:
let
  module = { ... }: {
    packages = { };
  };
in
pkgs.haskell-nix.cabalProject' (_: {

  compiler-nix-name = ghc;

  src = flakeopts.repoRoot;

  # Reccomended, otherwise you'll have to build haddock for the entire haskell
  # dependecy tree.
  shell.withHoogle = false;

  inputMap = {
    "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
  };

  modules =
    [ module ] ++ pkgs.lib.optional enableProfiling { enableProfiling = true; };
})
