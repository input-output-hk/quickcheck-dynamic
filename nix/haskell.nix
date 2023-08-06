{ inputs, inputs', pkgs, lib, ... }:
{
  enable = lib.mkForce true;

  supportedCompilers = ["ghc928"];

  makeShellWith = { meta, ... }: {
    name = "${meta.haskellCompiler}";


    # preCommitHooks = {
    #   cabal-fmt.enable = true;
    # };
  };

  cabal-fmt.enable = true;

  fourmolu.enable = true;
  fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
}