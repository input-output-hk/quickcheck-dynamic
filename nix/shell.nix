{ repoRoot, inputs, pkgs, lib, system }:

_cabalProject: 

{
  name = "quickcheck-dynamic";

  preCommit = {
    cabal-fmt.enable = true;
    fourmolu.enable = true;
    fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
  };
}