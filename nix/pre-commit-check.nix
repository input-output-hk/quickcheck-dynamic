{
  cabal-fmt.enable = true;

  nixpkgs-fmt.enable = true;

  fourmolu.enable = true;
  fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
}
