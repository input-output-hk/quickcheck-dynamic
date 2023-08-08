# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#38-nixformattersnix

{
  cabal-fmt.enable = true;

  fourmolu.enable = true;
  fourmolu.extraOptions = "-o -XImportQualifiedPost -o -XTypeApplications -o -XPatternSynonyms";
}