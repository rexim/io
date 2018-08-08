let
  pkgs = import <nixpkgs> {};
  customTex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-basic
      minted
      fancyvrb
      float
      ifplatform
      etoolbox
      xstring
      lineno
      framed
      xcolor
      beamer
      ms
      csquotes;
  };
in rec {
  lhsEnv = pkgs.stdenv.mkDerivation {
    name = "lhs-env";
    buildInputs = [
                    pkgs.stdenv
                    pkgs.ghc
                    pkgs.stack
                    pkgs.cabal-install
                    pkgs.python27Packages.pygments
                    pkgs.graphviz
                    customTex
                  ];
  };
}
