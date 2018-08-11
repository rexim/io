let
  pkgs = import <nixpkgs> {};
  customTex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-basic
      minted
      fancyvrb
      float
      fvextra
      ifplatform
      etoolbox
      xstring
      lineno
      framed
      xcolor
      beamer
      upquote
      ms
      textpos
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
