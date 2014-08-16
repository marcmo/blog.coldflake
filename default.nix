{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_3
    text mtl transformers hakyll; # Haskell dependencies here

in cabal.mkDerivation (self: {
  pname = "coldflake-blog";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    # As imported above
    text mtl transformers hakyll
  ];
  buildTools = [ cabalInstall_1_18_0_3 ];
  enableSplitObjs = false;
})


