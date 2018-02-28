{ package ? "quickcheck-classes", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).quickcheck-classes
