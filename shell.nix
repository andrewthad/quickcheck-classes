{ package ? "quickcheck-classes", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).quickcheck-classes
