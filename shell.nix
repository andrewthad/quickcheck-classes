{ package ? "quickcheck-classes", compiler ? "ghc742" }:

(import ./default.nix {
  inherit package compiler;
}).quickcheck-classes
