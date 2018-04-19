{ mkDerivation, base, constrictor, containers, deepseq, fetchgit
, hashable, integer-gmp, stdenv, unordered-containers, vector
}:
mkDerivation {
  pname = "semirings";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/chessai/semirings.git";
    sha256 = "0l4crfpbvrf3mgh25lwzm1gsvn3mqihwrdprnpfdbf5hr1q8mrfk";
    rev = "799274b32d40ba72d9fdf5321ca3a6a655ac0f35";
  };
  libraryHaskellDepends = [
    base constrictor containers deepseq hashable integer-gmp
    unordered-containers vector
  ];
  homepage = "http://github.com/chessai/semirings";
  description = "smush those monoids";
  license = stdenv.lib.licenses.bsd3;
}
