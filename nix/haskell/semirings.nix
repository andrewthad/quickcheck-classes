{ mkDerivation, base, containers, fetchgit, hashable, integer-gmp
, stdenv, unordered-containers, vector
}:
mkDerivation {
  pname = "semirings";
  version = "0.1.2";
  src = fetchgit {
    url = "https://github.com/chessai/semirings.git";
    sha256 = "18nqyk08cf382dsjdhvn00znivmlliqsdma799l9ljw601np24l8";
    rev = "df5536a0d11c764fa158db4908a5d3ab1b483e02";
  };
  libraryHaskellDepends = [
    base containers hashable integer-gmp unordered-containers vector
  ];
  homepage = "http://github.com/chessai/semirings";
  description = "two monoids as one, in holy haskimony";
  license = stdenv.lib.licenses.bsd3;
}
