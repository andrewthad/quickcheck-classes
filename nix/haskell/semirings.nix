{ mkDerivation, base, constrictor, containers, deepseq, fetchgit
, hashable, integer-gmp, log-domain, primitive, stdenv
, unordered-containers, vector
}:
mkDerivation {
  pname = "semirings";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/chessai/semirings.git";
    sha256 = "0kp933dkl5nbwlapq9qh0ml4z91h1m9gxvhymsk7v08cl9x8c3pd";
    rev = "b8092bdf815a81cbfbc6e93bb34901bd8bb9f7d9";
  };
  libraryHaskellDepends = [
    base constrictor containers deepseq hashable integer-gmp log-domain
    primitive unordered-containers vector
  ];
  homepage = "https://github.com/chessai/semirings#readme";
  description = "semirings";
  license = stdenv.lib.licenses.bsd3;
}
