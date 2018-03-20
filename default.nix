{ package ? "quickcheck-classes", compiler ? "ghc822" }:
let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "01705125314fa0c7753f27c3dd7c4bfbda55c375"; 
    sha256 = "1a96vb4hlhnadm445lifq02wg2vz0a2hyxrcl6d0jy2cn7427aq6"; 
  };
  pkgs = import nixpkgs { config = {}; overlays = []; };
  inherit (pkgs) haskell;

  
  filterPredicate = p: type:
    let path = baseNameOf p; in !(
       (type == "directory" && path == "dist")
    || (type == "symlink"   && path == "result")
    || (type == "directory" && path == ".git")
    || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
    || pkgs.lib.hasSuffix "~" path
    || pkgs.lib.hasSuffix ".o" path
    || pkgs.lib.hasSuffix ".so" path
    || pkgs.lib.hasSuffix ".nix" path);
    
  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {}; 
         };

    {
      #mkDerivation = args: super.mkDerivation (args // {
        #doBenchmark = pkgs.lib.elem args.pname [ "quickcheck-classes" ]; 
        #doCheck = pkgs.lib.elem args.pname [ "quickcheck-classes" ]; 
        #doHaddock = false;
      #});
      
      quickcheck-classes = overrideCabal (build "quickcheck-classes" ./.) (drv: {
        doBenchmark = true;
        doCheck = true;
        doHaddock = false;
      });
    };
  };
in rec {
  drv = overrides.${package};
  quickcheck-classes = if pkgs.lib.inNixShell then drv.env else drv;
}
