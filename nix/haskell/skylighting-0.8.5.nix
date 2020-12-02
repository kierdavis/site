{ mkDerivation, base, binary, bytestring, containers
, skylighting-core, stdenv
}:
mkDerivation {
  pname = "skylighting";
  version = "0.8.5";
  sha256 = "5ba30911f5718ec4c5e76d160c358892b301b3ea58d009cb0506dcabaf0615ad";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers skylighting-core
  ];
  homepage = "https://github.com/jgm/skylighting";
  description = "syntax highlighting library";
  license = stdenv.lib.licenses.gpl2;
}
