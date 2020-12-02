{ mkDerivation, base, hakyll, pandoc, stdenv, src }:
mkDerivation {
  pname = "site";
  version = "0.1.0.0";
  inherit src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll pandoc ];
  homepage = "https://github.com/kierdavis/site";
  description = "Source code of http://kierdavis.com/";
  license = stdenv.lib.licenses.publicDomain;
}
