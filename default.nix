let stuff = rec {
  generatorSrc = builtins.filterSource (path: type: builtins.elem (baseNameOf path) [
    "src"
    "Main.hs"
    "site.cabal"
    "LICENSE"
  ]) ./.;
  siteSrc = builtins.filterSource (path: type: !builtins.elem (baseNameOf path) [
    "_cache"
    "_site"
    "default.nix"
    "LICENSE"
    "nix"
    "publish.sh"
    "result"
    "site.cabal"
    "src"
    "stack.yaml"
    "stack.yaml.lock"
  ]) ./.;
  nixpkgs = import <nixpkgs> {
    overlays = [(self: super: {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          hakyll = self.callPackage ./nix/haskell/hakyll-4.13.4.1.nix {};
          pandoc = self.callPackage ./nix/haskell/pandoc-2.10.1.nix {};
          pandoc-types = self.callPackage ./nix/haskell/pandoc-types-1.21.nix {};
          skylighting = self.callPackage ./nix/haskell/skylighting-0.8.5.nix {};
          skylighting-core = self.callPackage ./nix/haskell/skylighting-core-0.8.5.nix {};
          site = self.callPackage ./nix/haskell/site.nix { src = generatorSrc; };
        };
      };
    })];
  };
  generator = nixpkgs.haskell.packages.ghc884.site;
  site = nixpkgs.stdenv.mkDerivation {
    name = "site";
    src = siteSrc;
    phases = ["unpackPhase" "buildPhase" "installPhase"];
    buildPhase = "${generator}/bin/site build";
    installPhase = "mv _site $out";
    LANG = "en_GB.UTF-8";
    LOCALE_ARCHIVE = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
  };
  nginxConf = nixpkgs.writeText "nginx.conf" ''
    daemon off;
    user root;
    worker_processes 1;
    events { worker_connections 1024; }
    http {
      include ${nixpkgs.nginx}/conf/mime.types;
      server {
        listen 80;
        root ${site};
        index index.html;
      }
    }
  '';
  passwd = nixpkgs.writeText "passwd" ''
    root:x:0:0:root:/root:${nixpkgs.bash}/bin/bash
  '';
  group = nixpkgs.writeText "group" ''
    root:x:0:
  '';
  imageF = debug: nixpkgs.dockerTools.buildImage {
    name = "site";
    tag = "latest";
    contents = nixpkgs.buildEnv {
      name = "contents";
      paths = if debug then nixpkgs.stdenv.initialPath ++ [ nixpkgs.nginx nixpkgs.strace ] else [];
      pathsToLink = ["/bin"];
      postBuild = ''
        mkdir -p $out{/etc/nginx,/var/cache/nginx,/var/log/nginx}
        ln -sfT ${nginxConf} $out/etc/nginx/nginx.conf
        ln -sfT ${passwd} $out/etc/passwd
        ln -sfT ${group} $out/etc/group
      '';
    };
    config = {
      Entrypoint = ["${nixpkgs.dumb-init}/bin/dumb-init"];
      Cmd = ["${nixpkgs.nginx}/bin/nginx" "-c" "${nginxConf}"];
    };
  };
  image = imageF false;
  debugImage = imageF true;
}; in stuff.image // { passthru = stuff; }
