{ nixpkgs }:

nixpkgs.stdenv.mkDerivation rec {
  name    = "super_prove-${version}";
  version = "2017.08.13";
  src     = nixpkgs.fetchurl {
    url    = "http://downloads.bvsrc.org/super_prove/super_prove-hwmcc17_8-Ubuntu_14.04-Release.tar.gz";
    sha256 = "11gk4d1hpaydvgz36klbpgbxblcsk4906nnvbxcpqzvxgxzi378q";
  };

  buildPhase = "true";
  installPhase = ''
    mkdir -p $out/libexec $out/bin
    mv bin $out/libexec
    mv lib $out/libexec
    cat > $out/bin/suprove <<EOF
    #!${nixpkgs.stdenv.shell}
    tool=super_prove; if [ "x\$1" != "x" ]; then tool="\$1"; shift; fi
    exec $out/libexec/bin/\''${tool}.sh "\$@"
    EOF
    chmod +x $out/bin/suprove
  '';

  checkPhase = "${nixpkgs.stdenv.shell} -n $out/bin/suprove";
}
