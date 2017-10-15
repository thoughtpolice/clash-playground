{ nixpkgs }:

let
  fpgaTools = rec {
    ## -----------------------------------------------------------------------
    ## -- Verification tools: SMT, Icarus Verilog, etc

    suprove = nixpkgs.stdenv.mkDerivation rec {
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
    };

    yices = nixpkgs.yices.overrideAttrs (old: rec {
      name    = "yices-${version}";
      version = "2.5.3";
      src = nixpkgs.fetchFromGitHub {
        owner = "sri-csl";
        repo = "yices2";
        rev = "2502a30dce7786223e06c1fe12d889871567c100";
        sha256 = "1sv5zmpydi5clmh99ciwad3jz2s8h9ip0a9acp1gfnbrpq991gbx";
      };
    });

    symbiyosys = nixpkgs.stdenv.mkDerivation rec {
      name = "symbiyosys-${version}";
      version = "2017.09.01";
      src = nixpkgs.fetchFromGitHub {
        owner  = "cliffordwolf";
        repo   = "symbiyosys";
        rev    = "68d90a55104c0c681fcb6aa02a26a83ff92367b8";
        sha256 = "1c2zwd7403r5f3wpjrmvaa1qq9g7cpbzhblbc1nkizgr0wa5fiks";
      };

      buildPhase = "true";
      installPhase = ''
        mkdir -p $out/bin $out/share/yosys/python3/
        cp sbysrc/sby_*.py $out/share/yosys/python3/
        cp sbysrc/sby.py $out/bin/sby
        chmod +x $out/bin/sby

        substituteInPlace $out/bin/sby \
          --replace "##yosys-sys-path##" \
                    "sys.path += [p + \"/share/yosys/python3/\" for p in [\"$out\", \"${nixpkgs.pkgs.yosys}\"]]"
      '';
    };
  };
in fpgaTools
