let
  branches =
    [ "master"
    ];

  convert = nixrev: branch:
    let common = { type = "git"; emailresponsible = false; };
        url    = "https://github.com/thoughtpolice/clash-playground.git ${branch}";
    in { enabled           = 1;
         hidden            = false;
         description       = "${branch} builds";
         nixexprinput      = "clash-playground";
         nixexprpath       = "release.nix";
         checkinterval     = 250;
         schedulingshares  = 100;
         enableemail       = false;
         emailoverride     = "";
         keepnr            = 50;

         inputs =
           { clash-playground = common // { value = url; };
           };
       };
in

{ declInput }:
let
  json = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = import ./nixpkgs.nix { config = {}; };
  spec = builtins.toJSON (pkgs.lib.genAttrs branches (name: convert json.rev name));
in
{
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    ${spec}
    EOF
  '';
}
