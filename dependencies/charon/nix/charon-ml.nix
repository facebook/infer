{ lib
, charon
, fetchFromGitHub
, ocaml-ng
, ocamlPackages
, runCommand
, stdenv
}:
let
  easy_logging = ocamlPackages.buildDunePackage rec {
    pname = "easy_logging";
    version = "0.8.2";
    src = fetchFromGitHub {
      owner = "sapristi";
      repo = "easy_logging";
      rev = "v${version}";
      sha256 = "sha256-Xy6Rfef7r2K8DTok7AYa/9m3ZEV07LlUeMQSRayLBco=";
    };
    buildInputs = [ ocamlPackages.calendar ];
  };

  # We need both `charon-ml` and the `dune-project` file.
  src = lib.cleanSourceWith {
    src = ./..;
    filter =
      path: type:
      (lib.hasPrefix (toString ../charon-ml) path)
      || (lib.hasPrefix (toString ../dune-project) path);
  };

  charon-name_matcher_parser =
    ocamlPackages.buildDunePackage {
      pname = "name_matcher_parser";
      version = "0.1.0";
      duneVersion = "3";
      inherit src;

      nativeBuildInputs = with ocamlPackages; [
        menhir
      ];
      propagatedBuildInputs = with ocamlPackages; [
        ppx_deriving
        visitors
        zarith
        menhirLib
      ];
    };

  charon-ml-check-fmt = stdenv.mkDerivation {
    name = "charon-ml-check-fmt";
    inherit src;

    buildInputs = [
      ocamlPackages.dune_3
      ocamlPackages.ocaml
      ocamlPackages.ocamlformat_0_27_0
    ];
    buildPhase = ''
      if ! dune build @fmt; then
        echo 'ERROR: Ocaml code is not formatted. Run `make format` to format the project files'.
        exit 1
      fi
    '';
    installPhase = "touch $out";
  };

  mk-charon-ml = doCheck:
    ocamlPackages.buildDunePackage {
      pname = "charon";
      version = "0.1.0";
      duneVersion = "3";
      inherit src;

      OCAMLPARAM = "_,warn-error=+A"; # Turn all warnings into errors.
      preCheck =
        if doCheck then ''
          ln -sf ${charon}/tests-llbc charon-ml/tests/test-outputs
        '' else
          "";
      propagatedBuildInputs = with ocamlPackages; [
        core
        ppx_deriving
        visitors
        easy_logging
        zarith
        yojson
        calendar
        charon-name_matcher_parser
        unionFind
        ocaml-ng.ocamlPackages_4_14.ppx_tools # to view the output of visitor derivation
      ];
      inherit doCheck;

      passthru = { inherit charon-ml-tests charon-ml-check-fmt; };
    };

  charon-ml = mk-charon-ml false;
  charon-ml-tests = mk-charon-ml true;

in
charon-ml
