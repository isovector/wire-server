# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation, base, extra, gitignoreSource, imports, lib
, swagger2
}:
mkDerivation {
  pname = "deriving-swagger2";
  version = "0.1.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [ base extra imports swagger2 ];
  license = lib.licenses.agpl3Only;
}