{ mkDerivation, base, containers, lib }:
mkDerivation {
  pname = "aoc2023";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers ];
  license = "unknown";
  mainProgram = "aoc2023";
}
