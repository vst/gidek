{ writeShellScriptBin, ... }:

writeShellScriptBin "dev-build-static" (builtins.readFile ./script.sh)
