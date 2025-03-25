{ writeShellApplication
, bash
, bc
, moreutils
, ...
}:

writeShellApplication {
  name = "dev-test-build";
  text = builtins.readFile ./script.sh;
  runtimeInputs = [ bash bc moreutils ];
}
