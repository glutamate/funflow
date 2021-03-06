let
  default = import ./default.nix {};
  required-packages = with default; [
    cas-hashable.components.library
    cas-hashable-s3.components.library
    cas-store.components.library
    funflow.components.library
    funflow.components.exes
    funflow-aws.components.library
    funflow-checkpoints.components.library
    funflow-cwl.components.library
    funflow-examples.components.exes
    funflow-jobs.components.library
  ];
  required-tests = with default; [
    cas-store.checks
    funflow.checks
    funflow-checkpoints.checks
  ];
in {
  inherit required-packages;
  inherit required-tests;
}
