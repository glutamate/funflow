let 
  default = import ./default.nix {};
  required-packages = with default; [
    funflow.components.library
    funflow.components.exes
    funflow-aws.components.library
    funflow-checkpoints.components.library
    funflow-cwl.components.library
    funflow-examples.components.exes
    funflow-jobs.components.library
  ];
  required-tests = with default; [
    funflow.components.tests.unit-tests
    funflow-checkpoints.components.tests.unit-tests
  ];
in {
  inherit required-packages;
  inherit required-tests;
}