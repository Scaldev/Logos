# Dynamic Epistemic Structures as First-order Interpretations

First-Order Dynamic Epistemic Logic (FODEL) structures arise from the iterative update of an
initial model by event triggering. Such structures are of utter importance for automated planning
reasonning and solving. On the other hand, First-Order Interpretations (FOI) relate relational
structures in a formal way by means of reinterpreting a structure into another.

This project is about the relationship between the FODEL update mechanism and FOI.

### 

Use `dune build` and `dune exec main` to execute the project. Use `dun runtest` to run tests.

The following alias allows you to use `bisect-clean-test` at the root of the project in order to test coverage.

```sh
alias bisect-clean-test='find . -name "*.coverage" | xargs rm -f && dune runtest --instrument-with bisect_ppx --force && bisect-ppx-report html'
```