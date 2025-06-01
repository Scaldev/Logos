(lang dune 3.18)

(name Logos)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Scaldev <scaldev@gmail.com>")

(maintainers "Scaldev <scaldev@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name Logos)
 (synopsis "A library about the relationship between the FODEL Update mechanism and First-Order Interpretations.")
 (description "First-Order Dynamic Epistemic Logic (FODEL) structures arise from the iterative update of an initial model by event triggering. Such structures are of utter importance for automated planning reasonning and solving. On the other hand, First-Order Interpretations (FOI) relate relational structures in a formal way by means of reinterpreting a structure into another.")
 (depends ocaml)
 (tags ("logic" "first-order"))
)