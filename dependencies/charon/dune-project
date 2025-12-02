(lang dune 3.7)

(using menhir 2.1)

(map_workspace_root false)

(name charon)

(version 0.1)

(generate_opam_files true)

(source
 (uri git+https://github.com/AeneasVerif/charon.git))

(homepage "https://github.com/AeneasVerif/charon")

(bug_reports "https://github.com/AeneasVerif/charon/issues")

(authors "Son Ho" "Jonathan Protzenko" "Aymeric Fromherz" "Sidney Congard")

(maintainers
 "Son Ho"
 "Jonathan Protzenko"
 "Aymeric Fromherz"
 "Sidney Congard")

(license Apache-2.0)

(package
 (name charon)
 (synopsis
  "Manipulation of Low-level Borrow Calculus, a MIR-like representation for Rust Code")
 (description
  "\n   This library allows for manipulation of LLBC, the language output by\n   Charon. Charon acts as an interface between the rustc compiler and program\n   verification projects.  Its purpose is to process Rust .rs files and convert\n   them into files easy to handle by program verifiers.")
 (depends
  easy_logging
  ppx_deriving
  unionFind
  visitors
  yojson
  zarith
  name_matcher_parser
  (ocamlformat :with-test)))

(package
 (name name_matcher_parser)
 (synopsis "Parser to define name matchers")
 (description "")
 (depends
  (menhir :build)
  menhirLib
  ppx_deriving
  visitors
  zarith))
