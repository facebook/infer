(lang dune 2.7)

(name parmap)

(generate_opam_files true)

(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(authors "Roberto Di Cosmo <roberto@dicosmo.org>")

(maintainers "Roberto Di Cosmo <roberto@dicosmo.org>")

(homepage "https://github.com/rdicosmo/parmap")

(bug_reports "https://github.com/rdicosmo/parmap/issues")

(source (github rdicosmo/parmap))

(package
 (name parmap)
 (synopsis "Minimalistic library allowing to exploit multicore architecture")
 (description
   "Parmap is a minimalistic library allowing to exploit multicore\narchitecture for OCaml programs with minimal modifications: if you\nwant to use your many cores to accelerate an operation which happens\nto be a map, fold or map/fold (map-reduce), just use Parmap\226\128\153s parmap,\nparfold and parmapfold primitives in place of the standard List.map\nand friends, and specify the number of subprocesses to use by the\noptional parameter ~ncores.")
 (depends
  dune-configurator
  base-bigarray
  base-unix
  (ocaml
  (>= 4.03.0)))
 (conflicts
  ocaml-option-no-flat-float-array))
