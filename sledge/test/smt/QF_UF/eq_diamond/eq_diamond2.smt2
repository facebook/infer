(set-info :smt-lib-version 2.6)
(set-logic QF_UF)
(set-info :source |
Generating minimum transitivity constraints in P-time for deciding Equality Logic,
Ofer Strichman and Mirron Rozanov,
SMT Workshop 2005.

Translator: Leonardo de Moura. |)
(set-info :category "crafted")
(set-info :status unsat)
(declare-sort U 0)
(declare-fun x0 () U)
(declare-fun y0 () U)
(declare-fun z0 () U)
(declare-fun x1 () U)
(declare-fun y1 () U)
(declare-fun z1 () U)
(assert (and (or (and (= x0 y0) (= y0 x1)) (and (= x0 z0) (= z0 x1))) (not (= x0 x1))))
(check-sat)
(exit)
