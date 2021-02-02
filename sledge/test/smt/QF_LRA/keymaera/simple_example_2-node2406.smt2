(set-info :smt-lib-version 2.6)
(set-logic QF_LRA)
(set-info :source |
These benchmarks used in the paper:

  Dejan Jovanovic and Leonardo de Moura.  Solving Non-Linear Arithmetic.
  In IJCAR 2012, published as LNCS volume 7364, pp. 339--354.

The keymaera family contains VCs from Keymaera verification, see:

  A. Platzer, J.-D. Quesel, and P. Rummer.  Real world verification.
  In CADE 2009, pages 485-501. Springer, 2009.

Submitted by Dejan Jovanovic for SMT-LIB.

 KeYmaera example: simple_example_2, node 2406 For more info see: No further information available.
|)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun e () Real)
(declare-fun buscore2dollarskuscore1 () Real)
(declare-fun cuscore2dollarskuscore1 () Real)
(declare-fun auscore2dollarskuscore1 () Real)
(assert (let ((?v_0 (* 5 auscore2dollarskuscore1)) (?v_1 (* 3 buscore2dollarskuscore1))) (not (=> (= (+ (+ ?v_0 ?v_1) cuscore2dollarskuscore1) 10) (or (= e 0) (= (+ (+ (+ ?v_0 5) (- ?v_1 3)) (- cuscore2dollarskuscore1 2)) 10))))))
(check-sat)
(exit)
