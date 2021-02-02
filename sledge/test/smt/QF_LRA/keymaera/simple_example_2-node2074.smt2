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

 KeYmaera example: simple_example_2, node 2074 For more info see: No further information available.
|)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun e () Real)
(declare-fun buscore2dollarskuscore0 () Real)
(declare-fun auscore2dollarskuscore0 () Real)
(declare-fun cuscore2dollarskuscore0 () Real)
(assert (let ((?v_0 (* 5 auscore2dollarskuscore0)) (?v_1 (* 3 buscore2dollarskuscore0))) (not (=> (and (= e 0) (= (+ (+ ?v_0 ?v_1) cuscore2dollarskuscore0) 10)) (= (+ (+ (- ?v_0 5) (+ ?v_1 6)) (- cuscore2dollarskuscore0 1)) 10)))))
(check-sat)
(exit)
