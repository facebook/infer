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

 KeYmaera example: simple_example_1, node 2318 For more info see: No further information available.
|)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun e () Real)
(declare-fun buscore2dollarskuscore1 () Real)
(declare-fun duscore2dollarskuscore1 () Real)
(declare-fun cuscore2dollarskuscore1 () Real)
(declare-fun auscore2dollarskuscore1 () Real)
(assert (not (=> (and (and (= e 0) (= (* 2 auscore2dollarskuscore1) buscore2dollarskuscore1)) (= cuscore2dollarskuscore1 duscore2dollarskuscore1)) (= (+ cuscore2dollarskuscore1 1) (+ duscore2dollarskuscore1 1)))))
(check-sat)
(exit)
