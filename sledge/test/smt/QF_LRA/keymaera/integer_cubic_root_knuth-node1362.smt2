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

 KeYmaera example: integer_cubic_root_knuth, node 1362 For more info see: No further information available.
|)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun a () Real)
(assert (let ((?v_0 (* 4 a))) (not (= (+ (+ 1 ?v_0) 6) (+ 7 ?v_0)))))
(check-sat)
(exit)
