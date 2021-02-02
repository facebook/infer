(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with bounded model checking on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")


(define-fun _2 () Bool false)


(push 1)
(assert _2)
(set-info :status unsat)
(check-sat)
(pop 1)
(push 1)
(assert _2)
(set-info :status unsat)
(check-sat)
(pop 1)
(exit)
