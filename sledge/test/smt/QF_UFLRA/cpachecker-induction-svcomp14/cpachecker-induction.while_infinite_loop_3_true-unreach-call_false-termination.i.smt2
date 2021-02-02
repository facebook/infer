(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with k-induction on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")
(set-info :status unsat)


(declare-fun x@1 () Real)
(define-fun _7 () Real 0)
(define-fun _43 () Real x@1)
(define-fun _44 () Bool (= _43 _7))
(define-fun _2 () Bool false)



(assert _44)

(assert _2)
(check-sat)


(exit)
