(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with k-induction on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")
(set-info :status unsat)


(declare-fun |main::x@1| () Real)
(declare-fun |main::y@1| () Real)
(define-fun _19 () Real 63)
(define-fun _23 () Real 18)
(define-fun _147 () Real |main::y@1|)
(define-fun _148 () Bool (= _147 _23))
(define-fun _149 () Real |main::x@1|)
(define-fun _150 () Bool (= _149 _19))
(define-fun _151 () Bool (and _148 _150))
(define-fun _2 () Bool false)



(assert _151)

(assert _2)
(check-sat)


(exit)
