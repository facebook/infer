(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with k-induction on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")
(set-info :status unsat)


(declare-fun ldv_spin@1 () Real)
(declare-fun |main::ldv_s_ab8500_ponkey_driver_platform_driver@1| () Real)
(define-fun _7 () Real 0)
(define-fun _299 () Real |main::ldv_s_ab8500_ponkey_driver_platform_driver@1|)
(define-fun _300 () Bool (= _299 _7))
(define-fun _535 () Real ldv_spin@1)
(define-fun _536 () Bool (= _535 _7))
(define-fun _537 () Bool (and _300 _536))
(define-fun _2 () Bool false)



(assert _537)

(assert _2)
(check-sat)


(exit)
