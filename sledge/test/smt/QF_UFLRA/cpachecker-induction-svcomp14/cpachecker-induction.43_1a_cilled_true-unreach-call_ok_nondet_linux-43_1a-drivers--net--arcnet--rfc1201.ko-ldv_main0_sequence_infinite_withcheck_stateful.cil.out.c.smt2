(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with k-induction on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")
(set-info :status unsat)


(declare-fun |main::tmp@1| () Real)
(declare-fun ldv_spin@1 () Real)
(declare-fun |arcnet_rfc1201_init::___cpa_temp_result_var_@1| () Real)
(define-fun _7 () Real 0)
(define-fun _9494 () Real |main::tmp@1|)
(define-fun _9495 () Real |arcnet_rfc1201_init::___cpa_temp_result_var_@1|)
(define-fun _9496 () Bool (= _9494 _9495))
(define-fun _9497 () Bool (= _9495 _7))
(define-fun _9498 () Real ldv_spin@1)
(define-fun _9499 () Bool (= _9498 _7))
(define-fun _9500 () Bool (and _9497 _9499))
(define-fun _9501 () Bool (and _9496 _9500))
(define-fun _2 () Bool false)



(assert _9501)

(assert _2)
(check-sat)


(exit)
