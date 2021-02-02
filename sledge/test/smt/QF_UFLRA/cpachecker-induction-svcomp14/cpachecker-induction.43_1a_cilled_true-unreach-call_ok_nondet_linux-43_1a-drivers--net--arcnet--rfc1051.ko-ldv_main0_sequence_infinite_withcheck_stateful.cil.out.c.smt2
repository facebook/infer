(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with k-induction on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")
(set-info :status unsat)


(declare-fun |arcnet_rfc1051_init::___cpa_temp_result_var_@1| () Real)
(declare-fun |main::tmp@1| () Real)
(declare-fun ldv_spin@1 () Real)
(define-fun _7 () Real 0)
(define-fun _4288 () Real ldv_spin@1)
(define-fun _4289 () Bool (= _4288 _7))
(define-fun _7063 () Real |arcnet_rfc1051_init::___cpa_temp_result_var_@1|)
(define-fun _7064 () Bool (= _7063 _7))
(define-fun _7065 () Real |main::tmp@1|)
(define-fun _7066 () Bool (= _7063 _7065))
(define-fun _7067 () Bool (and _4289 _7066))
(define-fun _7068 () Bool (and _7064 _7067))
(define-fun _2 () Bool false)



(assert _7068)

(assert _2)
(check-sat)


(exit)
