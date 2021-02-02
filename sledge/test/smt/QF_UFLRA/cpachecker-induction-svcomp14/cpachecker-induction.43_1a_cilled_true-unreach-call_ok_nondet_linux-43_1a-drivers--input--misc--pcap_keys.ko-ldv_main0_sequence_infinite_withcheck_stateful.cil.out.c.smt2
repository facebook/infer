(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with k-induction on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")
(set-info :status unsat)


(declare-fun ldv_spin@1 () Real)
(declare-fun |main::ldv_s_pcap_keys_device_driver_platform_driver@1| () Real)
(define-fun _7 () Real 0)
(define-fun _541 () Real |main::ldv_s_pcap_keys_device_driver_platform_driver@1|)
(define-fun _542 () Bool (= _541 _7))
(define-fun _991 () Real ldv_spin@1)
(define-fun _992 () Bool (= _991 _7))
(define-fun _993 () Bool (and _542 _992))
(define-fun _2 () Bool false)



(assert _993)

(assert _2)
(check-sat)


(exit)
