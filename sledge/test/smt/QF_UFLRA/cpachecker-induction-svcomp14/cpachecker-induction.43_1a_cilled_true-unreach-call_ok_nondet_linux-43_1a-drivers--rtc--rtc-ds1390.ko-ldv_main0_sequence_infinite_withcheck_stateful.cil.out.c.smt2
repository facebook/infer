(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with k-induction on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")
(set-info :status unsat)


(declare-fun ldv_spin@1 () Real)
(declare-fun |main::ldv_s_ds1390_driver_spi_driver@1| () Real)
(define-fun _7 () Real 0)
(define-fun _1303 () Real |main::ldv_s_ds1390_driver_spi_driver@1|)
(define-fun _1304 () Bool (= _1303 _7))
(define-fun _2713 () Real ldv_spin@1)
(define-fun _2714 () Bool (= _2713 _7))
(define-fun _2715 () Bool (and _1304 _2714))
(define-fun _2 () Bool false)



(assert _2715)

(assert _2)
(check-sat)


(exit)
