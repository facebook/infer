(set-info :smt-lib-version 2.6)
(set-logic QF_UFLRA)
(set-info :source |CPAchecker with k-induction on SV-COMP14 program using MathSAT5, submitted by Philipp Wendler, http://cpachecker.sosy-lab.org|)
(set-info :category "industrial")
(set-info :status unsat)


(declare-fun ldv_spin@1 () Real)
(declare-fun |main::ldv_s_mpu3050_i2c_driver_i2c_driver@1| () Real)
(define-fun _7 () Real 0)
(define-fun _1937 () Real |main::ldv_s_mpu3050_i2c_driver_i2c_driver@1|)
(define-fun _1938 () Bool (= _1937 _7))
(define-fun _3608 () Real ldv_spin@1)
(define-fun _3609 () Bool (= _3608 _7))
(define-fun _3610 () Bool (and _1938 _3609))
(define-fun _2 () Bool false)



(assert _3610)

(assert _2)
(check-sat)


(exit)
