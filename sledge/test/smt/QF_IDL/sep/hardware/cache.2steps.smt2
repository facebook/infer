(set-info :smt-lib-version 2.6)
(set-logic QF_IDL)
(set-info :source |
Source unknown
This benchmark was automatically translated into SMT-LIB format from
CVC format using CVC Lite
|)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun cvclZero () Int)
(declare-fun CVC_c_cl0 () Int)
(declare-fun CVC_c_c0 () Int)
(declare-fun CVC_c1 () Int)
(declare-fun CVC_c2 () Int)
(declare-fun CVC_V_c_ArbVal_1 () Int)
(declare-fun CVC_V_c_ArbVal_2 () Int)
(declare-fun CVC___en_cache_state () Int)
(declare-fun CVC_V_c_newCl_1 () Int)
(declare-fun CVC___en_message () Int)
(assert (let ((?v_0 (= (- CVC_c1 CVC_V_c_ArbVal_1) 0)) (?v_1 (= (- CVC_c_c0 CVC_V_c_newCl_1) 0)) (?v_3 (and (= (- CVC_V_c_ArbVal_1 CVC_c_cl0) 0) false))) (let ((?v_2 (ite ?v_3 false false)) (?v_6 (ite false false true))) (let ((?v_7 (and ?v_2 (ite (not (ite ?v_1 true (= (- CVC_V_c_ArbVal_1 CVC_V_c_ArbVal_2) 0))) true ?v_6))) (?v_8 (ite ?v_3 true false)) (?v_4 (ite (not (ite ?v_1 ?v_0 (= (- CVC_c1 CVC_V_c_ArbVal_2) 0))) false (ite false false (ite false false (ite false true false))))) (?v_5 (= (- CVC_c2 CVC_V_c_ArbVal_1) 0))) (let ((?v_9 (ite (not (ite ?v_1 ?v_5 (= (- CVC_c2 CVC_V_c_ArbVal_2) 0))) true (ite false true (ite false false ?v_6))))) (not (or (not (and (not (= (- CVC_c1 CVC_c2) 0)) (ite (not ?v_0) ?v_4 (ite ?v_7 false (ite ?v_2 false (ite ?v_8 true ?v_4)))))) (ite (not ?v_5) ?v_9 (ite ?v_7 true (ite ?v_2 false (ite ?v_8 false ?v_9)))))))))))
(check-sat)
(exit)
