/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * checking for false positives and false negatives for infer
 * reporting on NuppPoint exceptions
 */

package codetoanalyze.java.infer;

class A {
  int i;
}

public class NullPointerExceptionsMoreTests {

  int testNullStringDereferencedBad() {
    String s = new String("abc");
    int j = s.length();
    s = null;
    j = j + s.length();
    return 42;
  }

  int testBrachesAvoidNullPointerExceptionOK(int k) {
    String s = new String("abc");
    int j = 100;
    if (k > 10) {
      s = null; /* DANGER? */
    }
    if (k == 10) {
      /* looks as if infer realizes that when k==10,
      we arrived here without executing the DANGER? branch,
      and s is guaranteed not to be null  */
      j = s.length();
    }
    return j;
  }

  int FN_testParameterRec(A arg, int k) {
    /* should have inferrred the precondition that
         a =/= null
    and then should have seen that the recursive call
    does not satisfy that precondition */
    arg.i = 17;
    if (k > 0) {
      return this.FN_testParameterRec(null, k - 1);
    } else {
      return 430;
    }
  }

  int FN_testParameterRecBool(A arg, boolean b) {
    /* should have inferrred the precondition that
         arg =/= null
    and then should have seen that the recursive call
    does not satisfy it */
    arg.i = 17;
    if (b) {
      return this.FN_testParameterRecBool(null, false);
    } else {
      return 430;
    }
  }

  void testParameterOk(A arg) {
    /* this is not an error for infer,
    because it infers that precondition is arg=/=null */
    arg.i = 17;
  }

  int testArithmeticOk(int k) {
    /* error cannot happen due to arithmetic argument */
    String s = new String("abc");
    int j = 100;
    if (k > 10) {
      s = null; /* DANGER? */
    }
    if (k <= 10) {

      /* infer does deduce that ik k>10 then NOT(k<=10), and
      and therefore this branch will be taken only
      if DANGER? was not  */
      j = s.length();
    }
    return j;
  }

  int testArithmeticOneOK(int k) {
    /* error cannot happen due to arithmetic argument  */
    String s = new String("abc");
    int j = 100;
    if (k > 10) {
      s = null; /* DANGER? */
    }
    if ((2 * k) == 20) {
      /* infer does deduce that ik k>10 then 2*k =/=20, and
      and therefore this branch will be taken only
      if DANGER? was not executed */
      j = s.length();
    }
    return j;
  }

  int f_ident(int k) {
    return k;
  }

  int FP_testArithmeticTwo(int k) {
    /* error cannot happen due to arithmetic argument, but infer
    does not see that */
    String s = new String("abc");
    int j = 100;
    if (k > 10) {
      s = null; /* DANGER? */
    }
    if ((f_ident(k)) <= 10) {

      /* looks as if infer does not realize  that if k>10
      then NOT(f_ident(k)<=1o) and therefore we arrived here without executing
      the DANGER? branch, and therefore
      s is guaranteed not to be null  */
      j = s.length();
    }
    return j;
  }
}
