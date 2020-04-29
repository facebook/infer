/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.util.Iterator;

public class AnalysisStops {

  private native Object externalFunc();

  public void skipPointerDerefMayCauseLocalFalseNegative() {
    Object ret = externalFunc();
    ret.toString();
    int i = 1 / 0;
  }

  private Object skipPointerDerefPreventsSpecInferenceRetObj() {
    Object ret = externalFunc();
    ret.toString();
    return new Object();
  }

  public void skipPointerDerefMayCauseCalleeFalsePositive() {
    Object o = skipPointerDerefPreventsSpecInferenceRetObj();
    o.toString();
  }

  private int skipPointerDerefPreventsSpecInferenceRetZero() {
    Object ret = externalFunc();
    ret.toString();
    return 0;
  }

  public void skipPointerDerefMayCauseCalleeFalseNegative() {
    int ret = skipPointerDerefPreventsSpecInferenceRetZero();
    int i = 1 / ret;
  }

  private void divideByParam(int i) {
    int j = 1 / i;
  }

  public void skipPointerDerefMayCauseInterprocFalseNegative() {
    int i = skipPointerDerefPreventsSpecInferenceRetZero();
    divideByParam(i);
  }

  private String castExternalPreventsSpecInference() {
    return (String) externalFunc();
  }

  public void castFailureOnUndefinedObjMayCauseFalseNegative() {
    castExternalPreventsSpecInference();
    int i = 1 / 0;
  }

  public void callOnCastUndefinedObjMayCauseFalseNegative() {
    String s = castExternalPreventsSpecInference();
    s.toString();
    int i = 1 / 0;
  }

  private static class MyObj {
    Object f;
    MyObj rec;
    int i;

    public int retOne() {
      return 1;
    }

    public int retZero() {
      return 0;
    }
  }

  private native MyObj externalFunc2();

  public void callOnUndefinedObjMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    int i = 1 / ret.retZero();
  }

  public void callOnUndefinedObjMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    int i = 1 / ret.retOne();
  }

  public void fieldWriteOnUndefinedObjMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    ret.f = new Object();
    int i = 1 / 0;
  }

  public void fieldWriteOnUndefinedObjMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    ret.f = new Object();
    ret.f.toString();
  }

  public void fieldReadOnUndefinedObjMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    Object o = ret.f;
    int i = 1 / 0;
  }

  public void fieldReadOnUndefinedObjMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    Object o = ret.f;
    o.toString();
  }

  public void recursiveAngelicTypesMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    MyObj rec1 = ret.rec;
    MyObj rec2 = rec1.rec;
    int i = 1 / 0;
  }

  public void recursiveAngelicTypesMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    MyObj rec1 = ret.rec;
    rec1.rec.toString();
  }

  public void infiniteMaterializationMayCauseFalseNegative(boolean b) {
    MyObj rec = externalFunc2();
    while (b) {
      rec = rec.rec;
    }
    int i = 1 / 0;
  }

  public void infiniteMaterializationMayCauseFalsePositive(boolean b) {
    MyObj rec = externalFunc2();
    while (b) {
      rec = rec.rec;
    }
    rec.toString();
  }

  public void primitiveFieldOfAngelicObjMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    if (ret.i == 0) {
      int i = 1 / 0;
    } else {
      int i = 1 / 0;
    }
  }

  public void primitiveFieldOfAngelicObjMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    if (ret.i != 0) {
      int i = 1 / ret.i;
    }
  }

  public void heapFieldOfAngelicObjMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    Object obj = ret.f;
    if (obj == ret.f) {
      int i = 1 / 0;
    }
  }

  public void heapFieldOfAngelicObjMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    Object obj = ret.f;
    if (obj != ret.f) {
      int i = 1 / 0;
    }
  }

  public void fieldReadAferCastMayCauseFalseNegative(Iterator<MyObj> iter) {
    MyObj ret = iter.next();
    Object obj = ret.f;
    obj.toString();
    int i = ret.i;
    if (i == 7) {
      int j = 1 / 0;
    }
  }

  public void derefParam(MyObj obj) {
    Object f = obj.f;
    f.toString();
  }

  public void fieldReadInCalleeMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    derefParam(ret);
  }

  public void fieldReadInCalleeMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    ret.f = null;
    derefParam(ret);
  }

  public void fieldReadInCalleeWithAngelicObjFieldMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    derefParam(ret.rec);
  }

  public void fieldReadInCalleeWithAngelicObjFieldMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    ret.rec.f = null;
    derefParam(ret.rec);
  }

  public void accessPathOnParam(MyObj obj) {
    MyObj ret = obj.rec;
    Object f = ret.f;
    f.toString();
  }

  public void accessPathInCalleeMayCauseFalsePositive() {
    MyObj ret = externalFunc2();
    accessPathOnParam(ret);
  }

  public void accessPathInCalleeMayCauseFalseNegative() {
    MyObj ret = externalFunc2();
    ret.rec.f = null;
    accessPathOnParam(ret);
  }

  public void skipFunctionInLoopMayCauseFalseNegative() {
    Object o = null;
    for (int i = 0; i < 10; i++) {
      externalFunc();
    }
    o.toString();
  }

  // will fail to find error unless spec inference succeeds for all callees
  public void specInferenceMayFailAndCauseFalseNegative(boolean b, Iterator<MyObj> iter) {
    skipPointerDerefMayCauseLocalFalseNegative();
    skipPointerDerefPreventsSpecInferenceRetObj();
    skipPointerDerefPreventsSpecInferenceRetZero();
    skipPointerDerefMayCauseCalleeFalseNegative();
    skipPointerDerefMayCauseInterprocFalseNegative();
    castFailureOnUndefinedObjMayCauseFalseNegative();
    callOnCastUndefinedObjMayCauseFalseNegative();
    callOnUndefinedObjMayCauseFalseNegative();
    callOnUndefinedObjMayCauseFalsePositive();
    fieldWriteOnUndefinedObjMayCauseFalseNegative();
    fieldWriteOnUndefinedObjMayCauseFalsePositive();
    fieldReadOnUndefinedObjMayCauseFalseNegative();
    fieldReadOnUndefinedObjMayCauseFalsePositive();
    recursiveAngelicTypesMayCauseFalseNegative();
    recursiveAngelicTypesMayCauseFalsePositive();
    infiniteMaterializationMayCauseFalseNegative(b);
    infiniteMaterializationMayCauseFalsePositive(b);
    primitiveFieldOfAngelicObjMayCauseFalsePositive();
    primitiveFieldOfAngelicObjMayCauseFalseNegative();
    heapFieldOfAngelicObjMayCauseFalsePositive();
    heapFieldOfAngelicObjMayCauseFalseNegative();
    fieldReadAferCastMayCauseFalseNegative(iter);
    fieldReadInCalleeMayCauseFalsePositive();
    fieldReadInCalleeWithAngelicObjFieldMayCauseFalsePositive();
    accessPathInCalleeMayCauseFalsePositive();
    int i = 1 / 0;
  }
}
