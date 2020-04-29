/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import android.app.AlarmManager;
import android.app.PendingIntent;
import com.facebook.infer.annotation.Assertions;
import com.google.common.base.Preconditions;
import com.google.common.base.Verify;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import javax.annotation.Nullable;

public class NullMethodCall {

  void callOnNull() {
    String s = null;
    int n = s.length();
  }

  void callOnEmptyString() {
    String s = "";
    int n = s.length();
  }

  void callAfterYodaCondition(@Nullable String s) {
    if (null != s) {
      int n = s.length();
    }
  }

  int objectLength(@Nullable Object o) {
    if (o instanceof String) {
      String s = (String) o;
      return s.length(); // OK: s cannot be null because of instanceof
    }
    return 0;
  }

  int testCheckState(@Nullable String s1, @Nullable String s2) {
    Preconditions.checkState(s1 != null && s2 != null, "bad");
    return s1.length() + s2.length();
  }

  int testPrivateStaticInnerClassField() {
    String s;
    S.sfld = "abc";
    s = S.sfld;
    return s.length();
  }

  private static class S {
    private static @Nullable String sfld;
  }

  @Nullable String fld;
  private @Nullable String pfld;

  public class Inner {
    int outerField() {
      String s = fld;
      return s.length();
    }

    int outerFieldInitialized() {
      fld = "abc";
      String s = fld;
      return s.length();
    }

    int outerPrivateField() {
      String s = pfld;
      return s.length();
    }

    int outerPrivateFieldInitialized() {
      pfld = "abc";
      String s = pfld;
      return s.length();
    }

    int outerPrivateFieldCheckNotNull() {
      Preconditions.checkNotNull(pfld);
      String s = pfld;
      return s.length();
    }

    int outerPrivateFieldCheckState() {
      Preconditions.checkState(pfld != null);
      String s = pfld;
      return s.length();
    }

    int outerPrivateFieldAssertNotNull() {
      Assertions.assertNotNull(pfld);
      String s = pfld;
      return s.length();
    }

    int outerPrivateFieldAssumeNotNull() {
      Assertions.assumeNotNull(pfld);
      String s = pfld;
      return s.length();
    }

    int outerPrivateFieldAssertCondition() {
      Assertions.assertCondition(pfld != null, "explanation");
      String s = pfld;
      return s.length();
    }

    int outerPrivateFieldAssumeCondition() {
      Assertions.assumeCondition(pfld != null, "explanation");
      String s = pfld;
      return s.length();
    }

    int outerPrivateFieldCheckStateYoda() {
      Preconditions.checkState(null != pfld);
      String s = pfld;
      return s.length();
    }

    String outerFieldGuardPrivate() {
      if (pfld != null) return pfld.toString();
      return "";
    }

    String outerFieldGuardPublic() {
      if (fld != null) return fld.toString();
      return "";
    }

    public class InnerInner {
      int outerouterPrivateFieldInitialized() {
        pfld = "abc";
        String s = pfld;
        return s.length();
      }
    }
  }

  @Nullable
  String getNullable() {
    return null;
  }

  void testVariableAssigmentInsideConditional() {
    String s = null;
    if ((s = getNullable()) != null) {
      int n = s.length();
    }
  }

  void testFieldAssigmentInsideConditional() {
    if ((fld = getNullable()) != null) {
      int n = fld.length();
    }
  }

  String abc = "abc";

  void testFieldAssignmentIfThenElse(String name) {
    String s = (name.length() == 0) ? null : abc;
    int n = s.length();
  }

  static String throwsExn() throws java.io.IOException {
    throw new java.io.IOException();
  }

  void testExceptionPerInstruction(int z) throws java.io.IOException {
    String s = null;

    try {
      s = throwsExn();
    } finally {
      int n = s.length();
    }
  }

  public class InitializeAndExceptions {
    String s;

    String bad() throws java.io.IOException {
      throw new java.io.IOException();
    }

    InitializeAndExceptions() throws java.io.IOException {
      s = bad(); // should not report field not initialized
    }
  }

  public class InitializeViaPrivateMethod {
    String name;

    private void reallyInitName(String s) {
      name = s;
    }

    private void initName(String s) {
      reallyInitName(s);
    }

    InitializeViaPrivateMethod() {
      initName("abc");
    }
  }

  class CheckNotNullVararg {
    void checkNotNull(String msg, Object... objects) {}

    void testCheckNotNullVaratg(@Nullable String s1, @Nullable String s2) {
      checkNotNull("hello", s1, s2);
      s1.isEmpty();
      s2.isEmpty();
    }

    void testRepeatedCheckNotNull(@Nullable String s) {
      checkNotNull("abc", s);
      checkNotNull("abc", s.toString());
      s.toString().isEmpty();
    }
  }

  public void testSystemGetPropertyReturn() {
    String s = System.getProperty("");
    int n = s.length();
  }

  int testSystemGetenvBad() {
    String envValue = System.getenv("WHATEVER");
    return envValue.length();
  }

  class SystemExitDoesNotReturn {
    native boolean whoknows();

    void testOK() {
      String s = null;
      if (whoknows()) {
        s = "a";
      } else {
        System.exit(1);
      }
      int n = s.length();
    }
  }

  public void testMapGetBad(
      Map<String, String> m, HashMap<String, String> hm, ConcurrentHashMap<String, String> chm) {
    m.get("foo").toString();
    hm.get("foo").toString();
    chm.get("foo").toString();
  }

  public void testMapRemoveBad(
      Map<String, String> m, HashMap<String, String> hm, ConcurrentHashMap<String, String> chm) {
    m.remove("foo").toString();
    hm.remove("foo").toString();
    chm.remove("foo").toString();
  }

  @Nullable Object nullableField;

  void FP_propagatesNonNullAfterComparisonFieldOkay(Object nonNullObject) {
    if (nullableField == nonNullObject) {
      nullableField.toString();
    }
  }

  void FP_propagatesNonNullAfterComparisonParameterOkay(
      @Nullable Object nullableParameter, Object nonNullParameter) {
    if (nullableParameter == nonNullParameter) {
      nullableParameter.toString();
    }
  }

  String customPreconditionsCheckNotNullOkay() {
    MyPreconditions.checkNotNull(nullableField);
    return nullableField.toString();
  }

  String customPreconditionsCheckStateOkay() {
    MyPreconditions.checkState(nullableField != null);
    return nullableField.toString();
  }

  String customPreconditionsCheckArgumentOkay(@Nullable Object arg) {
    MyPreconditions.checkState(arg != null);
    return arg.toString();
  }

  void nullMethodCallWithAlarmManager(AlarmManager manager, @Nullable PendingIntent intent) {
    manager.cancel(intent);
  }

  String callingSeverSideNullableGetter(ServerSideDeserializer deserializer) {
    return deserializer.nullableGetter().toString();
  }

  interface AnotherI {
    void withBooleanParameter(boolean test);

    void withObjectParameter(Object object);
  }

  void withConjuction(@Nullable AnotherI i, boolean test1, boolean test2) {
    i.withBooleanParameter(test1 && test2);
  }

  void withConditionalAssignemnt(
      @Nullable AnotherI i, boolean test, Object object1, Object object2) {
    i.withObjectParameter(test ? object1 : object2);
  }

  String assertGetOnMapOK(Map<Integer, Object> map, Integer key) {
    return Assertions.assertGet(key, map).toString(); // No warning here
  }

  String assertGetOnListOK(List<Object> list, int index) {
    return Assertions.assertGet(index, list).toString(); // No warning here
  }

  String guavaVerifyNotNullOK(@Nullable Object object) {
    Verify.verifyNotNull(object);
    return object.toString();
  }

  void nullabilityNotPreservedAfterAssignment() {
    if (getNullable() != null) {
      Object t = getNullable();
      t.toString(); // Should not warn here
    }
  }

  void nullabilityStoredInBooleanFP() {
    boolean isNotNull = getNullable() != null;
    if (isNotNull) {
      getNullable().toString(); // Should not warn here
    }
  }

  void testInAssignmentFP(@Nullable Object object) {
    Object t;
    while ((t = getNullable()) != null) {
      t.toString(); // Should not warn here
    }
  }

  String testPathGetParent() {
    return Paths.get("foo").getParent().toString();
  }

  String testNotDetectingInvariantFP(@Nullable Object object1, @Nullable Object object2) {
    if (object1 == null && object2 == null) {
      return "both null";
    }
    return object1 == null ? object2.toString() : "null";
  }
}
