/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe;

import com.facebook.infer.annotation.NullsafeStrict;
import java.util.List;
import javax.annotation.Nullable;
import some.test.pckg.ThirdPartyTestClass;

/**
 * In this test, we test how Strict mode works for calls of 3rd party libraries, and how detection
 * differs based on if the function is whitelisted or not in 3rd party signatures repository.
 */
@NullsafeStrict
public class StrictModeForThirdParty {

  ThirdPartyTestClass obj;

  StrictModeForThirdParty() {
    obj = new ThirdPartyTestClass();
  }

  public @Nullable String getNullable() {
    return null;
  }

  public String getNonnull() {
    return "";
  }

  // Return values.
  // In strict mode, return values should be pessimistically treated as nullable
  // if the function is unspecified, and treated according to their return annotation if
  // the function is whitelisted in the 3rd party repo.

  public void dereferenceUnspecifiedIsBAD() {
    obj.returnUnspecified().toString();
  }

  public void dereferenceSpecifiedAsNullableIsBAD() {
    obj.returnSpecifiedAsNullable().toString();
  }

  public void dereferenceFieldIsBAD() {
    obj.nonNullableField.toString();
  }

  public void dereferenceSpecifiedAsNonnullIsOK() {
    obj.returnSpecifiedAsNonnull().toString();
  }

  // Params.
  // In strict mode, params should be pessimistically treated as non-nullable if the function is
  // unspecified,
  // and treated based on their annotation if the function is whitelisted in the 3rd party repo.

  public void passingNullableParamToUnspecifiedIsBAD() {
    obj.paramUnspecified(getNullable());
  }

  public void passingNonnullParamToUnspecifiedIsOK() {
    obj.paramUnspecified(getNonnull());
  }

  public void passingNullableToParamSpecifiedAsNonnullIsBAD() {
    obj.secondParamSpecifiedAsNonnull(getNonnull(), getNullable());
  }

  public void passingNullableToParamSpecifiedAsNullableIsOK() {
    // first param is explicitly whitelisted as specified as nullable, so everything is OK
    obj.secondParamSpecifiedAsNonnull(getNullable(), getNonnull());
  }

  public void passingNonnullToParamIsOK() {
    // Independently of param signature, it is safe to pass non-nullables
    obj.secondParamSpecifiedAsNonnull(getNonnull(), getNonnull());
  }

  // Below follow tests ensuring how we represent third party methods in the output .json file
  // for interesting edge cases.

  // Expect the dependent third party signature to be correctly rendered in .json output as
  // "some.test.pckg.ThirdPartyTestClass#genericObjectRepresentation(java.lang.Object,
  // java.util.List)"
  public String genericObjectRepresentation(String s, List<String> l) {
    return obj.generic(s, l);
  }

  // Expect the dependent third party signature to be correctly rendered in .json output as
  // "some.test.pckg.ThirdPartyTestClass#genericExtendsStringRepresentation(java.lang.String,
  // java.util.List)"
  public String genericExtendsStringRepresentation(String s, List<String> l) {
    return obj.genericString(s, l);
  }

  // Expect the dependent third party signature to be correctly rendered in .json output as
  // "some.test.pckg.ThirdPartyTestClass#arrayRepresentation(java.lang.String, java.lang.String[])"
  public String arrayRepresentation(String s, String[] arr) {
    return obj.array(s, arr);
  }

  // Expect the dependent third party signature to be correctly rendered in .json output as
  // "some.test.pckg.ThirdPartyTestClass#varargRepresentation(java.lang.String, java.lang.String[])"
  public String varargRepresentation(String s) {
    return obj.vararg(s, s, s, "Hello");
  }

  // Expect the dependent third party signature to be correctly rendered in .json output as
  // "some.test.pckg.ThirdPartyTestClass#varargGenericRepresentation(java.lang.String,
  // java.lang.String[])"
  public String varargGenericRepresentation(String s) {
    return obj.varargGeneric(s, s, s, "Hello");
  }
}
