/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import android.app.Activity;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import com.facebook.infer.annotation.Assertions;
import com.facebook.infer.annotation.Cleanup;
import com.facebook.infer.annotation.Initializer;
import javax.annotation.Nullable;

/**
 * It is common in Android code to recycle objects (e.g. views) by nullifying them in the "cleanup"
 * methods that are called after object lifecycle is over. This allows the GC to recycle without
 * waiting for the outer object to be freed. This is safe because these fields are not going to be
 * accessed after cleanup. So it is not necessary to annotate those fields with @Nullable.
 */
class CanAssignNullInCleanupMethods extends Fragment {

  String someObject = "";

  @Override
  public void onDestroyView() {
    // onDestroyView is a special method: OK to nullify here
    someObject = null;
  }

  @Override
  public void onDestroy() {
    // onDestroy is a special method: OK to nullify here
    someObject = null;
  }

  @Cleanup
  public void assignNullInCleanupMethodIsOK() {
    // The method is marked as cleanup.
    // OK to nullify here.
    someObject = null;
  }

  public void assignNullInAnyOtherMethodIsBAD() {
    someObject = null; // BAD: field is not nullable
  }
}

public class FieldNotNullable {
  @Nullable String nullable = "";
  String notNullable = "";

  String initializeNonNullableWithNullIsBAD = null;
  @Nullable String initializeNullableWithNullIsOK = null;

  @Nullable
  String getNullable() {
    return "";
  }

  String getNotNullable() {
    return "";
  }

  void setNullableToNotNullableIsBAD(@Nullable String s) {
    notNullable = null; // BAD
    notNullable = s; // BAD
    notNullable = getNullable(); // BAD (even though getNullable() does not really return null)
  }

  void setNullableToNullableIsOK(@Nullable String s) {
    nullable = null; // OK
    nullable = s; // OK
    nullable = getNullable(); // OK
  }

  void setNotNullableToNotNullableIsOK(String s) {
    notNullable = "abc"; // OK
    notNullable = s; // OK
    notNullable = getNotNullable(); // OK
  }
}

class MixedInitializers extends Activity {

  private String field1 = "1";
  private String field2;
  private String field3;

  MixedInitializers() {
    field2 = "2";
  }

  protected void onCreate(Bundle bundle) {
    field3 = "3";
  }
}

class TestInitializerBuilder {
  String required1;
  String required2;
  @Nullable String optional;

  // No FIELD_NOT_INITIALIZED error should be reported, because of the @Initializer annotations.
  TestInitializerBuilder() {}

  // This is an initializer and must always be called before build().
  @Initializer
  TestInitializerBuilder setRequired1(String required1) {
    this.required1 = required1;
    return this;
  }

  // This is an initializer and must always be called before build().
  @Initializer
  TestInitializerBuilder setRequired2(String required2) {
    this.required2 = required2;
    return this;
  }

  TestInitializerBuilder setOptional(String optional) {
    this.optional = optional;
    return this;
  }

  TestInitializer build() {
    // Fail hard if the required fields are not initialzed
    Assertions.assertCondition(required1 != null && required2 != null);

    return new TestInitializer(this);
  }

}

class TestInitializer {
  String required1; // should always be set
  String required2; // should always be set
  @Nullable String optional; // optionally set

  TestInitializer(TestInitializerBuilder b) {
    required1 = b.required1;
    required2 = b.required2;
    optional = b.optional;
  }

  static void testInitializerClientA() {
    TestInitializerBuilder b = new TestInitializerBuilder();
    b.setRequired1("hello");
    b.setRequired2("world");
    TestInitializer x = b.build();
  }

  static void testInitializerClientB() {
    TestInitializerBuilder b = new TestInitializerBuilder();
    b.setRequired1("a");
    b.setRequired2("b");
    b.setOptional("c");
    TestInitializer x = b.build();
  }
}
