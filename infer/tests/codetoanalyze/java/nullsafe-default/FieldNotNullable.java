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
import android.view.View;
import com.facebook.infer.annotation.Assertions;
import com.facebook.infer.annotation.Cleanup;
import com.facebook.infer.annotation.Initializer;
import javax.annotation.Nullable;

abstract class A {
  final String fld;

  A(String s) {
    this.fld = s;
  }
}

class FragmentExample extends Fragment {

  View view;

  @Override
  public void onDestroyView() {
    view = null;
  }
}

public class FieldNotNullable extends A {
  @Nullable String x;
  String y;
  String fld; // Shadow the field defined in A
  String static_s = null; // Static initializer error

  FieldNotNullable(String s) {
    super(s);
    x = null;
    y = s;
    this.fld = s;
  }

  void setXNull() {
    x = null;
  }

  void setXNullable(@Nullable String s) {
    x = s;
  }

  void setYNull() {
    y = null;
  }

  void setYNullable(@Nullable String s) {
    y = s;
  }

  FieldNotNullable(Integer n) {
    super("");
    this.fld = "";
    y = x == null ? "abc" : "def";
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

  @Cleanup
  void testCleanup() {
    this.required1 = null;
    this.required2 = null;
    this.optional = null;
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
