/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.litho;

import com.facebook.litho.Column;
import com.facebook.litho.Component;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

enum ResType {
  SOME,
  NONE
}

@Target({ ElementType.PARAMETER, ElementType.FIELD })
@Retention(RetentionPolicy.CLASS)
@interface Prop {
  ResType resType();
  boolean optional() default false;
}

class MyComponent extends Component {
  @Prop(resType = ResType.NONE, optional = false)
  Object prop1;

  @Prop(resType = ResType.NONE, optional = true)
  Object prop2;

  @Prop(resType = ResType.SOME, optional = false)
  Object prop3;

  Object nonProp;

  public Builder create() {
    return new Builder();
  }

  static class Builder extends Component.Builder {
    MyComponent mMyComponent;

    public Builder prop1(Object o) {
      this.mMyComponent.prop1 = o;
      return this;
    }

    public Builder prop2(Object o) {
      this.mMyComponent.prop2 = o;
      return this;
    }

    public Builder prop3(Object o) {
      this.mMyComponent.prop3 = o;
      return this;
    }

    public MyComponent build() {
      return mMyComponent;
    }

  }

}

public class RequiredProps {

  public MyComponent mMyComponent;

  public MyComponent buildWithAllOk() {
    return
      mMyComponent
        .create()
        .prop1(new Object())
        .prop2(new Object())
        .prop3(new Object())
        .build();
  }

  // prop 2 is optional
  public MyComponent buildWithout2Ok() {
    return
      mMyComponent
        .create()
        .prop1(new Object())
        .prop3(new Object())
        .build();
  }

  // prop 1 is required
  public MyComponent buildWithout1Bad() {
    return
      mMyComponent
        .create()
        .prop2(new Object())
        .prop3(new Object())
        .build();
  }

  // prop3 is required
  public MyComponent buildWithout3Bad() {
    return
      mMyComponent
        .create()
        .prop1(new Object())
        .prop2(new Object())
        .build();
  }

  private static MyComponent.Builder setProp1(MyComponent.Builder builder) {
    return builder.prop1(new Object());
  }

  private static MyComponent.Builder setProp3(MyComponent.Builder builder) {
    return builder.prop3(new Object());
  }

  public MyComponent setProp1InCalleeOk() {
    return
      setProp1(
        mMyComponent
          .create()
          .prop2(new Object()))
      .prop3(new Object())
      .build();
  }

  public MyComponent setProp3InCalleeOk() {
    return
      setProp3(
        mMyComponent
          .create()
          .prop1(new Object())
          .prop2(new Object()))
      .build();
  }

  public MyComponent setProp3InCalleeButForgetProp1Bad() {
    return
      setProp3(mMyComponent.create())
      .prop2(new Object())
      .build();
  }

  public MyComponent setRequiredOnOneBranchBad(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder = builder.prop1(new Object());
    }
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  public MyComponent FP_setRequiredOnBothBranchesOk(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder = builder.prop1(new Object());
    } else {
      builder = builder.prop1(new Object());
    }
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  // don't want to report here; want to report at clients that don't pass prop1
  private MyComponent buildSuffix(MyComponent.Builder builder) {
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  // shouldn't report here; prop 1 passed
  public MyComponent callBuildSuffixWithRequiredOk() {
    return buildSuffix(mMyComponent.create().prop1(new Object()));
  }

  // should report here; forgot prop 1
  public MyComponent callBuildSuffixWithoutRequiredBad() {
    return buildSuffix(mMyComponent.create());
  }

  public Object generalTypeForgot3Bad() {
    MyComponent.Builder builder1 = mMyComponent.create();
    Component.Builder builder2 = (Component.Builder) builder1.prop1(new Object());
    // don't fail to find required @Prop's for MyComponent.Builder even though the static type that
    // build is invoked on is [builder2]
    return builder2.build();
  }

  public void buildWithColumnChildBad() {
    Column.Builder builder = Column.create();
    MyComponent.Builder childBuilder =
      mMyComponent.create().prop1(new Object());
    // forgot prop 3, and builder.child() will invoke build() on childBuilder
    builder.child(childBuilder);
  }

}
