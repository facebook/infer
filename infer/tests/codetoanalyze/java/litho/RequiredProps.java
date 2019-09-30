/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.litho;

import com.facebook.litho.Column;
import com.facebook.litho.Component;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.ArrayList;
import java.util.List;

enum ResType {
  SOME,
  NONE
}

@Target({ElementType.PARAMETER, ElementType.FIELD})
@Retention(RetentionPolicy.CLASS)
@interface Prop {
  ResType resType() default ResType.NONE;

  boolean optional() default false;

  String varArg() default "";
}

@Target({ElementType.PARAMETER, ElementType.FIELD})
@Retention(RetentionPolicy.CLASS)
@interface TreeProp {
  ResType resType() default ResType.NONE;

  boolean optional() default false;
}

class MyComponent extends Component {
  @Prop Object prop1; // implicitly non-optional

  @Prop(optional = true)
  Object prop2; // explicitly optional

  @Prop(optional = false)
  Object prop3; // explicitly non-optional

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

class MyTreeComponent extends Component {
  @TreeProp Object prop1; // implicitly non-optional

  Object nonProp;

  public Builder create() {
    return new Builder();
  }

  static class Builder extends Component.Builder {
    MyTreeComponent mMyTreeComponent;

    public Builder prop1(Object o) {
      this.mMyTreeComponent.prop1 = o;
      return this;
    }

    public MyTreeComponent build() {
      return mMyTreeComponent;
    }
  }
}

/**
 * using @Prop(resType = ..) allows you to set the Prop with any of .propname, .propnameRes, or
 * .propnameAttr
 */
class ResPropComponent extends Component {

  @Prop(resType = ResType.SOME)
  Object prop; // implicitly non-optional with resType

  public Builder create() {
    return new Builder();
  }

  static class Builder extends Component.Builder {

    ResPropComponent mResPropComponent;

    public Builder prop(Object o) {
      this.mResPropComponent.prop = o;
      return this;
    }

    public Builder propRes(Object o) {
      this.mResPropComponent.prop = o;
      return this;
    }

    public Builder propAttr(Object o) {
      this.mResPropComponent.prop = o;
      return this;
    }

    public Builder propDip(Object o) {
      this.mResPropComponent.prop = o;
      return this;
    }

    public Builder propPx(Object o) {
      this.mResPropComponent.prop = o;
      return this;
    }

    public Builder propSp(Object o) {
      this.mResPropComponent.prop = o;
      return this;
    }

    public ResPropComponent build() {
      return mResPropComponent;
    }
  }
}

/** varArg test */
class VarArgPropComponent extends Component {

  @Prop(varArg = "prop")
  List<Object> props;

  public Builder create() {
    return new Builder();
  }

  static class Builder extends Component.Builder {

    VarArgPropComponent mVarArgPropComponent;

    public Builder prop(Object prop) {
      if (prop == null) {
        return this;
      }
      if (this.mVarArgPropComponent.props == null) {
        this.mVarArgPropComponent.props = new ArrayList<Object>();
      }
      this.mVarArgPropComponent.props.add(prop);
      return this;
    }

    public Builder propAttr(Object prop) {
      if (prop == null) {
        return this;
      }
      if (this.mVarArgPropComponent.props == null) {
        this.mVarArgPropComponent.props = new ArrayList<Object>();
      }
      this.mVarArgPropComponent.props.add(prop);
      return this;
    }

    public Builder propsAttr(List<Object> props) {
      if (props == null) {
        return this;
      }
      if (this.mVarArgPropComponent.props == null || this.mVarArgPropComponent.props.isEmpty()) {
        this.mVarArgPropComponent.props = props;
      } else {
        this.mVarArgPropComponent.props.addAll(props);
      }
      return this;
    }

    public Builder props(List<Object> props) {
      if (props == null) {
        return this;
      }
      if (this.mVarArgPropComponent.props == null || this.mVarArgPropComponent.props.isEmpty()) {
        this.mVarArgPropComponent.props = props;
      } else {
        this.mVarArgPropComponent.props.addAll(props);
      }
      return this;
    }

    public VarArgPropComponent build() {
      return mVarArgPropComponent;
    }
  }
}

public class RequiredProps {

  public MyComponent mMyComponent;
  public ResPropComponent mResPropComponent;
  public VarArgPropComponent mVarArgPropComponent;

  public MyComponent buildWithAllOk() {
    return mMyComponent
        .create()
        .prop1(new Object())
        .prop2(new Object())
        .prop3(new Object())
        .build();
  }

  // prop 2 is optional
  public MyComponent buildWithout2Ok() {
    return mMyComponent.create().prop1(new Object()).prop3(new Object()).build();
  }

  // prop 1 is required
  public MyComponent buildWithout1Bad() {
    return mMyComponent.create().prop2(new Object()).prop3(new Object()).build();
  }

  // prop3 is required
  public MyComponent buildWithout3Bad() {
    return mMyComponent.create().prop1(new Object()).prop2(new Object()).build();
  }

  private static MyComponent.Builder setProp1(MyComponent.Builder builder) {
    return builder.prop1(new Object());
  }

  private static MyComponent.Builder setProp3(MyComponent.Builder builder) {
    return builder.prop3(new Object());
  }

  public MyComponent setProp1InCalleeOk() {
    return setProp1(mMyComponent.create().prop2(new Object())).prop3(new Object()).build();
  }

  public MyComponent setProp3InCalleeOk() {
    return setProp3(mMyComponent.create().prop1(new Object()).prop2(new Object())).build();
  }

  public MyComponent setProp3InCalleeButForgetProp1Bad() {
    return setProp3(mMyComponent.create()).prop2(new Object()).build();
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
    MyComponent.Builder childBuilder = mMyComponent.create().prop1(new Object());
    // forgot prop 3, and builder.child() will invoke build() on childBuilder
    builder.child(childBuilder);
  }

  public void buildPropResWithNormalOk() {
    mResPropComponent.create().prop(new Object()).build();
  }

  public void buildPropResWithResOk() {
    mResPropComponent.create().propRes(new Object()).build();
  }

  public void buildPropResWithAttrOk() {
    mResPropComponent.create().propAttr(new Object()).build();
  }

  public void buildPropResWithDipOk() {
    mResPropComponent.create().propDip(new Object()).build();
  }

  public void buildPropResWithPxOk() {
    mResPropComponent.create().propPx(new Object()).build();
  }

  public void buildPropResWithSpOk() {
    mResPropComponent.create().propSp(new Object()).build();
  }

  public void buildPropResMissingBad() {
    mResPropComponent.create().build();
  }

  public void buildPropVarArgNormalOk() {
    mVarArgPropComponent.create().props(new ArrayList<Object>()).build();
  }

  public void buildPropVarArgElementOk() {
    mVarArgPropComponent.create().prop(new Object()).build();
  }

  public void buildPropVarArgAttrElementOk() {
    mVarArgPropComponent.create().propAttr(new Object()).build();
  }

  public void buildPropVarArgNormalAttrElementOk() {
    mVarArgPropComponent.create().propsAttr(new ArrayList<Object>()).build();
  }

  public void buildPropVarArgMissingBad() {
    mVarArgPropComponent.create().build();
  }

  public class NonRequiredTreeProps {

    public MyTreeComponent mMyTreeComponent;

    public MyTreeComponent buildWithoutOk() {
      return mMyTreeComponent.create().build();
    }
  }
}
