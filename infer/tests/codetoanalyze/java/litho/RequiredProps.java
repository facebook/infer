/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.litho;

import com.facebook.litho.Column;
import com.facebook.litho.Component;
import com.facebook.litho.MyLithoComponent;
import java.util.ArrayList;

public class RequiredProps {

  public MyComponent mMyComponent;
  public MyLithoComponent mMyLithoComponent;
  public ResPropComponent mResPropComponent;
  public VarArgPropComponent mVarArgPropComponent;

  public Component buildWithAllOk() {
    return mMyComponent
        .create()
        .prop1(new Object())
        .prop2(new Object())
        .prop3(new Object())
        .build();
  }

  // prop 2 is optional
  public Component buildWithout2Ok() {
    return mMyComponent.create().prop1(new Object()).prop3(new Object()).build();
  }

  // prop 1 is required
  public Component buildWithout1Bad() {
    return mMyComponent.create().prop2(new Object()).prop3(new Object()).build();
  }

  // prop3 is required
  public Component buildWithout3Bad() {
    return mMyComponent.create().prop1(new Object()).prop2(new Object()).build();
  }

  public Component buildWithCommonPropOk() {
    return mMyComponent
        .create()
        .prop1(new Object())
        .commonProp(new Object())
        .prop3(new Object())
        .build();
  }

  private static MyComponent.Builder setProp1(MyComponent.Builder builder) {
    return builder.prop1(new Object());
  }

  private static MyComponent.Builder setProp3(MyComponent.Builder builder) {
    return builder.prop3(new Object());
  }

  public Component setProp1InCalleeOk() {
    return setProp1(mMyComponent.create().prop2(new Object())).prop3(new Object()).build();
  }

  public Component setProp3InCalleeOk() {
    return setProp3(mMyComponent.create().prop1(new Object()).prop2(new Object())).build();
  }

  public Component setProp3InCalleeButForgetProp1Bad() {
    return setProp3(mMyComponent.create()).prop2(new Object()).build();
  }

  public Component setRequiredOnOneBranchBad(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder = builder.prop1(new Object());
    }
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  public Component FP_setRequiredOnBothBranchesOk(boolean b) {
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
  public Component callBuildSuffixWithRequiredOk() {
    return buildSuffix(mMyComponent.create().prop1(new Object()));
  }

  // should report here; forgot prop 1
  public Component callBuildSuffixWithoutRequiredBad() {
    return buildSuffix(mMyComponent.create());
  }

  public Object generalTypeWithout2Ok() {
    Component.Builder builder = mMyComponent.create().prop1(new Object()).prop3(new Object());
    return builder.build();
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
    Component.Builder childBuilder = mMyComponent.create().prop1(new Object());
    // forgot prop 3, and builder.child() will invoke build() on childBuilder
    builder.child(childBuilder);
  }

  public Component buildWithColumnChildOk() {
    return Column.create()
        .child(mMyComponent.create().prop1(new Object()).prop3(new Object()))
        .build();
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

  // can't track MyLithoComponent.build() because it is on litho, hence we miss the error
  public Component buildPropLithoMissingBothBad_FN() {
    return mMyLithoComponent.create().build();
  }

  // we pick up Component.build() rather than
  // MyLithoComponent.build(). Hence, we can't detect that it has
  // missing props at all.
  public void buildPropLithoMissingOneBad_FN() {
    Column.create()
        .child(mMyLithoComponent.create().prop1(new Object()).commonProp(new Object()))
        .build();
  }

  // We can't track prop1 and prop2 because they are on litho
  public Component buildPropLithoOK_FP() {
    Component.Builder layoutBuilder =
        mMyLithoComponent.create().prop1(new Object()).prop2(new Object());
    return layoutBuilder.build();
  }

  public class NonRequiredTreeProps {

    public MyTreeComponent mMyTreeComponent;

    public MyTreeComponent buildWithoutOk() {
      return mMyTreeComponent.create().build();
    }
  }
}
