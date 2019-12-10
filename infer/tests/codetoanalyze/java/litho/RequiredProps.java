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

  public Component setRequiredOnBothBranchesOk(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder = builder.prop1(new Object());
    } else {
      builder = builder.prop1(new Object());
    }
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  // current domain can't handle implicit calls like this
  public Component setRequiredOnBothBranchesNoAssignOk_FP(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder.prop1(new Object());
    } else {
      builder.prop1(new Object());
    }
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  // gets confused at cyclic dependency to builder when setting prop1
  public Component setRequiredOk(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    builder = builder.prop1(new Object());
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  // gets confused at side-effectfull prop setting
  public Component setRequiredEffectful_FP(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    builder.prop1(new Object());
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  public Component setRequiredOnOneBranchEffectfulBad(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder.prop1(new Object());
    }
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  public void buildPropLithoMissingOneInLoopBad(int x) {

    for (int i = 0; i < x; i++) {
      Column.create()
          .child(mMyLithoComponent.create().prop1(new Object()).commonProp(new Object()))
          .build();
    }
  }
  // due to mutual recursion check, we break cycle at seen props
  public Component doubleSetMissingBad_FN() {
    Component.Builder builder =
        mMyComponent.create().commonProp(new Object()).prop3(new Object()).commonProp(new Object());
    return builder.build();
  }

  // due to mutual recursion check, we break cycle at seen props
  public Component doubleSetCommonOk() {
    Component.Builder builder =
        mMyComponent
            .create()
            .prop1(new Object())
            .commonProp(new Object())
            .prop3(new Object())
            .commonProp(new Object());
    return builder.build();
  }

  // only missing prop3
  public Component setRequiredOnBothBranchesMissingProp3Bad(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder.prop1(new Object());
    } else {
      builder.prop1(new Object());
    }
    return builder.prop2(new Object()).build();
  }

  public void buildPropInConditionalOk_FP(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder.prop1(new Object()).prop3(new Object());
    } else {
      builder.prop1(new Object()).prop3(new Object());
    }
    builder.build();
  }

  // should be only missing prop3
  public void buildPropMissingInConditionalBad(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder.prop1(new Object()).prop3(new Object());
    } else {
      builder.prop2(new Object()).prop1(new Object());
    }
    builder.build();
  }

  public boolean isEmptyOrNull(String str) {
    return str == null || str.isEmpty();
  }

  public void buildInterProcUnrelatedBad(boolean b, String s) {
    MyComponent.Builder builder = mMyComponent.create().prop1(new Object());
    if (!isEmptyOrNull(s)) {
      builder.prop3(new Object());
    }
    builder.build();
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

  public void buildPropResInCondOk_FP(boolean b) {
    ResPropComponent.Builder builder = mResPropComponent.create();
    if (b) {
      builder.propAttr(new Object());
    } else {
      builder.propDip(new Object());
    }
    builder.build();
  }

  public void buildPropResInCondOneNormalOk_FP(boolean b) {
    ResPropComponent.Builder builder = mResPropComponent.create();
    if (b) {
      builder.propAttr(new Object());
    } else {
      builder.prop(new Object());
    }
    builder.build();
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

  public Component buildPropLithoMissingBothBad() {
    return mMyLithoComponent.create().build();
  }

  public void buildPropLithoMissingOneBad() {
    Column.create()
        .child(mMyLithoComponent.create().prop1(new Object()).commonProp(new Object()))
        .build();
  }

  public Component buildPropLithoOK() {
    Component.Builder layoutBuilder =
        mMyLithoComponent.create().prop1(new Object()).prop2(new Object());
    return layoutBuilder.build();
  }

  public void castImpossibleOk_FP(Object o1) {
    Component.Builder builder = mMyLithoComponent.create();
    if (builder instanceof MyComponent.Builder)
      ((MyComponent.Builder) builder)
          .build(); // this branch will never be taken but we can't detect it yet
  }

  void castOk(Object o1) {
    Component.Builder builder = mMyLithoComponent.create().prop1(new Object()).prop2(new Object());
    if (builder instanceof MyLithoComponent.Builder)
      ((MyLithoComponent.Builder) builder).build(); // this branch will be taken
  }

  Component.Builder createBuilder() {
    return mMyLithoComponent.create();
  }

  void castMissingOneBad(Object o1) {
    Component.Builder builder = createBuilder();
    if (builder instanceof MyLithoComponent.Builder)
      ((MyLithoComponent.Builder) builder).prop2(new Object()).build(); // this branch will be taken
  }

  void buildMissingProp3_FN() {
    Component.Builder builder = mMyComponent.create();
    ((MyLithoComponent.Builder) builder).prop1(new Object()).prop2(new Object()).build();
  }

  public class NonRequiredTreeProps {

    public MyTreeComponent mMyTreeComponent;

    public MyTreeComponent buildWithoutOk() {
      return mMyTreeComponent.create().build();
    }
  }

  public Component buildPropLithoMissingInOneBranchBad(boolean b) {
    if (b) {
      return mMyLithoComponent.create().prop1(new Object()).build();
    } else {
      return mMyLithoComponent.create().prop1(new Object()).prop2(new Object()).build();
    }
  }

  public Component buildPropLithoMissingInOneBranchBeforeBuildBad(boolean b) {
    MyLithoComponent.Builder builder =
        b
            ? mMyLithoComponent.create().prop1(new Object())
            : mMyLithoComponent.create().prop1(new Object()).prop2(new Object());
    return builder.build();
  }

  public Component setRequiredOnOneBothBranchesWithCreateOk_FP(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder.prop1(new Object());
    } else {
      builder = mMyComponent.create().prop1(new Object());
    }
    return builder.prop2(new Object()).prop3(new Object()).build();
  }

  public Component missingProp3InOneBranchBeforeBuildBad(boolean b) {
    Component.Builder builder =
        b
            ? mMyComponent.create().prop1(new Object())
            : mMyLithoComponent.create().prop1(new Object()).prop2(new Object());
    return builder.build();
  }

  public Component missingProp2InOneBranchBeforeBuildBad(boolean b) {
    Component.Builder builder =
        b
            ? mMyComponent.create().prop1(new Object()).prop3(new Object())
            : mMyLithoComponent.create().prop1(new Object());
    return builder.build();
  }

  public Component missingProp1InBothBranchesBeforeBuildBad(boolean b) {
    Component.Builder builder =
        b
            ? mMyComponent.create().prop3(new Object())
            : mMyLithoComponent.create().prop2(new Object());
    return builder.build();
  }

  public Component createDiffferentInBranchesBeforeBuildOk(boolean b) {
    Component.Builder builder =
        b
            ? mMyComponent.create().prop1(new Object()).prop3(new Object())
            : mMyLithoComponent.create().prop1(new Object()).prop2(new Object());
    return builder.build();
  }
}
