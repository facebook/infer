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
  public ResPropDoubleComponent mResPropDoubleComponent;
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

  public Component setRequiredOnBothBranchesNoAssignOk(boolean b) {
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

  public Component setRequiredEffectful(boolean b) {
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

  public Component doubleSetMissingBad() {
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

  public void buildPropInConditionalOk(boolean b) {
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

  Component.Builder getBuilder(MyComponent.Builder builder) {
    if (builder == null) {
      return null;
    } else {
      return builder.prop1(new Object());
    }
  }

  public void buildWithColumnChildBadNullOk() {
    Column.Builder builder = Column.create();
    builder.child(getBuilder(null));
  }

  public void buildWithColumnChildBadCalleeBad() {
    Column.Builder builder = Column.create();
    builder.child(getBuilder(mMyComponent.create()));
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

  public void buildPropResWithPxDoubleOk() {
    mResPropDoubleComponent.create().propPx(new Object()).propPx(0).build();
  }

  public void buildPropResWithPxDoubleAlsoOk() {
    mResPropDoubleComponent.create().prop(new Object()).propPx(0).build();
  }

  public void buildPropResWithPxDoubleBad() {
    mResPropDoubleComponent.create().prop(new Object()).build();
  }

  public void buildPropResWithSpOk() {
    mResPropComponent.create().propSp(new Object()).build();
  }

  public void buildPropResMissingBad() {
    mResPropComponent.create().build();
  }

  public void buildPropResInCondOk(boolean b) {
    ResPropComponent.Builder builder = mResPropComponent.create();
    if (b) {
      builder.propAttr(new Object());
    } else {
      builder.propDip(new Object());
    }
    builder.build();
  }

  public void buildPropResInCondOneNormalOk(boolean b) {
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

  void buildMissingProp3Bad() {
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

  public Component buildJoinOk(boolean b, String s) {
    MyComponent.Builder builder = mMyComponent.create().prop1(new Object());
    if (b) {
      builder.prop3(new Object());
    } else {
      return null;
    }
    return builder.build();
  }

  public void buildJoinVoidOk(boolean b, String s) {
    MyComponent.Builder builder = mMyComponent.create().prop1(new Object());
    if (b) {
      builder.prop3(new Object());
    } else {
      return;
    }
    builder.build();
  }

  public MyComponent.Builder createJoin(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder.prop3(new Object());
    } else {
      return null;
    }
    return builder;
  }

  // should only miss prop1
  public Component callCreateJoinBad(boolean b) {

    return createJoin(b).build();
  }

  public MyComponent.Builder createInsideJoinBad(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (b) {
      builder.prop3(new Object());
    } else {
      Component.Builder resBuilder = mResPropComponent.create();
      resBuilder.build();
      return null;
    }
    return builder;
  }

  public Component callCreateJoinOk(boolean b) {

    return createJoin(b).prop1(new Object()).build();
  }

  public MyComponent.Builder returnNullWithProp3(boolean b, MyComponent.Builder builder) {
    if (b) {
      builder.prop3(new Object());
      return null;
    }
    return builder;
  }

  public void callReturnNullWithProp3_FP(boolean b) {
    MyComponent.Builder builder = mMyComponent.create();
    if (returnNullWithProp3(b, builder) == null) {
      builder.prop1(new Object()).build();
    }
  }

  private static Component setProp3AndBuild(MyComponent.Builder builder) {
    return setProp3(builder).build();
  }

  public void unionAtFunctionCallOk() {
    MyComponent.Builder builder1 = mMyComponent.create().prop1(new Object()).prop3(new Object());
    builder1.build();
    MyComponent.Builder builder2 = mMyComponent.create().prop1(new Object());
    MyComponent.Builder builder3 = mMyComponent.create();
    setProp3AndBuild(builder2);
  }

  public void ignoreLocationOk() {
    MyComponent.Builder builder1 = mMyComponent.create().prop1(new Object());
    MyComponent.Builder builder2 = mMyComponent.create();
    setProp3AndBuild(builder1);
  }

  public void ignoreLocationBad() {
    MyComponent.Builder builder1 = mMyComponent.create();
    MyComponent.Builder builder2 = mMyComponent.create().prop1(new Object());
    setProp3AndBuild(builder1);
  }

  public MyComponent.Builder createWrapper() {
    return mMyComponent.create();
  }

  public void twoBuildersOk() {
    MyComponent.Builder builder1 = createWrapper();
    MyComponent.Builder builder2 = createWrapper().prop1(new Object());
    builder1.prop1(new Object()).prop3(new Object()).build();
    builder2.prop3(new Object()).build();
  }

  public void twoBuildersBad() {
    MyComponent.Builder builder1 = createWrapper();
    MyComponent.Builder builder2 = createWrapper();
    builder1.prop1(new Object()).prop3(new Object()).build();
    builder2.prop3(new Object()).build();
  }

  public void nullableBuilderOk(boolean b) {
    MyComponent.Builder builderOk =
        mMyComponent.create().prop1(new Object()).prop2(new Object()).prop3(new Object());
    MyComponent.Builder builder = null;
    if (b) {
      builder = mMyComponent.create().prop1(new Object()).prop2(new Object());
    }
    (builder == null ? builderOk : builder.prop3(new Object())).build();
  }

  public void nullableBuilderBad(boolean b) {
    MyComponent.Builder builderOk =
        mMyComponent.create().prop1(new Object()).prop2(new Object()).prop3(new Object());
    MyComponent.Builder builder = null;
    if (b) {
      builder = mMyComponent.create().prop2(new Object());
    }
    (builder == null ? builderOk : builder.prop3(new Object())).build();
  }

  public void nullableBuilderAliasOk_FP(boolean b) {
    MyComponent.Builder builder =
        mMyComponent.create().prop1(new Object()).prop2(new Object()).prop3(new Object());
    MyComponent.Builder alias = builder;
    if (b) {
      builder = null;
    }
    if (builder == null) {
      alias.build();
    }
  }
}
