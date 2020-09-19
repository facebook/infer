/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.litho;

import com.facebook.litho.Component;
import com.facebook.litho.annotations.Prop;
import com.facebook.litho.annotations.ResType;

/**
 * using @Prop(resType = ..) allows you to set the Prop with any of .propname, .propnameRes, or
 * .propnameAttr
 */
public class ResPropDoubleComponent extends Component {

  @Prop(resType = ResType.SOME)
  Object prop; // implicitly non-optional with resType

  @Prop
  Integer
      propPx; // note that setter for propPx(Integer) is not same as propPx(Object) corresponding to
  // prop

  public Builder create() {
    return new Builder();
  }

  public static class Builder extends Component.Builder<Builder> {

    ResPropDoubleComponent mResPropDoubleComponent;

    public Builder prop(Object o) {
      this.mResPropDoubleComponent.prop = o;
      return this;
    }

    public Builder propPx(Integer o) {
      this.mResPropDoubleComponent.propPx = o;
      return this;
    }

    public Builder propPx(Object o) {
      this.mResPropDoubleComponent.prop = o;
      return this;
    }

    public ResPropDoubleComponent build() {
      return mResPropDoubleComponent;
    }

    @Override
    public Builder getThis() {
      return this;
    }
  }
}
