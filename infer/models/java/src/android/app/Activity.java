/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package android.app;

import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.ContextThemeWrapper;
import com.facebook.infer.builtins.InferUndefined;

public abstract class Activity extends ContextThemeWrapper {

  public TypedArray obtainStyledAttributes(int[] attrs) {
    return new TypedArray(null, attrs, attrs, 1);
  }

  public TypedArray obtainStyledAttributes(int resid, int[] attrs) throws NotFoundException {
    if (InferUndefined.boolean_undefined()) {
      throw new NotFoundException();
    }
    return new TypedArray(null, attrs, attrs, 1);
  }

  public TypedArray obtainStyledAttributes(
      AttributeSet set, int[] attrs, int defStyleAttr, int defStyleRes) {
    return new TypedArray(null, attrs, attrs, 1);
  }

  public static class NotFoundException extends RuntimeException {
    public NotFoundException() {}

    public NotFoundException(String name) {
      super(name);
    }
  }

  public TypedArray obtainAttributes(AttributeSet set, int[] attrs) {
    return new TypedArray(null, attrs, attrs, 1);
  }
}
