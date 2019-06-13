/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;
import javax.annotation.Nullable;

public class File {

  public @Nullable File[] listFiles() {
    if (InferUndefined.boolean_undefined()) {
      return null;
    } else {
      return (File[]) InferUndefined.object_undefined();
    }
  }
}
