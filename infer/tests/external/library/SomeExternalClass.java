/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package external.library;

import javax.annotation.Nullable;

public class SomeExternalClass {
  public Object field;

  public Object externalMethod1() {
    return null;
  }

  public void externalMethod2(@Nullable Object object) {}
}
