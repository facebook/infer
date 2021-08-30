/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util;

public abstract class Objects {

  public static boolean isNull(Object obj) {
    return obj == null;
  }

  public static boolean nonNull(Object obj) {
    return obj != null;
  }

}
