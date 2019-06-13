/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import com.facebook.infer.builtins.InferUndefined;

public class Object {

  public Class getClass() {
    Class c = new Class();
    c.name = (String) InferUndefined.object_undefined();
    return c;
  }

  public int hashCode() {
    return InferUndefined.int_undefined();
  }
}
