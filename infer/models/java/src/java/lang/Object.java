/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.lang;

import com.facebook.infer.builtins.InferUndefined;

public class Object {

    public Class getClass() {
        Class c = new Class();
        c.name = (String)InferUndefined.object_undefined();
        return c;
    }

    public int hashCode() {
      return InferUndefined.int_undefined();
    }
}
