/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;
import javax.annotation.Nullable;

public class File {

    public @Nullable File[] listFiles() {
        if (InferUndefined.boolean_undefined()) {
            return null;
        } else {
            return (File[])InferUndefined.object_undefined();
        }
    }

}
