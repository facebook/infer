/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.squareup.okhttp.internal;

import com.facebook.infer.models.InferCloseables;

import java.io.Closeable;
import java.io.IOException;


public class Util {

    public static void closeQuietly(Closeable closeable) throws IOException {
        InferCloseables.closeQuietly(closeable);
    }

}
