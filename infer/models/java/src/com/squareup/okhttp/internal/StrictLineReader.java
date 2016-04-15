/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.squareup.okhttp.internal;

import com.facebook.infer.models.InferUndefined;

import java.io.IOException;

public class StrictLineReader {

    public String readLine() throws IOException {
        return InferUndefined.can_throw_ioexception_string();
    }

    public int readInt() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

}
