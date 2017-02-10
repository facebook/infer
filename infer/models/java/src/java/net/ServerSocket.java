/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.net;

import com.facebook.infer.builtins.InferUndefined;

import java.io.IOException;

public class ServerSocket {

    public Socket accept() throws IOException {
        if (InferUndefined.boolean_undefined()) {
            return new Socket();
        } else throw new IOException();
    }

}
