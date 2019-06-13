/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
