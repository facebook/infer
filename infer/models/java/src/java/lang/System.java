/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

public final class System {

  private System() {}

  public static final InputStream in;

  static {
    byte[] arr = {0};
    in = new ByteArrayInputStream(arr);
  }

  public static final PrintStream out = new PrintStream(new ByteArrayOutputStream());

  public static final PrintStream err = new PrintStream(new ByteArrayOutputStream());

  public static void exit(int status) {
    InferBuiltins._exit();
  }

  public static String getProperty(String key) {
    int n = key.length(); // key must not be null
    if (InferUndefined.boolean_undefined()) {
      return null;
    }
    return (String) InferUndefined.object_undefined();
  }
}
