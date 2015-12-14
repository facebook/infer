/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import java.io.Closeable;

public class Utils {

    public static void closeQuietly(Closeable closeable) {
      try {
        if (closeable != null) {
          closeable.close();
        }
      } catch (Exception ex) {
      }
    }

}
