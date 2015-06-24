// Copyright (c) 2015-Present Facebook. All rights reserved.

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
