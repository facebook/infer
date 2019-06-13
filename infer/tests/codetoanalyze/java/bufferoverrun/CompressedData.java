/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.bufferoverrun;

/* Mimics https://fburl.com/f61h6rbl */
class CompressedData {
  class C {
    public static final int CCI = 4;
  }

  class DI {
    int s;
  }

  class D {
    final DI[] cci = new DI[C.CCI];
    int cis;
  }

  int yy;

  int decompressData(D d) {
    int output = 0;
    DI di;
    final int cis = d.cis;

    for (int y = 0; y < yy; ++y) {
      for (int ci = 0; ci < cis; ++ci) {
        di = d.cci[ci];
        final int s = di.s;
        output = y * s;
      }
    }
    return output;
  }
}
