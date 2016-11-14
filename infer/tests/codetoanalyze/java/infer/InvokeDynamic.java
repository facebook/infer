/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import java.util.Collections;
import java.util.List;

public class InvokeDynamic {

  void invokeDynamicThenNpeBad(List<String> list) {
    Object o = null;
    Collections.sort(list, (String a, String b) -> {
        return b.compareTo(a);
      });
    o.toString();
  }

  void npeInLambdaBad(List<String> list) {
    Collections.sort(list, (String a, String b) -> {
        Object o = null;
        o.toString();
        return b.compareTo(a);
      });
  }

  // we won't get this one because we don't actually translate the invocation of the lambda
  void FN_npeViaCaptureBad(List<String> list) {
    String s = null;
    Collections.sort(list, (String a, String b) -> {
        return s.compareTo(a);
      });
  }

}
