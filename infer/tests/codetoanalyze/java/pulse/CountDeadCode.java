/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

/*
 This is a test file specific to the unreachability counting
*/

public class CountDeadCode {

  public int no_deadcode() {
    int i = 0;
    int x = 0;
    if (i == 0) {
      x = 1;
    }
    return x;
  }

  public int else_with_deadcode() {
    int i = 0;
    int x;
    if (i == 0) {
      x = 0;
    } else { // only this part
      x = 1; // is counted as unreachable
    }
    return x;
  }

  public int long_catch_block() {
    try {
      int i = 0;
      int x;
      if (i == 0) {
        x = 0;
      } else { // only this part
        x = 1; // is counted as unreachable
      }
      return x;
    } catch (Exception e) { // the unreachability count
      int i = 0; // will not take into account
      i++; // this block
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      i++;
      return 0;
    }
  }
}
