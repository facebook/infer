/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;

public class Continue{
  int continue_outer_loop_FN ()
{
  outer:
  for (int i = 2; i < 1000; i++) {
      for (int j = 2; j < i; j++) {
          if (i % j == 0)
          continue outer;
     }
  }
  return 0;
}
}
