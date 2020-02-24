/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdbool.h>

const int gvar = 0;

enum {
  kEof = gvar + 1,

};

// expected linear
bool wrong_cvar_FP(int x) {

  while (true) {
    switch (x) {
      // can't pick up the control variable gvar here due to dangling node.
      case kEof:
        return false;
    }
    x++;
  }
  return false;
}
