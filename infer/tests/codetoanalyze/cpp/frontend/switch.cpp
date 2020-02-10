/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void default_last(int i) {
  switch (i) {
    case 1:
      i = 2;
      break;
    case 2:
      i = 3;
      break;
    default:
      i = 0;
      break;
  }
}

void default_first(int i) {
  switch (i) {
    default:
      i = 0;
      break;
    case 1:
      i = 2;
      break;
    case 2:
      i = 3;
      break;
  }
}

void default_middle(int i) {
  switch (i) {
    case 1:
      i = 2;
      break;
    default:
      i = 0;
      break;
    case 2:
      i = 3;
      break;
  }
}

void default_middle_no_break(int i) {
  switch (i) {
    case 1:
      i = 2;
    default:
      i = 0;
    case 2:
      i = 3;
  }
}
