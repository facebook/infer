/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

enum week {
  sunday,
  monday,
  tuesday,
  wednesday = 0,
  thursday,
  friday,
  saturday
};

int main() {
  enum week today;
  today = wednesday;
  today = monday;
  today = today + 4;
  today = (enum week)tuesday + 1;
  int i = tuesday + (friday - sunday);
  return 0;
}
