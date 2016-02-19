/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
