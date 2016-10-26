/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>
#import "EOCPerson.h"

int CategoryProcdescMain() {
  EOCPerson* person = [[EOCPerson alloc] init];
  [person performDaysWork];
  int* x = malloc(sizeof(int));
  return 0;
}
