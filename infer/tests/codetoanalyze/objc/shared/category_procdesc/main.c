/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#import "EOCPerson.h"

int CategoryProcdescMain() {
  EOCPerson* person = [[EOCPerson alloc] init];
  [person performDaysWork];
  int* x = malloc(sizeof(int));
  return 0;
}
