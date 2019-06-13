/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "EOCPerson.h"

@implementation EOCPerson (Work)

- (void)performDaysWork {
  NSLog(@"Performing days at work");
}

- (void)takeVacationFromWork {
  NSLog(@"BTaking vacations");
}
@end
