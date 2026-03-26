/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "LegacyHardware.h"

@implementation LegacyHardware
+ (int)globalHardwareVersion {
  return 101;
}
- (int)currentBatteryLevel {
  return 75;
}
@end
