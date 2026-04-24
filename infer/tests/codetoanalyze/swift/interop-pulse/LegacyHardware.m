/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "LegacyHardware.h"

@implementation LegacyHardware
+ (int)getHardwareRevision {
  return 101;
}
- (int)getBatteryStatus {
  return 75;
}
@end

@implementation SomeOtherHardware
- (int)getBatteryStatus {
  return 50;
}
- (NSString*)getFirmwareVersion {
  return @"v0.1";
}
- (NSString*)getModelName {
  return @"Other";
}
@end
