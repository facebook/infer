/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wnullability-completeness"

#import <Foundation/Foundation.h>

@interface LegacyHardware : NSObject
// Standard Class Method (+)
+ (int)getHardwareRevision;
// Standard Instance Method (-)
- (int)getBatteryStatus;

// Missing nullability! Swift sees this as: func getFirmwareVersion() -> String!
- (NSString*)getFirmwareVersion;

// SAFE: Swift sees: func getModelName() -> String
- (NSString* _Nonnull)getModelName;
@end

@interface SomeOtherHardware : NSObject
- (int)getBatteryStatus;
// Missing nullability: Swift sees this as: func getFirmwareVersion() -> String!
- (NSString*)getFirmwareVersion;
// SAFE: Swift sees: func getModelName() -> String
- (NSString* _Nonnull)getModelName;
@end

#pragma clang diagnostic pop
