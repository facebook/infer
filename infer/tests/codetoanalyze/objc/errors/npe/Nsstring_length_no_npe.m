/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <Foundation/Foundation.h>
@interface Nsstring_length_no_npe : NSObject
@end

@implementation Nsstring_length_no_npe

- (NSDictionary*)logMessage:(NSString* __nullable)message {
  if (message.length > 0) {
    return @{@"key" : message}; // No NPE because of model of NSString length.
  } else
    return nil;
}
@end
