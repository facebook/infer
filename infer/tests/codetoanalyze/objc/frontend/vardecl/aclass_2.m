/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface AClass : NSObject {
}
- (NSObject*)sharedInstance;
@end

@implementation AClass
NSObject* aVariable;

- (NSObject*)sharedInstance {
  return aVariable;
}

@end
