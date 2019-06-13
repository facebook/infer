/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface B : NSObject

@end

@interface A

+ (nullable NSData*)foo:(B*)b;

@end

@implementation B {
  B* _decodedMetadata;
}

- (B*)decodedMetadata {
  NSData* metadata = [A foo:self];
  return _decodedMetadata; // No NPE here
}
@end
