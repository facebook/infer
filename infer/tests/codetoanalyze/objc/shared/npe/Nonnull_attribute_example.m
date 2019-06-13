/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSString.h>

void __infer_assume(int cond);

@interface NonnullA : NSObject {
 @public
  int x;
}

- (NonnullA*)getA;

@end

@implementation NonnullA

- (NonnullA*)getA {
  return [NonnullA new];
}

@end

@interface NonnullC : NSObject
@property(copy, nonnull) NSString* name;

@end

@implementation NonnullC

- (instancetype)initWithCoder:(NSString*)aDecoder and:(NonnullA* __nonnull)a {
  NonnullA* a1 = [a getA];
  int y = a1->x;
  return self;
}

@end

void NonnullAtrributeTest(void (^__nonnull callback)(NSError*, id)) {
  callback(NULL, NULL);
}
