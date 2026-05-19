/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "LegacyAPI.h"

@implementation LegacyAPI
- (NSString*)getUnannotatedString {
  return @"x";
}
- (NSString*)getNonnullString {
  return @"x";
}
- (NSString*)getNullableString {
  return @"x";
}
@end

@implementation AssumedNonnullAPI
- (NSString*)getAssumedNonnullString {
  return @"x";
}
- (NSString*)getExplicitlyNullableInsideBlock {
  return @"x";
}
@end

@implementation FactoryAPI
+ (instancetype)factoryInstance {
  return [self new];
}
@end

@implementation ErrorOutParamAPI
- (BOOL)doThingWithError:(NSError**)error {
  return YES;
}
@end
