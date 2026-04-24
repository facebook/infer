/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "LegacyAPI.h"

@implementation LegacyAPI
- (NSString*)getUnannotatedString {
  return @"unannotated";
}
- (NSString* _Nonnull)getNonnullString {
  return @"nonnull";
}
- (NSString* _Nullable)getNullableString {
  return nil;
}
@end
