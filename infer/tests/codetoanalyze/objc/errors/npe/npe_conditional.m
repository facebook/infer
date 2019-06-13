/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface ConditionalA : NSObject {
  int x;
}

- (NSString*)name;

@end

@implementation ConditionalA

- (NSString*)name {
  return @"1";
}

void binaryConditionalNoNPE(ConditionalA* transfer) {
  NSString* val = transfer.name ?: @"0";
  NSMutableDictionary* extraBlock =
      [[NSMutableDictionary alloc] initWithDictionary:@{@"key" : val}];
}

void conditionalNPE(ConditionalA* transfer) {
  NSString* val = transfer.name ? transfer.name : @"0";
  NSMutableDictionary* extraBlock =
      [[NSMutableDictionary alloc] initWithDictionary:@{@"key" : val}];
}

@end
