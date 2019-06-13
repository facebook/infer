/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

static NSString* const GlobalConst = @"A Global Const!";
static NSString* Global = @"A Global!";

@interface SimpleRoot : NSObject

@end

@implementation SimpleRoot

- (void)doSomethingOkWithDict:(NSMutableDictionary*)dict
                    andString:(NSString*)input {
  NSString* str = [GlobalConst stringByAppendingString:input];
  [dict removeObjectForKey:str];
}

- (void)doSomethingBadWithDict:(NSMutableDictionary*)dict
                     andString:(NSString*)input {
  NSString* str = [Global stringByAppendingString:input];
  [dict removeObjectForKey:str];
}

@end
