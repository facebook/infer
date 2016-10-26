/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
