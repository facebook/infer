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
// GlobalConst is a constant global variable that was initialized, so it
// shouldn't be nil
- (void)potentialNilGlobalConstNoNPEOk:(NSMutableDictionary*)dict
                             andString:(NSString*)input {
  NSString* str = [GlobalConst stringByAppendingString:input];
  [dict removeObjectForKey:str];
}

// This isn't a definite NPE, but there could be one if the global is nil.
// We may want to report that in the future.
- (void)potentialNilGlobalNoNPEOk:(NSMutableDictionary*)dict
                        andString:(NSString*)input {
  NSString* str = [Global stringByAppendingString:input];
  [dict removeObjectForKey:str];
}

@end
