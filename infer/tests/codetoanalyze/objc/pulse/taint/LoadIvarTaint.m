/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface LoadIvarTaint : NSObject

- (void)logString:(NSString*)s;

@end

@implementation LoadIvarTaint {
  NSString* _elem;
}

- (void)logElem {
  NSString* elem = [self getElem];
  [self logString:elem];
}

- (NSString*)getElem {
  NSString* s = _elem;
  return s;
}


@end
