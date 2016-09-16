/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

@interface ArcA : NSObject {
  int x;
}
@property ArcA* son;
@end

@implementation ArcA

/* autorelease is added */
- (NSString*)getS {
  NSString* s = [NSString alloc];
  return s;
}

/* autorelease is not added */
- (NSString*)newS {
  NSString* s = [NSString alloc];
  return s;
}

@end
