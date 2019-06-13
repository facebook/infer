/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
