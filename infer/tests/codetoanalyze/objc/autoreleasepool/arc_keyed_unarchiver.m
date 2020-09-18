/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface ArcKeyedUnarchiver : NSObject
@end

@implementation ArcKeyedUnarchiver

- (void)callInitForReadingFromData_constant:(int)n data:(NSData*)data {
  NSKeyedUnarchiver* x = [[NSKeyedUnarchiver alloc] initForReadingFromData:data
                                                                     error:nil];
}

- (void)callInitForReadingWithData_constant:(int)n data:(NSData*)data {
  NSKeyedUnarchiver* x =
      [[NSKeyedUnarchiver alloc] initForReadingWithData:data];
}

- (void)callUnarchivedObjectOfClass_constant:(int)n data:(NSData*)data {
  NSArray* x = [NSKeyedUnarchiver unarchivedObjectOfClass:[NSArray class]
                                                 fromData:data
                                                    error:nil];
}

- (void)callUnarchivedObjectOfClasses_constant:(int)n
                                           set:(NSSet*)set
                                          data:(NSData*)data {
  NSArray* x = [NSKeyedUnarchiver unarchivedObjectOfClasses:set
                                                   fromData:data
                                                      error:nil];
}

@end
