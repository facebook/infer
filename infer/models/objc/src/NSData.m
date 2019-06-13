/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#include <stdlib.h>

NSData* __objc_alloc(NSData*);

@interface NSData : NSObject

@property(nonatomic, readonly) const void* bytes;

+ (instancetype)dataWithBytesNoCopy:(void*)bytes length:(NSUInteger)length;
+ (instancetype)dataWithBytesNoCopy:(void*)bytes
                             length:(NSUInteger)length
                       freeWhenDone:(BOOL)b;

@end

@implementation NSData

+ (instancetype)dataWithBytesNoCopy:(void*)bytes length:(NSUInteger)length {
  return [NSData dataWithBytesNoCopy:bytes length:length freeWhenDone:YES];
}

+ (instancetype)dataWithBytesNoCopy:(void*)bytes
                             length:(NSUInteger)length
                       freeWhenDone:(BOOL)b {
  NSData* data = __objc_alloc(self);
  if (data) {
    data->_bytes = bytes;
    return data;
  } else
    return nil;
}

- (instancetype)initWithBytesNoCopy:(void*)bytes length:(NSUInteger)length {
  return [self initWithBytesNoCopy:bytes length:length freeWhenDone:YES];
}

- (instancetype)initWithBytesNoCopy:(void*)bytes
                             length:(NSUInteger)length
                       freeWhenDone:(BOOL)flag {
  self->_bytes = bytes;
  return self;
}

- (void)dealloc {
  if (self)
    free((void*)self->_bytes);
}
@end
