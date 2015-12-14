/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "NSString.h"
#import <stdlib.h>

void __get_array_size(const UInt8);

@implementation NSString

+ (instancetype)stringWithUTF8String:(const char *)bytes {
    // using alloc as the documentation doesn't say it may return nil
    NSString *s = [NSString alloc];
    s->value = bytes;
    return s;
}

- (instancetype)initWithBytesNoCopy:(char*)bytes
                             length:(NSUInteger)length
                           encoding:(id)encoding
                       freeWhenDone:(BOOL)flag {
     if (flag == YES) {
        if (bytes)  {
            __get_array_size(bytes);
            free(bytes);
        }
     }
     return self;

}
@end
