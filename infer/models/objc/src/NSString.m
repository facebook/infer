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

void __get_array_length(const UInt8);

void __infer_assume(bool cond);

@implementation NSString

+ (instancetype)stringWithUTF8String:(const char*)bytes {
  // using alloc as the documentation doesn't say it may return nil
  NSString* s = [NSString alloc];
  s->value = bytes;
  return s;
}

+ (instancetype)stringWithString:(NSString*)aString {
  NSString* s = [NSString alloc];
  s->value = aString->value;
  return s;
}

+ (instancetype)stringWithFormat:(NSString*)format, ... {
  return format;
}

+ (instancetype)localizedStringWithFormat:(NSString*)format, ... {
  return format;
}

- (instancetype)initWithBytesNoCopy:(char*)bytes
                             length:(NSUInteger)length
                           encoding:(id)encoding
                       freeWhenDone:(BOOL)flag {
  if (flag == YES) {
    if (bytes) {
      __get_array_length(bytes);
      free(bytes);
    }
  }
  return self;
}

- (instancetype)initWithFormat:(NSString*)format arguments:(va_list)argList {
  self->value = format->value;
  return self;
}

- (instancetype)initWithFormat:(NSString*)format, ... {
  self->value = format->value;
  return self;
}

- (int)length {
  if (self == nil) {
    return 0;
  } else {
    int res;
    __infer_assume(res >= 0);
    return res;
  }
}

@end
