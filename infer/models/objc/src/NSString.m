/*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "NSString.h"
#import <stdlib.h>

size_t __get_array_length(const void* arr);

void __infer_assume(bool cond);
void __set_wont_leak_attribute(const void* ptr);

@implementation NSString

+ (instancetype)stringWithUTF8String:(const char*)bytes {
  // using alloc as the documentation doesn't say it may return nil
  NSString* s = [NSString alloc];
  int len;
  len = __get_array_length(bytes);
  s->value = (const char*)malloc(len);
  // The newly allocated string will be autoreleased by the runtime
  __set_wont_leak_attribute(s->value);
  memcpy(s->value, bytes, len);
  return s;
}

+ (instancetype)stringWithString:(NSString*)aString {
  NSString* s = [NSString alloc];
  int len;
  len = __get_array_length(aString->value);
  s->value = (const char*)malloc(len);
  // The newly allocated string will be autoreleased by the runtime
  __set_wont_leak_attribute(s->value);
  memcpy(s->value, aString->value, len);
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

- (void)dealloc {
  if (self != nil && self->value != 0) {
    free(self->value);
  }
  [super dealloc];
}

@end
