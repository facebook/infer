/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface NSString : NSObject {
  const char* value;
}

+ (instancetype)stringWithUTF8String:(const char*)bytes;
+ (instancetype)stringWithString:(NSString*)aString;
+ (instancetype)stringWithFormat:(NSString*)format, ...;
+ (instancetype)localizedStringWithFormat:(NSString*)format, ...;
- (instancetype)initWithFormat:(NSString*)format, ...;
- (instancetype)initWithFormat:(NSString*)format arguments:(va_list)argList;
- (void)dealloc;
@end
