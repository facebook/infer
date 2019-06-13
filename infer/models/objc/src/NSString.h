/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
- (instancetype)stringByAppendingString:(NSString*)aString;
- (void)dealloc;
@end
