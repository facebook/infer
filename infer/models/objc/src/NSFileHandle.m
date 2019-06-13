/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#include <unistd.h>

@class NSString, NSData, NSError;

@interface NSFileHandle : NSObject

- (void)closeFile;

- (instancetype)initWithFileDescriptor:(int)fd closeOnDealloc:(BOOL)closeopt;

- (instancetype)initWithFileDescriptor:(int)fd;

@property(nonatomic, readonly) int fileDescriptor;

@end

@implementation NSFileHandle

- (instancetype)initWithFileDescriptor:(int)fd closeOnDealloc:(BOOL)closeopt {
  if (self) {
    self->_fileDescriptor = fd;
    return self;
  }
}

- (instancetype)initWithFileDescriptor:(int)fd {
  [self initWithFileDescriptor:fd closeOnDealloc:NO];
}

- (void)closeFile {
  close(self->_fileDescriptor);
}

- (void)dealloc {
  [self closeFile];
}

@end
