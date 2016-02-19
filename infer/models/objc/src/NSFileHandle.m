/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
