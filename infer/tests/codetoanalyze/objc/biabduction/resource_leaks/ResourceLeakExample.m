/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
#include <unistd.h>

@implementation NSFileHandle (A)

- (id)fileHandleForLoggingAtPath:(NSString*)path mode:(mode_t)mode {
  int fd = -1;
  if (path) {
    int flags = O_WRONLY | O_APPEND | O_CREAT;
    fd = open([path fileSystemRepresentation], flags, mode);
  }
  if (fd == -1)
    return nil;
  if (self) {
    return [[self initWithFileDescriptor:fd closeOnDealloc:YES] autorelease];
  } else {
    close(fd);
    return self;
  }
}

- (id)newOutput:(out NSError**)error {
  int fileDescriptor = open(
      "file.txt", O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if (fileDescriptor == -1) {
    if (error)
      *error = [NSError errorWithDomain:NSPOSIXErrorDomain
                                   code:errno
                               userInfo:nil];
    return nil;
  } else {
    if (self)
      return [self initWithFileDescriptor:fileDescriptor];
    else {
      close(fileDescriptor);
      return nil;
    }
  }
}

@end
