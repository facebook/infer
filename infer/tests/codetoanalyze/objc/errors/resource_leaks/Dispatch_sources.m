/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

Boolean MyProcessFileData(char* buffer, ssize_t actual);

dispatch_source_t ProcessContentsOfFile(const char* filename) {
  // Prepare the file for reading.
  int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd == -1)
    return NULL;
  fcntl(fd, F_SETFL, O_NONBLOCK); // Avoid blocking the read operation

  dispatch_queue_t queue =
      dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
  dispatch_source_t readSource =
      dispatch_source_create(DISPATCH_SOURCE_TYPE_READ, fd, 0, queue);
  if (!readSource) {
    close(fd);
    return NULL;
  }

  // Install the event handler
  dispatch_source_set_event_handler(readSource, ^{
    size_t estimated = dispatch_source_get_data(readSource) + 1;
    // Read the data into a text buffer.
    char* buffer = (char*)malloc(estimated);
    if (buffer) {
      ssize_t actual = read(fd, buffer, (estimated));
      Boolean done = MyProcessFileData(buffer, actual); // Process the data.

      // Release the buffer when done.
      // free(buffer); // Don't release the buffer, report memory leak.

      // If there is no more data, cancel the source.
      if (done)
        dispatch_source_cancel(readSource);
    }
  });

  // Install the cancellation handler
  dispatch_source_set_cancel_handler(readSource, ^{
    close(fd);
  });

  // Start reading the file.
  dispatch_resume(readSource);
  return readSource;
}
