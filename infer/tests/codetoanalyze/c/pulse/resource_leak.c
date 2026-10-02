/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

void fileNotClosed_bad() {
  int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd != -1) {
    char buffer[256];
    write(fd, buffer, strlen(buffer));
  }
}

void fileClosed_ok() {
  int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd != -1) {
    char buffer[256];
    write(fd, buffer, strlen(buffer));
    close(fd);
  }
}

FILE* handler;

void fdopen_to_global_ok() {
  int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd != -1) {
    handler = fdopen(fd, "w");
    fclose(handler);
  }
}

void gzdopen_to_global_ok() {
  int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd != -1) {
    handler = gzdopen(fd, "w");
    fclose(handler);
  }
}

void socketNotClosed_bad() {
  int fd = socket(AF_LOCAL, SOCK_RAW, 0);
  if (fd != -1) {
    char buffer[256];
    write(fd, buffer, strlen(buffer));
  }
}

int socketClosed_ok() {
  int socketFD = socket(AF_LOCAL, SOCK_RAW, 0);
  if (socketFD == -1) {
    return -1;
  }

  int status;

  status = fcntl(socketFD, F_SETFL, O_NONBLOCK);
  if (status == -1) {
    close(socketFD);
    return -1;
  }

  int reuseaddr = 1;
  status = setsockopt(
      socketFD, SOL_SOCKET, SO_REUSEADDR, &reuseaddr, sizeof(reuseaddr));
  if (status == -1) {
    close(socketFD);
    return -1;
  }

  int nosigpipe = 1;
  status = setsockopt(
      socketFD, SOL_SOCKET, SO_REUSEADDR, &nosigpipe, sizeof(nosigpipe));
  if (status == -1) {
    close(socketFD);
    return -1;
  }

  return socketFD;
}

void openWithoutModeNotClosed_bad() {
  int fd = open("hi.txt", O_RDONLY);
  if (fd != -1) {
    char buffer[256];
    read(fd, buffer, sizeof(buffer));
  }
}

int openWithoutModeNotClosedOnError_bad(char* buffer) {
  int fd = open("hi.txt", O_RDONLY | O_CLOEXEC);
  if (fd < 0) {
    return -1;
  }
  if (read(fd, buffer, 4) != 4) {
    return -1;
  }
  close(fd);
  return 0;
}

int openWithoutModeClosed_ok() {
  int fd = open("hi.txt", O_RDONLY);
  if (fd < 0) {
    return -1;
  }
  close(fd);
  return 0;
}

int openWithoutModeReturned_ok() { return open("hi.txt", O_RDONLY); }

void openatNotClosed_bad(int dirFd) {
  int fd = openat(dirFd, "hi.txt", O_RDONLY);
  if (fd != -1) {
    char buffer[256];
    read(fd, buffer, sizeof(buffer));
  }
}

void openatWithModeNotClosed_bad() {
  int fd = openat(AT_FDCWD, "hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd != -1) {
    char buffer[256];
    write(fd, buffer, strlen(buffer));
  }
}

void openatClosed_ok(int dirFd) {
  int fd = openat(dirFd, "hi.txt", O_RDONLY);
  if (fd != -1) {
    close(fd);
  }
}

// glibc declares these only with _LARGEFILE64_SOURCE, macOS not at all
int open64(const char* path, int flags, ...);
int openat64(int dirFd, const char* path, int flags, ...);

void open64NotClosed_bad() {
  int fd = open64("hi.txt", O_RDONLY);
  if (fd != -1) {
    char buffer[256];
    read(fd, buffer, sizeof(buffer));
  }
}

void openat64NotClosed_bad(int dirFd) {
  int fd = openat64(dirFd, "hi.txt", O_RDONLY);
  if (fd != -1) {
    char buffer[256];
    read(fd, buffer, sizeof(buffer));
  }
}

// from Android's <android/fdsan.h>
int android_fdsan_close_with_tag(int fd, uint64_t expected_tag);

void fdsanClosed_ok() {
  int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd != -1) {
    android_fdsan_close_with_tag(fd, 0);
  }
}
