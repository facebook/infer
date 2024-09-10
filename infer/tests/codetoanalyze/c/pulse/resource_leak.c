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
