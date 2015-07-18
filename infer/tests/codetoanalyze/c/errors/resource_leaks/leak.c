/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
 */

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/select.h>
#include <unistd.h>


void fileNotClosed()
{
    int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd != -1) {
        char buffer[256];
        // We can easily batch that by separating with space
        write(fd, buffer, strlen(buffer));
    }
}

void fileClosed()
{
    int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd != -1) {
        char buffer[256];
        // We can easily batch that by separating with space
        write(fd, buffer, strlen(buffer));
        close(fd);
    }
}
