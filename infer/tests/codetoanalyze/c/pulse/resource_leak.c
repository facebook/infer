/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <dirent.h>
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

// not declared by the headers above on every platform
int accept4(int sockfd, struct sockaddr* addr, socklen_t* addrlen, int flags);
int creat64(const char* path, mode_t mode);
int epoll_create(int size);
int epoll_create1(int flags);
int eventfd(unsigned int initval, int flags);
int inotify_init(void);
int inotify_init1(int flags);
int memfd_create(const char* name, unsigned int flags);
int mkostemp(char* tmpl, int flags);
int pipe2(int fds[2], int flags);
int timerfd_create(int clockid, int flags);

int* get_fds_slot(void* ctx);
int* get_global_fds(void);
void register_fd(int fd);

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
    if (handler) {
      fclose(handler);
    } else {
      close(fd);
    }
  }
}

void gzdopen_to_global_ok() {
  int fd = open("hi.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (fd != -1) {
    handler = gzdopen(fd, "w");
    if (handler) {
      fclose(handler);
    } else {
      close(fd);
    }
  }
}

int fdopen_failure_leaks_fd_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  FILE* f = fdopen(fd, "r");
  if (!f) {
    return -1;
  }
  int c = fgetc(f);
  fclose(f);
  return c;
}

void fdopen_then_close_fd_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  FILE* f = fdopen(fd, "r");
  close(fd);
  if (f) {
    fclose(f);
  }
}

void fdopen_unchecked_fclose_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  FILE* f = fdopen(fd, "r");
  fclose(f);
}

void gzdopen_failure_leaks_fd_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  handler = gzdopen(fd, "r");
  if (handler) {
    fclose(handler);
  }
}

void fdopen_minus_one_then_null_deref_bad() {
  FILE* f = fdopen(-1, "r");
  int* p = NULL;
  *p = 42;
}

int fdopendir_failure_leaks_fd_bad() {
  int fd = open(".", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  DIR* d = fdopendir(fd);
  if (!d) {
    return -1;
  }
  closedir(d);
  return 0;
}

int fdopendir_ok() {
  int fd = open(".", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  DIR* d = fdopendir(fd);
  if (!d) {
    close(fd);
    return -1;
  }
  closedir(d);
  return 0;
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

void creat_not_closed_bad() { int fd = creat("hi.txt", 0600); }

void creat64_not_closed_bad() { int fd = creat64("hi.txt", 0600); }

void dup_not_closed_bad(int fd) { int copy = dup(fd); }

void dup_closed_ok(int fd) {
  int copy = dup(fd);
  if (copy != -1) {
    close(copy);
  }
}

void dup_does_not_close_argument_ok() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  int copy = dup(fd);
  if (copy != -1) {
    close(copy);
  }
  close(fd);
}

void epoll_create_not_closed_bad() { int fd = epoll_create(1); }

void epoll_create1_not_closed_bad() { int fd = epoll_create1(0); }

void eventfd_not_closed_bad() { int fd = eventfd(0, 0); }

int eventfd_returned_ok() { return eventfd(0, 0); }

// Pulse does not know that an unknown function can take ownership of a
// descriptor passed by value
void FP_eventfd_passed_to_unknown_ok() {
  int fd = eventfd(0, 0);
  if (fd != -1) {
    register_fd(fd);
  }
}

void inotify_init_not_closed_bad() { int fd = inotify_init(); }

void inotify_init1_not_closed_bad() { int fd = inotify_init1(0); }

void timerfd_create_not_closed_bad() { int fd = timerfd_create(1, 0); }

void memfd_create_not_closed_bad() { int fd = memfd_create("buffer", 0); }

void mkstemp_not_closed_bad() {
  char name[] = "/tmp/fileXXXXXX";
  int fd = mkstemp(name);
}

void mkostemp_not_closed_bad() {
  char name[] = "/tmp/fileXXXXXX";
  int fd = mkostemp(name, 0);
}

int accept_not_closed_bad(int listen_fd) {
  int fd = accept(listen_fd, NULL, NULL);
  if (fd == -1) {
    return -1;
  }
  return 0;
}

int accept4_not_closed_bad(int listen_fd) {
  int fd = accept4(listen_fd, NULL, NULL, 0);
  if (fd == -1) {
    return -1;
  }
  return 0;
}

int accept_writes_address_ok(int listen_fd) {
  struct sockaddr addr;
  socklen_t len = sizeof(addr);
  int fd = accept(listen_fd, &addr, &len);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  return addr.sa_family;
}

void accept_twice_ok(int listen_fd) {
  int fd1 = accept(listen_fd, NULL, NULL);
  int fd2 = accept(listen_fd, NULL, NULL);
  if (fd1 != -1) {
    close(fd1);
  }
  if (fd2 != -1) {
    close(fd2);
  }
}

int pipe_not_closed_bad() {
  int fds[2];
  if (pipe(fds) == -1) {
    return -1;
  }
  return 0;
}

int pipe_one_end_closed_bad() {
  int fds[2];
  if (pipe(fds) == -1) {
    return -1;
  }
  close(fds[0]);
  return 0;
}

int pipe_closed_ok() {
  int fds[2];
  if (pipe(fds) == -1) {
    return -1;
  }
  close(fds[0]);
  close(fds[1]);
  return 0;
}

// Pulse does not identify `(&fds[0])[i]`, where the model stores the
// descriptors, with `fds[i]`
int FP_pipe_address_of_first_element_ok() {
  int fds[2];
  if (pipe(&fds[0]) == -1) {
    return -1;
  }
  close(fds[0]);
  close(fds[1]);
  return 0;
}

int pipe_pointer_offset_ok(int* fds, int i) { return pipe(fds + 2 * i); }

// Pulse does not relate `slot` to the memory of `fds`, so the descriptors are
// unreachable as soon as they are created
int FP_pipe_pointer_offset_in_variable_ok(int* fds, int i) {
  int* slot = fds + 2 * i;
  return pipe(slot);
}

int pipe_unknown_destination_ok(void* ctx) { return pipe(get_fds_slot(ctx)); }

// Pulse only remembers that a pointer was returned by an unknown function when
// the call has arguments
int FP_pipe_unknown_destination_without_arguments_ok() {
  return pipe(get_global_fds());
}

int pipe2_not_closed_bad() {
  int fds[2];
  if (pipe2(fds, 0) == -1) {
    return -1;
  }
  close(fds[1]);
  return 0;
}

int socketpair_not_closed_bad() {
  int fds[2];
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == -1) {
    return -1;
  }
  return 0;
}

void socketpair_one_end_returned_ok(int* out) {
  int fds[2];
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0) {
    close(fds[0]);
    *out = fds[1];
  }
}

int popen_not_closed_bad(const char* command) {
  char buf[64];
  FILE* p = popen(command, "r");
  if (!p) {
    return -1;
  }
  if (!fgets(buf, sizeof(buf), p)) {
    return -1;
  }
  return pclose(p);
}

int popen_closed_ok(const char* command) {
  char buf[64];
  FILE* p = popen(command, "r");
  if (!p) {
    return -1;
  }
  fgets(buf, sizeof(buf), p);
  return pclose(p);
}

int popen_unchecked_bad(const char* command) {
  FILE* p = popen(command, "r");
  int c = fgetc(p);
  pclose(p);
  return c;
}

void pclose_twice_bad(const char* command) {
  FILE* p = popen(command, "r");
  if (p) {
    pclose(p);
    pclose(p);
  }
}

void tmpfile_not_closed_bad() { FILE* f = tmpfile(); }
