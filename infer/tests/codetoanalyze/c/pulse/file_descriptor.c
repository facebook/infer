/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <fcntl.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

int read_after_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  char buf[4];
  return read(fd, buf, sizeof(buf));
}

int write_after_close_bad() {
  int fd = open("hi.txt", O_WRONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  char buf[4] = {0};
  return write(fd, buf, sizeof(buf));
}

int ioctl_after_close_bad() {
  int fd = open("hi.txt", O_RDWR, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  return ioctl(fd, 42, 0);
}

int fstat_after_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  struct stat st;
  return fstat(fd, &st);
}

int dup_after_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  return dup(fd);
}

int read_after_close_long_fd_bad() {
  long fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  char buf[4];
  return read(fd, buf, sizeof(buf));
}

void double_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  close(fd);
  close(fd);
}

int read_after_syscall_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  syscall(SYS_close, fd);
  char buf[4];
  return read(fd, buf, sizeof(buf));
}

void close_fd(int fd) { close(fd); }

int read_after_close_in_callee_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close_fd(fd);
  char buf[4];
  return read(fd, buf, sizeof(buf));
}

int read_fd(int fd) {
  char buf[4];
  return read(fd, buf, sizeof(buf));
}

int read_fd_indirect(int fd) { return read_fd(fd); }

int read_in_callee_after_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  return read_fd(fd);
}

int read_in_nested_callee_after_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  return read_fd_indirect(fd);
}

int read_after_reopen_ok() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  close(fd);
  fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return -1;
  }
  int ret = read_fd(fd);
  close(fd);
  return ret;
}

int read_stdin_ok() {
  char buf[4];
  return read(STDIN_FILENO, buf, sizeof(buf));
}

int write_stdout_ok() {
  char buf[4] = {0};
  return write(STDOUT_FILENO, buf, sizeof(buf));
}

int read_fd_constants_ok() {
  return read_fd(STDIN_FILENO) + read_fd(STDERR_FILENO) + read_fd(-1) +
         read_fd_indirect(STDIN_FILENO) + read_fd_indirect(STDERR_FILENO);
}

int ioctl_writes_argument_ok(int fd) {
  int arg;
  if (ioctl(fd, 42, &arg) == -1) {
    return -1;
  }
  return arg;
}

void fcntl_twice_equal_ok(int fd) {
  if (fcntl(fd, F_GETFL) != fcntl(fd, F_GETFL)) {
    int* p = NULL;
    *p = 42;
  }
}

void close_negative_fd_continues_bad() {
  close(-1);
  int* p = NULL;
  *p = 42;
}

void close_negative_fd_fails_ok() {
  if (close(-1) != -1) {
    int* p = NULL;
    *p = 42;
  }
}

void close_negative_fd_in_callee_continues_bad() {
  close_fd(-1);
  int* p = NULL;
  *p = 42;
}

void close_failed_open_continues_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  close(fd);
  if (fd == -1) {
    int* p = NULL;
    *p = 42;
  }
}

void close_stdout_continues_bad() {
  close(STDOUT_FILENO);
  int* p = NULL;
  *p = 42;
}

int fsync_after_fdopen_ok() {
  int fd = open("hi.txt", O_WRONLY, 0);
  if (fd == -1) {
    return -1;
  }
  FILE* f = fdopen(fd, "w");
  if (f == NULL) {
    close(fd);
    return -1;
  }
  fflush(f);
  int ret = fsync(fd);
  fclose(f);
  return ret;
}

FILE* stream_of_fd(int fd) { return fdopen(fd, "r"); }

void fdopen_in_callee_ok() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  FILE* f = stream_of_fd(fd);
  if (f == NULL) {
    close(fd);
    return;
  }
  fclose(f);
}

void close_in_callee_after_fdopen_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  FILE* f = fdopen(fd, "r");
  if (f == NULL) {
    close(fd);
    return;
  }
  close_fd(fd);
  fclose(f);
}

void read_then_close_fd(int fd) {
  char c;
  read(fd, &c, 1);
  close(fd);
}

void fdopen_then_callee_read_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  FILE* f = fdopen(fd, "r");
  if (f == NULL) {
    close(fd);
    return;
  }
  read_then_close_fd(fd);
  fclose(f);
}

void fdopen_after_close_bad() {
  int fd = open("hi.txt", O_RDONLY, 0);
  if (fd == -1) {
    return;
  }
  close(fd);
  FILE* f = fdopen(fd, "r");
  if (f != NULL) {
    fclose(f);
  }
}

void fdopen_stdout_continues_bad() {
  FILE* f = fdopen(STDOUT_FILENO, "w");
  if (f != NULL) {
    fclose(f);
    int* p = NULL;
    *p = 42;
  }
}

// descriptor numbers are reused by open() and dup(), so constant descriptors
// such as STDOUT_FILENO or 1, also when stored in a variable, are not tracked
// as closed
int FN_write_after_close_stdout_bad() {
  close(STDOUT_FILENO);
  char buf[4] = {0};
  return write(STDOUT_FILENO, buf, sizeof(buf));
}

int FN_literal_fd_write_after_close_bad() {
  int fd = 1;
  close(fd);
  char buf[4] = {0};
  return write(fd, buf, sizeof(buf));
}

// dup2() makes fd refer to an open file again but is not modelled, so fd stays
// closed
int FP_dup2_after_close_ok(int fd, int other) {
  close(fd);
  if (dup2(other, fd) == -1) {
    return -1;
  }
  char buf[4];
  return read(fd, buf, sizeof(buf));
}

// the new descriptors replace the standard ones for the rest of the process,
// but are reported as leaked
void FP_daemonize_close_std_fds_ok() {
  close(STDIN_FILENO);
  close(STDOUT_FILENO);
  close(STDERR_FILENO);
  int fd = open("/dev/null", O_RDWR, 0);
  dup(fd);
  dup(fd);
}
