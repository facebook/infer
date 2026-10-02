/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <fcntl.h>
#include <unistd.h>

namespace owned_fd {

class OwnedFd {
 public:
  explicit OwnedFd(int fd) : fd_(fd) {}
  ~OwnedFd() {
    if (fd_ != -1) {
      close(fd_);
    }
  }

  bool operator<(int n) const { return fd_ < n; }
  bool operator>=(int n) const { return fd_ >= n; }
  int get() const { return fd_; }
  int release() {
    int fd = fd_;
    fd_ = -1;
    return fd;
  }

 private:
  int fd_;
};

int read_byte(int fd, char* c);

int less_than_check_return_param_ok(const char* path, int x) {
  OwnedFd fd(open(path, O_RDONLY, 0));
  if (fd < 0) {
    return -1;
  }
  return x;
}

int less_than_check_return_out_param_ok(const char* path) {
  OwnedFd fd(open(path, O_RDONLY, 0));
  if (fd < 0) {
    return -1;
  }
  char c;
  read_byte(fd.get(), &c);
  return c;
}

int greater_equal_check_return_param_ok(const char* path, int x) {
  OwnedFd fd(open(path, O_RDONLY, 0));
  if (!(fd >= 0)) {
    return -1;
  }
  return x;
}

int less_than_check_release_bad(const char* path, int x) {
  OwnedFd fd(open(path, O_RDONLY, 0));
  if (fd < 0) {
    return -1;
  }
  fd.release();
  return x;
}

} // namespace owned_fd
