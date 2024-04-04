/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

// Caricature of folly::basic_fbstring without the union between small
// and medium/large data representations, and with an explicit field
// for category_ instead of bitmasking part of the data value.
// ref:
// https://github.com/facebook/folly/blob/72850c2ebfb94d87bea74d89fcf79f3aaa91a627/folly/FBString.h

enum category {
  small = 0, // ignore small strings for now
  medium = 1,
  large = 2,
};

void* checkedMalloc(size_t size) {
  void* ptr = malloc(size);
  if (ptr == nullptr) {
    exit(1);
  }
  return ptr;
}

struct LikeFBString {
  int category_;
  char* buffer_;
  size_t size_;
  unsigned int* refcount_;

  LikeFBString() { category_ = 0; }

  LikeFBString(const LikeFBString& src) {
    category_ = src.category();
    switch (src.category_) {
      case small:
        break;
      case medium:
        copyMedium(src);
        break;
      case large:
        copyLarge(src);
        break;
      default:
        exit(2);
    }
  }

  ~LikeFBString() {
    if (category() == medium) {
      free(buffer_);
    } else if (category() == large) {
      decr_ref_count();
    }
  }

  void copySmall(const LikeFBString& src) {}

  void copyMedium(const LikeFBString& src) {
    buffer_ = (char*)checkedMalloc(src.size_);
    size_ = src.size_;
  }

  void copyLarge(const LikeFBString& src) {
    buffer_ = src.buffer_;
    size_ = src.size_;
    refcount_ = src.refcount_;
    *refcount_ = *refcount_ + 1;
  }

  int category() const { return category_; }

  void decr_ref_count() {
    if (*refcount_ <= 0) {
      exit(1);
    }
    *refcount_ = *refcount_ - 1;
    if (*refcount_ == 0) {
      free(buffer_);
    }
  }
};

void copy_fbstring(LikeFBString& s) {
  // this might alias the underlying buffers if the string is large in
  // that case the destruction of t does not de-allocate its buffer
  // but pulse might think it does if it fails to remember which
  // category t belongs to and follows impossibly control flow
  LikeFBString t = s;
}

void pass_to_copy_ok() {
  LikeFBString s;
  copy_fbstring(s);
}
