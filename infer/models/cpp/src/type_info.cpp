/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace std {

class type_info {

  type_info& operator=(const type_info&);
  type_info(const type_info&);

 protected:
  const char* __type_name;

  explicit type_info(const char* __n) : __type_name(__n) {}

 public:
  virtual ~type_info();

  const char* name() const noexcept { return __type_name; }

  bool operator==(const type_info& __arg) const noexcept {
    return __type_name == __arg.__type_name;
  }

  bool operator!=(const type_info& __arg) const noexcept {
    return !operator==(__arg);
  }
};
}
