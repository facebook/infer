/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <string>

extern std::string __infer_string_sanitizer(std::string);

namespace facebook {
namespace fb303 {
namespace cpp2 {

class FacebookServiceSvIf {};

class FacebookServiceSvAsyncIf {};
} // namespace cpp2
} // namespace fb303
} // namespace facebook

namespace endpoints {

class Service1 : facebook::fb303::cpp2::FacebookServiceSvIf {

 public:
  void service1_endpoint_bad(std::string formal) { system(formal.c_str()); }

  void service1_endpoint_sanitized_ok(std::string formal) {
    system(__infer_string_sanitizer(formal).c_str());
  }

 private:
  void private_not_endpoint_ok(std::string formal) { system(formal.c_str()); }
};

class Service2 : facebook::fb303::cpp2::FacebookServiceSvAsyncIf {

 public:
  void service2_endpoint_bad(std::string formal) { system(formal.c_str()); }
};

class Service3 : Service1 {
 public:
  void service3_endpoint_bad(std::string formal) { system(formal.c_str()); }
};

} // namespace endpoints
