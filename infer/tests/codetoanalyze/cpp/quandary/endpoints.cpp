/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <string>

extern void __infer_sql_sink(std::string);
extern std::string __infer_all_sanitizer(std::string);
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
  void service1_endpoint_bad(std::string formal) {
    // this should report REMOTE_CODE_EXECUTION_RISK
    system(formal.c_str());
  }

  // this is specified as user-controlled in .inferconfig
  void user_controlled_endpoint_to_sql_bad(std::string formal) {
    // this should report SQL_INJECTION
    __infer_sql_sink(formal);
  }

  // this is specified as user-controlled in .inferconfig
  void user_controlled_endpoint_to_shell_bad(std::string formal) {
    // this should report SHELL_INJECTION
    system(formal.c_str());
  }

  void unsanitized_sql_bad(std::string formal) {
    // this should report REMOTE_CODE_EXECUTION_RISK
    __infer_sql_sink(formal);
  }

  void sanitized_sql_bad(std::string formal) {
    // this should report USER_CONTROLLED_SQL_RISK
    __infer_sql_sink(__infer_string_sanitizer(formal));
  }

  void service1_endpoint_sql_sanitized_ok(std::string formal) {
    __infer_sql_sink(__infer_all_sanitizer(formal));
  }

  void service1_endpoint_shell_sanitized_ok(std::string formal) {
    system(__infer_string_sanitizer(formal).c_str());
  }

 private:
  void private_not_endpoint_ok(std::string formal) { system(formal.c_str()); }
};

class Service2 : facebook::fb303::cpp2::FacebookServiceSvAsyncIf {

 public:
  void service2_endpoint_bad(std::string formal) {
    // this should report REMOTE_CODE_EXECUTION_RISK
    system(formal.c_str());
  }
};

class Service3 : Service1 {
 public:
  void service3_endpoint_bad(std::string formal) {
    // this should report REMOTE_CODE_EXECUTION_RISK
    system(formal.c_str());
  }
};

} // namespace endpoints
