/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <string>

extern void __infer_sql_sink(std::string);
extern void __infer_sql_read_sink(std::string);
extern void __infer_sql_write_sink(std::string);
extern std::string __infer_shell_sanitizer(std::string);
extern std::string __infer_sql_sanitizer(std::string);

extern void curl_easy_setopt(void*, int, ...);

struct request {
  std::string s;
  int i;
};

namespace gflags {

// Mock gflags SetCommandLineOption.
void SetCommandLineOption(const char* name, const char* value) {}
} // namespace gflags

namespace facebook {
namespace fb303 {
namespace cpp2 {

class FacebookServiceSvIf {
 public:
  void service1_endpoint_bad(std::string formal);
  void user_controlled_endpoint_to_sql_bad(std::string formal);
  void unsanitized_sql_bad(std::string formal);
  void sanitized_sql_with_shell_bad(std::string formal);
  void service1_endpoint_sql_sanitized_bad(std::string formal);
  void service1_endpoint_sql_read_bad(std::string formal);
  void service1_endpoint_sql_write_bad(std::string formal);
  void service1_endpoint_shell_sanitized_ok(std::string formal);
  void service1_endpoint_struct_string_field_bad(request formal);
  void open_or_create_c_style_file_bad(const char* filename);
  void ofstream_open_file_bad(std::string filename);
  void ifstream_open_file_bad(std::string filename);
  void fstream_open_file_bad(std::string filename);
  void endpoint_to_curl_url_bad(request formal);
  void endpoint_to_curl_url_exp_bad(request formal);
  void endpoint_to_curl_url_unknown_exp_bad(request formal, int i);
  void endpoint_to_curl_other_const_ok(request formal);
  void endpoint_to_curl_other_exp_ok(request formal);
  void FP_service1_endpoint_struct_int_field_ok(request formal);
  void service_this_ok();
  void service_return_param_ok(std::string& _return);
  void service3_endpoint_bad(std::string formal);
  void service3_endpoint_envchange_putenv_bad(std::string formal);
  void service3_endpoint_envchange_setoption_bad(std::string formal);

 private:
  void FP_private_not_endpoint_ok(std::string formal) {
    system(formal.c_str());
  }
};

class FacebookServiceSvAsyncIf {
 public:
  void service2_endpoint_bad(std::string formal);
};
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

  void sanitized_sql_with_shell_bad(std::string formal) {
    // this should report REMOTE_CODE_EXECUTION_RISK
    __infer_sql_sink(__infer_shell_sanitizer(formal));
  }

  void service1_endpoint_sql_sanitized_bad(std::string formal) {
    // this should report USER_CONTROLLED_SQL_RISK
    __infer_sql_sink(__infer_sql_sanitizer(formal));
  }

  void service1_endpoint_sql_read_bad(std::string formal) {
    // this should report USER_CONTROLLED_SQL_RISK
    __infer_sql_read_sink(formal);
  }

  void service1_endpoint_sql_write_bad(std::string formal) {
    // this should report USER_CONTROLLED_SQL_RISK
    __infer_sql_write_sink(formal);
  }

  void service1_endpoint_shell_sanitized_ok(std::string formal) {
    system(__infer_shell_sanitizer(formal).c_str());
  }

  void service1_endpoint_struct_string_field_bad(request formal) {
    system(formal.s.c_str());
  }

  void open_or_create_c_style_file_bad(const char* filename) {
    open(filename, 0);
    openat(1, filename, 2);
    creat(filename, 3);
    fopen(filename, "w");
    freopen(filename, "w", nullptr);
    rename(filename, "mud");
  }

  void ofstream_open_file_bad(std::string filename) {
    std::ofstream file1(filename);
    std::ofstream file2;
    file2.open(filename);
  }

  void ifstream_open_file_bad(std::string filename) {
    std::ifstream file1(filename);
    std::ifstream file2;
    file2.open(filename);
  }

  void fstream_open_file_bad(std::string filename) {
    std::fstream file1(filename);
    std::fstream file2;
    file2.open(filename);
  }

  const int CURLOPT_URL = 10002;

  void endpoint_to_curl_url_bad(request formal) {
    curl_easy_setopt(nullptr, CURLOPT_URL, formal.s.c_str());
  }

  void endpoint_to_curl_url_exp_bad(request formal) {
    curl_easy_setopt(nullptr, 10000 + 2, formal.s.c_str());
  }

  void endpoint_to_curl_url_unknown_exp_bad(request formal, int i) {
    curl_easy_setopt(nullptr, i + 17, formal.s.c_str());
  }

  void endpoint_to_curl_other_const_ok(request formal) {
    curl_easy_setopt(nullptr, 0, formal.s.c_str());
  }

  void endpoint_to_curl_other_exp_ok(request formal) {
    curl_easy_setopt(nullptr, 1 + 2, formal.s.c_str());
  }

  void FP_service1_endpoint_struct_int_field_ok(request formal) {
    system(std::to_string(formal.i).c_str());
  }

  void service_this_ok() {
    // endpoint object itself should not be treated as tainted
    system((const char*)this);
  }

  void service_return_param_ok(std::string& _return) {
    // dummy return object should not be treated as tainted
    system(_return.c_str());
  }

  // shadows a private method of super; not an override, but we'll flag it as
  // one because the code that checks for overrides can't see access modifiers.
  void FP_private_not_endpoint_ok(std::string formal) {
    system(formal.c_str());
  }

  // doesn't override a method from super
  void non_override_ok(std::string formal) { system(formal.c_str()); }

 private:
  // similar to above, but even easier
  void private_non_override_ok(std::string formal) { system(formal.c_str()); }
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

  void service3_endpoint_envchange_putenv_bad(std::string formal) {
    // this should report UNTRUSTED_ENVIRONMENT_CHANGE_RISK
    putenv(const_cast<char*>(formal.c_str()));
  }

  void service3_endpoint_envchange_setoption_bad(std::string formal) {
    // this should report UNTRUSTED_ENVIRONMENT_CHANGE_RISK
    gflags::SetCommandLineOption("teest", formal.c_str());
  }
};

} // namespace endpoints
