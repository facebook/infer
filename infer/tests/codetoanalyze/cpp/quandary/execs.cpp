/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdlib>
#include <string>
#include <stdio.h>
#include <unistd.h>

extern int rand();

extern void __infer_sql_sink(std::string query, int i);

namespace execs {

// mocking gflags-generated field
extern char* FLAGS_cli_string;

extern int FLAGS_cli_int;

int callAllSinks(const char* stringSource, char** arrSource) {
  switch (rand()) {
    case 1:
      return execl(NULL, stringSource);
    case 2:
      return execl(stringSource, NULL);
    case 3:
      // just one test for varargs; assume we get it right in the other cases
      return execl(NULL, NULL, stringSource);
    case 4:
      return execlp(NULL, stringSource);
    case 5:
      return execlp(stringSource, NULL);
    case 6:
      return execle(NULL, stringSource);
    case 7:
      return execle(stringSource, NULL);
    case 8:
      return execv(stringSource, NULL);
    case 9:
      return execvp(stringSource, NULL);
    case 10:
      return execv(NULL, arrSource);
    case 11:
      return execvp(NULL, arrSource);
  }
  return 0;
}

int callExecBad() {
  const char* stringSource = std::getenv("ENV_VAR");
  char* arrSource[1] = {std::getenv("ENV_VAR")};

  switch (rand()) {
    case 1:
      return execl(NULL, stringSource);
    case 2:
      return execl(stringSource, NULL);
    case 3:
      // just one test for varargs; assume we get it right in the other cases
      return execl(NULL, NULL, stringSource);
    case 4:
      return execlp(NULL, stringSource);
    case 5:
      return execlp(stringSource, NULL);
    case 6:
      return execle(NULL, stringSource);
    case 7:
      return execle(stringSource, NULL);
    case 8:
      return execv(stringSource, NULL);
    case 9:
      return execvp(stringSource, NULL);
    case 10:
      return execv(NULL, arrSource);
    case 11:
      return execvp(NULL, arrSource);
    case 12:
      return execve(stringSource, NULL, NULL);
    case 13:
      return execve(NULL, arrSource, NULL);
    case 14:
      return system(stringSource);
    case 15:
      FILE* f = popen(stringSource, "w");
      return pclose(f);
  }
  return 0;
}

extern char* getenv(const char* var);

void execConstantStringOk() { callAllSinks("something.sh", NULL); }
void customGetEnvOk() {
  const char* source = execs::getenv("ENV_VAR");
  return execl(NULL, source);
}

void exec_string_flag_bad() { execl(FLAGS_cli_string, NULL); }

void exec_int_flag_ok() {
  char buffer[25];
  sprintf(buffer, "foo -i %d", FLAGS_cli_int);
  execl(buffer, NULL);
}

char* return_global() {
  char* local = FLAGS_cli_string;
  return local;
}

void exec_string_flag_interproc_bad() {
  char* flag = return_global();
  execl(flag, NULL);
}

void sql_on_env_var_bad() {
  std::string source = (std::string)std::getenv("ENV_VAR");
  __infer_sql_sink(source, 0);
}

void tainted_flag_popen_ok() {
  char* tainted = std::getenv("something");
  popen("ls", tainted); // should not warn;
}

class Obj {
  void endpoint(int i,
                char c,
                std::string s,
                char* c_ptr,
                char c_arr[],
                std::string* s_ptr) {
    __infer_sql_sink(nullptr, i); // don't report
    __infer_sql_sink(nullptr, c); // don't report

    __infer_sql_sink(s, 0); // report
    __infer_sql_sink(*s_ptr, 0); // report
    __infer_sql_sink(c_ptr, 0); // report
    __infer_sql_sink(c_arr, 0); // report
  }
};
} // namespace execs
