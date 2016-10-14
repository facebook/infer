/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <cstdlib>
#include <unistd.h>

extern int rand();

namespace execs {

int callAllSinks(const char* stringSource) {
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
  }
  return 0;
}

void callExecBad() {
  const char* source = std::getenv("ENV_VAR");
  callAllSinks(source);
}

// need to treat an array as tainted when its contents are tainted to get these
// two
void FN_envVarFlowsToEnvironment() {
  char* execEnv[1] = {std::getenv("ENV_VAR")};
  execv("something.s", execEnv);
  execvp("something.sh", execEnv);
}

extern char* getenv(const char* var);

void execConstantStringOk() { callAllSinks("something.sh"); }

void customGetEnvOk() {
  const char* source = execs::getenv("ENV_VAR");
  callAllSinks(source);
}
}
