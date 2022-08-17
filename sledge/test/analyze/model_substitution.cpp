/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

// This test validates that modeled functions are replaced with the
// implementation of their models. That is, if __sledge_foo exists, then it is
// renamed to foo and the original foo is removed from the llair translation.
// To validate that substitution indeed occurs, we have foo implemented as an
// unsafe function and __sledge_foo implemented as a safe function.

namespace subst_test14 {
// Substitution should remove this function altogether.
void
foo10(unsigned i, char* buff)
{
  buff[0] = (char)i;
  if (i > 0) {
    foo10(i - 1, buff);
  }
  abort();
}

void
bar(unsigned i, char* buff)
{}

void
__sledge_foo10(unsigned i, char* buff)
{
  if (i > 0) {
    // Recursive calls should be substituted as well.
    __sledge_foo10(i - 1, buff);
    buff[0] = (char)i;
  }
}
}  // namespace subst_test14

using namespace subst_test14;

int
main(int argc, char* argv[])
{
  char* buff = (char*)malloc(10);
  if (buff == NULL || argc >= 10) {
    return 0;
  } else if (__llair_choice()) {
    // Direct call to a modeled function.
    foo10(argc, buff);
  } else {
    // Indirect call to a modeled function.
    void (*fptr)(unsigned, char*) = (argc > 1) ? foo10 : bar;
    fptr(argc, buff);
  }
  free(buff);

  return 0;
}
