/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int
main(int argc, char* argv[])
{
  return 0;
}

void
_llair_main()
{
  long argc = __llair_choice();
  __llair_assume(argc >= 0);
  char** argv = __llair_alloc(sizeof(char*) * argc);
  for (long i = 0; i < argc; ++i) {
    long arglen = __llair_choice();
    __llair_assume(arglen > 0);
    argv[i] = __llair_alloc(sizeof(char) * arglen);
    argv[i][arglen - 1] = 0;
  }
  main(argc, argv);
}
