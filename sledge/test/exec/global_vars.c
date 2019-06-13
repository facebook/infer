/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* The symbolic heap for this example should look like:
  ^ %.str --> <13,{}> * %c1 --> <8,%.str> * %retval --> <4,0>
  instead of
      ^ %.str --> <13,{}>
      * %a_string --> <8,%.str>
      * %an_int --> <4,0>
      * %c1 --> <8,%.str>
      * %retval --> <4,0>
  Which has an_int and a_string indirections. There can be obtained by
  sledge.dbg llvm analyze -trace Domain.call global_vars.bc */

const char* a_string = "I'm a string";
int an_int = 0;

int c() { return an_int; }

int main() {
  const char* c1 = a_string;

  return c();
}
