/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* Simple mutual recursion. Sledge should respect execution
 * bounds and terminate quickly for small bounds */
void mutal_rec_d();
void mutal_rec_a() { mutal_rec_d(); }
void mutal_rec_d() { mutal_rec_a(); }
void recurse() { recurse(); }
int main() {
  recurse();
  mutal_rec_a();
  return 0;
}
