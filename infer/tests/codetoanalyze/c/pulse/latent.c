/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void latent_use_after_free(int b, int* x) {
  if (b) {
    free(x);
  }
  *x = 42;
  if (!b) {
    // just to avoid memory leaks
    free(x);
  }
}

// Filtered out
void manifest_use_after_free(int* x) { latent_use_after_free(1, x); }

void deref_then_free_then_deref_bad(int* x) {
  *x = 42;
  free(x);
  *x = 42;
}

// FN because it's flagged only as latent at the moment
void FN_nonlatent_use_after_free_bad(int b, int* x) {
  // the branch is independent of the issue here, so we should report the issue
  // in this function
  if (b) {
  }
  free(x);
  *x = 42;
}

// all latent issues that reach main are manifest, so this should be called
// "main_bad" but that would defeat the actual point :)
int main(int argc, char** argv) {
  int* x = malloc(sizeof(int));
  if (x) {
    latent_use_after_free(argc, x);
  }
}
