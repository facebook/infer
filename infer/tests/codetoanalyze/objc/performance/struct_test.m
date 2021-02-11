/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

typedef struct MSpec_t {
  const char* fileName;
} MSpec;

#define SPEC_FILE_NAME(name) .fileName = (name),

static const MSpec mSpecs[] = {
    {SPEC_FILE_NAME("api")},
    {SPEC_FILE_NAME("graph")},
    {SPEC_FILE_NAME("disk")},
    {SPEC_FILE_NAME("io")},
};

void const_struct_field_read_constant() {
  MSpec spec = mSpecs[0];
  NSString* fileName =
      @(spec.fileName); // filename is constant, so this should be O(1)
}

void loop_array_struct_constant() {
  const MSpec* const endSpec = ((const MSpec*)mSpecs) + ARRAY_COUNT(mSpecs);
  for (const MSpec* spec = (const MSpec*)mSpecs; spec < endSpec; spec++) {
    NSString* fileName = @(spec->fileName);
  }
}
