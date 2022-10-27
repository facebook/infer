/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

int* release(int* object);

int* get_ctyperef(void) {
  int* p = NULL;
  return p;
}

int release_model_found_bad() {
  int* value = get_ctyperef();
  int* bridged_value = release(value);
  return *bridged_value;
}

@interface ModelClass : NSObject

@property int x;

- (int*)release:(int*)object;

- (id)initWith:(int)x;

+ (int*)bridge:(int*)object;

@end

int release_method_model_found_bad() {
  int* value = get_ctyperef();
  ModelClass* c = [[ModelClass alloc] init];
  int* bridged_value = [c release:value];
  return *bridged_value;
}

int bridge_method_model_found_bad() {
  int* value = get_ctyperef();
  ModelClass* c = [[ModelClass alloc] init];
  int* bridged_value = [ModelClass bridge:value];
  return *bridged_value;
}

int init_found_self_good() {
  int* value = get_ctyperef();
  ModelClass* c = [[ModelClass alloc] initWith:5];
  if (!c) {
    return *value;
  }
  return 0;
}

int* release_fst(int* object, int z);

int release_fst_found_bad() {
  int* value = get_ctyperef();
  int* bridged_value = release_fst(value, 5);
  return *bridged_value;
}
