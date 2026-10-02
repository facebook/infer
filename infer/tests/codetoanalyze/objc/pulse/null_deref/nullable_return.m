/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

// objc/pulse-nullability-annotations analyzes this file with
// --pulse-nullability-annotations

struct nullable_return_item {
  int data;
};

__attribute__((objc_root_class))
@interface NullableReturnObj {
 @public
  int _x;
}
- (int)x;
@end

// declared but not defined: calls to these functions are unknown to Pulse
struct nullable_return_item* _Nullable nullable_return_find_item(void);
struct nullable_return_item* _Nonnull nullable_return_get_item(void);
Class _Nonnull* _Nullable nullable_return_find_classes(void);
NullableReturnObj* _Nullable nullable_return_find_obj(void);
void (^_Nullable nullable_return_find_block(void))(void);

int nullable_c_struct_return_deref_bad(void) {
  return nullable_return_find_item()->data;
}

int nonnull_c_struct_return_null_branch_ok(void) {
  struct nullable_return_item* item = nullable_return_get_item();
  if (item == NULL) {
    int* leaked = malloc(sizeof(int));
    return -1;
  }
  return item->data;
}

Class nullable_class_array_return_deref_bad(void) {
  return nullable_return_find_classes()[0];
}

int nullable_objc_object_return_ivar_bad(void) {
  return nullable_return_find_obj()->_x;
}

int nullable_objc_object_return_message_ok(void) {
  return [nullable_return_find_obj() x];
}

void nullable_block_return_call_bad(void) { nullable_return_find_block()(); }

@implementation NullableReturnObj

- (int)x {
  return _x;
}

- (int)nullable_c_struct_return_deref_in_method_bad {
  return nullable_return_find_item()->data;
}

@end

typedef struct objc_property* objc_property_t;

// declared like the Objective-C runtime, which returns NULL only after setting
// *outCount to 0
objc_property_t _Nonnull* _Nullable class_copyPropertyList(
    Class _Nullable cls, unsigned int* _Nullable outCount);
const char* _Nonnull property_getName(objc_property_t _Nonnull property);
Class _Nonnull* _Nullable objc_copyClassList(unsigned int* _Nullable outCount);
const char* _Nonnull class_getName(Class _Nullable cls);

int copy_property_list_loop_ok(Class cls) {
  unsigned int count;
  objc_property_t* properties = class_copyPropertyList(cls, &count);
  int n = 0;
  for (unsigned int i = 0; i < count; i++) {
    n += property_getName(properties[i])[0];
  }
  return n;
}

const char* copy_property_list_first_bad(Class cls) {
  objc_property_t* properties = class_copyPropertyList(cls, NULL);
  return property_getName(properties[0]);
}

int copy_class_list_loop_ok(void) {
  unsigned int count;
  Class* classes = objc_copyClassList(&count);
  int n = 0;
  for (unsigned int i = 0; i < count; i++) {
    n += class_getName(classes[i])[0];
  }
  return n;
}

const char* copy_class_list_first_bad(void) {
  Class* classes = objc_copyClassList(NULL);
  return class_getName(classes[0]);
}
