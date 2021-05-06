/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

// set element
void nsmutabledictionary_set_element_in_loop_linear(NSMutableDictionary* dict) {
  for (NSString* key in dict) {
    dict[key] = key;
  }
}

void nsmutabledictionary_removeAll_linear(NSMutableDictionary* dict) {
  [dict removeAllObjects];
}

void nsmutabledictionary_addAll_linear(NSMutableDictionary* dict1,
                                       NSMutableDictionary* dict2) {
  [dict1 addEntriesFromDictionary:dict2];
}

void nsmutabledictionary_init_linear(NSMutableDictionary* dict) {
  NSMutableDictionary* new_dict =
      [[NSMutableDictionary alloc] initWithDictionary:dict];
}

void nsmutabledictionary_dict_init_linear(NSMutableDictionary* dict) {
  NSMutableDictionary* new_dict =
      [NSMutableDictionary dictionaryWithDictionary:dict];
  for (NSString* key in new_dict.allKeys) {
  }
}
