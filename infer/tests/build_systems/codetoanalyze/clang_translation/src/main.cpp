/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>
#include <string>
#include "main.h"

#include "external/external_lib.h"
#include "exclude_dir/lib.h"

int main() {
  internal::fun(1); // internal::fun definition should be translated
  internal_exclude::fun(1); // internal_exclude::fun shouldn't be translated
  external::fun(1); // external::fun definition shouldn't be translated
  std::shared_ptr<int> x; // shared_ptr::shared_ptr model should be translated
  std::string s("1234"); // string::string shouldn't be translated (there is no
  // model for it)
}
