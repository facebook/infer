/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace anamespace {
void the_function() {}

void the_other_function() {}
} // namespace anamespace

namespace anothernamespace {
void the_function() {}

void the_other_function() {}
} // namespace anothernamespace

int main() {
  anamespace::the_function();
  anothernamespace::the_function();
  anamespace::the_other_function();
  anothernamespace::the_other_function();
  {
    using namespace anamespace;
    the_function();
    the_other_function();
  }
  {
    using namespace anothernamespace;
    the_function();
    the_other_function();
  }
  return 0;
}
