/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>
#include <string>

namespace folly {

template <class T>
class fbvector;

template <class T>
struct Optional;

class fbstring;
class StringPiece;

template <class Delim, class String, class OutputType>
void split(const Delim& delimiter,
           const String& input,
           std::vector<OutputType>& out,
           const bool ignoreEmpty = false);

template <class Delim, class String, class OutputType>
void split(const Delim& delimiter,
           const String& input,
           folly::fbvector<OutputType>& out,
           const bool ignoreEmpty = false);

template <class OutputValueType,
          class Delim,
          class String,
          class OutputIterator>
void splitTo(const Delim& delimiter,
             const String& input,
             OutputIterator out,
             const bool ignoreEmpty = false);

} // namespace folly

namespace folly_split {
std::string do_not_ignore_empty_Good(const std::string& s) {
  std::vector<std::string> v;
  folly::split("delimiter", s, v);
  return v[0];
}

std::string do_not_ignore_empty2_Good(const std::string& s) {
  std::vector<std::string> v;
  folly::split("delimiter", s, v, false);
  return v[0];
}

std::string do_not_ignore_empty_Bad(const std::string& s) {
  std::vector<std::string> v;
  folly::split("delimiter", s, v, true);
  return v[0];
}
} // namespace folly_split
