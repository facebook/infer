/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "AttrParameterVectorStream.h"

#include <llvm/Support/raw_ostream.h>

namespace ASTLib {

AttrParameterVectorStream &AttrParameterVectorStream::operator<<(
    const std::string &str) {
  // hack to get rid of spurious leading " "s
  if (str != " ") {
    Content.push_back(str);
  }
  return *this;
}

AttrParameterVectorStream &AttrParameterVectorStream::operator<<(
    const unsigned int x) {
  return operator<<(std::to_string(x));
}

AttrParameterVectorStream &AttrParameterVectorStream::operator<<(
    const llvm::VersionTuple &verTup) {
  return operator<<(verTup.getAsString());
}

const std::vector<std::string> &AttrParameterVectorStream::getContent() {
  return Content;
}

} // end of namespace ASTLib
