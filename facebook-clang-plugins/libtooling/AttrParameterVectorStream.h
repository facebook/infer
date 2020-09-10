/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <llvm/Support/VersionTuple.h>
#include <string>
#include <vector>

namespace ASTLib {

class AttrParameterVectorStream {

 private:
  std::vector<std::string> Content;

 public:
  AttrParameterVectorStream() {}

  AttrParameterVectorStream &operator<<(const std::string &str);
  AttrParameterVectorStream &operator<<(const unsigned int x);
  AttrParameterVectorStream &operator<<(const llvm::VersionTuple &verTup);

  const std::vector<std::string> &getContent();
};

} // end of namespace ASTLib
