# Copyright (c) 2004 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

set(MODULE TRUE)

set(LLVM_LINK_COMPONENTS support mc)

add_clang_library(infer-plugin InferPlugin.cpp)

add_dependencies(infer-plugin
  ClangAttrClasses
  ClangAttrList
  ClangCommentNodes
  ClangDeclNodes
  ClangDiagnosticCommon
  ClangStmtNodes
  )

target_link_libraries(infer-plugin
  clangFrontend
  clangAST
  )

set_target_properties(infer-plugin
  PROPERTIES
  LINKER_LANGUAGE CXX
  PREFIX "")
