# Copyright 2004-present Facebook. All Rights Reserved. 

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
