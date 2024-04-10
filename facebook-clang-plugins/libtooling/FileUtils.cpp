/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <clang/AST/AST.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Path.h>
#include <vector>

#include "FileUtils.h"

namespace FileUtils {

/**
 * Simplify away "." and ".." elements.
 * If pathToNormalize is a relative path, it will be pre-pended with
 * currentWorkingDirectory unless currentWorkingDirectory == "".
 */
std::string makeAbsolutePath(const std::string &currentWorkingDirectory,
                             std::string path) {
  llvm::SmallVector<char, 16> result;
  std::vector<std::string> elements;
  int skip = 0;

  if (llvm::sys::path::is_relative(path)) {
    // Prepend currentWorkingDirectory to path (unless currentWorkingDirectory
    // is empty).
    llvm::SmallVector<char, 16> vec(currentWorkingDirectory.begin(),
                                    currentWorkingDirectory.end());
    llvm::sys::path::append(vec, path);
    path = std::string(vec.begin(), vec.end());
  } else {
    // Else copy the separator to maintain an absolute path.
    result.append(1, path.front());
  }

  elements.push_back(llvm::sys::path::filename(path).str());

  while (llvm::sys::path::has_parent_path(path)) {
    path = llvm::sys::path::parent_path(path).str();
    const std::string &element(llvm::sys::path::filename(path).str());
    if (element == ".") {
      continue;
    }
    if (element == "..") {
      skip++;
      continue;
    }
    if (skip > 0) {
      skip--;
      continue;
    }
    elements.push_back(element);
  }
  while (skip > 0) {
    elements.push_back("..");
    skip--;
  }

  for (auto I = elements.rbegin(), E = elements.rend(); I != E; I++) {
    llvm::sys::path::append(result, *I);
  }
  return std::string(result.begin(), result.end());
}

std::string makeRelativePath(const std::string &repoRoot,
                             const std::string &sysRoot,
                             bool keepExternalPaths,
                             bool allowSiblingsToRepoRoot,
                             const std::string &path) {
  if (repoRoot != "") {
    if (llvm::StringRef(path).starts_with(repoRoot + "/")) {
      return path.substr(repoRoot.size() + 1);
    }
    if (allowSiblingsToRepoRoot) {
      std::string parentOfRoot = llvm::sys::path::parent_path(repoRoot).str();
      if (llvm::StringRef(path).starts_with(parentOfRoot + "/")) {
        return "../" + path.substr(parentOfRoot.size() + 1);
      }
    }
  }
  if (sysRoot != "" && llvm::StringRef(path).starts_with(sysRoot + "/")) {
    // Intentionally keep the heading "/" in this case.
    return path.substr(sysRoot.size());
  }
  if (keepExternalPaths) {
    return path;
  }
  return "";
}

} // namespace FileUtils
