/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <clang/AST/Decl.h>
#include <string>

namespace FileUtils {

/**
 * Simplify away "." and ".." elements.
 * If pathToNormalize is a relative path, it will be pre-pended with
 * currentWorkingDirectory unless currentWorkingDirectory == "".
 */
std::string makeAbsolutePath(const std::string &currentWorkingDirectory,
                             std::string path);

/**
 * Try to delete a prefix "repoRoot/" OR "sysRoot" from the given absolute path.
 * If no rule applies AND keepExternalPaths is true, return the same path,
 * otherwise return the empty string.
 */
std::string makeRelativePath(const std::string &repoRoot,
                             const std::string &sysRoot,
                             bool keepExternalPaths,
                             bool allowSiblingsToRoot,
                             const std::string &path);

} // namespace FileUtils
