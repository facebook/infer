/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <functional>
#include <iostream>
#include <memory>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <unordered_map>

#include <llvm/Support/Path.h>

#include "FileUtils.h"
#include "SimplePluginASTAction.h"

namespace ASTPluginLib {

/**
 * The actual prefix to prepend to environment variables (but not for the
 * command line).
 */
const std::string PluginASTOptionsBase::envPrefix = "CLANG_FRONTEND_PLUGIN__";

PluginASTOptionsBase::argmap_t PluginASTOptionsBase::makeMap(
    const std::vector<std::string> &args) {
  argmap_t map;
  for (auto arg : args) {
    size_t pos = arg.find('=');
    if (pos != std::string::npos) {
      std::string key = arg.substr(0, pos);
      std::string value = arg.substr(pos + 1);
      map[key] = value;
    }
  }
  return map;
}

bool PluginASTOptionsBase::loadString(const argmap_t &map,
                                      const char *key,
                                      std::string &val) {
  auto I = map.find(key);
  if (I != map.end()) {
    val = I->second;
    return true;
  }
  const char *s = getenv((envPrefix + key).c_str());
  if (s != nullptr) {
    val = s;
    return true;
  }
  return false;
}

bool PluginASTOptionsBase::loadBool(const argmap_t &map,
                                    const char *key,
                                    bool &val) {
  std::string s_val;
  if (loadString(map, key, s_val)) {
    errno = 0;
    val = (bool)strtol(s_val.c_str(), nullptr, 10);
    if (errno) {
      std::cerr << "[!] Failed to read a bool from " << key << "\n";
    }
    return true;
  }
  return false;
}

bool PluginASTOptionsBase::loadInt(const argmap_t &map,
                                   const char *key,
                                   long &val) {
  std::string s_val;
  if (loadString(map, key, s_val)) {
    errno = 0;
    val = strtol(s_val.c_str(), nullptr, 10);
    if (errno) {
      std::cerr << "[!] Failed to read an int from " << key << "\n";
    }
    return true;
  }
  return false;
}

bool PluginASTOptionsBase::loadUnsignedInt(const argmap_t &map,
                                           const char *key,
                                           unsigned long &val) {
  std::string s_val;
  if (loadString(map, key, s_val)) {
    errno = 0;
    val = strtoul(s_val.c_str(), nullptr, 10);
    if (errno) {
      std::cerr << "[!] Failed to read an unsigned int from " << key << "\n";
    }
    return true;
  }
  return false;
}

void PluginASTOptionsBase::loadValuesFromEnvAndMap(const argmap_t map) {
  bool needBasePath = false;

  loadBool(map, "ALLOW_SIBLINGS_TO_REPO_ROOT", allowSiblingsToRepoRoot);
  loadBool(map, "KEEP_EXTERNAL_PATHS", keepExternalPaths);
  loadString(map, "MAKE_RELATIVE_TO", repoRoot);
  loadUnsignedInt(map, "MAX_STRING_SIZE", maxStringSize);

  // Possibly override the first argument given on the command line.
  loadString(map, "OUTPUT_FILE", outputFile);

  loadBool(map, "PREPEND_CURRENT_DIR", needBasePath);
  loadBool(map, "RESOLVE_SYMLINKS", resolveSymlinks);
  loadString(map, "STRIP_ISYSROOT", iSysRoot);

  if (needBasePath) {
    llvm::SmallString<1024> CurrentDir;
    if (llvm::sys::fs::current_path(CurrentDir)) {
      llvm::errs() << "Failed to retrieve current working directory\n";
    } else {
      basePath = CurrentDir.str().str();
    }
  }
}

void PluginASTOptionsBase::setObjectFile(const std::string &path) {
  objectFile = path;
  if (path != "" && outputFile.size() > 0 && outputFile[0] == '%') {
    outputFile = path + outputFile.substr(1);
  }
}

/**
 * Expects an immutable string on the heap as an argument
 * (e.g. a path extracted from a node in the AST)
 * Do not pass pointers to stack variables to this function.
 * (e.g. a .str() call on a StringRef)
 */
const std::string &PluginASTOptionsBase::normalizeSourcePath(
    const char *path) const {
  auto I = normalizationCache->find(path);
  if (I != normalizationCache->end()) {
    return I->second;
  }
  std::string &result = (*normalizationCache)[path];
  if (basePath == "") {
    result = path;
    return result;
  }
  std::string absPath = FileUtils::makeAbsolutePath(basePath, path);
  if (resolveSymlinks) {
    // if realPath is a symlink, resolve it
    char buf[2048];
    int len = readlink(absPath.c_str(), buf, sizeof(buf) - 1);
    if (len != -1) {
      buf[len] = '\0';
      absPath = buf;
    }
  }
  // By convention, relative paths are only activated when repoRoot != "".
  if (repoRoot != "") {
    result = FileUtils::makeRelativePath(repoRoot,
                                         iSysRoot,
                                         keepExternalPaths,
                                         allowSiblingsToRepoRoot,
                                         absPath);
  } else {
    result = absPath;
  }
  return result;
}

const std::string &PluginASTOptionsBase::normalizeSourcePath(
    llvm::StringRef path) const {
  return normalizeSourcePath(path.data());
}
} // namespace ASTPluginLib
