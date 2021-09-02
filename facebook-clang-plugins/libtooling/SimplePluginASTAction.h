/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <functional>
#include <memory>
#include <stdlib.h>
#include <string>
#include <unordered_map>

#include <clang/AST/ASTConsumer.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Tooling/Tooling.h>

#include "FileUtils.h"

namespace ASTPluginLib {

struct PluginASTOptionsBase {
  // source file being parsed
  clang::FrontendInputFile inputFile;
  // output file for the plugin
  std::string outputFile;
  // object file produced by the usual frontend (possibly empty)
  std::string objectFile;

  /* Will contain the current directory if PREPEND_CURRENT_DIR was specified.
   * The intention is to make file paths in the AST absolute if needed.
   */
  std::string basePath;

  /* Configure a second pass on file paths to make them relative to the repo
   * root. */
  std::string repoRoot;
  /* Configure a third pass on (absolute) file paths to blank the system root:
   *    /path/to/sysroot/usr/lib/foo.h --> /usr/lib/foo.h
   */
  std::string iSysRoot;
  /* Configure a fourth pass on (absolute) file paths to detect siblings to
   * the repo root. If the repo root is /some/path, /some/other_path will be
   * rewritten ../other_path
   */
  bool allowSiblingsToRepoRoot = false;
  /* Whether file paths that could not be normalized by any of the rules above
   * should be kept or blanked.
   */
  bool keepExternalPaths = false;
  /* Resolve symlinks to their real path. */
  bool resolveSymlinks = false;
  /* do not emit string literals larger than this size */
  unsigned long maxStringSize = 65535;

  typedef std::unordered_map<std::string, std::string> argmap_t;

  static argmap_t makeMap(const std::vector<std::string> &args);

 private:
  /* cache for normalizeSourcePath */
  std::unique_ptr<std::unordered_map<const char *, std::string>>
      normalizationCache;

 protected:
  static const std::string envPrefix;

  static bool loadString(const argmap_t &map,
                         const char *key,
                         std::string &val);

  static bool loadBool(const argmap_t &map, const char *key, bool &val);

  static bool loadInt(const argmap_t &map, const char *key, long &val);

  static bool loadUnsignedInt(const argmap_t &map,
                              const char *key,
                              unsigned long &val);

 public:
  PluginASTOptionsBase() {
    normalizationCache.reset(
        new std::unordered_map<const char *, std::string>());
  };

  void loadValuesFromEnvAndMap(const argmap_t map);

  // This should be called after outputFile has been set, so as to finalize
  // the output file in case a pattern "%.bla" was given.
  void setObjectFile(const std::string &path);

  const std::string &normalizeSourcePath(const char *path) const;
  const std::string &normalizeSourcePath(llvm::StringRef path) const;
};

struct EmptyPreprocessorHandlerData {};

struct EmptyPreprocessorHandler : public clang::PPCallbacks {
  EmptyPreprocessorHandler(
      clang::SourceManager &SM,
      std::shared_ptr<PluginASTOptionsBase> options,
      std::shared_ptr<EmptyPreprocessorHandlerData> sharedData) {}
};

template <class PluginASTOptions = PluginASTOptionsBase,
          class PreprocessorHandler = EmptyPreprocessorHandler,
          class PreprocessorHandlerData = EmptyPreprocessorHandlerData>
class SimplePluginASTActionBase : public clang::PluginASTAction {
 protected:
  std::shared_ptr<PluginASTOptions> options;
  std::shared_ptr<PreprocessorHandlerData> sharedData;

  void ExecuteAction() override {
    auto &preprocessor = getCompilerInstance().getPreprocessor();
    preprocessor.addPPCallbacks(std::make_unique<PreprocessorHandler>(
        preprocessor.getSourceManager(), options, sharedData));
    clang::PluginASTAction::ExecuteAction();
  }

  // Called when FrontendPluginRegistry is used.
  bool ParseArgs(const clang::CompilerInstance &CI,
                 const std::vector<std::string> &args_) override {
    std::vector<std::string> args = args_;
    if (args.size() > 0) {
      options->outputFile = args[0];
      args.erase(args.begin());
    }
    options->loadValuesFromEnvAndMap(PluginASTOptions::makeMap(args));
    return true;
  }

  SimplePluginASTActionBase() {
    // These data structures will be shared between PreprocessorHandler
    // and ASTConsumer (the relative lifetimes of which are unknown).
    // During the AST traversal, it is expected that `options` is only read
    // and `sharedData` is only written.
    options = std::make_shared<PluginASTOptions>();
    sharedData = std::make_shared<PreprocessorHandlerData>();
  }

  // Alternate constructor to pass an optional sequence "KEY=VALUE,.."
  // expected to be use with SimpleFrontendActionFactory below.
  explicit SimplePluginASTActionBase(const std::vector<std::string> &args)
      : SimplePluginASTActionBase() {
    options->loadValuesFromEnvAndMap(PluginASTOptions::makeMap(args));
  }

  bool SetFileOptions(clang::CompilerInstance &CI,
                      llvm::StringRef inputFilename) {
    // When running clang tool on more than one source file, CreateASTConsumer
    // will be ran for each of them separately. Hence, Inputs.size() = 1.
    clang::FrontendInputFile inputFile = CI.getFrontendOpts().Inputs[0];

    switch (inputFile.getKind().getLanguage()) {
    case clang::Language::Unknown:
    case clang::Language::Asm:
    case clang::Language::LLVM_IR:
      // We can't do anything with these - they may trigger errors when
      // running clang frontend
      return false;
    default:
      // run the consumer for IK_AST and all others
      break;
    }
    options->inputFile = inputFile;
    options->setObjectFile(CI.getFrontendOpts().OutputFile);
    // success
    return true;
  }
};

template <class SimpleASTAction>
class SimpleFrontendActionFactory
    : public clang::tooling::FrontendActionFactory {
  std::vector<std::string> args_;

 public:
  explicit SimpleFrontendActionFactory(std::vector<std::string> args)
      : args_(args) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return new SimpleASTAction(args_);
  }
};

template <class ASTConsumer,
          bool Binary = 0,
          bool RemoveFileOnSignal = 1,
          bool UseTemporary = 1,
          bool CreateMissingDirectories = 0>
class SimplePluginASTAction
    : public SimplePluginASTActionBase<
          typename ASTConsumer::ASTConsumerOptions,
          typename ASTConsumer::PreprocessorHandler,
          typename ASTConsumer::PreprocessorHandlerData> {
  using Parent =
      SimplePluginASTActionBase<typename ASTConsumer::ASTConsumerOptions,
                                typename ASTConsumer::PreprocessorHandler,
                                typename ASTConsumer::PreprocessorHandlerData>;

 public:
  SimplePluginASTAction() {}

  explicit SimplePluginASTAction(const std::vector<std::string> &args)
      : Parent(args) {}

 protected:
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance &CI, llvm::StringRef inputFilename) {
    if (!Parent::SetFileOptions(CI, inputFilename)) {
      return nullptr;
    }
    std::unique_ptr<llvm::raw_ostream> OS =
        CI.createOutputFile(Parent::options->outputFile,
                            Binary,
                            RemoveFileOnSignal,
                            UseTemporary,
                            CreateMissingDirectories);
    if (!OS) {
      return nullptr;
    }

    return std::unique_ptr<clang::ASTConsumer>(new ASTConsumer(
        CI, Parent::options, Parent::sharedData, std::move(OS)));
  }
};

template <class ASTConsumer>
class NoOpenSimplePluginASTAction
    : public SimplePluginASTActionBase<
          typename ASTConsumer::ASTConsumerOptions,
          typename ASTConsumer::PreprocessorHandler,
          typename ASTConsumer::PreprocessorHandlerData> {
  using Parent =
      SimplePluginASTActionBase<typename ASTConsumer::ASTConsumerOptions,
                                typename ASTConsumer::PreprocessorHandler,
                                typename ASTConsumer::PreprocessorHandlerData>;

 public:
  NoOpenSimplePluginASTAction() {}

  explicit NoOpenSimplePluginASTAction(const std::vector<std::string> &args)
      : Parent(args) {}

 protected:
  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance &CI, llvm::StringRef inputFilename) {
    if (!Parent::SetFileOptions(CI, inputFilename)) {
      return nullptr;
    }
    std::unique_ptr<std::string> outputFile = std::unique_ptr<std::string>(
        new std::string(Parent::options->outputFile));
    return std::unique_ptr<clang::ASTConsumer>(new ASTConsumer(
        CI, Parent::options, Parent::sharedData, std::move(outputFile)));
  }
};
} // namespace ASTPluginLib
