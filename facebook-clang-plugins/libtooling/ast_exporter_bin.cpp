/*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * Clang tool to export an AST of clang into Json, Yojson, and Biniou
 * while conforming to the inlined ATD specifications.
 */

#include "ASTExporter.h"

#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>

//===----------------------------------------------------------------------===//
// ASTExporter Tool
//===----------------------------------------------------------------------===//

static llvm::cl::OptionCategory astExporterCategory("ast_exporter_bin");

enum Mode { json, yojson, biniou };

static llvm::cl::opt<Mode> astExporterMode(
    "ast-exporter-mode",
    llvm::cl::desc("Mode of operation"),
    llvm::cl::values(clEnumVal(json, "json output"),
                     clEnumVal(yojson, "yojson output"),
                     clEnumVal(biniou, "biniou output")),
    llvm::cl::cat(astExporterCategory));

static llvm::cl::opt<std::string> astExporterOutput(
    "ast-exporter-output",
    llvm::cl::desc("output file"),
    llvm::cl::cat(astExporterCategory));

// TODO: Unpack the other ASTExporterOptions into native command line options.
static llvm::cl::list<std::string> astExporterOptions(
    "ast-exporter-option",
    llvm::cl::desc("One or several comma-separated pairs [KEY]=[VALUE]. "
                   "This will override the corresponding environment variables "
                   "CLANG_FRONTEND_PLUGIN__[KEY]."),
    llvm::cl::CommaSeparated,
    llvm::cl::cat(astExporterCategory));

static llvm::cl::extrahelp commonHelp(
    clang::tooling::CommonOptionsParser::HelpMessage);

int main(int argc, const char **argv) {
  clang::tooling::CommonOptionsParser optionsParser(
      argc, argv, astExporterCategory);
  clang::tooling::ClangTool tool(optionsParser.getCompilations(),
                                 optionsParser.getSourcePathList());

  if (!astExporterOutput.empty()) {
    astExporterOptions.push_back("OUTPUT_FILE=" + astExporterOutput);
  }

  std::unique_ptr<clang::tooling::ToolAction> factory = nullptr;
  switch (astExporterMode) {
  case json:
    factory.reset(new ASTPluginLib::SimpleFrontendActionFactory<
                  ASTLib::JsonExporterASTAction>(astExporterOptions));
    break;
  case yojson:
    factory.reset(new ASTPluginLib::SimpleFrontendActionFactory<
                  ASTLib::YojsonExporterASTAction>(astExporterOptions));
    break;
  case biniou:
    factory.reset(new ASTPluginLib::SimpleFrontendActionFactory<
                  ASTLib::BiniouExporterASTAction>(astExporterOptions));
    break;
  }

  return tool.run(factory.get());
}
