/**
 * Copyright (c) 2014, Facebook, Inc.
 * Copyright (c) 2003-2014 University of Illinois at Urbana-Champaign.
 * All rights reserved.
 *
 * This file is distributed under the University of Illinois Open Source
 * License.
 * See LLVM-LICENSE for details.
 *
 */

/**
 * Clang frontend plugin to export an AST of clang into Json, Yojson and Biniou
 * while conforming to the inlined ATD specifications.
 */

#include "ASTExporter.h"

//===----------------------------------------------------------------------===//
// ASTExporter Plugin
//===----------------------------------------------------------------------===//

static ASTLib::FrontendPluginRegistry::Add<ASTLib::JsonExporterASTAction> X(
    "JsonASTExporter",
    "Export the AST of source files into ATD-specified Json data");

static ASTLib::FrontendPluginRegistry::Add<ASTLib::YojsonExporterASTAction> Y(
    "YojsonASTExporter",
    "Export the AST of source files into ATD-specified Yojson data");

static ASTLib::FrontendPluginRegistry::Add<ASTLib::BiniouExporterASTAction> Z(
    "BiniouASTExporter",
    "Export the AST of source files into ATD-specified biniou data");
