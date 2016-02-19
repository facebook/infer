/*
 * Copyright (c) 2004 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/raw_ostream.h"
using namespace clang;

namespace {

class PrintFunctionsConsumer : public ASTConsumer {
 public:
  virtual bool HandleTopLevelDecl(DeclGroupRef DG) {
    for (DeclGroupRef::iterator i = DG.begin(), e = DG.end(); i != e; ++i) {
      const Decl* D = *i;
      if (const NamedDecl* ND = dyn_cast<NamedDecl>(D))
        llvm::errs() << "top-level-decl: \"" << ND->getNameAsString() << "\"\n";
    }

    return true;
  }
};

class PrintFunctionNamesAction : public PluginASTAction {
 protected:
  ASTConsumer* CreateASTConsumer(CompilerInstance& CI, llvm::StringRef) {
    return new PrintFunctionsConsumer();
  }

  bool ParseArgs(const CompilerInstance& CI,
                 const std::vector<std::string>& args) {
    for (unsigned i = 0, e = args.size(); i != e; ++i) {
      llvm::errs() << "toplevel-plugin arg = " << args[i] << "\n";

      // Example error handling.
      if (args[i] == "-an-error") {
        DiagnosticsEngine& D = CI.getDiagnostics();
        unsigned DiagID = D.getCustomDiagID(
            DiagnosticsEngine::Error, "invalid argument '" + args[i] + "'");
        D.Report(DiagID);
        return false;
      }
    }
    if (args.size() && args[0] == "help")
      PrintHelp(llvm::errs());

    return true;
  }
  void PrintHelp(llvm::raw_ostream& ros) {
    ros << "Help for toplevel-plugin plugin goes here\n";
  }
};
}

static FrontendPluginRegistry::Add<PrintFunctionNamesAction> X(
    "infer-plugin", "print function names");
