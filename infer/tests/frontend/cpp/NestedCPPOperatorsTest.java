/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.cpp;

import org.junit.Rule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.ClangFrontendUtils;

public class NestedCPPOperatorsTest {

  String nestedOperatorsBasePath = "infer/tests/codetoanalyze/cpp/frontend/nestedoperators/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCppDotFiles(folder, nestedOperatorsBasePath + fileRelative);
  }


  @Test
  public void testVarDeclInsideIfDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("var_decl_inside_if.cpp");
  }

  @Test
  public void testVarDeclInsideWhileDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("var_decl_inside_while.cpp");
  }

  @Test
  public void testVarDeclInsideForDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("var_decl_inside_for.cpp");
  }

  @Test
  public void testVarDeclInsideSwitchDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("var_decl_inside_switch.cpp");
  }
}
