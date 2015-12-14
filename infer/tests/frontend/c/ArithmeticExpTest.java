/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.c;

import org.junit.Rule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.ClangFrontendUtils;

public class ArithmeticExpTest {

  String arithmeticBasePath = "infer/tests/codetoanalyze/c/frontend/arithmetic/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCDotFiles(folder, arithmeticBasePath + fileRelative);
  }

  @Test
  public void whenCaptureRunOnPlus_exprThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("plus_expr.c");
  }

  @Test
  public void whenCaptureRunOnCompound_assignmentThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("compound_assignment.c");
  }

  @Test
  public void whenCaptureRunOnUnaryThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("unary.c");
  }

  @Test
  public void whenCaptureRunOnIntConstThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("int_const.c");
  }

}
