/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.c;

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.ClangFrontendUtils;

public class ConditionalOperatorTest {

  String conditionalOperatorBasePath = "infer/tests/codetoanalyze/c/frontend/conditional_operator/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCDotFiles(folder, conditionalOperatorBasePath + fileRelative);
  }

  @Test
  public void whenCaptureRunCommaThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("conditional_operator.c");
  }

  @Test
  public void whenCaptureRunShortCThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("if_short_circuit.c");
  }

  @Test
  public void whenCaptureRunCond2ThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("cond2.c");
  }

  @Ignore @Test
  public void whenCaptureRunOnAssertExampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("assert_example.c");
  }

  @Test
  public void whenCaptureRunOnIntNegationThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("int_negation.c");
  }

  @Test
  public void whenCaptureRunBinaryOperatorThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("binary_operator.c");
  }

  @Test
  public void whenCaptureRunUnaryOperatorThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("unary_operator.c");
  }

  @Test
  public void whenCaptureRunArrayAccessThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("array_access.c");
  }

  @Test
  public void whenCaptureRunMemberAccessThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("member_access.c");
  }

  @Test
  public void whenCaptureRunPreincrementThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("preincrement.c");
  }

  @Test
  public void whenCaptureRunFunctionCallThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("function_call.c");
  }

}
