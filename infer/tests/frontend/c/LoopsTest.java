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

public class LoopsTest {

  String loopsBasePath = "infer/tests/codetoanalyze/c/frontend/loops/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCDotFiles(folder, loopsBasePath + fileRelative);
  }


  @Test
  public void whenCaptureRunOnWhileThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("while.c");
  }

  @Test
  public void whenCaptureRunOnWhile_nestedThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("while_nested.c");
  }

  @Test
  public void whenCaptureRunOnWhile_side_effectsThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("while_condition_side_effects.c");
  }

  @Test
  public void whenCaptureRunOnWhile_no_bodyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("while_no_body.c");
  }

  @Test
  public void whenCaptureRunOnForside_effectsThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("for_condition_side_effects.c");
  }

  @Test
  public void whenCaptureRunOnFor_nestedThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("for_nested.c");
  }

  @Test
  public void whenCaptureRunOnFor_no_conditionThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("for_no_condition.c");
  }

  @Test
  public void whenCaptureRunOnFor_no_conditionincrThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("for_no_condition_incr.c");
  }

  @Test
  public void whenCaptureRunOnFor_emptyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("for_no_condition_incr_body.c");
  }

  @Test
  public void whenCaptureRunOnFor_only_bodyThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("for_only_body.c");
  }

  @Test
  public void whenCaptureRunOnFor_simpleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("for_simple.c");
  }

  @Test
  public void whenCaptureRunOnFor_while_nestedThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("for_while_nested.c");
  }

  @Test
  public void whenCaptureRunOnDo_whileThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("do_while.c");
  }

  @Test
  public void whenCaptureRunOnDo_while_side_effectsThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("do_while_condition_side_effects.c");
  }

  @Test
  public void whenCaptureRunOnDo_while_nestedThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("do_while_nested.c");
  }

  @Test
  public void whenCaptureWhile_with_continue_and_breakThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("while_with_continue_and_break.c");
  }
}
