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


public class NestedOperatorsTest {

  String nestedOperatorsBasePath = "infer/tests/codetoanalyze/c/frontend/nestedoperators/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCDotFiles(folder, nestedOperatorsBasePath + fileRelative);
  }


  @Test
  public void whenCaptureRunEnumThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("nestedassignment.c");
  }

  @Test
  public void whenCaptureRunUnionThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("union.c");
  }

  @Test
  public void whenCaptureRunAssignInConditionThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("assign_in_condition.c");
  }

  @Test
  public void whenCaptureRunAssignWithIncrementThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("assign_with_increment.c");
  }
}
