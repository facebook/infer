/*
 * Copyright (c) 2015 - present Facebook, Inc.
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

public class ReferenceTest {

  String basePath = "infer/tests/codetoanalyze/cpp/frontend/reference/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCppDotFiles(folder, basePath + fileRelative);
  }

  @Test
  public void testBoxDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("box.cpp");
  }

  @Test
  public void testUnboxDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("unbox.cpp");
  }

  @Test
  public void testInitDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("init.cpp");
  }

  @Test
  public void testMemberAccessDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("member_access.cpp");
  }

  @Test
  public void testMemberAccessFromReturnDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("member_access_from_return.cpp");
  }

  @Test
  public void testNestedAssignmentDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("nested_assignment.cpp");
  }

  @Test
  public void testIncrementDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("increment.cpp");
  }

  @Test
  public void testTemporaryLValueDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("temporary_lvalue.cpp");
  }

  @Test
  public void testReferenceTypeE2EDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("reference_type_e2e.cpp");
  }

  @Test
  public void testReferenceStructE2EDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("reference_struct_e2e.cpp");
  }
}
