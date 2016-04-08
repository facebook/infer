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

public class ConstructorsTest {

  String basePath = "infer/tests/codetoanalyze/cpp/frontend/constructors/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCppDotFiles(folder, basePath + fileRelative);
  }

  @Test
  public void testConstructorWithBodyDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("constructor_with_body.cpp");
  }

  @Test
  public void testConstructorDefaultArgDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("constructor_default_arg.cpp");
  }

  @Test
  public void testTempObjectDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("temp_object.cpp");
  }

  @Test
  public void testConstructorInitDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("constructor_init.cpp");
  }

  @Test
  public void testCopyMoveConstructorDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("copy_move_constructor.cpp");
  }

  @Test
  public void testDefaultFieldInitDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("default_field_init.cpp");
  }

  @Test
  public void testConstructorInitListDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("constructor_struct_init_list.cpp");
  }

  @Test
  public void testStdInitListDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("std_init_list.cpp");
  }

  @Test
  public void testConstructorNewFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("constructor_new.cpp");
  }

  @Test
  public void testConstructorArrayFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("constructor_array.cpp");
  }
}
