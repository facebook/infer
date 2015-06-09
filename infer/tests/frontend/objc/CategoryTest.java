/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package frontend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.DotFilesEqual.dotFileEqualTo;

import com.google.common.collect.ImmutableList;

import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferRunner;

public class CategoryTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnmainThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cat_expr =
        "infer/tests/codetoanalyze/" +
            "objc/errors/category_procdesc/main.c";

    String cat_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/errors/category_procdesc/main.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, cat_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cat_expr +
            " the dotty files should be the same. " +
            "In each procedure the translation of the boxing " +
            "syntactic sugar is the same " +
            "as the tranlation of the underlying method call.",
        newDotFile, dotFileEqualTo(cat_dotty));
  }

  @Test
  public void whenCaptureRunOnEOCPersonThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String cat_expr =
        "infer/tests/codetoanalyze/" +
            "objc/errors/category_procdesc/EOCPerson.m";

    String cat_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/errors/category_procdesc/EOCPerson.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createObjCInferCommandFrontend(folder, cat_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + cat_expr +
            " the dotty files should be the same. " +
            "In each procedure the translation of the boxing " +
            "syntactic sugar is the same " +
            "as the tranlation of the underlying method call.",
        newDotFile, dotFileEqualTo(cat_dotty));
  }
}
