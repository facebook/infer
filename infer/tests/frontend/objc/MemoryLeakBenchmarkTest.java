/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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


public class MemoryLeakBenchmarkTest {

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();


  @Test
  public void whenCaptureRunOnMemoryLeakExampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String ml_expr =
        "infer/tests/codetoanalyze/" +
            "objc/errors/memory_leaks_benchmark/MemoryLeakExample.m";

    String ml_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/errors/memory_leaks_benchmark/MemoryLeakExample.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, ml_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + ml_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(ml_dotty));
  }

  @Test
  public void whenCaptureRunOnRetainReleaseExampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String ml_expr =
        "infer/tests/codetoanalyze/" +
            "objc/errors/memory_leaks_benchmark/RetainReleaseExample.m";

    String ml_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/errors/memory_leaks_benchmark/RetainReleaseExample.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, ml_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + ml_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(ml_dotty));
  }

    @Test
    public void whenCaptureRunOnRetainReleaseExample2ThenDotFilesAreTheSame()
    throws InterruptedException, IOException, InferException {
        String ml_expr =
        "infer/tests/codetoanalyze/" +
        "objc/errors/memory_leaks_benchmark/RetainReleaseExample2.m";

        String ml_dotty =
        "infer/tests/codetoanalyze/" +
        "objc/errors/memory_leaks_benchmark/RetainReleaseExample2.dot";

        ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, ml_expr);
        File newDotFile = InferRunner.runInferFrontend(inferCmd);
        assertThat(
                   "In the capture of " + ml_expr +
                   " the dotty files should be the same.",
                   newDotFile, dotFileEqualTo(ml_dotty));
    }


  @Test
  public void whenCaptureRunOnAutoreleaseExampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String ml_expr =
        "infer/tests/codetoanalyze/" +
            "objc/errors/memory_leaks_benchmark/AutoreleaseExample.m";

    String ml_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/errors/memory_leaks_benchmark/AutoreleaseExample.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, ml_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + ml_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(ml_dotty));
  }

  @Test
  public void whenCaptureRunOnTollBridgeExampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    String ml_expr =
        "infer/tests/codetoanalyze/" +
            "objc/errors/memory_leaks_benchmark/TollBridgeExample.m";

    String ml_dotty =
        "infer/tests/codetoanalyze/" +
            "objc/errors/memory_leaks_benchmark/TollBridgeExample.dot";

    ImmutableList<String> inferCmd =
        InferRunner.createiOSInferCommandFrontend(folder, ml_expr);
    File newDotFile = InferRunner.runInferFrontend(inferCmd);
    assertThat(
        "In the capture of " + ml_expr +
            " the dotty files should be the same.",
        newDotFile, dotFileEqualTo(ml_dotty));
  }
}
