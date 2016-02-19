/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.cpp;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsExactly.containsExactly;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class CBugsTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/errors/c_tests/c_bugs.cpp";

  private static ImmutableList<String> inferCmd;

  public static final String MEMORY_LEAK = "MEMORY_LEAK";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createCPPInferCommand(folder, FILE);
  }

  @Test
  public void whenInferRunsOnMallocMemoryLeakThenMemoryLeakIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Using malloc without free should report memory leak",
        inferResults,
        contains(MEMORY_LEAK, FILE, "malloc_memory_leak_is_reported"));
  }

  @Test
  public void whenInferRunsOnMallocFreeWorksThenMemoryLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Using malloc with free should not report memory leak",
        inferResults,
        doesNotContain(MEMORY_LEAK, FILE, "malloc_free_works"));
  }

  @Test
  public void whenInferRunsOnMallocFailGetsReportedThenNullDereferenceIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Not checking malloc result should report null dereference",
        inferResults,
        contains(NULL_DEREFERENCE, FILE, "malloc_fail_gets_reported"));
  }


  @Test
  public void whenInferRunsOnResourceLeakIsReportedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain resource leak",
        inferResults,
        contains(RESOURCE_LEAK, FILE, "resource_leak_is_reported"));
  }

  @Test
  public void whenInferRunsFopenFcloseWorksThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should not contain resource leak",
        inferResults,
        doesNotContain(RESOURCE_LEAK, FILE, "fopen_fclose_works"));
  }

  @Test
  public void whenInferRunsMemcpySpecIsFoundThenDivBy0IsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "when spec for memcpy is found, analysis should fail before div by 0",
        inferResults,
        doesNotContain(DIVIDE_BY_ZERO, FILE, "memcpy_spec_is_found"));
  }

}
