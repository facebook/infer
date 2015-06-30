/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package endtoend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class NSMakeCollectableTest {

  public static final String memory_leak_file =
      "infer/tests/codetoanalyze/objc/errors/memory_leaks_benchmark/NSMakeCollectableExample.m";

  private static ImmutableList<String> inferCmd;

  public static final String MEMORY_LEAK = "MEMORY_LEAK";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommandWithMLBuckets(
        folder,
        memory_leak_file,
        "cf",
        false);
  }

  @Test
  public void whenInferRunsOnTestWithBucketingThenMLIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    String[] procedures = {
    };
    assertThat(
        "Results should not contain memory leak",
        inferResults,
        containsExactly(
            MEMORY_LEAK,
            memory_leak_file,
            procedures));
  }


}
