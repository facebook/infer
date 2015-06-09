/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package endtoend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class BlockTest {

  public static final String MAIN_FILE =
      "infer/tests/" +
          "codetoanalyze/objc/frontend/block/main.m";

  private static ImmutableList<String> inferCmd;

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommand(folder, MAIN_FILE);
  }

  @Test
  public void whenInferRunsOnEOCPersonThenMemoryLeakIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    assertThat(
        "Results should contain divide by zero error. " +
            "This shows that the translation is correct and res = 8.",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            MAIN_FILE,
            "main"
        )
    );
  }
}
