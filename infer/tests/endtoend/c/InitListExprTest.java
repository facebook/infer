/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package endtoend.c;

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

public class InitListExprTest {

  public static final String initlistexpr_file =
      "infer/tests/" +
          "codetoanalyze/c/errors/initialization/initlistexpr.c";

  private static ImmutableList<String> inferCmd;

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createCInferCommand(folder, initlistexpr_file);
  }

  @Test
  public void whenInferRunsOnInitListExprThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferC(inferCmd);
    assertThat(
        "Results should contain divide by zero error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            initlistexpr_file,
            "divide_by_zero"
        )
    );
  }

}
