/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ResourceLeakTest {

  public static final String SOURCE_FILE =
      "resource_leaks/leak.c";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(ResourceLeakTest.class, SOURCE_FILE);
  }

  @Test
  public void whenInferRunsOnFileNotClosedThenLeakFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak",
        inferResults,
        contains(
            RESOURCE_LEAK,
            SOURCE_FILE,
            "fileNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnFileClosedThenLeakNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            SOURCE_FILE,
            "fileClosed"));
  }

}
