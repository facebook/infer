// Copyright (c) 2015-Present Facebook. All rights reserved.

package endtoend.java.tracing;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ReportOnMainTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/tracing/ReportOnMainExample.java";

  public static final String NPE =
      "java.lang.NullPointerException";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadTracingResults(
        ReportOnMainTest.class,
        SOURCE_FILE);
  }

  @Test
  public void whenEradicateRunsOnConstructorThenFieldNotInitializedIsFound()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "main"
    };
    assertThat(
        "Results should contain " + NPE,
        inferResults,
        containsExactly(
            NPE,
            SOURCE_FILE,
            methods
        )
    );
  }


}
