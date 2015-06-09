// Copyright (c) 2015-Present Facebook. All rights reserved.

package endtoend.java.tracing;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ArrayIndexOutOfBoundsExceptionTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/tracing/ArrayIndexOutOfBoundsExceptionExample.java";

  public static final String ARRAY_OUT_OF_BOUND =
      "java.lang.ArrayIndexOutOfBoundsException";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadTracingResults(
        ArrayIndexOutOfBoundsExceptionTest.class,
        SOURCE_FILE);
  }

  @Test
  public void whenEradicateRunsOnConstructorThenFieldNotInitializedIsFound()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "Results should contain " + ARRAY_OUT_OF_BOUND,
        inferResults,
        contains(
            ARRAY_OUT_OF_BOUND,
            SOURCE_FILE,
            "callOutOfBound"
        )
    );
  }


}
