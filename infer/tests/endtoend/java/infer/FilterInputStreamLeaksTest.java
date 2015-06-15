/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/
//Class to tests resource leaks on the class FilterInputStream and its subclasses
package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class FilterInputStreamLeaksTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/FilterInputStreamLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(
        FilterInputStreamLeaksTest.class,
        SOURCE_FILE);
  }

  @Test
  public void test()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
        "bufferedInputStreamNotClosedAfterRead",
        "checkedInputStreamNotClosedAfterRead",
        "cipherInputStreamNotClosedAfterSkip",
        "dataInputStreamNotClosedAfterRead",
        "deflaterInputStreamNotClosedAfterRead",
        "gzipInputStreamNotClosedAfterRead",
        "gzipInputStreamNotClosedAfterRead",
        "digestInputStreamNotClosedAfterRead",
        "inflaterInputStreamNotClosedAfterRead",
        "pushbackInputStreamNotClosedAfterRead",
    };
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        containsExactly(
            RESOURCE_LEAK,
            SOURCE_FILE,
            methods
        )
    );
  }

}
