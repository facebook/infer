/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package endtoend.java.eradicate;


import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class NullFieldAccessTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/eradicate/NullFieldAccess.java";

  public static final String NULL_FIELD_ACCESS =
      "ERADICATE_NULL_FIELD_ACCESS";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadEradicateResults(NullFieldAccessTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "useX",
        "useZ",
        "useInterface",
        "arrayLength",
        "arrayAccess",
    };
    assertThat(
        "Results should contain " + NULL_FIELD_ACCESS,
        inferResults,
        containsExactly(
            NULL_FIELD_ACCESS,
            SOURCE_FILE,
            methods));
  }

}
