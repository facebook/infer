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

public class FieldNotNullableTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/eradicate/FieldNotNullable.java";

  public static final String FIELD_NOT_NULLABLE =
      "ERADICATE_FIELD_NOT_NULLABLE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadEradicateResults(FieldNotNullableTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "setYNull",
        "setYNullable",
        "<init>",
        "FlatBAD1",
        "FlatBAD2",
        "NestedBAD1",
        "NestedBAD2",
        "NestedBAD3",
    };
    assertThat(
        "Results should contain " + FIELD_NOT_NULLABLE,
        inferResults,
        containsExactly(
            FIELD_NOT_NULLABLE,
            SOURCE_FILE,
            methods));
  }

}
