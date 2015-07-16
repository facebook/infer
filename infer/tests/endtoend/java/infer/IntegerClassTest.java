/*
* Copyright (c) 2015-present Facebook.
* All rights reserved.
*/

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class IntegerClassTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/IntegerExample.java";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws IOException {
    inferResults = InferResults.loadInferResults(IntegerClassTest.class, SOURCE_FILE);
  }

  @Test
  public void test()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain NPE errors",
        inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            "testIntegerEquals"
        )
    );
  }

}
