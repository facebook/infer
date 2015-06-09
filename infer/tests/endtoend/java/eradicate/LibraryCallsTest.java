/*
 * Copyright 2013-present Facebook, Inc.
 */

package endtoend.java.eradicate;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;


import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class LibraryCallsTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/eradicate/LibraryCalls.java";

  public static final String NULL_METHOD_CALL =
      "ERADICATE_NULL_METHOD_CALL";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadEradicateResults(LibraryCallsTest.class, SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "badReferenceDereference",
        "badWeakReferenceDereference",
        "badPhantomReferenceDereference",
        "badSoftReferenceDereference",
        "badAtomicReferenceDereference",
    };
    assertThat(
        "Results should contain " + NULL_METHOD_CALL,
        inferResults,
        containsExactly(
            NULL_METHOD_CALL,
            SOURCE_FILE,
            methods));
  }

}
