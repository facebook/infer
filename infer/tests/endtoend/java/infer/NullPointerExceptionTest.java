/*
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class NullPointerExceptionTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/NullPointerExceptions.java";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(
        NullPointerExceptionTest.class,
        SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "nullPointerException",
        "nullPointerExceptionInterProc",
        "nullPointerExceptionWithExceptionHandling",
        "nullPointerExceptionWithArray",
        "nullPointerExceptionWithAChainOfFields",
        "nullPointerExceptionWithNullObjectParameter",
        "nullPointerExceptionWithNullArrayParameter",
        "nullPointerExceptionFromFaillingResourceConstructor",
        "nullPointerExceptionFromFailingFileOutputStreamConstructor",
        "nullPointerExceptionUnlessFrameFails",
        "hashmapNPE",
        "NPEvalueOfFromHashmapBad",
        "cursorFromContentResolverNPE",
        "nullPointerExceptionInArrayLengthLoop",
        "nullPointerExceptionCallArrayReadMethod",
        "nullableFieldNPE",
        "nullableParamNPE",
        "badCheckShouldCauseNPE",
        "nullPointerExceptionArrayLength",
        "npeWithDollars"
    };
    assertThat(
        "Results should contain " + NULL_DEREFERENCE,
        inferResults,
        containsExactly(
            NULL_DEREFERENCE,
            SOURCE_FILE,
            methods
        )
    );
  }

}
