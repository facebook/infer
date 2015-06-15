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

public class CursorLeaksTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/CursorLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws IOException {
    inferResults = InferResults.loadInferResults(CursorLeaksTest.class, SOURCE_FILE);
  }

  @Test
  public void test()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
        "cursorNotClosed",
        "getImageCountHelperNotClosed",
        "getBucketCountNotClosed",
        "queryUVMLegacyDbNotClosed",
        "completeDownloadNotClosed",
        "loadPrefsFromContentProviderNotClosed",
    };
    assertThat(
        "Results should not contain resource leak errors",
        inferResults,
        containsExactly(
            RESOURCE_LEAK,
            SOURCE_FILE,
            methods
        )
    );
  }

}
