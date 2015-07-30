/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ActivityLeaksTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/ActivityLeaks.java";

  public static final String ACTIVITY_LEAK = "ACTIVITY_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(
        ActivityLeaksTest.class,
        SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
        "directLeak",
        "indirectLeak",
        "nonStaticInnerClassLeak",
        "leakAfterInstanceFieldWrite",
        "singletonLeak"
    };
    assertThat(
        "Results should contain " + ACTIVITY_LEAK,
        inferResults,
        containsExactly(
            ACTIVITY_LEAK,
            SOURCE_FILE,
            methods
        )
    );
  }

}
