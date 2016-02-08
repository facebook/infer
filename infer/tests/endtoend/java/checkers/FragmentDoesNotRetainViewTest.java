/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.checkers;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class FragmentDoesNotRetainViewTest {

  public static final String SOURCE_FILE =
    "infer/tests/codetoanalyze/java/checkers/FragmentDoesNotRetainViewExample.java";

  public static final String FRAGMENT_RETAINS_VIEW =
    "CHECKERS_FRAGMENT_RETAINS_VIEW";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults =
      InferResults.loadCheckersResults(FragmentDoesNotRetainViewTest.class, SOURCE_FILE);
  }

  @Test
  public void matchNumberOfErrors()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "Results should contain 0 retained View errors",
        inferResults,
        doesNotContain(
            FRAGMENT_RETAINS_VIEW,
            SOURCE_FILE,
            "onDestroyView"
        )
    );
  }

}
