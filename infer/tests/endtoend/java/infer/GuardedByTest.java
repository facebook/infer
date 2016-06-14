/*
 * Copyright (c) 2016 - present Facebook, Inc.
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

public class GuardedByTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/GuardedByExample.java";

  public static final String UNSAFE_GUARDED_BY_ACCESS = "UNSAFE_GUARDED_BY_ACCESS";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(
        GuardedByTest.class,
        SOURCE_FILE);
  }

  @Test
  public void matchErrors()
      throws IOException, InterruptedException, InferException {
    String[] methods = {
      "readFBad",
      "writeFBad",
      "readFBadWrongLock",
      "writeFBadWrongLock",
      "readFAfterBlockBad",
      "writeFAfterBlockBad",
      "readFBadWrongAnnotation",
      "synchronizedMethodReadBad",
      "synchronizedMethodWriteBad",
      "readGFromCopyBad",
      "readHBad",
      "readHBadSynchronizedMethodShouldntHelp",
      // TODO: report these
      // "unguardedCallSiteBad1",
      // "unguardedCallSiteBad2",
      // "unguardedCallSiteBad3",
    };
    assertThat(
        "Results should contain " + UNSAFE_GUARDED_BY_ACCESS,
        inferResults,
        containsExactly(
            UNSAFE_GUARDED_BY_ACCESS,
            SOURCE_FILE,
            methods
        )
    );
  }

}
