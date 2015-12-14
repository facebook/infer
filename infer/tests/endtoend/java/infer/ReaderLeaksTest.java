/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

//Class to tests resource leaks on the class FilterOutputStream and its subclasses

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Path;

import utils.InferException;
import utils.InferResults;

public class ReaderLeaksTest {

  Path t;

  public static final String ReaderLeaks =
      "infer/tests/codetoanalyze/java/infer/ReaderLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(ReaderLeaksTest.class, ReaderLeaks);
  }

  @Test
  public void test()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
        "readerNotClosedAfterRead",
        "bufferedReaderNotClosedAfterRead",
        "inputStreamReaderNotClosedAfterRead",
        "fileReaderNotClosedAfterRead",
        "pushbackReaderNotClosedAfterRead",
        "pipedReaderNotClosedAfterConstructedWithWriter",
        "pipedReaderNotClosedAfterConnect",
        "pipedReaderFalsePositive",
    };
    assertThat(
        "Results should contain the following resource leak errors",
        inferResults,
        containsExactly(
            RESOURCE_LEAK,
            ReaderLeaks,
            methods
        )
    );
  }

}
