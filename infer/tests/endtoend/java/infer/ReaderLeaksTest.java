/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/
//Class to tests resource leaks on the class FilterOutputStream and its subclasses
package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

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

  //Reader  tests
  @Test
  public void whenInferRunsOnReaderNotClosedAfterReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ReaderLeaks,
            "readerNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnReaderClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "readerClosed"));
  }

  //BufferedReader  tests
  @Test
  public void whenInferRunsOnBufferedReaderNotClosedAfterReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ReaderLeaks,
            "bufferedReaderNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnBufferedReaderClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "bufferedReaderClosed"));
  }

  //InputStreamReader  tests
  @Test
  public void whenInferRunsOnInputStreamReadNotClosedAfterReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ReaderLeaks,
            "inputStreamReaderNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnInputStreamReaderClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "inputStreamReaderClosed"));
  }

  //FileReader tests

  @Test
  public void whenInferRunsOnFileReaderNotClosedAfterReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ReaderLeaks,
            "fileReaderNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnFileReaderClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "fileReaderClosed"));
  }

  //PushbackReader  tests
  @Test
  public void whenInferRunsOnPushbackReaderNotClosedAfterReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ReaderLeaks,
            "pushbackReaderNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnPushbackReaderClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "pushbackReaderClosed"));
  }

  //PipedReader  tests
  @Test
  public void whenInferRunsOnPipedReaderNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ReaderLeaks,
            "pipedReaderNotClosedAfterConstructedWithWriter"
        )
    );
  }

  @Test
  public void whenInferRunsOnPipedReaderNotClosedAfterConnectThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ReaderLeaks,
            "pipedReaderNotClosedAfterConnect"
        )
    );
  }

  @Test
  public void whenInferRunsOnPipedReaderClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "pipedReaderClosed"));
  }

  @Test
  public void whenInferRunsOnPipedReaderNotConnectedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "pipedReaderNotConnected"));
  }

  /**
   * This potential false positive is up for discussion...
   */
  @Test
  public void whenInferRunsOnPipedReaderFalsePositiveThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ReaderLeaks,
            "pipedReaderFalsePositive"
        )
    );
  }

  @Test
  public void whenInferRunsOnStrictLineReaderClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "strictLineReaderClosed"));
  }

  @Test
  public void whenInferRunsOnStrictLineReaderLeakThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ReaderLeaks,
            "strictLineReaderNoLeak"));
  }


  @Test
  public void whenInferRunsOnReaderLeaksThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {
        "readerNotClosedAfterRead",
        "readerClosed",
        "bufferedReaderNotClosedAfterRead",
        "bufferedReaderClosed",
        "inputStreamReaderNotClosedAfterRead",
        "inputStreamReaderClosed",
        "fileReaderNotClosedAfterRead",
        "fileReaderClosed",
        "pushbackReaderNotClosedAfterRead",
        "pushbackReaderClosed",
        "pipedReaderNotClosedAfterConstructedWithWriter",
        "pipedReaderNotClosedAfterConnect",
        "pipedReaderClosed",
        "pipedReaderNotConnected",
        "pipedReaderFalsePositive",
        "strictLineReaderClosed",
        "strictLineReaderNoLeak"
    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            RESOURCE_LEAK,
            ReaderLeaks,
            expectedMethods));
  }

}
