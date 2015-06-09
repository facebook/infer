/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class WriterLeaksTest {


  public static final String WriterLeaks =
      "infer/tests/codetoanalyze/java/infer/WriterLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(WriterLeaksTest.class, WriterLeaks);
  }

  //Writer tests

  @Test
  public void whenInferRunsOnWriterNotClosedAfterWriteThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            WriterLeaks,
            "writerNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnWriterClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            WriterLeaks,
            "writerClosed"
        )
    );
  }

  //PrintWriter tests

  @Test
  public void whenInferRunsOnPrintWriterNotClosedAfterAppendThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK, WriterLeaks,
            "printWriterNotClosedAfterAppend"
        )
    );
  }

  @Test
  public void whenInferRunsOnPrintWriterClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, WriterLeaks,
            "printWriterClosed"
        )
    );
  }

  //BufferedWriter tests

  @Test
  public void whenInferRunsOnBufferedWriterNotClosedAfterWriteThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            WriterLeaks,
            "bufferedWriterNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnBufferedWriterClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, WriterLeaks,
            "bufferedWriterClosed"
        )
    );
  }

  //OutputStreamWriter tests
  @Test
  public void whenInferRunsOnOutputStreamWriterNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            WriterLeaks,
            "outputStreamWriterNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnOutputStreamWriterClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, WriterLeaks,
            "outputStreamWriterClosed"
        )
    );
  }

  //FileWriter tests
  @Test
  public void whenInferRunsOnFileWriterNotClosedAfterWriteThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            WriterLeaks,
            "fileWriterNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnFileWriterClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            WriterLeaks,
            "fileWriterClosed"
        )
    );
  }

  //PipedWriter  tests
  @Test
  public void whenInferRunsOnPipedWriterNotClosedAfterConstrThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            WriterLeaks,
            "pipedWriterNotClosedAfterConstructedWithReader"
        )
    );
  }

  @Test
  public void whenInferRunsOnPipedWriterNotClosedAfterConnectThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            WriterLeaks,
            "pipedWriterNotClosedAfterConnect"
        )
    );
  }

  @Test
  public void whenInferRunsOnPipedWriterClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            WriterLeaks,
            "pipedWriterClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnPipedWriterNotConnectedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            WriterLeaks,
            "pipedWriterNotConnected"
        )
    );
  }

  @Test
  public void whenInferRunsOnPrintWriterThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {
        "writerNotClosedAfterWrite",
        "writerClosed",
        "printWriterNotClosedAfterAppend",
        "printWriterClosed",
        "bufferedWriterNotClosedAfterWrite",
        "bufferedWriterClosed",
        "outputStreamWriterNotClosedAfterWrite",
        "outputStreamWriterClosed",
        "fileWriterNotClosedAfterWrite",
        "fileWriterClosed",
        "pipedWriterNotClosedAfterConstructedWithReader",
        "pipedWriterNotClosedAfterConnect",
        "pipedWriterClosed",
        "pipedWriterNotConnected",
    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            RESOURCE_LEAK,
            WriterLeaks,
            expectedMethods));
  }

}
