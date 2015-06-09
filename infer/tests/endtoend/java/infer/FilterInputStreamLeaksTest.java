/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/
//Class to tests resource leaks on the class FilterInputStream and its subclasses
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

public class FilterInputStreamLeaksTest {

  public static final String FilterInputStreamLeaks =
      "infer/tests/codetoanalyze/java/infer/FilterInputStreamLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(
        FilterInputStreamLeaksTest.class,
        FilterInputStreamLeaks);
  }

  //BufferedInputStream tests
  @Test
  public void whenInferRunsOnBufferedInputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "bufferedInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnBufferedInputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "bufferedInputStreamClosedAfterReset"));
  }

  //CheckedInputStream tests
  @Test
  public void whenInferRunsOnCheckedInputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "checkedInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnCheckedInputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "checkedInputStreamClosedAfterSkip"));
  }

  //CipherInputStream tests
  @Test
  public void whenInferRunsOnCipherInputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "cipherInputStreamNotClosedAfterSkip"
        )
    );
  }

  @Test
  public void whenInferRunsOnCipherInputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "cipherInputStreamClosedAfterRead"));
  }

  //DataInputStream tests
  @Test
  public void whenInferRunsOnDataInputStreamNotClosedAfterReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "dataInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnDataInputStreamClosedAfterReadThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "dataInputStreamClosedAfterReadBoolean"));
  }

  //DeflaterInputStream tests
  @Test
  public void whenInferRunsOnDeflaterInputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "deflaterInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnDeflaterInputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "deflaterInputStreamClosedAfterReset"));
  }

  //GZipInputStream tests
  @Test
  public void whenInferRunsOnGzipInputStreamNotClosedAfterReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "gzipInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnGzipInputStreamClosedAfterReadThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "gzipInputStreamClosedAfterRead"));
  }

  //DigestInputStream tests
  @Test
  public void whenInferRunsOnDigestInputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "digestInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnDigestInputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "digestInputStreamClosedAfterRead"));
  }

  //InflaterInputStream tests
  @Test
  public void whenInferRunsOnInflaterInputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "inflaterInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnInflaterInputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "inflaterInputStreamClosedAfterAvailable"));
  }

  //PushbackInputStream tests
  @Test
  public void whenInferRunsOnInPushbackInputStreamNotClosedThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "pushbackInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnPushbackInputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            "pushbackInputStreamClosedAfterReset"));
  }

  @Test
  public void whenInferRunsThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {
        "bufferedInputStreamNotClosedAfterRead",
        "bufferedInputStreamClosedAfterReset",
        "checkedInputStreamNotClosedAfterRead",
        "checkedInputStreamClosedAfterSkip",
        "cipherInputStreamNotClosedAfterSkip",
        "cipherInputStreamClosedAfterRead",
        "dataInputStreamNotClosedAfterRead",
        "dataInputStreamClosedAfterReadBoolean",
        "deflaterInputStreamNotClosedAfterRead",
        "deflaterInputStreamClosedAfterReset",
        "gzipInputStreamNotClosedAfterRead",
        "gzipInputStreamClosedAfterRead",
        "digestInputStreamNotClosedAfterRead",
        "digestInputStreamClosedAfterRead",
        "inflaterInputStreamNotClosedAfterRead",
        "inflaterInputStreamClosedAfterAvailable",
        "progressMonitorInputStreamNotClosedAfterRead",
        "progressMonitorInputStreamClosedAfterSkip",
        "pushbackInputStreamNotClosedAfterRead",
        "pushbackInputStreamClosedAfterReset",

    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            RESOURCE_LEAK,
            FilterInputStreamLeaks,
            expectedMethods));
  }

}
