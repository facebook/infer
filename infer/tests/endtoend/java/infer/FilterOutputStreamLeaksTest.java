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

import utils.InferException;
import utils.InferResults;

public class FilterOutputStreamLeaksTest {

  public static final String FilterOutputStreamLeaks =
      "infer/tests/codetoanalyze/java/infer/FilterOutputStreamLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(
        FilterOutputStreamLeaksTest.class,
        FilterOutputStreamLeaks);
  }

  //FilterOutputStream  tests
  @Test
  public void whenInferRunsOnFilterOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "filterOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnFilterOutputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "filterOutputStreamClosedAfterWrite"
        )
    );
  }

  //DataOutputStream tests
  @Test
  public void whenInferRunsOnDataOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "dataOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnDataOutputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "dataOutputStreamClosedAfterWrite"
        )
    );
  }

  //BufferedOutputStream tests
  @Test
  public void whenInferRunsOnBufferedOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "bufferedOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnBufferedOutputStreamClosedAftThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "bufferedOutputStreamClosedAfterWrite"
        )
    );
  }

  //CheckedOutputStream tests
  @Test
  public void whenInferRunsOnCheckedOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "checkedOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnCheckedOutputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "checkedOutputStreamClosedAfterWrite"));
  }

  //CipherOutputStream tests
  @Test
  public void whenInferRunsOnCipherOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "cipherOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnCipherOutputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "cipherOutputStreamClosedAfterWrite"));
  }

  //DeflaterOutputStream tests
  @Test
  public void whenInferRunsOnDeflaterOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "deflaterOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnDeflaterOutputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "deflaterOutputStreamClosedAfterWrite"));
  }

  //GZipOutputStream tests
  @Test
  public void whenInferRunsOnGzipOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "gzipOutputStreamNotClosedAfterFlush"
        )
    );
  }

  @Test
  public void whenInferRunsOnGzipOutputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "gzipOutputStreamClosedAfterWrite"));
  }

  //DigestOutputStream tests
  @Test
  public void whenInferRunsOnDigestOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "digestOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnDigestOutputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "digestOutputStreamClosedAfterWrite"));
  }

  //InflaterOutputStream tests
  @Test
  public void whenInferRunsOnInflaterOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "inflaterOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnInflaterOutputStreamClosedAfterThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "inflaterOutputStreamClosedAfterWrite"));
  }

  //PrintStream tests
  @Test
  public void whenInferRunsOnPrintStreamNotClosedAfterWriteThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "printStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnPrintStreamClosedAfterWriteThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            "printStreamClosedAfterWrite"
        )
    );
  }


  @Test
  public void whenInferRunsOnResourceLeaksThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {
        "filterOutputStreamNotClosedAfterWrite",
        "filterOutputStreamClosedAfterWrite",
        "dataOutputStreamNotClosedAfterWrite",
        "dataOutputStreamClosedAfterWrite",
        "bufferedOutputStreamNotClosedAfterWrite",
        "bufferedOutputStreamClosedAfterWrite",
        "checkedOutputStreamNotClosedAfterWrite",
        "checkedOutputStreamClosedAfterWrite",
        "cipherOutputStreamNotClosedAfterWrite",
        "cipherOutputStreamClosedAfterWrite",
        "deflaterOutputStreamNotClosedAfterWrite",
        "deflaterOutputStreamClosedAfterWrite",
        "digestOutputStreamNotClosedAfterWrite",
        "digestOutputStreamClosedAfterWrite",
        "inflaterOutputStreamNotClosedAfterWrite",
        "inflaterOutputStreamClosedAfterWrite",
        "gzipOutputStreamNotClosedAfterFlush",
        "gzipOutputStreamClosedAfterWrite",
        "printStreamNotClosedAfterWrite",
        "printStreamClosedAfterWrite",
    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            RESOURCE_LEAK,
            FilterOutputStreamLeaks,
            expectedMethods));
  }

}
