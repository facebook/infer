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

public class CursorLeaksTest {

  public static final String CursorLeaks =
      "infer/tests/codetoanalyze/java/infer/CursorLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws IOException {
    inferResults = InferResults.loadInferResults(CursorLeaksTest.class, CursorLeaks);
  }

  @Test
  public void whenInferRunsOnCursorClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, CursorLeaks,
            "cursorClosed"));
  }

  @Test
  public void whenInferRunsOnCursorNotClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            CursorLeaks,
            "cursorNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnGetImageCountHelperNotClosedThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            CursorLeaks,
            "getImageCountHelperNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnGetImageCountHelperClosedThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, CursorLeaks,
            "getImageCountHelperClosed"));
  }

  @Test
  public void whenInferRunsOnGetBucketCountNotClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            CursorLeaks,
            "getBucketCountNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnGetBucketCountClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, CursorLeaks,
            "getBucketCountClosed"));
  }

  @Test
  public void whenInferRunsOnQueryUVMLegacyDbNotClosedThenResourceLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            CursorLeaks,
            "queryUVMLegacyDbNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnQueryUVMLegacyDbClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, CursorLeaks,
            "queryUVMLegacyDbClosed"));
  }

  @Test
  public void whenInferRunsOnCompleteDownloadNotClosedThenResourceLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            CursorLeaks,
            "completeDownloadNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnCompleteDownloadClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, CursorLeaks,
            "completeDownloadClosed"));
  }

  @Test
  public void whenInferRunsOnLoadPrefsFromContentProvNotClosedThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            CursorLeaks,
            "loadPrefsFromContentProviderNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnLoadPrefsFromContentProvClosedThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK, CursorLeaks,
            "loadPrefsFromContentProviderClosed"));
  }


  @Test
  public void whenInferRunsOnResourceLeaksThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {
        "cursorClosed",
        "cursorNotClosed",
        "getImageCountHelperClosed",
        "getImageCountHelperNotClosed",
        "getBucketCountNotClosed",
        "getBucketCountClosed",
        "queryUVMLegacyDbNotClosed",
        "queryUVMLegacyDbClosed",
        "completeDownloadClosed",
        "completeDownloadNotClosed",
        "loadPrefsFromContentProviderClosed",
        "loadPrefsFromContentProviderNotClosed"
    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            RESOURCE_LEAK,
            CursorLeaks,
            expectedMethods));
  }

}
