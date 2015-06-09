/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;
import static utils.matchers.ResultContainsNumberOfErrorsInMethod.containsNumberOfErrors;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ResourceLeaksTest {

  public static final String ResourceLeaks =
      "infer/tests/codetoanalyze/java/infer/ResourceLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(ResourceLeaksTest.class, ResourceLeaks);
  }


  //FileOutputStream tests

  @Test
  public void whenInferRunsOnFileOutputStreamNotClosedThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "fileOutputStreamNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnFileOutputStreamNotClosed2ThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "fileOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnFileOutputStreamClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "fileOutputStreamClosed"));
  }

  @Test
  public void whenInferRunsOnFileOutputStreamTwoLeaksThenTwoLeaksAreFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain 2 resource leak error",
        inferResults,
        containsNumberOfErrors(
            RESOURCE_LEAK,
            ResourceLeaks,
            "fileOutputStreamTwoLeaks",
            2
        )
    );
  }

  //TwoResource tests
  @Test
  public void whenInferRunsOnTwoResourcesHeliosFixThenResourceLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "twoResourcesHeliosFix"
        )
    );
  }

  @Test
  public void whenInferRunsOnTwoResourcesThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "twoResources"
        )
    );
  }

  @Test
  public void whenInferRunsOnTwoResourcesCommonFixThenResourceLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "twoResourcesCommonFix"
        )
    );
  }

  @Test
  public void whenInferRunsOnTwoResourcesServerSocketThenResourceLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "twoResourcesServerSocket"
        )
    );
  }

  @Test
  public void whenInferRunsOnTwoResourcesRandomAccessFileThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "twoResourcesRandomAccessFile"
        )
    );
  }


  @Test
  public void whenInferRunsOnTwoResourcesRAFCommonFixThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "twoResourcesRandomAccessFileCommonFix"
        )
    );
  }

  //NestedResource tests

  @Test
  public void whenInferRunsOnNestedGoodThenResourceLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "nestedGood"
        )
    );
  }

  @Test
  public void whenInferRunsOnNestedBad1ThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "nestedBad1"
        )
    );
  }


  @Test
  public void whenInferRunsOnNestedBad2ThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "nestedBad2"
        )
    );
  }


  @Test
  public void whenInferRunsOnObjectInputStreamClosedNestedBadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "objectInputStreamClosedNestedBad"
        )
    );
  }

  @Test
  public void whenInferRunsOnObjectOutputStreamClosedNestedBadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "objectOutputStreamClosedNestedBad"
        )
    );
  }


  //ZipFile tests   (JarFile tests also test ZipFiles)

  @Test
  public void whenInferRunsOnZipFileLeakExceptionalBranchThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK, ResourceLeaks,
            "zipFileLeakExceptionalBranch"
        )
    );
  }

  @Test
  public void whenInferRunsOnzipFileNoLeakThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "jarFileClosed"
        )
    );
  }

  //JarFile tests

  @Test
  public void whenInferRunsOnJarFileNotClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK, ResourceLeaks,
            "jarFileNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnJarFileClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "jarFileClosed"
        )
    );
  }

  //FileInputStream tests
  @Test
  public void whenInferRunsOnFileInputStreamNotClosedAfterReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK, ResourceLeaks,
            "fileInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnFileInputStreamClosedThenResourceLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "fileInputStreamClosed"
        )
    );
  }

  //PipedInputStream tests
  @Test
  public void whenInferRunsOnPipedInputStreamNotClosedAftReadThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK, ResourceLeaks,
            "pipedInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnPipedInputStreamClosedThenResourceLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "pipedInputStreamClosed"
        )
    );
  }

  //PipedOutputStream tests
  @Test
  public void whenInferRunsOnPipedOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "pipedOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnPipedOutputStreamClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "pipedOutputStreamClosed"
        )
    );
  }

  //ObjectOutputStream tests
  @Test
  public void whenInferRunsOnObjectOutputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "objectOutputStreamNotClosedAfterWrite"
        )
    );
  }

  @Test
  public void whenInferRunsOnObjectOutputStreamClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "objectOutputStreamClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnObjectOutputStreamClosed2ThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "objectOutputStreamClosed2"
        )
    );
  }

  //ObjectInputStream tests
  @Test
  public void whenInferRunsOnObjectInputStreamNotClosedAfterThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "objectInputStreamNotClosedAfterRead"
        )
    );
  }

  @Test
  public void whenInferRunsOnObjectInputStreamClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "objectInputStreamClosed"
        )
    );
  }

  //JarInputStream tests
  @Test
  public void whenInferRunsJarInputStreamLeakThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "jarInputStreamLeak"
        )
    );
  }

  @Test
  public void whenInferRunsOnJarInputStreamNoLeakThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "jarInputStreamNoLeak"
        )
    );
  }

  @Test
  public void whenInferRunsNestedBadJarInputStreamThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "nestedBadJarInputStream"
        )
    );
  }

  //JarOutputStream tests
  @Test
  public void whenInferRunsOnJarOutputStreamLeakThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK, ResourceLeaks,
            "jarOutputStreamLeak"
        )
    );
  }

  @Test
  public void whenInferRunsOnJarOutputStreamNoLeakThenResourceLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "jarOutputStreamNoLeak"
        )
    );
  }


  @Test
  public void whenInferRunsNestedBadJarOutputStreamThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK, ResourceLeaks,
            "nestedBadJarOutputStream"
        )
    );
  }


  //Socket tests
  @Test
  public void whenInferRunsOnSocketNotClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "socketNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnSocketClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "socketClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnSocketInputStreamNotClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "socketInputStreamNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnSocketInputStreamClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "socketInputStreamClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnSocketOutputStreamNotClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "socketOutputStreamNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnSocketOutputStreamClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "socketOutputStreamClosed"
        )
    );
  }

  //ServerSocket tests
  @Test
  public void whenInferRunsOnServerSocketNotClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "serverSocketNotClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnServerSocketClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "serverSocketClosed"
        )
    );
  }

  @Test
  public void whenInferRunsOnServerSocketWithSocketClosedThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "serverSocketWithSocketClosed"
        )
    );
  }

  //HttpURLConnection
  @Test
  public void whenInferRunsOnOpenHttpURLConnectionNotDisconnThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK, ResourceLeaks,
            "openHttpURLConnectionNotDisconnected"
        )
    );
  }

  @Test
  public void whenInferRunsOnOpenHttpURLConnectionDisconnThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "openHttpURLConnectionDisconnected"
        )
    );
  }

  @Test
  public void whenInferRunsOnOpenHttpsURLConnectionNotDisconnThenRLIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "openHttpsURLConnectionNotDisconnected"
        )
    );
  }

  @Test
  public void whenInferRunsOnOpenHttpsURLConnectionDisconnThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "openHttpsURLConnectionDisconnected"
        )
    );
  }

  //Closeables.close
  @Test
  public void whenInferRunsOnClosedWithCloseablesThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "closedWithCloseables"
        )
    );
  }

  @Test
  public void whenInferRunsOnNullClosedWithCloseablesThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "closeNullWithCloseables"
        )
    );
  }

  //Closeables.closeQuietly
  @Test
  public void whenInferRunsOnClosedQuietlyWithCloseablesThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "closedQuietlyWithCloseables"
        )
    );
  }

  @Test
  public void whenInferRunsOnNullClosedQuietlyWithCloseablesThenRLIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "closeNullQuietlyWithCloseables"
        )
    );
  }

  // JsonParser
  @Test
  public void whenInferRunsOnParseFromStringNoLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "parseFromStringAndNotClose"
        )
    );
  }

  @Test
  public void whenInferRunsOnParseFromInputStreamAndCloseNoLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "parseFromInputStreamAndClose"
        )
    );
  }

  @Test
  public void whenInferRunsOnParseFromInputStreamAndLeakLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "parseFromInputStreamAndLeak"
        )
    );
  }

// Irritating Installation.java fp that has been banished hopefully forever

  @Test
  public void whenInferRunsOnreadInstallationFileBad()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "readInstallationFileBad"
        )
    );
  }

  @Test
  public void whenInferRunsOnreadInstallationFileGood()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "readInstallationFileGood"
        )
    );
  }


  //URLConnection tests
  @Test
  public void whenInferRunsOnReadConfigCloseStreamThenLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "readConfigCloseStream"
        )
    );
  }

  @Test
  public void whenInferRunsOnReadConfigNotCloseStreamThenLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "readConfigNotCloseStream"
        )
    );
  }

  @Test
  public void whenInferRunsOnReadConfigNotClosedOK()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "readConfigNotClosedOK"
        )
    );
  }

  @Test
  public void whenInferRunsThemeObtainTypedArrayAndRecycle()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "themeObtainTypedArrayAndRecycle"
        )
    );
  }

  @Test
  public void whenInferRunsThemeObtainTypedArrayAndLeak()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "themeObtainTypedArrayAndLeak"
        )
    );
  }

  @Test
  public void whenInferRunsActivityObtainTypedArrayAndRecycle()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "activityObtainTypedArrayAndRecycle"
        )
    );
  }

  @Test
  public void whenInferRunsActivityObtainTypedArrayAndLeak()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "activityObtainTypedArrayAndLeak"
        )
    );
  }

  @Test
  public void whenInferRunsContextObtainTypedArrayAndRecycle()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "contextObtainTypedArrayAndRecycle"
        )
    );
  }

  @Test
  public void whenInferRunsContextObtainTypedArrayAndLeak()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "contextObtainTypedArrayAndLeak"
        )
    );
  }

  @Test
  public void whenInferRunsOnCopyFileLeak()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "copyFileLeak"
        )
    );
  }

  @Test
  public void whenInferRunsOnCopyFileClose()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "copyFileClose"
        )
    );
  }

  @Test
  public void whenInferRunsOnCheckNotNullCauseNoLeakClose()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "checkNotNullCauseNoLeak"
        )
    );
  }

  // java.utils.Scanner
  @Test
  public void whenInferRunsOnScannerNotClosedThenResourceLeakIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        contains(
            RESOURCE_LEAK,
            ResourceLeaks,
            "scannerNotClosed"
        )
    );
  }
  @Test
  public void whenInferRunsOnScannerClosedThenResourceLeakIsNotFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should not contain a resource leak error",
        inferResults,
        doesNotContain(
            RESOURCE_LEAK,
            ResourceLeaks,
            "scannerClosed"
        )
    );
  }


  // remember to put method names here
  @Test
  public void whenInferRunsOnResourceLeaksThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedMethods = {
        "fileOutputStreamNotClosed",
        "fileOutputStreamNotClosedAfterWrite",
        "fileOutputStreamClosed",
        "fileOutputStreamTwoLeaks",
        "twoResources",
        "twoResourcesHeliosFix",
        "twoResourcesCommonFix",
        "twoResourcesServerSocket",
        "twoResourcesRandomAccessFile",
        "twoResourcesRandomAccessFileCommonFix",
        "nestedGood",
        "nestedBad1",
        "nestedBad2",
        "objectInputStreamClosedNestedBad",
        "objectOutputStreamClosedNestedBad",
        "zipFileLeakExceptionalBranch",
        "zipFileNoLeak",
        "jarFileNotClosed",
        "jarFileClosed",
        "fileInputStreamNotClosedAfterRead",
        "fileInputStreamClosed",
        "pipedInputStreamNotClosedAfterRead",
        "pipedInputStreamClosed",
        "pipedOutputStreamNotClosedAfterWrite",
        "pipedOutputStreamClosed",
        "objectOutputStreamNotClosedAfterWrite",
        "objectOutputStreamClosed",
        "objectInputStreamNotClosedAfterRead",
        "objectInputStreamClosed",
        "objectInputStreamClosed2",
        "jarInputStreamNoLeak",
        "jarInputStreamLeak",
        "jarOutputStreamNoLeak",
        "jarOutputStreamLeak",
        "nestedBadJarInputStream",
        "nestedBadJarOutputStream",
        "socketNotClosed",
        "socketClosed",
        "socketInputStreamNotClosed",
        "socketInputStreamClosed",
        "socketOutputStreamNotClosed",
        "socketOutputStreamClosed",
        "serverSocketNotClosed",
        "serverSocketClosed",
        "serverSocketWithSocketClosed",
        "openHttpURLConnectionDisconnected",
        "openHttpURLConnectionNotDisconnected",
        "openHttpsURLConnectionNotDisconnected",
        "openHttpsURLConnectionDisconnected",
        "closedWithCloseables",
        "closedQuietlyWithCloseables",
        "parseFromStringAndClose",
        "parseFromStringAndLeak",
        "parseFromInputStreamAndClose",
        "parseFromInputStreamAndLeak",
        "ignore",
        "readInstallationFileGood",
        "readInstallationFileBad",
        "readConfigCloseStream",
        "readConfigNotCloseStream",
        "readConfigNotClosedOK",
        "themeObtainTypedArrayAndRecycle",
        "themeObtainTypedArrayAndLeak",
        "activityObtainTypedArrayAndRecycle",
        "activityObtainTypedArrayAndLeak",
        "contextObtainTypedArrayAndRecycle",
        "contextObtainTypedArrayAndLeak",
        "copyFileClose",
        "copyFileLeak",
        "checkNotNullCauseNoLeak",
        "scannerNotClosed",
        "scannerClosed"
    };
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            RESOURCE_LEAK,
            ResourceLeaks,
            expectedMethods));
  }

}
