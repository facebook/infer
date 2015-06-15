/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsNumberOfErrorsInMethod.containsNumberOfErrors;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class ResourceLeaksTest {

  public static final String SOURCE_FILE =
      "infer/tests/codetoanalyze/java/infer/ResourceLeaks.java";

  public static final String RESOURCE_LEAK = "RESOURCE_LEAK";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(ResourceLeaksTest.class, SOURCE_FILE);
  }


  @Test
  public void test()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
        "fileOutputStreamNotClosed",
        "fileOutputStreamNotClosedAfterWrite",
        "fileOutputStreamTwoLeaks",
        "twoResources",
        "twoResourcesServerSocket",
        "twoResourcesRandomAccessFile",
        "nestedBad1",
        "nestedBad2",
        "objectInputStreamClosedNestedBad",
        "objectOutputStreamClosedNestedBad",
        "zipFileLeakExceptionalBranch",
        "jarFileNotClosed",
        "fileInputStreamNotClosedAfterRead",
        "pipedInputStreamNotClosedAfterRead",
        "pipedOutputStreamNotClosedAfterWrite",
        "objectOutputStreamNotClosedAfterWrite",
        "objectInputStreamNotClosedAfterRead",
        "jarInputStreamLeak",
        "nestedBadJarInputStream",
        "jarOutputStreamLeak",
        "nestedBadJarOutputStream",
        "socketNotClosed",
        "serverSocketNotClosed",
        "openHttpURLConnectionNotDisconnected",
        "openHttpsURLConnectionNotDisconnected",
        "parseFromInputStreamAndLeak",
        "readInstallationFileBad",
        "readConfigNotCloseStream",
        "themeObtainTypedArrayAndLeak",
        "activityObtainTypedArrayAndLeak",
        "contextObtainTypedArrayAndLeak",
        "copyFileLeak",
        "copyFileLeak",
        "scannerNotClosed",
    };
    assertThat(
        "Results should contain the following resource leak errors",
        inferResults,
        containsExactly(
            RESOURCE_LEAK,
            SOURCE_FILE,
            methods
        )
    );
  }


  @Test
  public void whenInferRunsOnFileOutputStreamTwoLeaksThenTwoLeaksAreFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain 2 resource leak error",
        inferResults,
        containsNumberOfErrors(
            RESOURCE_LEAK,
            SOURCE_FILE,
            "fileOutputStreamTwoLeaks",
            2
        )
    );
  }

}
