/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objcpp.componentkit;

  import static org.hamcrest.MatcherAssert.assertThat;
  import static utils.matchers.ResultContainsLineNumbers.containsLinesOfError;
  import static org.hamcrest.core.IsNot.not;
  import com.google.common.collect.ImmutableList;
  import org.junit.BeforeClass;
  import org.junit.ClassRule;
  import org.junit.Test;
  import java.io.IOException;
  import utils.DebuggableTemporaryFolder;
  import utils.InferException;
  import utils.InferResults;
  import utils.InferRunner;

  public class MutableLocalVariableInNonComponentTest {

    public static final String FILE =
        "infer/tests/codetoanalyze/objcpp/componentkit/Test2.mm";
    private static ImmutableList<String> inferCmd;
    public static final String MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE =
        "MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE";

    @ClassRule
    public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

    @BeforeClass
    public static void runInfer() throws InterruptedException, IOException {
      inferCmd = InferRunner.createObjCPPInferCommand(
        folder,
        FILE);
    }
    @Test
    public void MLVsInComponentFile()
        throws InterruptedException, IOException, InferException {
      InferResults inferResults = InferRunner.runInferObjC(inferCmd);
      assertThat(
          "Results should not contain " + MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE,
          inferResults,
          not(containsLinesOfError(new int[]{38}, MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE)));
    }
}
