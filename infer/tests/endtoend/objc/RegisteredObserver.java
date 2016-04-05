/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.objc;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class RegisteredObserver {

  public static final String VCFile =
      "infer/tests/codetoanalyze/objc/errors/registered_observer/ViewController.m";

  private static ImmutableList<String> inferCmd;

  public static final String REGISTERED_OBSERVER = "REGISTERED_OBSERVER_BEING_DEALLOCATED";

  @ClassRule
  public static DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createObjCInferCommandSimple(
        folder,
        VCFile,
        "cf");
  }

  @Test
  public void RegisteredObserverShouldBeFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferObjC(inferCmd);
    String[] methods = {
      //"fooError", "barError"
    };
    assertThat(
        "Results should contain " + REGISTERED_OBSERVER,
        inferResults,
        containsExactly(
            REGISTERED_OBSERVER,
            VCFile,
            methods
        )
    );
  }

}
