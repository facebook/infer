/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.crashcontext;

import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.CrashContextResults;

public class MinimalCrashTest {

  public static final String TAG = "MinimalCrashExample";

  public static final String MAIN_METHOD =
    "codetoanalyze.java.crashcontext.MinimalCrashExample.main(java.lang.String[]):void";

  public static final String TO_STRING_METHOD =
    "java.lang.String.toString():java.lang.String";

  private static CrashContextResults crashcontext;

  @BeforeClass
  public static void loadResults() throws IOException {
    crashcontext =
      CrashContextResults.loadJSONResults(TAG);
  }

  @Test
  public void shapeOfTheStack() {
      assertThat("The stack trace should contain " + MAIN_METHOD,
                 crashcontext.hasStackFrame(MAIN_METHOD, 0));
  }

  @Test
  public void toStringMethodIsDetected() {
    assertThat("Method " + TO_STRING_METHOD + " should be part of the context",
               crashcontext.hasMethod(TO_STRING_METHOD));
    assertThat("Method " + TO_STRING_METHOD + " should be reachable in the " +
               "context tree from " + MAIN_METHOD,
               crashcontext.hasPath(MAIN_METHOD, TO_STRING_METHOD));
  }

}
