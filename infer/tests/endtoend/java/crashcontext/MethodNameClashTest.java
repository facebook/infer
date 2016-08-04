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

public class MethodNameClashTest {

  public static final String TAG = "MethodNameClashExample";

  public static final String MAIN_METHOD =
    "codetoanalyze.java.crashcontext.MethodNameClashExample.main(java.lang.String[]):void";

  public static final String A_FOO_METHOD =
    "codetoanalyze.java.crashcontext.MethodNameClashExample$A.foo():void";

  public static final String B_FOO_METHOD =
    "codetoanalyze.java.crashcontext.MethodNameClashExample$B.foo():void";

  private static CrashContextResults crashcontext;

  @BeforeClass
  public static void loadResults() throws IOException {
    crashcontext =
      CrashContextResults.loadJSONResults(TAG);
  }

  @Test
  public void shapeOfTheStack() {
      assertThat("The stack trace should contain " + A_FOO_METHOD,
                 crashcontext.hasStackFrame(A_FOO_METHOD, 0));
      assertThat("The stack trace should contain " + B_FOO_METHOD,
                 crashcontext.hasStackFrame(B_FOO_METHOD, 1));
      assertThat("The stack trace should contain " + MAIN_METHOD,
                 crashcontext.hasStackFrame(MAIN_METHOD, 2));
  }

}
