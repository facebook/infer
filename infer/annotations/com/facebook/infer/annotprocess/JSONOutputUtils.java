/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.infer.annotprocess;

import java.io.PrintWriter;

/** Clearly, we should be using an existing JSON output library like Jackson here. However, we can't
  * do this because we do not want to add Jackson (or another JSON library) to the classpath of the
  * Java program we are building (along with the JAR for this processor). The reason is that if
  * there is a different version of the JSON parser library somewhere in the classpath for the
  * project, it could cause *very* strange problems. Instead, we rol our own library to avoid
  * introducing depencies on code that the projects we analyze might be using.
  */
public class JSONOutputUtils {

  private JSONOutputUtils() {}

  // print a comma between all JSON objects except for the last one
  public static void outputCommaIfNotLast(PrintWriter out, int elemCount, int elemMax) {
      if (elemCount == elemMax) {
        out.println("");
      } else {
        out.println(",");
    }
  }

  public static void outputMethod(PrintWriter out, String clazz, String method,
                                  int elemCount, int elemMax) {
    String TAB1 = "  ";
    String TAB2 = TAB1 + TAB1;
    out.println(TAB1 + "{");
    out.println(TAB2 +"\"language\": \"Java\",");
    out.print(TAB2 + "\"class\": \"" + clazz + "\"");
    if (method != null) {
      out.println(",");
      out.println(TAB2 + "\"method\": \"" + method + "\"");
    } else {
      out.println();
    }
    out.print(TAB1 + "}");
    outputCommaIfNotLast(out, elemCount, elemMax);
  }

  public static void outputClass(PrintWriter out, String clazz, int elemCount, int elemMax) {
    outputMethod(out, clazz, null, elemCount, elemMax);
  }

  public static void outputClassSourcePair(PrintWriter out, String clazz, String source,
                                           int elemCount, int elemMax) {
    String TAB = "  ";
    out.print(TAB + "\"" + clazz + "\": \"" + source + "\"");
    outputCommaIfNotLast(out, elemCount, elemMax);
  }

}
