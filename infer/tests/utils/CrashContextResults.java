/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package utils;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.google.common.io.ByteStreams;

public class CrashContextResults {

  private static final String CRASHCONTEXT_JSON =
    "/buck-out/gen/infer/tests/codetoanalyze/java/crashcontext/analyze/crashcontext.json";

  private JsonNode json;
  private String filename;

  private CrashContextResults(String tag) throws IOException {
    String path =
      System.getProperty("user.dir") + CRASHCONTEXT_JSON + "." + tag;
    byte[] jsonData = ByteStreams.toByteArray(new FileInputStream(path));
    ObjectMapper objectMapper = new ObjectMapper();
    json = objectMapper.readTree(jsonData);
  }

  public boolean hasStackFrame(String methodSignature, int pos) {
    return methodSignature.equals(
      json.path("stack").get(pos).path("method_name").asText());
  }

  public boolean hasStackFrame(String methodSignature) {
    for (JsonNode frame : json.path("stack")) {
      if (methodSignature.equals(frame.path("method_name").asText())) {
        return true;
      }
    }
    return false;
  }

  public boolean hasLocationOnStack(String filename, int line) {
    for (JsonNode frame : json.path("stack")) {
      if (filename.equals(frame.path("location").path("file").asText()) &&
         line == frame.path("location").path("line").asInt()) {
        return true;
      }
    }
    return false;
  }

  public boolean hasNativeMethodOnStack() {
    for (JsonNode frame : json.path("stack")) {
      if (frame.path("location").path("file").asText()
            .equals("Native Method")) {
        return true;
      }
    }
    return false;
  }

  private List<JsonNode> findNodesForMethod(JsonNode node,
                                            String methodSignature,
                                            List<JsonNode> accumulator) {
    if (methodSignature.equals(node.path("method_name").asText())) {
      accumulator.add(node);
    }
    for (JsonNode callee : node.path("callees")) {
      findNodesForMethod(callee, methodSignature, accumulator);
    }
    return accumulator;
  }

  private List<JsonNode> findNodesForMethod(String methodSignature) {
    List<JsonNode> accumulator = new ArrayList<JsonNode>();
    for (JsonNode frame : json.path("stack")) {
      findNodesForMethod(frame, methodSignature, accumulator);
    }
    return accumulator;
  }

  public boolean hasMethod(String methodSignature) {
    return !findNodesForMethod(methodSignature).isEmpty();
  }

  public boolean hasNotMethod(String methodSignature) {
    return findNodesForMethod(methodSignature).isEmpty();
  }

  public boolean hasPath(String methodFrom, String methodTo) {
    for (JsonNode from : findNodesForMethod(methodFrom)) {
      if (!findNodesForMethod(from, methodTo, new ArrayList()).isEmpty()) {
        return true;
      }
    }
    return false;
  }

  public boolean hasMethodWithLocation(String methodSignature, String filename,
                                       int line, int start, int end) {
    for (JsonNode frame : findNodesForMethod(methodSignature)) {
      if (frame.path("location").path("file").asText().endsWith(filename) &&
         line == frame.path("location").path("line").asInt()) {
        for (JsonNode blameRange : frame.path("location").path("blame_range")) {
          if (blameRange.path("start_line").asInt() == start &&
              blameRange.path("end_line").asInt() == end) {
                return true;
          }
        }
      }
    }
    return false;
  }

  public static CrashContextResults loadJSONResults(String tag) throws IOException {
    return new CrashContextResults(tag);
  }

}
