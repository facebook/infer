/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package utils;


import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import au.com.bytecode.opencsv.CSVReader;

public class InferResults {

  private ImmutableList<String> inferCmd;

  public static final Pattern JAVA_METHOD_NAME = Pattern.compile("(\\.)(.*)(\\()");
  public static final Pattern C_FUNCTION_NAME = Pattern.compile("(\")(.*)(\")");
  public static final Pattern OBJC_FUNCTION_NAME = Pattern.compile("(.*)_(.*)");

  private Vector<InferError> errors = new Vector<InferError>();

  InferResults() {
    this.inferCmd = ImmutableList.of("");
  }

  InferResults(ImmutableList<String> inferCmd) {
    this.inferCmd = inferCmd;
  }

  public void parseInferResultsFromString(Pattern pattern, String errorString)
    throws IOException, InferException {

    try (CSVReader reader = new CSVReader(new StringReader(errorString))) {
      List<String[]> lines = reader.readAll();
      Path root = Paths.get(System.getProperty("user.dir"));

      for (String[] items : lines) {
        String errorKind = items[1].trim();
        String errorType = items[2].trim();
        if (errorKind.equals("ERROR") ||
            errorType.equals("ASSIGN_POINTER_WARNING") ||
            errorType.equals("STRONG_DELEGATE_WARNING") ||
            errorType.equals("DIRECT_ATOMIC_PROPERTY_ACCESS") ||
            errorType.equals("CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK") ||
            errorType.equals("REGISTERED_OBSERVER_BEING_DEALLOCATED") ||
            errorType.equals("GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL") ||
            errorType.equals("IMMUTABLE_CAST") ||
            errorType.equals("PARAMETER_NOT_NULL_CHECKED") ||
            errorType.equals("DANGLING_POINTER_DEREFERENCE") ||
            errorType.equals("IVAR_NOT_NULL_CHECKED") ||
            errorType.equals("BAD_POINTER_COMPARISON") ||
            errorType.equals("Bad_footprint") ||
            errorType.equals("MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE")) {
          Integer errorLine = Integer.parseInt(items[5].trim());
          String procedure = items[6];
          Path path = Paths.get(items[8]);
          if (path.isAbsolute()) {
            path = root.relativize(Paths.get(items[8]));
          }
          Matcher methodMatcher = pattern.matcher(procedure);
          boolean matching = methodMatcher.find();
          if (matching) {
            procedure = methodMatcher.group(2);
            if (procedure == null) {
              throw new InferException("Unexpected method name structure.");
            }
          }
          procedure = procedure.trim();
          InferError error = new InferError(errorType, path, procedure, errorLine);
          errors.add(error);
        }
      }
    }
  }

  public void parseJavaInferResultsFromString(String errorString)
      throws IOException, InferException {
    parseInferResultsFromString(JAVA_METHOD_NAME, errorString);
  }

  public void parseCInferResultsFromString(String errorString)
      throws IOException, InferException {
    parseInferResultsFromString(C_FUNCTION_NAME, errorString);
  }

  public void parseObjCInferResultsFromString(String errorString)
      throws IOException, InferException {
    parseInferResultsFromString(OBJC_FUNCTION_NAME, errorString);
  }

  public Vector<InferError> getErrors() {
    return errors;
  }

  public String inferCmdToString() {
    return "Infer command: " + Joiner.on(' ').join(inferCmd);
  }

  public String toString() {
    String s = "";
    for (InferError e : errors) {
      s = s + "\n" + e.toString();
    }
    if (s.length() == 0) return "No results.";
    return s;
  }

  public void filter(String filename) {
    Vector<InferError> filtered_errors = new Vector<InferError>();
    for (InferError error : errors) {
      if (error.getErrorFile().endsWith(filename)) {
        filtered_errors.add(error);
      }
    }
    errors = filtered_errors;
  }

  public static InferResults loadResultsFromReader(
      BufferedReader reader,
      String sourceFile,
      Pattern pattern) {
    InferResults inferResults = new InferResults();
    String resultString = "";
    try {
      String line = reader.readLine();
      while (line != null) {
        resultString += line + "\n";
        line = reader.readLine();
      }
      inferResults.parseInferResultsFromString(
          pattern,
          resultString);
    } catch (IOException e) {
      throw new RuntimeException(e.toString());
    }
    inferResults.filter(sourceFile);
    return inferResults;
  }

  private static InferResults loadResultsFromPath(Class currentClass, String sourceFile,
                                                  String csvPath, Pattern procnamePattern) {
    BufferedReader reader =
        new BufferedReader(
            new InputStreamReader(
                currentClass.getResourceAsStream(csvPath)));
    return loadResultsFromReader(
        Preconditions.checkNotNull(reader),
        sourceFile,
        procnamePattern);
  }

  public static InferResults loadInferResults(Class currentClass, String sourceFile) {
    return loadResultsFromPath(
      currentClass,
      sourceFile,
      "/infer/tests/codetoanalyze/java/infer/report.csv",
      InferResults.JAVA_METHOD_NAME);
  }

  public static InferResults loadTracingResults(Class currentClass, String sourceFile) {
    return loadResultsFromPath(
      currentClass,
      sourceFile,
      "/infer/tests/codetoanalyze/java/tracing/report.csv",
      InferResults.JAVA_METHOD_NAME);
  }

  public static InferResults loadTracingComparisonResults(Class currentClass, String sourceFile) {
    return loadResultsFromPath(
      currentClass,
      sourceFile,
      "/infer/tests/codetoanalyze/java/infer/comparison_report.csv",
      InferResults.JAVA_METHOD_NAME);
  }

  public static InferResults loadCInferResults(Class currentClass, String sourceFile) {
    return loadResultsFromPath(
      currentClass,
      sourceFile,
      "/infer/tests/codetoanalyze/c/errors/report.csv",
      InferResults.C_FUNCTION_NAME);
  }

}
