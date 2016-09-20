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
import com.google.common.collect.ImmutableList;

import org.junit.rules.TemporaryFolder;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import javax.annotation.Nullable;

public class InferRunner {

  public static final String BUGS_FILE_NAME = "report.csv";

  public static final String DOT_FILE_NAME = "icfg.dot";

  public static final String CAPTURED_FOLDER = "captured";

  private static File bugsFile;

  @Nullable
  private static File dotFile;

  private static File resultsDir;

  private static final ImmutableList<String> EMPTY_ARGS = ImmutableList.of();

  private static final String ANDROID =
      "/infer/lib/java/android/android-19.jar";

  private static final String[] LIBRARIES = {
      "/infer/lib/java/models.jar",
      "/infer/annotations/annotations.jar",
      "/dependencies/java/guava/guava-10.0.1-fork.jar",
      "/dependencies/java/jackson/jackson-2.2.3.jar",
  };

  private static HashMap<String, InferResults> inferResultsMap =
      new HashMap<String, InferResults>();

  private static String getXcodeRoot() throws IOException, InterruptedException {
    ProcessBuilder pb = new ProcessBuilder("xcode-select", "-p");
    Process process = pb.start();
    InputStream is = process.getInputStream();
    InputStreamReader isr = new InputStreamReader(is);
    BufferedReader br = new BufferedReader(isr);
    String line = br.readLine();
    process.waitFor();
    return line;
  }

  private static String getXCodeVersion() throws IOException, InterruptedException {
    ProcessBuilder pb = new ProcessBuilder("xcodebuild", "-version");
    Process process = pb.start();
    InputStream is = process.getInputStream();
    InputStreamReader isr = new InputStreamReader(is);
    BufferedReader br = new BufferedReader(isr);
    String line = br.readLine();
    process.waitFor();
    return line;
  }

  private static ImmutableList<String> createInferJavaCommand(
      TemporaryFolder folder,
      ImmutableList<String> sourceFiles,
      String analyzer,
      ImmutableList<String> args) {
    try {
      File resultsDir = createResultsDir(folder);
      String resultsDirName = resultsDir.getAbsolutePath();
      InferRunner.bugsFile = new File(resultsDir, BUGS_FILE_NAME);

      ImmutableList<String> javacCmd = createJavacCommand(
          folder,
          sourceFiles);

      return new ImmutableList.Builder<String>()
          .add("infer")
          .add("--no-progress-bar")
          .add("--no-filtering")
          .addAll(args)
          .add("-o")
          .add(resultsDirName)
          .add("-a")
          .add(analyzer)
          .add("--")
          .addAll(javacCmd)
          .build();
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(1);
      return ImmutableList.of(); // unreachable
    }
  }

  private static ImmutableList<String> createJavacCommand(
      TemporaryFolder folder,
      ImmutableList<String> sourceFiles)
      throws IOException {
    File classesDir = folder.newFolder("classes_out");
    String classesDirName = classesDir.getAbsolutePath();

    ImmutableList<String> javacCmd = new ImmutableList.Builder<String>()
        .add("javac")
        .add("-classpath")
        .add(getClasspath())
        .add("-bootclasspath")
        .add(getBootClasspath())
        .add("-d")
        .add(classesDirName)
        .addAll(sourceFiles).build();
    return javacCmd;
  }

  public static ImmutableList<String> createJavaInferHarnessCommand(
      TemporaryFolder folder,
      ImmutableList<String> sourceFiles) {
    ImmutableList<String> args = new ImmutableList.Builder<String>()
      .add("--android-harness")
      .build();
    return createInferJavaCommand(folder, sourceFiles, "infer", args);
  }

  public static ImmutableList<String> createJavaInferHarnessCommand(
      TemporaryFolder folder,
      String sourceFile) {
    return createJavaInferHarnessCommand(folder, ImmutableList.of(sourceFile));
  }

  public static ImmutableList<String> createJavaInferAngelicHarnessCommand(
      TemporaryFolder folder,
      ImmutableList<String> sourceFiles) {
    ImmutableList<String> args = (new ImmutableList.Builder<String>())
      .add("--android-harness")
      .build();
    return createInferJavaCommand(folder, sourceFiles, "infer", args);
  }

  private static InferResults runInfer(
      ImmutableList<String> inferCmd,
      Language lang)
      throws IOException, InterruptedException, InferException {
    String inferCmdString = Joiner.on(' ').join(inferCmd);
    InferResults results = inferResultsMap.get(inferCmdString);
    if (results == null) {
      if (lang == Language.Java) checkLibraries();

      runCommand(inferCmd, TestType.END_TO_END);
      String errorString = getErrors(bugsFile);
      results = new InferResults(inferCmd);
      if (lang == Language.Java)
        results.parseJavaInferResultsFromString(errorString);
      else if (lang == Language.C || lang == Language.CPP || lang == Language.ObjCPP)
        results.parseCInferResultsFromString(errorString);
      else if (lang == Language.ObjC)
        results.parseObjCInferResultsFromString(errorString);
      inferResultsMap.put(inferCmdString, results);
    }
    return results;
  }

  public static InferResults runInferJava(ImmutableList<String> inferCmd)
      throws IOException, InterruptedException, InferException {
    return runInfer(inferCmd, Language.Java);
  }

  public static InferResults runInferC(ImmutableList<String> inferCmd)
      throws IOException, InterruptedException, InferException {
    return runInfer(inferCmd, Language.C);
  }

  public static InferResults runInferCPP(ImmutableList<String> inferCmd)
      throws IOException, InterruptedException, InferException {
    return runInfer(inferCmd, Language.CPP);
  }

  public static InferResults runInferObjC(ImmutableList<String> inferCmd)
      throws IOException, InterruptedException, InferException {
    return runInfer(inferCmd, Language.ObjC);
  }

  public static InferResults runInferObjCPP(ImmutableList<String> inferCmd)
      throws IOException, InterruptedException, InferException {
    return runInfer(inferCmd, Language.ObjCPP);
  }


  private static void runCommand(
      ImmutableList<String> inferCmd,
      TestType testType)
      throws IOException, InterruptedException, InferException {
    ProcessBuilder pb = new ProcessBuilder(inferCmd);

    Map<String, String> env = pb.environment();
    env.put("INFER_REPORT_CUSTOM_ERROR", "1");

    Process process = pb.start();
    StringBuilder stderr = new StringBuilder();
    try (BufferedReader bufferedReader =
             new BufferedReader(
                 new InputStreamReader(
                     process.getErrorStream()))) {
      String line = null;
      while ((line = bufferedReader.readLine()) != null) {
        stderr.append(line + "\n");
      }
    }
    process.waitFor();

    File file = null;
    if (testType == TestType.END_TO_END) {
      file = bugsFile;
    } else if (testType == TestType.FRONTEND) {
      getDotFile();
      file = dotFile;
    }
    if (file == null || !file.exists()) {
      String fileNotFoundMessage = file == null ?
          "" : "\n\nFile " + file + " not found.";
      throw new InferException(
          "There was an error while calling Infer." +
              "\n\nCommand output:\n" + stderr +
              "\nCommand: " + Joiner.on(' ').join(inferCmd) +
              fileNotFoundMessage +
              "\n==========================================================================");
    }
  }

  static String getErrors(File file) throws IOException {
    String s = "";
    String line = "";
    try (BufferedReader br = new BufferedReader(new FileReader(file))) {
      br.readLine();
      while ((line = br.readLine()) != null) {
        s += line + "\n";
      }
    }
    return s;
  }

  private static String getBootClasspath() {
    String current_dir = System.getProperty("user.dir");
    String bootclasspath = current_dir + ANDROID;
    return bootclasspath;
  }

  private static String getClasspath() {
    String current_dir = System.getProperty("user.dir");
    StringBuilder classpath = new StringBuilder();
    for (String library : LIBRARIES) {
      classpath.append(current_dir);
      classpath.append(library);
      classpath.append(":");
    }
    classpath.deleteCharAt(classpath.length() - 1);
    return classpath.toString();
  }

  private static void checkLibraries() throws InferException {
    String current_dir = System.getProperty("user.dir");
    for (String lib_file : LIBRARIES) {
      File lib = new File(current_dir + lib_file);
      if (!lib.exists()) {
        throw new InferException("File " + lib_file + " not found.");
      }
    }
  }

  private static File createResultsDir(TemporaryFolder folder) {
    try {
      resultsDir = folder.newFolder("infer_out");
      return resultsDir;
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(1);
      return null; // unreachable
    }
  }

  private static void getDotFile() {
    File captured = new File(resultsDir, CAPTURED_FOLDER);
    if (captured.exists()) {
      File f = new File(captured, captured.list()[0]);
      dotFile = new File(f, DOT_FILE_NAME);
    } else {
      dotFile = null;
    }
  }
}
