/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package utils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Reader;

@JsonIgnoreProperties(ignoreUnknown=true)
public class InferStats {

  @JsonIgnoreProperties(ignoreUnknown=true)
  private static class IntFields {
    @JsonProperty(value = "files")
    int numFiles;

    @JsonProperty(value = "procedures")
    int numProcedures;

    @JsonProperty(value = "lines")
    int numLines;
  }

  private static class FloatFields {
    @JsonProperty(value = "reporting_time")
    float reportingTime;

    @JsonProperty(value = "capture_time")
    float captureTime;

    @JsonProperty(value = "analysis_time")
    float analysisTime;

    @JsonProperty(value = "makefile_generation_time")
    float makefileGenerationTime;
  }

  @JsonProperty(value = "int")
  private IntFields intFields;

  @JsonProperty(value = "float")
  private FloatFields floatFields;

  private static InferStats parseInferStatsFromJson(Reader reader) throws IOException {
    ObjectMapper mapper = new ObjectMapper();
    InferStats user = mapper.readValue(reader, InferStats.class);
    return user;
  }

  public static InferStats loadInferStats(Class currentClass, String sourceDir) throws IOException {
    BufferedReader reader =
      new BufferedReader(
        new InputStreamReader(
          currentClass.getResourceAsStream(sourceDir + "stats.json")));
    return parseInferStatsFromJson(reader);
  }

  public int getNumFiles() {
    return intFields.numFiles;
  }

  public int getNumProcedures() {
    return intFields.numProcedures;
  }

  public int getNumLines() {
    return intFields.numLines;
  }

  public float getReportingTime() {
    return floatFields.reportingTime;
  }

  public float getCaptureTime() {
    return floatFields.captureTime;
  }

  public float getAnalysisTime() {
    return floatFields.analysisTime;
  }

  public float getMakefileGenerationTime() {
    return floatFields.makefileGenerationTime;
  }

}
