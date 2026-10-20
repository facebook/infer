/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.File;
import java.io.IOException;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;

// A report service running a report generator in a new process, with an environment built from
// the request. An entry such as "LD_PRELOAD=/tmp/uploads/evil.so" or
// "BASH_ENV=/tmp/uploads/evil.sh" makes the new process load or run code chosen by the client.
@Path("/reports")
public class CommandEnvironment {

  private static final String GENERATOR = "/usr/bin/generate-report";

  private static final File REPORTS = new File("/var/reports");

  // the client sends whole environment entries
  @POST
  @Path("/generate")
  Process generateWithEnvironmentBad(@FormParam("env") String entry) throws IOException {
    return Runtime.getRuntime().exec(GENERATOR, new String[] {entry});
  }

  @POST
  @Path("/v2/generate")
  Process generateWithArgumentsAndEnvironmentBad(@FormParam("env") String entry)
      throws IOException {
    return Runtime.getRuntime()
        .exec(new String[] {GENERATOR, "--format", "pdf"}, new String[] {entry}, REPORTS);
  }

  // the client chooses the name of a variable
  @POST
  @Path("/v3/generate")
  Process generateWithVariableBad(@FormParam("name") String name, @FormParam("value") String value)
      throws IOException {
    return Runtime.getRuntime().exec(GENERATOR, new String[] {name + "=" + value});
  }

  @POST
  @Path("/v4/generate")
  Process generateWithFixedEnvironmentOk() throws IOException {
    return Runtime.getRuntime().exec(GENERATOR, new String[] {"LANG=C"});
  }

  // The client only chooses the value of a variable that the generator reads as data, such as the
  // title of the report. Pulse does not distinguish the name of the variable from its value.
  @POST
  @Path("/v5/generate")
  Process FP_generateWithTitleOk(@FormParam("title") String title) throws IOException {
    return Runtime.getRuntime().exec(GENERATOR, new String[] {"REPORT_TITLE=" + title});
  }

  // `ProcessBuilder.environment()` returns a map that the code fills, and `Map.put` is not a sink.
  @POST
  @Path("/v6/generate")
  Process FN_generateWithProcessBuilderEnvironmentBad(
      @FormParam("name") String name, @FormParam("value") String value) throws IOException {
    ProcessBuilder builder = new ProcessBuilder(GENERATOR);
    builder.environment().put(name, value);
    return builder.start();
  }
}
