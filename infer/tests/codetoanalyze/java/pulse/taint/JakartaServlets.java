/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;

// Servlets of the Jakarta API (jakarta.servlet), the successor of javax.servlet. The request
// handlers are inherited from HttpServlet, so the class names say whether they are expected to
// report. The stubs are in the stubs/jakarta/ directory.

class JakartaServlets {}

class AvatarServletBad extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    new File("/var/avatars", request.getParameter("name")).delete();
  }
}

// since Servlet 6.1, HttpServlet dispatches the PATCH requests to doPatch
class ProfilePatchServletBad extends HttpServlet {

  @Override
  protected void doPatch(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    Runtime.getRuntime().exec("/usr/bin/update-profile " + request.getParameter("user"));
  }
}

// since Servlet 6.2, HttpServlet dispatches the QUERY requests (a GET with a body) to doQuery
class ReportQueryServletBad extends HttpServlet {

  @Override
  protected void doQuery(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    new File("/var/reports", request.getParameter("report")).delete();
  }
}

class ProfilePatchServletOk extends HttpServlet {

  @Override
  protected void doPatch(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    Runtime.getRuntime().exec("/usr/bin/update-profile --all");
  }
}
