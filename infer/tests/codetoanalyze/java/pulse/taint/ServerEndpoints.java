/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

// The parameters of the methods handling HTTP requests are controlled by whoever sends the request.
// The stubs of the web frameworks are in the stubs/ directory.

class ServerEndpoints {}

// JAX-RS

@Path("/users")
class UserResource {

  Statement statement;

  @GET
  @Path("/search")
  ResultSet searchByNameBad(@QueryParam("name") String name) throws SQLException {
    return statement.executeQuery("SELECT * FROM users WHERE name = '" + name + "'");
  }

  @POST
  @Path("/ping")
  Process pingHostBad(@FormParam("host") String host) throws IOException {
    return Runtime.getRuntime().exec("ping -c 1 " + host);
  }

  @GET
  @Path("/admins")
  ResultSet listAdminsOk() throws SQLException {
    return statement.executeQuery("SELECT * FROM users WHERE role = 'admin'");
  }

  // not a request handler: its parameter is not controlled by the client
  ResultSet searchByNameNotEndpointOk(String name) throws SQLException {
    return statement.executeQuery("SELECT * FROM users WHERE name = '" + name + "'");
  }
}

// Spring MVC

@RestController
class PreviewController {

  @GetMapping("/preview")
  InputStream previewUrlBad(@RequestParam("url") String url) throws IOException {
    return new URL(url).openStream();
  }

  @PostMapping("/avatar")
  File avatarFileBad(@RequestParam("name") String name) {
    return new File("/var/avatars", name);
  }

  @GetMapping("/status")
  InputStream statusOk() throws IOException {
    return new URL("https://status.example.com/health").openStream();
  }
}

// Servlets: the request handlers are inherited from HttpServlet, so the class names say whether
// they are expected to report

class DownloadServletBad extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    File file = new File("/var/downloads", request.getParameter("file"));
    response.setStatus(file.exists() ? 200 : 404);
  }
}

class HeaderServletBad extends HttpServlet {

  Statement statement;

  @Override
  protected void doPost(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    try {
      statement.executeQuery(
          "SELECT * FROM sessions WHERE token = '" + request.getHeader("X-Token") + "'");
    } catch (SQLException e) {
      response.setStatus(500);
    }
  }
}

class HealthServletOk extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    File file = new File("/var/run/health");
    response.setStatus(file.exists() ? 200 : 503);
  }
}
