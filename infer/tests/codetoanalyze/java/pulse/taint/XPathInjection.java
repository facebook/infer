/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

// A directory of users stored as an XML document and queried with XPath
@Path("/directory")
public class XPathInjection {

  private Document users;

  private static XPath newXPath() {
    return XPathFactory.newInstance().newXPath();
  }

  // the name can close the quotes and change the query, e.g. "' or '1'='1" returns every email
  @GET
  @Path("/email")
  String emailByNameBad(@QueryParam("name") String name) throws Exception {
    return newXPath().evaluate("//user[@name='" + name + "']/email", users);
  }

  @GET
  @Path("/users")
  NodeList usersByRoleBad(@QueryParam("role") String role) throws Exception {
    String query = "//user[@role='" + role + "']";
    return (NodeList) newXPath().evaluate(query, users, XPathConstants.NODESET);
  }

  @GET
  @Path("/legacy/email")
  String emailByNameInFileBad(@QueryParam("name") String name) throws Exception {
    return newXPath()
        .evaluate("//user[@name='" + name + "']/email", new InputSource("users.xml"));
  }

  // the client sends the whole query
  @GET
  @Path("/search")
  NodeList searchBad(@QueryParam("query") String query) throws Exception {
    return newXPath().evaluateExpression(query, users, NodeList.class);
  }

  @POST
  @Path("/report")
  String reportBad(@FormParam("filter") String filter) throws Exception {
    XPathExpression expression = newXPath().compile("//user[" + filter + "]/email");
    return expression.evaluate(users);
  }

  @GET
  @Path("/phone")
  String phoneByNameBad(@QueryParam("name") String name) throws Exception {
    String query = String.format("//user[@name='%s']/phone", name);
    return newXPath().evaluate(query, users);
  }

  @GET
  @Path("/admin/email")
  String adminEmailOk() throws Exception {
    return newXPath().evaluate("//user[@name='admin']/email", users);
  }

  // the name is bound to the $name variable of the query, so it cannot change the query
  @GET
  @Path("/v2/email")
  String emailByNameWithVariableOk(@QueryParam("name") String name) throws Exception {
    XPath xpath = newXPath();
    xpath.setXPathVariableResolver(
        (QName variable) -> variable.getLocalPart().equals("name") ? name : null);
    return xpath.evaluate("//user[@name=$name]/email", users);
  }

  // The check only lets letters and digits through, which cannot change the query. Pulse does not
  // take the result of the check into account.
  @GET
  @Path("/v3/email")
  String FP_emailByAllowListedNameOk(@QueryParam("name") String name) throws Exception {
    if (!name.matches("[A-Za-z0-9]+")) {
      throw new IllegalArgumentException("invalid user name");
    }
    return newXPath().evaluate("//user[@name='" + name + "']/email", users);
  }

  // interprocedural

  private String findEmail(String name) throws Exception {
    return newXPath().evaluate("//user[@name='" + name + "']/email", users);
  }

  @GET
  @Path("/v4/email")
  String emailByNameViaHelperBad(@QueryParam("name") String name) throws Exception {
    return findEmail(name);
  }

  @GET
  @Path("/v4/admin/email")
  String adminEmailViaHelperOk() throws Exception {
    return findEmail("admin");
  }
}
