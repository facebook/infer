/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.BasicAttributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import javax.naming.ldap.LdapContext;
import javax.naming.ldap.LdapName;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;

// An employee directory backed by an LDAP server. A user name such as "*" or "*)(uid=*" changes
// the meaning of the search filter, e.g. to list every entry or to bypass a password check.
@Path("/directory")
public class LdapInjection {

  private static final String PEOPLE = "ou=people,dc=example,dc=com";

  private DirContext directory;

  private LdapContext ldap;

  private final SearchControls controls = new SearchControls();

  @GET
  @Path("/people")
  NamingEnumeration<SearchResult> personByUidBad(@QueryParam("uid") String uid)
      throws NamingException {
    return directory.search(PEOPLE, "(uid=" + uid + ")", controls);
  }

  @POST
  @Path("/login")
  boolean loginBad(@FormParam("user") String user, @FormParam("password") String password)
      throws NamingException {
    String filter = "(&(uid=" + user + ")(userPassword=" + password + "))";
    return directory.search(PEOPLE, filter, controls).hasMore();
  }

  @GET
  @Path("/people/by-mail")
  NamingEnumeration<SearchResult> personByMailBad(@QueryParam("mail") String mail)
      throws NamingException {
    String filter = String.format("(&(objectClass=person)(mail=%s))", mail);
    return directory.search(PEOPLE, filter, controls);
  }

  // the client sends the whole filter
  @GET
  @Path("/search")
  NamingEnumeration<SearchResult> searchBad(@QueryParam("filter") String filter)
      throws NamingException {
    return directory.search(PEOPLE, filter, new Object[0], controls);
  }

  // the search base is built from the client's input: "dc=example,dc=com" searches the whole tree
  @GET
  @Path("/departments")
  NamingEnumeration<SearchResult> departmentMembersBad(@QueryParam("department") String department)
      throws NamingException {
    return directory.search(
        "ou=" + department + ",dc=example,dc=com", "(objectClass=person)", controls);
  }

  @GET
  @Path("/v2/departments")
  NamingEnumeration<SearchResult> departmentMembersWithNameBad(
      @QueryParam("department") String department) throws NamingException {
    LdapName base = new LdapName("ou=" + department + ",dc=example,dc=com");
    return directory.search(base, "(objectClass=person)", controls);
  }

  @GET
  @Path("/v2/people")
  NamingEnumeration<SearchResult> personByUidWithLdapContextBad(@QueryParam("uid") String uid)
      throws NamingException {
    return ldap.search(PEOPLE, "(uid=" + uid + ")", controls);
  }

  @GET
  @Path("/admins")
  NamingEnumeration<SearchResult> adminsOk() throws NamingException {
    return directory.search(PEOPLE, "(memberOf=cn=admins,ou=groups,dc=example,dc=com)", controls);
  }

  // the uid is given as an argument of the filter, which the directory escapes
  @GET
  @Path("/v3/people")
  NamingEnumeration<SearchResult> personByUidWithFilterArgumentOk(@QueryParam("uid") String uid)
      throws NamingException {
    return directory.search(PEOPLE, "(uid={0})", new Object[] {uid}, controls);
  }

  // the uid is the value of an attribute to match, not part of a filter
  @GET
  @Path("/v4/people")
  NamingEnumeration<SearchResult> personByUidAttributeOk(@QueryParam("uid") String uid)
      throws NamingException {
    return directory.search(PEOPLE, new BasicAttributes("uid", uid));
  }

  // The check only lets letters and digits through, which cannot change the filter. Pulse does not
  // take the result of the check into account.
  @GET
  @Path("/v5/people")
  NamingEnumeration<SearchResult> FP_personByAllowListedUidOk(@QueryParam("uid") String uid)
      throws NamingException {
    if (!uid.matches("[A-Za-z0-9]+")) {
      throw new IllegalArgumentException("invalid user id");
    }
    return directory.search(PEOPLE, "(uid=" + uid + ")", controls);
  }

  // interprocedural

  private NamingEnumeration<SearchResult> findPerson(String uid) throws NamingException {
    return directory.search(PEOPLE, "(uid=" + uid + ")", controls);
  }

  @GET
  @Path("/v6/people")
  NamingEnumeration<SearchResult> personViaHelperBad(@QueryParam("uid") String uid)
      throws NamingException {
    return findPerson(uid);
  }

  @GET
  @Path("/me")
  NamingEnumeration<SearchResult> serviceAccountViaHelperOk() throws NamingException {
    return findPerson("service-directory");
  }
}
