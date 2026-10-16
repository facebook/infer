/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.StringReader;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;
import org.apache.commons.text.StringEscapeUtils;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.Scriptable;

// A pricing service that evaluates its pricing rules with a script engine. A script sent by the
// client runs with the permissions of the service, e.g. `java.lang.Runtime.getRuntime().exec(...)`.
@Path("/pricing")
public class ScriptInjection {

  private final ScriptEngine engine = new ScriptEngineManager().getEngineByName("javascript");

  // javax.script

  @GET
  @Path("/calculate")
  Object calculateBad(@QueryParam("expression") String expression) throws ScriptException {
    return engine.eval(expression);
  }

  @GET
  @Path("/discount")
  Object discountBad(@QueryParam("customer") String customer) throws ScriptException {
    return engine.eval("discountFor('" + customer + "')");
  }

  @POST
  @Path("/rules")
  Object compileRuleBad(@FormParam("rule") String rule) throws ScriptException {
    return ((Compilable) engine).compile(rule).eval();
  }

  @GET
  @Path("/total")
  Object totalOk() throws ScriptException {
    return engine.eval("basePrice() * 1.2");
  }

  // the customer is a variable of the script, not part of its code
  @GET
  @Path("/v2/discount")
  Object discountWithBindingsOk(@QueryParam("customer") String customer) throws ScriptException {
    Bindings bindings = engine.createBindings();
    bindings.put("customer", customer);
    return engine.eval("discountFor(customer)", bindings);
  }

  // the customer is escaped to stay inside the string literal of the script
  @GET
  @Path("/v3/discount")
  Object discountWithEscapingOk(@QueryParam("customer") String customer) throws ScriptException {
    return engine.eval("discountFor('" + StringEscapeUtils.escapeEcmaScript(customer) + "')");
  }

  // Escaping only keeps a value inside a string literal: evaluated as code, the escaped expression
  // can still call anything, e.g. `java.lang.Runtime.getRuntime()`. The sanitizer does not know
  // where the escaped value ends up.
  @GET
  @Path("/v7/calculate")
  Object FN_calculateEscapedExpressionBad(@QueryParam("expression") String expression)
      throws ScriptException {
    return engine.eval(StringEscapeUtils.escapeEcmaScript(expression));
  }

  // Rhino

  @GET
  @Path("/v2/calculate")
  Object calculateWithRhinoBad(@QueryParam("expression") String expression) {
    Context cx = Context.enter();
    Scriptable scope = cx.initStandardObjects();
    return cx.evaluateString(scope, expression, "pricing", 1, null);
  }

  @POST
  @Path("/v2/rules")
  Object compileRuleWithRhinoBad(@FormParam("rule") String rule) {
    Context cx = Context.enter();
    Scriptable scope = cx.initStandardObjects();
    return cx.compileString(rule, "pricing", 1, null).exec(cx, scope);
  }

  // the client only names the script, for the error messages
  @GET
  @Path("/v2/total")
  Object totalWithRhinoOk(@QueryParam("label") String label) {
    Context cx = Context.enter();
    Scriptable scope = cx.initStandardObjects();
    return cx.evaluateString(scope, "basePrice() * 1.2", label, 1, null);
  }

  // `initSafeStandardObjects` leaves the Java classes out of the scope, so the script cannot reach
  // them. Pulse does not know which scope the script runs in.
  @GET
  @Path("/v3/calculate")
  Object FP_calculateInSafeScopeOk(@QueryParam("expression") String expression) {
    Context cx = Context.enter();
    Scriptable scope = cx.initSafeStandardObjects();
    return cx.evaluateString(scope, expression, "pricing", 1, null);
  }

  // the script is read through a reader

  @GET
  @Path("/v4/calculate")
  Object calculateFromReaderBad(@QueryParam("expression") String expression)
      throws ScriptException {
    return engine.eval(new StringReader(expression));
  }

  @GET
  @Path("/v5/calculate")
  Object calculateFromReaderWithRhinoBad(@QueryParam("expression") String expression)
      throws java.io.IOException {
    Context cx = Context.enter();
    Scriptable scope = cx.initStandardObjects();
    return cx.evaluateReader(scope, new StringReader(expression), "pricing", 1, null);
  }

  // interprocedural

  private Object evaluate(String script) throws ScriptException {
    return engine.eval(script);
  }

  @GET
  @Path("/v6/calculate")
  Object calculateViaHelperBad(@QueryParam("expression") String expression)
      throws ScriptException {
    return evaluate(expression);
  }

  @GET
  @Path("/v6/total")
  Object totalViaHelperOk() throws ScriptException {
    return evaluate("basePrice() * 1.2");
  }
}
