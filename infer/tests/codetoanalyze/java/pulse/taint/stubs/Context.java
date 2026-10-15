package org.mozilla.javascript;

import java.io.Reader;

public class Context {
  public static native Context enter();

  public static native void exit();

  public native ScriptableObject initStandardObjects();

  public native ScriptableObject initSafeStandardObjects();

  public native Object evaluateString(
      Scriptable scope, String source, String sourceName, int lineno, Object securityDomain);

  public native Object evaluateReader(
      Scriptable scope, Reader in, String sourceName, int lineno, Object securityDomain);

  public native Script compileString(
      String source, String sourceName, int lineno, Object securityDomain);

  public native Script compileReader(
      Reader in, String sourceName, int lineno, Object securityDomain);

  public native Function compileFunction(
      Scriptable scope, String source, String sourceName, int lineno, Object securityDomain);
}
