package org.mozilla.javascript;

public interface Script {
  Object exec(Context cx, Scriptable scope);
}
