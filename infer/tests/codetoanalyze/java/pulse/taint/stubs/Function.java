package org.mozilla.javascript;

public interface Function extends Scriptable {
  Object call(Context cx, Scriptable scope, Scriptable thisObj, Object[] args);
}
