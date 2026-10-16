package org.simpleframework.xml.core;

public class Persister {
  public native <T> T read(Class<? extends T> type, String source);
}
