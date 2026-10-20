package org.slf4j;

public final class LoggerFactory {
  public static native Logger getLogger(Class<?> clazz);
}
