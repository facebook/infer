package org.apache.logging.log4j;

public class LogManager {
  public static native Logger getLogger(Class<?> clazz);
}
