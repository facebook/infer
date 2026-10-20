package org.apache.logging.log4j;

public interface Logger {
  void info(String message);

  void warn(String message, Object p0);

  void error(String message);
}
