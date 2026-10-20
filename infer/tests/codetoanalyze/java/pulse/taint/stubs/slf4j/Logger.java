package org.slf4j;

public interface Logger {
  void info(String msg);

  void info(String format, Object arg);

  void warn(String format, Object arg);

  void error(String format, Object... arguments);
}
