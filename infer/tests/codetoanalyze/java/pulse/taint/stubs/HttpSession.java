package javax.servlet.http;

public interface HttpSession {
  Object getAttribute(String name);

  void setAttribute(String name, Object value);

  void putValue(String name, Object value);
}
