package javax.servlet.http;

import java.io.IOException;
import java.io.PrintWriter;

public interface HttpServletResponse {
  void setStatus(int status);

  PrintWriter getWriter() throws IOException;
}
