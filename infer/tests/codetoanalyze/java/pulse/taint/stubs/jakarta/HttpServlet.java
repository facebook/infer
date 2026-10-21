package jakarta.servlet.http;

import jakarta.servlet.ServletException;
import java.io.IOException;

public abstract class HttpServlet {
  protected void doGet(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {}

  protected void doPatch(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {}

  protected void doQuery(HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException {}
}
