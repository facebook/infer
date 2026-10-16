package org.jdom2.input;

import java.io.InputStream;
import org.jdom2.Document;

public class SAXBuilder {
  public native Document build(InputStream in);
}
