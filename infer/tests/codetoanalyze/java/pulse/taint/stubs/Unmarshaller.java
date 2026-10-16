package javax.xml.bind;

import java.io.InputStream;
import javax.xml.stream.XMLStreamReader;

public interface Unmarshaller {
  Object unmarshal(InputStream is);

  Object unmarshal(XMLStreamReader reader);
}
