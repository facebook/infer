package javax.xml.bind;

public abstract class JAXBContext {
  public static native JAXBContext newInstance(Class<?>... classesToBeBound);

  public abstract Unmarshaller createUnmarshaller();
}
