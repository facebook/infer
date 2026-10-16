/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.File;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamReader;
import javax.xml.xpath.XPathFactory;
import org.simpleframework.xml.core.Persister;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

// A service importing the orders that its clients send as XML documents. With the default settings
// of the parsers, a document declaring an external entity such as
// <!DOCTYPE order [<!ENTITY e SYSTEM "file:///etc/passwd">]> makes the server read its own files or
// send requests to other hosts.
@Path("/orders")
public class XmlExternalEntityInjection {

  static class Order {}

  private static DocumentBuilder newDocumentBuilder() throws Exception {
    return DocumentBuilderFactory.newInstance().newDocumentBuilder();
  }

  // DOM

  @POST
  @Path("/import")
  Document importOrderBad(InputStream body) throws Exception {
    return newDocumentBuilder().parse(body);
  }

  @POST
  @Path("/v2/import")
  Document importOrderFromReaderBad(Reader body) throws Exception {
    return newDocumentBuilder().parse(new InputSource(body));
  }

  // The taint of the string is lost when the `StringReader` is wrapped in an `InputSource`.
  @POST
  @Path("/v2/import/string")
  Document FN_importOrderFromStringBad(String xml) throws Exception {
    try (StringReader reader = new StringReader(xml)) {
      return newDocumentBuilder().parse(new InputSource(reader));
    }
  }

  // the client chooses the document to fetch and parse
  @GET
  @Path("/remote")
  Document importRemoteOrderBad(@QueryParam("url") String url) throws Exception {
    return newDocumentBuilder().parse(url);
  }

  // SAX

  @POST
  @Path("/v3/import")
  void validateOrderBad(InputStream body) throws Exception {
    SAXParserFactory.newInstance().newSAXParser().parse(body, new DefaultHandler());
  }

  @POST
  @Path("/v4/import")
  void validateOrderWithXmlReaderBad(InputStream body) throws Exception {
    SAXParserFactory.newInstance().newSAXParser().getXMLReader().parse(new InputSource(body));
  }

  // StAX

  @POST
  @Path("/v5/import")
  XMLStreamReader streamOrderBad(InputStream body) throws Exception {
    return XMLInputFactory.newInstance().createXMLStreamReader(body);
  }

  @POST
  @Path("/v6/import")
  XMLEventReader streamOrderEventsBad(String xml) throws Exception {
    return XMLInputFactory.newInstance().createXMLEventReader(new StringReader(xml));
  }

  // XPath parses the document given as an `InputSource`

  @POST
  @Path("/customer")
  String customerOfOrderBad(InputStream body) throws Exception {
    InputSource order = new InputSource(body);
    return XPathFactory.newInstance().newXPath().evaluate("/order/customer", order);
  }

  @POST
  @Path("/v2/customer")
  String customerOfOrderWithExpressionBad(InputStream body) throws Exception {
    InputSource order = new InputSource(body);
    return XPathFactory.newInstance().newXPath().compile("/order/customer").evaluate(order);
  }

  // data binding libraries

  @POST
  @Path("/v7/import")
  Object bindOrderBad(InputStream body) throws Exception {
    return JAXBContext.newInstance(Order.class).createUnmarshaller().unmarshal(body);
  }

  @POST
  @Path("/v8/import")
  org.jdom2.Document importOrderWithJdomBad(InputStream body) throws Exception {
    return new org.jdom2.input.SAXBuilder().build(body);
  }

  @POST
  @Path("/v9/import")
  Order bindOrderWithSimpleXmlBad(String xml) throws Exception {
    return new Persister().read(Order.class, xml);
  }

  // interprocedural

  private static Document parseOrder(InputSource order) throws Exception {
    return newDocumentBuilder().parse(order);
  }

  @POST
  @Path("/v10/import")
  Document importOrderViaHelperBad(InputStream body) throws Exception {
    return parseOrder(new InputSource(body));
  }

  @GET
  @Path("/sample")
  Document sampleOrderViaHelperOk() throws Exception {
    return parseOrder(new InputSource("/var/orders/sample.xml"));
  }

  // no XML from the client is parsed

  @GET
  @Path("/schema")
  Document schemaOk() throws Exception {
    return newDocumentBuilder().parse(new File("/var/orders/schema.xsd"));
  }

  // the client's data is the text of an element, which is escaped when the document is written
  @POST
  @Path("/note")
  Document noteOk(String note) throws Exception {
    Document document = newDocumentBuilder().newDocument();
    Element element = document.createElement("note");
    element.setTextContent(note);
    document.appendChild(element);
    return document;
  }

  // The parsers below reject the document type declarations, and with them the external
  // entities. Pulse does not take the configuration of the parser into account.

  @POST
  @Path("/v11/import")
  Document FP_importOrderWithoutDoctypeOk(InputStream body) throws Exception {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
    return factory.newDocumentBuilder().parse(body);
  }

  @POST
  @Path("/v12/import")
  XMLStreamReader FP_streamOrderWithoutDtdOk(InputStream body) throws Exception {
    XMLInputFactory factory = XMLInputFactory.newInstance();
    factory.setProperty(XMLInputFactory.SUPPORT_DTD, false);
    return factory.createXMLStreamReader(body);
  }

  // sanitizers

  // the document is checked by a validator declared as a sanitizer (see Validators.java)
  @POST
  @Path("/v13/import")
  Order bindValidatedOrderOk(String xml) throws Exception {
    return new Persister().read(Order.class, Validators.requireNoDoctype(xml));
  }

  // the parser of the checks above, in a helper declared as a sanitizer
  static Document parseWithoutDoctype(InputStream in) throws Exception {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
    return factory.newDocumentBuilder().parse(in);
  }

  @POST
  @Path("/v14/import")
  Document importOrderWithSafeParserOk(InputStream body) throws Exception {
    return parseWithoutDoctype(body);
  }
}
