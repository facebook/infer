/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.UUID;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

// A document service storing the files of its users on disk. A file name or path such as
// "../../etc/passwd" lets the client read or write files outside of the directories of the service.
@Path("/documents")
public class PathTraversal {

  private static final String DOCUMENTS = "/var/documents";

  private static final java.nio.file.Path TEMPLATES = Paths.get("/var/templates");

  // java.io

  @GET
  @Path("/{name}")
  byte[] documentBad(@PathParam("name") String name) throws IOException {
    File file = new File(DOCUMENTS, name);
    return java.nio.file.Files.readAllBytes(file.toPath());
  }

  @GET
  @Path("/profile")
  byte[] profileBad(@QueryParam("user") String user) throws IOException {
    File file = new File(user, "profile.json");
    return java.nio.file.Files.readAllBytes(file.toPath());
  }

  @GET
  @Path("/file")
  File fileFromUriBad(@QueryParam("uri") String uri) {
    return new File(URI.create(uri));
  }

  @GET
  @Path("/raw")
  InputStream rawFileBad(@QueryParam("path") String path) throws IOException {
    return new FileInputStream(path);
  }

  @GET
  @Path("/notes")
  String firstLineOfNoteBad(@QueryParam("name") String name) throws IOException {
    try (BufferedReader reader = new BufferedReader(new FileReader(DOCUMENTS + "/" + name))) {
      return reader.readLine();
    }
  }

  @POST
  @Path("/upload")
  void uploadBad(@FormParam("name") String name, @FormParam("content") String content)
      throws IOException {
    try (FileOutputStream out = new FileOutputStream(DOCUMENTS + "/" + name)) {
      out.write(content.getBytes(StandardCharsets.UTF_8));
    }
  }

  @POST
  @Path("/comments")
  void appendCommentBad(
      @FormParam("document") String document, @FormParam("comment") String comment)
      throws IOException {
    try (FileWriter writer = new FileWriter(DOCUMENTS + "/" + document + ".comments", true)) {
      writer.write(comment);
    }
  }

  @GET
  @Path("/header")
  int headerByteBad(@QueryParam("name") String name) throws IOException {
    try (RandomAccessFile file = new RandomAccessFile(DOCUMENTS + "/" + name, "r")) {
      return file.read();
    }
  }

  // java.nio.file

  @GET
  @Path("/v2/{name}")
  byte[] documentWithPathsGetBad(@PathParam("name") String name) throws IOException {
    return java.nio.file.Files.readAllBytes(Paths.get(DOCUMENTS, name));
  }

  @GET
  @Path("/v3/{name}")
  byte[] documentWithPathOfBad(@PathParam("name") String name) throws IOException {
    return java.nio.file.Files.readAllBytes(java.nio.file.Path.of(DOCUMENTS, name));
  }

  @GET
  @Path("/templates/{name}")
  String templateBad(@PathParam("name") String name) throws IOException {
    return java.nio.file.Files.readString(TEMPLATES.resolve(name));
  }

  @GET
  @Path("/templates/{name}/preview")
  String templatePreviewBad(@PathParam("name") String name) throws IOException {
    java.nio.file.Path template = TEMPLATES.resolve("default.html");
    return java.nio.file.Files.readString(template.resolveSibling(name));
  }

  // interprocedural

  private static byte[] readDocument(String name) throws IOException {
    return java.nio.file.Files.readAllBytes(Paths.get(DOCUMENTS, name));
  }

  @GET
  @Path("/v4/{name}")
  byte[] documentViaHelperBad(@PathParam("name") String name) throws IOException {
    return readDocument(name);
  }

  @GET
  @Path("/terms")
  byte[] termsViaHelperOk() throws IOException {
    return readDocument("terms.txt");
  }

  // no user-controlled path

  @GET
  @Path("/readme")
  InputStream readmeOk() throws IOException {
    return new FileInputStream(DOCUMENTS + "/README");
  }

  // the client controls what is written to the file, not which file is written
  @POST
  @Path("/feedback")
  void feedbackOk(@FormParam("content") String content) throws IOException {
    try (FileOutputStream out = new FileOutputStream(DOCUMENTS + "/feedback.txt", true)) {
      out.write(content.getBytes(StandardCharsets.UTF_8));
    }
  }

  // the client sends the content of the document itself, which is not read from the disk
  @POST
  @Path("/preview")
  InputStream previewOk(@FormParam("content") String content) {
    return new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8));
  }

  // The checks below keep the file in the documents directory. Pulse does not take the result of
  // the check into account.

  @GET
  @Path("/v5/{name}")
  byte[] FP_documentInDocumentsDirectoryOk(@PathParam("name") String name) throws IOException {
    java.nio.file.Path documents = Paths.get(DOCUMENTS);
    java.nio.file.Path document = documents.resolve(name).normalize();
    if (!document.startsWith(documents)) {
      throw new SecurityException("invalid document name");
    }
    return java.nio.file.Files.readAllBytes(document);
  }

  // A UUID only contains hexadecimal digits and dashes. Pulse propagates the taint through the
  // conversion to and from a UUID.
  @GET
  @Path("/v6/{id}")
  byte[] FP_documentByIdOk(@PathParam("id") String id) throws IOException {
    UUID uuid = UUID.fromString(id);
    return java.nio.file.Files.readAllBytes(Paths.get(DOCUMENTS, uuid.toString()));
  }
}
