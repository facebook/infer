/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.Set;
import javax.imageio.ImageIO;

public class ServerSideRequestForgery {

  private static final Set<String> ALLOWED_HOSTS = Set.of("api.example.com");

  private static String userControlledString() {
    return InferTaint.inferSecretStringSource();
  }

  // java.net.URL

  InputStream urlOpenStreamBad() throws Exception {
    URL url = new URL(userControlledString());
    return url.openStream();
  }

  InputStream urlOpenConnectionBad() throws Exception {
    URLConnection connection = new URL(userControlledString()).openConnection();
    return connection.getInputStream();
  }

  Object urlGetContentBad() throws Exception {
    URL url = new URL(userControlledString());
    return url.getContent();
  }

  InputStream urlHostBad() throws Exception {
    URL url = new URL("https", userControlledString(), "/index.html");
    return url.openStream();
  }

  InputStream urlHostAndPortBad() throws Exception {
    URL url = new URL("https", userControlledString(), 8443, "/index.html");
    return url.openStream();
  }

  InputStream urlFileBad() throws Exception {
    URL url = new URL("https", "api.example.com", userControlledString());
    return url.openStream();
  }

  // the user-controlled string can redirect the request, e.g. with "@evil.com"
  InputStream urlWithConstantPrefixBad() throws Exception {
    URL url = new URL("https://api.example.com" + userControlledString());
    return url.openStream();
  }

  int urlNotRequestedOk() throws Exception {
    URL url = new URL(userControlledString());
    return url.getPort();
  }

  InputStream constantUrlOk() throws Exception {
    URL url = new URL("https://api.example.com/index.html");
    return url.openStream();
  }

  // java.net.URI

  InputStream uriToUrlBad() throws Exception {
    URI uri = URI.create(userControlledString());
    return uri.toURL().openStream();
  }

  InputStream uriHostBad() throws Exception {
    URI uri = new URI("https", userControlledString(), "/index.html", null);
    return uri.toURL().openStream();
  }

  InputStream uriPathBad() throws Exception {
    URI uri = new URI("https", "api.example.com", userControlledString(), null);
    return uri.toURL().openStream();
  }

  // The fragment is never sent to the server. Pulse propagates the taint from all the arguments of
  // the URI constructor and does not distinguish the components of the URI.
  InputStream FP_uriFragmentOk() throws Exception {
    URI uri = new URI("https", "api.example.com", "/index.html", userControlledString());
    return uri.toURL().openStream();
  }

  // the user-controlled string can only change the value of the query parameter after encoding
  InputStream encodedQueryParameterOk() throws Exception {
    String query = URLEncoder.encode(userControlledString(), StandardCharsets.UTF_8);
    URI uri = URI.create("https://api.example.com/search?q=" + query);
    return uri.toURL().openStream();
  }

  // URLEncoder.encode does not encode letters, digits, '.' and '-', so the user-controlled string
  // still chooses the host. URLEncoder.encode is modeled as a sanitizer regardless of the part of
  // the URL the encoded value ends up in.
  InputStream FN_encodedHostBad() throws Exception {
    String host = URLEncoder.encode(userControlledString(), StandardCharsets.UTF_8);
    URI uri = URI.create("https://" + host + "/index.html");
    return uri.toURL().openStream();
  }

  // Checking the host against an allow list is not understood as a sanitizer.
  InputStream FP_allowListCheckOk() throws Exception {
    URI uri = URI.create(userControlledString());
    if (!ALLOWED_HOSTS.contains(uri.getHost())) {
      throw new SecurityException("Host not allowed");
    }
    return uri.toURL().openStream();
  }

  // javax.imageio.ImageIO

  BufferedImage imageIOReadUrlBad() throws Exception {
    URL url = new URL(userControlledString());
    return ImageIO.read(url);
  }

  // the image is read from the user-controlled data itself, no request is made
  BufferedImage imageIOReadInputStreamOk() throws Exception {
    InputStream in = new ByteArrayInputStream(userControlledString().getBytes());
    return ImageIO.read(in);
  }

  // java.net.http

  HttpResponse<String> httpRequestNewBuilderBad() throws Exception {
    HttpRequest request = HttpRequest.newBuilder(URI.create(userControlledString())).build();
    return HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
  }

  HttpResponse<String> httpRequestBuilderUriBad() throws Exception {
    HttpRequest request = HttpRequest.newBuilder().uri(URI.create(userControlledString())).build();
    return HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
  }

  HttpResponse<String> httpRequestConstantUriOk() throws Exception {
    HttpRequest request =
        HttpRequest.newBuilder()
            .uri(URI.create("https://api.example.com/index.html"))
            .header("X-User", userControlledString())
            .build();
    return HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
  }

  // interprocedural

  private static InputStream fetch(String location) throws Exception {
    return URI.create(location).toURL().openStream();
  }

  InputStream fetchUserControlledLocationBad() throws Exception {
    return fetch(userControlledString());
  }

  InputStream fetchConstantLocationOk() throws Exception {
    return fetch("https://api.example.com/index.html");
  }
}
