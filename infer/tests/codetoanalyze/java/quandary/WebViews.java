/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.quandary;

import android.content.Context;

import android.content.Intent;
import android.net.Uri;
import android.webkit.JavascriptInterface;
import android.webkit.ValueCallback;
import android.webkit.WebMessage;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.webkit.WebChromeClient;

import com.facebook.infer.builtins.InferTaint;

public class WebViews {

  void callWebviewSinks(WebView webview) {
    String stringSource = (String) InferTaint.inferSecretSource();

    webview.addJavascriptInterface(new Object(), stringSource);
    webview.evaluateJavascript(stringSource, null);
    webview.loadData(stringSource, "", "");
    webview.loadDataWithBaseURL("", stringSource, "", "", "");
    webview.loadUrl(stringSource); // should have 5 reports
    webview.postWebMessage(null, (Uri) InferTaint.inferSecretSource());
  }

  void callWebviewClientSinks(WebView webview, WebViewClient client) {
    String stringSource = (String) InferTaint.inferSecretSource();

    client.onLoadResource(webview, stringSource);
    client.shouldInterceptRequest(webview, stringSource);
    client.shouldOverrideUrlLoading(webview, stringSource); // should have 3 reports
  }

  void callWebviewChromeClientSinks(WebView webview, WebChromeClient client) {
    String stringSource = (String) InferTaint.inferSecretSource();

    client.onJsAlert(webview, stringSource, "", null);
    client.onJsBeforeUnload(webview, stringSource, "", null);
    client.onJsConfirm(webview, stringSource, "", null);
    client.onJsPrompt(webview, stringSource, "", "", null); // should have 4 reports
  }

  // make sure all of the rules apply to subclasses as well
  class MyWebView extends WebView {
    public MyWebView(Context c) {
      super(c);
    }
  }

  class MyWebViewClient extends WebViewClient {
  }

  class MyWebChromeClient extends WebChromeClient {
  }

  void callWebviewSubclassSinks(
    MyWebView webview, MyWebViewClient client, MyWebChromeClient chromeClient) {

    String stringSource = (String) InferTaint.inferSecretSource();
    webview.evaluateJavascript(stringSource, null);
    client.onLoadResource(webview, stringSource);
    chromeClient.onJsAlert(webview, stringSource, "", null); // should have 3 reports
  }

  class JsObject {
    @JavascriptInterface
    Object returnSource() {
      return InferTaint.inferSecretSource();
    }
  }

  // in order to get this, we have to understand that addJavaScriptInterface can evaluate the
  // JsObject.returnSource method
  void FN_addJavascriptInterface(MyWebView webview) {
    // should warn here
    webview.addJavascriptInterface(new JsObject(), "injectedObject");
  }

}
