/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import React, { useState, useEffect } from 'react';

export default function HtmlWrap({ url }) {
  const [html, setHTML] = useState({__html: ''});

  useEffect(() => {
    fetch(url)
    .then(response => response.text())
    .then(html => setHTML({__html: html}))
  }, [url]);

  return (
    <div dangerouslySetInnerHTML={html}></div>
  );
}
