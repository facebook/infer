/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const checkers = require("./checkers");

module.exports = {
  docs: {
    "Quick Start": ["getting-started", "hello-world"],
    "User Guide": [
      "infer-workflow",
      "analyzing-apps-or-projects",
      "steps-for-ci",
      {"Infer Manuals": [
          "man-infer",
          "man-infer-analyze",
          "man-infer-capture",
          "man-infer-compile",
          "man-infer-debug",
          "man-infer-explore",
          "man-infer-help",
          "man-infer-report",
          "man-infer-reportdiff",
          "man-infer-run"
      ]}
    ],
    "Analyses and Issue Types": checkers.doc_entries,
    Foundations: [
      "about-Infer",
      "separation-logic-and-bi-abduction",
    ],
    Contribute: ["absint-framework", "internal-API"],
    Versions: ["versions"],
  },
};
