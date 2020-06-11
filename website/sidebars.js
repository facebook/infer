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
      "checkers",
      "eradicate",
      "linters",
      "racerd",
      "experimental-checkers",
      "advanced-features",
      "adding-models",
      "man-pages",
    ],
    Foundations: [
      "about-Infer",
      "separation-logic-and-bi-abduction",
      "limitations",
    ],
    "Analyses and Issue Types": checkers.doc_entries,
    Contribute: ["absint-framework", "adding-checkers", "internal-API"],
    Versions: ["versions"],
  },
};
