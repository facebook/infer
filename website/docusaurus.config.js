/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

module.exports = {
  title: "Infer",
  tagline:
    "A tool to detect bugs in Java and C/C++/Objective-C code before it ships",
  url: "https://fbinfer.com",
  baseUrl: "/",
  favicon: "img/favicon.png",
  organizationName: "facebook",
  projectName: "infer",
  themeConfig: {
    announcementBar: {
      id: 'support_ukraine',
      content:
        'Support Ukraine 🇺🇦 <a target="_blank" rel="noopener noreferrer" href="https://opensource.facebook.com/support-ukraine"> Help Provide Humanitarian Aid to Ukraine</a>.',
      backgroundColor: '#20232a',
      textColor: '#fff',
      isCloseable: false,
    },
    algolia: {
      appId: "WXX0LT0N1W",
      apiKey: "acb23fcd08afea7e89732c52fbd86974",
      indexName: "fbinfer",
    },
    navbar: {
      title: "Infer",
      logo: { alt: "Infer Logo", src: "img/logo.png" },
      items: [
        { label: "Docs", to: "docs/getting-started", position: "right" },
        { label: "Support", to: "docs/support", position: "right" },
        { label: "Blog", to: "blog", position: "right" },
        {
          href: "https://twitter.com/fbinfer",
          label: "Twitter",
          position: "right",
        },
        {
          href: "https://www.facebook.com/inferstaticanalyzer",
          label: "Facebook",
          position: "right",
        },
        {
          href: "https://github.com/facebook/infer",
          label: "GitHub",
          position: "right",
        },
      ],
    },
    footer: {
      style: "light",
      links: [
        {
          title: "Docs",
          items: [
            { label: "Quick Start", to: "docs/getting-started" },
            { label: "User Guide", to: "docs/infer-workflow" },
            { label: "Foundations", to: "docs/about-Infer" },
            { label: "Bug Types Reference", to: "docs/all-issue-types" },
            { label: "Contribute", to: "docs/absint-framework" },
          ],
        },
        {
          title: "Community",
          items: [{ label: "Support", to: "docs/support" }],
        },
        {
          title: "Social",
          items: [
            { label: "Blog", to: "blog" },
            { label: "GitHub", href: "https://github.com/facebook/infer" },
            { label: "Twitter", href: "https://twitter.com/fbinfer" },
            {
              label: "Facebook",
              href: "https://www.facebook.com/inferstaticanalyzer",
            },
          ],
        },
        {
          title: "Legal",
          // Please do not remove the privacy and terms, it's a legal requirement.
          items: [
            {
              label: "Privacy",
              href: "https://opensource.facebook.com/legal/privacy/",
              target: "_blank",
              rel: "noreferrer noopener",
            },
            {
              label: "Terms",
              href: "https://opensource.facebook.com/legal/terms/",
              target: "_blank",
              rel: "noreferrer noopener",
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Facebook, Inc. Built with Docusaurus.`,
    },
    prism: {
      additionalLanguages: ["java"],
    },
  },
  presets: [
    [
      "@docusaurus/preset-classic",
      {
        docs: { sidebarPath: require.resolve("./sidebars.js") },
        theme: { customCss: require.resolve("./src/css/custom.css") },
        gtag: {
          trackingID: 'G-6T8Z3855RZ',
          anonymizeIP: true,
        },
      },
    ],
  ],
};
