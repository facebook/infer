/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import Link from "@docusaurus/Link";
import useBaseUrl from "@docusaurus/useBaseUrl";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Layout from "@theme/Layout";
import classnames from "classnames";
import React from "react";
import GitHubButton from "react-github-btn";
import Script from "../components/Script";
import styles from "./styles.module.css";

const features = [
  {
    title: <>Android and Java</>,
    description: (
      <>
        Infer checks for null pointer exceptions, resource leaks, annotation
        reachability, missing lock guards, and concurrency race conditions in
        Android and Java code.
      </>
    ),
  },
  {
    title: <>C, C++, and iOS/Objective-C</>,
    description: (
      <>
        Infer checks for null pointer dereferences, memory leaks, coding
        conventions and unavailable API’s.
      </>
    ),
  },
];

const poweredByImages = [
  { url: "https://www.adacore.com", image: "/img/who/adacore.png" },
  { url: "https://aws.amazon.com", image: "/img/who/aws.svg" },
  { url: "https://facebook.com", image: "/img/who/facebook.png" },
  { url: "https://freefem.org", image: "/img/who/freefem.png" },
  { url: "https://www.instagram.com", image: "/img/who/instagram.png" },
  { url: "https://www.microsoft.com", image: "/img/who/microsoft.png" },
  { url: "http://www.mozilla.com", image: "/img/who/mozilla.png" },
  { url: "https://www.oculus.com", image: "/img/who/oculus.png" },
  { url: "https://www.sonatype.com", image: "/img/who/sonatype.png" },
  {
    url: "/blog/2016/03/17/collaboration-with-spotify",
    image: "/img/who/spotify.png",
  },
  { url: "https://tangramflex.com", image: "/img/who/tangramflex.png" },
  { url: "https://www.uber.com", image: "/img/who/uber.svg" },
  { url: "https://www.whatsapp.com", image: "/img/who/whatsapp.svg" },
];

const poweredByItems = [
  { url: "https://www.mycode.ai", name: "CodeAI" },
  { url: "http://jd.com", name: "JD.com" },
  { url: "https://www.marksandspencer.com", name: "Marks and Spencer" },
  { url: "https://moneylover.me", name: "Money Lover" },
  { url: "https://www.netcetera.com", name: "Netcetera" },
  { url: "https://www.olacabs.com", name: "OLA" },
  { url: "https://www.sky.com", name: "Sky" },
  { url: "https://tile.com", name: "Tile" },
  { url: "https://vuo.org", name: "Vuo" },
  { url: "https://wolfssl.com", name: "wolfSSL" },
];

function VideoContainer() {
  return (
    <div className="container text--center margin-bottom--xl margin-top--lg">
      <div className="row">
        <div className="col">
          <h2>Check it out in the intro video</h2>
          <div className={styles.ytVideo}>
            <iframe
              width="560"
              height="315"
              src="https://www.youtube.com/embed/swrmPTJAGqQ"
              title="Explain Like I'm 5: Infer"
              frameBorder="0"
              allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
              allowFullScreen
            />
          </div>
        </div>
      </div>
    </div>
  );
}

function Home() {
  const context = useDocusaurusContext();
  const { siteConfig = {} } = context;
  return (
    <Layout
      title="Infer Static Analyzer | Infer"
      description={`${siteConfig.tagline}`}
    >
      <header className={classnames("hero hero--primary", styles.heroBanner)}>
        <div className="container">
          <h1 className="hero__title">
            A tool to detect bugs in Java and C/C++/Objective-C code before it
            ships
          </h1>
          <p className="hero__subtitle">
            Infer is a static analysis tool - if you give Infer some Java or
            C/C++/Objective-C code it produces a list of potential bugs. Anyone
            can use Infer to intercept critical bugs before they have shipped to
            users, and help prevent crashes or poor performance.
          </p>
          <div className={styles.buttons}>
            <div className="col col--2 margin-horiz--sm">
              <Link
                className={classnames(
                  "button button--secondary button--lg",
                  styles.getStarted
                )}
                to={useBaseUrl("docs/getting-started")}
              >
                Get Started
              </Link>
            </div>
            <div className="col col--2 margin-horiz--sm">
              <Link
                className={classnames(
                  "button button--secondary button--lg",
                  styles.getStarted
                )}
                to={useBaseUrl("docs/about-Infer")}
              >
                Learn More
              </Link>
            </div>
          </div>
          <div className={styles.starCount}>
            <GitHubButton
              href="https://github.com/facebook/infer"
              data-icon="octicon-star"
              data-size="large"
              data-show-count="true"
              aria-label="Star facebook/infer on GitHub"
            >
              Star
            </GitHubButton>
          </div>
        </div>
      </header>
      <main>
      <VideoContainer />
        {features && features.length && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {features.map(({ title, description }, idx) => (
                  <div key={idx} className="col col--6">
                    <h3>{title}</h3>
                    <p>{description}</p>
                  </div>
                ))}
              </div>
            </div>
          </section>
        )}
        <section className={styles.features}>
          <div className="container">
            <div className="row">
              <div className="col col--6">
                <h3>Infer in Action</h3>
                <Script
                  id="asciicast-32101"
                  src="https://asciinema.org/a/32101.js"
                  data-autoplay="true"
                  data-loop="true"
                  data-speed="2"
                  async
                />
              </div>
              <div className="col col--6">
                <h3>Try Infer</h3>
                <iframe
                  className={styles.windows}
                  src="https://codeboard.io/projects/11587?view=2.1-21.0-22.0"
                ></iframe>
              </div>
            </div>
          </div>
        </section>
        <section className={styles.usingInfer}>
          <div className="container">
            <h2>Using Infer</h2>
            <p>
              Start with the <a href="/docs/getting-started">Getting Started</a>{" "}
              guide and our other <a href="/docs/getting-started">docs</a> to
              download and try Infer yourself. Infer is still evolving, and we
              want to continue to develop it in the open. We hope it will be
              useful for other projects, so please try it out or contribute to
              it, join the community and give us feedback!
            </p>
          </div>
        </section>
        <section className={styles.whoUses} id="who-uses-infer">
          <div className="container">
            <h2>Who Uses Infer?</h2>
            <div style={{ margin: "4rem 0" }}>
              <div className="row">
                {poweredByImages.map(({ url, image }, idx) => (
                  <div key={idx} className="col col--2">
                    <div className={styles.whoUsesItems}>
                      <a href={url}>
                        <img className={styles.poweredByImage} src={image} />
                      </a>
                    </div>
                  </div>
                ))}
              </div>
              <div className="row">
                {poweredByItems.map(({ url, name }, idx) => (
                  <div key={idx} className="col col--2">
                    <div className={styles.whoUsesItems}>
                      <a href={url}>
                        <h3 className={styles.poweredByItem}>{name}</h3>
                      </a>
                    </div>
                  </div>
                ))}
              </div>
            </div>
            <span>
              Does your project use Infer? Add it to this list with{" "}
              <a href="https://github.com/facebook/infer/edit/main/website/src/pages/index.js">
                a pull request!
              </a>
            </span>
          </div>
        </section>
      </main>
    </Layout>
  );
}

export default Home;
