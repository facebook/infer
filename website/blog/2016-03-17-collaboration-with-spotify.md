---
title: Collaboration with Spotify
author: Jules Villard
---

![Infer/Spotify collaboration](/img/blog/Infer-Spotify.png)

Working on deploying Infer inside Facebook has taught us how important it is to
have the analysis tool deeply embedded into the developers' workflow; see our
[“Moving Fast with Software Verification” paper](https://research.facebook.com/publications/moving-fast-with-software-verification/).

Infer runs as part of our continuous integration (CI) system, where it reports
issues on code modifications submitted for review by our engineers. We think
it's great when someone can hook up Infer to their workflow, and we're working
with several other companies to help integrate Infer into their own CI systems.
We've come far enough in a collaboration with Spotify to talk about it now!

Last July, shortly after Infer was open-sourced, we started talking with the
Marvin (Android Infrastructure) team at Spotify. They were interested in using
Infer on their Android app, but it did not work with their build system. They
were using the [Gradle](http://gradle.org/) build system, but Infer's deployment
within Facebook is done using a different build system, Facebook's
[Buck](https://buckbuild.com/); we had only an initial, basic integration with
Gradle, which did not work with Spotify's app. A Spotify engineer, Deniz
Türkoglu, made improvements to our Gradle integration, which he submitted as a
[pull request](https://github.com/facebook/infer/pull/131) to Infer's codebase,
which is hosted on [GitHub](https://github.com/facebook/infer/).

Then, in November 2015, two of our engineers, Dulma Churchill and Jules Villard,
traveled to the Spotify office in Stockholm to attend a Hack Week there. After
running Infer on the Spotify app, we discussed the analyzer reports with Spotify
engineers, and we agreed that they identified potential problems in the code.
Infer is now running as part of Spotify's CI system, and here is a quote from
Deniz on Spotify's perspective on Infer, which we include with his kind
permission.

> “At Spotify we are continuously working on making our codebase better, and in
> the Android infrastructure team we use a lot of tools: static analyzers,
> linters, thread/address sanitizers, etc. In our quest to make our code even
> better, we started using Infer. Infer found several legitimate issues that
> other tools had missed. The Infer team was also very helpful in following a
> few false positives that we encountered, and we now have it running on our
> build servers.
>
> Infer is a great add-on to a company's toolbox. It's not intrusive — you can
> simply add it to your flow and it will tell you where you forgot to close that
> cursor or leaked that context. If you find a false positive, just report it
> or, even better, make a PR. With more users, it will just keep getting
> better.”

This collaboration was truly a two-way street: Not only does Infer find issues
in Spotify, which helps improve its Android app, but feedback from Spotify led
to several improvements in Infer, including resolution of false positives and
improvements of Infer's UI and integration with Gradle. The better Gradle
integration will make it easier for other people to run Infer on lots of other
apps around the world.

We're excited to collaborate with other companies and individuals to help make
the world's software better. If you are interested in integrating Infer into CI
or otherwise hearing about our experience, [drop us a line](/docs/support)!
