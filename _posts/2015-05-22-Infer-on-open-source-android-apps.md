---
title: Infer on Open Source Android Apps
layout: post
author: dulmarod
category: blog
---
We ran Infer on a few open source Android apps with the aim of finding some bugs 
and getting them fixed. Some of those reports got indeed fixed. 

One of the apps analyzed was the search engine [DuckDuckGo](https://github.com/duckduckgo/android). We found that many database cursors were not closed. Soon after we reported the issue, a developer [fixed it](https://github.com/duckduckgo/android/commit/2c2d79f990dde0e44cdbecb1925b73c63bf9141d).

We also analyzed the popular email client [k-9](https://github.com/k9mail/k-9). We found a file not closed leak and reported it. Interestingly, a developer [fixed it](https://github.com/k9mail/k-9/commit/d538278be62687758c956af62ee47c53637d67d8) by not writing some logging info to the file at all. So Infer helped them to simplify their code.

[Conversations](https://github.com/siacs/Conversations) is an open source XMPP/Jabber client for Android smart phones. We analyzed it as well and found a file not closed leak, which was also [fixed](https://github.com/Flowdalic/MemorizingTrustManager/commit/190c57a9a8385f4726c817924b123438af6adc2f).
