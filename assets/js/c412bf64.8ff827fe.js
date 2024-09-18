"use strict";(self.webpackChunk=self.webpackChunk||[]).push([[4765],{1518:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>s,default:()=>u,frontMatter:()=>r,metadata:()=>a,toc:()=>d});var o=n(4848),i=n(8453);const r={title:"Collaboration with Spotify",author:"Jules Villard"},s=void 0,a={permalink:"/blog/2016/03/17/collaboration-with-spotify",source:"@site/blog/2016-03-17-collaboration-with-spotify.md",title:"Collaboration with Spotify",description:"Infer/Spotify collaboration",date:"2016-03-17T00:00:00.000Z",tags:[],readingTime:2.675,hasTruncateMarker:!1,authors:[{name:"Jules Villard",key:null,page:null}],frontMatter:{title:"Collaboration with Spotify",author:"Jules Villard"},unlisted:!1,prevItem:{title:"Talk at Mobile@Scale London",permalink:"/blog/2016/04/07/mobileatscale-london-talk"},nextItem:{title:"Infer on Open Source Android Apps",permalink:"/blog/2015/05/22/Infer-on-open-source-android-apps"}},l={authorsImageUrls:[void 0]},d=[];function h(e){const t={a:"a",blockquote:"blockquote",img:"img",p:"p",...(0,i.R)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsx)(t.p,{children:(0,o.jsx)(t.img,{alt:"Infer/Spotify collaboration",src:n(879).A+"",width:"432",height:"200"})}),"\n",(0,o.jsxs)(t.p,{children:["Working on deploying Infer inside Facebook has taught us how important it is to\nhave the analysis tool deeply embedded into the developers' workflow; see our\n",(0,o.jsx)(t.a,{href:"https://research.facebook.com/publications/moving-fast-with-software-verification/",children:"\u201cMoving Fast with Software Verification\u201d paper"}),"."]}),"\n",(0,o.jsx)(t.p,{children:"Infer runs as part of our continuous integration (CI) system, where it reports\nissues on code modifications submitted for review by our engineers. We think\nit's great when someone can hook up Infer to their workflow, and we're working\nwith several other companies to help integrate Infer into their own CI systems.\nWe've come far enough in a collaboration with Spotify to talk about it now!"}),"\n",(0,o.jsxs)(t.p,{children:["Last July, shortly after Infer was open-sourced, we started talking with the\nMarvin (Android Infrastructure) team at Spotify. They were interested in using\nInfer on their Android app, but it did not work with their build system. They\nwere using the ",(0,o.jsx)(t.a,{href:"http://gradle.org/",children:"Gradle"})," build system, but Infer's deployment\nwithin Facebook is done using a different build system, Facebook's\n",(0,o.jsx)(t.a,{href:"https://buckbuild.com/",children:"Buck"}),"; we had only an initial, basic integration with\nGradle, which did not work with Spotify's app. A Spotify engineer, Deniz\nT\xfcrkoglu, made improvements to our Gradle integration, which he submitted as a\n",(0,o.jsx)(t.a,{href:"https://github.com/facebook/infer/pull/131",children:"pull request"})," to Infer's codebase,\nwhich is hosted on ",(0,o.jsx)(t.a,{href:"https://github.com/facebook/infer/",children:"GitHub"}),"."]}),"\n",(0,o.jsx)(t.p,{children:"Then, in November 2015, two of our engineers, Dulma Churchill and Jules Villard,\ntraveled to the Spotify office in Stockholm to attend a Hack Week there. After\nrunning Infer on the Spotify app, we discussed the analyzer reports with Spotify\nengineers, and we agreed that they identified potential problems in the code.\nInfer is now running as part of Spotify's CI system, and here is a quote from\nDeniz on Spotify's perspective on Infer, which we include with his kind\npermission."}),"\n",(0,o.jsxs)(t.blockquote,{children:["\n",(0,o.jsx)(t.p,{children:"\u201cAt Spotify we are continuously working on making our codebase better, and in\nthe Android infrastructure team we use a lot of tools: static analyzers,\nlinters, thread/address sanitizers, etc. In our quest to make our code even\nbetter, we started using Infer. Infer found several legitimate issues that\nother tools had missed. The Infer team was also very helpful in following a\nfew false positives that we encountered, and we now have it running on our\nbuild servers."}),"\n",(0,o.jsx)(t.p,{children:"Infer is a great add-on to a company's toolbox. It's not intrusive \u2014 you can\nsimply add it to your flow and it will tell you where you forgot to close that\ncursor or leaked that context. If you find a false positive, just report it\nor, even better, make a PR. With more users, it will just keep getting\nbetter.\u201d"}),"\n"]}),"\n",(0,o.jsx)(t.p,{children:"This collaboration was truly a two-way street: Not only does Infer find issues\nin Spotify, which helps improve its Android app, but feedback from Spotify led\nto several improvements in Infer, including resolution of false positives and\nimprovements of Infer's UI and integration with Gradle. The better Gradle\nintegration will make it easier for other people to run Infer on lots of other\napps around the world."}),"\n",(0,o.jsxs)(t.p,{children:["We're excited to collaborate with other companies and individuals to help make\nthe world's software better. If you are interested in integrating Infer into CI\nor otherwise hearing about our experience, ",(0,o.jsx)(t.a,{href:"/docs/support",children:"drop us a line"}),"!"]})]})}function u(e={}){const{wrapper:t}={...(0,i.R)(),...e.components};return t?(0,o.jsx)(t,{...e,children:(0,o.jsx)(h,{...e})}):h(e)}},879:(e,t,n)=>{n.d(t,{A:()=>o});const o=n.p+"assets/images/Infer-Spotify-2ebcc6d44625d01149765676f1190b22.png"},8453:(e,t,n)=>{n.d(t,{R:()=>s,x:()=>a});var o=n(6540);const i={},r=o.createContext(i);function s(e){const t=o.useContext(r);return o.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function a(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:s(e.components),o.createElement(r.Provider,{value:t},e.children)}}}]);