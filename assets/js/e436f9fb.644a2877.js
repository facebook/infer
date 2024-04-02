"use strict";(self.webpackChunk=self.webpackChunk||[]).push([[9396],{2216:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>l,contentTitle:()=>r,default:()=>d,frontMatter:()=>s,metadata:()=>c,toc:()=>a});var n=i(7624),o=i(2172);const s={title:"Loop Hoisting",description:"Detect opportunities to hoist function calls that are invariant outside of loop bodies for efficiency."},r=void 0,c={id:"checker-loop-hoisting",title:"Loop Hoisting",description:"Detect opportunities to hoist function calls that are invariant outside of loop bodies for efficiency.",source:"@site/docs/checker-loop-hoisting.md",sourceDirName:".",slug:"/checker-loop-hoisting",permalink:"/docs/next/checker-loop-hoisting",draft:!1,unlisted:!1,tags:[],version:"current",frontMatter:{title:"Loop Hoisting",description:"Detect opportunities to hoist function calls that are invariant outside of loop bodies for efficiency."},sidebar:"docs",previous:{title:"Liveness",permalink:"/docs/next/checker-liveness"},next:{title:"Parameter Not Null Checked",permalink:"/docs/next/checker-parameter-not-null-checked"}},l={},a=[{value:"List of Issue Types",id:"list-of-issue-types",level:2}];function h(e){const t={a:"a",code:"code",h2:"h2",li:"li",p:"p",ul:"ul",...(0,o.M)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(t.p,{children:"Detect opportunities to hoist function calls that are invariant outside of loop bodies for efficiency."}),"\n",(0,n.jsxs)(t.p,{children:["Activate with ",(0,n.jsx)(t.code,{children:"--loop-hoisting"}),"."]}),"\n",(0,n.jsx)(t.p,{children:"Supported languages:"}),"\n",(0,n.jsxs)(t.ul,{children:["\n",(0,n.jsx)(t.li,{children:"C/C++/ObjC: Yes"}),"\n",(0,n.jsx)(t.li,{children:"C#/.Net: No"}),"\n",(0,n.jsx)(t.li,{children:"Erlang: No"}),"\n",(0,n.jsx)(t.li,{children:"Hack: No"}),"\n",(0,n.jsx)(t.li,{children:"Java: Yes"}),"\n",(0,n.jsx)(t.li,{children:"Python: No"}),"\n"]}),"\n",(0,n.jsxs)(t.p,{children:["This checker detects opportunities to hoist function calls that are invariant to outside of loop bodies. The hoisting analysis relies on ",(0,n.jsx)(t.a,{href:"/docs/next/checker-purity",children:"purity"})," analysis to determine whether a function is pure or not."]}),"\n",(0,n.jsxs)(t.p,{children:["It has an additional mode that reports ",(0,n.jsx)(t.a,{href:"/docs/next/all-issue-types#expensive_loop_invariant_call",children:"loop-invariant functions that are expensive"})," (i.e. at least linear). This is enabled by the flag ",(0,n.jsx)(t.code,{children:"--hoisting-report-only-expensive"}),"."]}),"\n",(0,n.jsx)(t.h2,{id:"list-of-issue-types",children:"List of Issue Types"}),"\n",(0,n.jsx)(t.p,{children:"The following issue types are reported by this checker:"}),"\n",(0,n.jsxs)(t.ul,{children:["\n",(0,n.jsx)(t.li,{children:(0,n.jsx)(t.a,{href:"/docs/next/all-issue-types#expensive_loop_invariant_call",children:"EXPENSIVE_LOOP_INVARIANT_CALL"})}),"\n",(0,n.jsx)(t.li,{children:(0,n.jsx)(t.a,{href:"/docs/next/all-issue-types#invariant_call",children:"INVARIANT_CALL"})}),"\n"]})]})}function d(e={}){const{wrapper:t}={...(0,o.M)(),...e.components};return t?(0,n.jsx)(t,{...e,children:(0,n.jsx)(h,{...e})}):h(e)}},2172:(e,t,i)=>{i.d(t,{I:()=>c,M:()=>r});var n=i(1504);const o={},s=n.createContext(o);function r(e){const t=n.useContext(s);return n.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function c(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(o):e.components||o:r(e.components),n.createElement(s.Provider,{value:t},e.children)}}}]);