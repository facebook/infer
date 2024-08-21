"use strict";(self.webpackChunk=self.webpackChunk||[]).push([[4856],{3916:(e,i,n)=>{n.r(i),n.d(i,{assets:()=>a,contentTitle:()=>o,default:()=>d,frontMatter:()=>s,metadata:()=>c,toc:()=>l});var r=n(7624),t=n(2172);const s={title:"List of all checkers",hide_table_of_contents:!0},o=void 0,c={id:"all-checkers",title:"List of all checkers",description:"Here is an overview of the checkers currently available in Infer.",source:"@site/docs/all-checkers.md",sourceDirName:".",slug:"/all-checkers",permalink:"/docs/next/all-checkers",draft:!1,unlisted:!1,tags:[],version:"current",frontMatter:{title:"List of all checkers",hide_table_of_contents:!0},sidebar:"docs",previous:{title:"infer run",permalink:"/docs/next/man-infer-run"},next:{title:"List of all categories of issue types",permalink:"/docs/next/all-categories"}},a={},l=[{value:"Annotation Reachability",id:"annotation-reachability",level:2},{value:"Biabduction",id:"biabduction",level:2},{value:"Buffer Overrun Analysis (InferBO)",id:"buffer-overrun-analysis-inferbo",level:2},{value:"Config Impact Analysis",id:"config-impact-analysis",level:2},{value:"Cost: Complexity Analysis",id:"cost-complexity-analysis",level:2},{value:"Fragment Retains View",id:"fragment-retains-view",level:2},{value:"Impurity",id:"impurity",level:2},{value:"Inefficient keySet Iterator",id:"inefficient-keyset-iterator",level:2},{value:"Lineage",id:"lineage",level:2},{value:"Litho &quot;Required Props&quot;",id:"litho-required-props",level:2},{value:"Liveness",id:"liveness",level:2},{value:"Loop Hoisting",id:"loop-hoisting",level:2},{value:"Parameter Not Null Checked",id:"parameter-not-null-checked",level:2},{value:"Pulse",id:"pulse",level:2},{value:"Purity",id:"purity",level:2},{value:"RacerD",id:"racerd",level:2},{value:"Resource Leak Lab Exercise",id:"resource-leak-lab-exercise",level:2},{value:"SIL validation",id:"sil-validation",level:2},{value:"Static Initialization Order Fiasco",id:"static-initialization-order-fiasco",level:2},{value:"Scope Leakage",id:"scope-leakage",level:2},{value:"Self in Block",id:"self-in-block",level:2},{value:"Starvation",id:"starvation",level:2},{value:"Topl",id:"topl",level:2}];function h(e){const i={a:"a",code:"code",h2:"h2",p:"p",strong:"strong",...(0,t.M)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(i.p,{children:"Here is an overview of the checkers currently available in Infer."}),"\n",(0,r.jsx)(i.h2,{id:"annotation-reachability",children:"Annotation Reachability"}),"\n",(0,r.jsxs)(i.p,{children:["Given pairs of source and sink annotations, e.g. ",(0,r.jsx)(i.code,{children:"@A"})," and ",(0,r.jsx)(i.code,{children:"@B"}),", this checker will warn whenever some method annotated with ",(0,r.jsx)(i.code,{children:"@A"})," calls, directly or indirectly, another method annotated with ",(0,r.jsx)(i.code,{children:"@B"}),". Besides the custom pairs, it is also possible to enable some built-in checks, such as ",(0,r.jsx)(i.code,{children:"@PerformanceCritical"})," reaching ",(0,r.jsx)(i.code,{children:"@Expensive"})," or ",(0,r.jsx)(i.code,{children:"@NoAllocation"})," reaching ",(0,r.jsx)(i.code,{children:"new"}),". See flags starting with ",(0,r.jsx)(i.code,{children:"--annotation-reachability"}),"."]}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-annotation-reachability",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"biabduction",children:"Biabduction"}),"\n",(0,r.jsx)(i.p,{children:"This analysis deals with a range of issues, many linked to memory safety."}),"\n",(0,r.jsxs)(i.p,{children:[(0,r.jsx)(i.strong,{children:"***DEPRECATED***"})," This has been replaced by Pulse and will be removed in the next release."]}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-biabduction",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"buffer-overrun-analysis-inferbo",children:"Buffer Overrun Analysis (InferBO)"}),"\n",(0,r.jsx)(i.p,{children:"InferBO is a detector for out-of-bounds array accesses."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-bufferoverrun",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"config-impact-analysis",children:"Config Impact Analysis"}),"\n",(0,r.jsx)(i.p,{children:"[EXPERIMENTAL] Collects function that are called without config checks."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-config-impact-analysis",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"cost-complexity-analysis",children:"Cost: Complexity Analysis"}),"\n",(0,r.jsxs)(i.p,{children:["Computes the asymptotic complexity of functions with respect to execution cost or other user defined resources. Can be used to detect changes in the complexity with ",(0,r.jsx)(i.code,{children:"infer reportdiff"}),"."]}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-cost",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"fragment-retains-view",children:"Fragment Retains View"}),"\n",(0,r.jsx)(i.p,{children:"Detects when Android fragments are not explicitly nullified before becoming unreachable."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-fragment-retains-view",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"impurity",children:"Impurity"}),"\n",(0,r.jsx)(i.p,{children:'Detects functions with potential side-effects. Same as "purity", but implemented on top of Pulse.'}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-impurity",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"inefficient-keyset-iterator",children:"Inefficient keySet Iterator"}),"\n",(0,r.jsx)(i.p,{children:"Check for inefficient uses of iterators that iterate on keys then lookup their values, instead of iterating on key-value pairs directly."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-inefficient-keyset-iterator",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"lineage",children:"Lineage"}),"\n",(0,r.jsx)(i.p,{children:"Computes a dataflow graph"}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-lineage",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"litho-required-props",children:'Litho "Required Props"'}),"\n",(0,r.jsxs)(i.p,{children:["Checks that all non-optional ",(0,r.jsx)(i.code,{children:"@Prop"}),"s have been specified when constructing Litho components."]}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-litho-required-props",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"liveness",children:"Liveness"}),"\n",(0,r.jsx)(i.p,{children:"Detection of dead stores and unused variables."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-liveness",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"loop-hoisting",children:"Loop Hoisting"}),"\n",(0,r.jsx)(i.p,{children:"Detect opportunities to hoist function calls that are invariant outside of loop bodies for efficiency."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-loop-hoisting",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"parameter-not-null-checked",children:"Parameter Not Null Checked"}),"\n",(0,r.jsx)(i.p,{children:"An Objective-C-specific analysis to detect when a block parameter is used before being checked for null first."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-parameter-not-null-checked",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"pulse",children:"Pulse"}),"\n",(0,r.jsx)(i.p,{children:"General-purpose memory and value analysis engine."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-pulse",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"purity",children:"Purity"}),"\n",(0,r.jsx)(i.p,{children:'Detects pure (side-effect-free) functions. A different implementation of "impurity".'}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-purity",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"racerd",children:"RacerD"}),"\n",(0,r.jsx)(i.p,{children:"Thread safety analysis."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-racerd",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"resource-leak-lab-exercise",children:"Resource Leak Lab Exercise"}),"\n",(0,r.jsx)(i.p,{children:'Toy checker for the "resource leak" write-your-own-checker exercise.'}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-resource-leak-lab",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"sil-validation",children:"SIL validation"}),"\n",(0,r.jsx)(i.p,{children:"This checker validates that all SIL instructions in all procedure bodies conform to a (front-end specific) subset of SIL."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-sil-validation",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"static-initialization-order-fiasco",children:"Static Initialization Order Fiasco"}),"\n",(0,r.jsx)(i.p,{children:"Catches Static Initialization Order Fiascos in C++, that can lead to subtle, compiler-version-dependent errors."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-siof",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"scope-leakage",children:"Scope Leakage"}),"\n",(0,r.jsx)(i.p,{children:'The Java/Kotlin checker takes into account a set of "scope" annotations and a must-not-hold relation over the scopes. The checker raises an alarm if there exists a field access path from object A to object B, with respective scopes SA and SB, such that must-not-hold(SA, SB).'}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-scope-leakage",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"self-in-block",children:"Self in Block"}),"\n",(0,r.jsxs)(i.p,{children:["An Objective-C-specific analysis to detect when a block captures ",(0,r.jsx)(i.code,{children:"self"}),"."]}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-self-in-block",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"starvation",children:"Starvation"}),"\n",(0,r.jsx)(i.p,{children:"Detect various kinds of situations when no progress is being made because of concurrency errors."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-starvation",children:"Visit here for more information."})}),"\n",(0,r.jsx)(i.h2,{id:"topl",children:"Topl"}),"\n",(0,r.jsx)(i.p,{children:"Detect errors based on user-provided state machines describing temporal properties over multiple objects."}),"\n",(0,r.jsx)(i.p,{children:(0,r.jsx)(i.a,{href:"/docs/next/checker-topl",children:"Visit here for more information."})})]})}function d(e={}){const{wrapper:i}={...(0,t.M)(),...e.components};return i?(0,r.jsx)(i,{...e,children:(0,r.jsx)(h,{...e})}):h(e)}},2172:(e,i,n)=>{n.d(i,{I:()=>c,M:()=>o});var r=n(1504);const t={},s=r.createContext(t);function o(e){const i=r.useContext(s);return r.useMemo((function(){return"function"==typeof e?e(i):{...i,...e}}),[i,e])}function c(e){let i;return i=e.disableParentContext?"function"==typeof e.components?e.components(t):e.components||t:o(e.components),r.createElement(s.Provider,{value:i},e.children)}}}]);