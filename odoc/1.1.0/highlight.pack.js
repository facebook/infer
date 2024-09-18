/*! For license information please see highlight.pack.js.LICENSE.txt */
!function(e){var n="object"==typeof window&&window||"object"==typeof self&&self;"undefined"!=typeof exports?e(exports):n&&(n.hljs=e({}),"function"==typeof define&&define.amd&&define([],(function(){return n.hljs})))}((function(e){var n=[],r=Object.keys,t={},a={},i=/^(no-?highlight|plain|text)$/i,c=/\blang(?:uage)?-([\w-]+)\b/i,o=/((^(<[^>]+>|\t|)+|(?:\n)))/gm,l={case_insensitive:"cI",lexemes:"l",contains:"c",keywords:"k",subLanguage:"sL",className:"cN",begin:"b",beginKeywords:"bK",end:"e",endsWithParent:"eW",illegal:"i",excludeBegin:"eB",excludeEnd:"eE",returnBegin:"rB",returnEnd:"rE",relevance:"r",variants:"v",IDENT_RE:"IR",UNDERSCORE_IDENT_RE:"UIR",NUMBER_RE:"NR",C_NUMBER_RE:"CNR",BINARY_NUMBER_RE:"BNR",RE_STARTERS_RE:"RSR",BACKSLASH_ESCAPE:"BE",APOS_STRING_MODE:"ASM",QUOTE_STRING_MODE:"QSM",PHRASAL_WORDS_MODE:"PWM",C_LINE_COMMENT_MODE:"CLCM",C_BLOCK_COMMENT_MODE:"CBCM",HASH_COMMENT_MODE:"HCM",NUMBER_MODE:"NM",C_NUMBER_MODE:"CNM",BINARY_NUMBER_MODE:"BNM",CSS_NUMBER_MODE:"CSSNM",REGEXP_MODE:"RM",TITLE_MODE:"TM",UNDERSCORE_TITLE_MODE:"UTM",COMMENT:"C",beginRe:"bR",endRe:"eR",illegalRe:"iR",lexemesRe:"lR",terminators:"t",terminator_end:"tE"},s="</span>",u={classPrefix:"hljs-",tabReplace:null,useBR:!1,languages:void 0};function f(e){return e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;")}function b(e){return e.nodeName.toLowerCase()}function g(e,n){var r=e&&e.exec(n);return r&&0===r.index}function E(e){return i.test(e)}function d(e){var n,r={},t=Array.prototype.slice.call(arguments,1);for(n in e)r[n]=e[n];return t.forEach((function(e){for(n in e)r[n]=e[n]})),r}function _(e){var n=[];return function e(r,t){for(var a=r.firstChild;a;a=a.nextSibling)3===a.nodeType?t+=a.nodeValue.length:1===a.nodeType&&(n.push({event:"start",offset:t,node:a}),t=e(a,t),b(a).match(/br|hr|img|input/)||n.push({event:"stop",offset:t,node:a}));return t}(e,0),n}function N(e){if(l&&!e.langApiRestored){for(var n in e.langApiRestored=!0,l)e[n]&&(e[l[n]]=e[n]);(e.c||[]).concat(e.v||[]).forEach(N)}}function p(e){function n(e){return e&&e.source||e}function t(r,t){return new RegExp(n(r),"m"+(e.cI?"i":"")+(t?"g":""))}!function a(i,c){if(!i.compiled){if(i.compiled=!0,i.k=i.k||i.bK,i.k){function o(n,r){e.cI&&(r=r.toLowerCase()),r.split(" ").forEach((function(e){var r=e.split("|");l[r[0]]=[n,r[1]?Number(r[1]):1]}))}var l={};"string"==typeof i.k?o("keyword",i.k):r(i.k).forEach((function(e){o(e,i.k[e])})),i.k=l}i.lR=t(i.l||/\w+/,!0),c&&(i.bK&&(i.b="\\b("+i.bK.split(" ").join("|")+")\\b"),i.b||(i.b=/\B|\b/),i.bR=t(i.b),i.endSameAsBegin&&(i.e=i.b),i.e||i.eW||(i.e=/\B|\b/),i.e&&(i.eR=t(i.e)),i.tE=n(i.e)||"",i.eW&&c.tE&&(i.tE+=(i.e?"|":"")+c.tE)),i.i&&(i.iR=t(i.i)),null==i.r&&(i.r=1),i.c||(i.c=[]),i.c=Array.prototype.concat.apply([],i.c.map((function(e){return function(e){return e.v&&!e.cached_variants&&(e.cached_variants=e.v.map((function(n){return d(e,{v:null},n)}))),e.cached_variants||e.eW&&[d(e)]||[e]}("self"===e?i:e)}))),i.c.forEach((function(e){a(e,i)})),i.starts&&a(i.starts,c);var s=i.c.map((function(e){return e.bK?"\\.?(?:"+e.b+")\\.?":e.b})).concat([i.tE,i.i]).map(n).filter(Boolean);i.t=s.length?t(function(e){for(var r=/\[(?:[^\\\]]|\\.)*\]|\(\??|\\([1-9][0-9]*)|\\./,t=0,a="",i=0;i<e.length;i++){var c=t,o=n(e[i]);for(0<i&&(a+="|");0<o.length;){var l=r.exec(o);if(null==l){a+=o;break}a+=o.substring(0,l.index),o=o.substring(l.index+l[0].length),"\\"==l[0][0]&&l[1]?a+="\\"+String(Number(l[1])+c):(a+=l[0],"("==l[0]&&t++)}}return a}(s),!0):{exec:function(){return null}}}}(e)}function h(e,n,r,a){function i(e,n,r,t){var a='<span class="'+(t?"":u.classPrefix);return e?(a+=e+'">')+n+(r?"":s):n}function c(){N+=null!=d.sL?function(){var e="string"==typeof d.sL;if(e&&!t[d.sL])return f(M);var n=e?h(d.sL,M,!0,_[d.sL]):R(M,d.sL.length?d.sL:void 0);return 0<d.r&&(m+=n.r),e&&(_[d.sL]=n.top),i(n.language,n.value,!1,!0)}():function(){var e,n,r,t,a,c,o;if(!d.k)return f(M);for(t="",n=0,d.lR.lastIndex=0,r=d.lR.exec(M);r;)t+=f(M.substring(n,r.index)),a=d,c=r,o=b.cI?c[0].toLowerCase():c[0],(e=a.k.hasOwnProperty(o)&&a.k[o])?(m+=e[1],t+=i(e[0],f(r[0]))):t+=f(r[0]),n=d.lR.lastIndex,r=d.lR.exec(M);return t+f(M.substr(n))}(),M=""}function o(e){N+=e.cN?i(e.cN,"",!0):"",d=Object.create(e,{parent:{value:d}})}function l(e,n){if(M+=e,null==n)return c(),0;var t=function(e,n){var r,t,a;for(r=0,t=n.c.length;r<t;r++)if(g(n.c[r].bR,e))return n.c[r].endSameAsBegin&&(n.c[r].eR=(a=n.c[r].bR.exec(e)[0],new RegExp(a.replace(/[-\/\\^$*+?.()|[\]{}]/g,"\\$&"),"m"))),n.c[r]}(n,d);if(t)return t.skip?M+=n:(t.eB&&(M+=n),c(),t.rB||t.eB||(M=n)),o(t),t.rB?0:n.length;var a=function e(n,r){if(g(n.eR,r)){for(;n.endsParent&&n.parent;)n=n.parent;return n}if(n.eW)return e(n.parent,r)}(d,n);if(a){var i=d;for(i.skip?M+=n:(i.rE||i.eE||(M+=n),c(),i.eE&&(M=n));d.cN&&(N+=s),d.skip||d.sL||(m+=d.r),(d=d.parent)!==a.parent;);return a.starts&&(a.endSameAsBegin&&(a.starts.eR=a.eR),o(a.starts)),i.rE?0:n.length}if(function(e,n){return!r&&g(n.iR,e)}(n,d))throw new Error('Illegal lexeme "'+n+'" for mode "'+(d.cN||"<unnamed>")+'"');return M+=n,n.length||1}var b=B(e);if(!b)throw new Error('Unknown language: "'+e+'"');p(b);var E,d=a||b,_={},N="";for(E=d;E!==b;E=E.parent)E.cN&&(N=i(E.cN,"",!0)+N);var M="",m=0;try{for(var v,C,O=0;d.t.lastIndex=O,v=d.t.exec(n);)C=l(n.substring(O,v.index),v[0]),O=v.index+C;for(l(n.substr(O)),E=d;E.parent;E=E.parent)E.cN&&(N+=s);return{r:m,value:N,language:e,top:d}}catch(e){if(e.message&&-1!==e.message.indexOf("Illegal"))return{r:0,value:f(n)};throw e}}function R(e,n){n=n||u.languages||r(t);var a={r:0,value:f(e)},i=a;return n.filter(B).filter(C).forEach((function(n){var r=h(n,e,!1);r.language=n,r.r>i.r&&(i=r),r.r>a.r&&(i=a,a=r)})),i.language&&(a.second_best=i),a}function M(e){return u.tabReplace||u.useBR?e.replace(o,(function(e,n){return u.useBR&&"\n"===e?"<br>":u.tabReplace?n.replace(/\t/g,u.tabReplace):""})):e}function m(e){var r,t,i,o,l,s=function(e){var n,r,t,a,i=e.className+" ";if(i+=e.parentNode?e.parentNode.className:"",r=c.exec(i))return B(r[1])?r[1]:"no-highlight";for(n=0,t=(i=i.split(/\s+/)).length;n<t;n++)if(E(a=i[n])||B(a))return a}(e);E(s)||(u.useBR?(r=document.createElementNS("http://www.w3.org/1999/xhtml","div")).innerHTML=e.innerHTML.replace(/\n/g,"").replace(/<br[ \/]*>/g,"\n"):r=e,l=r.textContent,i=s?h(s,l,!0):R(l),(t=_(r)).length&&((o=document.createElementNS("http://www.w3.org/1999/xhtml","div")).innerHTML=i.value,i.value=function(e,r,t){var a=0,i="",c=[];function o(){return e.length&&r.length?e[0].offset!==r[0].offset?e[0].offset<r[0].offset?e:r:"start"===r[0].event?e:r:e.length?e:r}function l(e){i+="<"+b(e)+n.map.call(e.attributes,(function(e){return" "+e.nodeName+'="'+f(e.value).replace('"',"&quot;")+'"'})).join("")+">"}function s(e){i+="</"+b(e)+">"}function u(e){("start"===e.event?l:s)(e.node)}for(;e.length||r.length;){var g=o();if(i+=f(t.substring(a,g[0].offset)),a=g[0].offset,g===e){for(c.reverse().forEach(s);u(g.splice(0,1)[0]),(g=o())===e&&g.length&&g[0].offset===a;);c.reverse().forEach(l)}else"start"===g[0].event?c.push(g[0].node):c.pop(),u(g.splice(0,1)[0])}return i+f(t.substr(a))}(t,_(o),l)),i.value=M(i.value),e.innerHTML=i.value,e.className=function(e,n,r){var t=n?a[n]:r,i=[e.trim()];return e.match(/\bhljs\b/)||i.push("hljs"),-1===e.indexOf(t)&&i.push(t),i.join(" ").trim()}(e.className,s,i.language),e.result={language:i.language,re:i.r},i.second_best&&(e.second_best={language:i.second_best.language,re:i.second_best.r}))}function v(){if(!v.called){v.called=!0;var e=document.querySelectorAll("pre code");n.forEach.call(e,m)}}function B(e){return e=(e||"").toLowerCase(),t[e]||t[a[e]]}function C(e){var n=B(e);return n&&!n.disableAutodetect}return e.highlight=h,e.highlightAuto=R,e.fixMarkup=M,e.highlightBlock=m,e.configure=function(e){u=d(u,e)},e.initHighlighting=v,e.initHighlightingOnLoad=function(){addEventListener("DOMContentLoaded",v,!1),addEventListener("load",v,!1)},e.registerLanguage=function(n,r){var i=t[n]=r(e);N(i),i.aliases&&i.aliases.forEach((function(e){a[e]=n}))},e.listLanguages=function(){return r(t)},e.getLanguage=B,e.autoDetection=C,e.inherit=d,e.IR=e.IDENT_RE="[a-zA-Z]\\w*",e.UIR=e.UNDERSCORE_IDENT_RE="[a-zA-Z_]\\w*",e.NR=e.NUMBER_RE="\\b\\d+(\\.\\d+)?",e.CNR=e.C_NUMBER_RE="(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)",e.BNR=e.BINARY_NUMBER_RE="\\b(0b[01]+)",e.RSR=e.RE_STARTERS_RE="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~",e.BE=e.BACKSLASH_ESCAPE={b:"\\\\[\\s\\S]",r:0},e.ASM=e.APOS_STRING_MODE={cN:"string",b:"'",e:"'",i:"\\n",c:[e.BE]},e.QSM=e.QUOTE_STRING_MODE={cN:"string",b:'"',e:'"',i:"\\n",c:[e.BE]},e.PWM=e.PHRASAL_WORDS_MODE={b:/\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/},e.C=e.COMMENT=function(n,r,t){var a=e.inherit({cN:"comment",b:n,e:r,c:[]},t||{});return a.c.push(e.PWM),a.c.push({cN:"doctag",b:"(?:TODO|FIXME|NOTE|BUG|XXX):",r:0}),a},e.CLCM=e.C_LINE_COMMENT_MODE=e.C("//","$"),e.CBCM=e.C_BLOCK_COMMENT_MODE=e.C("/\\*","\\*/"),e.HCM=e.HASH_COMMENT_MODE=e.C("#","$"),e.NM=e.NUMBER_MODE={cN:"number",b:e.NR,r:0},e.CNM=e.C_NUMBER_MODE={cN:"number",b:e.CNR,r:0},e.BNM=e.BINARY_NUMBER_MODE={cN:"number",b:e.BNR,r:0},e.CSSNM=e.CSS_NUMBER_MODE={cN:"number",b:e.NR+"(%|em|ex|ch|rem|vw|vh|vmin|vmax|cm|mm|in|pt|pc|px|deg|grad|rad|turn|s|ms|Hz|kHz|dpi|dpcm|dppx)?",r:0},e.RM=e.REGEXP_MODE={cN:"regexp",b:/\//,e:/\/[gimuy]*/,i:/\n/,c:[e.BE,{b:/\[/,e:/\]/,r:0,c:[e.BE]}]},e.TM=e.TITLE_MODE={cN:"title",b:e.IR,r:0},e.UTM=e.UNDERSCORE_TITLE_MODE={cN:"title",b:e.UIR,r:0},e.METHOD_GUARD={b:"\\.\\s*"+e.UIR,r:0},e})),hljs.registerLanguage("ocaml",(function(e){return{aliases:["ml"],k:{keyword:"and as assert asr begin class constraint do done downto else end exception external for fun function functor if in include inherit! inherit initializer land lazy let lor lsl lsr lxor match method!|10 method mod module mutable new object of open! open or private rec sig struct then to try type val! val virtual when while with parser value",built_in:"array bool bytes char exn|5 float int int32 int64 list lazy_t|5 nativeint|5 string unit in_channel out_channel ref",literal:"true false"},i:/\/\/|>>/,l:"[a-z_]\\w*!?",c:[{cN:"literal",b:"\\[(\\|\\|)?\\]|\\(\\)",r:0},e.C("\\(\\*","\\*\\)",{c:["self"]}),{cN:"symbol",b:"'[A-Za-z_](?!')[\\w']*"},{cN:"type",b:"`[A-Z][\\w']*"},{cN:"type",b:"\\b[A-Z][\\w']*",r:0},{b:"[a-z_]\\w*'[\\w']*",r:0},e.inherit(e.ASM,{cN:"string",r:0}),e.inherit(e.QSM,{i:null}),{cN:"number",b:"\\b(0[xX][a-fA-F0-9_]+[Lln]?|0[oO][0-7_]+[Lln]?|0[bB][01_]+[Lln]?|[0-9][0-9_]*([Lln]|(\\.[0-9_]*)?([eE][-+]?[0-9_]+)?)?)",r:0},{b:/[-=]>/}]}})),hljs.registerLanguage("reasonml",(function(e){var n="~?[a-z$_][0-9a-zA-Z$_]*",r="`?[A-Z$_][0-9a-zA-Z$_]*",t="("+["||","&&","++","**","+.","*","/","*.","/.","...","|>"].map((function(e){return e.split("").map((function(e){return"\\"+e})).join("")})).join("|")+"|==|===)",a="\\s+"+t+"\\s+",i={keyword:"and as asr assert begin class constraint do done downto else end exception externalfor fun function functor if in include inherit initializerland lazy let lor lsl lsr lxor match method mod module mutable new nonrecobject of open or private rec sig struct then to try type val virtual when while with",built_in:"array bool bytes char exn|5 float int int32 int64 list lazy_t|5 nativeint|5 ref string unit ",literal:"true false"},c="\\b(0[xX][a-fA-F0-9_]+[Lln]?|0[oO][0-7_]+[Lln]?|0[bB][01_]+[Lln]?|[0-9][0-9_]*([Lln]|(\\.[0-9_]*)?([eE][-+]?[0-9_]+)?)?)",o={cN:"number",r:0,v:[{b:c},{b:"\\(\\-"+c+"\\)"}]},l={cN:"operator",r:0,b:t},s=[{cN:"identifier",r:0,b:n},l,o],u=[e.QSM,l,{cN:"module",b:"\\b"+r,rB:!0,e:".",c:[{cN:"identifier",b:r,r:0}]}],f=[{cN:"module",b:"\\b"+r,rB:!0,e:".",r:0,c:[{cN:"identifier",b:r,r:0}]}],b={cN:"function",r:0,k:i,v:[{b:"\\s(\\(\\.?.*?\\)|"+n+")\\s*=>",e:"\\s*=>",rB:!0,r:0,c:[{cN:"params",v:[{b:n},{b:"~?[a-z$_][0-9a-zA-Z$_]*(s*:s*[a-z$_][0-9a-z$_]*((s*('?[a-z$_][0-9a-z$_]*s*(,'?[a-z$_][0-9a-z$_]*)*)?s*))?)?(s*:s*[a-z$_][0-9a-z$_]*((s*('?[a-z$_][0-9a-z$_]*s*(,'?[a-z$_][0-9a-z$_]*)*)?s*))?)?"},{b:/\(\s*\)/}]}]},{b:"\\s\\(\\.?[^;\\|]*\\)\\s*=>",e:"\\s=>",rB:!0,r:0,c:[{cN:"params",r:0,v:[{b:n,e:"(,|\\n|\\))",r:0,c:[l,{cN:"typing",b:":",e:"(,|\\n)",rB:!0,r:0,c:f}]}]}]},{b:"\\(\\.\\s"+n+"\\)\\s*=>"}]};u.push(b);var g={cN:"constructor",b:r+"\\(",e:"\\)",i:"\\n",k:i,c:[e.QSM,l,{cN:"params",b:"\\b"+n}]},E={cN:"pattern-match",b:"\\|",rB:!0,k:i,e:"=>",r:0,c:[g,l,{r:0,cN:"constructor",b:r}]},d={cN:"module-access",k:i,rB:!0,v:[{b:"\\b("+r+"\\.)+"+n},{b:"\\b("+r+"\\.)+\\(",e:"\\)",rB:!0,c:[b,{b:"\\(",e:"\\)",skip:!0}].concat(u)},{b:"\\b("+r+"\\.)+{",e:"}"}],c:u};return f.push(d),{aliases:["re"],k:i,i:"(:\\-|:=|\\${|\\+=)",c:[e.C("/\\*","\\*/",{i:"^(\\#,\\/\\/)"}),{cN:"character",b:"'(\\\\[^']+|[^'])'",i:"\\n",r:0},e.QSM,{cN:"literal",b:"\\(\\)",r:0},{cN:"literal",b:"\\[\\|",e:"\\|\\]",r:0,c:s},{cN:"literal",b:"\\[",e:"\\]",r:0,c:s},g,{cN:"operator",b:a,i:"\\-\\->",r:0},o,e.CLCM,E,b,{cN:"module-def",b:"\\bmodule\\s+"+n+"\\s+"+r+"\\s+=\\s+{",e:"}",rB:!0,k:i,r:0,c:[{cN:"module",r:0,b:r},{b:"{",e:"}",skip:!0}].concat(u)},d]}}));