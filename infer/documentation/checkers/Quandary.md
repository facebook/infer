Quandary is a static taint analyzer that identifies a variety of unsafe
information flows. It has a small list of built-in
[sources](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L36)
and
[sinks](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L178),
and you can define custom sources and sinks in your `.inferconfig` file (see
example
[here](https://github.com/facebook/infer/blob/master/infer/tests/codetoanalyze/java/quandary/.inferconfig)).
