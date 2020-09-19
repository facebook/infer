\[EXPERIMENTAL\] Infer reports this issue when the ObjC autoreleasepool's size complexity of a
program increases in degree: e.g. from constant to linear or from logarithmic to quadratic. This
issue type is only reported in differential mode: i.e when we are comparing the analysis results of
two runs of infer on a file.
