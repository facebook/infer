# PULSE-OO (read "pulse-infinite") : An under-approximate non-termination checker

Pulse-OO is based on the Pulse checker part of Infer by Meta.

The new checker adds a new Error Type in Pulse: PULSE_INFINITE, which is now part of the error report printed by Pulse at the end of analysis.

To compile pulse-OO, there is nothing special to do, just build infer the normal way. We currently test on C/C++ programs, so it suffices to build infer as such:

$ build-infer clang

# Run Pulse-OO on divergence test cases (infinite.cpp) 

First edit termination-run-all.sh to change the HOME value to yours, then run:

$ cd infer/tests/codetoanalyze/c/pulse
$ ./termination-run-all.sh

The results go into infer-run.log as well as on the standard output

# Run Pulse/Pulse-OO on some OOS project (ex: openssl)

git clone https://github.com/openssl/openssl
cp termination-run-all.sh ./openssl/

Edit termination-run-all.sh and change "clang -c infinite.cpp" by "make" which is how openssl is built. This will build the target and run the analysis automatically after the build.

$ ./termination-run-all.sh

The results go to infer-run.log again (unless you changed that name for your OSS project log file in termination-run-all.sh)

# How to remove debug output

The new Pulse-OO checker is a DEVELOPMENT build which prints a lot of debug output and generate very large log files for development purpose.

To remove the excessive debug output printed by default in pulse-OO, do the following:

Edit the termination-run-all.sh and remove: "--debug-level=2" and "-g" on the pulse invocation line


