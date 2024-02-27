# PULSE-♾️("pulse-infinite")

Pulse-♾️ is an under-approximate non-termination checker based on the Pulse checker, part of the Infer framework by Meta.

Program termination is a well-known undecidable problem in the general case, and under-approximation is a tractable strategy to find non-termination bugs in real programs.

The new checker adds a new Error Type in Pulse: PULSE_INFINITE, which is now part of the error report printed by Pulse at the end of analysis.

To compile Pulse-♾️, just build infer the normal way. We currently test on C/C++ programs, so it suffices to build infer as such:

$ build-infer clang  

# Run Pulse-♾️ on divergence test cases (infinite.cpp) 

Tests are currently all written into a single file infinite.cpp located as below.

$ cd infer/tests/codetoanalyze/c/pulse  

IF needed, edit termination-run-all.sh to change the HOME value to yours, then run:

$ ./termination-run-all.sh  

The results go into infer-run.log as well as on the standard output

# Run Pulse-♾️ on an OSS project (ex: openssl)

git clone https://github.com/openssl/openssl  
cp termination-run-all.sh ./openssl/termination-run-openssl.sh  

Edit termination-run-openssl.sh and change "clang -c infinite.cpp" by "make" which is how openssl is built. This will build the target and run the analysis automatically after the build.

$ cd openssl
$ ./config
$ ./termination-run-openssl.sh  

The results go to infer-run.log again (unless you changed that name for your OSS project log file in termination-run-openssl.sh)

# How to remove debug output

The Pulse-♾️ checker is a DEVELOPMENT build which prints a lot of debug output and generate very large log files for development purpose.

To remove the verbose debug output printed by default in pulse-♾️, please delete "--debug-level=2" and "-g" from the pulse invocation line in the script.

Enjoy!


