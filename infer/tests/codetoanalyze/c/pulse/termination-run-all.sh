#!/bin/bash

# This is for Julien's setup - you can do like me and store your build tree somewhere with a lot of space.
# Change this to point to your infer build tree
HOME=/huge/jvanegue/PUBLIC_GITHUB/infer

rm -fr infer-out/ infinite.o *~ infer-run.log

# Individual test suite as expected by Infer
# In the process of moving all tests to their own files, commented for now.
#$HOME/infer/bin/infer run --pulse-only --print-logs -g -- clang++ -c pulseinf/simple_loop_terminate.cpp 2> infer-run.log
#$HOME/infer/bin/infer run --pulse-only --print-logs -g -- clang++ -c pulseinf/simple_loop_break.cpp 2> infer-run.log
#$HOME/infer/bin/infer run --pulse-only --print-logs -g -- clang++ -c pulseinf/simple_goto_nonterminate.cpp 2> infer-run.log
#$HOME/infer/bin/infer run --pulse-only --print-logs -g -- clang++ -c pulseinf/two_liner_terminate.cpp 2> infer-run.log

# All tests in one file (Debug mode)
$HOME/infer/bin/infer run --debug-level=2 --pulse-only -g -- clang++ -c infinite.cpp 2> infer-run.log
python3 -m json.tool infer-out/report.json > pulseinf-report.json

# All tests in one file (No Debug mode)
#$HOME/infer/bin/infer run --pulse-only -- clang++ -c infinite.cpp 2> infer-run.log

# Julien should test with --pulse-widen-threshold N (for N big) that may be useful  for Gupta's btree test and others
# Print a chosen subset of debug logs on stdout 
grep JV infer-out/logs

echo "Pulse Infinite Analysis Completed - see pulseinf-report.json for details"
