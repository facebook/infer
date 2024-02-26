#!/bin/bash

HOME=/huge/jvanegue/PUBLIC_GITHUB/infer

rm -fr infer-out/ infinite.o *~ infer-run.log

# Individual test suite as expected by Infer
#$HOME/infer/bin/infer run --pulse-only --print-logs -g -- clang++ -c pulseinf/simple_loop_terminate.cpp 2> infer-run.log
#$HOME/infer/bin/infer run --pulse-only --print-logs -g -- clang++ -c pulseinf/simple_loop_break.cpp 2> infer-run.log

# All tests in one file
$HOME/infer/bin/infer run --debug-level=2 --pulse-only -g -- clang++ -c infinite.cpp 2> infer-run.log
python3 -m json.tool infer-out/report.json > report-indented.json
echo "Now: less report-indented.json to look at results"

# Works good debug mode
#$HOME/infer/bin/infer run --debug-level 2 --print-logs --pulse-only -g -- clang++ -c infinite.cpp 2> infer-run.log
# Julien: should test with --pulse-widen-threshold N and N big value (for gupta test)

# Julien's original
#~/infer/infer/bin/infer run --debug-level 2 --print-logs --pulse-only -g -- clang++ -c infinite.cpp 2> infer-run.log
#~/infer/infer/bin/infer capture --debug-level 2 --print-logs --pulse-only -g -- clang++ -c infinite.cpp 2> infer-capture.log
#~/infer/infer/bin/infer analyze --debug-level 2 --print-logs --pulse-only -g -- clang++ -c infinite.cpp 2> infer-analyze.log

# Suggested by Jules for debugging
#~/infer/infer/bin/infer --debug-level 2 --print-logs --pulse-only -g -- clang++ -c infinite.cpp 2> infer-pulseonly.log

echo Finished running termination tests. Grepping for JV in logs
echo
#grep JV *.log
grep JV infer-out/logs
