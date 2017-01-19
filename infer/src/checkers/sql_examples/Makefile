all:
	InferAnalyze --results-dir out -checkers

.PHONY: capture
capture:
	InferCapture out javac *.java

.PHONY: clean
clean:
	rm -rf out *.class

