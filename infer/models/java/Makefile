SHELL := /bin/bash
CWD = $(shell pwd)
BINDIR = $(CWD)/../../bin

INFERJ = ANALYZE_MODELS=1 $(BINDIR)/inferJ --buck --analyzer infer --multicore 1

ANDROID_JAR = ../../lib/java/android/android-19.jar
JACKSON_JAR = ../../../dependencies/java/jackson/jackson-2.2.3.jar

REMOVE = rm -rf
MAKE = mkdir

CURRENT_DIR = $(shell pwd)

MODELS_OUT = $(CURRENT_DIR)/models
CSV_REPORT = $(MODELS_OUT)/infer/report.csv
MODELS_JAR = $(CURRENT_DIR)/models.jar
DEPLOYED_MODELS_JAR = ../../lib/java/models.jar

JAVA_SOURCES = $(shell find src -name "*.java")

.PHONY: all clean

all: clean $(MODELS_JAR)

clean:
	$(REMOVE) $(MODELS_OUT)
	$(REMOVE) $(DEPLOYED_MODELS_JAR)

$(CSV_REPORT): $(JAVA_SOURCES)
	mkdir $(MODELS_OUT)
	$(INFERJ) javac -bootclasspath $(ANDROID_JAR) -d $(MODELS_OUT) -classpath $(JACKSON_JAR) $(JAVA_SOURCES)

$(MODELS_JAR): $(CSV_REPORT)
	cd $(MODELS_OUT); jar cf $(MODELS_JAR) *
	mv $(MODELS_JAR) $(DEPLOYED_MODELS_JAR)
