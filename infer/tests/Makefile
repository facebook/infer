
ANNOTATIONS_JAR = ../annotations/annotations.jar
JACKSON_JAR = ../../dependencies/java/jackson/jackson-2.2.3.jar
ANDROID_JAR = ../lib/java/android/android-19.jar
GUAVA_JAR = ../../dependencies/java/guava/guava-10.0.1-fork.jar
JSR_JAR = ../../dependencies/java/jsr-305/jsr305.jar

INFERJ = inferJ

TRACING_OUT = tracing_out
TRACING_REPORT = $(TRACING_OUT)/report.csv
TRACING_SOURCES = $(shell find codetoanalyze/java/tracing -name "*.java")
TRACING_JAVAC_CMD = javac -cp $(ANNOTATIONS_JAR) $(TRACING_SOURCES)
TRACING_CMD = $(INFERJ) -o $(TRACING_OUT) -a tracing $(TRACING_JAVAC_CMD)

INFER_OUT = infer_out
INFER_REPORT = $(INFER_OUT)/report.csv
INFER_SOURCES = $(shell find codetoanalyze/java/infer -name "*.java")
INFER_JAVAC_CMD = javac -cp $(JACKSON_JAR):$(ANDROID_JAR) $(INFER_SOURCES)
INFER_CMD = $(INFERJ) -o $(INFER_OUT) -a infer $(INFER_JAVAC_CMD)

ERADICATE_OUT = eradicate_out
ERADICATE_REPORT = $(ERADICATE_OUT)/report.csv
ERADICATE_SOURCES = $(shell find codetoanalyze/java/eradicate -name "*.java")
ERADICATE_JAVAC_CMD = javac -cp $(JSR_JAR):$(ANNOTATIONS_JAR):$(GUAVA_JAR):$(ANDROID_JAR) $(ERADICATE_SOURCES)
ERADICATE_CMD = $(INFERJ) -o $(ERADICATE_OUT) -a eradicate $(ERADICATE_JAVAC_CMD)

CHECKERS_OUT = checkers_out
CHECKERS_REPORT = $(CHECKERS_OUT)/report.csv
CHECKERS_SOURCES = $(shell find codetoanalyze/java/checkers -name "*.java")
CHECKERS_JAVAC_CMD = javac -cp $(ANNOTATIONS_JAR):$(GUAVA_JAR):$(ANDROID_JAR) $(CHECKERS_SOURCES)
CHECKERS_CMD = $(INFERJ) -o $(CHECKERS_OUT) -a checkers $(CHECKERS_JAVAC_CMD)

.PHONY: tracing infer eradicate checkers

all: tracing infer eradicate checkers


tracing:
	$(TRACING_CMD)

infer:
	$(INFER_CMD)

eradicate:
	$(ERADICATE_CMD)

checkers:
	$(CHECKERS_CMD)

clean:
	rm -rf $(TRACING_OUT)
	rm -rf $(INFER_OUT)
	rm -rf $(CHECKERS_OUT)
	rm -rf $(ERADICATE_OUT)
	find . -name "*.o" | xargs rm
	find . -name "*.class" | xargs rm
