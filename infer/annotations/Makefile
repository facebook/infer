CWD = $(shell pwd)
DEPENDENCIES = $(CWD)/../../dependencies
LIB = $(CWD)/../lib/java
JSR_JAR = $(DEPENDENCIES)/java/jsr-305/jsr305.jar
ANNOT_SOURCES = $(shell find com/facebook/infer/annotation -name "*.java")
PROCESSOR_SOURCES = $(shell find com/facebook/infer/annotprocess -name "*.java")
ANNOT_CLASSES = 'annot_classes'
PROCESSOR_CLASSES = 'processor_classes'

ANNOTATIONS_JAR = $(CWD)/annotations.jar
PROCESSOR_JAR = $(LIB)/processor.jar

all: $(ANNOTATIONS_JAR) $(PROCESSOR_JAR)

$(ANNOTATIONS_JAR): $(ANNOT_SOURCES)
	mkdir -p $(ANNOT_CLASSES)
	javac -cp $(JSR_JAR) $(ANNOT_SOURCES) -d $(ANNOT_CLASSES)
	cd $(ANNOT_CLASSES) && jar cvf $(ANNOTATIONS_JAR) com

$(PROCESSOR_JAR): $(PROCESSOR_SOURCES)
	mkdir -p $(PROCESSOR_CLASSES)
	javac $(PROCESSOR_SOURCES) -d $(PROCESSOR_CLASSES)
	jar cvMf processor.jar -C resources META-INF -C $(PROCESSOR_CLASSES) com
	mv processor.jar $(PROCESSOR_JAR)

clean:
	rm -rf $(ANNOT_CLASSES)
	rm -f $(ANNOTATIONS_JAR)
	rm -rf $(PROCESSOR_CLASSES)
	rm -f $(PROCESSOR_JAR)
