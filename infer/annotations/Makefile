DEPENDENCIES = ../../dependencies
JSR_JAR = $(DEPENDENCIES)/java/jsr-305/jsr305.jar
ANNOT_SOURCES = `find com/facebook/infer/annotation -name "*.java"`
PROCESSOR_SOURCES = `find com/facebook/infer/annotprocess -name "*.java"`
ANNOT_CLASSES = 'annot_classes'
PROCESSOR_CLASSES = 'processor_classes'

all: annotations processor

annotations:
	mkdir -p $(ANNOT_CLASSES)
	javac -cp $(JSR_JAR) $(ANNOT_SOURCES) -d $(ANNOT_CLASSES)
	cd $(ANNOT_CLASSES) && jar cvf annotations.jar com && cp annotations.jar ../

processor:
	mkdir -p $(PROCESSOR_CLASSES)
	javac $(PROCESSOR_SOURCES) -d $(PROCESSOR_CLASSES)
	cd $(PROCESSOR_CLASSES) && cp -r ../resources/META-INF . && jar cvMf processor.jar com META-INF && cp processor.jar ../

clean:
	rm -r $(ANNOT_CLASSES)
	rm annotations.jar
	rm -r $(PROCESSOR_CLASSES)
	rm processor.jar
