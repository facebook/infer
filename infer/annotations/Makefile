DEPENDENCIES = ../../dependencies
JSR_JAR = $(DEPENDENCIES)/java/jsr-305/jsr305.jar
CLASSES_OUT = classes
SOURCES = `find . -name "*.java"`

all:
	mkdir -p classes
	javac -cp $(JSR_JAR) $(SOURCES)
	jar cvf annotations.jar $(SOURCES) `find . -name "*.class"`

clean:
	rm -rvf `find . -name "*.class"`
	rm -rvf annotations.jar
