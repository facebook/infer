MAKE = make
JAVA_MODELS = java
C_MODELS = c
CPP_MODELS = cpp
OBJC_MODELS = objc
CWD = $(shell pwd)
BINDIR = $(CWD)/../bin
LIBDIR = $(CWD)/../lib
PYTHONLIBDIR = $(LIBDIR)/python/inferlib
CAPTURELIBDIR = $(PYTHONLIBDIR)/capture
LIB_SPECS = $(LIBDIR)/specs

INFERANALYZE = $(BINDIR)/InferAnalyze
INFERCLANG = $(BINDIR)/InferClang
INFERJAVA = $(BINDIR)/InferJava
JAVA_SCRIPTS = $(addprefix $(PYTHONLIBDIR)/, jwlib.py analyze.py) \
               $(BINDIR)/infer
CLANG_SCRIPTS = $(addprefix $(CAPTURELIBDIR)/, make.py) \
                $(BINDIR)/infer

PLATFORM = $(shell uname)

JAVA_MODELS_JAR = ../lib/java/models.jar

JAVA_MODELS_SOURCES = $(shell find $(JAVA_MODELS)/src -name "*.java")
C_MODELS_SOURCES = $(shell find $(C_MODELS)/src -name "*.c")
CPP_MODELS_SOURCES = $(shell find $(CPP_MODELS)/src -name "*.cpp")
OBJC_MODELS_SOURCES = $(shell find $(OBJC_MODELS)/src -name "*.m"  -or  -name "*.c")

C_MODELS_FILE = $(LIB_SPECS)/c_models
CPP_MODELS_FILE = $(LIB_SPECS)/cpp_models
OBJC_MODELS_FILE = $(LIB_SPECS)/objc_models

.PHONY: java clang clean

java: $(JAVA_MODELS_JAR)

clang: $(C_MODELS_FILE) $(CPP_MODELS_FILE) $(OBJC_MODELS_FILE)

$(JAVA_MODELS_JAR): $(JAVA_MODELS_SOURCES) $(INFERANALYZE) $(INFERJAVA) $(JAVA_SCRIPTS)
	make -C $(JAVA_MODELS)

$(C_MODELS_FILE): $(C_MODELS_SOURCES) $(INFERANALYZE) $(INFERCLANG)
	make -C $(C_MODELS) install

$(CPP_MODELS_FILE): $(CPP_MODELS_SOURCES) $(INFERANALYZE) $(INFERCLANG)
	make -C $(CPP_MODELS) install

$(OBJC_MODELS_FILE): $(OBJC_MODELS_SOURCES) $(INFERANALYZE) $(INFERCLANG)
ifneq (, $(shell which xcode-select 2>&-))
	make -C $(OBJC_MODELS) install
else
	@echo "Xcode not installed or platform $(PLATFORM) not supported for objc models, skipping."
endif

clean:
	rm -f $(LIB_SPECS)/*.specs
	$(MAKE) -C $(JAVA_MODELS) clean
	$(MAKE) -C $(C_MODELS) clean
	$(MAKE) -C $(CPP_MODELS) clean
	$(MAKE) -C $(OBJC_MODELS) clean
