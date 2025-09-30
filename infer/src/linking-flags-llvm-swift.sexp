(-cclib -lLLVMIRPika
;  ...a circular dependency between these two libraries...
   -cclib -Wl,--start-group -cclib -lLLVMTransformUtils -cclib -lLLVMTransformPika -cclib -Wl,--end-group
;  ...and finally the remaining Pika dependencies.
   -cclib -lLLVMCodeGen -cclib -lLLVMAnalysis -cclib -lLLVMCore -cclib -lLLVMCASUtil
)
