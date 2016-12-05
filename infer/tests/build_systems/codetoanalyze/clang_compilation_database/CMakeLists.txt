cmake_minimum_required (VERSION 3.1)
set(CMAKE_CXX_STANDARD 11)
project (HELLO)
add_library (Hello hello.cpp lib1.cpp lib2.cpp lib3.cpp)
