cmake_minimum_required(VERSION 3.20)
project(ReplaceAbsoluteAddress LANGUAGES CXX C)

# Use C++17.
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Enable RTTI (required since Clang libraries use RTTI) and set the DWARF version.
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")

# Locate LLVM and Clang packages.
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Found Clang ${CLANG_VERSION}")

# Add LLVM and Clang include directories.
include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${CLANG_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Set the source file.
set(SOURCES ReplaceAbsoluteAddress.cpp)

# Create the executable.
add_executable(replace-absaddr ${SOURCES})

# Link against a comprehensive set of Clang and LLVM libraries.
target_link_libraries(replace-absaddr
  PRIVATE
    clangTooling
    clangToolingCore
    clangBasic
    clangFrontend
    clangASTMatchers
    clangAST
    clangLex
    clangDriver
    LLVMCore
    LLVMSupport
    LLVMPasses 
    LLVMIRReader
)

message(STATUS "CXX Flags: ${CMAKE_CXX_FLAGS}")
