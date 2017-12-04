cmake_minimum_required(VERSION 3.8)
project(OpenTC)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_CXX_FLAGS "-Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "-g")
set(CMAKE_CXX_FLAGS_RELEASE "-Os")

set(CMAKE_C_STANDARD 99)

set(SOURCE_FILES src/OpenTC.h src/OpenTC.c)
add_library(opentc ${SOURCE_FILES})