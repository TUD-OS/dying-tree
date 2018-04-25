cmake_minimum_required(VERSION 3.1.0)
project(dying)

find_package(MPI REQUIRED)
include_directories(${MPI_C_INCLUDE_PATH})

set(WRAP ${PROJECT_SOURCE_DIR}/wrap/wrap.py)
include(wrap/WrapConfig.cmake)

SET_SOURCE_FILES_PROPERTIES(
  ${CMAKE_CURRENT_BINARY_DIR}/dying_wrap.h
  PROPERTIES GENERATED 1)
SET_SOURCE_FILES_PROPERTIES(dying.c OBJECT_DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/dying_wrap.h)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_wrapped_file(dying_wrap.h dying_wrap.w)
add_library(dying SHARED
  dying.c
  corrected_tree.c
  dying_wrap.h)
target_link_libraries(dying ${MPI_C_LIBRARIES})
