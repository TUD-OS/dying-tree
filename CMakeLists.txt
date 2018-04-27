cmake_minimum_required(VERSION 3.1.0)
project(dying)

find_package(MPI REQUIRED)
include_directories(${MPI_C_INCLUDE_PATH})

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
SET_PROPERTY(GLOBAL PROPERTY TARGET_SUPPORTS_SHARED_LIBS TRUE)

set(WRAP ${PROJECT_SOURCE_DIR}/wrap/wrap.py)
include(wrap/WrapConfig.cmake)

SET_SOURCE_FILES_PROPERTIES(
  ${CMAKE_CURRENT_BINARY_DIR}/dying_wrap.h
  PROPERTIES GENERATED 1)
SET_SOURCE_FILES_PROPERTIES(dying.c OBJECT_DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/dying_wrap.h)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

string(APPEND CMAKE_SHARED_LINKER_FLAGS " -Wl,--no-undefined")
string(APPEND CMAKE_C_FLAGS             " -fdiagnostics-color=always -fstrict-overflow -Wstrict-overflow")

add_wrapped_file(dying_wrap.h dying_wrap.w)
add_library(dying SHARED
  dying.c
  util.c
  corrected_tree.c
  corrected_binomial-tree.c
  corrected_binomial-tree_io.c
  corrected_gossip.c
  corrected_lame-tree.c
)
target_link_libraries(dying ${MPI_C_LIBRARIES})
