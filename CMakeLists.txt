cmake_minimum_required(VERSION 3.1.0)
project(dying)

find_package(MPI REQUIRED)
include_directories(${MPI_C_INCLUDE_PATH})

IF(PIC_BUILD)
  SET(CMAKE_POSITION_INDEPENDENT_CODE ON)
  SET_PROPERTY(GLOBAL PROPERTY TARGET_SUPPORTS_SHARED_LIBS TRUE)
  STRING(APPEND CMAKE_SHARED_LINKER_FLAGS " -shared -fPIC")
  STRING(APPEND CMAKE_C_FLAGS             " -shared -fPIC")
ENDIF(PIC_BUILD)

set_property(GLOBAL PROPERTY C_STANDARD 99)

set(WRAP ${PROJECT_SOURCE_DIR}/wrap/wrap.py)
include(wrap/WrapConfig.cmake)

SET_SOURCE_FILES_PROPERTIES(
  ${CMAKE_CURRENT_BINARY_DIR}/dying_wrap.h
  PROPERTIES GENERATED 1)
SET_SOURCE_FILES_PROPERTIES(dying.c OBJECT_DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/dying_wrap.h)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

IF(CMAKE_COMPILER_IS_GNUCXX)
  string(APPEND CMAKE_SHARED_LINKER_FLAGS " -Wl,--no-undefined")
  #string(APPEND CMAKE_SHARED_LINKER_FLAGS_DEBUG " -fsanitize=address -fsanitize=undefined")
  string(APPEND CMAKE_C_FLAGS             " -fdiagnostics-color=always -fstrict-overflow -Wstrict-overflow")
  #string(APPEND CMAKE_C_FLAGS_DEBUG       " -fsanitize=address -fsanitize=undefined")
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

SET(CORRT_SRC
  util.c
  corrected_tree.c
  corrected_binomial-tree.c
  corrected_binomial-tree_io.c
  corrected_gossip.c
  corrected_lame-tree.c
)

add_wrapped_file(dying_wrap.h dying_wrap.w)
add_library(dying SHARED
  dying.c
  ${CORRT_SRC}
)
target_link_libraries(dying ${MPI_C_LIBRARIES})

add_executable(corrt_profile
  profile.c
  ${CORRT_SRC}
)
target_link_libraries(corrt_profile ${MPI_C_LIBRARIES})
