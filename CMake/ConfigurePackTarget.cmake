execute_process(
   COMMAND git rev-parse --show-toplevel
   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
   OUTPUT_VARIABLE GIT_WORK_TREE
   OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(
   COMMAND git rev-parse --git-dir
   WORKING_DIRECTORY ${GIT_WORK_TREE}
   OUTPUT_VARIABLE GIT_DIR
   OUTPUT_STRIP_TRAILING_WHITESPACE)

# Force GIT_DIR to be an absolute path
if(NOT IS_ABSOLUTE ${GIT_DIR})
   # FIXME: These unset commands will break any project that rely on
   #        GIT_DIR1 being defined in the cache.
   #        Ideally, we should create a temporary unique variable
   #        name, similar to the 'mktemp' command, but I don't know
   #        how to do that in cmake. (AMW)
   unset(GIT_DIR1)
   unset(GIT_DIR1 CACHE)
   find_file(GIT_DIR1 ${GIT_DIR} PATHS ${GIT_WORK_TREE} NO_DEFAULT_PATH)
   set(GIT_DIR ${GIT_DIR1})
   unset(GIT_DIR1 CACHE)
endif()

configure_file(${PACK_CMAKE_IN} pack.cmake)
add_custom_command(
   OUTPUT ${INFO_FILE}
   COMMAND ${CMAKE_COMMAND} -E remove -f ${FORT}
   COMMAND ${CMAKE_COMMAND} -P pack.cmake
   DEPENDS ${PACK_PY} ${CMAKE_CURRENT_BINARY_DIR}/pack.cmake ${SOURCE_ALL}
   VERBATIM)
