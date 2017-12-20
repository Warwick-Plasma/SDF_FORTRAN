set(SOURCES
   sdf.f90
   sdf_common.f90
   sdf_control.f90
   sdf_input.f90
   sdf_input_cartesian.f90
   sdf_input_cartesian_r4.f90
   sdf_input_cartesian_r8.f90
   sdf_input_cartesian_ru.f90
   sdf_input_point.f90
   sdf_input_point_r4.f90
   sdf_input_point_r8.f90
   sdf_input_point_ru.f90
   sdf_input_r4.f90
   sdf_input_r8.f90
   sdf_input_ru.f90
   sdf_input_station.f90
   sdf_input_station_r4.f90
   sdf_input_station_r8.f90
   sdf_input_station_ru.f90
   sdf_input_util.f90
   sdf_job_info.f90
   sdf_output.f90
   sdf_output_cartesian.f90
   sdf_output_cartesian_r4.f90
   sdf_output_cartesian_r8.f90
   sdf_output_cartesian_ru.f90
   sdf_output_point.f90
   sdf_output_point_r4.f90
   sdf_output_point_r8.f90
   sdf_output_point_ru.f90
   sdf_output_r4.f90
   sdf_output_r8.f90
   sdf_output_ru.f90
   sdf_output_source.f90
   sdf_output_station.f90
   sdf_output_station_r4.f90
   sdf_output_station_r8.f90
   sdf_output_station_ru.f90
   sdf_output_util.f90
   sdf_md5.f90
   )

execute_process(COMMAND ${CMAKE_Fortran_COMPILER} --version
  COMMAND grep [a-zA-Z] COMMAND head -n 1
  OUTPUT_VARIABLE FC_INFO OUTPUT_STRIP_TRAILING_WHITESPACE)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
   set(FFLAGS ${CMAKE_Fortran_FLAGS_DEBUG})
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
   set(FFLAGS ${CMAKE_Fortran_FLAGS_RELEASE})
else()
   set(FFLAGS ${CMAKE_Fortran_FLAGS})
endif()

set(INFO_FILE sdf_source_info.f90)

set(PACK_OPTS ${GIT_DIR} sdf ${PACK_SOURCE_CODE} ${PACK_GIT_DIFF}
   ${PACK_GIT_DIFF_FROM_ORIGIN} ${GENERATE_CHECKSUM} ${F77_OUTPUT})

include(${CONFIGURE_PACK_TARGET})
list(APPEND SOURCES ${INFO_FILE})

set(MCMODEL "-mcmodel=medium")
if(APPLE)
  set(MCMODEL " ")
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
   set(INFO_FLAGS "-Wno-conversion -fno-range-check")
   set_source_files_properties(${INFO_FILE} PROPERTIES COMPILE_FLAGS
      ${INFO_FLAGS})
endif()

if(NOT TARGET sdf)
   add_library(sdf ${SOURCES})
   set_target_properties(
       sdf
       PROPERTIES
       COMPILE_FLAGS ${MCMODEL}
       Fortran_MODULE_DIRECTORY include)
   install(TARGETS sdf DESTINATION lib COMPONENT develop)
   install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include  DESTINATION . COMPONENT develop)
endif()
