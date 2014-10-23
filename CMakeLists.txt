project(SDF Fortran)
cmake_minimum_required(VERSION 2.8.8)
find_package(MPI REQUIRED)

set(INHERIT_FLAGS OFF CACHE BOOL "Inherit compiler flags")

if(NOT INHERIT_FLAGS)
   if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
      set(CPU "" CACHE STRING "Which CPU? (see pgf90 man page for options)")
      if(CPU)
         set(CPU "-tp ${CPU}")
      endif()

      set(CMAKE_Fortran_FLAGS
	      "-Mnodefaultunit -Ktrap=fp -mcmodel=medium -Mdclchk ${CPU} ${MPI_Fortran_COMPILE_FLAGS}"
         CACHE STRING "Fortran flags" FORCE)
      set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -Mvect -Munroll"
         CACHE STRING "Fortran flags for release build" FORCE)
      set(CMAKE_Fortran_FLAGS_DEBUG
         "-O0 -g -Ktrap=denorm -Mbounds -Mchkfpstk -Mdepchk -Mstandard -C"
         CACHE STRING "Fortran flags for debug build" FORCE)

   elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
      set(CMAKE_Fortran_FLAGS "-fpe0"
         CACHE STRING "Fortran flags" FORCE)
      set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ip -vec-report0"
         CACHE STRING "Fortran flags for release build" FORCE)
      set(CMAKE_Fortran_FLAGS_DEBUG
         "-O0 -g -u -ftrapuv -traceback -nothreads -fltconsistency -C -warn -save-temps -fpic -Wl,-no_pie"
         CACHE STRING "Fortran flags for debug build" FORCE)

   elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
      set(CMAKE_Fortran_FLAGS "-Wall -frecord-marker=4 -mcmodel=medium"
         CACHE STRING "Fortran flags" FORCE)
      set(CMAKE_Fortran_FLAGS_RELEASE "-O3"
         CACHE STRING "Fortran flags for release build" FORCE)
      set(CMAKE_Fortran_FLAGS_DEBUG
         "-O0 -g -fimplicit-none -fbounds-check -fbacktrace -Wextra -ffpe-trap=invalid,zero,overflow -pedantic"
         CACHE STRING "Fortran flags for debug build" FORCE)

   endif()
endif()

set(TOPLEVEL "${CMAKE_CURRENT_SOURCE_DIR}")
file(GLOB SOURCE_ALL CMakeLists.txt Makefile*)

set(PACK_SOURCE_CODE 1 CACHE STRING "Pack source code")
set(PACK_GIT_DIFF 1 CACHE STRING "Pack git diff")
set(PACK_GIT_DIFF_FROM_ORIGIN 1 CACHE STRING "Pack git diff from origin")
set(GENERATE_CHECKSUM 1 CACHE 1 "Generate checksum")
set(F77_OUTPUT 0)
set(GIT_DIR ${CMAKE_CURRENT_SOURCE_DIR})

include_directories(${MPI_Fortran_INCLUDE_PATH})
add_subdirectory(src lib)
