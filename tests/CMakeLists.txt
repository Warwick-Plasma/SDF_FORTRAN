# Check whether the MPI Fortran 2008 interfaces are available
add_executable(test_sdf_f08_available EXCLUDE_FROM_ALL test_sdf_f08_available.f90)
target_link_libraries(test_sdf_f08_available sdf ${MPI_Fortran_LIBRARIES})
add_test(
    NAME SDF-sdf_f08-available
    COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target test_sdf_f08_available
)
option(NO_MPI_F08 "Indicate the MPI Fortran 2008 interface is not expected" OFF)
set_tests_properties(
    SDF-sdf_f08-available
    PROPERTIES LABELS sdf_f08
               WILL_FAIL ${NO_MPI_F08}
)
