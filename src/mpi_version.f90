PROGRAM check_mpi_version

  USE mpi

  IMPLICIT INTEGER (o)
  CHARACTER(LEN=3) :: major, minor, release

  WRITE(major, '(I3)') OMPI_MAJOR_VERSION
  WRITE(minor, '(I3)') OMPI_MINOR_VERSION
  WRITE(release, '(I3)') OMPI_RELEASE_VERSION

  PRINT*, 'OMPI_' // TRIM(ADJUSTL(major)) // '.' // TRIM(ADJUSTL(minor)) &
      // '.' // TRIM(ADJUSTL(release))

  IF (OMPI_MAJOR_VERSION == 1 .AND. OMPI_MINOR_VERSION == 10 &
      .AND. OMPI_RELEASE_VERSION == 1) THEN
    PRINT*, '*** ERROR ***'
    PRINT*, 'OpenMPI 1.10.1 detected. This contains a serious bug and ', &
        'should not be used.'
    STOP 1
  END IF

END PROGRAM check_mpi_version
