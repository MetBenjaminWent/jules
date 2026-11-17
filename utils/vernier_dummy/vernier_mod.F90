#if ! defined(VERNIER_BUILD)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE vernier_mod

  USE, INTRINSIC :: iso_c_binding, ONLY: c_long
  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER :: vik = c_long

  PUBLIC :: vernier_start, vernier_stop, vik

CONTAINS

  SUBROUTINE vernier_start(hash_in, region_name)

    CHARACTER(LEN=*), INTENT(IN) :: region_name
    INTEGER(KIND=vik), INTENT(IN) :: hash_in

    RETURN
  END SUBROUTINE vernier_start

  SUBROUTINE vernier_stop(hash_in)
    INTEGER(KIND=vik), INTENT(IN) :: hash_in

    RETURN
  END SUBROUTINE vernier_stop

END MODULE vernier_mod
#endif
