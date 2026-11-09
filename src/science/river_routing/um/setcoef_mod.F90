#if defined(UM_JULES)
!Huw Lewis (MO), Jan 2015
!DEPRECATED CODE
!This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!Future developments will supercede these subroutines, and as such they
!should be considered deprecated. They will be retained in the codebase to
!maintain backward compatibility with functionality prior to
!UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
MODULE setcoef_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SETCOEF_MOD'

CONTAINS



SUBROUTINE setcoef(nx, ny, rlen, rvel, ratmed, rc, jmax)


USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_JULES
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE
!
!     calculate coefficient 'c' in the routing model
!     basically by c = u/d, but actual d is assumed to d*ratmed
!     considering the meandering of the river.
!
!     c has dimension of [1/s]
!     sea grids: c = 0.0
!
INTEGER :: nx, ny, i, j, jmax
REAL(KIND=real_jlslsm) :: rlen(nx, ny), rvel(nx, ny), ratmed, rc(nx, ny)

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='SETCOEF'

!
IF (LPROF_JULES) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)
DO i= 1, nx
  DO j = 1, ny
    IF (rlen(i,j) >  0.0) THEN
      rc(i, j) = rvel(i,j) / (rlen(i, j) * ratmed)
    ELSE
      rc(i, j) = 0.0
    END IF
  END DO
END DO
IF (LPROF_JULES) CALL vernier_stop(vnr_handle_1)
RETURN
END SUBROUTINE setcoef
END MODULE setcoef_mod
#endif
