! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE ROOT_FRAC---------------------------------------------

MODULE root_frac_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ROOT_FRAC_MOD'

CONTAINS
! Subroutine Interface:
SUBROUTINE root_frac (ft,nlayer,dz,rootd,f_root)

USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_JULES
USE parkind1, ONLY: jprb, jpim
USE pftparm, ONLY: fsmc_mod
IMPLICIT NONE

! Description:
!     Calculates the fraction of the total plant roots within each
!     soil layer.

! Documentation : UM Documentation Paper 25


! Subroutine arguments:
!   Scalar arguments with intent(IN) :
INTEGER, INTENT(IN) ::                                                         &
 ft,                                                                           &
                      ! IN Plant functional type.
 nlayer
                      ! IN Number of soil layers.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 dz(nlayer),                                                                   &
                      ! IN Soil layer thicknesses (m).
 rootd                ! IN Rootdepth (m).

!   Array arguments with intent(OUT) :
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 f_root(nlayer)       ! OUT Fraction of roots in each soil
                      !     layer.
! Local scalars:
INTEGER ::                                                                     &
 n                    ! WORK Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
 ftot,                                                                         &
                      ! WORK Normalisation factor.
 ztot,                                                                         &
                      ! WORK Total depth of soil (m).
 z1,z2                ! WORK Depth of the top and bottom of the
                      !      soil layers (m).

! Local parameters:
REAL(KIND=real_jlslsm), PARAMETER :: p = 1.0
                      ! WORK Power describing depth dependence
                      !           of the root density profile.

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='ROOT_FRAC'

IF (LPROF_JULES) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)
z2 = 0.0
ztot = 0.0
ftot = 0.0

IF (fsmc_mod(ft) == 1) THEN
  DO n = 1,nlayer
    z1 = z2
    z2 = z2 + dz(n)
    ztot = ztot + dz(n)
    IF (z1 > rootd) THEN
      f_root(n) = 0.0
    ELSE IF (z2 < rootd) THEN
      f_root(n) = dz(n)
    ELSE
      f_root(n) = rootd - z1
    END IF
  END DO

  ftot = MIN(rootd, ztot)
  f_root(:) = f_root(:) / ftot
ELSE
  DO n = 1,nlayer
    z1 = z2
    z2 = z2 + dz(n)
    ztot = ztot + dz(n)
    f_root(n) = EXP(-p * z1 / rootd) - EXP(-p * z2 / rootd)

  END DO

  ftot = 1.0 - EXP(-p * ztot / rootd)
  DO n = 1,nlayer
    f_root(n) = f_root(n) / ftot
  END DO
END IF

IF (LPROF_JULES) CALL vernier_stop(vnr_handle_1)
RETURN
END SUBROUTINE root_frac

END MODULE root_frac_mod
