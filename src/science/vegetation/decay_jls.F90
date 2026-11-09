! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE decay_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DECAY_MOD'

CONTAINS

!--------------------------------------------------------------------
! Subroutine DECAY --------------------------------------------------
!
! Purpose : Updates carbon contents of the soil.
!
!--------------------------------------------------------------------
SUBROUTINE decay (land_pts, trif_pts, trif_index, forw, r_gamma,               &
                  dpc_dcs, pc, cs)

USE descent, ONLY: denom_min
USE jules_soil_mod, ONLY: cs_min

USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_JULES

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts
    ! Number of points on which TRIFFID may operate.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  forw,                                                                        &
    ! Forward timestep weighting.
  r_gamma
    ! Inverse timestep (/360days).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dpc_dcs(land_pts,4),                                                         &
    ! Rate of change of PC with soil carbon (yr).
  pc(land_pts,4)
    ! Net carbon flux into the soil (kg C/m2/360days).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  cs(land_pts,4)  ! Soil carbon (kg C/m2).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l,t,n  ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  denom,                                                                       &
    ! Denominator of update equation.
  numer
    ! Numerator of the update equation.

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='DECAY'

!-----------------------------------------------------------------------------
!end of header
IF (LPROF_JULES) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

DO n = 1,4
  DO t = 1,trif_pts
    l = trif_index(t)

    numer = pc(l,n)
    denom = r_gamma + forw * dpc_dcs(l,n)
    denom = MAX(denom,denom_min)

    cs(l,n) = cs(l,n) + numer / denom

    cs(l,n) = MAX(cs_min,cs(l,n))

  END DO
END DO

IF (LPROF_JULES) CALL vernier_stop(vnr_handle_1)
RETURN
END SUBROUTINE decay

END MODULE decay_mod
