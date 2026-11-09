! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE ELEV_HTC------------------------------------------------

! Description:
!     Updates tiled bedrock temperatures for land ice. No external
!     subroutines are called. Modified from ice_htc @ julesvn4.1
!     using only the explicit scheme for a single layer

MODULE elev_htc_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ELEV_HTC_MOD'

CONTAINS

SUBROUTINE elev_htc ( lice_pts, npnts, nsurft, dz, timestep, lice_index,       &
                      snow_soil_htf, tsurf_elev_surft, l_lice_surft )

USE jules_snow_mod, ONLY:                                                      &
  snow_hcap

USE jules_soil_mod, ONLY:                                                      &
  hcapdeep

USE jules_surface_types_mod, ONLY: ntype

USE parkind1, ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_JULES

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  lice_pts,                                                                    &
    ! Number of land ice points.
  npnts,                                                                       &
    ! Number of land points.
  nsurft
    ! Number of tiles.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz,                                                                          &
    ! Thicknesses of the bedrock layer (m).
  timestep
    ! Model timestep (s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  lice_index(npnts)
  ! Array of ice points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  snow_soil_htf(npnts,nsurft)
    ! Net downward surface heat flux (W/m2).

LOGICAL, INTENT(IN) :: l_lice_surft(ntype)
!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  tsurf_elev_surft(npnts,nsurft)
    ! Sub-surface temperatures (K).

!-----------------------------------------------------------------------------
! Local scalars:
!-----------------------------------------------------------------------------
INTEGER  ::                                                                    &
  i, j, n
    ! Loop counters:
    ! i for land point
    ! j for tile point
    ! n for surface tile

REAL(KIND=real_jlslsm)     ::                                                  &
  recip_snowcap,                                                               &
    ! Reciprocal of the heat capacity of the glaciated subsurface.
  recip_deepcap
    ! Reciprocal of the heat capacity of the non-glaciated subsurface.


INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='ELEV_HTC'

IF (LPROF_JULES) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

!-----------------------------------------------------------------------------
! Original explicit scheme
!
!-----------------------------------------------------------------------------
! Update the sub-surface temperatures
!-----------------------------------------------------------------------------
!CDIR NOVECTOR
!   CDIR$ IVDEP here would force vectorization but has been seen to change
!         results in similar code (eg ice_htc)

recip_snowcap = 1.0 / (snow_hcap * dz)
recip_deepcap = 1.0 / (hcapdeep * dz)

DO j = 1,lice_pts
  i = lice_index(j)
  DO n = 1,nsurft
    IF (l_lice_surft(n)) THEN
      tsurf_elev_surft(i,n) = tsurf_elev_surft(i,n) +                          &
                              recip_snowcap *                                  &
                              snow_soil_htf(i,n) * timestep
    ELSE
      tsurf_elev_surft(i,n) = tsurf_elev_surft(i,n) +                          &
                              recip_deepcap *                                  &
                              snow_soil_htf(i,n) * timestep
    END IF
  END DO
END DO

IF (LPROF_JULES) CALL vernier_stop(vnr_handle_1)

RETURN

END SUBROUTINE elev_htc
END MODULE elev_htc_mod
