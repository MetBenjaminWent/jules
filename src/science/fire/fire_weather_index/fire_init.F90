MODULE fire_init_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FIRE_INIT_MOD'

CONTAINS

SUBROUTINE fire_init()

! < Module imports >
USE metstats_mod,       ONLY: metstats_flag
USE fire_mod,           ONLY: fire_cntl, l_fire_weather_index

USE jules_soil_mod,     ONLY: zsmc !Depth of layer for soil moisture diag (m)

USE parkind1,           ONLY: jprb, jpim
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_JULES

IMPLICIT NONE
!
! Description:
!   Deals with setting up the module variables.
!
! Method:
!   Must be called before fire_calc_daily()
!   Sets flags for the metstats module
!   Allocates and sets initial values for fire variables
!   Only called by JULES-standalone. The UM does other stuff.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

  !Arguments- none :-)

!Local variables
INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='FIRE_INIT'

! End of header
IF (LPROF_JULES) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

! ------------------------------------------
! Activate the required metstats

IF (l_fire_weather_index) THEN

  IF (fire_cntl%mcarthur%flag .OR. fire_cntl%nesterov%flag) THEN
    metstats_flag%prec_tot_00h = .TRUE.
  END IF

  IF (fire_cntl%mcarthur%flag) THEN
    metstats_flag%temp_max_00h = .TRUE.
    metstats_flag%rhum_min_00h = .TRUE.
    metstats_flag%wind_ave_00h = .TRUE.
  END IF

  IF (fire_cntl%canadian%flag) THEN
    metstats_flag%temp_pnt_12h = .TRUE.
    metstats_flag%rhum_pnt_12h = .TRUE.
    metstats_flag%wind_pnt_12h = .TRUE.
    metstats_flag%prec_tot_12h = .TRUE.
  END IF

  IF (fire_cntl%nesterov%flag) THEN
    metstats_flag%temp_ave_00h = .TRUE.
    metstats_flag%dewp_ave_00h = .TRUE.
  END IF

  ! ------------------------------------------
  ! Set up any control variables

  IF (fire_cntl%mcarthur%flag) THEN
    !Calculate the value of a coefficient needed to scale soil moisture values
    !to fit the soil column depth expected by the drought index.
    fire_cntl%mcarthur%smc_coeff = 1.0 /                                       &
     (1.0 + ((zsmc - fire_cntl%mcarthur%targ_depth)                            &
      / fire_cntl%mcarthur%targ_depth))
  END IF

  !Nothing to do for canadian

  !Nothing to do for nesterov

END IF !l_fire_weather_index

IF (LPROF_JULES) CALL vernier_stop(vnr_handle_1)
RETURN
END SUBROUTINE fire_init
END MODULE fire_init_mod
