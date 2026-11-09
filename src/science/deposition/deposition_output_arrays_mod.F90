!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE deposition_output_arrays_mod

!-----------------------------------------------------------------------------
! Description:
!   Output arrays, 10 elements per line
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE ereport_mod,             ONLY: ereport
USE jules_print_mgr,         ONLY: jules_print, jules_format, jules_message
USE parkind1,                ONLY: jprb, jpim
USE um_types,                ONLY: real_jlslsm
USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_JULES

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = "DEPOSITION_OUTPUT_ARRAYS_MOD"

CONTAINS

!-------------------------------------------------------------------------------
SUBROUTINE deposition_output_array_char(n_output, output)

IMPLICIT NONE

INTEGER, INTENT(IN)           :: n_output         ! Size of array
CHARACTER(LEN=10), INTENT(IN) :: output(n_output) ! Output to be printed

! Local variables
INTEGER                       :: i, j, n_times

CHARACTER(LEN=*), PARAMETER   :: RoutineName='DEPOSITION_OUTPUT_ARRAY_CHAR'
INTEGER(KIND=vik) :: vnr_handle_1
!-------------------------------------------------------------------------------

IF (LPROF_JULES) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

jules_format = '(10A10)'
n_times = INT(n_output/10)

DO i = 0, n_times-1
  WRITE(jules_message, jules_format) (output(j), j=i*10+1, i*10+10)
  CALL jules_print(RoutineName, jules_message)
END DO
WRITE(jules_message, jules_format) (output(j), j=10*n_times+1, n_output)
CALL jules_print(RoutineName, jules_message)

IF (LPROF_JULES) CALL vernier_stop(vnr_handle_1)

RETURN

END SUBROUTINE deposition_output_array_char

!-------------------------------------------------------------------------------
SUBROUTINE deposition_output_array_int(n_output, output)

IMPLICIT NONE

INTEGER, INTENT(IN)           :: n_output         ! Size of array
INTEGER, INTENT(IN)           :: output(n_output) ! Output to be printed

! Local variables
INTEGER                       :: i, j, n_times

CHARACTER(LEN=*), PARAMETER   :: RoutineName='DEPOSITION_OUTPUT_ARRAY_INT'
INTEGER(KIND=vik) :: vnr_handle_2
!-------------------------------------------------------------------------------

IF (LPROF_JULES) CALL vernier_start(vnr_handle_2, ModuleName//':'//RoutineName)

jules_format = '(10I10)'
n_times = INT(n_output/10)

DO i = 0, n_times-1
  WRITE(jules_message, jules_format) (output(j), j=i*10+1, i*10+10)
  CALL jules_print(RoutineName, jules_message)
END DO
WRITE(jules_message, jules_format) (output(j), j=10*n_times+1, n_output)
CALL jules_print(RoutineName, jules_message)

IF (LPROF_JULES) CALL vernier_stop(vnr_handle_2)

RETURN

END SUBROUTINE deposition_output_array_int

!-------------------------------------------------------------------------------
SUBROUTINE deposition_output_array_real(n_output, output)

IMPLICIT NONE

INTEGER, INTENT(IN)           :: n_output              ! Size of array
REAL(KIND=real_jlslsm), INTENT(IN) :: output(n_output) ! Output to be printed

! Local variables
INTEGER                       :: i, j, n_times

CHARACTER(LEN=*), PARAMETER   :: RoutineName='DEPOSITION_OUTPUT_ARRAY_REAL'
INTEGER(KIND=vik) :: vnr_handle_3
!-------------------------------------------------------------------------------

IF (LPROF_JULES) CALL vernier_start(vnr_handle_3, ModuleName//':'//RoutineName)

jules_format = '(10(1PE16.8))'
n_times = INT(n_output/10)

DO i = 0, n_times-1
  WRITE(jules_message, jules_format) (output(j), j=i*10+1, i*10+10)
  CALL jules_print(RoutineName, jules_message)
END DO
WRITE(jules_message, jules_format) (output(j), j=10*n_times+1, n_output)
CALL jules_print(RoutineName, jules_message)

IF (LPROF_JULES) CALL vernier_stop(vnr_handle_3)

RETURN

END SUBROUTINE deposition_output_array_real

END MODULE deposition_output_arrays_mod
