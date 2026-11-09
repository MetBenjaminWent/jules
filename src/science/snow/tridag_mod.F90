! *****************************COPYRIGHT*******************************

! (c) [University of Edinburgh]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC237]

! *****************************COPYRIGHT*******************************
!  SUBROUTINE TRIDAG---------------------------------------------------

! Description:
!     Solve tridiagonal matrix equation

! Subroutine Interface:
MODULE tridag_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='TRIDAG_MOD'

CONTAINS

SUBROUTINE tridag(nvec, nmax, a, b, c, r, s)

USE vernier_mod, ONLY: vernier_start, vernier_stop, vik
USE vernier_timing_mod, ONLY: LPROF_JULES
USE parkind1, ONLY: jprb, jpim

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nvec,                                                                        &
    ! Vector length.
  nmax
    ! Maximum vector length.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  a(nmax),                                                                     &
    ! Below-diagonal matrix elements.
  b(nmax),                                                                     &
    ! Diagonal matrix elements.
  c(nmax),                                                                     &
    ! Above-diagonal matrix elements.
  r(nmax)
    ! Matrix equation rhs.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  s(nmax)        ! Solution vector.

!-----------------------------------------------------------------------------
! Local scalars
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  n              ! loop counter

REAL(KIND=real_jlslsm) ::                                                      &
  beta           ! work

!-----------------------------------------------------------------------------
! Local arrays
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  gamm(nvec)    ! work

INTEGER(KIND=vik) :: vnr_handle_1

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIDAG'
!-----------------------------------------------------------------------

IF (LPROF_JULES) CALL vernier_start(vnr_handle_1, ModuleName//':'//RoutineName)

beta = b(1)
s(1) = r(1) / beta

DO n = 2,nvec
  gamm(n) = c(n-1) / beta
  beta    = b(n) - a(n) * gamm(n)
  s(n)    = ( r(n) - a(n) * s(n-1) ) / beta
END DO

DO n = nvec - 1,1,-1
  s(n) = s(n) - gamm(n+1) * s(n+1)
END DO

IF (LPROF_JULES) CALL vernier_stop(vnr_handle_1)
RETURN

END SUBROUTINE tridag
END MODULE tridag_mod
