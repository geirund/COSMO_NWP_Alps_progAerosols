!+ Data module for global KIND type parameters
!-------------------------------------------------------------------------------

MODULE data_parameters

!-------------------------------------------------------------------------------
!
! Description:
!  Global parameters for defining the KIND types of the real- and integer-
!  variables are defined.
!
! Current Code Owner: DWD, Ulrich Schaettler
!  phone:  +49  69  8062 2739
!  fax:    +49  69  8062 3721
!  email:  ulrich.schaettler@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.1        1998/03/11 Ulrich Schaettler
!  Initial release
! 1.8        1998/08/03 Ulrich Schaettler
!  Eliminated intgribf, intgribc, irealgrib, iwlength and put it to data_io.
! 1.10       1998/09/29 Ulrich Schaettler
!  Eliminated parameters for grid point and diagnostic calculations.
! 3.13       2004/12/03 Ulrich Schaettler
!  Introduced intgribf, intgribc, irealgrib, iwlength (again)
! 3.18       2006/03/03 Ulrich Schaettler
!  Introduced KIND parameters idouble, isingle for generic formulation of
!  some utility routines
! V4_13        2010/05/11 Michael Gertz
!  Adaptions to SVN
! V4_27        2013/03/19 Astrid Kerkweg, Ulrich Schaettler
!  MESSy interface introduced: use MESSy KIND definitions
! V4_28        2013/07/12 Ulrich Schaettler
!  Implemented KIND parameters int_ga for grib_api interface (number of bytes,
!   which could be 4 or 8 byte integers)
!  Implemented global KIND parameter int_dp for 8 byte integers
! V4_29        2013/10/04 Astrid Kerkweg, Ulrich Schaettler
!  Unification of MESSy interfaces and COSMO Tracer structure:
!   can remove all ifdef MESSY here
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================

#ifdef GRIBAPI
USE grib_api
#endif

  !
  ! Kind parameters of Real variables (precision)
  ! Use those of mo_kind to ensure consistency throughout the model
  !
  USE mo_kind, ONLY: &
    wp, & ! working precision | wp = sp/dp if -DSINGLEPRECISION is/is not set
    dp, & ! double  precision | dp = SELECTED_REAL_KIND(13)
    sp    ! single  precision | wp = SELECTED_REAL_KIND(6)

!==============================================================================

IMPLICIT NONE

!==============================================================================

! 1. KIND-Parameters for the Program:
! -----------------------------------

!!
!! Use definitions from mo_kind instead to make sure the same definitions
!! are used everywhere in the model. 
!!
!!  INTEGER, PARAMETER       ::                                           &
!! RUS use KIND parameters from mo_kind
!!       sp = SELECTED_REAL_KIND (12,200),                                & 
!!       dp = SELECTED_REAL_KIND ( 6, 37),                                &
!!#ifdef SINGLEPRECISION
!!       wp = sp
!!#else
!!       wp = dp
!!#endif
!!                     ! number of desired significant digits for
!!                     ! real variables
!!                     ! (12,200) corresponds to 8 byte real variables
!!                     ! (6,37)   corresponds to 8 byte real variables
!!

  INTEGER, PARAMETER       ::                                         &

       iintegers = KIND  (1)
                     ! kind-type parameter of the integer values
                     ! corresponds to the default integers

! 2. KIND-Parameters for the variables in the GRIB-library
! --------------------------------------------------------

  INTEGER, PARAMETER       ::                                         &
    intgribf  = KIND(1),                                              &
!   intgribf  = 4,      &  ! (if using libgrib1 on the T3E)
       ! Kind type for Fortran integer variables used in the GRIB library
       ! this normally is the Standard integer with the exception of using
       ! "libgrib1" (former supplib) on a machine with 8 byte INTEGER default
       ! (like Cray-machines; then intgribf has be set to 32-bit INTEGER).

    intgribc  = KIND(1),                                              &
       ! Kind type for C integer variables used in the GRIB library
       ! this always is the Standard integer

    irealgrib = KIND(1.0)
       ! Kind type for Fortran real variables used in the GRIB library
       ! this is the Standard real of the machine


  INTEGER                  ::                                         &
       ! this variable has to be set at the beginning of the program
       ! (at the beginning of organize_data)
    iwlength   ! length of integers used in the griblib in byte
               ! 8: for dwdlib on Cray PVP and T3E systems
               ! 4: for dwdlib on SGI systems and griblib on all systems

!!
!! Not necessary anymore as sp and dp are now provided anyway.
!! isingle and idouble have been renamed to sp and dp where they're used.
!!
!!! 3. KIND-Parameters for the generic formulation of some utility routines:
!!! ------------------------------------------------------------------------
!!
!!    ! The distinction between wp (working precision) and irealgrib
!!    ! is not enough, because it could be possible that these KIND parameters
!!    ! are the same. Compilers could get in trouble then, because they could
!!    ! not decide, which routine to take then.
!!    ! Therefore we define the KIND parameters idouble (for double precision
!!    ! or 8 byte reals) and isinge (for single precision, or 4 byte reals)
!!
!!  INTEGER, PARAMETER       ::                                         &
!!       idouble   = dp,                                                &
!!       isingle   = sp
!!

! 4. KIND-Parameters for different INTEGER precision:
! ---------------------------------------------------

  INTEGER, PARAMETER       ::                                         &
       int_dp    = SELECTED_INT_KIND (12),                            &
               ! should represent integers up to 10**12
               ! which should be a INTEGER*8 (in the old notation)

! integer precision necessary for grib_api in interfaces where length of
! message in bytes is involved
#ifdef GRIBAPI
       int_ga    = kindOfSize              ! should be INTEGER *8 where necessary
#else
       int_ga    = SELECTED_INT_KIND (8)   ! INTEGER *4
#endif

! 5. Precision-dependent security parameters (epsilons):
! ------------------------------------------------------
  
  REAL (KIND=wp), PARAMETER  ::                                      &
       repsilon = 1.0E8_wp*TINY(1.0_wp),                             &
              !
              ! Very small number near zero.
              ! To be used mainly to avoid division by zero, e.g.
              ! eps_div = repsilon ; x = y / MAX(z,eps_div) ! for z > 0.
              ! Note that the factor 1.0E-8 has been chosen rather
              ! arbitrarily to get some distance to zero to account
              ! for the magnitude of the dividend, which might be 1.0E5
              ! in case of pressure, for instance.
              !

       rprecision = 10.0_wp**(-PRECISION(1.0_wp))
              !
              ! Precision of 1.0 in additions/subtractions.
              ! To be used for instance to check equality of reals, e.g.
              ! eps_cmpr = rprecision ; IF (ABS(a-b) < eps_cmpr) equal=.true.,
              ! or to increase the magnitude of an epsilon only in SP, e.g.
              ! epsilon = MAX(1.0E-8_wp,rprecision).

              !
              ! Approximate magnitudes:
              ! -----------------------
              !
              !       | repsilon | rprecision
              !   ----+----------+------------
              !    SP |  1.0E-30 |   1.0E-7
              !   ----+----------+------------
              !    DP | 1.0E-300 +  1.0E-16
              !

!==============================================================================

END MODULE data_parameters
