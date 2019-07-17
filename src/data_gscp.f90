!+ Data module for variables of the grid scale parameterization
!------------------------------------------------------------------------------

MODULE data_gscp

!------------------------------------------------------------------------------
!
! Description:
!  This module contains variables that are used in the grid scale 
!  parameterizations (Microphysics). 
!
! Current Code Owner: DWD, Axel Seifert
!  phone:  +49  69  8062 2729
!  fax:    +49  69  8062 3721
!  email:  ulrich.schaettler@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 3.22       2007/01/24 Axel Seifert
!  Initial Release
! V4_5         2008/09/10 Ulrich Schaettler
!  Added variables mu_rain and cloud_num, which are now Namelist variables
! V4_13        2010/05/11 Michael Gertz
!  Adaptions to SVN
! V4_14        2010/06/14 Axel Seifert
!  Introduced v0snow as global variable
! V4_20        2011/08/31 Axel Seifert
!  Moved some global variables from src_gscp to data_gscp
! V4_21        2011/12/06 Axel Seifert
!  Additional variable rain_n0_factor
! V4_27        2013/03/19 Ulrich Schaettler
!  Modified default values of some tuning constants to reflect settings of COSMO-EU
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================
!
! Declarations:
!
! Modules used:

USE data_parameters, ONLY :   &
    wp,        & ! KIND-type parameter for real variables
    iintegers    ! KIND-type parameter for standard integer variables

!==============================================================================

IMPLICIT NONE

!==============================================================================

!for lINprog
REAL (KIND=wp) :: sumIN_t0,sumIN1_t0,sumIN2_t0,sumIN3_t0,sumIN4_t0,sumIN5_t0,sumIN6_t0,sumIN7_t0,&
                  sumIN8_t0,sumIN9_t0,sumIN10_t0,sumIN11_t0,sumIN12_t0,sumIN13_t0,sumIN14_t0,&
                  sumIN15_t0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15
!for lCCNprog
REAL (KIND=wp) :: sumCCN_t0,sumCCN1_t0,sumCCN2_t0,sumCCN3_t0,wccn1,wccn2,wccn3

! Variables for hydci_pp
! ----------------------

  REAL (KIND=wp)     ::           &
    ccsrim,    & !
    ccsagg,    & !
    ccsdep,    & !
    ccsvel,    & !
    ccsvxp,    & !
    ccslam,    & !
    ccslxp,    & !
    ccsaxp,    & !
    ccsdxp,    & !
    ccshi1,    & !
    ccdvtp,    & !
    ccidep,    & !
    ccswxp,    & !
    zconst,    & !
    zcev,      & !
    zbev,      & !
    zcevxp,    & !
    zbevxp,    & !
    zvzxp,     & !
    zvz0r

! Variables for hydci_pp and hydci_pp_gr
! --------------------------------------

  REAL (KIND=wp)     ::          &
    v0snow         = 25.0_wp,     & ! factor in the terminal velocity for snow
    cloud_num      = 5.00e+08_wp, & ! cloud droplet number concentration
    mu_rain        = 0.0_wp,      & ! COSMO_EU default
    rain_n0_factor = 1.0_wp         ! COSMO_EU default

!==============================================================================

END MODULE data_gscp
