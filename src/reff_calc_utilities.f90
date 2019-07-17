!+ Utility module for computing effective radii for revised cloud radiation coupling
!-----------------------------------------------------------------------------------

MODULE reff_calc_utilities

!------------------------------------------------------------------------------
!
! Description:
!   This module provides service utilities for computing hydrometeor
!   effective radii (Reff) for radiation.
!
!   Routines (module procedures) currently contained:
!
!
! Current Code Owner: DWD, Ulrich Blahak
!  phone:  +49 69 8062 2393
!  fax :   +49 69 8062 3721
!  email:  ulrich.blahak@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 5.01       2014/11/15 Ulrich Blahak
!  Initial release
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
!
USE data_parameters , ONLY :   &
  wp, dp,    & ! KIND-type parameters for real variables
  iintegers    ! KIND-type parameter for standard integer variables

!------------------------------------------------------------------------------

USE pp_utilities, ONLY : gamma_fct

!------------------------------------------------------------------------------

IMPLICIT NONE

!------------------------------------------------------------------------------

CONTAINS

!===========================================================================================
!===========================================================================================
!
! Number of ice nuclei (m^-3) as function of temperature as assumed in the COSMO 1-moment schemes
!
!===========================================================================================
!===========================================================================================

! Traditional relation, cf. COSMO documentation, produces lots of IN at higher temperatures:
REAL (KIND=wp) ELEMENTAL FUNCTION ice_nuclei_number_old(temp, t0_melt)

  IMPLICIT NONE

  REAL (KIND=wp), INTENT(IN)  :: temp, t0_melt

  ice_nuclei_number_old = MIN(1.0E2_wp * EXP(0.2_wp * (t0_melt - temp)), 163.598E3_wp)
  ! The value 163.598E3 is the value for temp=236.15, which is the limiting value in the microphysics

END FUNCTION ice_nuclei_number_old

! Cooper (1986) as used in the microphysics for lsuper_coolw = .true.:
REAL (KIND=wp) ELEMENTAL FUNCTION ice_nuclei_number(temp, t0_melt)

  IMPLICIT NONE

  REAL (KIND=wp), INTENT(IN)  :: temp, t0_melt

  ice_nuclei_number = MIN(5.0E0_wp * EXP(0.304_wp * (t0_melt - temp)), 250E3_wp)   !GE make ice formation 2K colder/warmer
  ! The value 250.0E3 is the limiting value in the microphysics

END FUNCTION ice_nuclei_number


!===========================================================================================
!===========================================================================================
!
!HP! Concentration number of snow particles as function of n0_s,ageos,bgeos,qs in the COSMO 1-moment scheme
!
! NCSNOW is computed from N0_snow(i,j,k) and QS(i,j,k) with the help of the gamma-function (exponent).
!
!===========================================================================================
!===========================================================================================

REAL (KIND=wp) FUNCTION diag_qns(n0_s, ageos, bgeos, qs)

  IMPLICIT NONE

  REAL(kind=wp), INTENT(in)  :: n0_s, ageos, bgeos, qs

  REAL (KIND=wp)             :: gamma_param, qs_epsi

!  diag_qns = zams*n0_s*pow(2.0d0*zams*n0_s/qs,-1/3)
  gamma_param = gamma_fct( bgeos+1.0_wp )
  qs_epsi=1e-9_wp

  IF (qs > qs_epsi) THEN
    diag_qns = ageos*n0_s*EXP((-1.0_wp/(bgeos+1.0_wp))*LOG(gamma_param*ageos*n0_s/qs))
  ELSE
    diag_qns=0.0_wp
  ENDIF

END FUNCTION diag_qns
!HP!




!===========================================================================================
!===========================================================================================
!
! Subroutine to calculate n0_s as a function of temperature after Field et al. (2005)
!
! Input:    T_a    Temperature in K
!           q_s    mass density of snow in kg/m^3
!           ageos  Parameter of mass_size-relation x=ageos*D^bgeos, x in kg, D in m
! Output:   n0_s   Intercept parameter of expon. snow size distrib. in m^-4
!
!===========================================================================================
!===========================================================================================

SUBROUTINE calc_n0_snow(isnow_n0temp, T_a, q_s, ageos, n0_s)

  IMPLICIT NONE

  INTEGER, INTENT(in)            :: isnow_n0temp

  REAL(kind=wp), INTENT(in)  :: T_a, q_s, ageos

  REAL(kind=wp), INTENT(out) :: n0_s

  REAL (KIND=wp)             :: ztc, zn0s, &
                                    alf, bet, hlp, m2s, m3s
  
  INTEGER(kind=iintegers)        :: nn

  REAL (KIND=wp), PARAMETER  :: zn0s1 = 13.5_wp * 5.65E5_wp, & ! parameter in N0S(T)
                                    zn0s2 = -0.107_wp                  ! parameter in N0S(T), Field et al

  !-----------------------------------------------------------------------------------

  ! Snow: Coeffs for moment relation based on 2nd moment (Field et al. 2005)
  !        (defined as parameters because of better inlining and vectorization properties)
  REAL (KIND=wp), DIMENSION(10), PARAMETER  ::   &
       mma = (/5.065339_wp, -0.062659_wp, -3.032362_wp, 0.029469_wp, -0.000285_wp, &
               0.312550_wp,  0.000204_wp,  0.003199_wp, 0.000000_wp, -0.015952_wp /)  , &
       mmb = (/0.476221_wp, -0.015896_wp,  0.165977_wp, 0.007468_wp, -0.000141_wp, &
               0.060366_wp,  0.000079_wp,  0.000594_wp, 0.000000_wp, -0.003577_wp /)

  !.. N0-Parameter for snow:
  n0_s = 1e9_wp  ! initialization
    
  IF (isnow_n0temp == 1) THEN
    ! Calculate n0s using the temperature-dependent formula of Field et al. (2005)
    ztc = T_a - 273.16
    ztc = MAX(MIN(ztc,0.0_wp),-40.0_wp)          
    zn0s = zn0s1*EXP(zn0s2*ztc)
    zn0s = MIN(zn0s,1e9_wp)
    zn0s = MAX(zn0s,1e6_wp)
    n0_s = zn0s
  ELSEIF (isnow_n0temp == 2) THEN
    ! Calculate n0s using the temperature-dependent moment relations of Field et al. (2005)
    ztc = T_a - 273.16
    ztc = MAX(MIN(ztc,0.0_wp),-40.0_wp)
    
    nn  = 3
    hlp = mma(1)      +mma(2)*ztc      +mma(3)*nn       +mma(4)*ztc*nn+mma(5)*ztc**2 &
         + mma(6)*nn**2+mma(7)*ztc**2*nn+mma(8)*ztc*nn**2+mma(9)*ztc**3+mma(10)*nn**3
    alf = 10.0_wp**hlp
    bet = mmb(1)      +mmb(2)*ztc      +mmb(3)*nn       +mmb(4)*ztc*nn+mmb(5)*ztc**2 &
         + mmb(6)*nn**2+mmb(7)*ztc**2*nn+mmb(8)*ztc*nn**2+mmb(9)*ztc**3+mmb(10)*nn**3
    IF (q_s >= 1e-20_wp) THEN
!!$ UB: caution! Here is the exponent bms=2.0 hardwired! not ideal!
      m2s = q_s / ageos
      m3s = alf*EXP(bet*LOG(m2s))
      hlp = zn0s1*EXP(zn0s2*ztc)
!!$ UB: the 13.5 is actually 3^3 / gamma(3) ...
      zn0s = 13.50_wp * m2s**4 / m3s**3
      zn0s = MAX(zn0s,0.5_wp*hlp)
      zn0s = MIN(zn0s,1e2_wp*hlp)
      zn0s = MIN(zn0s,1e9_wp)
      n0_s = MAX(zn0s,8e5_wp)
    ELSE
      n0_s = 1e9_wp
    END IF
  ELSE
    ! Old constant n0s
    n0_s = 8.0d5
  ENDIF
  
  RETURN
END SUBROUTINE calc_n0_snow

!==============================================================================

!==============================================================================
!==============================================================================
!
! Subroutines for diagnosis of the cloud and ice effective radii
! for computation of the optical parameters of clouds, designed for the 
! parameter conventions of the 1-moment schemes.
!
! Effective radii after the following definitions are applied:
!
! cloud droplets       : reff = 3 V / 4 A
!
! cloud ice, "solar"   : reff after definition of Key et al. (2002)
!
!                        (for both hexagonal plates and hexagonal columns, regularly oriented)
!
! cloud ice, "thermal" : reff after definition of Fu et al. (1998)
!                        (for both hexagonal plates and hexagonal columns, randomly oriented)
!
! NOTE: the word "solar" and suffix "_so" for effective radii and its coefficients 
!       is used as a synonym for the Key et al. (2002) convention,
!       and the word "thermal" and suffix "_th" 
!       is used as a synonym for the Fu et al. (1996/1998) convention.
!
!===========================================================================================
!===========================================================================================

!===========================================================================================
!
! .. useful for R_eff after definitions of Fu and Key of polydisperse hexagonal ice particles:
!
!===========================================================================================

REAL(kind=wp) FUNCTION reff_gammfac_1mom(bgeo,nue,mue,f) 
  IMPLICIT NONE
  
  REAL(kind=wp) :: bgeo, mue, nue, f
  
  ! .. Assumption: N(D) = N0 * D^nue * exp(-lam*D^mue)
  !                  x  = ageo * D^bgeo

  reff_gammfac_1mom  =  gamma_fct( (nue+1.0+bgeo)/mue ) / &
                        gamma_fct( (nue+1.0+bgeo-f*bgeo)/mue ) * &
                      ( gamma_fct( (nue+1.0)/mue ) / &
                        gamma_fct( (nue+1.0+bgeo)/mue ) )**f
END FUNCTION reff_gammfac_1mom

REAL(kind=wp) FUNCTION reff_gammfac_inv_1mom(bgeo,nue,mue,f) 
  IMPLICIT NONE
  
  REAL(kind=wp) :: bgeo, mue, nue, f
  
  ! .. Assumption: N(D) = N0 * D^nue * exp(-lam*D^mue)
  !                  x  = ageo * D^bgeo

  reff_gammfac_inv_1mom  =  gamma_fct( (nue+1.0+bgeo-f*bgeo)/mue ) / &
                            gamma_fct( (nue+1.0+bgeo)/mue ) * &
                          ( gamma_fct( (nue+1.0+bgeo)/mue ) / &
                            gamma_fct( (nue+1.0)/mue ) )**f
END FUNCTION reff_gammfac_inv_1mom

!===========================================================================================
!
! .. Constant prefactors for the calculation of effective radii.
!    This subroutine assumes polydisperse cloud PSD (nue_c, mue_c), but monodisperse ice PSD.
!    Call once at the beginning.
!
!===========================================================================================

SUBROUTINE prefactors_reff_1mom_cloud (pi, nue_c, mue_c, rho_w, ldebug, &
     c_reffc)

  IMPLICIT NONE

  REAL (kind=wp), INTENT(in)  :: pi, nue_c, mue_c, rho_w
  LOGICAL, INTENT(in)             :: ldebug
  REAL (kind=wp), INTENT(out) :: c_reffc(2)

  REAL (kind=wp), SAVE :: nue_c_l=-99.99, mue_c_l=-99.99
  LOGICAL,            SAVE :: firstcall = .TRUE.
  REAL (kind=wp), SAVE :: c_reffc_s(2)

  ! .. Assumption: N_c(D) = N0 * D^nue_c * exp(-lam*D^mue_c)
  !                   x_c = pi/6 * rhow * D^3
  !             cloud number density is an external parameter!
  !
  !    Reff = c1 * xquer**c2


  IF (firstcall .OR. (nue_c /= nue_c_l .OR.  mue_c /= mue_c_l)) THEN

    IF (ldebug) THEN
      WRITE (*,*) '  pp_utilities:  computing prefactors_reff_1mom_cloud()'
    END IF

    ! .. r_eff for cloud droplets solar / thermal, assumed gen. gamma DSD for radiation:
    c_reffc_s(1)       = 0.5 * (6.0 / (pi*rho_w))**(1.0/3.0) * &
         reff_gammfac_1mom(bgeo=3.0_wp, nue=nue_c, mue=mue_c, f=1.0_wp/3.0_wp )
    c_reffc_s(2)       = 1.0_wp / 3.0_wp

    firstcall = .FALSE.
    nue_c_l = nue_c
    mue_c_l = mue_c

  END IF

  c_reffc = c_reffc_s

END SUBROUTINE prefactors_reff_1mom_cloud


SUBROUTINE prefactors_reff_1mom_rain(pi, n0_r, nue_r, mue_r, rho_w, ldebug, &
     c_reffr)

  IMPLICIT NONE

  REAL (kind=wp), INTENT(in)  :: pi, n0_r, nue_r, mue_r, rho_w
  LOGICAL, INTENT(in)             :: ldebug
  REAL (kind=wp), INTENT(out) :: c_reffr(2)

  REAL (kind=wp), SAVE :: n0_r_l=-99.99, nue_r_l=-99.99, mue_r_l=-99.99
  LOGICAL,            SAVE :: firstcall = .TRUE.
  REAL (kind=wp), SAVE :: c_reffr_s(2)

  ! .. Assumption: N_r(D) = N0 * D^nue_r * exp(-lam*D^mue_r)
  !                   x_r = pi/6 * rhow * D^3
  !
  !    Reff = c1 * rhor**c2

  IF (firstcall .OR. (nue_r /= nue_r_l .OR.  mue_r /= mue_r_l .OR.  n0_r /= n0_r_l)) THEN

    IF (ldebug) THEN
      WRITE (*,*) '  pp_utilities:  computing prefactors_reff_1mom_rain()'
    END IF

    ! .. r_eff for rain drops solar / thermal, assumed gen. gamma DSD for radiation:
!!$    c_reffr_s(1)       = 0.5 * gamma_fct( (nue_r+4.0)/mue_r ) / gamma_fct( (nue_r+3.0)/mue_r ) * &
!!$         ( pi*rho_w*n0_r/(6.0*mue_r) * gamma_fct( (nue_r+1.0+3.0)/mue_r ) ) ** (-1.0/(nue_r+3.0+1.0))
    c_reffr_s(1)       = 0.5 * &
         gamma_fct( (nue_r+4.0)/mue_r )**((nue_r+3.0)/(nue_r+4.0)) / gamma_fct( (nue_r+3.0)/mue_r ) * &
         ( pi*rho_w*n0_r/(6.0*mue_r) ) ** (-1.0/(nue_r+4.0))
    c_reffr_s(2)       = 1.0_wp / (nue_r+4.0_wp)

    firstcall = .FALSE.
    n0_r_l  = n0_r
    nue_r_l = nue_r
    mue_r_l = mue_r

  END IF

  c_reffr = c_reffr_s

END SUBROUTINE prefactors_reff_1mom_rain


SUBROUTINE prefactors_reff_1mom_icemono (pi, ageo_i, bgeo_i, rho_ice, ldebug, &
     c_reffi_so, c_reffi_th, c_reffi_ls, c_rhoiquer, c_ari_th)

  IMPLICIT NONE

  REAL (kind=wp), INTENT(in)  :: pi, ageo_i, bgeo_i, rho_ice
  LOGICAL, INTENT(in)             :: ldebug
  REAL (kind=wp), INTENT(out) :: c_reffi_so(2,2), c_reffi_th(2,4), c_reffi_ls(2), c_rhoiquer(2), &
                                 c_ari_th(2,8)

  REAL (kind=wp), SAVE :: ageo_i_l=-99.99, bgeo_i_l=-99.99
  LOGICAL,        SAVE :: firstcall = .TRUE.
  REAL (kind=wp), SAVE :: c_reffi_so_s(2,2), c_reffi_th_s(2,4), c_reffi_ls_s(2), c_rhoiquer_s(2), c_ari_th_s(2,8)

  ! .. Assumption: N_i(D) = delta(D_0),  D_0 = (qi/(ni(T)*ageo_i)^(1/bgeo_i)
  !                   x_i = ageo_i * D^bgeo_i    (mass-size-relation, D = largest dimension)
  !                   xquer_i = qi/(ni(T)        (mean mass of N_i(D))
  !    D_0 = monodisperse diameter of all particles = mean mass diameter in this case
  !
  !    ni   = ni(T)
  !    Reff = c1 * xquer^c2         (convention of Key et al., 2002)
  !    Reff = 0.5 / (c1 * xquer^c2 + c3 * xquer^c4)  (convention of Fu et al., 1996)
  !    AR   = (c1 * xquer^c2 + c3 * xquer^c4) / (c5 * xquer^c6 + c7 * xquer^c8)
  !                        (convention of Fu et al., 2007)
  !
  !      c1...c4 are coefficients depending on ageo_i, bgeo_i

  IF (firstcall .OR. (ageo_i /= ageo_i_l .OR. bgeo_i /= bgeo_i_l)) THEN

    IF (ldebug) THEN
      WRITE (*,*) '  pp_utilities:  computing prefactors_reff_1mom_icemono()'
    END IF

    ! .. r_eff for ice solar, def. of Key et al. (2002), hex plates, monodisperse PSD:
    c_reffi_so_s(1,1)  = 2.0*SQRT(3.0)*ageo_i**(2.0/bgeo_i) / (3.0*rho_ice) ! prefactor
    c_reffi_so_s(1,2)  = ( (bgeo_i-2.0)/bgeo_i )                            ! exponent
    ! .. r_eff for ice solar, def. of Key et al. (2002), hex columns, monodisperse PSD:
    c_reffi_so_s(2,1)  = SQRT( 27.0*SQRT(3.0)*ageo_i**(1.0/bgeo_i) / (128.0*rho_ice) ) ! prefactor
    c_reffi_so_s(2,2)  = ( 0.5*(bgeo_i-1.0)/bgeo_i )                                   ! exponent
    ! .. r_eff for ice thermal, def. of Fu et al. (1998), hex plates, monodisperse PSD, needs four coefficients:
    c_reffi_th_s(1,1)  = ageo_i**(1.0/bgeo_i)                     ! prefactor
    c_reffi_th_s(1,2)  = ( -1.0/bgeo_i )                          ! exponent
    c_reffi_th_s(1,3)  = 9.0*ageo_i**(-2.0/bgeo_i)*rho_ice / 32.0 ! prefactor
    c_reffi_th_s(1,4)  = ( (2.0-bgeo_i)/bgeo_i )                  ! exponent
    ! .. r_eff for ice thermal, def. of Fu et al. (1998), hex columns, monodisperse PSD, needs four coefficients:
    c_reffi_th_s(2,1)  = SQRT( 3.0*SQRT(3.0)*rho_ice / ( 8.0*ageo_i**(1.0/bgeo_i) ) ) ! prefactor
    c_reffi_th_s(2,2)  = ( 0.5*(1.0-bgeo_i)/bgeo_i )                                  ! exponent
    c_reffi_th_s(2,3)  = SQRT(3.0)*ageo_i**(1.0/bgeo_i)/4.0                           ! prefactor
    c_reffi_th_s(2,4)  = ( -1.0/bgeo_i )                                              ! exponent

    ! .. The following coefficients are for r_eff for large-size limit: Reff = c1 * xquer_i**c2:
    c_reffi_ls_s(1) = 0.5*ageo_i**(-1.0/bgeo_i)
    c_reffi_ls_s(2) = 1.0/bgeo_i

    ! ..  sphere-volume-weighted equivalent average bulk particle density for the large-size limit:
    !     rhobulkiquer =  c1 * xquer_i**c2
    c_rhoiquer_s(1) = (6.0/pi) * ageo_i**(3.0_wp/bgeo_i)
    c_rhoiquer_s(2) = (bgeo_i-3.0)/bgeo_i

    ! .. The following coefficients are for mean axis ratio AR (D/L) for the parameterization
    !    of g and f_d after Fu (2007). So far we implement only AR <=1, which means hexagonal columns.
    !    The formula for polydisperse PSDs is:
    !      AR = (c1 * xquer_i**c2 + c3 * xquer_i**c4) / (c5 * xquer_i**c6 + c7 * xquer_i**c8)
    !    but we boil this down here to the formula for monodisperse PSDs:
    !      AR = c1 * xquer_i**c2
    c_ari_th_s(2,1)  =  SQRT( 8.0 / (3.0*SQRT(3.0)*rho_ice) * ageo_i**(3.0/bgeo_i) )  ! prefactor
    c_ari_th_s(2,2)  =  (bgeo_i-3.0)/(2.0*bgeo_i)                                     ! exponent
    c_ari_th_s(2,3)  =  0.0 ! prefactor
    c_ari_th_s(2,4)  =  0.0 ! exponent
    c_ari_th_s(2,5)  =  1.0 ! prefactor
    c_ari_th_s(2,6)  =  0.0 ! exponent
    c_ari_th_s(2,7)  =  0.0 ! prefactor
    c_ari_th_s(2,8)  =  0.0 ! prefactor

    ! .. currently the coeff. for hex. plates are not present, but just
    !    set to the ones for hex. columns:
    c_ari_th_s(1,:) = c_ari_th_s(2,:)

    firstcall = .FALSE.
    ageo_i_l   = ageo_i
    bgeo_i_l   = bgeo_i

  END IF

  c_reffi_so = c_reffi_so_s 
  c_reffi_th = c_reffi_th_s
  c_reffi_ls = c_reffi_ls_s      
  c_rhoiquer = c_rhoiquer_s
  c_ari_th   = c_ari_th_s

END SUBROUTINE prefactors_reff_1mom_icemono


!===========================================================================================
!
! AS ALTERNATIVE FOR FUTURE SCHEMES, WHICH MAY ASSUME A POLYDISPERSE PSD FOR CLOUD ICE,
! OR IF USING IT TO COMPUTE REFF FOR OTHER HYDROMETEORS LIKE SNOW AND GRAUPEL:
!
! .. Constant prefactors for the calculation of effective radii.
!    This subroutine assumes polydisperse cloud PSD and polydisperse ice PSD.
!    Call once at the beginning.
!
! The effective radii can be calculated with these coefficients using xquer_i=L_i/N_i,
!
!    Reff = c1 * xquer_i**c2
!
! First argument "istore": this is a storage index for certain hydrometeor types,
!   so that this routine can be called generically for all ice hydrometeors,
!   and the coefficients are calculated only on the first call for a certain hydrometeor
!   and restored in subsequent calls.
!
! Note: The n0_i (optional input) is only needed as input parameter for those hydrometeors,
!       for which it is necessary to diagnose xquer_i=L_i/N_I from L_i and n0_i.
!       For the COSMO 1-moment schemes, this is the case for snow and graupel, but
!       not for ice, because there, the N_i is a function of T.
!
! n0_i is disregarded if a value < 0.0 is input. Then, c_xiquer is computed without the n0_i contribution.
!   This contribution n0_i**(-c_xiquer(2)) has to be added subsequently, when computing xiquer.
!
! It is also possible to work with non-constant n0_i. In this case, call 
!   this routine with n0_i = 1.0 (neutral value), specify the optional n0_i-field later when
!   you call diag_reffs_1mom( ... , n0_i=n0_i_3dfield).
!
!===========================================================================================

SUBROUTINE prefactors_reff_1mom_solidpoly (istore, pi, ageo_i, bgeo_i, n0_i, nue_i, mue_i, rho_ice, ldebug, &
     c_reffi_so, c_reffi_th, c_reffi_ls, c_rhoiquer, c_ari_th, c_xiquer)

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in) :: istore
  REAL (kind=wp), INTENT(in)  :: pi, ageo_i, bgeo_i, nue_i, mue_i, rho_ice
  REAL (kind=wp), INTENT(in)  :: n0_i
  LOGICAL, INTENT(in)         :: ldebug
  REAL (kind=wp), INTENT(out) :: c_reffi_so(2,2), c_reffi_th(2,4), c_reffi_ls(2), c_rhoiquer(2), c_xiquer(2), &
                                 c_ari_th(2,8)

  INTEGER (kind=iintegers), PARAMETER :: istore_max = 10

  REAL (kind=wp), DIMENSION(istore_max), SAVE  :: nue_c_l=-99.99, mue_c_l=-99.99,   &
                                                      ageo_i_l=-99.99, bgeo_i_l=-99.99, &
                                                      nue_i_l=-99.99, mue_i_l=-99.99,   &
                                                      n0_i_l=-99.99

  LOGICAL,            DIMENSION(istore_max), SAVE  :: firstcall = .TRUE.
  REAL (kind=wp), SAVE  :: c_reffi_so_s(2,2,istore_max)=0.0, c_reffi_th_s(2,4,istore_max)=0.0, &
                               c_reffi_ls_s(2,istore_max)=0.0,   c_rhoiquer_s(2,istore_max)=0.0, &
                               c_xiquer_s(2,istore_max)=0.0,     c_ari_th_s  (2,8,istore_max)=0.0

  REAL (kind=wp) :: kappa

  ! .. Assumption: N_i(D) = delta(D_0),  D_0 = (qi/(ni(T)*ageo_i)^(1/bgeo_i)
  !                   x_i = ageo_i * D^bgeo_i    (mass-size-relation, D = largest dimension)
  !                   xquer_i = qi/(ni(T)        (mean mass of N_i(D))
  !    D_0 = monodisperse diameter of all particles = mean mass diameter in this case
  !
  !    ni   = ni(T)
  !    Reff = c1 * xquer^c2         (convention of Key et al., 2002)
  !    Reff = 0.5 / (c1 * xquer^c2 + c3 * xquer^c4)  (convention of Fu et al., 1996)
  !    AR   = (c1 * xquer^c2 + c3 * xquer^c4) / (c5 * xquer^c6 + c7 * xquer^c8)
  !                        (convention of Fu et al., 2007)
  !
  !      c1...c4 are coefficients depending on ageo_i, bgeo_i

  IF (istore < 0 .OR. istore > istore_max) THEN
    WRITE (*,*) 'ERROR: wrong value of istore in call to prefactors_reff_1mom_solidpoly(): ', istore
    STOP
  END IF

  IF ( firstcall(istore) .OR. &
       ( ageo_i /= ageo_i_l(istore) .OR. bgeo_i /= bgeo_i_l(istore) ) .OR. &
       ( nue_i  /= nue_i_l(istore)  .OR. mue_i  /= mue_i_l(istore)  ) .OR. &
       n0_i /= n0_i_l(istore) ) THEN
 
    IF (ldebug) THEN
      WRITE (*,*) '  pp_utilities:  computing prefactors_reff_1mom_solidpoly() for istore = ', istore
    END IF

    ! .. The following coefficients are for Reff = c1 * xquer_i**c2:

    ! .. r_eff for ice solar, def. of Key et al. (2002), hex plates, polydisperse PSD:
    c_reffi_so_s(1,1,istore)  = 2.0*SQRT(3.0)*ageo_i**(2.0/bgeo_i) / (3.0*rho_ice) * &         ! prefactor
         reff_gammfac_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i, f=(bgeo_i-2.0)/bgeo_i )
    c_reffi_so_s(1,2,istore)  = ( (bgeo_i-2.0)/bgeo_i )                                        ! exponent
    ! .. r_eff for ice solar, def. of Key et al. (2002), hex columns, polydisperse PSD:
    c_reffi_so_s(2,1,istore)  = SQRT( 27.0*SQRT(3.0)*ageo_i**(1.0/bgeo_i) / (128.0*rho_ice) ) * & ! prefactor
         reff_gammfac_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i, f=0.5*(bgeo_i-1.0)/bgeo_i )
    c_reffi_so_s(2,2,istore)  = ( 0.5*(bgeo_i-1.0)/bgeo_i )                                       ! exponent
    ! .. r_eff for ice thermal, def. of Fu et al. (1998), hex plates, polydisperse PSD, needs four coefficients:
    c_reffi_th_s(1,1,istore)  = ageo_i**(1.0/bgeo_i) / &                                      ! prefactor
         reff_gammfac_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i, f=1.0/bgeo_i )
    c_reffi_th_s(1,2,istore)  = ( -1.0/bgeo_i )                                               ! exponent
    c_reffi_th_s(1,3,istore)  = 9.0*ageo_i**(-2.0/bgeo_i)*rho_ice / 32.0 / &                  ! prefactor
         reff_gammfac_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i, f=(bgeo_i-2.0)/bgeo_i )
    c_reffi_th_s(1,4,istore)  = ( (2.0-bgeo_i)/bgeo_i )                                       ! exponent
    ! .. r_eff for ice thermal, def. of Fu et al. (1998), hex columns, polydisperse PSD, needs four coefficients:
    c_reffi_th_s(2,1,istore)  = SQRT( 3.0*SQRT(3.0)*rho_ice / ( 8.0*ageo_i**(1.0/bgeo_i) ) ) / &   ! prefactor
         reff_gammfac_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i,  f=0.5*(bgeo_i-1.0)/bgeo_i )
    c_reffi_th_s(2,2,istore)  = ( 0.5*(1.0-bgeo_i)/bgeo_i )                                        ! exponent
    c_reffi_th_s(2,3,istore)  = SQRT(3.0)*ageo_i**(1.0/bgeo_i)/4.0 / &                             ! prefactor
         reff_gammfac_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i, f=1.0/bgeo_i )
    c_reffi_th_s(2,4,istore)  = ( -1.0/bgeo_i )                                                    ! exponent


!!$    ! .. The following coefficients are for r_eff for large-size limit: Reff = c1 * rho_i**c2:
!!$    c_reffi_ls_s(1,istore) = 0.5 * gamma_fct( (nue_i+4.0)/mue_i ) / gamma_fct( (nue_i+3.0)/mue_i ) * &
!!$         ( ageo_i*n0_i/mue_i * gamma_fct( (nue_i+bgeo_i+1.0)/mue_i ) ) ** (-1.0/(nue_i+bgeo_i+1.0))
!!$    c_reffi_ls_s(2,istore) = 1.0/(nue_i+bgeo_i+1.0)
!!$
!!$    ! ..  sphere-volume-weighted equivalent average bulk particle density for the large-size limit:
!!$    !     rhobulkiquer =  c1 * rho_i**c2
!!$    c_rhoiquer_s(1,istore) = 6.0*mue_i/(pi*n0_i) * (ageo_i*n0_i/mue_i)**((nue_i+4.0)/(nue_i+bgeo_i+1.0)) * &
!!$         gamma_fct((nue_i+bgeo_i+1.0)/mue_i)**((bgeo_i-3.0)/(nue_i+bgeo_i+1.0)) / gamma_fct((nue_i+4.0)/mue_i)
!!$    c_rhoiquer_s(2,istore) = (bgeo_i-3.0) / (nue_i+bgeo_i+1.0)

    ! .. The following coefficients are for r_eff for large-size limit: Reff = c1 * xquer_i**c2:
    c_reffi_ls_s(1,istore) = 0.5 * gamma_fct( (nue_i+4.0)/mue_i ) / gamma_fct( (nue_i+3.0)/mue_i ) * &
         ( gamma_fct( (nue_i+1.0)/mue_i ) / ( ageo_i*gamma_fct( (nue_i+bgeo_i+1.0)/mue_i ) ) ) ** (1.0/bgeo_i)
    c_reffi_ls_s(2,istore) = 1.0/bgeo_i

    ! ..  sphere-volume-weighted equivalent average bulk particle density for the large-size limit:
    !     rhobulkiquer =  c1 * xquer_i**c2
    c_rhoiquer_s(1,istore) = (6.0/pi) * ageo_i**(3.0_wp/bgeo_i) * &
         reff_gammfac_1mom( bgeo=bgeo_i, nue=nue_i,mue=mue_i, f=(bgeo_i-3.0)/bgeo_i)
    c_rhoiquer_s(2,istore) = (bgeo_i-3.0)/bgeo_i

    ! .. The following coefficients are for mean axis ratio AR (D/L) for the parameterization
    !    of g and f_d after Fu (2007). So far we implement only AR <=1, which means hexagonal columns.
    !    The formula for polydisperse PSDs is:
    !      AR = (c1 * xquer_i**c2 + c3 * xquer_i**c4) / (c5 * xquer_i**c6 + c7 * xquer_i**c8)
    kappa = SQRT( 8.0*ageo_i / (3.0*SQRT(3.0)*rho_ice) )
    c_ari_th_s(2,2,istore)  =  ( 0.5*(1.0-5.0/bgeo_i) )                                           ! exponent
    c_ari_th_s(2,1,istore)  =  0.25*SQRT(3.0)*kappa*kappa*ageo_i**(-c_ari_th_s(2,2,istore)) * &   ! prefactor
         reff_gammfac_inv_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i, f=-c_ari_th_s(2,2,istore) )
    c_ari_th_s(2,4,istore)  =  ( -1.0/bgeo_i )                                                    ! exponent
    c_ari_th_s(2,3,istore)  =  kappa * ageo_i**(-c_ari_th_s(2,4,istore)) * &                      ! prefactor
         reff_gammfac_inv_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i, f=-c_ari_th_s(2,4,istore) )
    c_ari_th_s(2,6,istore)  =   c_ari_th_s(2,4,istore)                                            ! exponent
    c_ari_th_s(2,5,istore)  =   0.25*SQRT(3.0) * c_ari_th_s(2,3,istore)                           ! prefactor
    c_ari_th_s(2,8,istore)  =   ( 0.5*(1.0/bgeo_i - 1.0) )                                        ! exponent
    c_ari_th_s(2,7,istore)  =   ageo_i**(-c_ari_th_s(2,8,istore)) * &                             ! prefactor
         reff_gammfac_inv_1mom( bgeo=bgeo_i, nue=nue_i, mue=mue_i, f=-c_ari_th_s(2,8,istore) )

    ! .. currently the coeff. for hex. plates are not present, but just
    !    set to the ones for hex. columns:
    c_ari_th_s(1,:,istore) = c_ari_th_s(2,:,istore)

    ! .. The following coefficients are to compute xquer_i:
    IF (n0_i >= 0.0) THEN
      ! .. Coefficients to compute xquer_i = c1 * rho_i**c2, needs n0_i:
      c_xiquer_s(1,istore) = ageo_i * gamma_fct( (nue_i+bgeo_i+1.0)/mue_i ) / gamma_fct( (nue_i+1.0)/mue_i ) * &
           ( ageo_i*n0_i/mue_i * gamma_fct( (nue_i+bgeo_i+1.0)/mue_i ) ) ** (-bgeo_i/(nue_i+bgeo_i+1.0))
      c_xiquer_s(2,istore) = bgeo_i / (nue_i+bgeo_i+1.0)
    ELSE
      ! .. Coefficients to compute xquer_i = c1 * (rho_i/n0_i)**c2, does not need n0_i, but requires
      !    to include n0_i into the xquer_i-calculation!
      c_xiquer_s(1,istore) = ageo_i * gamma_fct( (nue_i+bgeo_i+1.0)/mue_i ) / gamma_fct( (nue_i+1.0)/mue_i ) * &
           ( ageo_i/mue_i * gamma_fct( (nue_i+bgeo_i+1.0)/mue_i ) ) ** (-bgeo_i/(nue_i+bgeo_i+1.0))
      c_xiquer_s(2,istore) = bgeo_i / (nue_i+bgeo_i+1.0)
    END IF

    firstcall(istore) = .FALSE.
    ageo_i_l(istore) = ageo_i
    bgeo_i_l(istore) = bgeo_i
    nue_i_l(istore)  = nue_i
    mue_i_l(istore)  = mue_i
    n0_i_l(istore)   = n0_i

  ENDIF

  c_reffi_so = c_reffi_so_s(:,:,istore)
  c_reffi_th = c_reffi_th_s(:,:,istore)
  c_reffi_ls = c_reffi_ls_s(:,istore)
  c_rhoiquer = c_rhoiquer_s(:,istore)
  c_ari_th   = c_ari_th_s(:,:,istore)
  c_xiquer   = c_xiquer_s(:,istore)

END SUBROUTINE prefactors_reff_1mom_solidpoly


!===========================================================================================
!
! .. Diagnosis of effective radii for gridscale clouds
!    (subgrid scale clouds are treated directly in the radiation scheme):
!
!===========================================================================================

SUBROUTINE diag_reffc_1mom(its, ite, jts, jte, kts, kte, &
     hhl, rhol, qc, cloud_num, qc_thresh, z0_ncn, z1oe_ncn, &
     rhoc_nclow, rhoc_nchigh, ncfact_low, c_reffc, r_effc_ini, r_effc, &
     do_qnc_profile)

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  REAL (kind=wp), INTENT(in)        :: hhl(its:ite,jts:jte,kts:kte+1), rhol(its:ite,jts:jte,kts:kte), &
                                       qc(its:ite,jts:jte,kts:kte)                    ! kg/kg
  REAL (kind=wp), INTENT(in)        :: cloud_num(its:ite,jts:jte,kts:kte), &          ! 1/kg
                                       qc_thresh, c_reffc(2), r_effc_ini, z0_ncn, z1oe_ncn, &
                                       rhoc_nclow, rhoc_nchigh, ncfact_low
  REAL (kind=wp), INTENT(out)       :: r_effc(its:ite,jts:jte,kts:kte)
  LOGICAL       , INTENT(in)        :: do_qnc_profile

  REAL(KIND=wp),  PARAMETER :: eps2 = 1e-20
  REAL (kind=wp), PARAMETER :: zxc_min = 6.54e-14_wp  ! min. allowed mean cloud droplet particle mass (D =  5e-6 m)
!!$  REAL (kind=wp), PARAMETER :: zxc_max = 2.60e-10_wp  ! max. allowed mean cloud droplet particle mass (D = 80e-6 m)
  REAL (kind=wp), PARAMETER :: zxc_max = 2.60e-9_wp  ! max. allowed mean cloud droplet particle mass (D = 170e-6 m)

  INTEGER (kind=iintegers) :: i, j, k
  REAL (kind=wp)       :: zxc, qnc, temp, zml_k

  r_effc= r_effc_ini ! preset reasonable value
!!$  rhoc_nchigh = 0.5e-4     ! EXPERIMENT  Reduction of cloud_num for thick clouds as function of rhoc
!!$  rhoc_nclow  = 2.0e-4     ! EXPERIMENT
!!$  ncfact_low  = 0.1        ! EXPERIMENT  [0 .... 1]
  DO k = kts, kte
!CDIR COLLAPSE
    DO j = jts, jte
      DO i = its, ite
        ! .. use same threshold as in src_radiation for grid scale clouds:
        IF (qc(i,j,k) > qc_thresh) THEN
          ! If do_qnc_profile=.TRUE., cloud droplet number asssumed as an exponentially decreasing function of height.
          ! Justification: aerosol concentration decreases approximately exponential with height.
          ! For thin clouds, thus the aerosol conc. at their height of origin is decisive,
          ! and Segal and Khain (2006) show that a large proportion of aerosols gets activated,
          ! at least for updrafts >~ 0.5 m/s.
          ! For thick clouds, activation takes place mainly at cloud base and as
          ! droplets are transported upwards through the cloud, their number
          ! decreases because of autoconversion, selfcollection and accretion/riming.
          !  (not sure, if the decrease will really be exponential, but a thick
          !   cloud will be nearly intransparent to light anyways and an exact
          !   resulting value for r_eff is not so important).
          zml_k = 0.5*(hhl(i,j,k)+hhl(i,j,k+1))
          IF (do_qnc_profile) THEN
            qnc = cloud_num(i,j,k) * MIN(EXP((z0_ncn-zml_k)/z1oe_ncn), 1.0_wp)
          ELSE
            qnc = cloud_num(i,j,k)
          END IF
!!$ Experiment: reduce qnc as function of qc:
          qnc = qnc * (1.0_wp - &
               MIN(MAX( (qc(i,j,k)*rhol(i,j,k)-rhoc_nchigh)/(rhoc_nclow-rhoc_nchigh), 0.0_wp), 1.0_wp) * &
               (1.0_wp-ncfact_low) )
          ! average cloud droplet mass:
          zxc = MIN(MAX( qc(i,j,k)/(qnc+eps2), zxc_min), zxc_max )
          ! effective radius
          r_effc(i,j,k) = c_reffc(1) *  zxc**c_reffc(2)
        ENDIF
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE diag_reffc_1mom

!HP!
! routine for cloud droplet number qnc print-out based on SUBROUTINE "diag_reffc_1mom"
SUBROUTINE qnc_calc(its, ite, jts, jte, kts, kte, &
     hhl, rhol, qc, qnc, qc_thresh, z0_ncn, z1oe_ncn, &
     rhoc_nclow, rhoc_nchigh, ncfact_low)

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  REAL (kind=wp), INTENT(in)        :: hhl(its:ite,jts:jte,kts:kte+1), rhol(its:ite,jts:jte,kts:kte), &
                                       qc(its:ite,jts:jte,kts:kte)                    ! kg/kg
  REAL (kind=wp), INTENT(in)        :: qc_thresh, z0_ncn, z1oe_ncn, &          ! 1/kg
                                       rhoc_nclow, rhoc_nchigh, ncfact_low
                                  
  REAL (kind=wp), INTENT(inout)        :: qnc(its:ite,jts:jte,kts:kte)

  INTEGER (kind=iintegers) :: i, j, k
  REAL (kind=wp)       ::  zml_k, qc_epsi

  qc_epsi=1e-9_wp

     DO k = kts, kte
      DO j = jts, jte
       DO i = its, ite
         IF (qc(i,j,k) > qc_epsi) THEN
        ! .. use same threshold as in src_radiation for grid scale clouds:
       ! IF (qc(i,j,k) > qc_thresh) THEN
          zml_k = 0.5*(hhl(i,j,k)+hhl(i,j,k+1))
          qnc(i,j,k) = qnc(i,j,k) * MIN(EXP((z0_ncn-zml_k)/z1oe_ncn), 1.0_wp)
          !!$ Experiment: reduce qnc as function of qc:
          qnc(i,j,k) = qnc(i,j,k) * (1.0_wp - &
               MIN(MAX( (qc(i,j,k)*rhol(i,j,k)-rhoc_nchigh)/(rhoc_nclow-rhoc_nchigh), 0.0_wp), 1.0_wp)&
                      *(1.0_wp-ncfact_low) )
      ! ENDIF
         ELSE
          qnc(i,j,k)= 0.0_wp
         ENDIF
       ENDDO
     ENDDO
    ENDDO

END SUBROUTINE qnc_calc


SUBROUTINE diag_reffr_1mom(its, ite, jts, jte, kts, kte, &
     rhol, qr, qr_thresh, c_reffr, r_effr_ini, r_effr)

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  REAL (kind=wp), INTENT(in)        :: rhol(its:ite,jts:jte,kts:kte), qr(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(in)        :: qr_thresh, c_reffr(2), r_effr_ini
  REAL (kind=wp), INTENT(out)       :: r_effr(its:ite,jts:jte,kts:kte)

  REAL(KIND=wp),  PARAMETER :: eps2 = 1e-20

  INTEGER (kind=iintegers) :: i, j, k

  r_effr= r_effr_ini ! preset reasonable value
  DO k = kts, kte
!CDIR COLLAPSE
    DO j = jts, jte
      DO i = its, ite
        ! .. use same threshold as in src_radiation for grid scale clouds:
        IF (qr(i,j,k) > qr_thresh) THEN
          ! effective radius
          r_effr(i,j,k) = c_reffr(1) *  (qr(i,j,k)*rhol(i,j,k))**c_reffr(2)
        ENDIF
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE diag_reffr_1mom

! routine for cloud ice (BEWARE OF THE xi_max = 1.0e-9_wp BELOW, WHICH COMES FROM THE COSMO MONODISPERSE ICE!!!):
SUBROUTINE diag_reffi_1mom(its, ite, jts, jte, kts, kte, &
     t, rhol, qi, bgeo_i, qi_thresh, &
     rhoi_nihigh, rhoi_nilow, nifact_low, &
     c_reffi_so, c_reffi_th, c_reffi_ls, &
     c_rhoibulk_ls, c_ari_th, r_effi_ini, rhoibulk_ls_ini, r_effi_so, r_effi_th, r_effi_ls, rhoibulk_ls, ar_i_th)

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  REAL (kind=wp), INTENT(in)        :: t(its:ite,jts:jte,kts:kte), qi(its:ite,jts:jte,kts:kte), &
                                           rhol(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(in)        :: bgeo_i, qi_thresh, c_reffi_so(2,2), c_reffi_th(2,4), &
                                           c_reffi_ls(2), c_rhoibulk_ls(2), r_effi_ini, rhoibulk_ls_ini, c_ari_th(2,8), &
                                           rhoi_nihigh, rhoi_nilow, nifact_low
  REAL (kind=wp), INTENT(out)       :: r_effi_so(its:ite,jts:jte,kts:kte,2)
  REAL (kind=wp), INTENT(out)       :: r_effi_th(its:ite,jts:jte,kts:kte,2)
  REAL (kind=wp), INTENT(out)       :: r_effi_ls(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(out)       :: rhoibulk_ls(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(out)       :: ar_i_th(its:ite,jts:jte,kts:kte,2)

  REAL(KIND=wp),  PARAMETER :: eps2 = 1e-20
  REAL (kind=wp), PARAMETER :: xi_min = 1.0e-15_wp  ! min. allowed mean ice particle mass
!!$  REAL (kind=wp), PARAMETER :: xi_max = 1.0e-9_wp   ! max. allowed mean ice particle mass (cf. COSMO docu)
  REAL (kind=wp), PARAMETER :: xi_max = 1.0e-8_wp   ! max. allowed mean ice particle mass (cf. COSMO docu)

  INTEGER (kind=iintegers) :: i, j, k
  REAL (kind=wp)       :: zxi, qni, temp


  r_effi_so  = r_effi_ini ! preset reasonable value
  r_effi_th  = r_effi_ini ! preset reasonable value
  r_effi_ls  = r_effi_ini * 10.0
  rhoibulk_ls = rhoibulk_ls_ini
  ar_i_th     = 1.0
!!$  rhoi_nihigh = 0.5e-5_wp     ! EXPERIMENT  Reduction of ice_num for thick clouds as function of rhoi
!!$  rhoi_nilow  = 2.0e-5_wp     ! EXPERIMENT
!!$  nifact_low  = 0.1               ! EXPERIMENT  [0 .... 1]
  DO k = kts, kte
!CDIR COLLAPSE
    DO j = jts, jte
      DO i = its, ite

        IF (qi(i,j,k) > qi_thresh) THEN

          ! .. ice crystal number is assumed to be equal to ice nuclei number,
          !    which is a function of temperature. The upper T-limit is
          !    due to the assumption that no nucleation takes place above
          !    that temperature, whereas the lower T-limit equals the
          !    homogeneous nucleation threshold, which imposes an upper
          !    limit on qni consistent to the cloud microphysics parameterizations
          !    below.
          temp = MIN(MAX(t(i,j,k), 236.15_wp), 267.15_wp)
          qni = ice_nuclei_number(temp, 273.15_wp)
!!$ Experiment: reduce qni as function of qi:
!!$          qni = qni * MIN(MAX( (rhoi_nlow-qi(i,j,k)*rhol(i,j,k))/(rhoi_nlow-rhoi_nhigh), 0.1_wp), 1.0_wp)
          qni = qni * (1.0_wp - &
               MIN(MAX( (qi(i,j,k)*rhol(i,j,k)-rhoi_nihigh)/(rhoi_nilow-rhoi_nihigh), 0.0_wp), 1.0_wp) * &
               (1.0_wp-nifact_low) )

          ! average ice-crystal mass assuming qni as function of T:
          zxi = MIN(MAX( qi(i,j,k)*rhol(i,j,k)/(qni+eps2), xi_min), xi_max)

          ! effective radius after Key et al. (2002) assuming hexagonal plates 
          ! oriented with their c-axes parallel to the direction of light:
          r_effi_so(i,j,k,1) = c_reffi_so(1,1) *  zxi**c_reffi_so(1,2)

          ! effective radius after Key et al. (2002) assuming hexagonal columns
          ! oriented with their c-axes parallel to the direction of light:
          r_effi_so(i,j,k,2) = c_reffi_so(2,1) *  zxi**c_reffi_so(2,2)

          ! effective radius after Fu et al. (1998) assuming hexagonal plates 
          ! randomly oriented in space:
          r_effi_th(i,j,k,1) = 0.5 / ( &
               c_reffi_th(1,1)*zxi**c_reffi_th(1,2) + &
               c_reffi_th(1,3)*zxi**c_reffi_th(1,4) &
               )
          ! effective radius after Fu et al. (1998) assuming hexagonal columns
          ! randomly oriented in space:
          r_effi_th(i,j,k,2) = 0.5 / ( &
               c_reffi_th(2,1)*zxi**c_reffi_th(2,2) + &
               c_reffi_th(2,3)*zxi**c_reffi_th(2,4) &
               )

          ! Reff for large-size limit:
          r_effi_ls(i,j,k) = c_reffi_ls(1) * zxi**c_reffi_ls(2)
          ! rhobulkquer for large-size limit:
          rhoibulk_ls(i,j,k) = c_rhoibulk_ls(1) * zxi**c_rhoibulk_ls(2)

          ! mean axis ratio after Fu (2007) assuming hexagonal plates 
          ! randomly oriented in space (AT THE MOMENT WE ASSUME COLUMNS,
          !  BECAUSE THE COEFFS ARE JUST THE SAME AS FOR THE COLUMNS. 
          !  PLATES NOT YET IMPLEMENTED!):
          ar_i_th(i,j,k,1) = &
               ( c_ari_th(1,1)*zxi**c_ari_th(1,2) +   &
                 c_ari_th(1,3)*zxi**c_ari_th(1,4)   ) &
               / & 
               ( c_ari_th(1,5)*zxi**c_ari_th(1,6) +   &
                 c_ari_th(1,7)*zxi**c_ari_th(1,8)   )
          ! mean axis ratio after Fu (2007) assuming hexagonal columns 
          ! randomly oriented in space:
          ar_i_th(i,j,k,2) = &
               ( c_ari_th(2,1)*zxi**c_ari_th(2,2) +   &
                 c_ari_th(2,3)*zxi**c_ari_th(2,4)   ) &
               / & 
               ( c_ari_th(2,5)*zxi**c_ari_th(2,6) +   &
                 c_ari_th(2,7)*zxi**c_ari_th(2,8)   )

        END IF
      ENDDO
    ENDDO
  ENDDO
  
END SUBROUTINE diag_reffi_1mom

!HP!
! routine for cloud ice number qni print-out
SUBROUTINE qni_calc(qni, its, ite, jts, jte, kts, kte, &
     t, rhol, qi, qi_thresh, rhoi_nihigh, rhoi_nilow, nifact_low)

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  REAL (kind=wp), INTENT(in)        :: t(its:ite,jts:jte,kts:kte), qi(its:ite,jts:jte,kts:kte), &
                                           rhol(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(in)        ::  qi_thresh,rhoi_nihigh, rhoi_nilow, nifact_low
  REAL (kind=wp), INTENT(inout)        :: qni(its:ite,jts:jte,kts:kte)

  INTEGER (kind=iintegers) :: i, j, k
  REAL (kind=wp)       :: temp

  DO k = kts, kte
!CDIR COLLAPSE
    DO j = jts, jte
      DO i = its, ite

        IF (qi(i,j,k) > qi_thresh) THEN
        !  WRITE (*,*) 'Harel Harel Harel Harel Harel Harel'
          ! .. ice crystal number is assumed to be equal to ice nuclei number,
          !    which is a function of temperature. The upper T-limit is
          !    due to the assumption that no nucleation takes place above
          !    that temperature, whereas the lower T-limit equals the
          !    homogeneous nucleation threshold, which imposes an upper
          !    limit on qni consistent to the cloud microphysics parameterizations
          !    below.
          temp = MIN(MAX(t(i,j,k), 236.15_wp), 267.15_wp)
          qni(i,j,k) = ice_nuclei_number(temp, 273.15_wp)
!!$ Experiment: reduce qni as function of qi:
!!$          qni = qni * MIN(MAX( (rhoi_nlow-qi(i,j,k)*rhol(i,j,k))/(rhoi_nlow-rhoi_nhigh), 0.1_wp), 1.0_wp)
          qni(i,j,k) = qni(i,j,k) * (1.0_wp - &
               MIN(MAX( (qi(i,j,k)*rhol(i,j,k)-rhoi_nihigh)/(rhoi_nilow-rhoi_nihigh), 0.0_wp), 1.0_wp) * &
               (1.0_wp-nifact_low) )

        END IF
      ENDDO
    ENDDO
  ENDDO
  
END SUBROUTINE qni_calc


! generic routine for snow and graupel: compute zxi within from additional factors
SUBROUTINE diag_reffs_1mom(its, ite, jts, jte, kts, kte, &
     rhol, qi, bgeo_i, qi_thresh, c_reffi_so, c_reffi_th, c_reffi_ls, c_rhoibulk_ls, c_ari_th, c_xiquer, &
     r_effi_ini, rhoibulk_ls_ini, r_effi_so, r_effi_th, r_effi_ls, rhoibulk_ls, ar_i_th, n0_i)

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  REAL (kind=wp), INTENT(in)        :: qi(its:ite,jts:jte,kts:kte), &
                                           rhol(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(in), OPTIONAL :: n0_i(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(in)        :: bgeo_i, qi_thresh, c_reffi_so(2,2), c_reffi_th(2,4), &
                                           c_reffi_ls(2), c_rhoibulk_ls(2), c_ari_th(2,8), &
                                           c_xiquer(2), r_effi_ini, rhoibulk_ls_ini
  REAL (kind=wp), INTENT(out)       :: r_effi_so(its:ite,jts:jte,kts:kte,2)
  REAL (kind=wp), INTENT(out)       :: r_effi_th(its:ite,jts:jte,kts:kte,2)
  REAL (kind=wp), INTENT(out)       :: r_effi_ls(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(out)       :: rhoibulk_ls(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(out)       :: ar_i_th(its:ite,jts:jte,kts:kte,2)

  REAL(KIND=wp),  PARAMETER :: eps2 = 1e-20
  REAL (kind=wp), PARAMETER :: xi_min = 1.0e-15_wp  ! min. allowed mean ice particle mass
  REAL (kind=wp), PARAMETER :: xi_max = 1.0e-4_wp   ! max. allowed mean ice particle mass

  INTEGER (kind=iintegers) :: i, j, k
  REAL (kind=wp)       :: zxi, qni


  r_effi_so  = r_effi_ini ! preset reasonable value
  r_effi_th  = r_effi_ini ! preset reasonable value
  r_effi_ls  = r_effi_ini * 10.0
  rhoibulk_ls = rhoibulk_ls_ini
  ar_i_th     = 1.0
  DO k = kts, kte
!CDIR COLLAPSE
    DO j = jts, jte
      DO i = its, ite

        IF (qi(i,j,k) > qi_thresh) THEN

          ! average ice-crystal mass:
          IF (PRESENT(n0_i)) THEN
            ! ... then the n0_i dependend prefactor for rhoibulk_ls is not
            !     yet included in the first coefficient. This might be the
            !     case if the n0_i is not constant but depends on the location,
            !     temperature etc. (e.g. for the COSMO 1-moment snow category)
            zxi =  c_xiquer(1)*(rhol(i,j,k)*qi(i,j,k)/n0_i(i,j,k))**c_xiquer(2)
          ELSE
            zxi =  c_xiquer(1)*(rhol(i,j,k)*qi(i,j,k))**c_xiquer(2)
          END IF
          zxi = MIN(MAX( zxi, xi_min), xi_max)

          ! effective radius after Key et al. (2002) assuming hexagonal plates 
          ! oriented with their c-axes parallel to the direction of light:
          r_effi_so(i,j,k,1) = c_reffi_so(1,1) *  zxi**c_reffi_so(1,2)

          ! effective radius after Key et al. (2002) assuming hexagonal columns
          ! oriented with their c-axes parallel to the direction of light:
          r_effi_so(i,j,k,2) = c_reffi_so(2,1) *  zxi**c_reffi_so(2,2)

          ! effective radius after Fu et al. (1998) assuming hexagonal plates 
          ! randomly oriented in space:
          r_effi_th(i,j,k,1) = 0.5 / ( &
               c_reffi_th(1,1)*zxi**c_reffi_th(1,2) + &
               c_reffi_th(1,3)*zxi**c_reffi_th(1,4) &
               )
          ! effective radius after Fu et al. (1998) assuming hexagonal columns
          ! randomly oriented in space:
          r_effi_th(i,j,k,2) = 0.5 / ( &
               c_reffi_th(2,1)*zxi**c_reffi_th(2,2) + &
               c_reffi_th(2,3)*zxi**c_reffi_th(2,4) &
               )

          ! Reff for large-size limit:
          r_effi_ls(i,j,k) = c_reffi_ls(1) * zxi**c_reffi_ls(2)
          ! rhobulkquer for large-size limit:
          rhoibulk_ls(i,j,k) = c_rhoibulk_ls(1) * zxi**c_rhoibulk_ls(2)

          ! mean axis ratio after Fu (2007) assuming hexagonal plates 
          ! randomly oriented in space (AT THE MOMENT WE ASSUME COLUMNS,
          !  BECAUSE THE COEFFS ARE JUST THE SAME AS FOR THE COLUMNS. 
          !  PLATES NOT YET IMPLEMENTED!):
          ar_i_th(i,j,k,1) = &
               ( c_ari_th(1,1)*zxi**c_ari_th(1,2) +   &
                 c_ari_th(1,3)*zxi**c_ari_th(1,4)   ) &
               / & 
               ( c_ari_th(1,5)*zxi**c_ari_th(1,6) +   &
                 c_ari_th(1,7)*zxi**c_ari_th(1,8)   )
          ! mean axis ratio after Fu (2007) assuming hexagonal columns 
          ! randomly oriented in space:
          ar_i_th(i,j,k,2) = &
               ( c_ari_th(2,1)*zxi**c_ari_th(2,2) +   &
                 c_ari_th(2,3)*zxi**c_ari_th(2,4)   ) &
               / & 
               ( c_ari_th(2,5)*zxi**c_ari_th(2,6) +   &
                 c_ari_th(2,7)*zxi**c_ari_th(2,8)   )

        END IF
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE diag_reffs_1mom


#ifdef TWOMOM_SB
!===========================================================================================
!===========================================================================================
!
! Subroutines for diagnosis of the cloud and ice effective radii
! for computation of the optical parameters of clouds, designed for the 
! parameter conventions of the 2-moment scheme.
!
! Effective radii after the following definitions are applied:
!
! cloud droplets       : reff = 3 V / 4 A
!
! cloud ice, "solar"   : reff after definition of Key et al. (2002)
!                        (for both hexagonal plates and hexagonal columns, regularly oriented)
!
! cloud ice, "thermal" : reff after definition of Fu et al. (1998)
!                        (for both hexagonal plates and hexagonal columns, randomly oriented)
!
! NOTE: the word "solar" and suffix "_so" for effective radii and its coefficients 
!       is used as a synonym for the Key et al. (2002) convention,
!       and the word "thermal" and suffix "_th" 
!       is used as a synonym for the Fu et al. (1996/1998) convention.
!
!===========================================================================================
!===========================================================================================

!===========================================================================================
!
! .. useful for R_eff after definitions of Fu and Key of polydisperse hexagonal ice particles:
!
!===========================================================================================

REAL(kind=wp) FUNCTION fracmoment_gamma(p,f) 

  USE wolken_konstanten, ONLY: particle

  IMPLICIT NONE

  REAL(kind=wp)    :: f
  TYPE(PARTICLE)   :: p
  
  fracmoment_gamma  = gamma_fct((f+p%nu+1.0)/p%mu) / gamma_fct((p%nu+1.0)/p%mu)        &
       &     * ( gamma_fct((  p%nu+1.0)/p%mu) / gamma_fct((p%nu+2.0)/p%mu) )**f
END FUNCTION fracmoment_gamma

REAL(kind=wp) FUNCTION reffi_gammfac(p,f) 

  USE wolken_konstanten, ONLY: particle

  IMPLICIT NONE

  REAL(kind=wp)    :: f
  TYPE(PARTICLE)   :: p

  ! .. Assumption: N(x) = N0 * x^nu * exp(-lam*x^mu)
  !                  D  = ageo * x^bgeo

  reffi_gammfac  = gamma_fct((p%nu+2.0)/p%mu) / gamma_fct((p%nu+2.0-f)/p%mu)        &
       &     * ( gamma_fct((  p%nu+1.0)/p%mu) / gamma_fct((p%nu+2.0)/p%mu) )**f
END FUNCTION reffi_gammfac

!===========================================================================================
!
! .. Constant prefactors for the calculation of effective radii (call once at the beginning):
!
!===========================================================================================

! Generic routine for all ice hydrometeors:
SUBROUTINE prefactors_reff_2mom_solid(istore, ldebug, pi, parti, c_reffi_so, c_reffi_th, c_reffi_ls, c_rhoiquer, c_ari_th)

  USE wolken_konstanten, ONLY: particle, rho_ice

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: istore
  REAL (kind=wp), INTENT(in)  :: pi
  LOGICAL, INTENT(in)             :: ldebug
  TYPE(particle), INTENT(in)      :: parti
  REAL (kind=wp), INTENT(out) :: c_reffi_so(2,2), c_reffi_th(2,4), c_ari_th(2,8), &
                                     c_reffi_ls(2), c_rhoiquer(2)

  INTEGER (kind=iintegers), PARAMETER :: istore_max = 10

  LOGICAL, SAVE :: firstcall(istore_max) = .TRUE.
  REAL (kind=wp), SAVE  :: c_reffi_so_s(2,2,istore_max), c_reffi_th_s(2,4,istore_max), &
                           c_reffi_ls_s(2,istore_max),   c_rhoiquer_s(2,istore_max), &
                           c_ari_th_s(2,8,istore_max)
  REAL (kind=wp) :: kappa

  CHARACTER(len=1000), SAVE :: parti_s(istore_max)
  CHARACTER(len=1000)       :: parti_a

  IF (istore < 0 .OR. istore > istore_max) THEN
    WRITE (*,*) 'ERROR: wrong value of istore in call to prefactors_reff_2mom_solid(): ', istore
    STOP
  END IF

  IF (firstcall(istore)) THEN
    parti_s(istore)(:)   = ' '
    WRITE (parti_s(istore),   *) parti
  END IF

  parti_a(:) = ' '
  WRITE (parti_a, *) parti

  IF (firstcall(istore) .OR. TRIM(parti_a) /= TRIM(parti_s(istore)) ) THEN

    IF (ldebug) THEN
      WRITE (*,*) '  pp_utilities:  computing prefactors_reff_2mom_solid() for istore = ', istore
    END IF

    ! .. r_eff for ice solar, def. of Key et al. (2002), hex plates:
    c_reffi_so_s(1,1,istore) = 2.0*SQRT(3.0)/(3.0*parti%a_geo**2*rho_ice)         * reffi_gammfac(parti, 1.0-2.0*parti%b_geo   )
    c_reffi_so_s(1,2,istore) = (1.0-2.0*parti%b_geo)
    ! .. r_eff for ice solar, def. of Key et al. (2002), hex columns:
    c_reffi_so_s(2,1,istore) = SQRT( 27.0*SQRT(3.0)/(128.0*parti%a_geo*rho_ice) ) * reffi_gammfac(parti, 0.5*(1.0-parti%b_geo) )
    c_reffi_so_s(2,2,istore) = (0.5*(1.0-parti%b_geo))
    ! .. r_eff for ice thermal, def. of Fu et al. (1998), hex plates, needs four coefficients:
    c_reffi_th_s(1,1,istore) = 1.0/parti%a_geo                                    / reffi_gammfac(parti, parti%b_geo           )
    c_reffi_th_s(1,2,istore) = (-parti%b_geo)
    c_reffi_th_s(1,3,istore) = 9.0*parti%a_geo**2*rho_ice / 32.0                  / reffi_gammfac(parti, 1.0-2.0*parti%b_geo   )
    c_reffi_th_s(1,4,istore) = (2.0*parti%b_geo-1.0)
    ! .. r_eff for ice thermal, def. of Fu et al. (1998), hex columns, needs four coefficients:
    c_reffi_th_s(2,1,istore) = SQRT( 3.0*SQRT(3.0)*parti%a_geo*rho_ice / 8.0 )    / reffi_gammfac(parti, 0.5*(1.0-parti%b_geo) )
    c_reffi_th_s(2,2,istore) = (0.5*(parti%b_geo-1.0))
    c_reffi_th_s(2,3,istore) = SQRT(3.0)/(4.0*parti%a_geo)                        / reffi_gammfac(parti, parti%b_geo           )
    c_reffi_th_s(2,4,istore) = (-parti%b_geo)

    ! .. The following coefficients are for r_eff for large-size limit: Reff = c1 * xquer_i**c2:
    c_reffi_ls_s(1,istore)   = 0.5 * parti%a_geo * fracmoment_gamma(parti,3.0*parti%b_geo) / &
                                                   fracmoment_gamma(parti,2.0*parti%b_geo)
    c_reffi_ls_s(2,istore)   = parti%b_geo

    ! .. The following coefficients are for mean axis ratio AR (D/L) for the parameterization
    !    of g and f_d after Fu (2007). So far we implement only AR <=1, which means hexagonal columns.
    !    The formula for polydisperse PSDs is:
    !      AR = (c1 * xquer_i**c2 + c3 * xquer_i**c4) / (c5 * xquer_i**c6 + c7 * xquer_i**c8)
    kappa = SQRT( 8.0 / (3.0*SQRT(3.0)*rho_ice*parti%a_geo) )
    c_ari_th_s(2,2,istore)  =  ( 0.5*(3.0-5.0*parti%b_geo) )
    c_ari_th_s(2,1,istore)  =  0.25*SQRT(3.0)*kappa*kappa/parti%a_geo   * fracmoment_gamma( p=parti, f=c_ari_th_s(2,2,istore) )
    c_ari_th_s(2,4,istore)  =  ( 1.0-parti%b_geo )
    c_ari_th_s(2,3,istore)  =  kappa                                    * fracmoment_gamma( p=parti, f=c_ari_th_s(2,4,istore) )
    c_ari_th_s(2,6,istore)  =   c_ari_th_s(2,4,istore)
    c_ari_th_s(2,5,istore)  =   0.25*SQRT(3.0) * c_ari_th_s(2,3,istore)
    c_ari_th_s(2,8,istore)  =   ( 0.5*(1.0+parti%b_geo) )
    c_ari_th_s(2,7,istore)  =   parti%a_geo                             * fracmoment_gamma( p=parti, f=c_ari_th_s(2,8,istore) )

    ! .. currently the coeff. for hex. plates are not present, but just
    !    set to the ones for hex. columns:
    c_ari_th_s(1,:,istore) = c_ari_th_s(2,:,istore)

    ! ..  sphere-volume-weighted equivalent average bulk particle density for the large-size limit:
    !     rhobulkiquer =  c1 * xquer_i**c2
    c_rhoiquer_s(1,istore)   = 6.0/(pi*parti%a_geo**3) * reffi_gammfac(parti,1.0-3.0*parti%b_geo)
    c_rhoiquer_s(2,istore)   = 1.0-3.0*parti%b_geo

    firstcall(istore) = .FALSE.
    parti_s(istore)     = parti_a

  END IF

  c_reffi_so = c_reffi_so_s(:,:,istore)
  c_reffi_th = c_reffi_th_s(:,:,istore)
  c_reffi_ls = c_reffi_ls_s(:,istore)
  c_rhoiquer = c_rhoiquer_s(:,istore)
  c_ari_th   = c_ari_th_s  (:,:,istore)

END SUBROUTINE prefactors_reff_2mom_solid

SUBROUTINE prefactors_reff_2mom_cloud(ldebug, c_reffc)

  USE wolken_konstanten, ONLY: cloud

  IMPLICIT NONE

  LOGICAL, INTENT(in)             :: ldebug
  REAL (kind=wp), INTENT(out) :: c_reffc(2)

  LOGICAL, SAVE :: firstcall = .TRUE.
  REAL (kind=wp), SAVE  :: c_reffc_s(2)

  CHARACTER(len=1000), SAVE :: cloud_s
  CHARACTER(len=1000)       :: cloud_a

  IF (firstcall) THEN
    cloud_s(:) = ' '
    WRITE (cloud_s, *) cloud
  END IF

  cloud_a(:) = ' '
  WRITE (cloud_a, *) cloud

  IF (firstcall .OR. TRIM(cloud_a) /= TRIM(cloud_s) ) THEN

    IF (ldebug) THEN
      WRITE (*,*) '  pp_utilities:  computing prefactors_reff_2mom()'
    END IF

    ! .. r_eff for cloud droplets solar / thermal:
    c_reffc_s(1) = 0.5 * cloud%a_geo * fracmoment_gamma(cloud, 3.0*cloud%b_geo) / &
                                       fracmoment_gamma(cloud, 2.0*cloud%b_geo )
    c_reffc_s(2) = cloud%b_geo

    firstcall = .FALSE.
    cloud_s   = cloud_a

  END IF

  c_reffc    = c_reffc_s

END SUBROUTINE prefactors_reff_2mom_cloud

SUBROUTINE prefactors_reff_2mom_rain(ldebug, c_reffr)

  USE wolken_konstanten, ONLY: rain

  IMPLICIT NONE

  LOGICAL, INTENT(in)             :: ldebug
  REAL (kind=wp), INTENT(out) :: c_reffr(2)

  LOGICAL, SAVE :: firstcall = .TRUE.
  REAL (kind=wp), SAVE  :: c_reffr_s(2)

  CHARACTER(len=1000), SAVE :: rain_s
  CHARACTER(len=1000)       :: rain_a

  IF (firstcall) THEN
    rain_s(:) = ' '
    WRITE (rain_s, *) rain
  END IF

  rain_a(:) = ' '
  WRITE (rain_a, *) rain

  IF (firstcall .OR. TRIM(rain_a) /= TRIM(rain_s) ) THEN

    IF (ldebug) THEN
      WRITE (*,*) '  pp_utilities:  computing prefactors_reff_2mom()'
    END IF

    ! .. r_eff for rain droplets solar / thermal:
    c_reffr_s(1) = 0.5 * rain%a_geo * fracmoment_gamma(rain, 3.0*rain%b_geo) / &
                                      fracmoment_gamma(rain, 2.0*rain%b_geo )
    c_reffr_s(2) = rain%b_geo

    firstcall = .FALSE.
    rain_s   = rain_a

  END IF

  c_reffr    = c_reffr_s

END SUBROUTINE prefactors_reff_2mom_rain


!===========================================================================================
!
! .. Diagnosis of effective radii for gridscale clouds
!    (subgrid scale clouds are treated directly in the radiation scheme):
!
!===========================================================================================

SUBROUTINE diag_reffc_2mom(its, ite, jts, jte, kts, kte, &
     qc, qnc, qc_thresh, c_reffc, r_effc_ini, r_effc)

  USE wolken_konstanten, ONLY: cloud

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  REAL (kind=wp), INTENT(in)        :: qc(its:ite,jts:jte,kts:kte), qnc(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(in)        :: c_reffc(2), qc_thresh, r_effc_ini
  REAL (kind=wp), INTENT(out)       :: r_effc(its:ite,jts:jte,kts:kte)

  REAL(KIND=wp),    PARAMETER :: eps2 = 1e-20

  INTEGER (kind=iintegers) :: i, j, k
  REAL (kind=wp)       :: zxc

  r_effc= r_effc_ini ! preset reasonable value
  DO k = kts, kte
!CDIR COLLAPSE
    DO j = jts, jte
      DO i = its, ite
        ! .. use same threshold as in src_radiation for grid scale clouds:
        IF (qc(i,j,k) > qc_thresh) THEN
          ! average cloud droplet mass
          zxc = MIN(MAX(qc(i,j,k)/(qnc(i,j,k)+eps2),cloud%x_min),cloud%x_max)          
          ! effective radius
          r_effc(i,j,k) = c_reffc(1) *  zxc**c_reffc(2)
        ENDIF
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE diag_reffc_2mom

SUBROUTINE diag_reffr_2mom(its, ite, jts, jte, kts, kte, &
     qr, qnr, qr_thresh, c_reffr, r_effr_ini, r_effr)

  USE wolken_konstanten, ONLY: rain

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  REAL (kind=wp), INTENT(in)        :: qr(its:ite,jts:jte,kts:kte), qnr(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(in)        :: c_reffr(2), qr_thresh, r_effr_ini
  REAL (kind=wp), INTENT(out)       :: r_effr(its:ite,jts:jte,kts:kte)

  REAL(KIND=wp),    PARAMETER :: eps2 = 1e-20

  INTEGER (kind=iintegers) :: i, j, k
  REAL (kind=wp)       :: zxr

  r_effr= r_effr_ini ! preset reasonable value
  DO k = kts, kte
!CDIR COLLAPSE
    DO j = jts, jte
      DO i = its, ite
        ! .. use same threshold as in src_radiation for grid scale clouds:
        IF (qr(i,j,k) > qr_thresh) THEN
          ! average rain drop mass
          zxr = MIN(MAX(qr(i,j,k)/(qnr(i,j,k)+eps2),rain%x_min),rain%x_max)          
          ! effective radius
          r_effr(i,j,k) = c_reffr(1) *  zxr**c_reffr(2)
        ENDIF
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE diag_reffr_2mom

! Generic routine for all ice (solid) species of the 2-moment scheme:
SUBROUTINE diag_reffs_2mom(its, ite, jts, jte, kts, kte, &
     parti, qi, qni, qi_thresh, c_reffi_so, c_reffi_th, c_reffi_ls, c_rhoibulk_ls, c_ari_th, r_effi_ini, &
     r_effi_so, r_effi_th, r_effi_ls, rhoibulk_ls, ar_i_th)

  USE wolken_konstanten, ONLY: particle

  IMPLICIT NONE

  INTEGER (kind=iintegers), INTENT(in)  :: its, ite, jts, jte, kts, kte
  TYPE(particle), INTENT(in)            :: parti
  REAL (kind=wp), INTENT(in)        :: qi(its:ite,jts:jte,kts:kte), qni(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(in)        :: qi_thresh, c_reffi_so(2,2), c_reffi_th(2,4), c_reffi_ls(2), &
                                       c_rhoibulk_ls(2), r_effi_ini, c_ari_th(2,8)
  REAL (kind=wp), INTENT(out)       :: r_effi_so(its:ite,jts:jte,kts:kte,2)
  REAL (kind=wp), INTENT(out)       :: r_effi_th(its:ite,jts:jte,kts:kte,2)
  REAL (kind=wp), INTENT(out)       :: r_effi_ls(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(out)       :: rhoibulk_ls(its:ite,jts:jte,kts:kte)
  REAL (kind=wp), INTENT(out)       :: ar_i_th(its:ite,jts:jte,kts:kte,2)

  REAL(KIND=wp),  PARAMETER :: eps2 = 1e-20

  INTEGER (kind=iintegers) :: i, j, k
  REAL (kind=wp)       :: zxi

  r_effi_so  = r_effi_ini ! preset reasonable value
  r_effi_th  = r_effi_ini ! preset reasonable value
  r_effi_ls  = r_effi_ini * 10.0
  rhoibulk_ls = 100.0_wp
  ar_i_th     = 1.0
  DO k = kts, kte
!CDIR COLLAPSE
    DO j = jts, jte
      DO i = its, ite
        ! .. use same threshold as in src_radiation for grid scale clouds:
        IF (qi(i,j,k) > qi_thresh) THEN
          ! average ice-crystal mass
          zxi = MIN(MAX(qi(i,j,k)/(qni(i,j,k)+eps2),parti%x_min),parti%x_max)

          ! effective radius after Key et al. (2002) assuming hexagonal plates 
          ! oriented with their c-axes parallel to the direction of light:
          r_effi_so(i,j,k,1) = c_reffi_so(1,1) *  zxi**c_reffi_so(1,2)

          ! effective radius after Key et al. (2002) assuming hexagonal columns 
          ! oriented with their c-axes parallel to the direction of light:
          r_effi_so(i,j,k,2) = c_reffi_so(2,1) *  zxi**c_reffi_so(2,2)

          ! effective radius after Fu et al. (1998) assuming hexagonal plates 
          ! randomly oriented in space:
          r_effi_th(i,j,k,1) = 0.5 / ( &
               c_reffi_th(1,1)*zxi**c_reffi_th(1,2) + &
               c_reffi_th(1,3)*zxi**c_reffi_th(1,4) &
               )
          ! effective radius after Fu et al. (1998) assuming hexagonal columns
          ! randomly oriented in space:
          r_effi_th(i,j,k,2) = 0.5 / ( &
               c_reffi_th(2,1)*zxi**c_reffi_th(2,2) + &
               c_reffi_th(2,3)*zxi**c_reffi_th(2,4) &
               )

          ! Reff for large-size limit:
          r_effi_ls(i,j,k) = c_reffi_ls(1) * zxi**c_reffi_ls(2)
          ! rhobulkquer for large-size limit:
          rhoibulk_ls(i,j,k) = c_rhoibulk_ls(1) * zxi**c_rhoibulk_ls(2)

          ! mean axis ratio after Fu (2007) assuming hexagonal plates 
          ! randomly oriented in space (AT THE MOMENT WE ASSUME COLUMNS,
          !  BECAUSE THE COEFFS ARE JUST THE SAME AS FOR THE COLUMNS. 
          !  PLATES NOT YET IMPLEMENTED!):
          ar_i_th(i,j,k,1) = &
               ( c_ari_th(1,1)*zxi**c_ari_th(1,2) +   &
                 c_ari_th(1,3)*zxi**c_ari_th(1,4)   ) &
               / & 
               ( c_ari_th(1,5)*zxi**c_ari_th(1,6) +   &
                 c_ari_th(1,7)*zxi**c_ari_th(1,8)   )
          ! mean axis ratio after Fu (2007) assuming hexagonal columns 
          ! randomly oriented in space:
          ar_i_th(i,j,k,2) = &
               ( c_ari_th(2,1)*zxi**c_ari_th(2,2) +   &
                 c_ari_th(2,3)*zxi**c_ari_th(2,4)   ) &
               / & 
               ( c_ari_th(2,5)*zxi**c_ari_th(2,6) +   &
                 c_ari_th(2,7)*zxi**c_ari_th(2,8)   )

        END IF
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE diag_reffs_2mom

#endif

!==============================================================================
!==============================================================================

END MODULE reff_calc_utilities
 
