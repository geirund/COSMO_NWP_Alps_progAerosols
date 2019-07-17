!+ src module for computing effective radii from COSMO microphysics
!-----------------------------------------------------------------------------------

!!$ UB june 2016: added optional argument "qc_for_reff" to subroutines

MODULE src_reff_calc

!------------------------------------------------------------------------------
!
! Description:
!   This module provides computation routines from computing hydrometeor
!   effective radii (Reff) for all COSMO microphysics schemes.
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

USE data_modelconfig, ONLY :   &
    ie,           & ! number of grid points in zonal direction
    je,           & ! number of grid points in meridional direction
    ke,           & ! number of grid points in vertical direction
    idt_qc,  idt_qr,  idt_qi,  idt_qs,  idt_qg,  idt_qh, &
    idt_qnc, idt_qnr, idt_qni, idt_qns, idt_qng, idt_qnh    

!------------------------------------------------------------------------------

USE data_constants  , ONLY :   &
    pi,           & ! 
    rho_w,        & ! density of liquid water
    rho_ice         ! density of ice          (kg/m^3)

!------------------------------------------------------------------------------

USE data_gscp,       ONLY : mu_rain

!------------------------------------------------------------------------------

USE data_fields     , ONLY :   &
     t, rho, hhl,    &
     refftc     ,    & ! global instance of type t_reff for eff. radii of cloud
     refftr     ,    & ! global instance of type t_reff for eff. radii of rain
     reffti     ,    & ! global instance of type t_reff for eff. radii of cloud ice
     reffts     ,    & ! global instance of type t_reff for eff. radii of snow
#ifdef TWOMOM_SB
     reffth     ,    & ! global instance of type t_reff for eff. radii of hail
#endif
     refftg            ! global instance of type t_reff for eff. radii of graupel

!------------------------------------------------------------------------------

USE data_runcontrol , ONLY :   &
    nnow,           & ! corresponds to ntstep 
    nnew,           & ! corresponds to ntstep + 1
    itype_gscp,     & ! type of grid-scale precipitation physics   
    lgsp,           & ! forecast with grid scale precipitation
    icloud_num_type_rad, &  ! type of cloud_num parameterization for itype_gscp=4
                            ! 1 = take cloud_num constant from TUNING
                            ! 2 = derive from Tegen aerosol climatology (only effective
                            !     if itype_aerosol = 2)
    itype_aerosol,  & ! type of aerosol map
    iradpar_cloud,  & ! type of parameterization of radiative transfer parameters (ext., sing. alb., asym.)  
                      ! for grid- and subgrid scale clouds (cloud water, ice water)
                      !   1 = old method
                      !   2 = based on eff. radiusf, fits from U. Blahak and E. Zubler (solar: Key et al., thermal: Fu et al.)
                      !   3 = based on eff. radius, fits from U. Blahak and B. Ritter (solar and thermal: Fu et al.)
                      !   4 = based on eff. radius and mean aspect ratio, fits from U. Blahak and H. Muskatel (solar and thermal: own fits based on raw data obtained by Q. Fu)

    lrad_incl_qrqsqg,&  ! include or exclude QR, QS and QG from radiative transfer calculations
    ldebug_gsp

!------------------------------------------------------------------------------

USE data_radiation, ONLY :   &
    reff_ini_c,       & ! background eff. Radius for cloud drops
    reff_ini_i,       & ! background eff. Radius for cloud ice
    rhobulk_ls_ini_i, & ! background sphere-volume-mean bulk density of cloud ice  (for init. and pure subgrid scale clouds)
    cloud_num_rad,    & ! cloud droplet number concentration assumed for radiation feedback at MSL
    zref_cloud_num_rad, &  ! height of lower layer with constant cloud number conc. above MSL in m
    dz_oe_cloud_num_rad, & ! 1/e decrease height in m of exponential decreasing
                           !   cloud number conc. above zref_cloud_num_rad
    rhos_n0shigh_rad, &  ! for rhos smaller than this value, n0s is not reduced [kg/m^3]
    rhos_n0slow_rad, &   ! for rhos_n0high_rad < rhos < rhos_n0low_rad, n0s is linearily reduced towards n0s_low_rad [kg/m^3]
    n0s_low_rad, &       ! for rhos >= rhos_n0low_rad, n0s attains this constant value [m^-3]
    rhoc_nchigh_rad, & ! for rhoc <= rhoc_nchigh_rad, cloud_num_rad is not reduced as function of rhoc [kg/m^3]
    rhoc_nclow_rad, &  ! for rhoc_nchigh_rad < rhoc < rhoc_nclow_rad, cloud_num_rad is linearily reduced as function of rhoc [kg/m^3]
    ncfact_low_rad, &  ! for rhoc >= rhoc_nclow_rad, the linear reduction bottoms out at the ncfact_low_rad'th fraction of cloud_num_rad  [0...1]
    rhoi_nihigh_rad, & ! for rhoi <= rhoi_nihigh_rad, ni(T) is not reduced as function of rhoc [kg/m^3]
    rhoi_nilow_rad, &  ! for rhoi_nihigh_rad < rhoi < rhoi_nilow_rad, ni(T) is linearily reduced as function of rhoi [kg/m^3]
    nifact_low_rad     ! for rhoi >= rhoi_nilow_rad, the linear reduction bottoms out at the nifact_low_rad'th fraction of ni(T)  [0...1]

!------------------------------------------------------------------------------

USE data_parallel,            ONLY : my_cart_id

!------------------------------------------------------------------------------

USE reff_calc_utilities,      ONLY : ice_nuclei_number,              calc_n0_snow,    &
                                     prefactors_reff_1mom_cloud,     diag_reffc_1mom, &
                                     prefactors_reff_1mom_rain,      diag_reffr_1mom, &
                                     prefactors_reff_1mom_icemono,   diag_reffi_1mom, &
                                     prefactors_reff_1mom_solidpoly, diag_reffs_1mom
#ifdef TWOMOM_SB
USE reff_calc_utilities,      ONLY:  prefactors_reff_2mom_cloud, diag_reffc_2mom, &
                                     prefactors_reff_2mom_rain,  diag_reffr_2mom, &
                                     prefactors_reff_2mom_solid, diag_reffs_2mom
#endif

!------------------------------------------------------------------------------

USE src_tracer,               ONLY : trcr_get, trcr_errorstr
USE data_tracer,              ONLY : T_ERR_NOTFOUND

!------------------------------------------------------------------------------

USE environment,              ONLY:  model_abort

!------------------------------------------------------------------------------

IMPLICIT NONE

!------------------------------------------------------------------------------

CONTAINS

!==============================================================================
!==============================================================================
!
! Subroutines for diagnosis of the cloud and ice effective radii
! for computation of the optical parameters of clouds, designed for the 
! parameter conventions of the 1-moment schemes (calc_reff_rad) and
! for the 2-moment scheme (calc_reff_rad_2mom).
!
! These routines are called from src_radiation.f90 in the case
! iradpar_cloud = 2, 3 and 4.
!
! Radii for the following definitions are calculated:
!
! cloud droplets       : reff = 3 V / 4 A
!
! cloud ice, "solar"   : reff after definition of Key et al. (2002)
!                        (for both hexagonal plates and hexagonal columns, regularly oriented)
!
! cloud ice, "thermal" : reff after definition of Fu et al. (1998)
!                        (for both hexagonal plates and hexagonal columns, randomly oriented)
!
! 
! IS CALLED IN THE RADIATION SCHEME ON THE APPROPRIATE TIME LEVEL NTLEV!
!
! Subroutine arguments:
!
!   ntlev            time level index
!   qc_thresh        qc threshold above which an Reff is computed (for gridpoints with
!                      equal or smaller values, Reff is initialized with namelist parameter
!                      reff_ini_c)
!   qi_thresh        qi threshold above which an Reff is computed (for gridpoints with
!                      equal or smaller values, Reff is initialized with namelist parameter
!                      reff_ini_i, but is overwritten later in src_radiation.f90).
!   qc_for_reff      3D field of an alternative cloud water content to use in Reff computation,
!                      which can be different from the grid scale values. It
!                      can be used, e.g., for a combinded field of grid scale values
!                      and subgrid scale values, so that Reff for pure SGS clouds will not
!                      be defined as constant by reff_ini_c but follows from xc = qc_for_reff / qnc 
!
! NOTE: the word "solar" and suffix "_so" for effective radii and its coefficients 
!       is used as a synonym for the Key et al. (2002) convention,
!       and the word "thermal" and suffix "_th" 
!       is used as a synonym for the Fu et al. (1996/1998) convention.
!
!===========================================================================================
!===========================================================================================

SUBROUTINE calc_reff_rad (ntlev, qc_thresh, qi_thresh, qc_for_reff)

  IMPLICIT NONE

  INTEGER(kind=iintegers), INTENT(in) :: ntlev
  REAL(kind=wp), INTENT(in)           :: qc_thresh, qi_thresh
  REAL(kind=wp), INTENT(in), OPTIONAL :: qc_for_reff(:,:,:)

  INTEGER(kind=iintegers) :: i, j, k
  REAL(kind=wp), ALLOCATABLE :: zn0_s(:,:,:)
  REAL(kind=wp) :: ageoi, bgeoi, ageos, bgeos, ageog, bgeog, n0_s_const, n0_g_const, n0_r_const, zxi
  LOGICAL :: do_qnc_profile

  ! .. integer flags denoting certain hydrometeor types in the
  !    calls to prefactors_reff_1mom_solidpoly() and prefactors_reff_2mom_solid():
  INTEGER(kind=iintegers), PARAMETER :: iice  = 1
  INTEGER(kind=iintegers), PARAMETER :: isnow = 2
  INTEGER(kind=iintegers), PARAMETER :: igrau = 3

  INTEGER (KIND=iintegers) :: izerror
  CHARACTER (LEN=255)      :: yzerrmsg, yzroutine

! Tracer pointers
!----------------
  REAL (KIND=wp),     POINTER :: &
    qc      (:,:,:)=> NULL(),   & ! QC at ntlev
    qnc     (:,:,:)=> NULL(),   & ! NCCLOUD at ntlev
    qr      (:,:,:)=> NULL(),   & ! QR at ntlev
    qi      (:,:,:)=> NULL(),   & ! QI at ntlev
    qs      (:,:,:)=> NULL(),   & ! QS at ntlev
    qg      (:,:,:)=> NULL()      ! QG at ntlev

  yzroutine(:) = ' '
  yzroutine = 'calc_reff_rad'

  IF (.NOT. lgsp) THEN
    WRITE (*,*) 'INFO from '//TRIM(yzroutine)//'(): lgsp = .FALSE., so no effective radii for hydrometeors are computed!'
    RETURN
  END IF

  izerror = 0_iintegers
  yzerrmsg(:) = ' '

  ! retrieve the required microphysics tracers at timelevel ntlev
  CALL trcr_get(izerror, idt_qc, ptr_tlev = ntlev, ptr = qc)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qr, ptr_tlev = ntlev, ptr = qr)
  IF (izerror /= 0_iintegers .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qi, ptr_tlev = ntlev, ptr = qi)
  IF (izerror /= 0_iintegers .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qs, ptr_tlev = ntlev, ptr = qs)
  IF (izerror /= 0_iintegers .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qg, ptr_tlev = ntlev, ptr = qg)
  IF (izerror /= 0_iintegers .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF

  IF (itype_aerosol == 2 .AND. icloud_num_type_rad == 2) THEN
    CALL trcr_get(izerror, idt_qnc, ptr_tlev = ntlev, ptr = qnc)
    IF (izerror /= 0_iintegers) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
    do_qnc_profile = .FALSE.  ! The exp. vertical profile is already contained in the qnc field
  ELSE
    ALLOCATE(qnc(ie,je,ke))
    qnc (:,:,:) = cloud_num_rad
    do_qnc_profile = .TRUE.
  END IF

  ! .. Mass-size-relation parameters:
  ageoi = 130.0_wp
  bgeoi = 3.0_wp
  ageog = 169.6_wp
  bgeog = 3.1_wp
  IF (itype_gscp == 4) THEN
    ageos = 0.038_wp
    bgeos = 2.0_wp
  ELSE
    ageos = 0.069_wp
    bgeos = 2.0_wp
  END IF

  ! .. constant N0-parameter for rain for itype_gscp = 3/4:
  n0_r_const = 8e6_wp*EXP(3.2*mu_rain)*(0.01)**(-mu_rain)
  ! .. old constant N0-parameter for snow for itype_gscp=2 (T_a and q_s are just dummies!):
  CALL calc_n0_snow(isnow_n0temp=0_iintegers, T_a=270.0_wp, q_s=1e-4_wp, &
                    ageos=ageos, n0_s=n0_s_const)
  ! .. constant N0-parameter for graupel:
  n0_g_const = 4.0E6_wp

  ! .. diagnose the N0-parameter of snow if needed below:
  IF (itype_gscp >= 3) THEN

    ALLOCATE(zn0_s(ie,je,ke))

!!$    rhos_n0high_rad = 1e-5_wp
!!$    rhos_n0low_rad  = 5e-5_wp
!!$    n0s_low_rad   = 8e5_wp

    DO k=1, ke
      DO j=1, je
        DO i=1, ie
          CALL calc_n0_snow(isnow_n0temp=2_iintegers, T_a=t(i,j,k,ntlev), q_s=qs(i,j,k)*rho(i,j,k), &
                            ageos=ageos, n0_s=zn0_s(i,j,k))
          ! .. Experiment: reduce n0s for higher values of qs to make optically thick clouds "thinner":
          zn0_s(i,j,k) = zn0_s(i,j,k) + (n0s_low_rad-zn0_s(i,j,k)) * &
               MAX(MIN((qs(i,j,k)*rho(i,j,k)-rhos_n0shigh_rad)/(rhos_n0slow_rad-rhos_n0shigh_rad),1.0_wp),0.0_wp)
        END DO
      END DO
    END DO
  END IF


  ! .. Diagnosis of Reff:
  SELECT CASE (itype_gscp)

  CASE (4)

    CALL prefactors_reff_1mom_cloud (pi=pi, nue_c=5.0_wp, mue_c=1.0_wp, &
         rho_w=rho_w, ldebug=ldebug_gsp, &
         c_reffc=refftc%c_reff)
    ! .. cloud ice:
    CALL prefactors_reff_1mom_icemono (pi=pi, &
         ageo_i=ageoi, bgeo_i=bgeoi, rho_ice=rho_ice, &
         ldebug=ldebug_gsp, &
         c_reffi_so=reffti%c_reff_so, &
         c_reffi_th=reffti%c_reff_th, &
         c_reffi_ls=reffti%c_reff_ls, &
         c_rhoiquer=reffti%c_rhoquer, &
         c_ari_th  =reffti%c_ar_th    &
         )
    IF (lrad_incl_qrqsqg) THEN
      ! .. rain:
      CALL prefactors_reff_1mom_rain (pi=pi, n0_r=n0_r_const, &
           nue_r=mu_rain, mue_r=1.0_wp, &
           rho_w=rho_w, ldebug=ldebug_gsp, &
           c_reffr=refftr%c_reff)
      ! .. snow: the n0_i=-999.99 is a neutral dummy value. n0_i(T) is later inputted into diag_reffs_1mom()!
      CALL prefactors_reff_1mom_solidpoly (istore=isnow, pi=pi, &
           ageo_i=ageos, bgeo_i=bgeos, &
           n0_i=-999.99_wp, nue_i=0.0_wp, mue_i=1.0_wp, rho_ice=rho_ice, &
           ldebug=ldebug_gsp, &
           c_reffi_so=reffts%c_reff_so, &
           c_reffi_th=reffts%c_reff_th, &
           c_reffi_ls=reffts%c_reff_ls, &
           c_rhoiquer=reffts%c_rhoquer, &
           c_ari_th  =reffts%c_ar_th  , &
           c_xiquer  =reffts%c_xquer    &
           )
      ! .. graupel:
      CALL prefactors_reff_1mom_solidpoly (istore=igrau, pi=pi, &
           ageo_i=ageog, bgeo_i=bgeog, &
           n0_i=n0_g_const, nue_i=0.0_wp, mue_i=1.0_wp, rho_ice=rho_ice, &
           ldebug=ldebug_gsp, &
           c_reffi_so=refftg%c_reff_so, &
           c_reffi_th=refftg%c_reff_th, &
           c_reffi_ls=refftg%c_reff_ls, &
           c_rhoiquer=refftg%c_rhoquer, &
           c_ari_th  =refftg%c_ar_th  , &
           c_xiquer  =refftg%c_xquer    &
           )
    END IF

    IF (PRESENT(qc_for_reff)) THEN
      ! UB: using alternative qc_for_reff, a cloud water content
      !     different from the grid scale values, for the Reff calculation:
      CALL diag_reffc_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           hhl=hhl, rhol=rho, qc=qc_for_reff(:,:,:), cloud_num=qnc, qc_thresh=qc_thresh, &
           z0_ncn=zref_cloud_num_rad, z1oe_ncn=dz_oe_cloud_num_rad, &
           rhoc_nclow=rhoc_nclow_rad, rhoc_nchigh=rhoc_nchigh_rad, ncfact_low=ncfact_low_rad, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c, do_qnc_profile=do_qnc_profile )
    ELSE
      CALL diag_reffc_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           hhl=hhl, rhol=rho, qc=qc(:,:,:), cloud_num=qnc, qc_thresh=qc_thresh, &
           z0_ncn=zref_cloud_num_rad, z1oe_ncn=dz_oe_cloud_num_rad, &
           rhoc_nclow=rhoc_nclow_rad, rhoc_nchigh=rhoc_nchigh_rad, ncfact_low=ncfact_low_rad, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c, do_qnc_profile=do_qnc_profile )
    END IF
    ! .. Reff of cloud ice:
    CALL diag_reffi_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, rhol=rho, &
         t=t(:,:,:,ntlev), qi=qi(:,:,:), bgeo_i=bgeoi, qi_thresh=qi_thresh, &
         rhoi_nihigh=rhoi_nihigh_rad, rhoi_nilow=rhoi_nilow_rad, nifact_low=nifact_low_rad, &
         c_reffi_so=reffti%c_reff_so, c_reffi_th=reffti%c_reff_th, &
         c_reffi_ls=reffti%c_reff_ls, c_rhoibulk_ls=reffti%c_rhoquer, &
         c_ari_th=reffti%c_ar_th, &
         r_effi_so=reffti%r_eff_so, r_effi_th=reffti%r_eff_th, &
         r_effi_ls=reffti%r_eff_ls, rhoibulk_ls=reffti%rhobulk_ls, &
         r_effi_ini=reff_ini_i, rhoibulk_ls_ini=rhobulk_ls_ini_i , &
         ar_i_th=reffti%ar_mean_th)
    IF (lrad_incl_qrqsqg) THEN
      ! .. Reff for rain
      CALL diag_reffr_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           rhol=rho, qr=qr(:,:,:), qr_thresh=0.0_wp, &
           c_reffr=refftr%c_reff, r_effr=refftr%r_eff, &
           r_effr_ini=100.0_wp)
      ! .. Reff of snow:
      CALL diag_reffs_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, rhol=rho, &
           qi=qs(:,:,:), bgeo_i=bgeos, qi_thresh=0.0_wp, &
           c_reffi_so=reffts%c_reff_so, c_reffi_th=reffts%c_reff_th, &
           c_reffi_ls=reffts%c_reff_ls, c_rhoibulk_ls=reffts%c_rhoquer, &
           c_xiquer=reffts%c_xquer, c_ari_th=reffts%c_ar_th, &
           r_effi_so=reffts%r_eff_so, r_effi_th=reffts%r_eff_th, &
           r_effi_ls=reffts%r_eff_ls, rhoibulk_ls=reffts%rhobulk_ls, &
           ar_i_th=reffts%ar_mean_th, &
           n0_i=zn0_s, &
           r_effi_ini=100.0e-6_wp, rhoibulk_ls_ini=50.0_wp)
      ! .. Reff of graupel:
      CALL diag_reffs_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, rhol=rho, &
           qi=qg(:,:,:), bgeo_i=bgeog, qi_thresh=0.0_wp, &
           c_reffi_so=refftg%c_reff_so, c_reffi_th=refftg%c_reff_th, &
           c_reffi_ls=refftg%c_reff_ls, c_rhoibulk_ls=refftg%c_rhoquer, &
           c_xiquer=refftg%c_xquer, c_ari_th=refftg%c_ar_th, &
           r_effi_so=refftg%r_eff_so, r_effi_th=refftg%r_eff_th, &
           r_effi_ls=refftg%r_eff_ls, rhoibulk_ls=refftg%rhobulk_ls, &
           ar_i_th=refftg%ar_mean_th, &
           r_effi_ini=100.0e-6_wp, rhoibulk_ls_ini=200.0_wp &
           )
    END IF


!!$    PRINT *, 'ULI0 # ice iradpar_cloud=3, created by print statements in src_gscp.f90, for ageoi=',ageoi,' bgeoi=',bgeoi
!!$    PRINT *, 'ULI0 # ', 'qi, t, xquer_i, reffti%r_eff_ls, reffti%r_eff_so(1:2), reffti%r_eff_th(1:2), reffti%rhobulk_ls'
!!$    DO k=1, ke
!!$      DO j=1, je
!!$        DO i=1,ie
!!$          zxi = (reffti%r_eff_ls(i,j,k)/reffti%c_reff_ls(1))**(1.0/reffti%c_reff_ls(2))
!!$          PRINT *, 'ULI0 ', qi(i,j,k), t(i,j,k,ntlev), zxi, reffti%r_eff_ls(i,j,k), reffti%r_eff_so(i,j,k,:), reffti%r_eff_th(i,j,k,:), reffti%rhobulk_ls(i,j,k)
!!$        END DO
!!$      END DO
!!$    END DO
!!$
!!$    PRINT *, 'ULI1 # snow iradpar_cloud=3, created by print statements in src_gscp.f90, for ageos=',ageos,' bgeos=',bgeos
!!$    PRINT *, 'ULI1 # ', 'qs, t, zn0_s, xquer_s, reffts%r_eff_ls, reffts%r_eff_so(1:2), reffts%r_eff_th(1:2), reffts%rhobulk_ls'
!!$    DO k=1, ke
!!$      DO j=1, je
!!$        DO i=1,ie
!!$          zxi = (reffts%r_eff_ls(i,j,k)/reffts%c_reff_ls(1))**(1.0/reffts%c_reff_ls(2))
!!$          PRINT *, 'ULI1 ', qs(i,j,k), t(i,j,k,ntlev), zn0_s(i,j,k), zxi, reffts%r_eff_ls(i,j,k), reffts%r_eff_so(i,j,k,:), reffts%r_eff_th(i,j,k,:), reffts%rhobulk_ls(i,j,k)
!!$        END DO
!!$      END DO
!!$    END DO
!!$
!!$    PRINT *, 'ULI2 # graupel iradpar_cloud=3, created by print statements in src_gscp.f90, for ageog=',ageog,' bgeog=',bgeog,' n0_g_const=',n0_g_const
!!$    PRINT *, 'ULI2 # ', 'qg, xquer_g, refftg%r_eff_ls, refftg%r_eff_so(1:2), refftg%r_eff_th(1:2), refftg%rhobulk_ls'
!!$    DO k=1, ke
!!$      DO j=1, je
!!$        DO i=1,ie
!!$          zxi = (refftg%r_eff_ls(i,j,k)/refftg%c_reff_ls(1))**(1.0/refftg%c_reff_ls(2))
!!$          PRINT *, 'ULI2 ', qg(i,j,k), zxi, refftg%r_eff_ls(i,j,k), refftg%r_eff_so(i,j,k,:), refftg%r_eff_th(i,j,k,:), refftg%rhobulk_ls(i,j,k)
!!$        END DO
!!$      END DO
!!$    END DO


  CASE (3) 

    CALL prefactors_reff_1mom_cloud (pi=pi, nue_c=5.0_wp, mue_c=1.0_wp, &
         rho_w=rho_w, ldebug=ldebug_gsp, &
         c_reffc=refftc%c_reff)
    ! .. cloud ice:
    CALL prefactors_reff_1mom_icemono (pi=pi, &
         ageo_i=ageoi, bgeo_i=bgeoi, rho_ice=rho_ice, &
         ldebug=ldebug_gsp, &
         c_reffi_so=reffti%c_reff_so, &
         c_reffi_th=reffti%c_reff_th, &
         c_reffi_ls=reffti%c_reff_ls, &
         c_rhoiquer=reffti%c_rhoquer, &
         c_ari_th  =reffti%c_ar_th    &
         )
    IF (lrad_incl_qrqsqg) THEN
      ! .. rain:
      CALL prefactors_reff_1mom_rain (pi=pi, n0_r=n0_r_const, &
           nue_r=mu_rain, mue_r=1.0_wp, &
           rho_w=rho_w, ldebug=ldebug_gsp, &
           c_reffr=refftr%c_reff)
      ! .. snow: the n0_i=-999.99 is a neutral dummy value. n0_i(T) is later inputted into diag_reffs_1mom()!
      CALL prefactors_reff_1mom_solidpoly (istore=isnow, pi=pi, &
           ageo_i=ageos, bgeo_i=bgeos, &
           n0_i=-999.99_wp, nue_i=0.0_wp, mue_i=1.0_wp, rho_ice=rho_ice, &
           ldebug=ldebug_gsp, &
           c_reffi_so=reffts%c_reff_so, &
           c_reffi_th=reffts%c_reff_th, &
           c_reffi_ls=reffts%c_reff_ls, &
           c_rhoiquer=reffts%c_rhoquer, &
           c_ari_th  =reffts%c_ar_th  , &
           c_xiquer  =reffts%c_xquer    &
           )
    END IF

    IF (PRESENT(qc_for_reff)) THEN
      ! UB: using alternative qc_for_reff, a cloud water content
      !     different from the grid scale values, for the Reff calculation:
      CALL diag_reffc_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           hhl=hhl, rhol=rho, qc=qc_for_reff(:,:,:), cloud_num=qnc, qc_thresh=qc_thresh, &
           z0_ncn=zref_cloud_num_rad, z1oe_ncn=dz_oe_cloud_num_rad, &
           rhoc_nclow=rhoc_nclow_rad, rhoc_nchigh=rhoc_nchigh_rad, ncfact_low=ncfact_low_rad, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c, do_qnc_profile=do_qnc_profile )
    ELSE
      CALL diag_reffc_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           hhl=hhl, rhol=rho, qc=qc(:,:,:), cloud_num=qnc, qc_thresh=qc_thresh, &
           z0_ncn=zref_cloud_num_rad, z1oe_ncn=dz_oe_cloud_num_rad, &
           rhoc_nclow=rhoc_nclow_rad, rhoc_nchigh=rhoc_nchigh_rad, ncfact_low=ncfact_low_rad, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c, do_qnc_profile=do_qnc_profile )
    END IF
    ! .. Reff of cloud ice:
    CALL diag_reffi_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, rhol=rho, &
         t=t(:,:,:,ntlev), qi=qi(:,:,:), bgeo_i=bgeoi, qi_thresh=qi_thresh, &
         rhoi_nihigh=rhoi_nihigh_rad, rhoi_nilow=rhoi_nilow_rad, nifact_low=nifact_low_rad, &
         c_reffi_so=reffti%c_reff_so, c_reffi_th=reffti%c_reff_th, &
         c_reffi_ls=reffti%c_reff_ls, c_rhoibulk_ls=reffti%c_rhoquer, &
         c_ari_th=refftI%c_ar_th, &
         r_effi_so=reffti%r_eff_so, r_effi_th=reffti%r_eff_th, &
         r_effi_ls=reffti%r_eff_ls, rhoibulk_ls=reffti%rhobulk_ls, &
         ar_i_th=reffti%ar_mean_th, &
         r_effi_ini=reff_ini_i, rhoibulk_ls_ini=rhobulk_ls_ini_i)
    IF (lrad_incl_qrqsqg) THEN
      ! .. Reff for rain:
      CALL diag_reffr_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           rhol=rho, qr=qr(:,:,:), qr_thresh=0.0_wp, &
           c_reffr=refftr%c_reff, r_effr=refftr%r_eff, &
           r_effr_ini=100.0_wp)
      ! .. Reff of snow:
      CALL diag_reffs_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, rhol=rho, &
           qi=qs(:,:,:), bgeo_i=bgeos, qi_thresh=0.0_wp, &
           c_reffi_so=reffts%c_reff_so, c_reffi_th=reffts%c_reff_th, &
           c_reffi_ls=reffts%c_reff_ls, c_rhoibulk_ls=reffts%c_rhoquer, &
           c_xiquer=reffts%c_xquer, c_ari_th=reffts%c_ar_th, &
           r_effi_so=reffts%r_eff_so, r_effi_th=reffts%r_eff_th, &
           r_effi_ls=reffts%r_eff_ls, rhoibulk_ls=reffts%rhobulk_ls, &
           ar_i_th=reffts%ar_mean_th, &
           n0_i=zn0_s, &
           r_effi_ini=100.0e-6_wp, rhoibulk_ls_ini=50.0_wp &
           )
    END IF

  CASE (2)

    CALL prefactors_reff_1mom_cloud (pi=pi, nue_c=5.0_wp, mue_c=1.0_wp, &
         rho_w=rho_w, ldebug=ldebug_gsp, &
         c_reffc=refftc%c_reff)
    IF (lrad_incl_qrqsqg) THEN
      ! .. rain:
      CALL prefactors_reff_1mom_rain (pi=pi, n0_r=8e6_wp, &
           nue_r=0.0_wp, mue_r=1.0_wp, &
           rho_w=rho_w, ldebug=ldebug_gsp, &
           c_reffr=refftr%c_reff)
      ! .. snow:
      CALL prefactors_reff_1mom_solidpoly (istore=isnow, pi=pi, &
           ageo_i=ageos, bgeo_i=bgeos, &
           n0_i=n0_s_const, nue_i=0.0_wp, mue_i=1.0_wp, rho_ice=rho_ice, &
           ldebug=ldebug_gsp, &
           c_reffi_so=reffts%c_reff_so, &
           c_reffi_th=reffts%c_reff_th, &
           c_reffi_ls=reffts%c_reff_ls, &
           c_rhoiquer=reffts%c_rhoquer, &
           c_ari_th  =reffts%c_ar_th  , &
           c_xiquer  =reffts%c_xquer    &
           )
    END IF

    IF (PRESENT(qc_for_reff)) THEN
      ! UB: using alternative qc_for_reff, a cloud water content
      !     different from the grid scale values, for the Reff calculation:
      CALL diag_reffc_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           hhl=hhl, rhol=rho, qc=qc_for_reff(:,:,:), cloud_num=qnc, qc_thresh=qc_thresh, &
           z0_ncn=zref_cloud_num_rad, z1oe_ncn=dz_oe_cloud_num_rad, &
           rhoc_nclow=rhoc_nclow_rad, rhoc_nchigh=rhoc_nchigh_rad, ncfact_low=ncfact_low_rad, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c, do_qnc_profile=do_qnc_profile )
    ELSE
      CALL diag_reffc_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           hhl=hhl, rhol=rho, qc=qc(:,:,:), cloud_num=qnc, qc_thresh=qc_thresh, &
           z0_ncn=zref_cloud_num_rad, z1oe_ncn=dz_oe_cloud_num_rad, &
           rhoc_nclow=rhoc_nclow_rad, rhoc_nchigh=rhoc_nchigh_rad, ncfact_low=ncfact_low_rad, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c, do_qnc_profile=do_qnc_profile )
    END IF
    IF (lrad_incl_qrqsqg) THEN
      ! .. Reff of rain:
      CALL diag_reffr_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           rhol=rho, qr=qr(:,:,:), qr_thresh=0.0_wp, &
           c_reffr=refftr%c_reff, r_effr=refftr%r_eff, &
           r_effr_ini=100.0_wp)
      ! .. Reff of snow:
      CALL diag_reffs_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, rhol=rho, &
           qi=qs(:,:,:), bgeo_i=bgeos, qi_thresh=0.0_wp, &
           c_reffi_so=reffts%c_reff_so, c_reffi_th=reffts%c_reff_th, &
           c_reffi_ls=reffts%c_reff_ls, c_rhoibulk_ls=reffts%c_rhoquer, &
           c_xiquer=reffts%c_xquer, c_ari_th=reffts%c_ar_th, &
           r_effi_so=reffts%r_eff_so, r_effi_th=reffts%r_eff_th, &
           r_effi_ls=reffts%r_eff_ls, rhoibulk_ls=reffts%rhobulk_ls, &
           ar_i_th=reffts%ar_mean_th, &
           n0_i=zn0_s, &
           r_effi_ini=100.0e-6_wp, rhoibulk_ls_ini=50.0_wp &
           )
    END IF

  CASE (1)

    CALL prefactors_reff_1mom_cloud (pi=pi, nue_c=5.0_wp, mue_c=1.0_wp, &
         rho_w=rho_w, ldebug=ldebug_gsp, &
         c_reffc=refftc%c_reff)
    IF (lrad_incl_qrqsqg) THEN
      CALL prefactors_reff_1mom_rain (pi=pi, n0_r=8e6_wp, &
           nue_r=0.0_wp, mue_r=1.0_wp, &
           rho_w=rho_w, ldebug=ldebug_gsp, &
           c_reffr=refftr%c_reff)
    END IF

    IF (PRESENT(qc_for_reff)) THEN
      ! UB: using alternative qc_for_reff, a cloud water content
      !     different from the grid scale values, for the Reff calculation:
      CALL diag_reffc_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           hhl=hhl, rhol=rho, qc=qc_for_reff(:,:,:), cloud_num=qnc, qc_thresh=qc_thresh, &
           z0_ncn=zref_cloud_num_rad, z1oe_ncn=dz_oe_cloud_num_rad, &
           rhoc_nclow=rhoc_nclow_rad, rhoc_nchigh=rhoc_nchigh_rad, ncfact_low=ncfact_low_rad, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c, do_qnc_profile=do_qnc_profile )
    ELSE
      CALL diag_reffc_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           hhl=hhl, rhol=rho, qc=qc(:,:,:), cloud_num=qnc, qc_thresh=qc_thresh, &
           z0_ncn=zref_cloud_num_rad, z1oe_ncn=dz_oe_cloud_num_rad, &
           rhoc_nclow=rhoc_nclow_rad, rhoc_nchigh=rhoc_nchigh_rad, ncfact_low=ncfact_low_rad, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c, do_qnc_profile=do_qnc_profile )
    END IF
    IF (lrad_incl_qrqsqg) THEN
      CALL diag_reffr_1mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           rhol=rho, qr=qr(:,:,:), qr_thresh=0.0_wp, &
           c_reffr=refftr%c_reff, r_effr=refftr%r_eff, &
           r_effr_ini=100.0_wp)
    END IF

  END SELECT

  ! .. clean up:
  IF (ALLOCATED(zn0_s)) THEN
    DEALLOCATE(zn0_s)
  END IF
  IF (.NOT.(itype_aerosol == 2 .AND. icloud_num_type_rad == 2)) THEN
    DEALLOCATE (qnc)
    NULLIFY(qnc)
  END IF

END SUBROUTINE calc_reff_rad

#ifdef TWOMOM_SB
SUBROUTINE calc_reff_rad_2mom (ntlev, qc_thresh, qi_thresh, qc_for_reff)

  USE wolken_konstanten, ONLY: cloud, rain, ice, snow, graupel, hail

  IMPLICIT NONE

  INTEGER(kind=iintegers), INTENT(in) :: ntlev
  REAL(kind=wp), INTENT(in)           :: qc_thresh, qi_thresh
  REAL(kind=wp), INTENT(in), OPTIONAL :: qc_for_reff(:,:,:)

  ! .. integer flags denoting certain hydrometeor types in the
  !    calls to prefactors_reff_1mom_solidpoly() and prefactors_reff_2mom_solid():
  INTEGER(kind=iintegers), PARAMETER :: iice  = 1
  INTEGER(kind=iintegers), PARAMETER :: isnow = 2
  INTEGER(kind=iintegers), PARAMETER :: igrau = 3
  INTEGER(kind=iintegers), PARAMETER :: ihail = 4

  INTEGER (KIND=iintegers) :: izerror
  CHARACTER (LEN=255)      :: yzerrmsg, yzroutine

! Tracer pointers
!----------------
  REAL (KIND=wp),     POINTER :: &
    qc      (:,:,:)=> NULL(),   & ! QC at ntlev
    qr      (:,:,:)=> NULL(),   & ! QR at ntlev
    qi      (:,:,:)=> NULL(),   & ! QI at ntlev
    qs      (:,:,:)=> NULL(),   & ! QS at ntlev
    qg      (:,:,:)=> NULL(),   & ! QG at ntlev
    qh      (:,:,:)=> NULL(),   & ! QH at ntlev
    qnc     (:,:,:)=> NULL(),   & ! NCCLOUD at ntlev
    qnr     (:,:,:)=> NULL(),   & ! NCRAIN at ntlev
    qni     (:,:,:)=> NULL(),   & ! NCICE at ntlev
    qns     (:,:,:)=> NULL(),   & ! NCSNOW at ntlev
    qng     (:,:,:)=> NULL(),   & ! NCGRAUPEL at ntlev
    qnh     (:,:,:)=> NULL()      ! NCHAIL at ntlev


  yzroutine(:) = ' '
  yzroutine = 'calc_reff_rad_2mom'

  IF (.NOT. lgsp) THEN
    WRITE (*,*) 'INFO from '//TRIM(yzroutine)//'(): lgsp = .FALSE., so no effective radii for hydrometeors are computed!'
    RETURN
  END IF

  izerror = 0
  yzerrmsg(:) = ' '

  ! retrieve the required microphysics tracers at timelevel ntlev
  CALL trcr_get(izerror, idt_qc, ptr_tlev = ntlev, ptr = qc)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qr, ptr_tlev = ntlev, ptr = qr)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qi, ptr_tlev = ntlev, ptr = qi)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qs, ptr_tlev = ntlev, ptr = qs)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qg, ptr_tlev = ntlev, ptr = qg)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qh, ptr_tlev = ntlev, ptr = qh)
  IF (izerror /= 0_iintegers .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qnc, ptr_tlev = ntlev, ptr = qnc)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qnr, ptr_tlev = ntlev, ptr = qnr)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qni, ptr_tlev = ntlev, ptr = qni)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qns, ptr_tlev = ntlev, ptr = qns)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qng, ptr_tlev = ntlev, ptr = qng)
  IF (izerror /= 0_iintegers) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qnh, ptr_tlev = ntlev, ptr = qnh)
  IF (izerror /= 0_iintegers .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF


  IF (itype_gscp >= 100) THEN

    CALL prefactors_reff_2mom_cloud( ldebug=ldebug_gsp, c_reffc=refftc%c_reff)
    CALL prefactors_reff_2mom_rain ( ldebug=ldebug_gsp, c_reffr=refftr%c_reff)
    ! .. cloud ice:
    CALL prefactors_reff_2mom_solid( istore=iice, ldebug=ldebug_gsp, pi=pi, parti=ice, &
                                     c_reffi_so=reffti%c_reff_so, &
                                     c_reffi_th=reffti%c_reff_th, &
                                     c_reffi_ls=reffti%c_reff_ls, &
                                     c_rhoiquer=reffti%c_rhoquer, &
                                     c_ari_th  =reffti%c_ar_th    &
                                     )
    IF (lrad_incl_qrqsqg) THEN
      ! .. snow:
      CALL prefactors_reff_2mom_solid( istore=isnow, ldebug=ldebug_gsp, pi=pi, parti=snow, &
                                       c_reffi_so=reffts%c_reff_so, &
                                       c_reffi_th=reffts%c_reff_th, &
                                       c_reffi_ls=reffts%c_reff_ls, &
                                       c_rhoiquer=reffts%c_rhoquer, &
                                       c_ari_th  =reffts%c_ar_th    &
                                       )
      ! .. graupel:
      CALL prefactors_reff_2mom_solid( istore=igrau, ldebug=ldebug_gsp, pi=pi, parti=graupel, &
                                       c_reffi_so=refftg%c_reff_so, &
                                       c_reffi_th=refftg%c_reff_th, &
                                       c_reffi_ls=refftg%c_reff_ls, &
                                       c_rhoiquer=refftg%c_rhoquer, &
                                       c_ari_th  =refftg%c_ar_th    &
                                       )
    ! .. hail: (not yet fully implemented)
!!$    CALL prefactors_reff_2mom_solid( istore=ihail, ldebug=ldebug_gsp, pi=pi, parti=hail, &
!!$                                     c_reffi_so=reffth%c_reff_so, &
!!$                                     c_reffi_th=reffth%c_reff_th, &
!!$                                     c_reffi_ls=reffth%c_reff_ls, &
!!$                                     c_rhoiquer=reffth%c_rhoquer, &
!!$                                     c_ari_th  =reffth%c_ar_th    &
!!$                                     )
    END IF

    IF (PRESENT(qc_for_reff)) THEN
      ! UB: using alternative qc_for_reff, a cloud water content
      !     different from the grid scale values, for the Reff calculation:
      CALL diag_reffc_2mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           qc=qc_for_reff(:,:,:), qnc=qnc(:,:,:), qc_thresh=qc_thresh, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c &
           )
    ELSE
      CALL diag_reffc_2mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           qc=qc(:,:,:), qnc=qnc(:,:,:), qc_thresh=qc_thresh, &
           c_reffc=refftc%c_reff, r_effc=refftc%r_eff, &
           r_effc_ini=reff_ini_c &
           )
    END IF
    CALL diag_reffr_2mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
         qr=qr(:,:,:), qnr=qnr(:,:,:), qr_thresh=0.0_wp, &
         c_reffr=refftr%c_reff, r_effr=refftr%r_eff, &
         r_effr_ini=100.0E-6_wp &
         )
    ! .. Reff for cloud ice:
    CALL diag_reffs_2mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
         parti=ice, qi=qi(:,:,:), qni=qni(:,:,:), qi_thresh=qi_thresh, &
         c_reffi_so=reffti%c_reff_so, c_reffi_th=reffti%c_reff_th, &
         c_reffi_ls=reffti%c_reff_ls, c_rhoibulk_ls=reffti%c_rhoquer, &
         c_ari_th=reffti%c_ar_th, &
         r_effi_so=reffti%r_eff_so, r_effi_th=reffti%r_eff_th, &
         r_effi_ls=reffti%r_eff_ls, rhoibulk_ls=reffti%rhobulk_ls, &
         ar_i_th=reffti%ar_mean_th, &
         r_effi_ini=reff_ini_i &
         )
    IF (lrad_incl_qrqsqg) THEN
      ! .. Reff for snow:
      CALL diag_reffs_2mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           parti=snow, qi=qs(:,:,:), qni=qns(:,:,:), qi_thresh=0.0_wp, &
           c_reffi_so=reffts%c_reff_so, c_reffi_th=reffts%c_reff_th, &
           c_reffi_ls=reffts%c_reff_ls, c_rhoibulk_ls=reffts%c_rhoquer, &
           c_ari_th=reffts%c_ar_th, &
           r_effi_so=reffts%r_eff_so, r_effi_th=reffts%r_eff_th, &
           r_effi_ls=reffts%r_eff_ls, rhoibulk_ls=reffts%rhobulk_ls, &
           ar_i_th=reffts%ar_mean_th, &
           r_effi_ini=50.0E-6_wp &
           )
      ! .. Reff for graupel:
      CALL diag_reffs_2mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
           parti=graupel, qi=qg(:,:,:), qni=qng(:,:,:), qi_thresh=0.0_wp, &
           c_reffi_so=refftg%c_reff_so, c_reffi_th=refftg%c_reff_th, &
           c_reffi_ls=refftg%c_reff_ls, c_rhoibulk_ls=refftg%c_rhoquer, &
           c_ari_th=refftg%c_ar_th, &
           r_effi_so=refftg%r_eff_so, r_effi_th=refftg%r_eff_th, &
           r_effi_ls=refftg%r_eff_ls, rhoibulk_ls=refftg%rhobulk_ls, &
           ar_i_th=refftg%ar_mean_th, &
           r_effi_ini=100.0E-6_wp &
           )
    ! .. Reff for hail: (not yet fully implemented)
!!$    CALL diag_reffs_2mom(its=1, ite=ie, jts=1, jte=je, kts=1, kte=ke, &
!!$         parti=hail, qi=qh(:,:,:), qni=qnh(:,:,:), qi_thresh=0.0_wp, &
!!$         c_reffi_so=reffth%c_reff_so, c_reffi_th=reffth%c_reff_th, &
!!$         c_reffi_ls=reffth%c_reff_ls, c_rhoibulk_ls=reffth%c_rhoquer, &
!!$         c_ari_th=reffth%c_ar_th, &
!!$         r_effi_so=reffth%r_eff_so, r_effi_th=reffth%r_eff_th, &
!!$         r_effi_ls=reffth%r_eff_ls, rhoibulk_ls=reffth%rhobulk_ls, &
!!$         ar_i_th=reffth%ar_mean_th, &
!!$         r_effi_ini=250.0E-6_wp &
!!$         )
    END IF

  END IF
  
END SUBROUTINE calc_reff_rad_2mom
#endif 

!==============================================================================
!==============================================================================

END MODULE src_reff_calc
