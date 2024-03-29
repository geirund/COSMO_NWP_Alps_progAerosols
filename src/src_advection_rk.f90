!+ Source module for advection routines for Runge-Kutta scheme
!------------------------------------------------------------------------------

MODULE src_advection_rk

!------------------------------------------------------------------------------
!
! Description:
!   This module contains subroutines which compute the advection for
!   the Runge-Kutta version.
!
!   These routines have been in module src_runge_kutta.f90 before, which 
!   has been splitted.
!
! Current Code Owner: DWD, Jochen Foerstner    
!  phone:  +49  69  678667 35
!  fax:    +49  69  8062 3721
!  email:  jochen.foerstner@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 3.21       2006/12/04 Ulrich Schaettler
!  Initial release
! V4_1         2007/12/04 Ulrich Schaettler, Michael Baldauf
!  Changed interface to SR wcfrac_crint_rk because of reproducibility problems
!  Alternative calls to SR multiplicative_filling_DDI
! V4_4         2008/07/16 Ulrich Schaettler, Lucio Torrisi, Michael Baldauf
!  Adapted interface of get_timings
!  Consideration of the sign of dt for DFI (Torrisi)
!  New SR adv_upwind1_lon, ..., adv_centdiff6_lat for higher efficiency (Baldauf)
! V4_5         2008/09/10 Michael Baldauf
!  Optimized horizontal advection routines used also for wcon in advection_pd
! V4_7         2008/12/12 Ulrich Schaettler
!  Technical adaptations in horiz_adv_driver
! V4_8         2009/02/16 Ulrich Schaettler
!  Initialization of error-variables in SR horiz_adv_driver
! V4_9         2009/07/16 Ulrich Schaettler, Christian Bollmann
!  Use 3D versions of the advection operators for the NEC-SX 
!    (implemented with ifdef: NECSX
!  Inserted compiler directives
! V4_11        2009/11/30 Guenther Zaengl
!  Implemented a limiter for temperature advection (itheta_adv=2 only)
!  Vectorized explicit vertical advection 3rd order
! V4_12        2010/05/11 Oli Fuhrer
!  Eliminated option itype_hdiff=3
! V4_13        2010/05/11 Michael Gertz
!  Adaptions to SVN
! V4_15        2010/11/19 Michael Baldauf
!  Replace NAMELIST-Var. lva_impl_dyn  by  y_vert_adv_dyn
! V4_17        2011/02/24 Ulrich Blahak
!  Adapted interface of exchg_boundaries; corrected kzdims(1:20) -> kzdims(1:24);
!  eliminated my_peri_neigh; fixed SR limit_contravar_vert_veloc (deleted unnecessary loops,
!  correctly implemented 1-proc run).
! V4_18        2011/05/26 Michael Baldauf, Guy deMorsier
!  Strang-splitting for Bott-Advection (Guy de Morsier)
!   Added SR advection_ef_zyxyz and exchange_runge_kutta_3dstrang for l3dstrang
!  Optional 'selective filling diffusion' for the Semi-Lagrangian advection:
!    --> new subr. advection_semi_lagrange_init,
!        new version of subr. advection_semi_lagrange  (Michael Baldauf)
!  Added parts for COSMOART and POLLEN (Christoph Knote)
! V4_19        2011/08/01 Ulrich Blahak
!  Adapted horizontal advection of T and pp to handle flow situations
!   with confluent horizontal flows in neighbouring grid boxes
!   (opposite signs of U and/or V) better. This avoids a spurious
!   and potentially detrimental heat source in such grid boxes, which
!   lead to extreme Theta-values, to "grid point storms" and other
!   unwanted effects. New subroutines "horiz_adv_driver_stagmix",
!   "adv_upwind?_???_stagmix", new internal switch "iztype_tppadv";
! V4_20        2011/08/31 Ulrich Schaettler
!  tgrlat needs 2 dimensions for v-point dependence
!  Changed tag for boundary exchange for strang splitting, which uses
!  MPI data types
! V4_21        2011/12/06 Oliver Fuhrer
!  Bug fix in call to exchg_boundaries in Pollen case: num_compute was forgotten
! V4_23        2012/05/10 Michael Baldauf, Oliver Fuhrer, Ulrich Schaettler
!  Reorganization of the Bott-Advection routines (to avoid multiple versions 
!   of the same !   Code)
!  Allow new variants of the Bott-Advection schemes:
!    BOTT2_STRANG_B, BOTT4_STRANG_B: Strang Splitting only at the bottom (5 levels)
!    BOTT2_XYZYX, BOTT4_XYZYX: modified sequence 'xyzyx' compared to the current 
!      Strang splitting, which uses 'zyxyz'
!  Implemented interfaces for COSMOART and POLLEN for Semi-Lagrange Advection
!  Shifted SR calc_wcon_sqrtg from src_runge_kutta to here
!  Modified interfaces to advection operators and preparations for calling these
!    operators according to changes in numeric_utilities_rk (Oliver Fuhrer)
!  Removed switches lprogprec, ltrans_prec  (Uli Schaettler)
!  Bug fix for call to SR global_values in SR limit_contravar_vert_veloc (line 1454):
!           needs imp_integers not imp_reals
! V4_24        2012/06/22 Michael Baldauf
!  In all upwind or centered differences advection routines, the transporting field
!     can have a different number of layers
! V4_25        2012/09/28 Anne Roches, Oliver Fuhrer, Hans-Juergen Panitz,
!                         Michael Baldauf
!  Replaced qx-variables by using them from the tracer module
!  Introduced nexch_tag for MPI boundary exchange tag to replace ntstep (HJP)
!  Computation of wcon in SR advection_pd: use already advected w to compute wcon
!  New T- and p- advection scheme (iztype_tppadv=2) only works for
!   odd-order upwind advection schemes (iadv_order=1,3,5), so in
!   case of even adv order, switch back to the old scheme and issue a
!   warning instead of aborting the run (UB).
! V4_26        2012/12/06 Ulrich Schaettler, Anne Roches
!  Copy TKE time levels for advection_pd only if lprog_tke is set (US)
!  Renaming of T_CLP_POSDEF to T_CLP_ON since only on and off are available
!  for the moment. (AR)
! V4_27        2013/03/19 Michael Baldauf, Astrid Kerkweg, Ulrich Schaettler
!  Bug correction for the earth curvature term in the advection of v:
!    unnecessary averaging factor 1/2 eliminated (A. Will/J. Ogaja)
!  Introduced a limiter for density in the finite-volume advection schemes
!    (as the Bott-scheme), if density is also transported.
!  MESSy interface introduced (AK)
! V4_28        2013/07/12 KIT, Damian Wojcik
!  Changes to adapt COSMO-ART to new tracer module: all dependencies to
!  COSMOART and POLLEN deleted, because this is now handled by the tracer module (KIT)
!  Bug fix: calls to trcr_get in SR advection_semi_lagrange were at a wrong location (DW)
! V4_29        2013/10/04 Astrid Kerkweg, Ulrich Schaettler, Ulrich Blahak
!  Unification of MESSy interfaces and COSMO Tracer structure
!  Insufficient loop index borders for calculating TKE transport velocities
!   in advection_ef_x and advection_ef_y corrected (UB)
! V4_30        2013-11-08 Ulrich Schaettler
!  Initialized izerror in SR horiz_adv_driver, implicit_sedim
!  Changed intent attribute of phi_new in SR implicit_sedim to INOUT, 
!    because otherwise it is used before it is defined!
! VX_XX        2014/03/24 Ulrich Blahak
!   Added advection of TKE with own tendency for itype_turb=3; added density correction
!    of TKE for conservative transport schemes analogue to QX
!   Added missing qh in case of 2-moment scheme
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

USE data_parameters , ONLY :   &
  wp,        & ! KIND-type parameters for real variables
  iintegers    ! KIND-type parameter for standard integer variables

!------------------------------------------------------------------------------

USE data_constants  , ONLY :   &

  ! 2. physical constants and related variables
  ! -------------------------------------------

  r_d,          & ! gas constant for dry air
  rvd_m_o,      & ! r_v/r_d - 1
  r_earth,      & ! mean radius of the earth
! <fuo_tadvlimit
  cp_d,         & ! specific heat capacity of dry air
! fuo_tadvlimit>

  ! 3. constants for parametrizations
  ! ---------------------------------

  aks4            ! variable for horizontal diffusion of fourth order

! end of data_constants

!------------------------------------------------------------------------------

USE data_fields     , ONLY :   &

  ! 1. constant fields for the reference atmosphere                 (unit)
  ! -----------------------------------------------
  p0         ,    & ! reference pressure at main levels             ( Pa  )
! <fuo_tadvlimit
  t0         ,    & ! reference temperature                         ( K   )
! fuo_tadvlimit>
  rho0       ,    & ! reference density  at main levels             ( kg/m3 )
  hhl        ,    & ! geometical height of half model levels        (  m  )

  ! 2. external parameter fields                                    (unit)
  ! ----------------------------
  crlat      ,    & ! cosine of transformed latitude
  acrlat     ,    & ! 1 / ( crlat * radius of the earth )           ( 1/m )
  tgrlat     ,    & ! TAN(phi)

  ! 3. prognostic variables                                         (unit)
  ! -----------------------
  u          ,    & ! zonal wind speed                              ( m/s )
  v          ,    & ! meridional wind speed                         ( m/s )
  w          ,    & ! vertical wind speed (defined on half levels)  ( m/s )
  t          ,    & ! temperature                                   (  k  )
  tke        ,    & ! turbulent kinetic energy (on half levels)     (m2/s2)
  pp         ,    & ! deviation from the reference pressure         ( pa  )

  ! 4. tendency fields for the prognostic variables                 (unit )
  ! -----------------------------------------------
  !    timely deviation  by diabatic and adiabatic processes
  !    without sound-wave terms
  wtens      ,    & ! w-tendency without sound-wave terms           ( m/s2)

  ! 6. fields that are computed in the parametrization and dynamics (unit )
  ! ---------------------------------------------------------------
  rho        ,    & ! density of moist air
  qrs        ,    & ! precipitation water (water loading)           (kg/kg)
  wcon       ,    & ! contravariant vertical velocity
  uadvt      ,    & ! advective tendency of u
  vadvt      ,    & ! advective tendency of v
  wadvt      ,    & ! advective tendency of w
  ppadvt     ,    & ! advective tendency of pp
  tket_adv   ,    & ! advective tke-tendency ([m/s2] for itype_turb == 3
  tadvt      ,    & ! advective tendency of t
  !WL2011b
  tt_hadv    ,    &
  tt_adv
  !WL2011e  
! end of data_fields

!------------------------------------------------------------------------------

USE data_modelconfig, ONLY :   &

  ! 2. horizontal and vertical sizes of the fields and related variables
  ! --------------------------------------------------------------------
  ie,           & ! number of grid points in zonal direction
  je,           & ! number of grid points in meridional direction
  ke,           & ! number of grid points in vertical direction
  ke1,          & ! ke+1

  ! 3. start- and end-indices for the computations in the horizontal layers
  ! -----------------------------------------------------------------------
  !    These variables give the start- and the end-indices of the 
  !    forecast for the prognostic variables in a horizontal layer.
  !    Note, that the indices for the wind-speeds u and v differ from 
  !    the other ones because of the use of the staggered Arakawa-B-grid.
  !    
  !   zonal direction
  istart,       & ! start index for the forecast of w, t, qv, qc and pp
  iend,         & ! end index for the forecast of w, t, qv, qc and pp
  istartu,      & ! start index for the forecast of u
  iendu,        & ! end index for the forecast of u
  istartv,      & ! start index for the forecast of v
  iendv,        & ! end index for the forecast of v

  !   meridional direction
  jstart,       & ! start index for the forecast of w, t, qv, qc and pp
  jend,         & ! end index for the forecast of w, t, qv, qc and pp
  jstartu,      & ! start index for the forecast of u
  jendu,        & ! start index for the forecast of u
  jstartv,      & ! start index for the forecast of v
  jendv,        & ! end index for the forecast of v
  jstartpar,    & ! start index for computations in the parallel program
  jendpar,      & ! end index for computations in the parallel program

  ! 4. constants for the horizontal rotated grid and related variables
  ! ------------------------------------------------------------------

  eddlon,       & ! 1 / dlon
  eddlat,       & ! 1 / dlat

  ! 5. variables for the time discretization and related variables
  ! --------------------------------------------------------------

  dt,           & ! long time-step
  lalloc_tke,   & !

! 8. Organizational variables to handle the COSMO humidity tracers
! ----------------------------------------------------------------
  idt_qv, idt_qc, idt_qi, idt_qs, idt_qr, idt_qg, idt_qh

! end of data_modelconfig

!------------------------------------------------------------------------------

USE data_parallel,      ONLY :  &
  num_compute,   & ! number of compute PEs
  nboundlines,   & ! number of boundary lines of the domain for which
                   ! no forecast is computed = overlapping boundary
                   ! lines of the subdomains
  ldatatypes,    & ! if .TRUE.: use MPI-Datatypes for some communications
  ltime_barrier, & ! if .TRUE.: use additional barriers for determining the
                   ! load-imbalance
  ncomm_type,    & ! type of communication
  my_cart_id,    & ! rank of this subdomain in the cartesian communicator
  my_cart_neigh, & ! neighbors of this subdomain in the cartesian grid
  icomm_cart,    & ! communicator for the virtual cartesian topology
                   ! that can be used by MPI_WAIT to identify the send
  imp_reals,     & ! determines the correct REAL type used in the model
                   ! for MPI
  imp_integers,  & ! determines the correct INTEGER type used in the model
                   ! for MPI
  nexch_tag,     & ! tag to be used for MPI boundary exchange
                   !  (in calls to exchg_boundaries)
  sendbuf,       & ! sending buffer for boundary exchange:
                   ! 1-4 are used for sending, 5-8 are used for receiving
  isendbuflen      ! length of one column of sendbuf

!------------------------------------------------------------------------------

USE data_runcontrol , ONLY :   &

  ! 1. start and end of the forecast
  ! --------------------------------
  ntstep,        & ! actual time step
  nnow,          & ! corresponds to ntstep
  nnew,          & ! corresponds to ntstep + 1
  ntke,          & ! TKE-timestep corresponds to ntstep

  ! 3. controlling the physics
  ! --------------------------
  itype_gscp,    & ! type of microphys. parametrization
  itype_turb,    & ! type of turbulent diffusion parametrization
  lprog_tke,     & ! prognostic treatment of TKE (for itype_turb=5/7)
  ldyn_bbc,      & ! dynamical bottom boundary condition
  ladv_deep,     & ! if =.TRUE.: use all metric advective terms
  l_cosmo_art,   & ! if .TRUE., run the COSMO_ART
  l_pollen,      & ! of pollen

  ! 7. additional control variables
  ! -------------------------------
  lperi_x,         & ! if lartif_data=.TRUE.: periodic boundary conditions in x-dir.
                     !               .FALSE.:   or with Davies conditions
  lperi_y,         & ! if lartif_data=.TRUE.: periodic boundary conditions in y-dir.
                     !               .FALSE.:   or with Davies conditions
  l2dim,           & ! 2 dimensional runs
  ltime,           & ! detailed timings of the program are given
  y_scalar_advect, & ! type of scalar advection scheme
                     ! "SL3_MF", "SL3_SFD", "Bott2", "Bott4"
                     ! "Bott2_Strang", "Bott4_Strang", "vanLeer", "PPM"
  y_vert_adv_dyn,  & ! switch to choose between several implicit and explicit
                     ! vertical advection schemes of dynamic variables
  lhordiff,        & ! running with horizontal diffusion
  nbl_exchg,       & ! number of boundlines to exchange
  irk_order,       & ! order of the Runge-Kutta time-integration scheme
  iadv_order,      & ! order of the horizontal advection scheme in dyn. core
  itheta_adv,      & ! =0: use T' (perturbation temperature) for advection
                     ! =1: use theta' (perturbation potential temperature)
                     ! =2: use theta (full potential temperature)
  ltadv_limiter,   & ! use limiter for temperature advection (itheta_adv=2 only)
  ieva_order,      & ! order of the explicit vertical adv. scheme in dyn. core
  intcr_max,       & ! max. allowed integer courant number in cr-indep. adv.
!WL2011b
  lbud,            &
!WL2011e
! 12. controlling verbosity of debug output
! -----------------------------------------
    idbg_level,   & ! to control the verbosity of debug output
    ldebug_dyn,   & ! if .TRUE., debug output for dynamics
    lprintdeb_all   ! .TRUE.:  all tasks print debug output
                    ! .FALSE.: only task 0 prints debug output

! end of data_runcontrol 

!------------------------------------------------------------------------------

USE data_tracer,              ONLY :  &
  T_ADV_ID    ,  T_ADV_ON    ,  T_CLP_ID  , T_CLP_ON  ,                       &
  T_LBC_ID    ,  T_LBC_ZERO  ,  T_LBC_ZEROGRAD,                               &
  T_ERR_NOTFOUND

!------------------------------------------------------------------------------

USE environment,              ONLY :  &
  exchg_boundaries,       & ! performs the boundary exchange between
                            ! neighboring processors
  comm_barrier,           & !
  model_abort

USE grid_metrics_utilities,   ONLY :  &
  sqrtg_r_s  ,    & ! reciprocal square root of G at skalar points  ( 1/m )
  sqrtg_r_u  ,    & ! reciprocal square root of G at u points       ( 1/m )
  sqrtg_r_v  ,    & ! reciprocal square root of G at v points       ( 1/m )
  sqrtg_r_w  ,    & ! reciprocal square root of G at w points       ( 1/m )
  wgtfac_u   ,    & ! 
  wgtfac_v   ,    & !
  wgtfac            !

USE time_utilities,           ONLY :  get_timings, i_horizontal_advection,  &
                                  i_barrier_waiting_dyn, i_communications_dyn

USE meteo_utilities,          ONLY :  &
  calrho,                 & !
  calrho_densities

USE numeric_utilities,        ONLY :  &
  backtraj_trilin_dt2_3tl,  & !
  postprocess_backtraj,     & !
  interpol_sl_trilin,       & !
  interpol_sl_tricubic,     & !
  remove_negative_values      !

USE numeric_utilities_rk,     ONLY :  &
  multiplicative_filling    , & !MB
  multiplicative_filling_DDI, & !MB
  clipping,                 & !
  init_bott_coeffs,         & !
  xadv_pd_rk_cri_bott,      & !
  yadv_pd_rk_cri_bott,      & !
  zadv_pd_rk_cri_bott,      & !
  xadv_rk_cri_ppm,          & !
  yadv_rk_cri_ppm,          & !
  zadv_rk_cri_ppm,          & !
  xadv_pd_rk_cri_vanleer,   & !
  yadv_pd_rk_cri_vanleer,   & !
  zadv_pd_rk_cri_vanleer,   & !
  xadv_pd_rk_bott,          & !
  yadv_pd_rk_bott,          & !
  zadv_pd_rk_bott,          & !
  xadv_rk_ppm,              & !
  yadv_rk_ppm,              & !
  zadv_rk_ppm,              & !
  xadv_pd_rk_vanleer,       & !
  yadv_pd_rk_vanleer,       & !
  zadv_pd_rk_vanleer,       & !
  ufrac_crint_rk,           & !
  vfrac_crint_rk,           & !
  wcfrac_crint_rk,          & !
  udsdx                       ! udsdx_* interface

USE parallel_utilities,       ONLY :  &
  i_global, j_global,     & !
  global_values             !

USE src_tracer,               ONLY :  &
  trcr_get,               & !
  trcr_calc,              & ! um_ak_20130522
  trcr_meta_get,          & !
  trcr_get_ntrcr,         & !
  trcr_errorstr

!==============================================================================

IMPLICIT NONE

!==============================================================================

!------------------------------------------------------------------------------
! Declarations
!------------------------------------------------------------------------------

LOGICAL                  ::  &
  lef_adv_trcr_notpd,   & ! .TRUE. if Euler forward adv. scheme for tracers
                          ! is NOT positive definite
  ltrcr_trilin,         & ! .TRUE. if trilin. interpol. is used for tracers
  ltrcr_conserv_form,   & ! .TRUE. if tracer transport in conservation form
  lcalrho_advprog         ! .TRUE. if rho is advected prognostically

CHARACTER(LEN=10) ::  &
  y_SL_diffus_type        ! Type of selective filling diffusion in
                          ! Semi-Lagrangian Adv. ("xyz", zyx", or "3D")

INTEGER :: i_clipping_type
    ! =0 no clipping
    ! =1 simple clipping of negative values (produces a lot of mass)
    ! =3 'selective filling diffusion' (better local conservation)
    ! =4 'multiplicative filling' (only nearly global conservation)

REAL (KIND=wp)           :: &
  tadv_thresh             ! threshold value for limiter of theta advection

INTEGER :: ngp_limit

!==============================================================================

CONTAINS

!==============================================================================
!==============================================================================
!+ Module procedure for computing iadv_order advection
!------------------------------------------------------------------------------

SUBROUTINE advection( nadv, irk, im, ip, imipmx, cfl_eva, j2dim, ny_2dim)

!------------------------------------------------------------------------------
!
! Description:
!   1.) Compute the time tendencies of u, v, w, pp, T due to
!       horizontal advection.
!   2.) Compute the contravariant vertical velocity wcon.
!   3.) if 'explicit vertical advection' is chosen: add it to the
!       horizontal tendencies, too
!   Only called within the 2-TL integration scheme
!
!   additionally to the formal parameters the following variables are modified:
!   - uadvt, vadvt, wadvt, tadvt, ppadvt
!   - wcon
!
! Method:
!   Horizontal advection is written in advection form and discretizised
!   with 1st-UP, 2nd-CD, ... 6th-CD differences on an Arakawa C-grid.
!   The scheme from Wicker (1997) is used.
!
!------------------------------------------------------------------------------

! Declarations:

! Subroutine arguments:
! ---------------------
INTEGER (KIND=iintegers), INTENT(IN) ::  &
  nadv, irk,           & !
  im, ip, imipmx,      & ! specify width of the stencil
  j2dim, ny_2dim         ! for 2dim runs

REAL (KIND=wp),           INTENT(IN) ::  &
  cfl_eva                ! CFL-value (for explicit vertical advection)

! Local scalars:
! -------------

INTEGER (KIND=iintegers) ::  &
  izstata,             & !  error status at allocation
  izstatd,             & !  error status at deallocation
  i,  j,  k,           & !  Loop indices in longitudinal, latitudinal and
  isp,                 & !  Loop indices for cosmo_art
  izerror,             & !
  iztype_tppadv          !  Switch to choose adv. scheme for t and pp

INTEGER (KIND=iintegers) ::  &
  kzdims(24)             !  vertical dimensions for exchg_boundaries

REAL    (KIND=wp   )     ::  &
  zui, zvi, zvn, zvs,  & !
  zwl, zwad,           & !
  zuvad, zsign           !

REAL    (KIND=wp   )     ::  &
! <fuo_tadvlimit
  r_earth_recip, rovcp, p00
! fuo_tadvlimit>

CHARACTER (LEN=80)       ::  &
  yzerrmsg

! Local (automatic) arrays:
! ------------

REAL    (KIND=wp   )     ::  &
  zfadsx   (je   ),    & !
  zfadsy   (je   ),    & !
  ztgrlatda(je, 2)

REAL    (KIND=wp),     ALLOCATABLE ::  &
  zw_ke(:,:,:),    & !
  zu   (:,:,:),    & !
  zv   (:,:,:)

! <fuo_tadvlimit
REAL    (KIND=wp),     POINTER     ::  &
  theta(:,:,:) => NULL()
! fuo_tadvlimit>


!- End of header
!==============================================================================

!------------------------------------------------------------------------------
! Begin Subroutine advection
!------------------------------------------------------------------------------

  izerror   = 0_iintegers
  kzdims(:) = 0_iintegers

  zsign = SIGN(1._wp,dt)

!------------------------------------------------------------------------------
! Section 1:  Calculation of advection tendencies
!------------------------------------------------------------------------------

  r_earth_recip = 1.0_wp / r_earth

  ALLOCATE( zu(1:ie, 1:je, 1:ke), zv(1:ie, 1:je, 1:ke), STAT=izstata)
  IF ( izstata /= 0 ) THEN
    yzerrmsg = 'allocation of zu, zv'
    CALL model_abort (my_cart_id, 100+izstata, yzerrmsg, 'advection')
  END IF

! <fuo_tadvlimit
  ! init theta field (this is only valid if itheta_adv = 2, theta is
  ! only used if ltadv_limiter = T, but we need to have a valid
  ! pointer because some compilers complain if a NULL pointer is
  ! passed as an argument)
  theta => t(:,:,:,nadv)

  IF (ltadv_limiter) THEN

    ! allocate memory if we actually need to compute the
    ! potential temperature field
    IF (itheta_adv /= 2) THEN
      NULLIFY( theta )
      ALLOCATE( theta(1:ie, 1:je, 1:ke), STAT=izstata)
      IF ( izstata /= 0 ) THEN
        yzerrmsg = 'allocation of theta'
        CALL model_abort (my_cart_id, 100+izstata, yzerrmsg, 'advection')
      END IF
    END IF

    ! compute potential temperature to use for limiter for temperature advection
    ! or in case potential temperature is prognostic variable, simply point to it
    IF (itheta_adv == 0) THEN
      rovcp = r_d/cp_d
      p00 = 1.0E-5_wp
      DO  k = 1, ke
        DO j = jstart-1, jend+1
          DO i = istart-1, iend+1
            theta(i,j,k) = ( t0(i,j,k) + t(i,j,k,nadv) )           &
                  * EXP(-rovcp*LOG( ( p0(i,j,k) + pp(i,j,k,nadv) ) * p00) )
          END DO
        END DO
      END DO
    ELSE IF (itheta_adv == 1) THEN
      DO  k = 1, ke
        DO j = jstart-1, jend+1
          DO i = istart-1, iend+1
            theta(i,j,k) = t0(i,j,k) + t(i,j,k,nadv)
          END DO
        END DO
      END DO
    ELSE IF (itheta_adv == 2) THEN
      theta => t(:,:,:,nadv)
    ENDIF

  END IF
! fuo_tadvlimit>

  ! advective tendencies for prognostic scalars pp and tp
  ! -----------------------------------------------------

  ppadvt(:,:,:) = 0.0_wp
  tadvt (:,:,:) = 0.0_wp

  ! calc.  u= d lambda/dt  and  v= d phi/dt and average to scalar points:

  ! Changed the advection-scheme of T/pp with respect to the
  ! calculation of the u/v- velocities in the gradient operator u * grad X:
  ! Before they were averaged to the mass points before multiplying
  ! with the gradient operator, which was detrimental in case of
  ! confluent/diffluent flow patterns with opposite sign of u/v
  ! at neighbouring u/v- points.
  ! Now, we calculate the advection operator separately for each
  ! neighbouring u/v- point and average the tendency afterwards.
  ! This cures the above problems with confluent/diffluent flow
  ! patterns, at the same time preserving results at all other
  ! flow configurations.

  ! The change is implemented as an alternative to the previous
  ! scheme by choosing the switch "iztype_tppadv = 2" instead of "1".

  iztype_tppadv = 2

  IF (iztype_tppadv == 2 .AND. MODULO(iadv_order,2) == 0) THEN
    ! New T- and p-advection scheme is not implemented for even order iadv_order,
    ! so reset to old method in that case and issue a warning:
    iztype_tppadv = 1
    WRITE (*,'(a/a,i2,a/a)') '  *** WARNING from advection(): new advection scheme for T and p', &
         '  ***     is not implemented for even advection order iadv_order = ', iadv_order, '!', &
         '  ***     Using old advection scheme (iztype_tppadv  = 1)!'
  END IF

  SELECT CASE (iztype_tppadv)

  CASE (1)

    ! previous scheme:
    DO  k = 1, ke
      DO j = jstart, jend
        DO i = istart, iend
          zu(i,j,k) = 0.5_wp * ( u(i,j,k,nadv) + u(i-1,j,k,nadv) )    &
               * acrlat(j,1)
          zv(i,j,k) = 0.5_wp * ( v(i,j,k,nadv) + v(i,j-1,k,nadv) )    &
               * r_earth_recip
        END DO
      END DO
    END DO

    CALL horiz_adv_driver( pp(:,:,:,nadv), zu, zv, ppadvt, zsign,         &
         &     istart, iend, jstart, jend, 1, ke, ke, ke, iadv_order )
! <fuo_tadvlimit
    CALL horiz_adv_driver(  t(:,:,:,nadv), zu, zv,  tadvt, zsign,         &
         &     istart, iend, jstart, jend, 1, ke, ke, ke, iadv_order,     &
               ltadv_limiter, theta )
! fuo_tadvlimit>

  CASE (2)

    ! new scheme:
    DO  k = 1, ke
      DO j = jstart-1, jend
        DO i = istart-1, iend
          zu(i,j,k) = u(i,j,k,nadv) * acrlat(j,2)
          zv(i,j,k) = v(i,j,k,nadv) * r_earth_recip
        END DO
      END DO
    END DO

    CALL horiz_adv_driver_stagmix( pp(:,:,:,nadv), zu, zv, ppadvt, zsign,   &
         &     istart, iend, jstart, jend, 1, ke, ke, ke, iadv_order )
! <fuo_tadvlimit
    CALL horiz_adv_driver_stagmix(  t(:,:,:,nadv), zu, zv,  tadvt, zsign,   &
         &     istart, iend, jstart, jend, 1, ke, ke, ke, iadv_order,       &
               ltadv_limiter, theta )
! fuo_tadvlimit>

  CASE default
    yzerrmsg="wrong value of ''iztype_tppadv'' for SR ''horiz_adv_driver_stagmix''"
    CALL model_abort (my_cart_id, 135, yzerrmsg, 'advection')
  END SELECT


! <fuo_tadvlimit
  ! deallocate memory
  IF (ltadv_limiter .AND. itheta_adv /= 2) THEN
    DEALLOCATE( theta )
  END IF
  NULLIFY( theta )
! fuo_tadvlimit>

  !WL2011b
  IF (irk==irk_order.and.lbud) THEN
     tt_hadv(:,:,:) = tadvt(:,:,:)
  ENDIF
  !WL2011e

  ! Advective tendencies for the horizontal wind components (u, v)
  ! --------------------------------------------------------------

  uadvt(:,:,:) = 0.0_wp

  ! calc.  u= d lambda/dt  and  v= d phi/dt and average to u-position:
  DO  k = 1, ke
    DO j = jstartu, jendu
      DO i = istartu, iendu
        !zu(i,j,k) = u(i,j,k,nadv) * acrlat(j,1)
        zu(i,j,k) = (u(i+1,j,k,nadv) + u(i,j,k,nadv) + u(i-1,j,k,nadv) ) &
                        /3.0_wp * acrlat(j,1)
        zv(i,j,k) = 0.25_wp * ( v(i,j  ,k,nadv) + v(i+1,j  ,k,nadv)      &
                                  + v(i,j-1,k,nadv) + v(i+1,j-1,k,nadv) )    &
                           * r_earth_recip
      END DO
    END DO
  END DO

  CALL horiz_adv_driver( u(:,:,:,nadv), zu, zv, uadvt, zsign, &
    &               istartu, iendu, jstartu, jendu, 1, ke, ke, ke, iadv_order )

  vadvt(:,:,:) = 0.0_wp

  ! calc.  u= d lambda/dt  and  v= d phi/dt and average to v-position:
  DO  k = 1, ke
    DO j = jstartv, jendv
      DO i = istartv, iendv
        zu(i,j,k) = 0.25_wp * ( u(i-1,j+1,k,nadv) + u(i,j+1,k,nadv)    &
                                  + u(i-1,j  ,k,nadv) + u(i,j  ,k,nadv) )  &
                  * acrlat(j,2)
        zv(i,j,k) = (v(i,j+1,k,nadv) + v(i,j,k,nadv) + v(i,j-1,k,nadv) ) &
                        / 3.0_wp * r_earth_recip
      END DO
    END DO
  END DO

  CALL horiz_adv_driver( v(:,:,:,nadv), zu, zv, vadvt, zsign, &
               istartv, iendv, jstartv, jendv, 1, ke, ke, ke, iadv_order )


  DEALLOCATE( zu, zv, STAT=izstatd )
  IF ( izstatd /= 0 ) THEN
    yzerrmsg="deallocation of zu, zv"
    CALL model_abort (my_cart_id, 100+izstatd, yzerrmsg, 'advection')
  END IF

  ! Advective tendency for the vertical wind component w
  ! -------------------------------------------------------

  ALLOCATE( zu(1:ie, 1:je, 1:ke1),  &
    &       zv(1:ie, 1:je, 1:ke1),  &
    &       STAT=izstata)
  IF ( izstata /= 0 ) THEN
    yzerrmsg="2nd allocation of zu, zv"
    CALL model_abort (my_cart_id, 100+izstata, yzerrmsg, 'advection')
  END IF


  wadvt(:,:,:) = 0.0_wp

  ! calc.  u= d lambda/dt  and  v= d phi/dt and average to w-position:
  DO j = jstart, jend
!CDIR OUTERUNROLL=8
    DO  k = 2, ke
!CDIR ON_ADB(u)
      DO i = istart, iend
        zu(i,j,k) = 0.25_wp * ( u(i-1,j  ,k-1,nadv) + u(i,j,k-1,nadv)    &
                                  + u(i-1,j  ,k  ,nadv) + u(i,j,k  ,nadv) )  &
                            * acrlat(j,1)
        zv(i,j,k) = 0.25_wp * ( v(i  ,j-1,k-1,nadv) + v(i,j,k-1,nadv)    &
                                  + v(i  ,j-1,k  ,nadv) + v(i,j,k  ,nadv) )  &
                            * r_earth_recip
      END DO
    END DO
  END DO

  CALL horiz_adv_driver( w(:,:,:,nadv), zu, zv, wadvt, zsign, &
    &             istart, iend, jstart, jend, 2, ke, ke1, ke, iadv_order )

!------------------------------------------------------------------------------
! Section 2: Calculation of the contravariant vertical velocity (wcon)
!------------------------------------------------------------------------------

  wcon(:,:,:)=0.0_wp

  ! (negative) tendency of horizontal advection of z:

  CALL horiz_adv_driver( hhl, zu, zv, wcon, zsign,  &
    &             istart, iend, jstart, jend, 2, ke, ke1, ke, iadv_order )

  DO k = 2, ke
    DO j = jstart, jend
      DO  i = istart, iend
        wcon(i,j,k) = sqrtg_r_w(i,j,k) * ( -wcon(i,j,k) - w(i,j,k,nadv) )
      ENDDO
    ENDDO
  ENDDO

  DEALLOCATE( zu, zv, STAT=izstatd )
  IF ( izstatd /= 0 ) THEN
    yzerrmsg = '2nd deallocation of zu, zv'
    CALL model_abort (my_cart_id, 100+izstatd, yzerrmsg, 'advection')
  END IF

!------------------------------------------------------------------------------
! Section 3: Limit contravariant vertical velocity if explicit vertical 
!            advection is used; exchange of wcon
!------------------------------------------------------------------------------

  IF ( y_vert_adv_dyn == "expl" ) THEN
    CALL limit_contravar_vert_veloc( wcon, nadv, irk, cfl_eva )
  ENDIF

  IF (ltime) THEN
    CALL get_timings (i_horizontal_advection, ntstep, dt, izerror)
    IF (ltime_barrier) THEN
      CALL comm_barrier (icomm_cart, izerror, yzerrmsg)
      CALL get_timings (i_barrier_waiting_dyn, ntstep, dt, izerror)
    ENDIF
  ENDIF

  kzdims(1:24)=(/ke1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  CALL exchg_boundaries                                                      &
    (  7  ,  sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,       &
     ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,             &
     my_cart_neigh, lperi_x, lperi_y, l2dim,                                 &
     20000+nexch_tag, ldatatypes, ncomm_type, izerror, yzerrmsg,             &
     wcon(:,:,:)  )

  IF (ltime) CALL get_timings (i_communications_dyn, ntstep, dt, izerror)

!------------------------------------------------------------------------------
! Section 4: Advective tendency for w at the full level ke
!            needed for the dynamical bottom boundary condition
!------------------------------------------------------------------------------

  ! Metrical factors for horizontal discretization

  DO  j = jstart-1 , jend+1
    zfadsx(j) = acrlat(j,1)*eddlon
    zfadsy(j) = acrlat(j,1)*eddlat
    ! tgrlat has 2 dimensions
    ztgrlatda(j,1) = tgrlat(j,1)/r_earth
    ztgrlatda(j,2) = tgrlat(j,2)/r_earth
  ENDDO


  IF ( ldyn_bbc ) THEN

    ! should have the same rank as the corresponding argument
    ! in udsdx routines
    ALLOCATE( zw_ke(ie,je,1), STAT=izstata )
    IF ( izstata /= 0 ) THEN
      yzerrmsg="allocation of zw_ke"
      CALL model_abort (my_cart_id, 100+izstata, yzerrmsg, 'advection')
    END IF

    DO   j = jstart-3, jend+3
      DO i = istart-3, iend+3
        zw_ke(i,j,1) = 0.5_wp * ( w (i,j,ke,nadv) + w (i,j,ke+1,nadv) )
      ENDDO
    ENDDO

    DO j = jstart, jend
      DO i = istart, iend

        ! average U to skalar points
        zui = 0.5_wp*( u(i,j,ke,nadv)+u(i-1,j,ke,nadv) )
        ! average V to skalar points
        zvi = 0.5_wp*( v(i,j,ke,nadv)+v(i,j-1,ke,nadv) ) * crlat(j,1)

        ! wadvt(:,:,ke1) is used to store the w-tendency (horizontal
        ! advection and buoyancy eff.) at the full level ke needed for the
        ! dynamical bottom boundary condition.

        wadvt(i,j,ke1) = wtens(i,j,ke1)             &
          - udsdx( 0, iadv_order, zw_ke(1,1,1),     &
                   ie, je, 1, i, j, 1, im, ip,      &
                   zui, zfadsx(j),                  &
                   zvi, zfadsy(j), zsign)

      ENDDO
    ENDDO

    IF (ltime) THEN
    CALL get_timings (i_horizontal_advection, ntstep, dt, izerror)
    IF (ltime_barrier) THEN
      CALL comm_barrier (icomm_cart, izerror, yzerrmsg)
      CALL get_timings (i_barrier_waiting_dyn, ntstep, dt, izerror)
      ENDIF
    ENDIF

    kzdims(1:24) = (/1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
    CALL exchg_boundaries                                                    &
      (  0  ,  sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,     &
       ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,           &
       my_cart_neigh, lperi_x, lperi_y, l2dim,                               &
       20000+nexch_tag, .FALSE., ncomm_type, izerror, yzerrmsg,              &
       wadvt(:,:,ke1)  )

    IF (ltime) CALL get_timings (i_communications_dyn, ntstep, dt, izerror)

    DEALLOCATE( zw_ke, STAT=izstatd )
    IF ( izstatd /= 0 ) THEN
      yzerrmsg="deallocation of zw_ke"
      CALL model_abort (my_cart_id, 100+izstatd, yzerrmsg, 'advection')
    END IF

  ENDIF

!------------------------------------------------------------------------------
! Section 5: Explicit vertical advective tendencies
!------------------------------------------------------------------------------

  IF ( y_vert_adv_dyn == "expl" ) THEN

    IF (ieva_order == 3) THEN
      ! Use optimized (vectorized) version for default option
      CALL explicit_vadv_organize_opt
    ELSE
      DO  k = 1, ke
        CALL explicit_vadv_organize
      ENDDO
    ENDIF

    !WL2011b
    IF (lbud.and.irk_order==irk) THEN
       tt_adv(:,:,:) = tadvt(:,:,:)
    ENDIF
    !WL2011e

  END IF

!------------------------------------------------------------------------------
! Section 6: Complete tendencies of velocity advection with the 
!            earth curvature terms
!------------------------------------------------------------------------------

  DO k = 1, ke
    DO j = jstartu, jendu
      DO i = istartu, iendu
        zvn = 0.25_wp * ( v(i,j  ,k,nadv) + v(i+1,j  ,k,nadv) &
                            + v(i,j-1,k,nadv) + v(i+1,j-1,k,nadv) )
        uadvt(i,j,k) = uadvt(i,j,k) + ztgrlatda(j,1) * u(i,j,k,nadv) * zvn
      ENDDO
    ENDDO
  ENDDO

  ! IF ladv_deep=.True. then add deep atmosphere terms:
  !   u*w/r and v*w/r and (u**2+v**2)/r
  ! additional -u*w/r term in the momentum equation for u

  IF (ladv_deep) THEN
    DO k = 1, ke
      DO j = jstartu, jendu
        DO i = istartu, iendu
          zwl  = 0.25_wp * ( w(i  ,j ,k ,nadv) + w(i  ,j ,k+1,nadv) + &
            &                    w(i+1,j ,k ,nadv) + w(i+1,j ,k+1,nadv) )
          zwad = zwl * u(i,j,k,nadv) / r_earth
          uadvt(i,j,k) = uadvt(i,j,k) - zwad
        ENDDO
      ENDDO
    ENDDO
  ENDIF

  DO k = 1, ke
    DO j = jstartv, jendv
      DO i = istartv, iendv
         ! tgrlat has 2 dimensions
        zvn = ztgrlatda(j,2) * &
          (0.25_wp*(u(i,j+1,k,nadv)+u(i-1,j+1,k,nadv) + &
                        u(i,j  ,k,nadv)+u(i-1,j  ,k,nadv)))**2
        vadvt(i,j,k) = vadvt(i,j,k) - zvn
      ENDDO
    ENDDO
  ENDDO

  ! additional -v*w/r term in the momentum equation for v
  IF ( ladv_deep .EQV. .TRUE. ) THEN
    DO k = 1, ke
      DO j = jstartv, jendv
        DO i = istartv, iendv
          zwl  = w(i ,j  ,k ,nadv) + w(i ,j  ,k+1,nadv) + &
                 w(i ,j+1,k ,nadv) + w(i ,j+1,k+1,nadv)
          zwad = 0.25_wp*( zwl )
          vadvt(i,j,k) = vadvt(i,j,k) - zwad*v(i,j,k,nadv)/r_earth
        ENDDO
      ENDDO
    ENDDO
  ENDIF

  ! addditional (u*u+v*v)/r term in the momentum equation for w
  IF ( ladv_deep .EQV. .TRUE. ) THEN
    DO k = 2, ke                 !!w is zero on the lowest and highest level
      DO j = jstart, jend
        DO i = istart, iend
          zvn   = ( 0.25_wp*(u(i-1,j,k  ,nadv)+u(i,j,k  ,nadv)  +     &
                                 u(i-1,j,k-1,nadv)+u(i,j,k-1,nadv)) )**2
          zvs   = ( 0.25_wp*(v(i,j-1,k  ,nadv)+v(i,j,k  ,nadv)  +     &
                                 v(i,j-1,k-1,nadv)+v(i,j,k-1,nadv)) )**2
          zuvad = (zvn + zvs)/r_earth
          wadvt(i,j,k) = wadvt(i,j,k) + zuvad
        ENDDO
      ENDDO
    ENDDO
  ENDIF

!------------------------------------------------------------------------------
! End of module procedure "advection"
!------------------------------------------------------------------------------

CONTAINS

!==============================================================================

SUBROUTINE explicit_vadv_organize_opt

!------------------------------------------------------------------------------
! vertical advection of u, v, pp and tp
! Optimized version for default option (ieva_order = 3)
!------------------------------------------------------------------------------

! Local variables

INTEGER(KIND=iintegers) :: i, j, k, kbdy, km, kp

REAL (KIND=wp)     :: zwi, zsign, one_over_12

  zsign = SIGN(1._wp,dt)
  one_over_12 = 1._wp/12._wp

  DO j = jstartu, jendu
    DO  k = 3, ke-2
!CDIR ON_ADB(wcon)
!CDIR ON_ADB(u)
      DO i = istartu, iendu

        ! average wcon to u-positions
        zwi = 0.25_wp * ( wcon(i  ,j,k) + wcon(i  ,j,k+1)    &
                            + wcon(i+1,j,k) + wcon(i+1,j,k+1) )

        uadvt(i,j,k) = uadvt(i,j,k)  &
                     - one_over_12 * (                    &
                     zwi * ( u(i,j,k-2,nadv) - 8._wp*(u(i,j,k-1,nadv)-u(i,j,k+1,nadv)) - u(i,j,k+2,nadv) ) &
                     + zsign * ABS(zwi) * ( u(i,j,k-2,nadv) - 4._wp*(u(i,j,k-1,nadv)+u(i,j,k+1,nadv)) &
                     + 6._wp*u(i,j,k,nadv) + u(i,j,k+2,nadv)) )
      ENDDO
    ENDDO
  ENDDO

  DO j = jstartv, jendv
    DO  k = 3, ke-2
!CDIR ON_ADB(wcon)
!CDIR ON_ADB(v)
      DO i = istartv, iendv

        ! average wcon to v-positions
        zwi = 0.25_wp * ( wcon(i,j  ,k) + wcon(i,j  ,k+1)    &
                            + wcon(i,j+1,k) + wcon(i,j+1,k+1) )

        vadvt(i,j,k) = vadvt(i,j,k)  &
                     - one_over_12 * (                    &
                     zwi * ( v(i,j,k-2,nadv) - 8._wp*(v(i,j,k-1,nadv)-v(i,j,k+1,nadv)) - v(i,j,k+2,nadv) ) &
                     + zsign * ABS(zwi) * ( v(i,j,k-2,nadv) - 4._wp*(v(i,j,k-1,nadv)+v(i,j,k+1,nadv)) &
                     + 6._wp*v(i,j,k,nadv) + v(i,j,k+2,nadv)) )
      ENDDO
    ENDDO
  ENDDO

! vertical advective tendency for the prognostic scalars pp and tp
! ----------------------------------------------------------------

  DO j = jstart, jend
    DO  k = 3, ke-2
!CDIR ON_ADB(pp)
!CDIR ON_ADB(t)
      DO  i = istart, iend

        ! average wcon to scalar positions
        zwi = 0.5_wp * ( wcon(i,j,k) + wcon(i,j,k+1) )

        ! pp
        ppadvt(i,j,k) = ppadvt(i,j,k)  &
                      - one_over_12 * (                    &
                      zwi * ( pp(i,j,k-2,nadv) - 8._wp*(pp(i,j,k-1,nadv)-pp(i,j,k+1,nadv)) - pp(i,j,k+2,nadv) ) &
                      + zsign * ABS(zwi) * ( pp(i,j,k-2,nadv) - 4._wp*(pp(i,j,k-1,nadv)+pp(i,j,k+1,nadv)) &
                      + 6._wp*pp(i,j,k,nadv) + pp(i,j,k+2,nadv)) )
        ! tp
        tadvt(i,j,k)  = tadvt(i,j,k)   &
                      - one_over_12 * (                    &
                      zwi * ( t(i,j,k-2,nadv) - 8._wp*(t(i,j,k-1,nadv)-t(i,j,k+1,nadv)) - t(i,j,k+2,nadv) ) &
                      + zsign * ABS(zwi) * ( t(i,j,k-2,nadv) - 4._wp*(t(i,j,k-1,nadv)+t(i,j,k+1,nadv)) &
                      + 6._wp*t(i,j,k,nadv) + t(i,j,k+2,nadv)) )

      ENDDO
    ENDDO
  ENDDO

! vertical advective tendencies for vertical velocity w
! -----------------------------------------------------

  DO j = jstart, jend
    DO  k = 3, ke1-2
!CDIR ON_ADB(w)
      DO i = istart, iend

        zwi = wcon(i,j,k)

        wadvt(i,j,k) = wadvt(i,j,k)  &
                      - one_over_12 * (                    &
                     zwi * ( w(i,j,k-2,nadv) - 8._wp*(w(i,j,k-1,nadv)-w(i,j,k+1,nadv)) - w(i,j,k+2,nadv) ) &
                     + zsign * ABS(zwi) * ( w(i,j,k-2,nadv) - 4._wp*(w(i,j,k-1,nadv)+w(i,j,k+1,nadv)) &
                     + 6._wp*w(i,j,k,nadv) + w(i,j,k+2,nadv)) )

      ENDDO
    ENDDO
  ENDDO

! First order advection for upper and lower boundaries
  DO kbdy = 1, 4
    IF ( kbdy == 1 ) THEN
      k = 1
      km = 1
      kp = 2
    ELSE IF ( kbdy == 2 ) THEN
      k = ke
      km = ke-1
      kp = ke
    ELSE IF ( kbdy == 3 ) THEN
      k = 2
      km = 1
      kp = 3
    ELSE IF ( kbdy == 4 ) THEN
      k = ke-1
      km = ke-2
      kp = ke
    ENDIF

    DO j = jstartu, jendu
      DO i = istartu, iendu

        ! average wcon to u-positions
        zwi = 0.25_wp * ( wcon(i  ,j,k) + wcon(i  ,j,k+1)    &
                            + wcon(i+1,j,k) + wcon(i+1,j,k+1) )

        uadvt(i,j,k) = uadvt(i,j,k)  &
                     - 0.5_wp * (                              &
                     zwi * ( - u(i,j,km,nadv) + u(i,j,kp,nadv) )                                &
                     + zsign * ABS(zwi) * ( - u(i,j,km,nadv) + 2._wp*u(i,j,k,nadv) - u(i,j,kp,nadv) ) )
      ENDDO
    ENDDO

    DO j = jstartv, jendv
      DO i = istartv, iendv

        ! average wcon to v-positions
        zwi = 0.25_wp * ( wcon(i,j  ,k) + wcon(i,j  ,k+1)    &
                            + wcon(i,j+1,k) + wcon(i,j+1,k+1) )

        vadvt(i,j,k) = vadvt(i,j,k)  &
                     - 0.5_wp * (                              &
                     zwi * ( - v(i,j,km,nadv) + v(i,j,kp,nadv) )                                &
                     + zsign * ABS(zwi) * ( - v(i,j,km,nadv) + 2._wp*v(i,j,k,nadv) - v(i,j,kp,nadv) ) )

      ENDDO
    ENDDO

! vertical advective tendency for the prognostic scalars pp and tp
! ----------------------------------------------------------------

    DO j = jstart, jend
      DO  i = istart, iend

        ! average wcon to scalar positions
        zwi = 0.5_wp * ( wcon(i,j,k) + wcon(i,j,k+1) )

        ! pp
        ppadvt(i,j,k) = ppadvt(i,j,k)  &
                      - 0.5_wp * (                              &
                      zwi * ( - pp(i,j,km,nadv) + pp(i,j,kp,nadv) )                                &
                      + zsign * ABS(zwi) * ( - pp(i,j,km,nadv) + 2._wp*pp(i,j,k,nadv) - pp(i,j,kp,nadv) ) )
        ! tp
        tadvt(i,j,k)  = tadvt(i,j,k)   &
                      - 0.5_wp * (                              &
                      zwi * ( - t(i,j,km,nadv) + t(i,j,kp,nadv) )                                &
                      + zsign * ABS(zwi) * ( - t(i,j,km,nadv) + 2._wp*t(i,j,k,nadv) - t(i,j,kp,nadv) ) )
      ENDDO
    ENDDO
  ENDDO

  DO kbdy = 1, 2
    IF ( kbdy == 1 ) THEN
      k = 2
      km = 1
      kp = 3
    ELSE IF ( kbdy == 2 ) THEN
      k = ke1-1
      km = ke1-2
      kp = ke1
    ENDIF

! vertical advective tendencies for vertical velocity w
! -----------------------------------------------------

    DO j = jstart, jend
      DO i = istart, iend

        zwi = wcon(i,j,k)

        wadvt(i,j,k) = wadvt(i,j,k)  &
                     - 0.5_wp * (                              &
                     zwi * ( - w(i,j,km,nadv) + w(i,j,kp,nadv) )                                &
                     + zsign * ABS(zwi) * ( - w(i,j,km,nadv) + 2._wp*w(i,j,k,nadv) - w(i,j,kp,nadv) ) )

      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE explicit_vadv_organize_opt

SUBROUTINE explicit_vadv_organize

!------------------------------------------------------------------------------
! vertical advection of u, v, pp and tp
!------------------------------------------------------------------------------

  IF ( k >= 1+imipmx .AND. k <= ke-imipmx ) THEN
    SELECT CASE(ieva_order)
    CASE(1,2)
      CALL explicit_vadv( ieva_order, -1, 1 )
    CASE(3,4)
      CALL explicit_vadv( ieva_order, -2, 2 )
    CASE(5,6)
      CALL explicit_vadv( ieva_order, -3, 3 )
    END SELECT
  ELSE
    SELECT CASE(ieva_order)
    CASE(1,3,5)
      IF ( k == 1 )                                       &
        CALL explicit_vadv( 1, 0, 1 )
      IF ( k == ke )                                      &
        CALL explicit_vadv( 1, -1, 0 )
      IF ( k == 2 .OR. ke-k == 1 )                        &
        CALL explicit_vadv( 1, -1, 1 )
      IF ( k == 3 .OR. ke-k == 2 )                        &
        CALL explicit_vadv( 3, -2, 2 )
    CASE(2,4,6)
      IF ( k == 1 )                                       &
        CALL explicit_vadv( 2, 0, 1 )
      IF ( k == ke )                                      &
        CALL explicit_vadv( 2, -1, 0 )
      IF ( k == 2 .OR. ke-k == 1 )                        &
        CALL explicit_vadv( 2, -1, 1 )
      IF ( k == 3 .OR. ke-k == 2 )                        &
        CALL explicit_vadv( 4, -2, 2 )
    END SELECT
  END IF

!------------------------------------------------------------------------------
! vertical advection of w
!------------------------------------------------------------------------------

  IF ( k >= 1+imipmx .AND. k <= ke1-imipmx ) THEN
    SELECT CASE(ieva_order)
    CASE(1,2)
      CALL explicit_vadv_w( ieva_order, -1, 1 )
    CASE(3,4)
      CALL explicit_vadv_w( ieva_order, -2, 2 )
    CASE(5,6)
      CALL explicit_vadv_w( ieva_order, -3, 3 )
    END SELECT
  ELSE
    SELECT CASE(ieva_order)
    CASE(1,3,5)
      IF ( k == 2 .OR. ke1-k == 1 )                       &
        CALL explicit_vadv_w( 1, -1, 1 )
      IF ( k == 3 .OR. ke1-k == 2 )                       &
        CALL explicit_vadv_w( 3, -2, 2 )
    CASE(2,4,6)
      IF ( k == 2 .OR. ke1-k == 1 )                       &
        CALL explicit_vadv_w( 2, -1, 1 )
      IF ( k == 3 .OR. ke1-k == 2 )                       &
        CALL explicit_vadv_w( 4, -2, 2 )
    END SELECT
  END IF

END SUBROUTINE explicit_vadv_organize

!==============================================================================

SUBROUTINE explicit_vadv( udsdx_eva_index, im_eva, ip_eva )

!------------------------------------------------------------------------------
! Description:
!   This procedure calculates the vertical advective tendencies for the
!   prognostic variables u, v, w, pp and tp using an explicit scheme
!------------------------------------------------------------------------------

! Declarations:

! Subroutine arguments:
! ---------------------
INTEGER (KIND=iintegers), INTENT(in) ::  &
  udsdx_eva_index

INTEGER (KIND=iintegers), INTENT(in) ::  &
  im_eva, ip_eva

! Local:
! -------------
REAL (KIND=wp)     ::  &
  sz(im_eva:ip_eva)     ! stencil of the advected quantity

REAL (KIND=wp)     ::  &
  zwi, zsign

!------------------------------------------------------------------------------

! vertical advective tendencies for horizontal velocities u and v
! ---------------------------------------------------------------
  zsign = SIGN(1._wp,dt)
  DO j = jstartu, jendu
    DO i = istartu, iendu

      ! average wcon to u-positions
      zwi = 0.25_wp * ( wcon(i  ,j,k) + wcon(i  ,j,k+1)    &
                          + wcon(i+1,j,k) + wcon(i+1,j,k+1) )

      uadvt(i,j,k) = uadvt(i,j,k)  &
               - udsdx( 3, udsdx_eva_index, u(1:ie,1:je,1:ke,nadv),   &
                             ie, je, ke, i, j, k, im_eva, ip_eva,     &
                             zwi, 1.0_wp,                         &
                             0.0_wp, 0.0_wp, zsign )

    ENDDO
  ENDDO

  DO j = jstartv, jendv
    DO i = istartv, iendv

      ! average wcon to v-positions
      zwi = 0.25_wp * ( wcon(i,j  ,k) + wcon(i,j  ,k+1)    &
                          + wcon(i,j+1,k) + wcon(i,j+1,k+1) )

      vadvt(i,j,k) = vadvt(i,j,k)  &
               - udsdx( 3, udsdx_eva_index, v(1:ie,1:je,1:ke,nadv),   &
                             ie, je, ke, i, j, k, im_eva, ip_eva,     &
                             zwi, 1.0_wp,                         &
                             0.0_wp, 0.0_wp, zsign)

    ENDDO
  ENDDO

! vertical advective tendency for the prognostic scalars pp and tp
! ----------------------------------------------------------------

  DO j = jstart, jend
    DO  i = istart, iend

      ! average wcon to scalar positions
      zwi = 0.5_wp * ( wcon(i,j,k) + wcon(i,j,k+1) )

      ! pp
      ppadvt(i,j,k) = ppadvt(i,j,k)  &
                - udsdx( 3, udsdx_eva_index, pp(1:ie,1:je,1:ke,nadv), &
                              ie, je, ke, i, j, k, im_eva, ip_eva,    &
                              zwi, 1.0_wp,                        &
                              0.0_wp, 0.0_wp, zsign)
      ! tp
      tadvt(i,j,k)  = tadvt(i,j,k)   &
               - udsdx( 3, udsdx_eva_index, t(1:ie,1:je,1:ke,nadv),   &
                              ie, je, ke, i, j, k, im_eva, ip_eva,    &
                              zwi, 1.0_wp,                        &
                              0.0_wp, 0.0_wp, zsign)

    ENDDO
  ENDDO

!------------------------------------------------------------------------------
! End of subroutine explicit_vadv
!------------------------------------------------------------------------------

END SUBROUTINE explicit_vadv

!==============================================================================

SUBROUTINE explicit_vadv_w( udsdx_eva_index, im_eva, ip_eva )

!------------------------------------------------------------------------------
!
! Description:
!   This procedure calculates the vertical advective tendency for the
!   prognostic variable w using an explicit scheme
!
!------------------------------------------------------------------------------

! Declarations:

! Subroutine arguments:
! ---------------------
INTEGER (KIND=iintegers), INTENT(in) ::  &
  udsdx_eva_index

INTEGER (KIND=iintegers), INTENT(in) ::  &
  im_eva, ip_eva

! Local:
! -------------
REAL (KIND=wp)     ::  &
  sz(im_eva:ip_eva)     ! stencil of the advected quantity

REAL (KIND=wp)     ::  &
  zwi, zsign

!--------------------------------------------------------------------------

  zsign = SIGN(1._wp, dt)

! vertical advective tendencies for vertical velocity w
! -----------------------------------------------------
  DO j = jstart, jend
    DO i = istart, iend

      zwi = wcon(i,j,k)

      wadvt(i,j,k) = wadvt(i,j,k)  &
               - udsdx( 3, udsdx_eva_index, w(1:ie,1:je,1:ke1,nadv),  &
                             ie, je, ke1, i, j, k, im_eva, ip_eva,    &
                             zwi, 1.0_wp,                         &
                             0.0_wp, 0.0_wp, zsign)

    ENDDO
  ENDDO

!------------------------------------------------------------------------------
! End of subroutine explicit_vadv_w
!------------------------------------------------------------------------------

END SUBROUTINE explicit_vadv_w

!==============================================================================

END SUBROUTINE advection

!==============================================================================
!==============================================================================

SUBROUTINE limit_contravar_vert_veloc( zwcon, nadv, irk, cfl_eva )

!------------------------------------------------------------------------------
!
! Description:
!   Limitation of the contravariant vertical velocity.
!   This is needed only, if explicit vertical advection is used
!
!------------------------------------------------------------------------------

  IMPLICIT NONE

REAL    (KIND=wp   ),     INTENT(inout) ::  zwcon(1:ie, 1:je, 1:ke1 )

INTEGER (KIND=iintegers), INTENT(IN) ::  &
  nadv,                & !
  irk

REAL    (KIND=wp   ),     INTENT(IN) ::  &
  cfl_eva                ! CFL-value (for explicit vertical advection)

REAL    (KIND=wp   )     ::  &
  zwccfl, zwcmin, zwcmax, zwcmin_loc, zwcmax_loc

INTEGER (KIND=iintegers) ::  i, j, k, m

INTEGER (KIND=iintegers) ::  &
  mijk(3),             & !  indices of min. max. value of zwcon
  ncfllim,             & !  number of CFL limitations of zwcon
  icfllim(ie,je,ke1),  & !
  kzdims(24),          & !  vertical dimensions for exchg_boundaries
  izerror

CHARACTER (LEN=80)       ::  &
  yzerrmsg

!------------------------------------------------------------------------------

  zwccfl = cfl_eva / ABS(dt)
  zwcmin_loc = MINVAL( zwcon(istart:iend,jstart:jend,2:ke) )
  zwcmax_loc = MAXVAL( zwcon(istart:iend,jstart:jend,2:ke) )
  zwcmin = zwcmin_loc
  zwcmax = zwcmax_loc

  IF (num_compute > 1) THEN
    IF (ltime) THEN
      CALL get_timings (i_horizontal_advection, ntstep, dt, izerror)
      IF (ltime_barrier) THEN
        CALL comm_barrier (icomm_cart, izerror, yzerrmsg)
        CALL get_timings (i_barrier_waiting_dyn, ntstep, dt, izerror)
      ENDIF
    ENDIF

    CALL global_values (zwcmin, 1, 'MIN', imp_reals, icomm_cart, -1,      &
      yzerrmsg, izerror)
    CALL global_values (zwcmax, 1, 'MAX', imp_reals, icomm_cart, -1,      &
      yzerrmsg, izerror)

    IF (ltime) CALL get_timings (i_communications_dyn, ntstep, dt, izerror)

  END IF ! IF (num_compute > 1)

  IF ( irk == irk_order ) THEN
!      zwcmin_loop: DO m = 0, num_compute-1
    IF ( zwcmin == zwcmin_loc ) THEN
      IF ( zwcmin < -zwccfl ) THEN
        mijk(:) = MINLOC( zwcon(istart:iend,jstart:jend,2:ke) )
        PRINT *, '     MINIMUM CFL_Z: ', zwcmin
        PRINT *, '     (i,j,k)   MIN: (', i_global(mijk(1)), ',',     &
             j_global(mijk(2)), ',',     &
             mijk(3), ')'
        PRINT *, '     w_cart    MIN: ', w(mijk(1),mijk(2),mijk(3),nadv)
      ENDIF
!          EXIT zwcmin_loop
    ENDIF
!      ENDDO zwcmin_loop

!      zwcmax_loop: DO m = 0, num_compute-1
    IF ( zwcmax == zwcmax_loc ) THEN
      IF ( zwcmax >  zwccfl ) THEN
        mijk(:) = MAXLOC( zwcon(istart:iend,jstart:jend,2:ke) )
        PRINT *, '     MAXIMUM CFL_Z: ', zwcmax
        PRINT *, '     (i,j,k)   MAX: (', i_global(mijk(1)), ',',     &
             j_global(mijk(2)), ',',     &
             mijk(3), ')'
        PRINT *, '     w_cart    MAX: ', w(mijk(1),mijk(2),mijk(3),nadv)
      ENDIF
!          EXIT zwcmax_loop
    ENDIF
!      ENDDO zwcmax_loop

    IF ( zwcmin < -zwccfl .OR. zwcmax > zwccfl ) THEN
      ncfllim = 0
      icfllim(:,:,:) = 0
      WHERE( zwcon(istart:iend,jstart:jend,2:ke) < -zwccfl )
        icfllim(istart:iend,jstart:jend,2:ke) = 1
      END WHERE
      WHERE( zwcon(istart:iend,jstart:jend,2:ke) >  zwccfl )
        icfllim(istart:iend,jstart:jend,2:ke) = 1
      END WHERE
      ncfllim = SUM( icfllim( istart:iend,jstart:jend,2:ke) )

      IF (num_compute > 1) THEN
        IF (ltime) THEN
          CALL get_timings (i_horizontal_advection, ntstep, dt, izerror)
          IF (ltime_barrier) THEN
            CALL comm_barrier (icomm_cart, izerror, yzerrmsg)
            CALL get_timings (i_barrier_waiting_dyn, ntstep, dt, izerror)
          ENDIF
        ENDIF

        CALL global_values (ncfllim, 1, 'SUM', imp_integers, icomm_cart, -1, &
          yzerrmsg, izerror)

        IF (ltime)  &
          CALL get_timings (i_communications_dyn, ntstep, dt, izerror)

        IF ( my_cart_id == 0 ) PRINT *, '     NUMBER of CFL_LIM: ', ncfllim
      ENDIF    ! num_compute > 1

    ENDIF

  ENDIF

  DO k = 2, ke
    DO j = jstart, jend
      DO i = istart, iend
        zwcon(i,j,k) = MIN( ABS(zwcon(i,j,k)), zwccfl )                   &
          * SIGN( 1.0_wp,zwcon(i,j,k) )
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE limit_contravar_vert_veloc

!==============================================================================
!+ Module procedure for computing monotone advection
!------------------------------------------------------------------------------

!option! -pvctl loopfusion
SUBROUTINE advection_pd( u_adv, v_adv, w_adv, nadv, dtadv, im, ip,    &
                         j2dim, ny_2dim )

!------------------------------------------------------------------------------
!
! Description:
!   This module procedure computes the advection for positive definite tracer
!   variables.
!
! Method:
!   This subroutine uses the positive definite van Leer type advection
!   algorithms. It uses the subroutines of "numeric_utilities".
!
!------------------------------------------------------------------------------

! Declarations:

! Subroutine arguments:
! ---------------------
REAL (KIND=wp),     INTENT(IN) ::  &
  u_adv(ie,je,ke),    & ! advection velocities
  v_adv(ie,je,ke),    & !
  w_adv(ie,je,ke1)      !

INTEGER (KIND=iintegers), INTENT(IN) ::  &
  nadv,                 & !
  im, ip,               & ! specify width of the stencil
  j2dim, ny_2dim          ! for 2dim runs

REAL (KIND=wp),     INTENT(IN) ::  &
  dtadv

!------------------------------------------------------------------------------

! Local parameters:
! ----------------
INTEGER (KIND=iintegers) ::  &
  ivl_off,             & ! arg of wcfrac_crint_rk
                         ! (set to 1 for van Leer and to 0 for PPM scheme)
  zicr_min, zicr_max,  & !
  iadvs, iadve,        & !
  jadvs, jadve,        & !
  i,  j,  k, ij, isp,  & !  Loop indices in longitudinal, latitudinal and
  izerror,             & !  vertical direction and COSMO-ART indices
  iztrcr,              & !  Index for tracer looping
  izdebug,             & !  for additional printouts
  kzdims(24)             !  vertical dimensions for exchg_boundaries

REAL    (KIND=wp   )     ::  &
  zeps_rho, zsign

REAL    (KIND=wp   )     ::  &
  r_earth_recip

LOGICAL ::  &
  lintcr_ne_zero,      & !
  calling_seq_xzy

! Local (automatic) arrays:
! ------------
INTEGER    (KIND=iintegers) ::  &
  zicr (ie,je,ke1)       !
  
REAL    (KIND=wp   )     ::  &
  zu   (ie,je,ke),           & !
  zv   (ie,je,ke),           & !
  zuvw_frac(ie,je,ke1),      & !
  zuv_tke(ie,je,ke),         & !
  zwc_tke(ie,je,ke1),        & !
  zrho_tke(ie,je,ke1),       & !
  ztkehlp(ie,je,ke1,2),      & !
  zdiv  (ie,je,ke),          & !
  zrdx  (   je   ),          & !
  zrdy  (  je,2  )             !

INTEGER (KIND=iintegers) :: &
  izadv  (trcr_get_ntrcr())      , & ! advection  for all tracers
  izlbc  (trcr_get_ntrcr())      , & ! lateral BC for all tracers
  izclip (trcr_get_ntrcr())          ! clipping   for all tracers

LOGICAL                  :: &
  lzadd_clp_adv(trcr_get_ntrcr()), & ! ADD_CLP_ADV for all tracers
  lz0grad_forced(trcr_get_ntrcr())   ! BD_0GRAD_FORCED for all tracers

! Tracer pointers:
!-----------------
REAL (KIND=wp),     POINTER :: &
  ztrcr      (:,:,:) => NULL(),& ! tracer variable at nnew
  ztrcr_nadv (:,:,:) => NULL(),& ! tracer variable at nadv
  qv_now     (:,:,:) => NULL(),& ! QV at nnow
  qc_now     (:,:,:) => NULL(),& ! QC at nnow
  qv         (:,:,:) => NULL(),& ! QV at nnew
  qc         (:,:,:) => NULL(),& ! QC at nnew
  qi         (:,:,:) => NULL(),& ! QI at nnew
  qr         (:,:,:) => NULL(),& ! QR at nnew
  qs         (:,:,:) => NULL(),& ! QS at nnew
  qg         (:,:,:) => NULL(),& ! QG at nnew
  qh         (:,:,:) => NULL()   ! QH at nnew

CHARACTER (LEN=80)       :: yzerrmsg
CHARACTER (LEN=25)       :: yzroutine

!- End of header
!==============================================================================

  yzroutine='advection_pd'

  IF (ldebug_dyn) THEN
    IF (lprintdeb_all) THEN
      izdebug = idbg_level
    ELSE
      IF (my_cart_id == 0) THEN
        izdebug = idbg_level
      ELSE
        izdebug = 0
      ENDIF
    ENDIF
  ELSE
    izdebug = 0
  ENDIF

  zeps_rho  = 1.0E-5_wp
  zsign = SIGN(1._wp,dt)

  iadvs = 1
  iadve = ie
  jadvs = 1
  jadve = je
  IF ( .NOT.lperi_x ) THEN
    ! west
    IF (my_cart_neigh(1) == -1) iadvs = istart
    ! east
    IF (my_cart_neigh(3) == -1) iadve = iend
  END IF
  IF ( .NOT.lperi_y ) THEN
    ! south
    IF (my_cart_neigh(4) == -1) jadvs = jstart
    ! north
    IF (my_cart_neigh(2) == -1) jadve = jend
  ENDIF

  ! set arg. of wcfrac_crint_rk
  SELECT CASE( TRIM(y_scalar_advect) )
  CASE( "VANLEER", "VANLEER_STRANG" )
    ivl_off = 1_iintegers
  CASE default
    ivl_off = 0_iintegers
  END SELECT

  ! Retrieve the required metadata
  !-----------------------------
  CALL trcr_meta_get(izerror, T_ADV_ID, izadv)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_meta_get(izerror, T_CLP_ID, izclip)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_meta_get(izerror, 'ADD_CLP_ADV', lzadd_clp_adv)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_meta_get(izerror, T_LBC_ID, izlbc)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_meta_get(izerror, 'BD_0GRAD_FORCED', lz0grad_forced)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF

  ! Retrieve the required microphysics tracers
  !-----------------------------------------
  CALL trcr_get(izerror, idt_qv, ptr_tlev=nnow, ptr=qv_now)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qc, ptr_tlev=nnow, ptr=qc_now)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qv, ptr_tlev=nnew, ptr=qv)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qc, ptr_tlev=nnew, ptr=qc)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qi, ptr_tlev=nnew, ptr=qi)
  IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qr, ptr_tlev=nnew, ptr=qr)
  IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qs, ptr_tlev=nnew, ptr=qs)
  IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(izerror, idt_qg, ptr_tlev=nnew, ptr=qg)
  IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
#ifdef TWOMOM_SB
  IF (itype_gscp >= 100) THEN
    CALL trcr_get(izerror, idt_qh, ptr_tlev=nnew, ptr=qh)
    IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
  END IF
#endif

  ! Metrical factors for horizontal discretization
  !-----------------------------------------------

  DO j = 1, je
    zrdx(j)   = acrlat(j,1)*eddlon
    zrdy(j,1) = acrlat(j,1)*eddlat
    zrdy(j,2) = acrlat(j,2)*eddlat
  ENDDO

  ! Calculation of the mass weighted contravariant vertical velocity (zwc)
  !        = sqrt(G) * dzeta/dt
  !--------------------------------------------------------------------------

  r_earth_recip = 1.0_wp / r_earth

  ! calc.  u= d lambda/dt  and  v= d phi/dt and average to w-position:
  DO  k = 2, ke
    DO j = jstart, jend
      DO i = istart, iend
        zu(i,j,k) = 0.25_wp * ( u_adv(i-1,j  ,k-1) + u_adv(i,j,k-1)    &
                                  + u_adv(i-1,j  ,k  ) + u_adv(i,j,k  ) )  &
                            * acrlat(j,1)
        zv(i,j,k) = 0.25_wp * ( v_adv(i  ,j-1,k-1) + v_adv(i,j,k-1)    &
                                  + v_adv(i  ,j-1,k  ) + v_adv(i,j,k  ) )  &
                            * r_earth_recip
      ENDDO
    ENDDO
  ENDDO

  wcon(:,:,:)=0.0_wp

  ! (negative) tendency of horizontal advection of z:

  CALL horiz_adv_driver( hhl, zu, zv, wcon, zsign,  &
                 istart, iend, jstart, jend, 2, ke, ke1, ke, iadv_order )

  DO  k = 2, ke
    DO  j = jstart, jend
      DO  i = istart, iend
        !wcon(i,j,k) = -wcon(i,j,k) - w(i,j,k,nadv)      !MB: not correct time level!!
        wcon(i,j,k) = -wcon(i,j,k) - w_adv(i,j,k)      !MB: better!!
      ENDDO
    ENDDO
  ENDDO

  !wcon(:,:,1  ) = 0.0_wp
  !wcon(:,:,ke1) = 0.0_wp
  IF ( (y_scalar_advect=="SL3_MF") .OR.    &
       (y_scalar_advect=="SL3_SFD") ) THEN
    DO  k = 2, ke
      DO  j = jstart, jend
        DO  i = istart, iend
          wcon(i,j,k) = sqrtg_r_w(i,j,k) * wcon(i,j,k)
        ENDDO
      ENDDO
    ENDDO
  END IF

  ! update halo of wcon
  IF (ltime) THEN
    CALL get_timings (i_horizontal_advection, ntstep, dt, izerror)
    IF (ltime_barrier) THEN
      CALL comm_barrier (icomm_cart, izerror, yzerrmsg)
      CALL get_timings (i_barrier_waiting_dyn, ntstep, dt, izerror)
    ENDIF
  ENDIF

  kzdims(1:24)=(/ke1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  CALL exchg_boundaries                                                      &
    (  7  ,  sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,       &
     ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,             &
     my_cart_neigh, lperi_x, lperi_y, l2dim,                                 &
     20000+nexch_tag, .FALSE.   , ncomm_type, izerror, yzerrmsg,             &
     wcon(:,:,:)  )

  IF (ltime) CALL get_timings (i_communications_dyn, ntstep, dt, izerror)

  ! pre-compute contravariant velocity at scalar point
  IF ( lprog_tke .AND. lalloc_tke ) THEN
    DO k = 2, ke1
      DO j = 1, je
        DO i = 1, ie
          zwc_tke(i,j,k) = 0.5_wp*( wcon(i,j,k-1) + wcon(i,j,k) )
        END DO
      END DO
    END DO
    zwc_tke(:,:,1) = 0.0_wp
    IF ( itype_turb == 3 ) THEN
      ! NOTE: in this case tke(:,:,:) it is q=SQRT(2*TKE), and for this quantity
      !       we need the tket_adv later.
      ! Save the current values of TKE in helper field:
      ztkehlp(:,:,:,nnow) = tke(:,:,:,nnow)
      ztkehlp(:,:,:,nnew) = tke(:,:,:,nnew)
      ! This is the advected quantity, either TKE or q:
      !  (will be multiplied by the density on half levels before calling the advection operator)
      IF ( (y_scalar_advect=="SL3_MF") .OR.    &
           (y_scalar_advect=="SL3_SFD") ) THEN
        tke(:,:,:,nnow) = tke(:,:,:,ntke)
      ELSE
        tke(:,:,:,nnew) = tke(:,:,:,ntke)
      END IF
    ELSE
      IF ( (y_scalar_advect=="SL3_MF") .OR.    &
           (y_scalar_advect=="SL3_SFD") ) THEN
        IF (nadv /= nnow) tke(:,:,:,nnow) = tke(:,:,:,nadv)
      ELSE    
        tke(:,:,:,nnew) = tke(:,:,:,nadv)
      END IF
    END IF
  END IF

#ifdef MESSY
  CALL trcr_calc(1)
#endif

  IF ( (y_scalar_advect=="SL3_MF") .OR.    &
       (y_scalar_advect=="SL3_SFD") ) THEN

    !--------------------------------------------------------------------------
    ! Calculate 3-dimensional semi-Lagrangian advection
    !--------------------------------------------------------------------------

    CALL advection_semi_lagrange( u_adv, v_adv, wcon, zwc_tke )

  ELSE

    ! In both cases of the transport (conservation form or not), the timelevel
    ! nnew has to be set now, to take care of the modified interfaces for 
    ! the advection operators: For the conservation form, this is just as before
    ! (but now also set tke(nnew). In the other case, timelevel nnew is just
    ! set to timelevel nadv.

    IF ( ltrcr_conserv_form ) THEN

      !------------------------------------------------------------------------
      ! Calculate densities of the tracer quantities
      ! --> Transport in conservation form!
      !------------------------------------------------------------------------

#ifndef MESSY
      ! Loop over tracers
      DO iztrcr = 1, trcr_get_ntrcr()

        ! get pointer to tracer (at nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr(izerror)
          CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
        ENDIF

        ! get pointer to tracer (at nadv)
        IF ( nadv /= nnew ) THEN
          ! get tracer at tlev=nadv 
          CALL trcr_get(izerror, iztrcr, ptr_tlev=nadv, ptr=ztrcr_nadv)
          IF (izerror /= 0) THEN
            yzerrmsg = trcr_errorstr(izerror)
            CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
          ENDIF
        ELSE
          ztrcr_nadv => ztrcr
        ENDIF

        ! Check for each tracer that advection is required
        IF ( izadv(iztrcr) == T_ADV_ON ) THEN
          ! conservative form of each tracer
!CDIR COLLAPSE
          ztrcr(:,:,:) = rho(:,:,:) * ztrcr_nadv(:,:,:)
        ELSE
          ! NOTE: even if advection is off for a tracer, the field has
          !       to be copied from nadv to nnew every timestep (if nadv /= nnew)
          IF ( nadv /= nnew ) THEN
!CDIR COLLAPSE
            ztrcr(:,:,:) = ztrcr_nadv(:,:,:)
          ENDIF
        ENDIF

      ENDDO
#endif

      IF ( lprog_tke .AND. lalloc_tke) THEN
        DO k = 2, ke
          DO j = 1, je
            DO i = 1, ie
              ! Esimate of rho at half levels:
              zrho_tke(i,j,k) = wgtfac(i,j,k) * rho(i,j,k) + (1.0_wp-wgtfac(i,j,k)) * rho(i,j,k-1)
            END DO
          END DO
        END DO
        zrho_tke(:,:,1) = rho(:,:,1)
        zrho_tke(:,:,ke1) = rho(:,:,ke)
        tke(:,:,:,nnew) = zrho_tke(:,:,:) * tke(:,:,:,nnew)
      ENDIF

    ELSE ! ltrcr_conserv_form

#ifndef MESSY
      ! Loop over tracers
      DO iztrcr = 1, trcr_get_ntrcr()

        ! get pointer to tracer (at nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr(izerror)
          CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
        ENDIF
           
        ! get pointer to tracer (at nadv)
        IF ( nadv /= nnew ) THEN
          ! get tracer at tlev=nadv 
          CALL trcr_get(izerror, iztrcr, ptr_tlev=nadv, ptr=ztrcr_nadv)
          IF (izerror /= 0) THEN
            yzerrmsg = trcr_errorstr(izerror)
            CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
          ENDIF
        ELSE
          ztrcr_nadv => ztrcr
        ENDIF

        ! tracers are transported as specific quantities
        ! NOTE: this copy has to be done also for tracers which don't
        !       undergo advection in order to copy from nnow to nnew
        IF ( nadv /= nnew ) THEN
!CDIR COLLAPSE
          ztrcr(:,:,:) = ztrcr_nadv(:,:,:)
        ENDIF

      ENDDO
#endif

      IF (lprog_tke .AND. lalloc_tke .AND. nnew /= nadv) THEN
        tke(:,:,:,nnew) = tke(:,:,:,nadv)
      END IF

    END IF

    ! Set outer boundary lines of qr, qs and qg (to zero gradient)
    CALL western_boundary
    CALL eastern_boundary
    CALL southern_boundary
    CALL northern_boundary

    ! Set outer boundary lines for the tracers in case of zero-gradient
    CALL lateral_boundaries_zerograd
    
#ifdef MESSY
    IF ( ltrcr_conserv_form ) THEN
       DO iztrcr = 1, trcr_get_ntrcr()
          IF ( izadv(iztrcr) == T_ADV_ON ) THEN

             ! get pointer to tracer (at nnew)
             CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
             IF (izerror /= 0) THEN
                yzerrmsg = trcr_errorstr(izerror)
                CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
             ENDIF

             ztrcr(:,:,:) = rho(:,:,:) * ztrcr(:,:,:)

          ENDIF
       ENDDO
    ENDIF
#endif

    SELECT CASE( TRIM(y_scalar_advect) )
      
    CASE( "BOTT2", "BOTT4" )

      IF ( MOD( ntstep, 2 ) == 0 ) THEN
        CALL advection_ef_xyz( xadv_pd_rk_cri_bott, yadv_pd_rk_cri_bott,  &
                               zadv_pd_rk_cri_bott,                       &
                               xadv_pd_rk_bott, yadv_pd_rk_bott,          &
                               zadv_pd_rk_bott )
      ELSE
        CALL advection_ef_zyx( xadv_pd_rk_cri_bott, yadv_pd_rk_cri_bott,  &
                               zadv_pd_rk_cri_bott,                       &
                               xadv_pd_rk_bott, yadv_pd_rk_bott,          &
                               zadv_pd_rk_bott )
      END IF

    CASE( "BOTT2_STRANG", "BOTT4_STRANG" )

      !------------------------------------------------------------------------
      ! Calculate 3-dimensional Bott-type advection
      ! - Courant number independent formulation
      ! - directional Marchuk- and Strang-splitting
      !------------------------------------------------------------------------

      CALL advection_ef_zyxyz( xadv_pd_rk_cri_bott, yadv_pd_rk_cri_bott,  &
                               zadv_pd_rk_cri_bott,                       &
                               xadv_pd_rk_bott, yadv_pd_rk_bott,          &
                               zadv_pd_rk_bott )

    CASE( "BOTT2_XYZYX", "BOTT4_XYZYX" )

      !------------------------------------------------------------------------
      ! Calculate 3-dimensional Bott-type advection
      ! - Courant number independent formulation
      ! - directional Marchuk- and Strang-splitting
      !------------------------------------------------------------------------

      CALL advection_ef_xyzyx( xadv_pd_rk_cri_bott, yadv_pd_rk_cri_bott,  &
                               zadv_pd_rk_cri_bott,                       &
                               xadv_pd_rk_bott, yadv_pd_rk_bott,          &
                               zadv_pd_rk_bott )

    CASE( "BOTT2_STRANG_B", "BOTT4_STRANG_B" )

      !------------------------------------------------------------------------
      ! Calculate 3-dimensional Bott-type advection
      ! - Courant number independent formulation
      ! - directional Marchuk- and Strang-splitting
      !------------------------------------------------------------------------

      IF ( MOD( ntstep, 2 ) == 0 ) THEN
        calling_seq_xzy = .TRUE.
      ELSE
        calling_seq_xzy = .FALSE.
      END IF

      CALL advection_ef_xyzyx_new( xadv_pd_rk_cri_bott, yadv_pd_rk_cri_bott,  &
                               zadv_pd_rk_cri_bott,                           &
                               xadv_pd_rk_bott, yadv_pd_rk_bott,              &
                               zadv_pd_rk_bott, calling_seq_xzy )

    CASE( "PPM" )

      !------------------------------------------------------------------------
      ! Calculate 3-dimensional PPM-type advection
      ! - Courant number independent formulation
      ! - directional Marchuk- and Strang-splitting
      !------------------------------------------------------------------------

      IF ( MOD( ntstep, 2 ) == 0 ) THEN
        CALL advection_ef_xyz( xadv_rk_cri_ppm, yadv_rk_cri_ppm,  &
                               zadv_rk_cri_ppm,                   &
                               xadv_rk_ppm, yadv_rk_ppm,          &
                               zadv_rk_ppm )
      ELSE
        CALL advection_ef_zyx( xadv_rk_cri_ppm, yadv_rk_cri_ppm,  &
                               zadv_rk_cri_ppm,                   &
                               xadv_rk_ppm, yadv_rk_ppm,          &
                               zadv_rk_ppm )
      END IF

    CASE( "PPM_STRANG" )

      CALL advection_ef_zyxyz( xadv_rk_cri_ppm, yadv_rk_cri_ppm,  &
                               zadv_rk_cri_ppm,                   &
                               xadv_rk_ppm, yadv_rk_ppm,          &
                               zadv_rk_ppm )

    CASE( "VANLEER" )
      
      !------------------------------------------------------------------------
      ! Calculate 3-dimensional positive definite vanLeer-type advection
      ! - Courant number independent formulation
      ! - directional Marchuk- and Strang-splitting
      !------------------------------------------------------------------------

      IF ( MOD( ntstep, 2 ) == 0 ) THEN
        CALL advection_ef_xyz( xadv_pd_rk_cri_vanleer, yadv_pd_rk_cri_vanleer,  &
                               zadv_pd_rk_cri_vanleer,                          &
                               xadv_pd_rk_vanleer, yadv_pd_rk_vanleer,          &
                               zadv_pd_rk_vanleer )
      ELSE
        CALL advection_ef_zyx( xadv_pd_rk_cri_vanleer, yadv_pd_rk_cri_vanleer,  &
                               zadv_pd_rk_cri_vanleer,                          &
                               xadv_pd_rk_vanleer, yadv_pd_rk_vanleer,          &
                               zadv_pd_rk_vanleer )
      END IF

    CASE( "VANLEER_STRANG" )

      CALL advection_ef_zyxyz( xadv_pd_rk_cri_vanleer, yadv_pd_rk_cri_vanleer,  &
                               zadv_pd_rk_cri_vanleer,                          &
                               xadv_pd_rk_vanleer, yadv_pd_rk_vanleer,          &
                               zadv_pd_rk_vanleer )

    CASE DEFAULT
      yzerrmsg = 'false value in y_scalar_advect'
      CALL model_abort (my_cart_id, 140, yzerrmsg, 'advection_pd')
    END SELECT

    !--------------------------------------------------------------------------
    ! Re-Calculate density of moist air and specific moisture quantities
    !--------------------------------------------------------------------------
    IF ( ltrcr_conserv_form ) THEN

      ! Loop over tracers
      DO iztrcr = 1, trcr_get_ntrcr() 

        ! get pointer to tracer (at nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr(izerror)
          CALL model_abort(my_cart_id, izerror, yzerrmsg, 'advection_pd')
        ENDIF

        ! Check for each tracer if clipping ensuring positive definiteness
        ! is required.
        IF ( izadv(iztrcr) == T_ADV_ON .AND.                                  &
             izclip(iztrcr) == T_CLP_ON ) THEN
           ! if so clip, eventual neg. val. created by advection to zero
           CALL clipping (ztrcr(:,:,:), ie, je , ke)
        ENDIF

      ENDDO

      IF ( lprog_tke .AND. lalloc_tke ) THEN
        CALL clipping( tke(:,:,:,nnew), ie, je, ke )
      END IF

      IF ( .NOT.lcalrho_advprog ) THEN
        qrs(:,:,:) = 0.0_wp
        IF ( ASSOCIATED(qi) ) THEN
          qrs(:,:,:) = qrs(:,:,:) + qi(:,:,:)
        END IF
        IF ( ASSOCIATED(qr) ) THEN
          qrs(:,:,:) = qrs(:,:,:) + qr(:,:,:)
        END IF
        IF ( ASSOCIATED(qs) ) THEN
          qrs(:,:,:) = qrs(:,:,:) + qs(:,:,:)
        END IF
        IF ( ASSOCIATED(qg) ) THEN
          qrs(:,:,:) = qrs(:,:,:) + qg(:,:,:)
        END IF
#ifdef TWOMOM_SB
        IF ( ASSOCIATED(qh) ) THEN
          qrs(:,:,:) = qrs(:,:,:) + qh(:,:,:)
        END IF
#endif
      END IF
      
      IF ( lcalrho_advprog ) THEN
        
        !CALL clipping( rho(:,:,:), ie, je, ke, zeps_rho )

        ! In this case it is highly important that the density does not
        ! take unrealistically small values.
        ! A rough estimate is the density value of the reference atmosphere.

        DO k=1, ke
          DO j=1, je
            DO i=1, ie
              IF ( rho(i,j,k) < 0.1_wp * rho0(i,j,k) ) THEN
                rho(i,j,k) = 0.1_wp * rho0(i,j,k)
              END IF
            END DO
          END DO
        END DO

      ELSE
        
        ! diagnostically compute density of moist air for time-level nnew
        ! ... using moisture densities
        CALL calrho_densities( t(:,:,:,nnew), pp(:,:,:,nnew), qv(:,:,:),      &
                               qc(:,:,:)    , qrs, p0, rho, ie, je, ke, r_d,  &
                               rvd_m_o )
        
      END IF

      ! compute specific tracer quantities for time-level nnew

      ! Loop over tracers
      DO iztrcr = 1, trcr_get_ntrcr()

        ! get pointer to tracer (at nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr(izerror)
          CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
        ENDIF

        ! check for each tracer if advection is required
        IF ( izadv(iztrcr) == T_ADV_ON ) THEN
          ! if so, calculate back the specific quantities
!CDIR COLLAPSE
          ztrcr(:,:,:) = ztrcr(:,:,:) / rho(:,:,:)
        ENDIF

      ENDDO

      IF ( lprog_tke .AND. lalloc_tke ) THEN
        DO k = 2, ke
          DO j = jstart, jend
            DO i = istart, iend
              zrho_tke(i,j,k) = wgtfac(i,j,k) * rho(i,j,k-1) + (1.0_wp-wgtfac(i,j,k)) * rho(i,j,k)
            END DO
          END DO
        END DO
        zrho_tke(istart:iend,jstart:jend,1) = rho(istart:iend,jstart:jend,1)
        zrho_tke(istart:iend,jstart:jend,ke1) = rho(istart:iend,jstart:jend,ke)
        tke(istart:iend,jstart:jend,:,nnew) = tke(istart:iend,jstart:jend,:,nnew) / &
             zrho_tke(istart:iend,jstart:jend,:)
      ENDIF

!CDIR COLLAPSE
      IF ( .NOT.lcalrho_advprog ) qrs(:,:,:) = qrs(:,:,:) / rho(:,:,:)

      IF ( lcalrho_advprog ) THEN
        
        ! diagnostically (re)compute density of moist air for time-level nnow
        ! ... using specific moisture quantities
        CALL calrho( t(:,:,:,nnow), pp(:,:,:,nnow), qv_now(:,:,:),  &
                     qc_now(:,:,:), qrs, p0, rho, ie, je, ke, r_d,  &
                     rvd_m_o )
        
      END IF
      
    ELSE
    
      !------------------------------------------------------------------------
      ! Add 3-dimensional divergence term (flux --> advection form)
      !------------------------------------------------------------------------

      DO k = 1, ke
        DO j = jstart, jend
          DO i = istart, iend
            zdiv(i,j,k) = dtadv * sqrtg_r_s(i,j,k) *               &
                 ( zrdx(j) *   (zu(i,j,k)-zu(i-1,j,k)) +           &
                   zrdy(j,1) * (zv(i,j,k)-zv(i,j-1,k)) +           &
                               (wcon(i,j,k+1)-wcon(i,j,k)) )
          ENDDO
        ENDDO
      ENDDO

      ! loop over tracers
      DO iztrcr = 1, trcr_get_ntrcr() 

        ! get pointer to tracer (at nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr(izerror)
          CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
        ENDIF

        ! check for each tracer if advection is required
        IF ( izadv(iztrcr) == T_ADV_ON ) THEN
          DO k = 1, ke
            DO j = jstart, jend
              DO i = istart, iend
                ztrcr(i,j,k) = ztrcr(i,j,k)                           &
                             + ztrcr(i,j,k)*zdiv(i,j,k)
              ENDDO
            ENDDO
          ENDDO
          IF ( lzadd_clp_adv(iztrcr) ) THEN
            CALL clipping (ztrcr(:,:,:), ie, je, ke)
          ENDIF
        ENDIF
      ENDDO

      IF ( lprog_tke .AND. lalloc_tke ) THEN
        DO k = 1, ke
          DO j = jstart, jend
            DO i = istart, iend
              tke(i,j,k,nnew) = tke(i,j,k,nnew) + tke(i,j,k,nnew)*zdiv(i,j,k)
            ENDDO
          ENDDO
        ENDDO
      ENDIF

    ENDIF

  ENDIF

#ifdef MESSY
  CALL trcr_calc(-1)
#endif

  IF (itype_turb == 3 .AND. lalloc_tke) THEN
    tket_adv(:,:,:) = 0.0_wp
    IF ( lprog_tke ) THEN
      ! Compute the SQRT(2*TKE)-tendency and add it to TKET_ADV-field:
      tket_adv(:,:,:) = ( tke(:,:,:,nnew) - ztkehlp(:,:,:,ntke) ) /  dt
      ! Save back the original SQRT(2*TKE) - values, because
      ! M. Raschendorfers scheme expects the advection as
      ! a separate TKET_ADV, which is added to TKE only within
      ! turbdiff.incf:
      tke(:,:,:,nnow) =  ztkehlp(:,:,:,nnow)
      tke(:,:,:,nnew) =  ztkehlp(:,:,:,nnew)
      ! For safety: clip tket_adv in a way to prevent the tke
      ! to become negative in M. Raschendorfers scheme:
      tket_adv(:,:,:) = MAX( -tke(:,:,:,ntke)/dt, tket_adv(:,:,:) )
    END IF
  END IF

!------------------------------------------------------------------------------
! End of module procedure "advection_pd"
!------------------------------------------------------------------------------
  
CONTAINS

!==============================================================================

SUBROUTINE advection_ef_xyz( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,  &
                             xadv_rk,     yadv_rk,     zadv_rk )

  ! Subroutine arguments:
  ! ---------------------
  EXTERNAL xadv_rk_cri, yadv_rk_cri, zadv_rk_cri, xadv_rk, yadv_rk, zadv_rk

  INTEGER (KIND=iintegers) :: kbegin, kend

  IF (izdebug > 10) THEN
    WRITE(*,*) "Subr. [advection_ef_xyz] ..."
  ENDIF

  CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jadvs, jadve, 1, ke, dtadv )

  CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, 1, ke, dtadv )

  CALL advection_ef_z( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, dtadv )

END SUBROUTINE advection_ef_xyz

!==============================================================================
!==============================================================================

SUBROUTINE advection_ef_zyx( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,  &
                             xadv_rk, yadv_rk, zadv_rk )

  ! Subroutine arguments:
  ! ---------------------
  EXTERNAL xadv_rk_cri, yadv_rk_cri, zadv_rk_cri, xadv_rk, yadv_rk, zadv_rk

  INTEGER(KIND=iintegers) :: kbegin, kend

  IF (izdebug > 10) THEN
    WRITE(*,*) "Subr. [advection_ef_zyx] ..."
  ENDIF

  CALL advection_ef_z( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       iadvs, iadve, jadvs, jadve, dtadv )

  CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       iadvs, iadve, jstart, jend, 1, ke, dtadv )

  CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, 1, ke, dtadv )

END SUBROUTINE advection_ef_zyx

!==============================================================================
!==============================================================================

SUBROUTINE advection_ef_zyxyz( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,  &
                               xadv_rk,     yadv_rk,     zadv_rk )
  
  ! Subroutine arguments:
  ! ---------------------
  EXTERNAL xadv_rk_cri, yadv_rk_cri, zadv_rk_cri, xadv_rk, yadv_rk, zadv_rk
  
  REAL   (KIND=wp)        :: dtadv2
  INTEGER(KIND=iintegers) :: kbegin, kend

  IF (izdebug > 10) THEN
    WRITE(*,*) "Subr. [advection_ef_zyxyz] ..."
  ENDIF

  dtadv2 = 0.5_wp * dtadv


  CALL advection_ef_z( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       iadvs, iadve, jadvs, jadve, dtadv2 ) 

  CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       iadvs, iadve, jstart, jend, 1, ke, dtadv2 ) 

  CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, 1, ke, dtadv ) 

  !----------------------------------------------------------------------------
  ! Boundary exchange for all related tracer variables and tke
  !----------------------------------------------------------------------------

  CALL exchange_runge_kutta_3dstrang

  CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       iadvs, iadve, jstart, jend, 1, ke, dtadv2 ) 

  CALL advection_ef_z( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, dtadv2 ) 

END SUBROUTINE advection_ef_zyxyz

!==============================================================================
!==============================================================================

SUBROUTINE advection_ef_xyzyx( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,  &
                               xadv_rk,     yadv_rk,     zadv_rk )
  
! New  Subroutine
! to test whether the new order x-y-2z-y-x is running stable

  ! Subroutine arguments:
  ! ---------------------
  EXTERNAL xadv_rk_cri, yadv_rk_cri, zadv_rk_cri, xadv_rk, yadv_rk, zadv_rk
  
  REAL   (KIND=wp)        :: dtadv2
  INTEGER(KIND=iintegers) :: kbegin, kend

  IF (izdebug > 10) THEN
    WRITE(*,*) "Subr. [advection_ef_xyzyx] ..."
  ENDIF

  dtadv2 = 0.5_wp * dtadv


  CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jadvs, jadve, 1, ke, dtadv2 ) 

  CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, 1, ke, dtadv2 ) 

  CALL advection_ef_z( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, dtadv ) 

  !----------------------------------------------------------------------------
  ! Boundary exchange for all related tracer variables and tke
  !----------------------------------------------------------------------------

  CALL exchange_runge_kutta_3dstrang

  CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       iadvs, iadve, jstart, jend, 1, ke, dtadv2 ) 

  CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, 1, ke, dtadv2 ) 

END SUBROUTINE advection_ef_xyzyx

!==============================================================================
!==============================================================================

SUBROUTINE advection_ef_xyzyx_new( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,  &
                               xadv_rk, yadv_rk, zadv_rk, calling_seq_xzy )
  
! New  Subroutine
! not yet fully adapted

  ! Subroutine arguments:
  ! ---------------------
  EXTERNAL xadv_rk_cri, yadv_rk_cri, zadv_rk_cri, xadv_rk, yadv_rk, zadv_rk
 
  LOGICAL, INTENT(in) :: calling_seq_xzy
 
  REAL   (KIND=wp)        :: dtadv2
  INTEGER(KIND=iintegers) :: k_offset

  IF (izdebug > 10) THEN
    WRITE(*,*) "Subr. [advection_ef_xyzyx_new] ..."
  ENDIF

  dtadv2 = 0.5_wp * dtadv

  k_offset = 5  ! Strang-splitting only in the lowest k_offset levels


  IF ( calling_seq_xzy ) THEN
    CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,      &
                         xadv_rk,     yadv_rk,     zadv_rk,          &
                         istart, iend, jadvs, jadve,                 &
                         1, ke-k_offset, dtadv ) 
  ELSE
    CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,      &
                         xadv_rk,     yadv_rk,     zadv_rk,          &
                         iadvs, iadve, jstart, jend,                 &
                         1, ke-k_offset, dtadv ) 
  END IF

  ! Strang-splitting in the lowest k_offset levels:
  CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jadvs, jadve,                   &
                       ke-k_offset+1, ke, dtadv2 ) 

  CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend,                   &
                       ke-k_offset+1, ke, dtadv2 ) 


  CALL advection_ef_z( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend, dtadv ) 

  !----------------------------------------------------------------------------
  ! Boundary exchange for all related tracer variables and tke
  !----------------------------------------------------------------------------

  CALL exchange_runge_kutta_3dstrang
  !MB: this exchange is not yet very efficient, because only the lowest
  ! k_offset levels must be exchanged

  IF ( calling_seq_xzy ) THEN
    CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,      &
                         xadv_rk,     yadv_rk,     zadv_rk,          &
                         istart, iend, jstart, jend,                 &
                         1, ke-k_offset, dtadv ) 
  ELSE
    CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,      &
                         xadv_rk,     yadv_rk,     zadv_rk,          &
                         istart, iend, jstart, jend,                 &
                         1, ke-k_offset, dtadv ) 
  END IF

  ! Strang-splitting in the lowest k_offset levels:
  CALL advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       iadvs, iadve, jstart, jend,                   &
                       ke-k_offset+1, ke, dtadv2 ) 

  CALL advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,        &
                       xadv_rk,     yadv_rk,     zadv_rk,            &
                       istart, iend, jstart, jend,                   &
                       ke-k_offset+1, ke, dtadv2 ) 

END SUBROUTINE advection_ef_xyzyx_new

!==============================================================================
!==============================================================================

SUBROUTINE exchange_runge_kutta_3dstrang

INTEGER (KIND=iintegers) :: my_ns_neigh(4)
CHARACTER(LEN=25)        :: yzroutine='exchange_rk_3dstrang'

! Set my_ns_neigh using cartesian neighbours but delete E-W neighbours

my_ns_neigh(:) = my_cart_neigh(:)
my_ns_neigh(1) = -1_iintegers      ! Remove left neighbour
my_ns_neigh(3) = -1_iintegers      ! Remove right neighbour

  ! loop over tracers
  DO iztrcr = 1, trcr_get_ntrcr()

    ! if advection has been done for this tracer, update halo
    IF ( izadv(iztrcr) == T_ADV_ON ) THEN

      ! get pointer to tracer (at nnew)
      CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
      IF (izerror /= 0) THEN
        yzerrmsg = trcr_errorstr(izerror)
        CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
      ENDIF

      ! halo-updated
      kzdims(1:24)=(/ke,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      CALL exchg_boundaries                                                   &
        (60+nnew, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,   &
         ie, je, kzdims, jstartpar, jendpar,                                  &
         nbl_exchg, nboundlines, my_ns_neigh, lperi_x, lperi_y, l2dim,        &
         20000+nexch_tag, ldatatypes, ncomm_type, izerror, yzerrmsg,          &
         ztrcr(:,:,:) )
    ENDIF

  ENDDO

  kzdims(1:24)=(/ke,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  CALL exchg_boundaries                                                       &
    (60+nnew, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,       &
     ie, je, kzdims, jstartpar, jendpar,                                      &
     nbl_exchg, nboundlines, my_ns_neigh, lperi_x, lperi_y, l2dim,            &
     20000+nexch_tag, ldatatypes, ncomm_type, izerror, yzerrmsg,              &
     qrs(:,:,:) )

  IF ( lcalrho_advprog) THEN
    kzdims(1:24)=(/ke,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
    CALL exchg_boundaries                                                 &
      (0, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,       &
       ie, je, kzdims, jstartpar, jendpar,                                &
       nbl_exchg, nboundlines, my_ns_neigh, lperi_x, lperi_y, l2dim,      &
       20000+nexch_tag, .FALSE., ncomm_type, izerror, yzerrmsg,           &
       rho(:,:,:) )
  END IF

  IF ( lprog_tke .AND. lalloc_tke ) THEN
    kzdims(1:24)=(/ke1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
    CALL exchg_boundaries                                                 &
      (0, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,       &
       ie, je, kzdims, jstartpar, jendpar,                                &
       nbl_exchg, nboundlines, my_ns_neigh, lperi_x, lperi_y, l2dim,      &
       20000+nexch_tag, .FALSE., ncomm_type, izerror, yzerrmsg,           &
       tke(:,:,:,nnew) )
  END IF

END SUBROUTINE exchange_runge_kutta_3dstrang

!==============================================================================
!==============================================================================

SUBROUTINE advection_ef_x( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,  &
                           xadv_rk,     yadv_rk,     zadv_rk,      &
                           i_start, i_stop, j_start, j_stop,       &
                           k_start, k_stop, dtadv ) 

  !----------------------------------------------------------------------------
  !   x-advection of positive definite Variables:
  !     tracers and tke
  !----------------------------------------------------------------------------

  ! Subroutine arguments:
  ! ---------------------
  EXTERNAL xadv_rk_cri, yadv_rk_cri, zadv_rk_cri, xadv_rk, yadv_rk, zadv_rk

  INTEGER, INTENT(IN) :: i_start, i_stop
  INTEGER, INTENT(IN) :: j_start, j_stop
  INTEGER, INTENT(IN) :: k_start, k_stop
  REAL (KIND=wp),     INTENT(in) :: dtadv

  INTEGER :: kbegin, kend

  INTEGER (KIND=iintegers) :: iztrcr

  REAL (KIND=wp),     POINTER :: &
    zrtcr_new(:,:,:) => NULL()     ! tracer variable at nnew

  CHARACTER(LEN=25) :: yzroutine='advection_ef_x'

  IF (izdebug > 10) THEN
    WRITE(*,*) "Subr. [advection_ef_x] ..."
  ENDIF

  ! Some previous calculations
  DO  k = k_start, k_stop

#ifndef NECSX
    kbegin = k
    kend   = k    
#endif

    !MB:
    !DO j = j_start, j_stop
    !  DO i = istart-2, iend+1
    DO j = 1, je
      DO i = 1, ie
        ! weighted u-velocity
        zu(i,j,k) = u_adv(i,j,k) / sqrtg_r_u(i,j,k)
      ENDDO
    ENDDO

#ifdef NECSX
  ENDDO

  kbegin = k_start
  kend   = k_stop
#endif

  ! Calculate fractional transport u-velocity and
  ! integer courant numbers for courant number independent x-advection
  CALL ufrac_crint_rk  &
    ( zu(:,:,kbegin:kend), zuvw_frac(:,:,kbegin:kend),              &
    zicr(:,:,kbegin:kend), zrdx(:),                               &
    dtadv, ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend,  &
    intcr_max, sqrtg_r_s(:,:,kbegin:kend), lintcr_ne_zero )

  IF ( lintcr_ne_zero ) THEN

#ifdef NECSX
    DO  k = k_start, k_stop
#endif

      ! loop over tracer for x-advection
      DO iztrcr = 1, trcr_get_ntrcr()

        ! advection on?
        IF ( izadv(iztrcr) == T_ADV_ON ) THEN

          ! get pointer to tracer
          CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
          IF (izerror /= 0) THEN
            yzerrmsg = trcr_errorstr(izerror)
            CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
          ENDIF

          ! x-advection of tracer
          CALL xadv_rk_cri  &
            ( zuvw_frac(:,:,k), zicr(:,:,k), zrdx(:),                         &
              ztrcr(:,:,k), dtadv,                                            &
              ie, je, i_start, i_stop, j_start, j_stop,                       &
              sqrtg_r_s(:,:,k) )

        ENDIF

      ENDDO

      ! x-advection for  rho    
      IF ( lcalrho_advprog ) CALL xadv_rk_cri              &
        ( zuvw_frac(:,:,k), zicr(:,:,k), zrdx(:),          &
        rho(:,:,k), dtadv,                                 &
        ie, je, i_start, i_stop, j_start, j_stop,          &
        sqrtg_r_s(:,:,k) )

#ifdef NECSX
    ENDDO
#endif

  ELSE

    ! loop over tracers for x-advection
    DO iztrcr = 1, trcr_get_ntrcr()

      ! advection on?
      IF ( izadv(iztrcr) == T_ADV_ON  ) THEN

        ! get pointer to tracer (for nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr( izerror )
          CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
        ENDIF

        ! x-advection
        CALL xadv_rk  &
          ( zu(:,:,kbegin:kend), zrdx(:),                                     &
            ztrcr(:,:,kbegin:kend)  , dtadv,                                  &
            ie, je, ke, i_start, i_stop, j_start, j_stop,kbegin, kend,        &
            sqrtg_r_s(:,:,kbegin:kend) )

      ENDIF

    ENDDO

    ! x-advection for  rho    
    IF ( lcalrho_advprog ) CALL xadv_rk                               &
      ( zu(:,:,kbegin:kend), zrdx(:),                                 &
      rho(:,:,kbegin:kend), dtadv,                                    &
      ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend,     &
      sqrtg_r_s(:,:,kbegin:kend) )

  END IF

#ifndef NECSX
  ENDDO
#endif

  IF ( lprog_tke ) THEN

    DO  k = MAX(k_start,2), k_stop
#ifndef NECSX
      kbegin = k
      kend   = k    
#endif

      DO j = 1, je
        DO i = 1, ie
          zuv_tke(i,j,k) = 0.5_wp*( zu(i,j,k) + zu(i,j,k-1) )
        END DO
      END DO
#ifdef NECSX
    END DO

    kbegin = MAX(k_start,2)
    kend   = k_stop
#endif

    ! Calculate fractional transport u-velocity and
    ! integer courant numbers for courant number independent x-advection
    CALL ufrac_crint_rk  &
        ( zuv_tke(:,:,kbegin:kend), zuvw_frac(:,:,kbegin:kend),              &
          zicr(:,:,kbegin:kend), zrdx(:),                                    &
          dtadv, ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend, &
          intcr_max, sqrtg_r_w(:,:,kbegin:kend), lintcr_ne_zero )

    IF ( lintcr_ne_zero ) THEN
      ! x-advection for  tke
#ifdef NECSX
      DO  k = MAX(k_start,2), k_stop
#endif
        CALL xadv_rk_cri                                              &
           ( zuvw_frac(:,:,k), zicr(:,:,k), zrdx(:),                  &
             tke(:,:,k,nnew), dtadv,                                  &
             ie, je, i_start, i_stop, j_start, j_stop,                &
             sqrtg_r_w(:,:,k) )
#ifdef NECSX
      ENDDO
#endif

    ELSE
      ! x-advection for  tke
      CALL xadv_rk                                                        &
          ( zuv_tke(:,:,kbegin:kend), zrdx(:),                            &
            tke(:,:,kbegin:kend,nnew), dtadv,                             &
            ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend,   &
            sqrtg_r_w(:,:,kbegin:kend) )
    END IF
#ifndef NECSX
   ENDDO
#endif

  END IF


END SUBROUTINE advection_ef_x

!==============================================================================
!==============================================================================

SUBROUTINE advection_ef_y( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,  &
                           xadv_rk,     yadv_rk,     zadv_rk,      &
                           i_start, i_stop, j_start, j_stop,       &
                           k_start, k_stop, dtadv )

  !----------------------------------------------------------------------------
  ! Part B: y-advection of positive definite Variables:
  !         tracers and tke
  !----------------------------------------------------------------------------

  ! Subroutine arguments:
  ! ---------------------
  EXTERNAL xadv_rk_cri, yadv_rk_cri, zadv_rk_cri, xadv_rk, yadv_rk, zadv_rk

  INTEGER, INTENT(IN) :: i_start, i_stop
  INTEGER, INTENT(IN) :: j_start, j_stop
  INTEGER, INTENT(IN) :: k_start, k_stop
  REAL (KIND=wp),     INTENT(in) :: dtadv

  INTEGER :: kbegin, kend
  INTEGER (KIND=iintegers) :: iztrcr

  REAL (KIND=wp),     POINTER :: &
    zrtcr_new(:,:,:) => NULL()    ! tracer variable at tlev=nnew

  CHARACTER(LEN=25) :: yzroutine='advection_ef_y'

  IF (izdebug > 10) THEN
    WRITE(*,*) "Subr. [advection_ef_y] ..."
  ENDIF

  ! Some previous calculations
  DO  k = k_start, k_stop

#ifndef NECSX
    kbegin = k
    kend  = k
#endif

    !MB:
    !DO j = jstart-2, jend+1
    !  DO i = istart, iend

    DO j = 1, je
      DO i = 1, ie
        ! weighted v-velocity
        zv(i,j,k) = crlat(j,2) * v_adv(i,j,k) / sqrtg_r_v(i,j,k)
      ENDDO
    ENDDO

#ifdef NECSX
  ENDDO

  kbegin = k_start
  kend   = k_stop
#endif


  ! Calculate fractional transport v-velocity and
  ! integer courant numbers for courant number independent y-advection
  CALL vfrac_crint_rk  &
      ( zv(:,:,kbegin:kend), zuvw_frac(:,:,kbegin:kend),              &
        zicr(:,:,kbegin:kend), zrdy(:,:),                             &
        dtadv, ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend,  &
        intcr_max, sqrtg_r_s(:,:,kbegin:kend), lintcr_ne_zero )

  IF ( lintcr_ne_zero ) THEN
      
#ifdef NECSX
    DO  k = k_start, k_stop
#endif

      ! loop over tracers for y-advection
      DO iztrcr = 1, trcr_get_ntrcr()

        ! advection on?
        IF ( izadv(iztrcr) == T_ADV_ON ) THEN

          ! get pointer to tracer (at nnew)
          CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
          IF (izerror /= 0) THEN
            yzerrmsg = trcr_errorstr(izerror)
            CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
          ENDIF

          ! y-advection
          CALL yadv_rk_cri  &
            ( zuvw_frac(:,:,k),zicr(:,:,k), zrdy(:,:),                        &
              ztrcr(:,:,k),dtadv,                                             &
              ie, je, i_start, i_stop, j_start, j_stop,                       &
              sqrtg_r_s(:,:,k) )

        ENDIF

      ENDDO

      ! y-advection for  rho    
      IF ( lcalrho_advprog ) CALL yadv_rk_cri                          &
        ( zuvw_frac(:,:,k), zicr(:,:,k), zrdy(:,:),                    &
          rho(:,:,k), dtadv,                                           &
          ie, je, i_start, i_stop, j_start, j_stop,                    &
          sqrtg_r_s(:,:,k) )

#ifdef NECSX
    ENDDO
#endif

  ELSE

    ! loop over tracers for y-advection
    DO iztrcr = 1, trcr_get_ntrcr()

      ! advection on?
      IF ( izadv(iztrcr) == T_ADV_ON ) THEN

        ! get pointer to tracer (at nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr(izerror)
          CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
        ENDIF

        ! y-advection
        CALL yadv_rk                                                          &
          ( zv(:,:,kbegin:kend), zrdy(:,:),                                   &
            ztrcr(:,:,kbegin:kend),dtadv,                                     &
            ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend,       &
            sqrtg_r_s(:,:,kbegin:kend) )

      ENDIF

    ENDDO

    ! y-advection for  rho
    IF ( lcalrho_advprog ) CALL yadv_rk                                &
      ( zv(:,:,kbegin:kend), zrdy(:,:),                                &
        rho(:,:,kbegin:kend), dtadv,                                   &
        ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend,    &
        sqrtg_r_s(:,:,kbegin:kend) )

  END IF

#ifndef NECSX
  ENDDO
#endif

  IF ( lprog_tke ) THEN

    DO  k = MAX(k_start,2), k_stop
#ifndef NECSX
      kbegin = k
      kend  = k
#endif

      DO j = 1, je
        DO i = 1, ie
          zuv_tke(i,j,k) = 0.5_wp*( zv(i,j,k) + zv(i,j,k-1) )
        END DO
      END DO
#ifdef NECSX
    END DO

    kbegin = MAX(k_start,2)
    kend  = k_stop
#endif

    ! Calculate fractional transport v-velocity and
    ! integer courant numbers for courant number independent y-advection
    CALL vfrac_crint_rk   &
      ( zuv_tke(:,:,kbegin:kend), zuvw_frac(:,:,kbegin:kend),              &
        zicr(:,:,kbegin:kend), zrdy(:,:),                                  &
        dtadv, ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend, &
        intcr_max, sqrtg_r_w(:,:,kbegin:kend), lintcr_ne_zero )

    IF ( lintcr_ne_zero ) THEN
      ! y-advection for  tke
#ifdef NECSX
      DO  k = MAX(k_start,2), k_stop
#endif
        CALL yadv_rk_cri                                                   &
          ( zuvw_frac(:,:,k), zicr(:,:,k), zrdy(:,:),                      &
            tke(:,:,k,nnew), dtadv,                                        &
            ie, je, i_start, i_stop, j_start, j_stop,                      &
            sqrtg_r_w(:,:,k) )
#ifdef NECSX
      ENDDO
#endif
    ELSE
      ! y-advection for  tke
      CALL yadv_rk                                                         &
          ( zuv_tke(:,:,kbegin:kend), zrdy(:,:),                           &
            tke(:,:,kbegin:kend,nnew), dtadv,                              &
            ie, je, ke, i_start, i_stop, j_start, j_stop, kbegin, kend,    &
            sqrtg_r_w(:,:,kbegin:kend) )
    END IF
#ifndef NECSX
   ENDDO
#endif
  END IF

END SUBROUTINE advection_ef_y

!==============================================================================
!==============================================================================

SUBROUTINE advection_ef_z( xadv_rk_cri, yadv_rk_cri, zadv_rk_cri,  &
                           xadv_rk,     yadv_rk,     zadv_rk,      &
                           i_start, i_stop, j_start, j_stop, dtadv )

  !----------------------------------------------------------------------------
  ! Part C: vertical advection of positive definite Variables:
  !         tracers and tke
  !----------------------------------------------------------------------------

  ! Subroutine arguments:
  ! ---------------------
  EXTERNAL xadv_rk_cri, yadv_rk_cri, zadv_rk_cri, xadv_rk, yadv_rk, zadv_rk

  INTEGER, INTENT(in) :: i_start, i_stop, j_start, j_stop
  REAL (KIND=wp),     INTENT(in) :: dtadv

  INTEGER :: kbegin, kend
  INTEGER (KIND=iintegers) :: iztrcr

  REAL (KIND=wp),     POINTER :: &
    zrtcr_new(:,:,:) => NULL()     ! tracer variable at nnew

  CHARACTER(LEN=25) :: yzroutine = 'advection_ef_z'

  IF (izdebug > 10) THEN
    WRITE(*,*) "Subr. [advection_ef_z] ..."
  ENDIF

  ! Calculate fractional contravariant vertical transport velocity and
  ! integer courant numbers for courant number independent vertical advection
  CALL wcfrac_crint_rk  &
    ( wcon(:,:,:), zuvw_frac(:,:,:), zicr(:,:,:), dtadv,       &
      ie, je, ke, ke1, i_start, i_stop, j_start, j_stop,       &
      sqrtg_r_s(:,:,:), lintcr_ne_zero, ivl_off, num_compute,  &
      icomm_cart, imp_integers )

  ! Calculation of positive definite vertical advection
  ! tracer variables and tke
  !-----------------------------------------------------

  IF ( lintcr_ne_zero ) THEN

    ! loop over tracers for z-advection
    DO iztrcr = 1, trcr_get_ntrcr() 

      ! advection on?
      IF ( izadv(iztrcr) == T_ADV_ON ) THEN

        ! get pointer to tracer (at nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr(izerror)
          CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
        ENDIF

        ! z-advection
        CALL zadv_rk_cri                                                      &
          ( zuvw_frac(:,:,:), zicr(:,:,:),                                    &
            ztrcr(:,:,:), dtadv,                                              &
            ie, je, ke, ke1, i_start, i_stop, j_start, j_stop,                &
            sqrtg_r_s(:,:,:) )

      ENDIF

    ENDDO

    IF ( lcalrho_advprog ) THEN
     CALL zadv_rk_cri                                              &
        ( zuvw_frac(:,:,:), zicr(:,:,:),                           &
          rho(:,:,:), dtadv,                                       &
          ie, je, ke, ke1, i_start, i_stop, j_start, j_stop,       &
          sqrtg_r_s(:,:,:) )
    ENDIF

  ELSE

    ! loop over tracers for z-advection
    DO iztrcr = 1, trcr_get_ntrcr()

      ! advection on?
      IF ( izadv(iztrcr) == T_ADV_ON ) THEN

        ! get pointer to tracer (at nnew)
        CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
        IF (izerror /= 0) THEN
          yzerrmsg = trcr_errorstr(izerror)
          CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
        ENDIF

        ! z-advection
        CALL zadv_rk  &
          ( wcon(:,:,:),                                                      &
            ztrcr(:,:,:), dtadv,                                              &
            ie, je, ke, ke1, i_start, i_stop, j_start, j_stop,                &
            sqrtg_r_s(:,:,:) )

      ENDIF

    ENDDO

    IF ( lcalrho_advprog ) THEN
      CALL zadv_rk                                                 &
        & ( wcon(:,:,:),                                           &
        &   rho(:,:,:), dtadv,                                     &
        &   ie, je, ke, ke1, i_start, i_stop, j_start, j_stop,     &
        &   sqrtg_r_s(:,:,:) )
    ENDIF

  END IF

  IF ( lprog_tke ) THEN
    ! Calculate fractional contravariant vertical transport velocity and
    ! integer courant numbers for courant number independent vertical advec.
    CALL wcfrac_crint_rk  &
        ( zwc_tke(:,:,:), zuvw_frac(:,:,:), zicr(:,:,:), dtadv,       &
          ie, je, ke, ke1, i_start, i_stop, j_start, j_stop,          &
          sqrtg_r_w(:,:,1:ke), lintcr_ne_zero, ivl_off, num_compute,  &
          icomm_cart, imp_integers )
    IF ( lintcr_ne_zero ) THEN
      ! vertical advection for  tke
      CALL zadv_rk_cri                                                &
          ( zuvw_frac(:,:,:), zicr(:,:,:),                            &
            tke(:,:,1:ke,nnew), dtadv,                                &
            ie, je, ke, ke1, i_start, i_stop, j_start, j_stop,        &
            sqrtg_r_w(:,:,1:ke) )
    ELSE
      ! vertical advection for  tke
      CALL zadv_rk                                                    &
          ( zwc_tke(:,:,:),                                           &
            tke(:,:,1:ke,nnew), dtadv,                                &
            ie, je, ke, ke1, i_start, i_stop, j_start, j_stop,        &
            sqrtg_r_w(:,:,1:ke) )
    END IF
  END IF

END SUBROUTINE advection_ef_z

!==============================================================================
!==============================================================================

SUBROUTINE western_boundary

  ! west
  IF (my_cart_neigh(1) == -1) THEN
    IF ( lcalrho_advprog ) THEN
      DO i = istart-1, 1, -1
!CDIR NOLOOPCHG,COLLAPSE
        rho(i,:,:) = rho(istart,:,:)
      END DO
    END IF
    IF ( lprog_tke ) THEN
      DO i = istart-1, 1, -1
!CDIR NOLOOPCHG,COLLAPSE
        tke(i,:,:,nnew) = tke(istart,:,:,nnew)
      END DO
    END IF
  END IF

END SUBROUTINE western_boundary

!==============================================================================

SUBROUTINE eastern_boundary

  ! east
  IF (my_cart_neigh(3) == -1) THEN
    IF ( lcalrho_advprog ) THEN
      DO i = iend+1, ie
!CDIR NOLOOPCHG,COLLAPSE
        rho(i,:,:) = rho(iend,:,:)
      END DO
    END IF
    IF ( lprog_tke ) THEN
      DO i = iend+1, ie
!CDIR NOLOOPCHG,COLLAPSE
        tke(i,:,:,nnew) = tke(iend,:,:,nnew)
      END DO
    END IF
  END IF

END SUBROUTINE eastern_boundary

!==============================================================================

SUBROUTINE southern_boundary

  ! south
  IF (my_cart_neigh(4) == -1) THEN
    IF ( lcalrho_advprog ) THEN
      DO j = jstart-1, 1, -1
        rho(:,j,:) = rho(:,jstart,:)
      END DO
    END IF
    IF ( lprog_tke ) THEN
      DO j = jstart-1, 1, -1
        tke(:,j,:,nnew) = tke(:,jstart,:,nnew)
      END DO
    END IF
  END IF

END SUBROUTINE southern_boundary

!==============================================================================

SUBROUTINE northern_boundary

  ! north
  IF (my_cart_neigh(2) == -1) THEN
    IF ( lcalrho_advprog ) THEN
      DO j = jend+1, je
        rho(:,j,:) = rho(:,jend,:)
      END DO
    END IF
    IF ( lprog_tke ) THEN
      DO j = jend+1, je
        tke(:,j,:,nnew) = tke(:,jend,:,nnew)
      END DO
    END IF
  END IF

END SUBROUTINE northern_boundary

!==============================================================================
!+ Module procedure lateral_boundaries_zerograd for zero gradient lat BC.
!------------------------------------------------------------------------------

SUBROUTINE lateral_boundaries_zerograd

!------------------------------------------------------------------------------
!
! Description:
!   This module procedure defines the lateral boundary conditions
!   in case of zero gradient.
!
! Method:
!   This subroutine uses the methods of the current subroutines eastern_/
!   southern_/western_/northern_boundaries and applies them to the tracers.
!
!   The values at the outermost points of the computation domain (whole domain)
!   are assigned to the halo points around the computation domain.
!------------------------------------------------------------------------------

! Declarations:


! Local parameters:
!-------------------

  INTEGER (KIND=iintegers)   :: iztrcr
  CHARACTER(LEN=25)          :: yzroutine = 'lat_bd_zerograd'
  REAL (KIND=wp),    POINTER :: &
    ztrcr(:,:,:) => NULL()   ! tracer variable at nnew

  ! loop over tracers 
  DO iztrcr = 1, trcr_get_ntrcr()

    ! check if zero-grad BCs are needed for this tracer
    IF (izadv(iztrcr) == T_ADV_ON .AND.                                        &
       (izlbc(iztrcr) == T_LBC_ZEROGRAD .OR. (lz0grad_forced(iztrcr) .AND.     &
       (izlbc(iztrcr) == T_LBC_ZERO)))) THEN

      ! get pointer to tracer (at nnew)
      CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
      IF (izerror /= 0) THEN
        yzerrmsg = trcr_errorstr(izerror)
        CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
      ENDIF

      ! Treatment of the western boundary
      IF (my_cart_neigh(1) == -1) THEN
        DO i = istart-1, 1, -1
          !CDIR NOLOOPCHG,COLLAPSE
          ztrcr(i,:,:) = ztrcr(istart,:,:)
        END DO
      END IF

      ! Treatment of the eastern boundary
      IF (my_cart_neigh(3) == -1) THEN
        DO i = iend+1, ie
          !CDIR NOLOOPCHG,COLLAPSE
          ztrcr(i,:,:) = ztrcr(iend,:,:)
        END DO
      END IF

      ! Treatment of the southern boundary
      IF (my_cart_neigh(4) == -1) THEN
        DO j = jstart-1, 1, -1
          !CDIR NOLOOPCHG,COLLAPSE
          ztrcr(:,j,:) = ztrcr(:,jstart,:)
        END DO
      END IF

      ! Treatment of the northern boundary
      IF (my_cart_neigh(2) == -1) THEN
        DO j = jend+1, je
          !CDIR NOLOOPCHG,COLLAPSE
          ztrcr(:,j,:) = ztrcr(:,jend,:)
        END DO
      END IF

    END IF

  ENDDO

!------------------------------------------------------------------------------
! End of module procedure "lateral_boundaries_zerograd"
!------------------------------------------------------------------------------

END SUBROUTINE lateral_boundaries_zerograd

!==============================================================================

END SUBROUTINE advection_pd

!==============================================================================
!==============================================================================

SUBROUTINE advection_semi_lagrange_init

!--------------------------------------------------------------------------
!
! Description:
!   Initializations and consistency checks for
!   subroutine advection_semi_lagrange
!
!--------------------------------------------------------------------------

USE data_runcontrol, ONLY:  idbg_level, l2tls

  CHARACTER(LEN=80) :: yzerrmsg

   IF ( ( idbg_level >= 10 ) .AND. ( my_cart_id==0 ) ) THEN
    WRITE(*,*) "[Subr. advection_semi_lagrange_init ...]"
  END IF

  i_clipping_type = 4  ! default value
    ! =0 no clipping
    ! =1 simple clipping of negative values (produces a lot of mass)
    ! =3 'selective filling diffusion' (better local conservation)
    ! =4 'multiplicative filling' (only nearly global conservation)

  SELECT CASE ( TRIM(y_scalar_advect) )
  CASE( "SL3_MF" )
    i_clipping_type = 4
  CASE( "SL3_SFD" )
    i_clipping_type = 3
  END SELECT

  IF ( ltrcr_trilin ) THEN
    i_clipping_type = 1 ! only simple clipping
    ! i_clipping_type = 0 ! even no clipping should work
  END IF

  IF ( ( idbg_level >= 10 ) .AND. ( my_cart_id==0 ) ) THEN
    WRITE(*,*) "i_clipping_type=", i_clipping_type
  END IF


  ! Diffusion-method or -sequence for 'selective filling diffusion':
  y_SL_diffus_type = "zyx"
  !y_SL_diffus_type = "zxy"

  IF ( l2tls .AND. ( .NOT. ltrcr_trilin )     &
    &        .AND. ( i_clipping_type==3 )     &
    &        .AND. ( nboundlines < 2 )      ) THEN
    yzerrmsg = "nboundlines >= 2 is required for selective filling diffusion"
    CALL model_abort (my_cart_id, 100, yzerrmsg, 'advection_semi_lagrange_init')
  END IF

END SUBROUTINE advection_semi_lagrange_init

!==============================================================================

SUBROUTINE advection_semi_lagrange( u_adv, v_adv, wcon, zwc_tke )

!--------------------------------------------------------------------------
!
! Description:
! calculate 3D semi-Lagrangian advection of scalar variables
!
!--------------------------------------------------------------------------

  IMPLICIT NONE

  ! Declarations:

  REAL (KIND=wp),     INTENT(in) ::  &
    u_adv(ie,je,ke),    & ! advection velocities
    v_adv(ie,je,ke)

  REAL (KIND=wp),     INTENT(in) ::  &
    wcon   (ie,je,ke1),  & ! backtraj_trilin_... sets wcon(:,:,ke+1)=0 !
    zwc_tke(ie,je,ke1)     ! backtraj_trilin_... sets zwc_tke(:,:,ke+1)=0 !
                           ! (therefore: copy these fields into zwcon1, zwcon2)
  ! Local scalars:
  ! -------------
  INTEGER (KIND=iintegers) ::  &
    iztrcr,              & !
    km1,                 & !
    izstata,             & !  error status at allocation
    izstatd,             & !  error status at deallocation
    isp

  ! Local automatic arrays:
  !-----------------------
  INTEGER (KIND=iintegers) :: &
    izadv(trcr_get_ntrcr()) , & !
    izclip(trcr_get_ntrcr())

  ! Local (allocatable) arrays:
  ! ------------
  INTEGER (KIND=iintegers), ALLOCATABLE ::  &
    btrj_idx (:,:,:,:)
  REAL    (KIND=wp),        ALLOCATABLE ::  &
    btrj_wght(:,:,:,:)
  REAL    (KIND=wp),        ALLOCATABLE ::  &
    zu(:,:,:), zv(:,:,:), zwcon1(:,:,:), zwcon2(:,:,:) 

  INTEGER (KIND=iintegers) :: i, j, k
  INTEGER (KIND=iintegers) :: izerror

  INTEGER (KIND=iintegers) ::  &
    kzdims(24)             !  vertical dimensions for exchg_boundaries

  CHARACTER(LEN=80) :: yzerrmsg
  CHARACTER(LEN=25) :: yzroutine = 'advection_sl'

  ! Tracer pointers:
  !-----------------
  REAL (KIND=wp),     POINTER     :: &
    ztrcr    (:,:,:) => NULL(), &  ! tracer variable at tlev=nnew
    ztrcr_now(:,:,:) => NULL()     ! tracer variable at tlev=nnow

!- End of header
!==============================================================================

  ALLOCATE ( btrj_wght(ie, je, ke, 3), STAT=izstata )
  ALLOCATE ( btrj_idx (ie, je, ke, 3), STAT=izstata )
  ALLOCATE ( zu(ie, je, ke), STAT=izstata )
  ALLOCATE ( zv(ie, je, ke), STAT=izstata )
  ALLOCATE ( zwcon1(ie, je, ke1), STAT=izstata )
  ALLOCATE ( zwcon2(ie, je, ke1), STAT=izstata )

!ROA REMARK: this initialization is required in order to get
!            the same results using the tracer module for the
!            microphysics species and using the original version.
!            This is strange because it means that the edges (where
!            the weights are not calculated) are used in some way
!            in the interpolation.
!            There might be a bug here...
  btrj_wght(:,:,:,:) = 0.0_wp
  btrj_idx (:,:,:,:) = 0_iintegers

  zwcon1(:,:,:) = wcon(:,:,:)
  zwcon2(:,:,:) = zwc_tke(:,:,:)

  ! Retrieve the required metadata
  CALL trcr_meta_get(izerror, T_ADV_ID, izadv)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF
  CALL trcr_meta_get(izerror, T_CLP_ID, izclip)
  IF (izerror /= 0) THEN
    yzerrmsg = trcr_errorstr(izerror)
    CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
  ENDIF

  ! calculate transformed velocity components
  DO k=1, ke
    DO j=1, je
      DO i=1, ie
        zu(i,j,k) = acrlat(j,1) * u_adv(i,j,k)
        zv(i,j,k) = v_adv(i,j,k) / r_earth
      ENDDO
    ENDDO
  ENDDO

  ! calculate backward trajectories
  ! -------------------------------
  CALL backtraj_trilin_dt2_3tl( zu, zv, zwcon1,                            &
                                eddlon, eddlat, 1.0_wp, 0.5_wp*dt, &
                                ie, je, ke, istart, iend, jstart, jend,    &
                                btrj_idx, btrj_wght )

  ! treat the cases, where backward trajectory runs outside of the domain:
  CALL postprocess_backtraj( btrj_idx, btrj_wght, ie, je, ke,  &
                       istart, iend, jstart, jend )


  ! in the case of 'selective filling diffusion'
  ! choose diffusion type and/or alternate the calling sequence:

  IF ( y_SL_diffus_type == "xyz" ) THEN
    ! alternate sequence:
    y_SL_diffus_type = "zyx"
  ELSE IF ( y_SL_diffus_type == "zyx" ) THEN
    ! alternate sequence:
    y_SL_diffus_type = "xyz"
  END IF


  ! perform interpolation step of SL-scheme:
  ! ----------------------------------------

  ! Loop over tracers
  DO iztrcr = 1, trcr_get_ntrcr()

    ! get pointer to tracer (at nnew)
    CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr)
    IF (izerror /= 0) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF

    ! get pointer to tracer (at nnew)
#ifndef MESSY
    CALL trcr_get(izerror, iztrcr, ptr_tlev=nnow, ptr=ztrcr_now)
#else
      ! Necessary due to different treatment of tendencies /time splitting
      CALL trcr_get(izerror, iztrcr, ptr_tlev=nnew, ptr=ztrcr_now)
#endif
    IF (izerror /= 0) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF

    ! advection on?
    IF ( izadv(iztrcr) == T_ADV_ON ) THEN

      IF ( .NOT. ltrcr_trilin ) THEN
        !tricubic interp. of tracer
        CALL interpol_sl_tricubic ( ztrcr_now(:,:,:), ztrcr(:,:,:),           &
                                    btrj_idx, btrj_wght, dt, ie, je, ke,      &
                                    istart, iend, jstart, jend )
      ELSE
        !trilinear interp. of tracer
        CALL interpol_sl_trilin   ( ztrcr_now(:,:,:), ztrcr(:,:,:),           &
                                    btrj_idx, btrj_wght, dt, ie, je, ke,      &
                                    istart, iend, jstart, jend )
      ENDIF

      ! test for each tracer if clipping ensuring positive definiteness
      ! is required (this clipping ensures global conservation)
      IF ( izclip(iztrcr) == T_CLP_ON ) THEN

        ! perform a halo-update for type=3 clipping
        IF (i_clipping_type == 3 ) THEN
          kzdims(1:24) =                                                      &
           (/ke,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
          CALL exchg_boundaries                                               &
          ( 0, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,      &
            ie, je, kzdims, jstartpar, jendpar, nboundlines, nboundlines,     &
            my_cart_neigh, lperi_x, lperi_y, l2dim, 20000+nexch_tag, .FALSE., &
            ncomm_type, izerror, yzerrmsg, ztrcr(:,:,:) )
        ENDIF

        CALL remove_negative_values(ztrcr(:,:,:), ie, je, ke, istart,         &
                         iend, jstart, jend, i_clipping_type, y_SL_diffus_type)

      ENDIF

#ifndef MESSY
      ! Necessary due to different treatment of tendencies /time splitting
    ELSE

      ! if advection is switched of for this tracer, simply copy nnow to nnew
      ztrcr(:,:,:) = ztrcr_now(:,:,:)
#endif

    END IF

  ENDDO

  ! analogous for the advection of TKE (placed at the w-position)
  ! -------------------------------------------------------------

  IF ( lprog_tke ) THEN

    ! calculate transformed velocity components
    DO k=1, ke
      km1 = MAX( 1, k-1 )
      DO j=1, je
        DO i=1, ie
          zu(i,j,k) = 0.5_wp*( u_adv(i,j,k) + u_adv(i,j,km1) )  &
                    * acrlat(j,1)
          zv(i,j,k) = 0.5_wp*( v_adv(i,j,k) + v_adv(i,j,km1) )  &
                    / r_earth
        ENDDO
      ENDDO
    ENDDO
    ! calculate backtrajectories
    CALL backtraj_trilin_dt2_3tl( zu, zv, zwcon2,                            &
                                  eddlon, eddlat, 1.0_wp, 0.5_wp*dt, &
                                  ie, je, ke, istart, iend, jstart, jend,    &
                                  btrj_idx, btrj_wght )

    ! calculate 3D semi-Lagrangian advection of tke
    CALL interpol_sl_tricubic( tke(:,:,1:ke,nnow), tke(:,:,1:ke,nnew),       &
                                  btrj_idx, btrj_wght, dt, ie, je, ke,       &
                                  istart, iend, jstart, jend )

    IF ( i_clipping_type == 3 ) THEN
    ! for the 'selective filling diffusion' an exchange of data is needed:

      kzdims(1:24) =                                          &
         (/ke1,0,0,0,0,0,0,0,0,0,0,0,                         &
           0,0,0,0,0,0,0,0,0,0,0,0/)
      CALL exchg_boundaries                                                   &
       ( 0, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute, ie, je, &
        kzdims, jstartpar, jendpar, nboundlines, nboundlines, my_cart_neigh,  &
        lperi_x, lperi_y, l2dim,                                              &
        20000+nexch_tag, .FALSE., ncomm_type, izerror, yzerrmsg,              &
        tke(:,:,:,nnew) )

    END IF

    CALL remove_negative_values( tke(:,:,1:ke,nnew), ie, je, ke,  &
               istart, iend, jstart, jend, i_clipping_type, y_SL_diffus_type)

  END IF


  DEALLOCATE( btrj_idx, btrj_wght, STAT=izstatd )
  DEALLOCATE( zu, zv, STAT=izstatd )
  DEALLOCATE( zwcon1, zwcon2, STAT=izstatd )

END SUBROUTINE advection_semi_lagrange

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind1_lon( q, dlam_dt, tend, ssign,          &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
!
! Method:
!   upwind formula of 1st order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(IN)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(IN)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(IN)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(INOUT) :: tend
  REAL (KIND=wp),     INTENT(IN) :: ssign
  INTEGER,          INTENT(IN)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(IN)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k

  REAL (KIND=wp)     :: const1, cd_2order, diff_2order, advtend

  const1 = 0.5_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order = dlam_dt(i,j,k) * ( q(i+1,j,k) - q(i-1,j,k) )

          diff_2order = ABS( dlam_dt(i,j,k) ) *           &
            &    ( q(i+1,j,k) - 2.0_wp * q(i,j,k) + q(i-1,j,k) )

          advtend = - const1 * ( cd_2order - ssign*diff_2order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO

  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order = dlam_dt(i,j,k) * ( q(i+1,j,k) - q(i-1,j,k) )

          diff_2order = ABS( dlam_dt(i,j,k) ) *           &
            &    ( q(i+1,j,k) - 2.0_wp * q(i,j,k) + q(i-1,j,k) )

          tend(i,j,k) = tend(i,j,k) - const1 * ( cd_2order - ssign*diff_2order )

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind1_lon

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind1_lat( q, dphi_dt, tend, ssign,          &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)
!   and ADD it to the field 'tend'
!
! Method:
!   upwind formula of 1st order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k

  REAL (KIND=wp)     :: const1, cd_2order, diff_2order, advtend

  const1 = 0.5_wp * eddlat

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order = dphi_dt(i,j,k) * ( q(i,j+1,k) - q(i,j-1,k) )

          diff_2order = ABS( dphi_dt(i,j,k) ) *           &
            &    ( q(i,j+1,k) - 2.0_wp * q(i,j,k) + q(i,j-1,k) )

          advtend = - const1 * ( cd_2order - ssign*diff_2order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order = dphi_dt(i,j,k) * ( q(i,j+1,k) - q(i,j-1,k) )

          diff_2order = ABS( dphi_dt(i,j,k) ) *           &
            &    ( q(i,j+1,k) - 2.0_wp * q(i,j,k) + q(i,j-1,k) )

          tend(i,j,k) = tend(i,j,k) - const1 * ( cd_2order - ssign*diff_2order )

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind1_lat

!==============================================================================
!==============================================================================

SUBROUTINE adv_centdiff2_lon( q, dlam_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
!
! Method:
!   centered difference formula of 2nd order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k

  REAL (KIND=wp)     :: const1, cd_2order, advtend

  const1 = 0.5_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order = dlam_dt(i,j,k) * ( q(i+1,j,k) - q(i-1,j,k) )

          advtend = - const1 * cd_2order

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order = dlam_dt(i,j,k) * ( q(i+1,j,k) - q(i-1,j,k) )

          tend(i,j,k) = tend(i,j,k) - const1 * cd_2order

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_centdiff2_lon

!==============================================================================
!==============================================================================

SUBROUTINE adv_centdiff2_lat( q, dphi_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)
!   and ADD it to the field 'tend'
!
! Method:
!   centered difference formula of 2nd order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k

  REAL (KIND=wp)     :: const1, cd_2order, advtend

  const1 = 0.5_wp * eddlat

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order = dphi_dt(i,j,k) * ( q(i,j+1,k) - q(i,j-1,k) )

          advtend = - const1 * cd_2order

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order = dphi_dt(i,j,k) * ( q(i,j+1,k) - q(i,j-1,k) )

          tend(i,j,k) = tend(i,j,k) - const1 * cd_2order

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_centdiff2_lat

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind3_lon( q, dlam_dt, tend, ssign,          &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
!
! Method:
!   upwind formula of 3rd order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_4order, diff_4order, advtend

  const1 = 1.0_wp / 12.0_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order = dlam_dt(i,j,k) *   ( - ( q(i+2,j,k) - q(i-2,j,k) )     &
            &                +  8.0_wp * ( q(i+1,j,k) - q(i-1,j,k) )  )

          diff_4order = ABS( dlam_dt(i,j,k) ) * ( ( q(i+2,j,k) + q(i-2,j,k) ) &
            &                      - 4.0_wp * ( q(i+1,j,k) + q(i-1,j,k) ) &
            &                      + 6.0_wp *   q(i,j,k)    )

          advtend = - const1 * ( cd_4order + ssign*diff_4order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order = dlam_dt(i,j,k) *   ( - ( q(i+2,j,k) - q(i-2,j,k) )     &
            &                +  8.0_wp * ( q(i+1,j,k) - q(i-1,j,k) )  )

          diff_4order = ABS( dlam_dt(i,j,k) ) * ( ( q(i+2,j,k) + q(i-2,j,k) ) &
            &                      - 4.0_wp * ( q(i+1,j,k) + q(i-1,j,k) ) &
            &                      + 6.0_wp *   q(i,j,k)    )

          tend(i,j,k) = tend(i,j,k) - const1 * ( cd_4order + ssign*diff_4order )

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind3_lon

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind3_lat( q, dphi_dt, tend, ssign,          &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)
!   and ADD it to the field 'tend'
!
! Method:
!   upwind formula of 3rd order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_4order, diff_4order, advtend

  const1 = 1.0_wp / 12.0_wp * eddlat

  IF (limiter) THEN
     DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order = dphi_dt(i,j,k) *   ( - ( q(i,j+2,k) - q(i,j-2,k) )     &
            &                +  8.0_wp * ( q(i,j+1,k) - q(i,j-1,k) ) )

          diff_4order = ABS( dphi_dt(i,j,k) ) * ( ( q(i,j+2,k) + q(i,j-2,k) ) &
            &                      - 4.0_wp * ( q(i,j+1,k) + q(i,j-1,k) ) &
            &                      + 6.0_wp *   q(i,j,k)    )

          advtend = - const1 * ( cd_4order + ssign*diff_4order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
 ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order = dphi_dt(i,j,k) *   ( - ( q(i,j+2,k) - q(i,j-2,k) )     &
            &                +  8.0_wp * ( q(i,j+1,k) - q(i,j-1,k) ) )

          diff_4order = ABS( dphi_dt(i,j,k) ) * ( ( q(i,j+2,k) + q(i,j-2,k) ) &
            &                      - 4.0_wp * ( q(i,j+1,k) + q(i,j-1,k) ) &
            &                      + 6.0_wp *   q(i,j,k)    )

          tend(i,j,k) = tend(i,j,k) - const1 * ( cd_4order + ssign*diff_4order )

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind3_lat

!==============================================================================
!==============================================================================

SUBROUTINE adv_centdiff4_lon( q, dlam_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
! 
! Method:
!   centered difference formula of 4th order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_4order, advtend

  const1 = 1.0_wp / 12.0_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order = dlam_dt(i,j,k) *   ( - ( q(i+2,j,k) - q(i-2,j,k) )     &
            &                +  8.0_wp * ( q(i+1,j,k) - q(i-1,j,k) )  )

          advtend = - const1 * cd_4order

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order = dlam_dt(i,j,k) *   ( - ( q(i+2,j,k) - q(i-2,j,k) )     &
            &                +  8.0_wp * ( q(i+1,j,k) - q(i-1,j,k) )  )

          tend(i,j,k) = tend(i,j,k) - const1 * cd_4order

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_centdiff4_lon

!==============================================================================
!==============================================================================

SUBROUTINE adv_centdiff4_lat( q, dphi_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)  
!   and ADD it to the field 'tend'
! 
! Method:
!   centered difference formula of 4th order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_4order, advtend

  const1 = 1.0_wp / 12.0_wp * eddlat

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order = dphi_dt(i,j,k) *   ( - ( q(i,j+2,k) - q(i,j-2,k) )     &
            &                +  8.0_wp * ( q(i,j+1,k) - q(i,j-1,k) ) )

          advtend = - const1 * cd_4order

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order = dphi_dt(i,j,k) *   ( - ( q(i,j+2,k) - q(i,j-2,k) )     &
            &                +  8.0_wp * ( q(i,j+1,k) - q(i,j-1,k) ) )

          tend(i,j,k) = tend(i,j,k) - const1 * cd_4order

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_centdiff4_lat

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind5_lon( q, dlam_dt, tend, ssign,          &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
! 
! Method:
!   upwind formula of 5th order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_6order, diff_6order, advtend

  const1 = 1.0_wp / 60.0_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
!CDIR OUTERUNROLL=8
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order = dlam_dt(i,j,k) *   ( ( q(i+3,j,k) - q(i-3,j,k) )     &
            &              -  9.0_wp * ( q(i+2,j,k) - q(i-2,j,k) )     &
            &              + 45.0_wp * ( q(i+1,j,k) - q(i-1,j,k) ) )

          diff_6order = ABS( dlam_dt(i,j,k) ) * ( ( q(i+3,j,k) + q(i-3,j,k) ) &
            &                     -  6.0_wp * ( q(i+2,j,k) + q(i-2,j,k) ) &
            &                     + 15.0_wp * ( q(i+1,j,k) + q(i-1,j,k) ) &
            &                     - 20.0_wp *   q(i,j,k)    )

          advtend = - const1 * ( cd_6order - ssign*diff_6order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
!CDIR OUTERUNROLL=8
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order = dlam_dt(i,j,k) *   ( ( q(i+3,j,k) - q(i-3,j,k) )     &
            &              -  9.0_wp * ( q(i+2,j,k) - q(i-2,j,k) )     &
            &              + 45.0_wp * ( q(i+1,j,k) - q(i-1,j,k) ) )

          diff_6order = ABS( dlam_dt(i,j,k) ) * ( ( q(i+3,j,k) + q(i-3,j,k) ) &
            &                     -  6.0_wp * ( q(i+2,j,k) + q(i-2,j,k) ) &
            &                     + 15.0_wp * ( q(i+1,j,k) + q(i-1,j,k) ) &
            &                     - 20.0_wp *   q(i,j,k)    )

          tend(i,j,k) = tend(i,j,k) - const1 * ( cd_6order - ssign*diff_6order )

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind5_lon

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind5_lat( q, dphi_dt, tend, ssign,          &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)  
!   and ADD it to the field 'tend'
! 
! Method:
!   upwind formula of 5th order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_6order, diff_6order, advtend

  const1 = 1.0_wp / 60.0_wp * eddlat

  IF (limiter) THEN
    DO k=kstart, kend
!CDIR OUTERUNROLL=8
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order = dphi_dt(i,j,k) *   ( ( q(i,j+3,k) - q(i,j-3,k) )     &
            &              -  9.0_wp * ( q(i,j+2,k) - q(i,j-2,k) )     &
            &              + 45.0_wp * ( q(i,j+1,k) - q(i,j-1,k) ) )

          diff_6order = ABS( dphi_dt(i,j,k) ) * ( ( q(i,j+3,k) + q(i,j-3,k) ) & 
            &                     -  6.0_wp * ( q(i,j+2,k) + q(i,j-2,k) ) & 
            &                     + 15.0_wp * ( q(i,j+1,k) + q(i,j-1,k) ) &
            &                     - 20.0_wp *   q(i,j,k)    )

          advtend = - const1 * ( cd_6order - ssign*diff_6order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

       END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
!CDIR OUTERUNROLL=8
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order = dphi_dt(i,j,k) *   ( ( q(i,j+3,k) - q(i,j-3,k) )     &
            &              -  9.0_wp * ( q(i,j+2,k) - q(i,j-2,k) )     &
            &              + 45.0_wp * ( q(i,j+1,k) - q(i,j-1,k) ) )

          diff_6order = ABS( dphi_dt(i,j,k) ) * ( ( q(i,j+3,k) + q(i,j-3,k) ) & 
            &                     -  6.0_wp * ( q(i,j+2,k) + q(i,j-2,k) ) & 
            &                     + 15.0_wp * ( q(i,j+1,k) + q(i,j-1,k) ) &
            &                     - 20.0_wp *   q(i,j,k)    )

          tend(i,j,k) = tend(i,j,k) - const1 * ( cd_6order - ssign*diff_6order )

       END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind5_lat

!==============================================================================
!==============================================================================

SUBROUTINE adv_centdiff6_lon( q, dlam_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
! 
! Method:
!   centered difference formula of 6th order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_6order, advtend

  const1 = 1.0_wp / 60.0_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order = dlam_dt(i,j,k) *   ( ( q(i+3,j,k) - q(i-3,j,k) )     &
            &              -  9.0_wp * ( q(i+2,j,k) - q(i-2,j,k) )     &
            &              + 45.0_wp * ( q(i+1,j,k) - q(i-1,j,k) ) )

          advtend = - const1 * cd_6order

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order = dlam_dt(i,j,k) *   ( ( q(i+3,j,k) - q(i-3,j,k) )     &
            &              -  9.0_wp * ( q(i+2,j,k) - q(i-2,j,k) )     &
            &              + 45.0_wp * ( q(i+1,j,k) - q(i-1,j,k) ) )

          tend(i,j,k) = tend(i,j,k) - const1 * cd_6order

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_centdiff6_lon

!==============================================================================
!==============================================================================

SUBROUTINE adv_centdiff6_lat( q, dphi_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,  &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)  
!   and ADD it to the field 'tend'
! 
! Method:
!   centered difference formula of 6th order
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_6order, advtend

  const1 = 1.0_wp / 60.0_wp * eddlat

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order = dphi_dt(i,j,k) * ( ( q(i,j+3,k) - q(i,j-3,k) )     &
            &            -  9.0_wp * ( q(i,j+2,k) - q(i,j-2,k) )     &
            &            + 45.0_wp * ( q(i,j+1,k) - q(i,j-1,k) ) )

          advtend = - const1 * cd_6order

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order = dphi_dt(i,j,k) * ( ( q(i,j+3,k) - q(i,j-3,k) )     &
            &            -  9.0_wp * ( q(i,j+2,k) - q(i,j-2,k) )     &
            &            + 45.0_wp * ( q(i,j+1,k) - q(i,j-1,k) ) )

          tend(i,j,k) = tend(i,j,k) - const1 * cd_6order

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_centdiff6_lat

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind1_lon_stagmix( q, dlam_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,        &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
!
! Method:
!   upwind formula of 1st order, but with staggered grid points for dlam_dt
!
! Difference to adv_upwind1_lon():
!   dlam_dt is not treated as beeing on the same grid point as q when computing
!   the advective tendencies. Here we assume that dlam_dt resides on staggered
!   ("1/2") grid points in relation to q. Two advective tendencies are independently
!   computed with the "left" and "right" dlam_dt-velocities at the staggered points
!   and then averaged to the center point (q-point) to get a better "grid-box-volume-representative"
!   tendency.
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_2order, diff_2order, advtend

  const1 = 0.5_wp * 0.5_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order =    q(i+1,j,k) - q(i-1,j,k)

          diff_2order =  q(i+1,j,k) - 2.0_wp * q(i,j,k) + q(i-1,j,k)

          advtend = - const1 *  &
       (          (      dlam_dt(i-1,j,k)  +     dlam_dt(i,j,k)  ) * cd_2order - &
          ssign * ( ABS( dlam_dt(i-1,j,k)) + ABS(dlam_dt(i,j,k)) ) * diff_2order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order =    q(i+1,j,k) - q(i-1,j,k)

          diff_2order =  q(i+1,j,k) - 2.0_wp * q(i,j,k) + q(i-1,j,k)

          tend(i,j,k) = tend(i,j,k) - const1 * &
       (          (      dlam_dt(i-1,j,k)  +     dlam_dt(i,j,k)  ) * cd_2order - &
          ssign * ( ABS( dlam_dt(i-1,j,k)) + ABS(dlam_dt(i,j,k)) ) * diff_2order )

        END DO
      END DO
    END DO
  ENDIF
  
END SUBROUTINE adv_upwind1_lon_stagmix

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind1_lat_stagmix( q, dphi_dt, tend, ssign,       &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,       &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)
!   and ADD it to the field 'tend'
!
! Method:
!   upwind formula of 1st order, but with staggered grid points for dphi_dt
!
! Difference to adv_upwind1_lat():
!   dphi_dt is not treated as beeing on the same grid point as q when computing
!   the advective tendencies. Here we assume that dphi_dt resides on staggered
!   ("1/2") grid points in relation to q. Two advective tendencies are independently
!   computed with the "left" and "right" dphi_dt-velocities at the staggered points
!   and then averaged to the center point (q-point) to get a better "grid-box-volume-representative"
!   tendency.
!

!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_2order, diff_2order, advtend

  const1 = 0.5_wp * 0.5_wp * eddlat

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order =    q(i,j+1,k) - q(i,j-1,k)

          diff_2order =  q(i,j+1,k) - 2.0_wp * q(i,j,k) + q(i,j-1,k)

          advtend = - const1 *  &
       (          (      dphi_dt(i,j-1,k)  +     dphi_dt(i,j,k)  ) * cd_2order - &
          ssign * ( ABS( dphi_dt(i,j-1,k)) + ABS(dphi_dt(i,j,k)) ) * diff_2order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_2order =    q(i,j+1,k) - q(i,j-1,k)

          diff_2order =  q(i,j+1,k) - 2.0_wp * q(i,j,k) + q(i,j-1,k)

          tend(i,j,k) = tend(i,j,k) - const1 *  &
       (          (      dphi_dt(i,j-1,k)  +     dphi_dt(i,j,k)  ) * cd_2order - &
          ssign * ( ABS( dphi_dt(i,j-1,k)) + ABS(dphi_dt(i,j,k)) ) * diff_2order )

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind1_lat_stagmix

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind3_lon_stagmix( q, dlam_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,        &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
!
! Method:
!   upwind formula of 3rd order, but with staggered grid points for dlam_dt
!
! Difference to adv_upwind3_lon():
!   dlam_dt is not treated as beeing on the same grid point as q when computing
!   the advective tendencies. Here we assume that dlam_dt resides on staggered
!   ("1/2") grid points in relation to q. Two advective tendencies are independently
!   computed with the "left" and "right" dlam_dt-velocities at the staggered points
!   and then averaged to the center point (q-point) to get a better "grid-box-volume-representative"
!   tendency.
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_4order, diff_4order, advtend

  const1 = 0.5_wp / 12.0_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order =                - ( q(i+2,j,k) - q(i-2,j,k) ) &
            &          +  8.0_wp * ( q(i+1,j,k) - q(i-1,j,k) )

          diff_4order =                ( q(i+2,j,k) + q(i-2,j,k) ) &
            &           - 4.0_wp * ( q(i+1,j,k) + q(i-1,j,k) ) &
            &           + 6.0_wp *   q(i,j,k)

          advtend = - const1 *  &
       (          (      dlam_dt(i-1,j,k)  +     dlam_dt(i,j,k)  ) * cd_4order + &
          ssign * ( ABS( dlam_dt(i-1,j,k)) + ABS(dlam_dt(i,j,k)) ) * diff_4order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order =                - ( q(i+2,j,k) - q(i-2,j,k) ) &
            &          +  8.0_wp * ( q(i+1,j,k) - q(i-1,j,k) )

          diff_4order =                ( q(i+2,j,k) + q(i-2,j,k) ) &
            &           - 4.0_wp * ( q(i+1,j,k) + q(i-1,j,k) ) &
            &           + 6.0_wp *   q(i,j,k)

          tend(i,j,k) = tend(i,j,k) - const1 * &
       (          (      dlam_dt(i-1,j,k)  +     dlam_dt(i,j,k)  ) * cd_4order + &
          ssign * ( ABS( dlam_dt(i-1,j,k)) + ABS(dlam_dt(i,j,k)) ) * diff_4order )

        END DO
      END DO
    END DO
  ENDIF
  
END SUBROUTINE adv_upwind3_lon_stagmix

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind3_lat_stagmix( q, dphi_dt, tend, ssign,       &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,       &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)
!   and ADD it to the field 'tend'
!
! Method:
!   upwind formula of 3rd order, but with staggered grid points for dphi_dt
!
! Difference to adv_upwind3_lat():
!   dphi_dt is not treated as beeing on the same grid point as q when computing
!   the advective tendencies. Here we assume that dphi_dt resides on staggered
!   ("1/2") grid points in relation to q. Two advective tendencies are independently
!   computed with the "left" and "right" dphi_dt-velocities at the staggered points
!   and then averaged to the center point (q-point) to get a better "grid-box-volume-representative"
!   tendency.
!

!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k

  REAL (KIND=wp)     :: const1, cd_4order, diff_4order, advtend

  const1 = 0.5_wp / 12.0_wp * eddlat

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order =               - ( q(i,j+2,k) - q(i,j-2,k) ) &
            &          + 8.0_wp * ( q(i,j+1,k) - q(i,j-1,k) )

          diff_4order =               ( q(i,j+2,k) + q(i,j-2,k) ) &
            &          - 4.0_wp * ( q(i,j+1,k) + q(i,j-1,k) ) &
            &          + 6.0_wp *   q(i,j,k)

          advtend = - const1 *  &
       (          (      dphi_dt(i,j-1,k)  +     dphi_dt(i,j,k)  ) * cd_4order + &
          ssign * ( ABS( dphi_dt(i,j-1,k)) + ABS(dphi_dt(i,j,k)) ) * diff_4order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_4order =               - ( q(i,j+2,k) - q(i,j-2,k) ) &
            &          + 8.0_wp * ( q(i,j+1,k) - q(i,j-1,k) )

          diff_4order =               ( q(i,j+2,k) + q(i,j-2,k) ) &
            &          - 4.0_wp * ( q(i,j+1,k) + q(i,j-1,k) ) &
            &          + 6.0_wp *   q(i,j,k)

          tend(i,j,k) = tend(i,j,k) - const1 *  &
       (          (      dphi_dt(i,j-1,k)  +     dphi_dt(i,j,k)  ) * cd_4order + &
          ssign * ( ABS( dphi_dt(i,j-1,k)) + ABS(dphi_dt(i,j,k)) ) * diff_4order )

        END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind3_lat_stagmix

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind5_lon_stagmix( q, dlam_dt, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,        &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -u * dq / dlambda  ( u = d lambda / dt )
!   and ADD it to the field 'tend'
! 
! Method:
!   upwind formula of 5th order, but with staggered grid points for dlam_dt
!
! Difference to adv_upwind5_lon():
!   dlam_dt is not treated as beeing on the same grid point as q when computing
!   the advective tendencies. Here we assume that dlam_dt resides on staggered
!   ("1/2") grid points in relation to q. Two advective tendencies are independently
!   computed with the "left" and "right" dlam_dt-velocities at the staggered points
!   and then averaged to the center point (q-point) to get a better "grid-box-volume-representative"
!   tendency.
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlon

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dlam_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_6order, diff_6order, advtend

  const1 = 0.5_wp / 60.0_wp * eddlon

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order =                  ( q(i+3,j,k) - q(i-3,j,k) ) &
            &          -  9.0_wp * ( q(i+2,j,k) - q(i-2,j,k) ) &
            &          + 45.0_wp * ( q(i+1,j,k) - q(i-1,j,k) )

          diff_6order =                ( q(i+3,j,k) + q(i-3,j,k) ) &
            &          -  6.0_wp * ( q(i+2,j,k) + q(i-2,j,k) ) &
            &          + 15.0_wp * ( q(i+1,j,k) + q(i-1,j,k) ) &
            &          - 20.0_wp *   q(i,j,k)

          advtend = - const1 *  &
       (          (      dlam_dt(i-1,j,k)  +     dlam_dt(i,j,k)  ) * cd_6order - &
          ssign * ( ABS( dlam_dt(i-1,j,k)) + ABS(dlam_dt(i,j,k)) ) * diff_6order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i+1,j,k) > qlim(i,j,k) .AND. qlim(i-1,j,k) > qlim(i,j,k) .AND. &
              (qlim(i+1,j,k) + qlim(i-1,j,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            !IF (advtend < 0.0_wp) WRITE(*,'(3(I4),7(F10.4))') i_global(i),j_global(j),k, &
            !    qlim(i-1,j,k),qlim(i,j,k),qlim(i+1,j,k),advtend,hhl(i-1,j,k),hhl(i,j,k),hhl(i+1,j,k)
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i+1,j,k) .AND. qlim(i,j,k) > qlim(i-1,j,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i+1,j,k) - qlim(i-1,j,k) > tadv_thresh) ) THEN
            !IF (advtend > 0.0_wp) WRITE(*,'(3(I4),7(F10.4))') i_global(i),j_global(j),k, &
            !    qlim(i-1,j,k),qlim(i,j,k),qlim(i+1,j,k),advtend,hhl(i-1,j,k),hhl(i,j,k),hhl(i+1,j,k)
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

        END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order =                  ( q(i+3,j,k) - q(i-3,j,k) ) &
            &          -  9.0_wp * ( q(i+2,j,k) - q(i-2,j,k) ) &
            &          + 45.0_wp * ( q(i+1,j,k) - q(i-1,j,k) )

          diff_6order =                ( q(i+3,j,k) + q(i-3,j,k) ) &
            &          -  6.0_wp * ( q(i+2,j,k) + q(i-2,j,k) ) &
            &          + 15.0_wp * ( q(i+1,j,k) + q(i-1,j,k) ) &
            &          - 20.0_wp *   q(i,j,k)

          tend(i,j,k) = tend(i,j,k) - const1 * &
       (          (      dlam_dt(i-1,j,k)  +     dlam_dt(i,j,k)  ) * cd_6order - &
          ssign * ( ABS( dlam_dt(i-1,j,k)) + ABS(dlam_dt(i,j,k)) ) * diff_6order )

        END DO
      END DO
    END DO
  ENDIF
  
END SUBROUTINE adv_upwind5_lon_stagmix

!==============================================================================
!==============================================================================

SUBROUTINE adv_upwind5_lat_stagmix( q, dphi_dt, tend, ssign,       &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, limiter,       &
  qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency   -v * dq / dphi  (v = d phi / dt)  
!   and ADD it to the field 'tend'
! 
! Method:
!   upwind formula of 5th order, but with staggered grid points for dphi_dt
!
! Difference to adv_upwind5_lat():
!   dphi_dt is not treated as beeing on the same grid point as q when computing
!   the advective tendencies. Here we assume that dphi_dt resides on staggered
!   ("1/2") grid points in relation to q. Two advective tendencies are independently
!   computed with the "left" and "right" dphi_dt-velocities at the staggered points
!   and then averaged to the center point (q-point) to get a better "grid-box-volume-representative"
!   tendency.
!
!
!------------------------------------------------------------------------------

  USE data_modelconfig, ONLY: eddlat

  INTEGER(KIND=iintegers), INTENT(in)                          :: kn, kv
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: q
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)    :: dphi_dt
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) :: tend
  REAL (KIND=wp),     INTENT(in) :: ssign
  INTEGER,          INTENT(in)   :: istart, iend, jstart, jend, kstart, kend
  LOGICAL,          INTENT(in)   :: limiter
! <fuo_tadvlimit
  REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in)    :: qlim
! fuo_tadvlimit>

  INTEGER :: i,j,k
  REAL (KIND=wp)     :: const1, cd_6order, diff_6order, advtend

  const1 = 0.5_wp / 60.0_wp * eddlat

  IF (limiter) THEN
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order =                  ( q(i,j+3,k) - q(i,j-3,k) ) &
            &          -  9.0_wp * ( q(i,j+2,k) - q(i,j-2,k) ) &
            &          + 45.0_wp * ( q(i,j+1,k) - q(i,j-1,k) )

          diff_6order =                ( q(i,j+3,k) + q(i,j-3,k) ) &
            &          -  6.0_wp * ( q(i,j+2,k) + q(i,j-2,k) ) &
            &          + 15.0_wp * ( q(i,j+1,k) + q(i,j-1,k) ) &
            &          - 20.0_wp *   q(i,j,k)

          advtend = - const1 *  &
       (          (      dphi_dt(i,j-1,k)  +     dphi_dt(i,j,k)  ) * cd_6order - &
          ssign * ( ABS( dphi_dt(i,j-1,k)) + ABS(dphi_dt(i,j,k)) ) * diff_6order )

          ! Local extrema exceeding the threshold must not be further amplified
! <fuo_tadvlimit
          IF ( qlim(i,j+1,k) > qlim(i,j,k) .AND. qlim(i,j-1,k) > qlim(i,j,k) .AND. &
              (qlim(i,j+1,k) + qlim(i,j-1,k) - 2._wp*qlim(i,j,k) > tadv_thresh) ) THEN
            !IF (advtend < 0.0_wp) WRITE(*,'(3(I4),7(F10.4))') i_global(i),j_global(j),k, &
            !    qlim(i,j-1,k),qlim(i,j,k),qlim(i,j+1,k),advtend,hhl(i,j-1,k),hhl(i,j,k),hhl(i,j+1,k)
            IF (advtend < 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MAX(0._wp,advtend)
          END IF
          IF ( qlim(i,j,k) > qlim(i,j+1,k) .AND. qlim(i,j,k) > qlim(i,j-1,k) .AND. &
              (2._wp*qlim(i,j,k) - qlim(i,j+1,k) - qlim(i,j-1,k) > tadv_thresh) ) THEN
            !IF (advtend > 0.0_wp) WRITE(*,'(3(I4),7(F10.4))') i_global(i),j_global(j),k, &
            !    qlim(i,j-1,k),qlim(i,j,k),qlim(i,j+1,k),advtend,hhl(i,j-1,k),hhl(i,j,k),hhl(i,j+1,k)
            IF (advtend > 0.0_wp) ngp_limit = ngp_limit + 1_iintegers
            advtend = MIN(0._wp,advtend)
          END IF
! fuo_tadvlimit>

          tend(i,j,k) = tend(i,j,k) + advtend

       END DO
      END DO
    END DO
  ELSE
    DO k=kstart, kend
      DO j=jstart, jend
        DO i=istart, iend

          cd_6order =                  ( q(i,j+3,k) - q(i,j-3,k) ) &
            &          -  9.0_wp * ( q(i,j+2,k) - q(i,j-2,k) ) &
            &          + 45.0_wp * ( q(i,j+1,k) - q(i,j-1,k) )

          diff_6order =                ( q(i,j+3,k) + q(i,j-3,k) ) &
            &          -  6.0_wp * ( q(i,j+2,k) + q(i,j-2,k) ) &
            &          + 15.0_wp * ( q(i,j+1,k) + q(i,j-1,k) ) &
            &          - 20.0_wp *   q(i,j,k)


          tend(i,j,k) = tend(i,j,k) - const1 *  &
       (          (      dphi_dt(i,j-1,k)  +     dphi_dt(i,j,k)  ) * cd_6order - &
          ssign * ( ABS( dphi_dt(i,j-1,k)) + ABS(dphi_dt(i,j,k)) ) * diff_6order )

       END DO
      END DO
    END DO
  ENDIF

END SUBROUTINE adv_upwind5_lat_stagmix

!==============================================================================
!==============================================================================

SUBROUTINE horiz_adv_driver( zq, zu, zv, tend, ssign,             &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, iadv_order,   &
  opt_limit, opt_qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   calculate the tendency
!     - u * dq/dlam - v * dq/dphi  (with u = d lam/dt, v = d phi/dt)
!   and ADD it to the field 'tend'
!
! Method:
!   this is a driver routine for adv_upwind1_lon, ..., adv_cd6_lat
!
!------------------------------------------------------------------------------

INTEGER(KIND=iintegers), INTENT(in) ::  &
  kn, kv
! <fuo_tadvlimit
REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in), TARGET  ::  &
  zq           
! fuo_tadvlimit>
REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)  ::  &
  zu, zv 
REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) ::  &
  tend
REAL (KIND=wp),     INTENT(in) ::  &
  ssign
INTEGER(KIND=iintegers), INTENT(in) ::  &
  istart, iend, jstart, jend, kstart, kend
INTEGER(KIND=iintegers), INTENT(in) ::  &
  iadv_order
LOGICAL, INTENT(in), OPTIONAL :: opt_limit
! <fuo_tadvlimit
REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in), TARGET, OPTIONAL  ::  &
  opt_qlim
! fuo_tadvlimit>

INTEGER (KIND=iintegers) ::  &
  izerror      
CHARACTER (LEN=80)       ::  &
  yzerrmsg

!NEC_CB
!INTEGER :: i,j,k
!REAL (KIND=wp)     :: const1, const2, cd_6order, diff_6order
!NEC_CB

LOGICAL :: limiter
! <fuo_tadvlimit
REAL (KIND=wp),     DIMENSION(:,:,:), POINTER :: zqlim
! fuo_tadvlimit>

  izerror = 0_iintegers

  IF (PRESENT(opt_limit)) THEN
    ! Use limiter for potential temperature advection
    limiter = opt_limit
  ELSE
    limiter = .FALSE.
  ENDIF

! <fuo_tadvlimit
  IF (PRESENT(opt_qlim)) THEN
    zqlim => opt_qlim
  ELSE
    zqlim => zq
  END IF
! fuo_tadvlimit>

  IF ( (kend > kv) .OR. (kend > kn) ) THEN
    yzerrmsg="kend>kv or kend>kn is not possible!"
    izerror = 1
    CALL model_abort (my_cart_id, 100+izerror, yzerrmsg, 'horiz_adv_driver')
  END IF

  ! Threshold value for limiter of potential temperature advection
  tadv_thresh = 5._wp
  ngp_limit = 0_iintegers

  IF ( iadv_order == 1 ) THEN 

! <fuo_tadvlimit
    CALL adv_upwind1_lon  ( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_upwind1_lat  ( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )

  ELSE IF ( iadv_order == 2 ) THEN 

    CALL adv_centdiff2_lon( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_centdiff2_lat( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )

  ELSE IF ( iadv_order == 3 ) THEN 

    CALL adv_upwind3_lon  ( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_upwind3_lat  ( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )

  ELSE IF ( iadv_order == 4 ) THEN 

    CALL adv_centdiff4_lon( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_centdiff4_lat( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )

  ELSE IF ( iadv_order == 5 ) THEN 

    CALL adv_upwind5_lon  ( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_upwind5_lat  ( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )

  ELSE IF ( iadv_order == 6 ) THEN 

    CALL adv_centdiff6_lon( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_centdiff6_lat( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
! fuo_tadvlimit>

  ELSE 

    yzerrmsg="false value for iadv_order"
    CALL model_abort (my_cart_id, 142, yzerrmsg, 'horiz_adv_driver')

  END IF

  IF (ngp_limit > 0) THEN
    WRITE(*,'(A,I6,A)') "  WARNING: advection limiter at ", ngp_limit, " gridpoints"
  END IF

END SUBROUTINE horiz_adv_driver

!==============================================================================
!==============================================================================

SUBROUTINE horiz_adv_driver_stagmix( zq, zu, zv, tend, ssign,        &
! <fuo_tadvlimit
  istart, iend, jstart, jend, kstart, kend, kn, kv, iadv_order,      &
  opt_limit, opt_qlim )
! fuo_tadvlimit>

!------------------------------------------------------------------------------
!
! Description:
!   driver routine for adv_upwind1_lon, ..., adv_upwind5_lat
!   with the premise that zu, zv are on the STAGGERED velocity points
!   and zq is on the MASS points.
!
!   It is meant as a testbed for testing new horizontal advection
!   operators for pp and T- advection operators. In this case,
!   we first compute two advective tendencies with the velocities
!   on the staggered velocity points at either side of the mass points
!   and subsequently average these two tendencies to get a better
!   grid-volume representative tendency value.
!
!------------------------------------------------------------------------------

INTEGER(KIND=iintegers), INTENT(in) ::  &
  kn, kv
! <fuo_tadvlimit
REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in), TARGET  ::  &
  zq
! fuo_tadvlimit>
REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kv), INTENT(in)  ::  &
  zu, zv
REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(inout) ::  &
  tend
REAL (KIND=wp),     INTENT(in) ::  &
  ssign
INTEGER(KIND=iintegers), INTENT(in) ::  &
  istart, iend, jstart, jend, kstart, kend
INTEGER(KIND=iintegers), INTENT(in) ::  &
  iadv_order
LOGICAL, INTENT(in), OPTIONAL :: opt_limit
! <fuo_tadvlimit
REAL (KIND=wp),     DIMENSION(1:ie,1:je,1:kn), INTENT(in), TARGET, OPTIONAL  ::  &
  opt_qlim
! fuo_tadvlimit>

CHARACTER (LEN=80)       ::  &
  yzerrmsg

!NEC_CB
!INTEGER :: i,j,k
!REAL (KIND=wp)     :: const1, const2, cd_6order, diff_6order
!NEC_CB

LOGICAL :: limiter
! <fuo_tadvlimit
REAL (KIND=wp),     DIMENSION(:,:,:), POINTER :: zqlim
! fuo_tadvlimit>

  IF (PRESENT(opt_limit)) THEN
    ! Use limiter for potential temperature advection
    limiter = opt_limit
  ELSE
    limiter = .FALSE.
  ENDIF

! <fuo_tadvlimit
  IF (PRESENT(opt_qlim)) THEN
    zqlim => opt_qlim
  ELSE
    zqlim => zq
  END IF
! fuo_tadvlimit>

  ! Threshold value for limiter of potential temperature advection
  tadv_thresh = 5._wp
  ngp_limit = 0_iintegers

  IF ( iadv_order == 1 ) THEN

! <fuo_tadvlimit
    CALL adv_upwind1_lon_stagmix  ( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_upwind1_lat_stagmix  ( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )

  ELSE IF ( iadv_order == 3 ) THEN

    CALL adv_upwind3_lon_stagmix  ( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_upwind3_lat_stagmix  ( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )

  ELSE IF ( iadv_order == 5 ) THEN

    CALL adv_upwind5_lon_stagmix  ( zq, zu, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
    CALL adv_upwind5_lat_stagmix  ( zq, zv, tend, ssign,  &
      istart, iend, jstart, jend, kstart, kend, kn, kv, limiter, zqlim )
! fuo_tadvlimit>

  ELSE

    yzerrmsg="false value for iadv_order"
    CALL model_abort (my_cart_id, 143, yzerrmsg, 'horiz_adv_driver_stagmix')

  END IF

  IF (ngp_limit > 0) THEN
    WRITE(*,'(A,I6,A)') "  WARNING: advection limiter at ", ngp_limit, " gridpoints"
  END IF

END SUBROUTINE horiz_adv_driver_stagmix

!==============================================================================
!==============================================================================
!+ CK 20101204 ART RK interfaces
SUBROUTINE implicit_sedim( phi_new, phi_old, v_new, v_old, rhoS)
!---------------------------------------------------------------
!
! Description:
!   solve the vertical flux advection equation for sedimentation
!   of scalar variables (a purely downward directed transport)
!   with the 2-point implicit scheme described in
!   COSMO Sci. doc. II, section 5.2.4.
!
! Method:
!   index convention for the sedimentation velocity :
!   v_new(i,j,k) = v(i,j,k+1/2)
!   sign: v_new, v_old > 0 ! (i.e. directed downward)
!
!   negative values in phi_new are clipped; this destroys
!   mass conservation.
!
!---------------------------------------------------------------

  IMPLICIT NONE

  REAL (KIND=wp),     INTENT(in)    :: phi_old(1:ie,1:je,1:ke)
  REAL (KIND=wp),     INTENT(in)    :: v_new  (1:ie,1:je,1:ke)
  REAL (KIND=wp),     INTENT(in)    :: v_old  (1:ie,1:je,1:ke)
!US: phi_new was with intent(out) before, but it is used before it is defined!
  REAL (KIND=wp),     INTENT(inout) :: phi_new(1:ie,1:je,1:ke)
  REAL (KIND=wp),     OPTIONAL, INTENT(in)   :: rhoS(1:ie,1:je,1:ke)

  INTEGER (KIND=iintegers) :: i,j,k
  REAL (KIND=wp)     :: h, dz, c, lambda_im

  DO k=2, ke
    DO j=jstart, jend
      DO i=istart, iend

        dz = hhl(i,j,k) - hhl(i,j,k+1)
        c = dt / ( 2.0_wp * dz );
        lambda_im = 1.0_wp / ( 1.0_wp + c * v_new(i,j,k) )

        h = phi_old(i,j,k) + c *                &
          ( v_new(i,j,k-1) * phi_new(i,j,k-1)   &
          + v_old(i,j,k-1) * phi_old(i,j,k-1)   &
          - v_old(i,j,k) * phi_old(i,j,k)  )
! HV 20101222 feedback M. Baldauf needed...
!CK          - v_old(i,j,k+1) * phi_old(i,j,k)  )

        IF ( PRESENT( rhoS ) ) THEN
          phi_new(i,j,k) = MAX( lambda_im * ( h + rhoS(i,j,k)*dt ), 0.0_wp)
        ELSE
          phi_new(i,j,k) = MAX( lambda_im * h, 0.0_wp)
        END IF

      END DO
    END DO
  END DO


END SUBROUTINE implicit_sedim

!==============================================================================
!==============================================================================

SUBROUTINE calc_wcon_sqrtg( u, v, w, wcon_sqrtg )

!------------------------------------------------------------------------------
! Description:
!   calculate the product
!   contravariant vertical velocity wcon * sqrt(G)
!   from the spherical wind components u, v, w 
!   (or from other spherical vector components)
! 
! Input:
!   u(:,:,:), v(:,:,:), w(:,:,:)
!
! Output: 
!   wcon_sqrtg(:,:,:)
!------------------------------------------------------------------------------

  REAL  (KIND=wp),     INTENT(IN):: &
    &    u(1:ie, 1:je, 1:ke), &
    &    v(1:ie, 1:je, 1:ke), &
    &    w(1:ie, 1:je, 1:ke+1)

  REAL  (KIND=wp),     INTENT(OUT):: &
    &    wcon_sqrtg(1:ie, 1:je, 1:ke+1)


  REAL  (KIND=wp),     ALLOCATABLE ::  &
    zu   (:,:,:),    & !
    zv   (:,:,:)

  REAL  (KIND=wp)           :: zsign

  INTEGER  (KIND=iintegers) :: i, j, k

  INTEGER  (KIND=iintegers) :: izstata, izstatd, izerror
  CHARACTER(LEN=80)         :: yzerrmsg

  REAL    (KIND=wp   )      :: r_earth_recip

!------------------------------------------------------------------------------

  izerror = 0_iintegers

  ALLOCATE( zu(1:ie, 1:je, 1:ke1),  &
            zv(1:ie, 1:je, 1:ke1),  &
            STAT=izstata)
  IF ( izstata /= 0 ) THEN
    yzerrmsg="allocation of zu, zv"
    izerror = 1_iintegers
    CALL model_abort (my_cart_id, 100+izerror, yzerrmsg, 'calc_wcon_sqrtg')
  END IF

  zsign = SIGN(1._wp,dt)

  r_earth_recip = 1.0_wp / r_earth

  ! calc.  u= d lambda/dt  and  v= d phi/dt and average to w-position:
  DO  k = 2, ke
    DO j = jstart, jend
      DO i = istart, iend
        zu(i,j,k) = (                wgtfac_u(i-1,j,k)   * u(i-1,j,k  )    &
          &         + ( 1.0_wp - wgtfac_u(i-1,j,k) ) * u(i-1,j,k-1)    &
          &         +                wgtfac_u(i  ,j,k)   * u(i,  j,k  )    &
          &         + ( 1.0_wp - wgtfac_u(i  ,j,k) ) * u(i,  j,k-1) )  &
          &                 * 0.5_wp * acrlat(j,1)
        zv(i,j,k) = (                wgtfac_v(i,j-1,k)   * v(i,j-1,k  )    &
          &         + ( 1.0_wp - wgtfac_v(i,j-1,k) ) * v(i,j-1,k-1)    &
          &         +                wgtfac_v(i,j  ,k)   * v(i,j  ,k  )    &
          &         + ( 1.0_wp - wgtfac_v(i,j  ,k) ) * v(i,j  ,k-1) )  &
          &                 * 0.5_wp * r_earth_recip
      END DO
    END DO
  END DO

  ! Calculation of the contravariant vertical velocity (wcon):

  wcon_sqrtg(:,:,:) = 0.0_wp

  ! (negative) tendency of horizontal advection of z:

  CALL horiz_adv_driver( hhl, zu, zv, wcon_sqrtg, zsign,  &
    &             istart, iend, jstart, jend, 2, ke, ke1, ke, iadv_order )

  ! die folgenden beiden Zeilen sollten NICHT noetig sein:
  wcon_sqrtg(:,:,1)    = 0.0_wp
  wcon_sqrtg(:,:,ke+1) = 0.0_wp

  DO k = 2, ke
    DO j = jstart, jend
      DO  i = istart, iend
        !wcon(i,j,k) = sqrtg_r_w(i,j,k) * ( -wcon(i,j,k) - w(i,j,k) )
        wcon_sqrtg(i,j,k) = -wcon_sqrtg(i,j,k) - w(i,j,k)
      ENDDO
    ENDDO
  ENDDO

  IF ( l2dim ) THEN
    DO j=1, nboundlines
      wcon_sqrtg(:,jstart-j,:) = wcon_sqrtg(:,jstart,:)
      wcon_sqrtg(:,jend  +j,:) = wcon_sqrtg(:,jstart,:)
    END DO
  END IF

  DEALLOCATE( zu, zv, STAT=izstatd )
  IF ( izstatd /= 0 ) THEN
    yzerrmsg = 'deallocation of zu, zv'
    izerror = 2_iintegers
    CALL model_abort (my_cart_id, 100+izerror, yzerrmsg, 'calc_wcon_sqrtg')
  END IF

END SUBROUTINE  calc_wcon_sqrtg


!==============================================================================

END MODULE src_advection_rk
