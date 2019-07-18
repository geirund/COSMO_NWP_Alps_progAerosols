!
!
! This module manages the activation of a fixed aerosol size distribution containing a number of NMOD modes to cloud droplets
!
!------------------------------------------------------------------------------

      Module art_aci

!------------------------------------------------------------------------------
!
! Description:
!
! Current Code Owner:
!  phone:  
!  fax:    
!  email:  max.bangert@kit.edu    
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! !VERSION!  !DATE!     Max Bangert
!  Initial Release
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================

USE data_parameters, ONLY :   &
    wp,    &                ! KIND-type parameter for real variables
    iintegers                   ! KIND-type parameter for standard integer variables

USE data_fields, ONLY:  &
     w            , &           ! vertical wind velocity
     t            , &           ! temperature
     pp           , &           ! deviation from the reference pressure         ( pa  )
     p0           , &           ! reference pressure at full levels             ( Pa  )
     tke          , &           ! SQRT(2 * turbulent kinetik energy)            ( m/s )
     sohr         , &           ! rate of solar heating                         ( K/s )
     thhr         , &           ! rate of thermal heating                       ( K/s )
     hhl          , &           ! geometrical height of half levels             ( m   )
     tkvh                       ! vertical turbulent diffusion coefficient for heat and moisture (m2/s)

USE data_parallel     , ONLY :   & 
    my_cart_id

USE data_runcontrol , ONLY :   &
    l2tls,        & ! rk or leapfrog
    ntstep,       & ! actual time step
    nstart,       & ! first time step of the forecast
    nold,         & ! corresponds to ntstep - 1
    nnow,         & ! corresponds to ntstep 
    nnew            ! corresponds to ntstep + 1

USE data_modelconfig, ONLY :   &
    ke,           &
    dt,           & ! timestep
    dt2             ! 2 * dt

USE art_aci_data,  ONLY: nmodes, CLOUD, MINCCN, CCN_A, CCN_B, CCN_C, &
                         act_indices,                       &
                         t_mode_aci_data

!==============================================================================

IMPLICIT NONE

CHARACTER (LEN=80)        :: yzerrmsg, yinput, yzroutine
INTEGER (KIND=iintegers)  :: izerror

TYPE(t_mode_aci_data), DIMENSION(nmodes) ::  activ_modes

!==============================================================================

 CONTAINS

!==============================================================================

SUBROUTINE call_activation(typeact,ii,jj,kk_in,n_cloud,Ntotal,Npact,ccn_n1,ccn_n2,ccn_n3)

 USE art_activation, ONLY : activation_master
 USE environment     ,   ONLY : model_abort
 USE data_parallel   ,   ONLY : my_cart_id
 USE data_runcontrol, ONLY: lCCNprog
 
 IMPLICIT NONE

 INTEGER (KIND=iintegers), INTENT (IN) ::  &
          typeact, &
          ii,jj,kk_in

 REAL (KIND=wp), INTENT (IN)       ::  &  ! This is actual a double precision variable in the 2mom scheme
          n_cloud

 DOUBLE PRECISION, INTENT (OUT)        ::  &  ! Keep this as dp to avoid problems
          Ntotal, Npact

 DOUBLE PRECISION, INTENT (IN), OPTIONAL :: &
          ccn_n1,ccn_n2,ccn_n3

 REAL (KIND=wp) ::  step_time,         & 
                        adv_factor

 INTEGER (KIND=iintegers)              ::  &
          i,j,iact,kk,m

 REAL (KIND=wp), PARAMETER :: massmin   = 1.0E-10_wp, & ! minimum total mass concentration per mode
                                  numbermin = 1.0_wp,     & ! miminum number concentration per mode 
                                  dummy     = -999.0_wp


 REAL (KIND=wp) ::  mom3,       & !
                        totmass,       & !
                        solmass,    & !
                        volsol,     & !
                        volinsol,   & !
                        n_act,      & !
                        n_act_fhh,  & !
                        s_max         !

 ! Is there any advantage in defining the following stuff as pointers?::

 LOGICAL, ALLOCATABLE ::                        &
    l_koehler(:) 

 REAL (KIND=wp), ALLOCATABLE ::             &
     dpg(:)      , & !
     amfs(:)     , & !
     vhf(:)      , & !
     ams(:)      , & !
     rho_sol(:)  , & !
     rho_insol(:), & !
     nmb_conc(:) , & !
     sig_g(:)     

 REAL (KIND=wp) ::               &
     temp     , & !
     pres     , & !
     w_parc   , & !
     sig_w        !

 LOGICAL :: lconst_aero_distr
  lconst_aero_distr = .TRUE. ! set TRUE by prescribed emissions, should be FALSE by default 

  IF(typeact==2 .AND. kk_in<ke) THEN
    kk=kk_in+1 ! below cloud aerosol activation
  ELSE
    kk=kk_in
  ENDIF

IF (t(ii,jj,kk,nnow) > 223._wp .AND. t(ii,jj,kk,nnow) < 323._wp) THEN !ik: check for reasonable temperature range: saturated water pressure only valid from 223 to 323K

  activ_modes(:)%do_activ =.FALSE.
  
  ! Consider supersaturation depletion by cloud and rain droplets for in-cloud and cloud base activation
  ! TODO: ice and snow could be consered in the same way as cloud liquid
  IF(.NOT. typeact==1) THEN
    activ_modes(CLOUD)%do_activ=.TRUE.
  ENDIF
 
IF(lconst_aero_distr) THEN   !20140519 cwalter

  iact=0 
  iact=iact+1
  act_indices(iact)=CCN_A
  activ_modes(CCN_A)%l_koehler = .TRUE.
  if (lCCNprog) then
    activ_modes(CCN_A)%number    = ccn_n2       ! is it possible to convert double precision to wp?!
    !print *,'CCN_A set to: ',ccn_n2
  else
    activ_modes(CCN_A)%number    = 12000e+06_wp
  endif
  activ_modes(CCN_A)%sigma     = 1.5_wp
  activ_modes(CCN_A)%solmassfr = 1.0_wp
  activ_modes(CCN_A)%diameter  = 0.094e-06_wp
  activ_modes(CCN_A)%dissfac_mean = 1.0_wp ! muss bei seasalts gerechnet werden 
  activ_modes(CCN_A)%molweight_mean = 132.14e-03_wp
  activ_modes(CCN_A)%rhosol_mean = 1.77e+03_wp
  activ_modes(CCN_A)%rhoinsol_mean = 2.2e+03_wp
  iact=iact+1
  act_indices(iact)=CCN_B
  activ_modes(CCN_B)%l_koehler = .TRUE.
  if (lCCNprog) then
    activ_modes(CCN_B)%number    = ccn_n1      ! is it possible to convert double precision to wp?!
  else
    activ_modes(CCN_B)%number    = 0.0_wp
  endif
  activ_modes(CCN_B)%sigma     = 2.5_wp
  activ_modes(CCN_B)%solmassfr = 0.7_wp
  activ_modes(CCN_B)%diameter  = 1.3e-06_wp
  activ_modes(CCN_B)%dissfac_mean = 1.0_wp ! muss bei seasalts gerechnet werden 
  activ_modes(CCN_B)%molweight_mean = 115.11e-03_wp
  activ_modes(CCN_B)%rhosol_mean = 1.78e+03_wp
  activ_modes(CCN_B)%rhoinsol_mean = 2.2e+03_wp
  iact=iact+1
  act_indices(iact)=CCN_C
  activ_modes(CCN_C)%l_koehler = .TRUE.
  if (lCCNprog) then
    activ_modes(CCN_C)%number    = ccn_n3     ! is it possible to convert double precision to wp?!
  else
    activ_modes(CCN_C)%number    = CCN_ship_N
  endif
  activ_modes(CCN_C)%sigma     = 1.59_wp
  activ_modes(CCN_C)%solmassfr = 0.62_wp
  activ_modes(CCN_C)%diameter  = 0.015e-06_wp
  activ_modes(CCN_C)%dissfac_mean = 2.0_wp ! muss bei seasalts gerechnet werden
  activ_modes(CCN_C)%molweight_mean = 61.20e-03_wp
  activ_modes(CCN_C)%rhosol_mean = 1.86e+03_wp
  activ_modes(CCN_C)%rhoinsol_mean = 2.0e+03_wp

! Consider cloud/rain droplets in in-cloud activation
  IF(activ_modes(CLOUD)%do_activ) THEN
    iact=iact+1
    act_indices(iact)=CLOUD
    activ_modes(CLOUD)%l_koehler = .TRUE.
    activ_modes(CLOUD)%number    = n_cloud       ! is it possible to convert double precision to wp?!
    activ_modes(CLOUD)%sigma     = 1.5_wp
    activ_modes(CLOUD)%solmassfr = 1.0_wp
    activ_modes(CLOUD)%diameter  = 2.0e-06_wp
!    activ_modes(CLOUD)%surface  =  pi * caero(vseasc0)*(activ_modes(CLOUD)%diameter)**2.0*en1**16.0   
    activ_modes(CLOUD)%dissfac_mean = 1.0 ! muss bei seasalts gerechnet werden 
    activ_modes(CLOUD)%molweight_mean = 100.0e-03_wp
    activ_modes(CLOUD)%rhosol_mean = 1.8e+03_wp
    activ_modes(CLOUD)%rhoinsol_mean = 1.0e+03
  ENDIF

ELSE
  CALL model_abort(my_cart_id, 1650, "lconst_aero_distr = .FALSE. not implemented", "art_aci.f90")
ENDIF

  ! Prepare input for activation parameterization 
  ALLOCATE(l_koehler(iact),dpg(iact),amfs(iact),vhf(iact),ams(iact),  &
            &  rho_sol(iact),rho_insol(iact),nmb_conc(iact),sig_g(iact))
  
  Ntotal=0.0

  ! Aerosol
  DO i=1,iact
    l_koehler(i)= activ_modes(act_indices(i))%l_koehler
    dpg(i)      = activ_modes(act_indices(i))%diameter
    amfs(i)     = activ_modes(act_indices(i))%solmassfr
    vhf(i)      = activ_modes(act_indices(i))%dissfac_mean
    ams(i)      = activ_modes(act_indices(i))%molweight_mean
    rho_sol(i)  = activ_modes(act_indices(i))%rhosol_mean
    rho_insol(i)= activ_modes(act_indices(i))%rhoinsol_mean
    nmb_conc(i) = activ_modes(act_indices(i))%number
    sig_g(i)    = activ_modes(act_indices(i))%sigma

    ! Total Aerosol number to constrain total cloud droplet number in cloud scheme
    IF(act_indices(i)/=CLOUD)  Ntotal = Ntotal + nmb_conc(i)

  ENDDO
 
  ! Meteorological conditions
  temp  = t(ii,jj,kk,nnow) 
  pres  = p0(ii,jj,kk) + pp(ii,jj,kk,nnow)
  IF (typeact==2) THEN ! subgridscale effects are considered by advection factor
    !w_parc= MAX( w(ii,jj,kk,nnow)   ,1.e-10_wp)
    w_parc=MAX( w(ii,jj,kk,nnow) + 0.8 * sqrt(tke(ii,jj,kk,nnow))  ,1.e-10_wp)
    sig_w=1.e-30_wp
  ELSE
    w_parc=MAX( w(ii,jj,kk,nnow) + 0.8 * sqrt(tke(ii,jj,kk,nnow))  ,1.e-10_wp)
    !w_parc= MAX( w(ii,jj,kk,nnow)   ,1.e-10_wp)
    sig_w = MAX( tke(ii,jj,kk,nnow) ,0.1_wp)
    !sig_w=1.e-30_wp
  ENDIF

  ! meaning of settings above:
  !w_parc= MAX( w(ii,jj,kk,nnow)   ,0.01_wp)
  !--> old cutoff value that is inconsistent with 2M-scheme
  !w_parc=MAX( w(ii,jj,kk,nnow) + 0.8 * sqrt(tke(ii,jj,kk,nnow))  ,0.01_wp)
  ! --> poor-man's replacement for updraft-pdf
  !sig_w=1.e-30_wp --> switch off updraft-pdf

  !===============================================================================================================
  !=========   call activation  ==================================================================================
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>=======================================================================
  CALL activation_master(iact,l_koehler,dpg,amfs,vhf,ams,  &  !< INTENT(IN)
          &              rho_sol,rho_insol,                &  !< INTENT(IN)
          &              temp,pres,w_parc,sig_w,           &  !< INTENT(IN) 
          &              nmb_conc,sig_g,                   &  !< INTENT(IN)
          &              n_act,n_act_fhh,s_max)               !< INTENT(OUT)
  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<========================================================================
  !===============================================================================================================
  DEALLOCATE(l_koehler,dpg,amfs,vhf,ams,  &
          &  rho_sol,rho_insol,nmb_conc,sig_g)

  ! In case of in-cloud activation substract the cloud droplets again
  IF(activ_modes(CLOUD)%do_activ) THEN
    n_act=MAX(0.0, n_act-activ_modes(CLOUD)%number )
  ENDIF

  ! substract existing cloud droplets to prevent multiple activation of aerosol
  ! (not needed for cloud-base activation because unactivated aerosol is mixed in)
  IF (typeact==3) THEN
    n_act=MAX(0.0, n_act-activ_modes(CLOUD)%number)
  ENDIF

  ! select timelevel and timestep for calculations                          
  IF ( l2tls ) THEN
    step_time = dt
  ELSE
    step_time = dt2
  ENDIF
    
  IF(typeact==1 .OR. typeact==3) adv_factor = 1.0 / step_time
    
  IF(typeact==2) THEN   ! should be changed to a consistent formulation with updraft PDF, w -> w_eff?! without turb
    IF(kk<=ke) THEN
      adv_factor = min( w(ii,jj,kk,nnow)   / (hhl(ii,jj,kk-1)-hhl(ii,jj,kk)) & !,1.0_wp/ step_time)  &
                        + tkvh(ii,jj,kk)  &
                        /  (0.5_wp* (hhl(ii,jj,kk-1)+hhl(ii,jj,kk)) -  0.5_wp* (hhl(ii,jj,kk)+hhl(ii,jj,kk+1))) &  
                        /  (hhl(ii,jj,kk-1)-hhl(ii,jj,kk)) &
                       ,1.0_wp/ step_time)
    ELSE 
      adv_factor = min( (w(ii,jj,kk+1,nnow) + w(ii,jj,kk,nnow))/ 2.0_wp / (hhl(ii,jj,kk)-hhl(ii,jj,kk+1)) , &
                         1.0_wp/ step_time)
    ENDIF
  ENDIF
    
  ! **  Calculate activation rate **
  Npact = DBLE(n_act * adv_factor) 
ELSE
 Ntotal = 0._wp     !ik: no minimum value, because a minimum value is added in 2mom-scheme
 Npact = 0._wp
ENDIF			!ik: check for reasonable temperature range
END SUBROUTINE call_activation 

!==============================================================================
END MODULE art_aci
