!+ Source module for stochastics physics utility routines
!==============================================================================

MODULE stoch_physics

!==============================================================================
!
! Description:
!   This module provides service utilities for stochastic physics 
!   calculations mainly based on Buizza et al (1999) and Palmer et al. (2009).
!   Options for "smoothed" perturbations in time and space are
!   also provided.   
!
!     A random number rn is drawn from a uniform distribution 
!     or a gaussian distribution (lgauss_rn) for every point of
!     the externally specified horizontal coarse grid (box) having 
!     (ie_rn, je_rn) points and (dlon_rn, dlat_rn) grid spacings.
!     The same random number is used for every point of the 
!     vertical grid column. 
!
!     All the model grid points contained in the same  
!     coarse grid point have the same random number. 
!
!     A new random number is drawn every ninc_rn time steps.
!
!     The random number stream is defined by the initial datetime and
!     the member number. It is also possible to specify an external seed 
!     for the random number generation (nseed_rn).  
!
!     In case of uniform distribution (0<rn<1) the magnitude of stochastics 
!     	physics perturbation is defined as: 
!          ppt/pt=1+a*(1-2*rn)
!  	   where:
!     		pt  : tendency
!     		ppt : perturbed tendency
!     		a   : max perturbation amplitude (range_rn)
!     		rn  : random number (0<rn<1)         
!
!     In case of gaussian distribution (lgauss_rn) the magnitude of stochastics 
!     	physics perturbation is defined as: 
!          ppt/pt=1+MIN(a, std*rn) if rn > 0
!          ppt/pt=1-MIN(a,-std*rn) if rn < 0
!  	   where:
!     		std : perturbation standard deviation (stdv_rn)
!  
!     Thus, depending on a (range_rn), the relative size of the tendency 
!     	perturbation can be:   (1-a) < ppt/pt < (1+a)
! 
!     The same random number is applied to perturb T, u, v, qv and optionally
!     qc, qi, qr, qs, qg physical tendencies.
!
!     itype_vtaper_rn: type of random number tapering near surface/stratosphere
!                  itype_vtaper_rn=0 (no vert. tapering)
!                  itype_vtaper_rn=1 (prescribed: sfc and stratosph.)
!                  itype_vtaper_rn=2 (prescribed: only stratosph.)
!                  itype_vtaper_rn=3 (specified from namelist - vtaper_rn(1:ke))
!
!     If lhorint_rn=T the random numbers are horizontally interpolated on
!       the model grid
! 
!     If ltimeint_rn=T the random numbers are linearly interpolated in time 
!       every time step between the previous and the next drawn value (every
!       hinc_rn time steps) 
!
!     If npattern_rn > 1 it is possible to specify more random number patterns  
!
!     itype_qxpert_rn define which hum variables tend. are perturbed
!                 0:    Only qv tendencies are perturbed
!                 1:    qv,qc,qi tendencies are perturbed
!                 2:    qv,qc,qi,qr,qs,qg tendencies are perturbed
!
!
!     itype_qxlim_rn  type of reduction/removal of the perturbation 
!                     in case of negative (qv, qc, qi) or 
!                     supersaturated (qv) values
!-------------------------------------------------------------------------------
!   Routines (module procedures) currently contained:
!
! init_stoch_phys
! compute_stoch_phys
! init_rand_numb
! gen_rand_numb
! int_rand_numb
! set_seed_rand_numb
! apply_tqx_tend_adj
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
!------------------------------------------------------------------------------

USE data_parameters , ONLY :   &
  wp,        & ! KIND-type parameters for real variables
  iintegers    ! KIND-type parameter for standard integer variables

!------------------------------------------------------------------------------

USE vgrid_refatm_utils, ONLY :   &
    refatm,         & ! reference pressure at sea level
    vcoord            ! sigma-coordinate refering to PMSL

!------------------------------------------------------------------------------

USE data_modelconfig, ONLY :   &
    dt,           & ! long time-step
    dt2,          & ! 2*long time-step

! horizontal and vertical sizes of the fields and related variables
! --------------------------------------------------------------------
    dlon,         & ! grid point distance in zonal direction (in degrees)
    dlat,         & ! grid point distance in meridional direction (in degrees)
    startlon_tot, & ! transformed longitude of the lower left grid point
                    ! of the total domain (in degrees, E>0)
    startlat_tot, & ! transformed latitude of the lower left grid point
                    ! of the total domain (in degrees, N>0)
    ie_tot,       & ! number of grid points in zonal direction
    je_tot,       & ! number of grid points in meridional direction
    ke_tot,       & ! number of grid points in vertical direction
    ie,           & ! number of grid points in zonal direction
    je,           & ! number of grid points in meridional direction
    ke,           & ! number of grid points in vertical direction
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
    jendu,        & ! end index for the forecast of u
    jstartv,      & ! start index for the forecast of v
    jendv           ! end index for the forecast of v

USE data_fields     , ONLY :   &
! variables                                                 (unit )
! -----------------------------------------------------------------
    p0               ! reference pressure                    ( pa  )

USE data_runcontrol,    ONLY :  &
    ntstep,        & ! actual time step
    nstart,        & ! first time step of the forecast
    l2tls,         & ! forecast with 2-TL integration scheme
    iepsmem,       & ! ID of the member in the ensemble (ID >= 0) 

    lhorint_rn,    & ! random numbers (defined on a rn hor. coarse grid)
                     ! horizontally interpolated on model grid (otherwise
                     ! model grid points contained in the same rn coarse grid
                     ! point have the same random number value)
    ltimeint_rn,   & ! random numbers (defined on a rn hor. coarse grid) are 
                     ! interpolated in time
    lgauss_rn,     & ! use a gaussian distribution of random numbers
                     ! (otherwise a uniform distribution is used)
    dlat_rn   ,    & ! rn coarse grid point distance in meridional direction 
                     ! (in degrees)
    dlon_rn,       & ! rn coarse grid point distance in zonal direction 
                     ! (in degrees)
    rvtaper_rn,    & ! externally specified function for vertical tapering of
                     ! random number
    itype_vtaper_rn,&! type of tapering near surface and in stratosphere
    ninc_rn,       & ! time step increment for drawing a new set of 
                     ! random numbers
    nstep_rn,      & ! actual random number time step
    npattern_rn,   & ! number of patterns with different scale/correl. 
    nseed_rn,      & ! external specified seed for random number generation
    stdv_rn,       & ! standard deviation of the gaussian distribution of 
                     ! random numbers
    range_rn,      & ! max magnitude of random numbers 
    n1_rn,         & ! indices for permutation of the
    n2_rn,         & ! two random number time levels
    ie_rn,         & ! number of hor. coarse grid points in zonal direction 
    je_rn            ! number of hor. coarse grid points in meridional direction
                     ! where random numbers are defined

USE data_io,            ONLY :  &
    ydate_ini        ! start of the forecast 
                     ! yyyymmddhh (year, month, day, hour)

USE data_parallel,      ONLY :  &
    icomm_compute, & ! communicator for the group of compute PEs
    imp_reals,     & ! correct type for MPI
    imp_integers,  & ! correct type for MPI
    num_compute,   & ! number of compute PEs
    my_cart_id       ! rank of this subdomain in the cartesian communicator
                     ! model for MPI
   
USE data_constants  , ONLY :   &
!  physical constants and related variables
! -------------------------------------------
    rdv,          & ! r_d / r_v
    o_m_rdv,      & ! 1 - r_d/r_v
!  constants for parametrizations
! ---------------------------------
    b1,           & ! variables for computing the saturation vapour pressure
    b2w,          & ! over water (w) and ice (i)
    b3,           & !               -- " --
    b4w             !               -- " --
    
USE parallel_utilities, ONLY :  distribute_values, i_global, j_global

USE mo_kind,                 ONLY :  &
    dp                 ! double precision (required for random_gauss)

USE mo_random,               ONLY :  &
    random_state_t  ,& ! derived type: random generator state
    construct       ,& ! constructs new random generator (state) from seed
    random_number   ,& ! random number generator
    random_gauss       ! generator for Gaussian random number distribution

!==============================================================================

IMPLICIT NONE

!==============================================================================

!------------------------------------------------------------------------------
! Declarations
!------------------------------------------------------------------------------

REAL (KIND=wp)     ::  &
     fi_rn,&  !
     sig_rn,& !
     range_tot,&!
     zdt      ! time step

REAL (KIND=wp),     ALLOCATABLE :: &
     taper(:),             & ! vertical tapering function  
     rlatwei_rn(:,:,:),      & ! weights of for horizontal interpolation of rn
     rlonwei_rn(:,:,:),      & !          
     randnumb(:,:,:,:),      & ! random numbers on coarse grid
     randnumbint(:,:,:,:,:), & ! random numbers on model grid
     pertstoph(:,:,:)        ! stoch. phys. perturbations

INTEGER (KIND=iintegers) ::   &
     k50,     & ! indexes for vertical tapering
     k100,    & !
     ksfc,    & !
     k870       ! 

INTEGER (KIND=iintegers), ALLOCATABLE :: &
     jlatbox_rn(:,:,:), & ! Indexes of nearest rn coarse grid points for each 
     ilonbox_rn(:,:,:)    ! model grid point used in rn hor. int.
      

!LOGICAL :: lspaceint_rn(npattern_rn) !Internal switch that actives the hor. int.
LOGICAL, ALLOCATABLE :: lspaceint_rn(:) !Internal switch that actives the hor. int.

REAL (KIND=wp)     :: tau_rn = 6.0_wp ! (to implement)

LOGICAL :: ldfi_step = .false.    ! DFI must not be used with stoch. phys. 
                                  ! (to implement)
LOGICAL :: ltimar1_rn = .false.   ! (to implement) 

SAVE
 
TYPE (random_state_t), ALLOCATABLE :: yd_random_stream(:)

!------------------------------------------------------------------------------

CONTAINS


!==============================================================================
!+ Module procedure in "src_stoch_phys" for initialise stoch. phys.
!------------------------------------------------------------------------------

SUBROUTINE init_stoch_phys

!----------------------------------------------------------------------------
!
! Description: Initialise random number generator, generate first set of rn 
!              and distribute it to the other processors. In case of restart
!              reproduces the same rn (gen_rand_numb is called the same
!              number of times)
!             
!
!----------------------------------------------------------------------------

IMPLICIT NONE 

INTEGER (KIND=iintegers) :: mtstep,    & !
                            j,n,       & !
                            izerrstat    ! error control

REAL (KIND=wp)     , ALLOCATABLE :: zhelp (:)

  n1_rn = 1
  n2_rn = 2

! Initialisation of random number (rn) generator 

  CALL init_rand_numb

! First set of rn

  DO n=1,npattern_rn
    nstep_rn(n) = (ntstep/ninc_rn(n))*ninc_rn(n)
    IF (my_cart_id == 0) THEN
      CALL gen_rand_numb(n1_rn(n),n)
      IF(ltimeint_rn)CALL gen_rand_numb(n2_rn(n),n)
    ENDIF

! rn distribution to other processors

    IF(num_compute > 1) THEN
      ALLOCATE(zhelp(ie_rn(n)))
      DO j=1,je_rn(n)
        zhelp(:)=randnumb(1:ie_rn(n),j,n1_rn(n),n)
        CALL distribute_values (zhelp,ie_rn(n), 0,&
           imp_reals,    icomm_compute, izerrstat)
        IF(my_cart_id /= 0)randnumb(:,j,n1_rn(n),n)=zhelp(:)
        IF(ltimeint_rn)THEN
          zhelp(:)=randnumb(1:ie_rn(n),j,n2_rn(n),n)
          CALL distribute_values (zhelp,ie_rn(n), 0,&
             imp_reals,    icomm_compute, izerrstat)
          IF(my_cart_id /= 0)randnumb(1:ie_rn(n),j,n2_rn(n),n)=zhelp(:)
        ENDIF
      ENDDO
      DEALLOCATE(zhelp)
    ENDIF
  ENDDO

! Restart case: all rn before nstart are generated to have reproduc. results
! CHECK....

  IF(nstart > 0)THEN
  DO n=1,npattern_rn
    DO mtstep = 1, nstart-1
      IF ((mtstep+1 > nstep_rn(n) + ninc_rn(n) &
         .OR.(ltimar1_rn.AND.mtstep>0)).AND. .NOT.ldfi_step) THEN
        IF(.NOT.(mtstep == 1 .AND. ltimar1_rn)) THEN
          n1_rn(n) = 3 - n1_rn(n)
          n2_rn(n) = 3 - n2_rn(n)
        ENDIF
        nstep_rn(n) = (mtstep/ninc_rn(n))*ninc_rn(n)
        IF (my_cart_id == 0) CALL gen_rand_numb(n2_rn(n),n)
        IF(num_compute > 1) THEN
          ALLOCATE(zhelp(ie_rn(n)))
          DO j =1 ,je_rn(n)
            zhelp(:)=randnumb(1:ie_rn(n),j,n2_rn(n),n)
            CALL distribute_values (zhelp,ie_rn(n), 0,&
                 imp_reals,    icomm_compute, izerrstat)
            IF(my_cart_id /= 0)randnumb(1:ie_rn(n),j,n2_rn(n),n)=zhelp(:)
          ENDDO
          DEALLOCATE(zhelp)
        ENDIF
      ENDIF
    ENDDO
  ENDDO
  ENDIF

END SUBROUTINE init_stoch_phys

!==============================================================================
!+ Module procedure in "src_stoch_phys" for computing stoch. phys. pert.
!------------------------------------------------------------------------------

SUBROUTINE compute_stoch_phys

!------------------------------------------------------------------------------
!
! Description: Compute stochastic physics perturbations. 
!       Model grid points contained in the same box
!       (coarse grid point) have the same random number.
!       If requested:
!       - hor. interp. of random numbers from coarse grid 
!         to model grid every ninc_rn time steps
!       - time interp. of random numbers every time step 
!
!------------------------------------------------------------------------------

!============================================================================

IMPLICIT NONE

REAL (KIND=wp)     :: ppert, zwgt1, zwgt2
REAL (KIND=wp)     , ALLOCATABLE :: zhelp (:)
INTEGER (KIND=iintegers) :: i, j, k, n
INTEGER (KIND=iintegers) :: mtstep,                   &
    izerrstat                  ! error control

  pertstoph(:,:,:) = 0.0_wp 

  DO n=1,npattern_rn
    lspaceint_rn(n)=.FALSE.

! Compute a new set of random numbers every ninc_rn time steps
    IF ((ntstep+1 > nstep_rn(n) + ninc_rn(n) &
     .OR.(ltimar1_rn.AND.ntstep>0)).AND. .NOT.ldfi_step) THEN
      IF(.NOT.(ntstep == 1 .AND. ltimar1_rn)) THEN
        n1_rn(n) = 3 - n1_rn(n)
        n2_rn(n) = 3 - n2_rn(n)
      ENDIF
      nstep_rn(n) = (ntstep/ninc_rn(n))*ninc_rn(n)
      IF (my_cart_id == 0) PRINT*,'Compute a new set of random numbers ',n
      IF (my_cart_id == 0) CALL gen_rand_numb(n2_rn(n),n)
      IF(num_compute > 1) THEN
        ALLOCATE(zhelp(ie_rn(n)))
        DO j =1 ,je_rn(n)
          zhelp(:)=randnumb(1:ie_rn(n),j,n2_rn(n),n)
          CALL distribute_values (zhelp,ie_rn(n), 0,&
             imp_reals,    icomm_compute, izerrstat)
          IF(my_cart_id /= 0)randnumb(1:ie_rn(n),j,n2_rn(n),n)=zhelp(:)
        ENDDO
        DEALLOCATE(zhelp)
      ENDIF
      lspaceint_rn(n)=.TRUE.
    ENDIF

!----------------------------------------------------------
!       compute stochastic physics perturbations 
!----------------------------------------------------------

    IF (ntstep > 0) THEN

!  Horizontal interpolation and vertical tapering of rn
!  when new rn are computed
      IF (lspaceint_rn(n))THEN
        CALL int_rand_numb( &
            istart, iend+1, jstart, jend+1, 1, ke,&
            n2_rn(n),n,randnumbint(:,:,:,n2_rn(n),n))
      ENDIF

!  Calculation of coefficients for time interpolation if requested
      IF (ltimeint_rn) THEN
        zwgt2 = FLOAT(ntstep + 1 - nstep_rn(n))/FLOAT(ninc_rn(n))
        zwgt1 = 1._wp - zwgt2
      ELSEIF(ltimar1_rn)THEN
        zwgt1 = fi_rn
        IF(ntstep == 1) zwgt1 = zwgt1 * stdv_rn(n)
        zwgt2 = sig_rn
      ELSE
        zwgt1 = 0._wp
        zwgt2 = 1._wp
      ENDIF

      DO k = 1, ke
        DO j = jstart, jend+1
          DO i = istart, iend+1
            ppert = &
             (zwgt1*randnumbint(i,j,k,n1_rn(n),n)+ &
              zwgt2*randnumbint(i,j,k,n2_rn(n),n))

!  Sum of the perturbation patterns and their limitation to max/min values
            pertstoph(i,j,k) = pertstoph(i,j,k) + ppert
            IF (n == npattern_rn .AND. n > 1) THEN
              IF(pertstoph(i,j,k) > range_tot) THEN
                pertstoph(i,j,k) =  range_tot
              ELSEIF(pertstoph(i,j,k) < -range_tot) THEN
                pertstoph(i,j,k) = - range_tot
              ENDIF
            ENDIF

          END DO
        END DO
      END DO

    ELSE

!  Horizontal interpolation and vertical tapering of the first computed rn 
      CALL int_rand_numb( &
            istart, iend+1, jstart, jend+1, 1, ke,&
            n1_rn(n),n,randnumbint(:,:,:,n1_rn(n),n))

!  Horizontal interpolation and vertical tapering of the second computed rn
!  if time interpolation is activated 
      IF (ltimeint_rn) THEN
        CALL int_rand_numb( &
              istart, iend+1, jstart, jend+1, 1, ke,&
              n2_rn(n),n,randnumbint(:,:,:,n2_rn(n),n))
      ELSE
        randnumbint(:,:,:,n2_rn(n),n) = randnumbint(:,:,:,n1_rn(n),n)
      ENDIF
    ENDIF
  ENDDO

! Perturbations are centered around 1
  pertstoph(:,:,:) = pertstoph(:,:,:) + 1._wp

END SUBROUTINE compute_stoch_phys

!==============================================================================
!+ Module procedure in "src_stoch_phys" for initialising random numbers 
!------------------------------------------------------------------------------

SUBROUTINE init_rand_numb

!----------------------------------------------------------------------------
!
! Description: Set the seed of random number stream, compute 
!              vertical tapering function and rn coarse grid
!              and its parameters
!
!
!----------------------------------------------------------------------------

IMPLICIT NONE
 
INTEGER (KIND=iintegers) :: i, j, k, n, izerrstat, koutseed

INTEGER (KIND=iintegers) :: ishift(npattern_rn), jshift(npattern_rn)
 
REAL (KIND=wp   )     :: z, zlat, zlon, zpf1, zpf2, p50, p100, &
             p870, psfc, zpno, zpnu 
REAL (KIND=dp   ) :: zshift(2)

  IF ( l2tls ) THEN
    zdt   = dt
  ELSE
    zdt   = dt2
  ENDIF


  ALLOCATE (lspaceint_rn(npattern_rn))
  ALLOCATE (yd_random_stream(npattern_rn))
  ALLOCATE(rlatwei_rn(1+1:ie_tot-1,1+1:je_tot-1,npattern_rn), &
           rlonwei_rn(1+1:ie_tot-1,1+1:je_tot-1,npattern_rn))
  ALLOCATE(jlatbox_rn(1+1:ie_tot-1,1+1:je_tot-1,npattern_rn), &
           ilonbox_rn(1+1:ie_tot-1,1+1:je_tot-1,npattern_rn))
  ALLOCATE(taper(1:ke_tot))
  taper=1.0_wp

!  ALLOCATE(randnumbint(1:ie, 1:je, ke, 2, npattern_rn))
  ALLOCATE(randnumbint(istart:iend+1, jstart:jend+1, ke, 2, npattern_rn))
  randnumbint=0._wp

  ALLOCATE(pertstoph(1:ie, 1:je, ke))
  pertstoph=1._wp

! Vertical tapering function definition
  IF(itype_vtaper_rn > 0.AND.itype_vtaper_rn <= 2)THEN
    zpno=0._wp
    DO k = 1,ke_tot
      zpno = refatm%p0sl*vcoord%sigm_coord(k  )
      zpnu = refatm%p0sl*vcoord%sigm_coord(k+1)
      IF ( (zpno <= 870.0E2_wp) .AND. (870.0E2_wp < zpnu) ) THEN
        k870 = k
        p870 = zpno
      ENDIF
      IF ( (zpno <= 100.0E2_wp) .AND. (100.0E2_wp < zpnu) ) THEN
        k100 = k
        p100 = zpno
      ENDIF
      IF ( (zpno <= 50.0E2_wp) .AND. (50.0E2_wp < zpnu) )  THEN
        k50 = k
        p50 = zpno
      ENDIF
    ENDDO
    ksfc = ke_tot+1
    psfc = refatm%p0sl*vcoord%sigm_coord(ksfc)
    DO k= 1,ke_tot
      IF(k <= k50)taper(k)=0._wp
      IF(k > k50 .AND. k < k100)taper(k)= &
         (refatm%p0sl*0.5_wp*(vcoord%sigm_coord(k)+ &
          vcoord%sigm_coord(k+1))-p50)/(p100-p50)
      IF(k >= k100.AND. k <= k870)taper(k)=1.0_wp
      IF (itype_vtaper_rn == 2)THEN
        IF(k > k870)taper(k)=1.0_wp
      ELSE
        IF(k > k870 .AND. k < ksfc)taper(k)= &
          (refatm%p0sl*0.5_wp*(vcoord%sigm_coord(k)+ &
           vcoord%sigm_coord(k+1))-psfc)/(p870-psfc)
      ENDIF
    ENDDO
  ELSEIF(itype_vtaper_rn == 3)THEN
    k50=0
    ksfc=ke_tot+1
    DO k= 1,ke_tot
      taper(k)=rvtaper_rn(k)
    ENDDO
  ELSE
    k50=0
    ksfc=ke_tot+1
  ENDIF

  IF (my_cart_id==0.AND.itype_vtaper_rn > 0) PRINT*,'Vert. tapering indexes and function', &
         k50,k100, k870, ksfc,taper

!!!!i
  range_tot=0.0_wp
  DO n=1,npattern_rn
    range_tot = range_tot + (range_rn(n)**2)
    IF(n == npattern_rn)range_tot = SQRT(range_tot)
    IF (my_cart_id == 0) THEN
    IF(n == npattern_rn)PRINT*,'Total range is ',range_tot
    IF (nseed_rn(n) == 0) THEN
      CALL set_seed_rand_numb (ydate_ini,iepsmem+(n-1)*2000,yd_random_stream(n), &
        koutseed)
    ELSE
      CALL construct ( yd_random_stream(n), seed=nseed_rn(n) )
    ENDIF
    ENDIF
!
! Coarse grid has to cover COSMO grid with an extra half grid spacing due to 
! the wind staggering and without two grid spacing due to the external 
! boundary frame that have values defined by lateral BC
!   dim_rn = 2 + INT(((idim_tot-1)*ddim + 0.5*ddim - 2.0 * ddim)/ddim_rn(n))
!
    ie_rn(n)=2+INT(((ie_tot-2.5_wp)*dlon)/dlon_rn(n))+1
    je_rn(n)=2+INT(((je_tot-2.5_wp)*dlat)/dlat_rn(n))+1
  ENDDO

  ALLOCATE (randnumb(maxval(ie_rn),maxval(je_rn),2,npattern_rn))

  fi_rn=exp(-zdt/tau_rn)
  sig_rn = stdv_rn(1)*sqrt(1.0_wp-fi_rn**2)

! Definition of correspondence between rn coarse and model grid
! SW corner is shifted randomly for each member 

  IF (my_cart_id == 0) THEN
    DO n=1,npattern_rn
      CALL random_number (zshift(1:2), yd_random_stream(n))
      WHERE (zshift == 1._wp)
        zshift = 0._wp
      END WHERE
      jshift(n)=INT(zshift(1)*dlat_rn(n)/dlat)
      ishift(n)=INT(zshift(2)*dlon_rn(n)/dlon)
      PRINT*,'Shift SW corner grid of ',jshift(n),ishift(n),' model points ',n,'ie_rn',ie_rn,'je_rn',je_rn
    ENDDO
  ENDIF
  IF (num_compute > 1) THEN
    CALL distribute_values (ishift,npattern_rn, 0,&
             imp_integers,    icomm_compute, izerrstat)
    CALL distribute_values (jshift,npattern_rn, 0,&
             imp_integers,    icomm_compute, izerrstat)
  ENDIF

  DO n=1,npattern_rn
    DO j=2,je_tot-1
      rlatwei_rn(:,j,n)=MOD(REAL(j+jshift(n)-2,wp)*dlat,dlat_rn(n))/dlat_rn(n)
      jlatbox_rn(:,j,n)=INT((REAL(j+jshift(n)-2,wp)*dlat)/dlat_rn(n))+1
    ENDDO
    DO i=2,ie_tot-1
      rlonwei_rn(i,:,n)=MOD(REAL(i+ishift(n)-2,wp)*dlon,dlon_rn(n))/dlon_rn(n)
      ilonbox_rn(i,:,n)=INT((REAL(i+ishift(n)-2,wp)*dlon)/dlon_rn(n))+1
    ENDDO
  ENDDO
 
  IF (my_cart_id == 0) THEN
    DO n=1,npattern_rn
      PRINT*,'n',n,'maxilonboxrn',maxval(ilonbox_rn(:,:,n)),'maxjlatboxrn',maxval(jlatbox_rn(:,:,n))
    ENDDO
  ENDIF

!!!!f

END SUBROUTINE init_rand_numb

!==============================================================================
!+ Module procedure in "src_stoch_phys" for drawing random numbers 
!------------------------------------------------------------------------------

SUBROUTINE gen_rand_numb(n_rn,np)

!----------------------------------------------------------------------------
!
! Description: Generate a set of random numbers 
!
!
!----------------------------------------------------------------------------

IMPLICIT NONE
INTEGER (KIND=iintegers), INTENT(IN)  :: n_rn,np

INTEGER (KIND=iintegers)  :: i, j, k, ierr
REAL (KIND=dp   ),ALLOCATABLE :: zglptsu(:,:)
LOGICAL :: lgauss_rn_loc
 

  lgauss_rn_loc = lgauss_rn
  IF(ltimar1_rn.AND..NOT.lgauss_rn)lgauss_rn_loc =.true.
  ALLOCATE(zglptsu(ie_rn(np),je_rn(np)))
  zglptsu=0._wp


!     A random number is defined for each coarse grid point (box), so that
!     model grid points inside the same box have the same random number
  DO j=1,je_rn(np)
    IF (lgauss_rn_loc) THEN

! N(0,1) random numbers (zglptsu) .
      CALL random_gauss (zglptsu(:,j), yd_random_stream(np))

! Transformation to N(0,stdv_rn) random numbers.
      IF(.not.ltimar1_rn)zglptsu(:,j)=zglptsu(:,j)*stdv_rn(np)

! Limitation of maximum/minimum values (+/- range_rn) for each pattern
      WHERE (zglptsu(:,j) > range_rn(np))
        zglptsu(:,j) = range_rn(np)      
      ELSEWHERE (zglptsu(:,j) < -range_rn(np))
        zglptsu(:,j) = -range_rn(np)      
      END WHERE
    ELSE
      CALL random_number (zglptsu(:,j), yd_random_stream(np))
      zglptsu(:,j) = range_rn(np)-2.0_wp*range_rn(np)*zglptsu(:,j)
    ENDIF
  ENDDO

  randnumb(1:ie_rn(np),1:je_rn(np),n_rn,np)=zglptsu(:,:)
  PRINT*,'RN MIN, MAX, MEAN ',minval(randnumb(1:ie_rn(np),1:je_rn(np),n_rn,np)), &
  maxval(randnumb(1:ie_rn(np),1:je_rn(np),n_rn,np)), &
  sum(zglptsu(:,:))/(ie_rn(np)*je_rn(np))
     
  DEALLOCATE(zglptsu)

END SUBROUTINE gen_rand_numb

!==============================================================================
!+ Module procedure in "src_stoch_phys" for interpolating random numbers
!------------------------------------------------------------------------------

SUBROUTINE int_rand_numb(ki1sc,ki1ec,ki2sc,ki2ec,ki3s,ki3e,&
                         kn_rn,np,prandnumbint)

!----------------------------------------------------------------------------
!
! Description: Horizontal interpolation and vertical tapering of random numbers 
!
!
!----------------------------------------------------------------------------

IMPLICIT NONE
INTEGER (KIND=iintegers), INTENT(IN) :: &
              ki1sc  , &! first  dimension of calculation, start index
              ki1ec  , &! first  dimension of calculation, end   index
              ki2sc  , &! second dimension of calculation, start index
              ki2ec  , &! second dimension of calculation, end   index
              ki3s   , &! third dimension of calculation,  start index
              ki3e      ! third dimension of calculation,  end   index
INTEGER (KIND=iintegers), INTENT(IN)  :: kn_rn,np       
REAL  (KIND=wp   ),     INTENT(OUT)  :: &
              prandnumbint(ki1sc:ki1ec,ki2sc:ki2ec,ki3s:ki3e)
REAL  (KIND=wp   )     :: zrand_help(ki1sc:ki1ec,ki2sc:ki2ec)
INTEGER (KIND=iintegers) :: i,j,k
INTEGER (KIND=iintegers) :: ii,jj

  zrand_help(ki1sc:ki1ec,ki2sc:ki2ec)=0._wp

! Horizontal interpolation of the random numbers (smoothed values)
  IF (lhorint_rn) THEN
    DO jj=ki2sc,ki2ec
      DO ii=ki1sc,ki1ec
        i=i_global(ii)
        j=j_global(jj)
        zrand_help(ii,jj)= &
           randnumb(ilonbox_rn(i,j,np),jlatbox_rn(i,j,np),kn_rn,np)* &
           (1-rlatwei_rn(i,j,np))*(1-rlonwei_rn(i,j,np))+ &
           randnumb(ilonbox_rn(i,j,np)+1,jlatbox_rn(i,j,np),kn_rn,np)* &
           rlonwei_rn(i,j,np)*(1-rlatwei_rn(i,j,np))+randnumb(ilonbox_rn(i,j,np), &
           jlatbox_rn(i,j,np)+1,kn_rn,np)*(1-rlonwei_rn(i,j,np))*rlatwei_rn(i,j,np)+ &
           randnumb(ilonbox_rn(i,j,np)+1,jlatbox_rn(i,j,np)+1,kn_rn,np) &
           *rlonwei_rn(i,j,np)*rlatwei_rn(i,j,np)
      ENDDO
    ENDDO
  ELSE
! No horizontal interpolation of the random numbers (constant values in boxes)
    DO jj=ki2sc,ki2ec
      DO ii=ki1sc,ki1ec
        i=i_global(ii)
        j=j_global(jj)
        zrand_help(ii,jj)= &
          randnumb(ilonbox_rn(i,j,np),jlatbox_rn(i,j,np),kn_rn,np)
      ENDDO
    ENDDO
  ENDIF

! Vertical tapering of the random numbers as prescribed
  IF(itype_vtaper_rn > 0)THEN
    DO j=ki2sc,ki2ec
      DO i=ki1sc,ki1ec
        DO k=ki3s,ki3e
          prandnumbint(i,j,k) = zrand_help(i,j)*taper(k)
        ENDDO
      ENDDO
    ENDDO
  ELSE
! No vertical tapering of the random numbers 
    DO j=ki2sc,ki2ec
      DO i=ki1sc,ki1ec
        DO k=ki3s,ki3e
          prandnumbint(i,j,k)= zrand_help(i,j)
        ENDDO
      ENDDO
    ENDDO
  ENDIF

END SUBROUTINE int_rand_numb

!==============================================================================
!+ Module procedure in "src_stoch_phys" for set the seed of random numbers
!==============================================================================

SUBROUTINE set_seed_rand_numb (ydate_ini,kconseed,yyd_random_stream, koutseed)

!----------------------------------------------------------------------------
!
! Description:
!
!      From IFS library of ECMWF (setran.F)
!      *set_seed_rand_numb* - Sets the seed for a random number stream
!      as a function of initial date and member number  
!      
!
! Method:
!
!      The seed is set to a function of the initial time of the run,
!      and of an input integer koutseed (member number).
!      For dates chosen at random, the seeds are approximately
!      uniformly distributed between 1 and HUGE(0). A highly nonlinear
!      function is used to reduce the possibility of correlations
!      between random sequences generated for different initial dates.
!
!----------------------------------------------------------------------------
 
IMPLICIT NONE
 
CHARACTER(LEN=10), INTENT(IN) :: ydate_ini
INTEGER  (KIND=iintegers),INTENT(IN)    :: kconseed 
TYPE(random_state_t), INTENT(INOUT):: yyd_random_stream
INTEGER  (KIND=iintegers),INTENT(OUT) :: koutseed
!      INPUT: kconseed                   - integer to control the seeding
!      OUTPUT: koutseed         - a seed constructed from kconseed and the model time

INTEGER  (KIND=iintegers) :: idigits, iradix, is, jdigit, iscale, &
                             iseed, nindat, nsssss
INTEGER  (KIND=iintegers) :: ndd, nmm, nccaa, naa, namd, ncth, nzzaa,   &
                             nzzmm, ncent, nyearc 
INTEGER  (KIND=iintegers) :: kgrdat, ksec, kaaaa, kmm, kdd, kss
INTEGER  (KIND=iintegers) :: kcent, kyearc, kmonth, kday, khh 
REAL (KIND=wp)     :: zirr1, zs, zt, ztim
REAL (KIND=wp)     :: rjudat
 
ndd(kgrdat)   = MOD(kgrdat,100)
nmm(kgrdat)   = MOD((kgrdat-ndd(kgrdat))/100,100)
nccaa(kgrdat) = kgrdat/10000
nzzaa(kaaaa,kmm) = kaaaa-( (1-SIGN(1,kmm-3))/2 )
nzzmm(kmm) = kmm+6*(1-SIGN(1,kmm-3))
rjudat(kaaaa,kmm,kdd) = 1720994.5_wp + REAL(2-nzzaa(kaaaa,kmm)/100  &
                        + (nzzaa(kaaaa,kmm)/100)/4 &
                        + INT(365.25_wp*REAL(nzzaa(kaaaa,kmm),wp))&
                        + INT(30.601_wp*REAL(nzzmm(kmm)+1,wp))&
                        + kdd,wp)

! End of header
!==============================================================================

  READ ( ydate_ini(1:4) , '(I4.4)' ) kaaaa
  READ ( ydate_ini(5:6) , '(I2.2)' ) kmm
  READ ( ydate_ini(7:8) , '(I2.2)' ) kdd
  READ ( ydate_ini(9:10), '(I2.2)' ) khh 
  NINDAT = kaaaa*10000+kmm*100+kdd
  NSSSSS = khh*3600
  iradix  = RADIX(ztim)
  idigits = DIGITS(ztim)
  zirr1 = 0.5_wp*(SQRT(5.0_wp)-1.0_wp)

!--- generate a unique number from the date and the input kconseed
 
  ztim = rjudat(nccaa(nindat),nmm(nindat),ndd(nindat)) &
       -1720994.5_wp + REAL(nsssss,wp)/86400.0_wp &
       -2581470.3_wp*kconseed  
 
!--- multiply by an irrational number to randomize the bits and scale
!--- to between 0 and 1.
 
  ztim = FRACTION(zirr1*ABS(ztim))
 
!--- reverse the bits
 
  zs = 0.0_wp
  zt = ztim
  DO jdigit = 1, idigits
    zt = zt*iradix
    is = int(zt)
    zt = zt-is
    zs = (zs+is)/iradix
  ENDDO

!--- Scale to an odd number between 0 and HUGE-100000000
!--- (Allow some headroom in order to use set_seed_rand_numb to set an initial 
!---  seed and then generate new seeds by incrementing.)
 
  iscale = (HUGE(iseed)-100000000)/2
  iseed = 1 + 2*INT( iscale*zs )

!--- set/output the seed
 
  koutseed = ISEED
 
  PRINT*,'Compute seed for ',nindat,nsssss,kconseed,' that is ',iseed

  CALL construct ( yyd_random_stream, seed=iseed )

END SUBROUTINE set_seed_rand_numb


!==============================================================================
!+ Module procedure in "src_stoch_phys" for checking neg./sup.sat. humidity 
!-----------------------------------------------------------------------------

SUBROUTINE apply_tqx_tend_adj(iitype_qxpert_rn,iitype_qxlim_rn,zpf,zt, &
                              zqv,zqc,zqi,zqr,zqs,zpertu,zttens, &
                 zqvtens,zqctens,zqitens,zqrtens,zqstens,lnopertu,zqg,zqgtens)
!------------------------------------------------------------------------------
!
!    itype_qxpert_rn define which hum variables tend. are perturbed
!                 0:    Only qv tendencies are perturbed
!                 1:    qv,qc,qi tendencies are perturbed
!                 2:    qv,qc,qi,qr,qs,qg tendencies are perturbed
!
!
!    itype_qxlim_rn  type of reduction/removal of the perturbation 
!                    in case of negative (qv, qc, qi) or 
!                    supersaturated (qv) values
!                 0:    No limitation of perturbed tendencies
!                 1:    If new qv values are negative and super-sat. -> T and qv
!                       tendencies are not perturbed
!                       If new qx (qc,qi,qr,qs,qg values) are negative -> qx
!                       tendencies are not perturbed
!                 2:    Compute the perturbed tendencies of T and qv, such that the
!                       new values do not exceed the limits for qv, and the limitation
!                       procedure does not introduce any bias by definition.
!                       The lower limit for qv is set to zero, the upper limit to the
!                       specific water vapour content at saturation over water.
!------------------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=iintegers) , INTENT(in) :: iitype_qxlim_rn, iitype_qxpert_rn

LOGICAL , INTENT(out) :: lnopertu

REAL (KIND=wp)     , INTENT(inout) :: &
  zttens,  & !
  zqvtens, & !
  zqctens, & !
  zqitens, & !
  zqrtens, & !
  zqstens    !

REAL (KIND=wp)     , INTENT(in) :: &
  zpertu,  & !
  zt,      & ! 
  zqv,     & !
  zqc,     & !
  zqi,     & !
  zqr,     & !
  zqs,     & !
  zpf        !

REAL (KIND=wp)     , INTENT(in), OPTIONAL :: zqg

REAL (KIND=wp)     , INTENT(inout), OPTIONAL :: zqgtens

REAL (KIND=wp)     :: ztx, zpv, zqvsp, fpvs, fqvs, zpx, ztten, &
                      zqvten, zqcten, zqiten, zqvn, ztn, zqcn, &
                      zqrten, zqsten, zqgten, zqrn, zqsn, zqgn, &
                      zqin       

REAL (KIND=wp)     , PARAMETER :: c0 = 0.0_wp, c1 = 1.0_wp

REAL (KIND=wp)     ::  &
  zlim_low ,& ! lower limit to new value of qv
  zlim_upp ,& ! upper limit to new value of qv
  zapertu  ,& ! test perturbation factor applied to tendency
  zfpert      ! perturbation factor applied to tendency, replaces 'zpertu'

! saturation vapour pressure over water (fpvs)
! and specific humidity at vapour saturation (fqvs)
  fpvs(ztx)     = b1*EXP( b2w*(ztx-b3)/(ztx-b4w) )
  fqvs (zpv,zpx) = rdv*zpv/( zpx - o_m_rdv*zpv )

  lnopertu = .false.

  IF(iitype_qxlim_rn == 0)THEN
    zttens = zttens * zpertu
    zqvtens = zqvtens * zpertu
    IF(iitype_qxpert_rn >= 1)THEN
      zqctens = zqctens * zpertu
      zqitens = zqitens * zpertu
    ENDIF
    IF(iitype_qxpert_rn == 2)THEN
      zqrtens = zqrtens * zpertu
      zqstens = zqstens * zpertu
      IF (PRESENT(zqg)) THEN
        zqgtens = zqgtens * zpertu
      ENDIF
    ENDIF

! Original IFS humidity check 
  ELSEIF(iitype_qxlim_rn == 1)THEN
    ztten = zttens * zpertu 
    zqvten = zqvtens * zpertu
    zqvn=zqv+zdt*zqvten
    ztn=zt+zdt*ztten
    zqvsp = fqvs(fpvs(ztn), zpf)
!    IF (zqvn < 0. .OR. (zqvn > zqvsp .AND.ztn > 248.15)) THEN
    IF (zqvn < 0._wp .OR. zqvn > zqvsp ) THEN
        lnopertu= .true.
    ELSE
        zttens = ztten
        zqvtens = zqvten
    ENDIF

    IF(iitype_qxpert_rn >= 1)THEN
      zqcten = zqctens * zpertu
      zqcn=zqc+zdt*zqcten
      IF (zqcn >= 0._wp) THEN
        zqctens = zqcten
      ENDIF

      zqiten = zqitens * zpertu
      zqin=zqi+zdt*zqiten
      IF (zqin >= 0._wp) THEN
        zqitens = zqiten
      ENDIF
    ENDIF

    IF(iitype_qxpert_rn == 2)THEN
      zqrten = zqrtens * zpertu
      zqrn=zqr+zdt*zqrten
      IF (zqrn >= 0._wp) THEN
        zqrtens = zqrten
      ENDIF

      zqsten = zqstens * zpertu
      zqsn=zqs+zdt*zqsten
      IF (zqsn >= 0._wp) THEN
        zqstens = zqsten
      ENDIF

      IF (PRESENT(zqg)) THEN
        zqgten = zqgtens * zpertu
        zqgn=zqg+zdt*zqgten
        IF (zqgn >= 0._wp) THEN
          zqgtens = zqgten
        ENDIF
      ENDIF
    ENDIF    

  ELSEIF(iitype_qxlim_rn == 2)THEN
! Written by Christoph Schraff. This code should avoid a dry bias of the model

    zfpert   = zpertu

    !  limit the tendency perturbation such that if the modulus of the limited
    !  tendency perturbation is added to the original tendency, then the limits
    !  are not exceeded  (it is assumed that 'zpertu > 0')
      !   test perturbation factor = 1.0 + modulus of perturbation random number
      zapertu = 1.0_wp + ABS( zpertu - 1.0_wp )
      !   lower limit on qv : 0.0
      zlim_low = c0
      !   upper limit on qv : qv_sat over water (not only imposed for T > -25 C)
      !   (qv_sat is related to the minimum of new T with or w/o perturbation;
      !   unless the safe approximation of taking the minimum was used here,
      !   an iterative procedure would be needed)
      zlim_upp = fqvs( fpvs( zt + zdt *MIN( zttens, zttens *zapertu ) ), zpf )
      !   new qv-value without perturbation
      zqvn = zqv + zdt *zqvtens
      !   if the unperturbed new qv-value exceeds the limits then do not perturb
      IF ((zqvn <= zlim_low) .OR. (zqvn >= zlim_upp)) THEN
        zfpert   = c1
        lnopertu = .true.
      !   check if adding the test tendency lets 'qv_n' exceed the upper limit
      ELSEIF ((zqv + zdt*zqvtens* zapertu > zlim_upp) .AND. (zqvtens > c0)) THEN
        !   zlim_upp - zqvn  : modulus of limited (adjusted) tendency perturbat.
        zfpert  =  c1  +  SIGN( (zlim_upp - zqvn) / zqvtens , zpertu - c1 )
      !   check if adding the test tendency lets 'qv_n' go below the lower limit
      ELSEIF ((zqv + zdt*zqvtens* zapertu < zlim_low) .AND. (zqvtens < c0)) THEN
        zfpert  =  c1  +  SIGN( (zqvn - zlim_low) / zqvtens , zpertu - c1 )
      ENDIF
    zttens  = zttens  * zfpert
    zqvtens = zqvtens * zfpert

    IF(iitype_qxpert_rn >= 1)THEN
    IF (      (zqc + zdt*zqctens         >= zlim_low)                          &
        .AND. (zqc + zdt*zqctens* zapertu < zlim_low) .AND. (zqctens < c0)) THEN
      zqctens = zqctens + SIGN( (zqc + zdt *zqctens - zlim_low) , zpertu - c1 )
    ELSEIF (zqc + zdt*zqctens* zapertu >= zlim_low) THEN
      zqctens = zqctens * zpertu
    ENDIF
    IF (      (zqi + zdt*zqitens         >= zlim_low)                          &
        .AND. (zqi + zdt*zqitens* zapertu < zlim_low) .AND. (zqitens < c0)) THEN
      zqitens = zqitens + SIGN( (zqi + zdt *zqitens - zlim_low) , zpertu - c1 )
    ELSEIF (zqi + zdt*zqitens* zapertu >= zlim_low) THEN
      zqitens = zqitens * zpertu
    ENDIF
    ENDIF
    
    IF(iitype_qxpert_rn == 2)THEN
      IF (      (zqr + zdt*zqrtens         >= zlim_low)                      &
          .AND. (zqr + zdt*zqrtens* zapertu < zlim_low) .AND. (zqrtens < c0)) THEN
        zqrtens = zqrtens + SIGN( (zqr + zdt *zqrtens - zlim_low) , zpertu - c1 )
      ELSEIF (zqr + zdt*zqrtens* zapertu >= zlim_low) THEN
        zqrtens = zqrtens * zpertu
      ENDIF
      IF (      (zqs + zdt*zqstens         >= zlim_low)                      &
          .AND. (zqs + zdt*zqstens* zapertu < zlim_low) .AND. (zqstens < c0)) THEN
        zqstens = zqstens + SIGN( (zqs + zdt *zqstens - zlim_low) , zpertu - c1 )
      ELSEIF (zqs + zdt*zqstens* zapertu >= zlim_low) THEN
        zqstens = zqstens * zpertu
      ENDIF
      IF (PRESENT(zqg)) THEN
        IF (      (zqg + zdt*zqgtens         >= zlim_low)                      &
          .AND. (zqg + zdt*zqgtens* zapertu < zlim_low) .AND. (zqgtens < c0)) THEN
          zqgtens = zqgtens + SIGN( (zqg + zdt *zqgtens - zlim_low) , zpertu - c1 )
        ELSEIF (zqg + zdt*zqgtens* zapertu >= zlim_low) THEN
          zqgtens = zqgtens * zpertu
        ENDIF
      ENDIF
    ENDIF   
  ENDIF

END SUBROUTINE apply_tqx_tend_adj

END MODULE stoch_physics
