MODULE rttov7_dp_wrapper

USE data_parameters, ONLY: wp, sp, dp, iintegers

CONTAINS

SUBROUTINE RTTOV7_RTTVI_new_wrapper(                                   &
  kerr    , kppf    , kpnsat   , kplev     , kpch    , kpchus,         &
  kpnav   , kpnsav  , kpnssv   , kpncv     ,                           &
  nrttovid, platform, satellite, instrument, numchans,                 &
  preslev , otmin   , otmax    , oqmin     , oqmax   , oozmin, oozmax, &
  ivch    , niu1    ,                                                  &
  jpch    , jpnsat  , jplev                                            )

  IMPLICIT NONE

  ! Integer arguments (wrapper only)
  INTEGER(KIND=iintegers),INTENT(IN)    :: jpch
  INTEGER(KIND=iintegers),INTENT(IN)    :: jpnsat
  INTEGER(KIND=iintegers),INTENT(IN)    :: jplev

  ! Integer arguments
  INTEGER(KIND=iintegers),INTENT(IN)    :: nrttovid
  INTEGER(KIND=iintegers),INTENT(IN)    :: platform   (*)
  INTEGER(KIND=iintegers),INTENT(IN)    :: satellite  (*)
  INTEGER(KIND=iintegers),INTENT(IN)    :: instrument (*)
  INTEGER(KIND=iintegers),INTENT(INOUT) :: numchans   (*)
  INTEGER(KIND=iintegers),INTENT(INOUT) :: ivch       (jpch,jpnsat)
  INTEGER(KIND=iintegers),INTENT(INOUT) :: niu1       (*)
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kerr
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kppf
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kpnsat
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kplev
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kpch
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kpchus
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kpnav
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kpnsav
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kpnssv
  INTEGER(KIND=iintegers),INTENT(OUT)   :: kpncv

  ! Real arguments (working precision)
  REAL(KIND=wp),INTENT(OUT) :: preslev(jplev,nrttovid)
  REAL(KIND=wp),INTENT(OUT) :: otmin  (jplev,nrttovid)
  REAL(KIND=wp),INTENT(OUT) :: otmax  (jplev,nrttovid)
  REAL(KIND=wp),INTENT(OUT) :: oqmin  (jplev,nrttovid)
  REAL(KIND=wp),INTENT(OUT) :: oqmax  (jplev,nrttovid)
  REAL(KIND=wp),INTENT(OUT) :: oozmin (jplev,nrttovid)
  REAL(KIND=wp),INTENT(OUT) :: oozmax (jplev,nrttovid)

#ifdef RTTOV7

  ! Local real arrays (double precision)
  REAL(KIND=dp) :: preslev8(jplev,nrttovid)
  REAL(KIND=dp) :: otmin8  (jplev,nrttovid)
  REAL(KIND=dp) :: otmax8  (jplev,nrttovid)
  REAL(KIND=dp) :: oqmin8  (jplev,nrttovid)
  REAL(KIND=dp) :: oqmax8  (jplev,nrttovid)
  REAL(KIND=dp) :: oozmin8 (jplev,nrttovid)
  REAL(KIND=dp) :: oozmax8 (jplev,nrttovid)

  IF (wp /= dp) THEN
    preslev8 (:,:) = 0.0_dp
    otmin8   (:,:) = 0.0_dp
    otmax8   (:,:) = 0.0_dp
    oqmin8   (:,:) = 0.0_dp
    oqmax8   (:,:) = 0.0_dp
    oozmin8  (:,:) = 0.0_dp
    oozmax8  (:,:) = 0.0_dp
  END IF

  CALL RTTOV7_RTTVI_new(                                                    &
    kerr    , kppf    , kpnsat   , kplev     , kpch    , kpchus,            &
    kpnav   , kpnsav  , kpnssv   , kpncv     ,                              &
    nrttovid, platform, satellite, instrument, numchans,                    &
    preslev8, otmin8  , otmax8   , oqmin8    , oqmax8  , oozmin8, oozmax8,  &
    ivch    , niu1                                                          )

  IF (wp == dp) THEN
    preslev (:,:) = preslev8 (:,:)
    otmin   (:,:) = otmin8   (:,:)
    otmax   (:,:) = otmax8   (:,:)
    oqmin   (:,:) = oqmin8   (:,:)
    oqmax   (:,:) = oqmax8   (:,:)
    oozmin  (:,:) = oozmin8  (:,:)
    oozmax  (:,:) = oozmax8  (:,:)
  ELSE
    preslev (:,:) = 0.0_wp
    otmin   (:,:) = 0.0_wp
    otmax   (:,:) = 0.0_wp
    oqmin   (:,:) = 0.0_wp
    oqmax   (:,:) = 0.0_wp
    oozmin  (:,:) = 0.0_wp
    oozmax  (:,:) = 0.0_wp
    preslev (:,:) = REAL(preslev8 (:,:),wp)
    otmin   (:,:) = REAL(otmin8   (:,:),wp)
    otmax   (:,:) = REAL(otmax8   (:,:),wp)
    oqmin   (:,:) = REAL(oqmin8   (:,:),wp)
    oqmax   (:,:) = REAL(oqmax8   (:,:),wp)
    oozmin  (:,:) = REAL(oozmin8  (:,:),wp)
    oozmax  (:,:) = REAL(oozmax8  (:,:),wp)
  END IF

#endif

END SUBROUTINE RTTOV7_RTTVI_new_wrapper

SUBROUTINE RTTOV7_supsat_vec_wrapper(temp, wv, pres, ndim, nstart, nend)

  IMPLICIT NONE

  ! Integer arguments 
  INTEGER(KIND=iintegers),INTENT(IN)  :: ndim, nstart, nend

  ! Real arguments (working precision)
  REAL(KIND=wp),INTENT(IN)  :: temp (ndim)
  REAL(KIND=wp),INTENT(IN)  :: pres (ndim)
  REAL(KIND=wp),INTENT(OUT) :: wv   (ndim)

#ifdef RTTOV7

  ! Local arrays (double precision)
  REAL(KIND=dp) :: temp8 (ndim)
  REAL(KIND=dp) :: pres8 (ndim)
  REAL(KIND=dp) :: wv8   (ndim)

  IF (wp == dp) THEN
    temp8 (:) = temp (:)
    pres8 (:) = pres (:)
  ELSE
    temp8 (:) = 0.0_dp
    pres8 (:) = 0.0_dp
    temp8 (:) = REAL(temp (:),dp)
    pres8 (:) = REAL(pres (:),dp)
    wv8   (:) = 0.0_dp
  END IF

  CALL RTTOV7_supsat_vec(temp8, wv8, pres8, ndim, nstart, nend)

  IF (wp == dp) THEN
    wv (:) = wv8 (:)
  ELSE
    wv (:) = 0.0_wp
    wv (:) = REAL(wv8 (:),wp)
  END IF

#endif

END SUBROUTINE RTTOV7_supsat_vec_wrapper

SUBROUTINE RTTOV7_spline_vec_wrapper(xi,yi,xo,yo,as,index,nimax,no,idim,idims,idime,ii)

  IMPLICIT NONE

  ! Integer arguments
  INTEGER(KIND=iintegers),INTENT(IN) :: nimax
  INTEGER(KIND=iintegers),INTENT(IN) :: no
  INTEGER(KIND=iintegers),INTENT(IN) :: idim
  INTEGER(KIND=iintegers),INTENT(IN) :: idims
  INTEGER(KIND=iintegers),INTENT(IN) :: idime
  INTEGER(KIND=iintegers),INTENT(IN) :: ii
  INTEGER(KIND=iintegers),INTENT(IN) :: index(idim)
  
  ! Real arguments (working precision)
  REAL(KIND=wp),INTENT(IN)    :: xi (idim,nimax)
  REAL(KIND=wp),INTENT(IN)    :: yi (idim,nimax)
  REAL(KIND=wp),INTENT(IN)    :: xo (no)
  REAL(KIND=wp),INTENT(INOUT) :: as (idim,5,nimax)
  REAL(KIND=wp),INTENT(OUT)   :: yo (idim,no)

#ifdef RTTOV7

  ! Local real arrays (double precision)
  REAL(KIND=dp) :: xi8 (idim,nimax)
  REAL(KIND=dp) :: yi8 (idim,nimax)
  REAL(KIND=dp) :: xo8 (no)
  REAL(KIND=dp) :: as8 (idim,5,nimax)
  REAL(KIND=dp) :: yo8 (idim,no)

  IF (wp == dp) THEN
    xi8 (:,:)   = xi (:,:)
    yi8 (:,:)   = yi (:,:)
    xo8 (:)     = xo (:)  
    as8 (:,:,:) = as (:,:,:)
  ELSE
    xi8 (:,:)   = 0.0_dp
    yi8 (:,:)   = 0.0_dp
    xo8 (:)     = 0.0_dp
    as8 (:,:,:) = 0.0_dp
    xi8 (:,:)   = REAL(xi (:,:)  ,dp)
    yi8 (:,:)   = REAL(yi (:,:)  ,dp)
    xo8 (:)     = REAL(xo (:)    ,dp)
    as8 (:,:,:) = REAL(as (:,:,:),dp)
    yo8 (:,:)   = 0.0_dp
  END IF

  CALL RTTOV7_spline_vec(xi8,yi8,xo8,yo8,as8,index,nimax,no,idim,idims,idime,ii)

  IF (wp == dp) THEN
    as (:,:,:) = as8 (:,:,:)
    yo (:,:)   = yo8 (:,:)
  ELSE
    as (:,:,:) = 0.0_wp
    yo (:,:)   = 0.0_wp
    as (:,:,:) = REAL(as8 (:,:,:),wp)
    yo (:,:)   = REAL(yo8 (:,:)  ,wp)
  END IF 

#endif

END SUBROUTINE RTTOV7_spline_vec_wrapper

SUBROUTINE RTTOV7_RTTOVCLD_wrapper(                                  &
  knpf   , klenpf  , klevm  ,                                        &
  ppres  , pangl   , pangs  , ksurf  , ksat   , knchpf,              &
  kchan  , kprof   , pav    , psav   , pssv   , pcvm  , pap, paph  , &
  pemis  , ifail   , prad   , ptb    , pradcld, ptbcld, tau, tausfc, &
  pradovm, pcldemis, pait   , pais   ,                               &
  njpnsat, njplev  , njpnssv, njpnav , njpnsav)

  IMPLICIT NONE

  ! Integer arguments (wrapper only)
  INTEGER(KIND=iintegers),INTENT(IN)  :: njpnsat
  INTEGER(KIND=iintegers),INTENT(IN)  :: njplev
  INTEGER(KIND=iintegers),INTENT(IN)  :: njpnssv
  INTEGER(KIND=iintegers),INTENT(IN)  :: njpnav
  INTEGER(KIND=iintegers),INTENT(IN)  :: njpnsav


  ! Integer arguments
  INTEGER(KIND=iintegers),INTENT(IN)  :: knpf
  INTEGER(KIND=iintegers),INTENT(IN)  :: klenpf
  INTEGER(KIND=iintegers),INTENT(IN)  :: ksat
  INTEGER(KIND=iintegers),INTENT(IN)  :: knchpf
  INTEGER(KIND=iintegers),INTENT(IN)  :: klevm
  INTEGER(KIND=iintegers),INTENT(IN)  :: kchan (knchpf)
  INTEGER(KIND=iintegers),INTENT(IN)  :: kprof (knchpf)
  INTEGER(KIND=iintegers),INTENT(IN)  :: ksurf (knpf)
  INTEGER(KIND=iintegers),INTENT(OUT) :: ifail (knpf,njpnsat)

  ! Real arguments (working precision)
  REAL(KIND=wp),INTENT(IN)    :: ppres    (njplev)
  REAL(KIND=wp),INTENT(IN)    :: pangl    (knpf)
  REAL(KIND=wp),INTENT(IN)    :: pangs    (knpf)
  REAL(KIND=wp),INTENT(IN)    :: pav      (knpf,njplev,njpnav)
  REAL(KIND=wp),INTENT(IN)    :: psav     (knpf,njpnsav)
  REAL(KIND=wp),INTENT(IN)    :: pssv     (knpf,njpnssv)
  REAL(KIND=wp),INTENT(IN)    :: pap      (knpf,klevm)
  REAL(KIND=wp),INTENT(IN)    :: paph     (knpf,klevm+1)
  REAL(KIND=wp),INTENT(IN)    :: pcvm     (knpf,klevm,4)
  REAL(KIND=wp),INTENT(INOUT) :: pemis    (knchpf)
  REAL(KIND=wp),INTENT(OUT)   :: prad     (knchpf)
  REAL(KIND=wp),INTENT(OUT)   :: ptb      (knchpf)
  REAL(KIND=wp),INTENT(OUT)   :: pradcld  (knchpf)
  REAL(KIND=wp),INTENT(OUT)   :: ptbcld   (knchpf)
  REAL(KIND=wp),INTENT(OUT)   :: tau      (knchpf,njplev)
  REAL(KIND=wp),INTENT(OUT)   :: tausfc   (knchpf)
  REAL(KIND=wp),INTENT(OUT)   :: pradovm  (knchpf,2*klevm+2)   
  REAL(KIND=wp),INTENT(OUT)   :: pcldemis (knchpf,klevm)
  REAL(KIND=wp),INTENT(OUT)   :: pait     (knchpf,klevm+1)
  REAL(KIND=wp),INTENT(OUT)   :: pais     (knchpf,klevm+1)

#ifdef RTTOV7

  ! Local arrays (double precision)
  REAL(KIND=dp) :: ppres8    (njplev)
  REAL(KIND=dp) :: pangl8    (knpf)
  REAL(KIND=dp) :: pangs8    (knpf)
  REAL(KIND=dp) :: pav8      (knpf,njplev,njpnav)
  REAL(KIND=dp) :: psav8     (knpf,njpnsav)
  REAL(KIND=dp) :: pssv8     (knpf,njpnssv)
  REAL(KIND=dp) :: pap8      (knpf,klevm)
  REAL(KIND=dp) :: paph8     (knpf,klevm+1)
  REAL(KIND=dp) :: pcvm8     (knpf,klevm,4)
  REAL(KIND=dp) :: pemis8    (knchpf)
  REAL(KIND=dp) :: prad8     (knchpf)
  REAL(KIND=dp) :: ptb8      (knchpf)
  REAL(KIND=dp) :: pradcld8  (knchpf)
  REAL(KIND=dp) :: ptbcld8   (knchpf)
  REAL(KIND=dp) :: tau8      (knchpf,njplev)
  REAL(KIND=dp) :: tausfc8   (knchpf)
  REAL(KIND=dp) :: pradovm8  (knchpf,2*klevm+2)   
  REAL(KIND=dp) :: pcldemis8 (knchpf,klevm)
  REAL(KIND=dp) :: pait8     (knchpf,klevm+1)
  REAL(KIND=dp) :: pais8     (knchpf,klevm+1)

  IF (wp == dp) THEN
    ppres8 (:)      = REAL(ppres (:),dp)
    pangl8 (:)      = REAL(pangl (:),dp)
    pangs8 (:)      = REAL(pangs (:),dp)
    pav8   (:,:,:)  = REAL(pav   (:,:,:),dp)
    psav8  (:,:)    = REAL(psav  (:,:),dp)
    pssv8  (:,:)    = REAL(pssv  (:,:),dp)
    pap8   (:,:)    = REAL(pap   (:,:),dp)
    paph8  (:,:)    = REAL(paph  (:,:),dp)
    pcvm8  (:,:,:)  = REAL(pcvm  (:,:,:),dp)
    pemis8 (:)      = REAL(pemis (:),dp)
  ELSE
    ppres8 (:)      = 0.0_dp
    pangl8 (:)      = 0.0_dp
    pangs8 (:)      = 0.0_dp
    pav8   (:,:,:)  = 0.0_dp
    psav8  (:,:)    = 0.0_dp
    pssv8  (:,:)    = 0.0_dp
    pap8   (:,:)    = 0.0_dp
    paph8  (:,:)    = 0.0_dp
    pcvm8  (:,:,:)  = 0.0_dp
    pemis8 (:)      = 0.0_dp
    ppres8 (:)      = REAL(ppres (:)    ,dp)
    pangl8 (:)      = REAL(pangl (:)    ,dp)
    pangs8 (:)      = REAL(pangs (:)    ,dp)
    pav8   (:,:,:)  = REAL(pav   (:,:,:),dp)
    psav8  (:,:)    = REAL(psav  (:,:)  ,dp)
    pssv8  (:,:)    = REAL(pssv  (:,:)  ,dp)
    pap8   (:,:)    = REAL(pap   (:,:)  ,dp)
    paph8  (:,:)    = REAL(paph  (:,:)  ,dp)
    pcvm8  (:,:,:)  = REAL(pcvm  (:,:,:),dp)
    pemis8 (:)      = REAL(pemis (:)    ,dp)
    prad8     (:)   = 0.0_dp
    ptb8      (:)   = 0.0_dp
    pradcld8  (:)   = 0.0_dp
    ptbcld8   (:)   = 0.0_dp
    tau8      (:,:) = 0.0_dp
    tausfc8   (:)   = 0.0_dp
    pradovm8  (:,:) = 0.0_dp
    pcldemis8 (:,:) = 0.0_dp
    pait8     (:,:) = 0.0_dp
    pais8     (:,:) = 0.0_dp
  END IF

  CALL RTTOV7_RTTOVCLD(                                                   &
    knpf    , klenpf   , klevm ,                                          &
    ppres8  , pangl8   , pangs8, ksurf, ksat    , knchpf ,                &
    kchan   , kprof    , pav8  , psav8, pssv8   , pcvm8  , pap8, paph8  , &
    pemis8  , ifail    , prad8 , ptb8 , pradcld8, ptbcld8, tau8, tausfc8, &
    pradovm8, pcldemis8, pait8 , pais8                                    )

  IF (wp == dp) THEN
    pemis    (:)      = REAL(pemis8    (:),wp)
    prad     (:)      = REAL(prad8     (:),wp)
    ptb      (:)      = REAL(ptb8      (:),wp)
    pradcld  (:)      = REAL(pradcld8  (:),wp)
    ptbcld   (:)      = REAL(ptbcld8   (:),wp)
    tau      (:,:)    = REAL(tau8      (:,:),wp)
    tausfc   (:)      = REAL(tausfc8   (:),wp)
    pradovm  (:,:)    = REAL(pradovm8  (:,:),wp) 
    pcldemis (:,:)    = REAL(pcldemis8 (:,:),wp)
    pait     (:,:)    = REAL(pait8     (:,:),wp)
    pais     (:,:)    = REAL(pais8     (:,:),wp)
  ELSE
    pemis    (:)      = 0.0_wp
    prad     (:)      = 0.0_wp
    ptb      (:)      = 0.0_wp
    pradcld  (:)      = 0.0_wp
    ptbcld   (:)      = 0.0_wp
    tau      (:,:)    = 0.0_wp
    tausfc   (:)      = 0.0_wp
    pradovm  (:,:)    = 0.0_wp
    pcldemis (:,:)    = 0.0_wp
    pait     (:,:)    = 0.0_wp
    pais     (:,:)    = 0.0_wp
    pemis    (:)      = REAL(pemis8    (:)  ,wp)
    prad     (:)      = REAL(prad8     (:)  ,wp)
    ptb      (:)      = REAL(ptb8      (:)  ,wp)
    pradcld  (:)      = REAL(pradcld8  (:)  ,wp)
    ptbcld   (:)      = REAL(ptbcld8   (:)  ,wp)
    tau      (:,:)    = REAL(tau8      (:,:),wp)
    tausfc   (:)      = REAL(tausfc8   (:)  ,wp)
    pradovm  (:,:)    = REAL(pradovm8  (:,:),wp)
    pcldemis (:,:)    = REAL(pcldemis8 (:,:),wp)
    pait     (:,:)    = REAL(pait8     (:,:),wp)
    pais     (:,:)    = REAL(pais8     (:,:),wp)
  END IF

#endif

END SUBROUTINE RTTOV7_RTTOVCLD_wrapper 

END MODULE rttov7_dp_wrapper
