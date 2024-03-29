!+ Data module for all data arrays, that are used by the radiation scheme
!-------------------------------------------------------------------------------

MODULE data_radiation

!-------------------------------------------------------------------------------
!
! Description:
!  This module declares and initializes all parametric data and data arrays 
!  that are used by the radiation scheme (i.e. the FESFT-routine). 
!  This data module replaces the COMMON blocks and local data arrays that are 
!  used in the original Fortran77 version of the FESFT-code 
!  provided by Bodo Ritter.
!
! Current Code Owner: DWD, Bodo Ritter
!  phone:  +49  69  8062 2703
!  fax:    +49  69  8062 3721
!  email:  Bodo.Ritter@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.1        1998/03/11 Guenter Doms
!  Initial release
! 3.6        2003/12/11 Ulrich Schaettler
!  Editorial changes (some lines have been too long)
! 3.21       2006/12/04 Ulrich Schaettler, Thorsten Reinhardt
!  New dimensions for running the radiation on a coarser grid
! V4_9         2009/07/16 Ulrich Schaettler
!  Added storage for slope of solar albedo with respect to soil water content
! V4_11        2009/11/30 Juergen Helmert
!  Definition of optical thickness variables for the different
!  possibilities according to setting of itype_aerosol
! V4_12        2010/05/11 Juergen Helmert
!  Unification of variables for optical thickness
! V4_13        2010/05/11 Michael Gertz
!  Adaptions to SVN
! V4_18        2011/05/26 Ulrich Schaettler
!  Changed the code owner
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!=======================================================================
!
! Declarations:
!
! Modules used:
USE data_parameters , ONLY :   &
           wp,       &! KIND-type parameters for real variables
           iintegers  ! kind-type parameter for "normal" integer variables

!-------------------------------------------------------------------------------

IMPLICIT NONE

! Local Declarations:

  INTEGER (KIND=iintegers), PRIVATE ::    &
    i ! index for data initalization

! Global (i.e. public) Declarations:

! Global Parameters
! -----------------

  INTEGER (KIND=iintegers), PARAMETER  ::    &

    ! Parameters for spectral resolution of radiative transfer code
    jpsol  = 3 ,     & ! Number of solar spectral intervals
    jpther = 5 ,     & ! Number of thermal spectral intervals
    jpspec = 8 ,     & ! (= jpsol+jpther) Total number of spectral intervals

    ! Parameters for gas absorbtion                                
    jpgas  = 3 ,     & ! Number of gases considered by radative transfer code
    jpabsc = 7         ! Maximum number of absorbtion coefficients in each   
                       ! spectral interval

! Global dimensions
! -----------------

  INTEGER (KIND=iintegers)             ::    &
    idim_rad,        & ! ie-dimension of the coarser grid
    istartrad,       & ! start- and end-indices for computing the radiation
    iendrad,         & !   (when running on a coarser grid, the input values for
    jstartrad,       & !    fesft are computed on all grid points, to compute an
    jendrad,         & !    average input over several grid points)
    iendparrad,      & ! end-index just for preparations
    jendparrad         ! end-index just for preparations

! Global Arrays and Scalars
! -------------------------

! 1. absorption properties of atmospheric gases (..,1=h2o; ..,2 =CO2; ..,3=O3)
! --------------------------------------------

  REAL  (KIND=wp)         ::    &
    coai (jpabsc,jpspec,jpgas), & ! weigthing coefficients
    cobi (jpabsc,jpspec,jpgas), & ! absorption coefficients
    coali(jpabsc,jpspec,jpgas), & ! pressure correction coefficients
    cobti(jpabsc,jpspec,jpgas), & ! temperature correction coefficients
    pgas (       jpspec,jpgas), & ! reference pressure
    tgas (       jpspec,jpgas)    ! reference temperature

  INTEGER (KIND=iintegers)::    &
    ncgas(jpspec,3),            & ! number of coefficients for each spectral  
                                  ! interval and gas (maximum=7)
    nfast(jpspec)                 ! control variable for choice between 
                                  ! ESFT/FESFT in each spectral interval

  ! Initialization of above arrays

  DATA nfast / 1, 1, 1, 1, 1, 1, 1, 1/
                                                                        
  ! coefficients for H2O in spectral interval 1
  DATA ncgas(1,1) /7/ ; DATA pgas(1,1) /101325.000_wp/ ; DATA tgas(1,1) /281.700_wp/
  DATA (coai (i,1,1),i=1,7)/ .114190E-01_wp,  .600200E-01_wp,  .111201E+00_wp,  .123340E+00_wp,  .902500E-01_wp,  .199632E+00_wp,  .404139E+00_wp/
  DATA (cobi (i,1,1),i=1,7)/ .209894E+02_wp,  .208930E+01_wp,  .184502E+00_wp,  .217771E-01_wp,  .279254E-02_wp,  .463447E-03_wp,  .000000E+00_wp/
  DATA (coali(i,1,1),i=1,7)/ .285370E-01_wp,  .688620E+00_wp,  .766031E+00_wp,  .833136E+00_wp,  .773491E+00_wp,  .768818E+00_wp,  .100000E+01_wp/
  DATA (cobti(i,1,1),i=1,7)/ .473006E+00_wp, -.468639E+00_wp, -.599601E+00_wp, -.162223E+01_wp, -.176002E+01_wp, -.153131E+01_wp,  .100000E+01_wp/

  ! coefficients for H2O in spectral interval 2
  DATA ncgas(2,1) /7/ ; DATA pgas(2,1) /101325.000_wp/ ; DATA tgas(2,1) /281.700_wp/
  DATA (coai (i,2,1),i=1,7)/ .201500E-02_wp,  .268530E-01_wp,  .598920E-01_wp,  .907740E-01_wp,  .102284E+00_wp,  .217298E+00_wp,  .500884E+00_wp/
  DATA (cobi (i,2,1),i=1,7)/ .508159E+01_wp,  .519996E+00_wp,  .465586E-01_wp,  .891251E-02_wp,  .159221E-02_wp,  .374973E-03_wp,  .000000E+00_wp/
  DATA (coali(i,2,1),i=1,7)/-.482300E-02_wp,  .529161E+00_wp,  .587751E+00_wp,  .756567E+00_wp,  .774607E+00_wp,  .733883E+00_wp,  .100000E+01_wp/
  DATA (cobti(i,2,1),i=1,7)/ .499755E+00_wp, -.529716E+00_wp, -.177970E-01_wp, -.746447E+00_wp, -.106191E+00_wp, -.727589E+00_wp,  .100000E+01_wp/
                                                                        
  ! coefficients for H2O in spectral interval 3
  DATA ncgas(3,1) /3/ ; DATA pgas(3,1) /101325.000_wp/ ; DATA tgas(3,1) /281.700_wp/
  DATA (coai (i,3,1),i=1,7)/ .566900E-02_wp,  .346720E-01_wp,  .959659E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,3,1),i=1,7)/ .716144E-03_wp,  .256449E-03_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,3,1),i=1,7)/-.281669E+00_wp,  .611673E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,3,1),i=1,7)/ .418657E+00_wp,  .405230E-01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for H2O in spectral interval 4
  DATA ncgas(4,1) /7/ ; DATA pgas(4,1) / 50662.500_wp/ ; DATA tgas(4,1) /255.800_wp/
  DATA (coai (i,4,1),i=1,7)/ .641200E-02_wp,  .362630E-01_wp,  .147064E+00_wp,  .285387E+00_wp,  .246376E+00_wp,  .226899E+00_wp,  .515980E-01_wp/
  DATA (cobi (i,4,1),i=1,7)/ .298538E+04_wp,  .139959E+03_wp,  .152405E+02_wp,  .144212E+01_wp,  .183654E+00_wp,  .283139E-01_wp,  .409261E-02_wp/
  DATA (coali(i,4,1),i=1,7)/ .183780E-01_wp,  .410557E+00_wp,  .808897E+00_wp,  .897332E+00_wp,  .932149E+00_wp,  .978389E+00_wp,  .100000E+01_wp/
  DATA (cobti(i,4,1),i=1,7)/ .413777E+00_wp, -.663704E+00_wp, -.953789E+00_wp, -.111883E+01_wp, -.156269E+01_wp, -.330557E+01_wp,  .100000E+01_wp/
                                                                        
  ! coefficients for H2O in spectral interval 5
  DATA ncgas(5,1) /7/ ; DATA pgas(5,1) / 86126.250_wp/ ; DATA tgas(5,1) /281.700_wp/
  DATA (coai (i,5,1),i=1,7)/ .147700E-02_wp,  .345020E-01_wp,  .865590E-01_wp,  .144237E+00_wp,  .218089E+00_wp,  .339440E+00_wp,  .175697E+00_wp/
  DATA (cobi (i,5,1),i=1,7)/ .126765E+02_wp,  .149624E+01_wp,  .147571E+00_wp,  .368129E-01_wp,  .792501E-02_wp,  .208930E-02_wp,  .000000E+00_wp/
  DATA (coali(i,5,1),i=1,7)/-.414300E-02_wp,  .504464E+00_wp,  .670985E+00_wp,  .920940E+00_wp,  .889089E+00_wp,  .966028E+00_wp,  .100000E+01_wp/
  DATA (cobti(i,5,1),i=1,7)/ .454691E+00_wp, -.423980E+01_wp, -.340869E+01_wp, -.410896E+01_wp, -.268068E+01_wp, -.250967E+01_wp,  .100000E+01_wp/

  ! coefficients for H2O in spectral interval 6
  DATA ncgas(6,1) /4/ ; DATA pgas(6,1) / 86126.250_wp/ ; DATA tgas(6,1) /281.700_wp/
  DATA (coai (i,6,1),i=1,7)/ .653200E-02_wp,  .700040E-01_wp,  .243768E+00_wp,  .679696E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,6,1),i=1,7)/ .632412E+00_wp,  .473151E-02_wp,  .163305E-02_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,6,1),i=1,7)/ .794801E+00_wp,  .306898E+00_wp,  .100000E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,6,1),i=1,7)/-.100000E+02_wp, -.219711E+01_wp, -.369325E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for H2O in spectral interval 7
  DATA ncgas(7,1) /3/ ; DATA pgas(7,1) / 86126.250_wp/ ; DATA tgas(7,1) /281.700_wp/
  DATA (coai (i,7,1),i=1,7)/ .138610E-01_wp,  .226595E+00_wp,  .759544E+00_wp, 0.000000E+00_wp,  .000000E+00_wp, 0.000000E+00_wp, 0.000000E+00_wp/
  DATA (cobi (i,7,1),i=1,7)/ .425598E-02_wp,  .155239E-02_wp, 0.000000E+00_wp, 0.000000E+00_wp,  .000000E+00_wp, 0.000000E+00_wp, 0.000000E+00_wp/
  DATA (coali(i,7,1),i=1,7)/-.736171E+00_wp,  .805828E+00_wp,  .100000E+01_wp, 0.000000E+00_wp,  .000000E+00_wp, 0.000000E+00_wp, 0.000000E+00_wp/
  DATA (cobti(i,7,1),i=1,7)/ .308301E+00_wp, -.267573E+01_wp,  .100000E+01_wp, 0.000000E+00_wp,  .000000E+00_wp, 0.000000E+00_wp, 0.000000E+00_wp/
  ! coefficients for H2O in spectral interval 8
  DATA ncgas(8,1) /7/ ; DATA pgas(8,1) / 75993.750_wp/ ; DATA tgas(8,1) /281.700_wp/
  DATA (coai (i,8,1),i=1,7)/ .181840E-01_wp,  .106586E+00_wp,  .237611E+00_wp,  .241085E+00_wp,  .157304E+00_wp,  .178767E+00_wp,  .604640E-01_wp/
  DATA (cobi (i,8,1),i=1,7)/ .822243E+02_wp,  .979490E+01_wp,  .905733E+00_wp,  .140281E+00_wp,  .193197E-01_wp,  .320627E-02_wp,  .000000E+00_wp/
  DATA (coali(i,8,1),i=1,7)/-.126888E+00_wp,  .701873E+00_wp,  .834941E+00_wp,  .920550E+00_wp,  .849506E+00_wp,  .931957E+00_wp,  .100000E+01_wp/
  DATA (cobti(i,8,1),i=1,7)/ .384580E+00_wp, -.187972E+01_wp, -.226834E+01_wp, -.475940E+01_wp, -.589531E+01_wp, -.395962E+01_wp,  .100000E+01_wp/
                    
  ! coefficients for CO2 in spectral interval 1
  DATA ncgas(1,2) /6/ ; DATA pgas(1,2) / 86126.250_wp/ ; DATA tgas(1,2) /255.800_wp/
  DATA (coai (i,1,2),i=1,7)/ .592000E-02_wp,  .667700E-02_wp,  .423020E-01_wp,  .732310E-01_wp,  .140143E+00_wp,  .731727E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,1,2),i=1,7)/ .760326E+02_wp,  .480839E+01_wp,  .391742E+00_wp,  .133968E-01_wp,  .355631E-02_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,1,2),i=1,7)/ .659071E+00_wp,  .240858E+00_wp,  .694157E+00_wp,  .424843E+00_wp,  .694262E+00_wp,  .100000E+01_wp,  .000000E+00_wp/
  DATA (cobti(i,1,2),i=1,7)/ .467048E+00_wp,  .395422E+00_wp, -.902210E+00_wp, -.557526E+00_wp, -.473196E+00_wp,  .100000E+01_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for CO2 in spectral interval 2
  DATA ncgas(2,2) /3/ ; DATA pgas(2,2)/ 86126.250_wp/ ;  DATA tgas(2,2) /255.800_wp/
  DATA (coai (i,2,2),i=1,7)/ .278000E-02_wp,  .197330E-01_wp,  .977487E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,2,2),i=1,7)/ .169434E+00_wp,  .103753E-01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,2,2),i=1,7)/ .138563E+00_wp,  .831359E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,2,2),i=1,7)/ .475293E+00_wp, -.496213E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for CO2 in spectral interval 3
  DATA ncgas(3,2) /2/ ; DATA pgas(3,2) / 86126.250_wp/ ; DATA tgas(3,2) /255.800_wp/
  DATA (coai (i,3,2),i=1,7)/ .306100E-02_wp,  .996939E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,3,2),i=1,7)/ .101625E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,3,2),i=1,7)/ .100000E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,3,2),i=1,7)/-.100670E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for CO2 in spectral interval 4
  DATA ncgas(4,2) /0/ ; DATA pgas(4,2) / 60795.000_wp/ ; DATA tgas(4,2) /255.800_wp/
  DATA (coai (i,4,2),i=1,7)/ .335800E-02_wp,  .996642E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/ 
  DATA (cobi (i,4,2),i=1,7)/ .247172E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,4,2),i=1,7)/ .100000E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,4,2),i=1,7)/-.807310E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for CO2 in spectral interval 5
  DATA ncgas(5,2) /7/ ; DATA pgas(5,2) / 10132.500_wp/ ; DATA tgas(5,2) /229.900_wp/
  DATA (coai (i,5,2),i=1,7)/ .452500E-02_wp,  .321420E-01_wp,  .659180E-01_wp,  .101074E+00_wp,  .107224E+00_wp,  .186663E+00_wp,  .502454E+00_wp/
  DATA (cobi (i,5,2),i=1,7)/ .299226E+03_wp,  .364754E+02_wp,  .271644E+01_wp,  .570164E+00_wp,  .100231E+00_wp,  .224388E-01_wp,  .000000E+00_wp/
  DATA (coali(i,5,2),i=1,7)/ .466819E+00_wp,  .319510E+00_wp,  .596734E+00_wp,  .751216E+00_wp,  .708519E+00_wp,  .744381E+00_wp,  .100000E+01_wp/
  DATA (cobti(i,5,2),i=1,7)/ .358348E+00_wp, -.739332E+00_wp, -.183599E+01_wp, -.289470E+01_wp, -.214575E+01_wp, -.585028E+01_wp,  .100000E+01_wp/
                                                                        
  ! coefficients for CO2 in spectral interval 6
  DATA ncgas(6,2) /3/ ; DATA pgas(6,2) / 50662.500_wp/ ; DATA tgas(6,2) /255.800_wp/
  DATA (coai (i,6,2),i=1,7)/ .119551E+00_wp,  .899140E-01_wp,  .790535E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,6,2),i=1,7)/ .305492E-02_wp,  .148936E-02_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,6,2),i=1,7)/ .783365E+00_wp, -.113116E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,6,2),i=1,7)/-.447333E+01_wp,  .296352E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for CO2 in spectral interval 7
  DATA ncgas(7,2) /3/ ; DATA pgas(7,2) / 50662.500_wp/ ; DATA tgas(7,2) /255.800_wp/
  DATA (coai (i,7,2),i=1,7)/ .577890E-01_wp,  .321750E-01_wp,  .910036E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,7,2),i=1,7)/ .650130E-02_wp,  .309030E-02_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,7,2),i=1,7)/ .295465E+00_wp,  .930860E-01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,7,2),i=1,7)/-.562957E+01_wp, -.984577E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for CO2 in spectral interval 8
  DATA ncgas(8,2) /4/ ; DATA pgas(8,2) / 50662.500_wp/ ; DATA tgas(8,2) /255.800_wp/
  DATA (coai (i,8,2),i=1,7)/ .317000E-02_wp,  .127109E+00_wp,  .114118E+00_wp,  .755604E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,8,2),i=1,7)/ .174181E+02_wp,  .495450E-01_wp,  .165196E-01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,8,2),i=1,7)/ .511300E-02_wp,  .252848E+00_wp,  .851104E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,8,2),i=1,7)/ .495222E+00_wp,  .445084E+00_wp,  .117957E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for O3  in spectral interval 1
  DATA ncgas(1,3) /0/ ; DATA pgas(1,3) /  3039.75_wp/ ; DATA tgas(1,3) /229.900_wp/
  DATA (coai (i,1,3),i=1,7)/ .306000E-03_wp,  .999694E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,1,3),i=1,7)/ .409261E+02_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,1,3),i=1,7)/ .618332E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,1,3),i=1,7)/-.974847E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for O3  in spectral interval 2
  DATA ncgas(2,3) /0/ ; DATA pgas(2,3) /  3039.75_wp/ ; DATA tgas(2,3) /229.900_wp/
  DATA (coai (i,2,3),i=1,7)/ .154800E-02_wp,  .998452E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,2,3),i=1,7)/ .395367E+02_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,2,3),i=1,7)/ .592629E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,2,3),i=1,7)/-.106087E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for O3  in spectral interval 3
  DATA ncgas(3,3) /5/ ; DATA pgas(3,3) /  3039.75_wp/ ; DATA tgas(3,3) /229.900_wp/
  DATA (coai (i,3,3),i=1,7)/ .564000E-03_wp,  .108690E-01_wp,  .124320E-01_wp,  .184417E+00_wp,  .791718E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,3,3),i=1,7)/ .191426E+05_wp,  .579429E+03_wp,  .717794E+02_wp,  .187068E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,3,3),i=1,7)/-.204400E-02_wp,  .776840E-01_wp, -.229667E+00_wp,  .994500E-01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,3,3),i=1,7)/ .499912E+00_wp,  .485463E+00_wp,  .464581E+00_wp, -.254634E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for O3  in spectral interval 4
  DATA ncgas(4,3) /0/ ; DATA pgas(4,3) /  3039.75_wp/ ; DATA tgas(4,3) /229.900_wp/
  DATA (coai (i,4,3),i=1,7)/ .540000E-04_wp,  .999946E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,4,3),i=1,7)/ .210378E+03_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,4,3),i=1,7)/ .490324E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,4,3),i=1,7)/ .500000E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for O3  in spectral interval 5
  DATA ncgas(5,3) /2/ ; DATA pgas(5,3) /  3039.75_wp/ ; DATA tgas(5,3) /229.900_wp/
  DATA (coai (i,5,3),i=1,7)/ .587700E-02_wp,  .994123E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,5,3),i=1,7)/ .223357E+03_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,5,3),i=1,7)/ .551312E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,5,3),i=1,7)/-.140025E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for O3  in spectral interval 6
  DATA ncgas(6,3) /0/ ; DATA pgas(6,3) /  3039.75_wp/ ; DATA tgas(6,3) /229.900_wp/
  DATA (coai (i,6,3),i=1,7)/ .154100E-02_wp,  .998459E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (i,6,3),i=1,7)/ .221820E+03_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(i,6,3),i=1,7)/ .546048E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(i,6,3),i=1,7)/-.273183E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
                                                                        
  ! coefficients for O3  in spectral interval 7
  DATA ncgas(7,3) /7/ ; DATA pgas(7,3) / 10132.50_wp/ ; DATA tgas(7,3) /204.000_wp/
  DATA (coai (i,7,3),i=1,7)/ .220500E-02_wp,  .523500E-02_wp,  .951500E-02_wp,  .578800E-01_wp,  .277389E+00_wp,  .643850E-01_wp,  .583391E+00_wp/
  DATA (cobi (i,7,3),i=1,7)/ .434510E+03_wp,  .299916E+03_wp,  .121339E+03_wp,  .827942E+02_wp,  .157398E+02_wp,  .615177E+01_wp,  .000000E+00_wp/
  DATA (coali(i,7,3),i=1,7)/ .224000E-03_wp,  .100500E-02_wp,  .571600E-02_wp,  .508760E-01_wp,  .524641E+00_wp,  .896800E-01_wp,  .100000E+01_wp/
  DATA (cobti(i,7,3),i=1,7)/ .320370E+01_wp,  .130031E+01_wp, -.332851E+01_wp,  .105177E+01_wp, -.561714E+00_wp, -.357670E+01_wp,  .100000E+01_wp/
                                                                        
  ! coefficients for O3  in spectral interval 8
  DATA ncgas(8,3) /0/ ; DATA pgas(8,3) /  3039.75_wp/ ; DATA tgas(8,3) /229.900_wp/
  DATA (coai (I,8,3),I=1,7)/ .397000E-03_wp,  .999603E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobi (I,8,3),I=1,7)/ .230675E+03_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (coali(I,8,3),I=1,7)/ .564371E+00_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
  DATA (cobti(I,8,3),I=1,7)/-.479075E+01_wp,  .100000E+01_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp,  .000000E+00_wp/
 

! 2. Limits of spectral intervals in the radiation code (for information only)
! ------------------------------------------------------
 
  REAL  (KIND=wp)       ::   &
    grenze(2,2,jpspec)       ! limits of spectral intervals jpspec

  !              WMIN1    WMAX1     WMIN2    WMAX2
  DATA grenze/  1.5300_wp,   4.6420_wp    ,999._wp    ,  0._wp    , &
                0.7000_wp,   1.5300_wp    ,999._wp    ,  0._wp    , &
                0.2451_wp,   0.7000_wp    ,999._wp    ,  0._wp    , &
               20.0000_wp, 104.5150_wp    ,999._wp    ,  0._wp    , &
               12.5000_wp,  20.0000_wp    ,999._wp    ,  0._wp    , &
                8.3333_wp,   9.0090_wp    , 10.3093_wp, 12.5000_wp, &
                9.0090_wp,  10.3093_wp    ,999._wp    ,  0._wp    , &
                4.6420_wp,   8.3333_wp    ,999._wp    ,  0._wp    /


! 3. Rayleigh scattering coefficients in solar spectral intervals
! --------------------------------------------------------------

  REAL  (KIND=wp)       ::   &
    zrsc(jpsol)              ! Coefficients for Rayleigh scattering in solar spectral intervals

  DATA zrsc / 0.59776370E-08_wp, 0.13266702E-06_wp, 0.20634412E-05_wp /

 
! 4. Fraction of solar energy at TOA contained in individual solar spectral intervals 
!    based on data from LABS and NECKEL (1970/1984):
! --------------------------------------------------------------

  REAL  (KIND=wp)       ::   &
    solant(jpsol)            ! Fraction of solar energy at TOA in individual spectral intervals

  DATA solant / 0.12888167_wp, 0.41683156_wp, 0.45428677_wp /

 
! 5. Coefficients for black body radiation and E-type coefficients                   
! --------------------------------------------------------------

  REAL  (KIND=wp)       ::   &
      planck(3,jpther), & !
      zketypr (jpther), & !
      ztetypr (jpther), & !
      zketypa (jpther), & !
      ztetypa (jpther), & !
      zteref
      ! planck: coefficients for the description of the fraction of the total 
      !         black body radiation contained in an thermal spectral interval 
      !         as a function (2.order polynomial) of temperature:
      !
      !         F(T) = PLANCK(1,ISPEC) + PLANCK(2,ISPEC)*T + PLANCK(3,ISPEC)*T**2
      !
      ! zketyp: e-type continuum-coefficient for all spectral intervals 
      !         (PA (H2O)**-2) at 296 K
      !         (r)  following ROBERTS ET AL. 1976
      !         (a)  implicitly derived from the AFGL spectral data
      ! ztetyp: constant for the temperature dependancy of e-type   
      !         absorption for all intervals
      ! zteref: reference temperaure 
 
  DATA planck / 0.157656E+01_wp, -0.711486E-02_wp,  0.908220E-05_wp, &
               -0.761337E-01_wp,  0.339014E-02_wp, -0.703246E-05_wp, &
               -0.353624E+00_wp,  0.321131E-02_wp, -0.472513E-05_wp, &
               -0.180726E+00_wp,  0.148131E-02_wp, -0.195189E-05_wp, &
                0.343422E-01_wp, -0.971936E-03_wp,  0.463714E-05_wp  /

  DATA zketypr / 0.0_wp       , 0.418E-06_wp , 0.859E-06_wp , 0.594E-06_wp , 0.767E-07_wp  / 
  DATA ztetypr / 0.0_wp       , 0.0_wp       , 1800.0_wp    , 1800._wp     , 1800._wp      /
  DATA zketypa / 0.8426E-05_wp, 0.8982E-06_wp, 0.5489E-06_wp, 0.4743E-06_wp, 0.7040E-06_wp /
  DATA ztetypa / 1365.55_wp   , 1544.38_wp   , 1699.06_wp   , 1724.39_wp   , 1668.94_wp    /
  DATA zteref  / 296.0_wp     /
 
! 6. Aerosol optical properties for 8 spectral intervals
! ------------------------------------------------------
 
  REAL  (KIND=wp)                  ::           &
  zaea  (jpspec,5),& ! ratio of optical thickness for the absorption in spectral
                     ! interval jpspec  and total optical thickness at 0.55m*1.E-06
                     ! for an aerosoltyp specified by second array index
  zaes  (jpspec,5),& ! analog for the optical thickness of scattering
                     !
  zaeg  (jpspec,5),& ! factor of asymetry for specified aerosoltyp in spectral
                     ! interval jspec

  zaef  (jpspec,5)   ! forward scatterd fraction from aerosols. This array is 
                     ! initialized with 0 but modified later in opt_th and opt_so.
 
                     ! the following aerosoltyps (second array index) are considered:
                     ! 1 : continental
                     ! 2 : maritim
                     ! 3 : urban
                     ! 4 : vulcano ashes
                     ! 5 : stratosphaeric background aerosol

  ! The DATA statements for zaea, zaes, zaeg, zaef have been removed. These 
  ! variables are now set in init_radiation

 
! 7. Optical properties of liquid water for all 8 spectral intervals (solar and thermal)
! --------------------------------------------------------------------------------------
  ! These data-arrays are used localy in routines opt_so and opt_th

  ! For the calculation of the coefficients, spectral integration is performed 
  ! with NLA LSF and extinction = absorption + scattering is assumed.
  ! Single scattering albedo is used lateron.

  REAL  (KIND=wp)      ::  &
    zlwe(4,jpspec), &  ! 
    zlww(2,jpspec), &  !
    zlwg(2,jpspec), &  !
    zlwemn(jpspec), &  ! minimum values of the extinction coefficients in Pa(H2O)**-1
    zlwemx(jpspec)     ! maximum values of the extinction coefficients in Pa(H2O)**-1

  DATA zlwe / -23.014052_wp,     .173026_wp,     .811865_wp,     .000453_wp, &
              -28.122596_wp,     .172211_wp,     .705673_wp,     .000457_wp, &
              -28.162592_wp,     .198665_wp,     .810637_wp,     .000550_wp, &
             -170.687770_wp,     .498371_wp,     .356225_wp,     .001330_wp, &
              -68.573703_wp,     .263182_wp,     .568143_wp,     .000776_wp, &
             -122.833213_wp,     .297599_wp,     .306486_wp,     .000976_wp, &
             -192.594948_wp,     .440659_wp,     .317142_wp,     .001027_wp, &
              -62.018469_wp,     .281406_wp,     .732715_wp,     .000611_wp  /

  DATA zlww /    .989679_wp,  -22.291412_wp, &
                 .999529_wp,     .020875_wp, &
                 .999999_wp,     .000000_wp, &
                 .302657_wp,  102.711916_wp, &
                 .337398_wp,   80.596716_wp, &
                 .449308_wp,   52.823880_wp, &
                 .686930_wp,  -29.876242_wp, &
                 .804203_wp, -103.022685_wp  /
  DATA zlwg /    .804992_wp,   17.901033_wp, &
                 .814785_wp,   14.204375_wp, &
                 .843955_wp,    8.306586_wp, &
                 .279400_wp,  124.179115_wp, &
                 .499491_wp,  131.635343_wp, &
                 .696708_wp,   75.061613_wp, &
                 .704732_wp,   77.778408_wp, &
                 .784672_wp,   38.002913_wp  /

  DATA zlwemn / 5.000_wp,  5.000_wp,  4.930_wp,  5.800_wp,  5.400_wp,  5.200_wp,  5.500_wp,  5.5000_wp /
  DATA zlwemx /32.500_wp, 32.500_wp, 31.360_wp, 18.600_wp, 24.500_wp, 18.200_wp, 20.200_wp, 32.4000_wp /


! 8. Optical properties of ice clouds for all 8 spectral intervals (solar and thermal)
! --------------------------------------------------------------------------------------
  ! These data-arrays are used localy in routines opt_so and opt_th

  ! The coefficients are derived using spectral averaging by weighted nonlinear LSF;
  ! Recombination of extinction, scattering and absorption after averaging of
  ! extinction = scattering + absorption

  REAL  (KIND=wp)      ::  &
    ziwe(4,jpspec), &  ! 
    ziww(2,jpspec), &  ! coefficients for logarithmic fit
    ziwg(2,jpspec), &  ! coefficients for logarithmic fit
    ziwemn(jpspec), &  ! minimum values of the extinction coefficients in Pa(H2O)**-1
    ziwemx(jpspec)     ! maximum values of the extinction coefficients in Pa(H2O)**-1
 
  DATA ziwe / 16.726535_wp,    0.007465_wp,    1.354626_wp,    0.000112_wp, &
              17.531261_wp,    0.003949_wp,    0.669605_wp,    0.000058_wp, &
              17.698999_wp,    0.003657_wp,    0.625067_wp,    0.000055_wp, &
              19.592746_wp,    0.008644_wp,    1.153213_wp,    0.000101_wp, &
              18.990998_wp,    0.006743_wp,    0.997361_wp,    0.000080_wp, &
              18.482156_wp,    0.004408_wp,    0.693883_wp,    0.000060_wp, &
              18.603168_wp,    0.005260_wp,    0.813026_wp,    0.000064_wp, &
              18.437818_wp,    0.004378_wp,    0.692678_wp,    0.000057_wp  /

  DATA ziww /  0.694631_wp,   -0.022160_wp, &
               0.998669_wp,   -0.000107_wp, &
               0.999993_wp,    0.000000_wp, &
               0.289966_wp,   -0.033855_wp, &
               0.555820_wp,    0.004491_wp, &
               0.554495_wp,    0.004904_wp, &
               0.375319_wp,   -0.017168_wp, &
               0.485290_wp,   -0.004358_wp  /

  DATA ziwg /  0.976960_wp,    0.007938_wp, &
               0.914842_wp,    0.003334_wp, &
               0.900536_wp,    0.001797_wp, &
               1.134025_wp,    0.032141_wp, &
               1.053136_wp,    0.015721_wp, &
               1.010632_wp,    0.006844_wp, &
               1.063545_wp,    0.011772_wp, &
               1.035725_wp,    0.008755_wp  /

  DATA ziwemn/ 2.000_wp, 2.000_wp, 2.000_wp, 2.000_wp, 2.000_wp, 2.000_wp, 2.000_wp, 2.0000_wp /
  DATA ziwemx/30.000_wp,30.000_wp,30.000_wp,30.000_wp,30.000_wp,30.000_wp,30.000_wp, 30.000_wp /

! 9. Optical properties of ice clouds for all 8 spectral intervals (solar and thermal)
! --------------------------------------------------------------------------------------

  REAL  (KIND=wp)      ::  &
    rad_csalbw(10)     !  slope of solar albedo with respect to soil water content
                       ! as a function of depth of upper soil layer
                       ! (has been computed every radiation time step before,
                       !  and is now computed in init_radiation)

!=======================================================================

END MODULE data_radiation
