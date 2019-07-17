#ifdef TWOMOM_SB

MODULE src_cloud_opt_reff

!-------------------------------------------------------------------------------
!
! Description:
! 
! 
!
!
!
!
! Current Code Owner:
!  phone: 
!  fax:   
!  email:  
!
! History:
! Version    Date       Name
! ---------- ---------- ----
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
USE data_parameters, ONLY :   &
           wp,   &! KIND-type parameters for real variables
           dp,   &
           iintegers  ! kind-type parameter for "normal" integer variables


USE data_parallel , ONLY:    &
    my_cart_id

USE data_radiation, ONLY : &
  jpsol   , & ! Number of solar spectral intervals
  jpther  , & ! Number of thermal spectral intervals
  jpspec      ! =jpsol+jpther (Total number of spectral intervals)

USE data_modelconfig, ONLY :   &
    ie,           & ! number of grid points in zonal direction
    je,           & !
    ke,           & ! number of grid points in vertical direction
    istartpar,    &
    iendpar

!-------------------------------------------------------------------------------
IMPLICIT NONE


! Optical properties of liquid water for all 8 spectral intervals (solar and thermal)
! --------------------------------------------------------------------------------------


REAL(KIND=wp), ALLOCATABLE ::    & 
   zlwoda_so_prefac(:,:,:,:),          &
   zlwods_so_prefac(:,:,:,:),          &
   zlwb0_so(:,:,:,:),                  &
   zlwb_so_prefac(:,:,:,:),            &
   ziwoda_so_prefac(:,:,:,:),          &
   ziwods_so_prefac(:,:,:,:),          &
   ziwb0_so(:,:,:,:),                  &
   ziwb_so_prefac(:,:,:,:),            &
   zlwoda_th_prefac(:,:,:,:),          &
   zlwods_th_prefac(:,:,:,:),          &
   zlwb0_th(:,:,:,:),                  &
   zlwb_th(:,:,:,:),                   &
   ziwoda_th_prefac(:,:,:,:),          &
   ziwods_th_prefac(:,:,:,:),          &
   ziwb0_th(:,:,:,:),                  &
   w_extinct(:,:,:,:),               &
   i_extinct(:,:,:,:)


  ! .. data for iradpar_cloud == 2 :
  !----------------------------------

  ! Hu & Stamnes (1993) for infrared and solar:

  ! Coefficients for the fit functions of the optical properties of cloud droplets
  ! as function of eff. radius:
  REAL  (KIND=wp)  ::  &
    zgc1(3,jpspec), &  ! coefficient for fit of asymmetry coef. (small r_eff )
    zgc2(3,jpspec), &  !                                        (medium r_eff)
    zgc3(3,jpspec), &  !                                        (large r_eff )
    zwc1(3,jpspec), &  ! coefficient for fit of sing.-scat. alb.(small r_eff )
    zwc2(3,jpspec), &  !                                        (medium r_eff)
    zwc3(3,jpspec), &  !                                        (large r_eff )
    zec1(3,jpspec), &  ! coefficient for fit of sing.-scat. alb.(small r_eff )
    zec2(3,jpspec), &  !                                        (medium r_eff)
    zec3(3,jpspec),   &   !                                        (large r_eff )
    zlwemn_loc(jpspec,3), &  ! minimum values of the extinction coefficients in Pa(H2O)**-1
    zlwemx_loc(jpspec,3), &  ! maximum values of the extinction coefficients in Pa(H2O)**-1
    ziwemn_loc(jpspec,3), &  ! minimum values of the extinction coefficients in Pa(H2O)**-1
    ziwemx_loc(jpspec,3)     ! maximum values of the extinction coefficients in Pa(H2O)**-1

    DATA zgc1(1:3,1) /  1.35155139e-03_dp ,  1.37919662e+00_dp ,  8.61329693e-01_dp /
  DATA zgc1(1:3,2) / -4.08189497e-01_dp , -1.27270997e+00_dp ,  8.71600615e-01_dp /
  DATA zgc1(1:3,3) / -9.68604617e-02_dp , -5.33538940e-01_dp ,  8.92138933e-01_dp /
  DATA zgc1(1:3,4) / -1.88135801e+00_dp , -6.44478276e-01_dp ,  1.11637275e+00_dp /
  DATA zgc1(1:3,5) / -2.41189309e+00_dp , -1.33289366e+00_dp ,  9.58865701e-01_dp /
  DATA zgc1(1:3,6) / -2.14454970e+00_dp , -1.63554204e+00_dp ,  9.62747659e-01_dp /
  DATA zgc1(1:3,7) / -2.06515711e+00_dp , -1.95356112e+00_dp ,  9.17550008e-01_dp /
  DATA zgc1(1:3,8) / -2.32150720e+00_dp , -2.84391926e+00_dp ,  8.84933671e-01_dp /

  DATA zgc2(1:3,1) / -5.21025320e-01_dp , -1.07539351e+00_dp ,  9.38249930e-01_dp /
  DATA zgc2(1:3,2) / -2.16114961e-01_dp , -6.48494355e-01_dp ,  8.97293947e-01_dp /
  DATA zgc2(1:3,3) / -1.57506916e-01_dp , -9.68044744e-01_dp ,  8.81523330e-01_dp /
  DATA zgc2(1:3,4) / -3.31316873e+00_dp , -1.10913884e+00_dp ,  9.45509280e-01_dp /
  DATA zgc2(1:3,5) / -2.75897852e+00_dp , -1.44231454e+00_dp ,  9.47403178e-01_dp /
  DATA zgc2(1:3,6) / -3.16371321e-01_dp , -2.30272653e-01_dp ,  1.10433012e+00_dp /
  DATA zgc2(1:3,7) /  1.04458555e-03_dp ,  1.22456891e+00_dp ,  8.76228385e-01_dp /
  DATA zgc2(1:3,8) / -6.57145274e-01_dp , -2.78203089e-01_dp ,  1.20223190e+00_dp /

  DATA zgc3(1:3,1) / -2.01780162e-01_dp , -5.79270612e-01_dp ,  9.53270216e-01_dp /
  DATA zgc3(1:3,2) / -2.38862952e-01_dp , -6.92283725e-01_dp ,  8.95753046e-01_dp /
  DATA zgc3(1:3,3) / -1.71271436e-01_dp , -9.69125115e-01_dp ,  8.81838376e-01_dp /
  DATA zgc3(1:3,4) / -9.47504022e+00_dp , -1.49199962e+00_dp ,  9.28185824e-01_dp /
  DATA zgc3(1:3,5) / -3.41318673e+00_dp , -1.51533707e+00_dp ,  9.46533750e-01_dp /
  DATA zgc3(1:3,6) / -4.81294210e+00_dp , -1.55463251e+00_dp ,  9.83758483e-01_dp /
  DATA zgc3(1:3,7) / -3.00488637e+01_dp , -1.96229250e+00_dp ,  9.79789555e-01_dp /
  DATA zgc3(1:3,8) / -3.36755340e+01_dp , -2.08919516e+00_dp ,  9.74027070e-01_dp /

  ! Coeffs. for coalbedo = (1 - single scat. alb.) as function of Reff in mue-m:

  DATA zwc1(1:3,1) / -1.77484297e-01_dp , -3.82659292e-01_dp ,  1.57411270e-01_dp /
  DATA zwc1(1:3,2) / -3.95088467e+02_dp , -1.38803637e-06_dp ,  3.95088075e+02_dp /
  DATA zwc1(1:3,3) / -2.00411661e+00_dp , -5.53000038e-07_dp ,  2.00411587e+00_dp /
  DATA zwc1(1:3,4) /  1.10545624e+00_dp , -9.50675210e-01_dp ,  4.51590386e-01_dp /
  DATA zwc1(1:3,5) /  8.80652139e-01_dp , -8.14816312e-01_dp ,  4.45618477e-01_dp /
  DATA zwc1(1:3,6) / -7.60878380e+03_dp ,  1.12738374e-05_dp ,  7.60940910e+03_dp /
  DATA zwc1(1:3,7) /  4.81469213e-14_dp ,  1.08360561e+01_dp ,  2.72996507e-01_dp /
  DATA zwc1(1:3,8) /  1.18094219e-03_dp ,  1.96103353e+00_dp ,  1.73555077e-01_dp /

  DATA zwc2(1:3,1) / -3.99713397e+03_dp , -9.35025282e-06_dp ,  3.99712987e+03_dp /
  DATA zwc2(1:3,2) / -4.69627738e+02_dp , -3.20613944e-06_dp ,  4.69624928e+02_dp /
  DATA zwc2(1:3,3) / -3.74264515e+00_dp , -9.17983686e-07_dp ,  3.74263852e+00_dp /
  DATA zwc2(1:3,4) /  8.12808997e-01_dp , -1.09473634e+00_dp ,  5.00557021e-01_dp /
  DATA zwc2(1:3,5) /  5.29896829e-01_dp , -5.37977174e-01_dp ,  4.21587773e-01_dp /
  DATA zwc2(1:3,6) / -9.42787209e-01_dp , -8.13821122e-01_dp ,  5.44872162e-01_dp /
  DATA zwc2(1:3,7) / -3.50807690e+00_dp , -9.83284687e-01_dp ,  5.91069272e-01_dp /
  DATA zwc2(1:3,8) / -1.23069431e+01_dp , -1.73041492e+00_dp ,  4.89368484e-01_dp /

  DATA zwc3(1:3,1) / -3.90469846e+03_dp , -1.36180240e-05_dp ,  3.90464094e+03_dp /
  DATA zwc3(1:3,2) / -5.83150345e+02_dp , -5.26289656e-06_dp ,  5.83142203e+02_dp /
  DATA zwc3(1:3,3) / -4.00552597e+00_dp , -1.69134430e-06_dp ,  4.00550801e+00_dp /
  DATA zwc3(1:3,4) / -1.19184186e-01_dp ,  1.73105279e-01_dp ,  7.34958637e-01_dp /
  DATA zwc3(1:3,5) / -3.13798540e+03_dp ,  1.22089852e-05_dp ,  3.13862082e+03_dp /
  DATA zwc3(1:3,6) / -1.21627107e+07_dp , -6.09496567e+00_dp ,  4.97988214e-01_dp /
  DATA zwc3(1:3,7) / -4.74763482e+04_dp , -4.22223945e+00_dp ,  4.90639909e-01_dp /
  DATA zwc3(1:3,8) / -1.52202307e+02_dp , -2.55364031e+00_dp ,  4.80018143e-01_dp /

  ! Coeffs. for ( ext. coeff. [1/m] / LWC [km-1 g-1 m^3] ) as function of Reff in mue-m:

  DATA zec1(1:3,1) /  2.25651225e+03_dp , -1.18200363e+00_dp ,  1.85876391e+01_dp /
  DATA zec1(1:3,2) /  1.86410201e+03_dp , -1.08712336e+00_dp ,  7.27453246e+00_dp /
  DATA zec1(1:3,3) /  1.69181603e+03_dp , -1.04463256e+00_dp ,  3.08062592e+00_dp /
  DATA zec1(1:3,4) /  3.06599650e+05_dp ,  7.16096801e-05_dp , -3.06526626e+05_dp /
  DATA zec1(1:3,5) /  8.37129973e+05_dp , -1.18388602e-04_dp , -8.36746626e+05_dp /
  DATA zec1(1:3,6) / -2.66375660e-03_dp ,  4.03989422e+00_dp ,  2.06537195e+02_dp /
  DATA zec1(1:3,7) / -2.22599856e-06_dp ,  6.58432531e+00_dp ,  1.95925581e+02_dp /
  DATA zec1(1:3,8) / -3.58965590e+00_dp ,  1.70778763e+00_dp ,  3.90986067e+02_dp /

  DATA zec2(1:3,1) /  1.93521777e+03_dp , -1.07056981e+00_dp ,  2.01486248e+00_dp /
  DATA zec2(1:3,2) /  1.74656230e+03_dp , -1.04362183e+00_dp ,  1.19587445e+00_dp /
  DATA zec2(1:3,3) /  1.65347784e+03_dp , -1.02739040e+00_dp ,  7.99955802e-01_dp /
  DATA zec2(1:3,4) /  6.74912209e+05_dp , -1.21406634e-04_dp , -6.74571840e+05_dp /
  DATA zec2(1:3,5) /  1.00701104e+03_dp , -7.24080462e-01_dp , -2.94367855e+01_dp /
  DATA zec2(1:3,6) /  2.47250409e+03_dp , -1.14285816e+00_dp ,  5.43005969e+00_dp /
  DATA zec2(1:3,7) /  3.57709225e+03_dp , -1.22369076e+00_dp ,  1.22795013e+00_dp /
  DATA zec2(1:3,8) /  3.37380122e+03_dp , -1.26127491e+00_dp ,  9.84848914e+00_dp /

  DATA zec3(1:3,1) /  1.89490365e+03_dp , -1.05319405e+00_dp ,  0.00000000e+00_dp /
  DATA zec3(1:3,2) /  1.79205315e+03_dp , -1.04209999e+00_dp ,  0.00000000e+00_dp /
  DATA zec3(1:3,3) /  1.73788689e+03_dp , -1.03645435e+00_dp ,  0.00000000e+00_dp /
  DATA zec3(1:3,4) /  2.81835046e+03_dp , -1.12043012e+00_dp ,  0.00000000e+00_dp /
  DATA zec3(1:3,5) /  2.17029592e+03_dp , -1.07118149e+00_dp ,  0.00000000e+00_dp /
  DATA zec3(1:3,6) /  2.25095287e+03_dp , -1.08438660e+00_dp ,  0.00000000e+00_dp /
  DATA zec3(1:3,7) /  2.47267932e+03_dp , -1.10598151e+00_dp ,  0.00000000e+00_dp /
  DATA zec3(1:3,8) /  2.23347618e+03_dp , -1.08400166e+00_dp ,  0.00000000e+00_dp /


  ! Minimum and maximum of extinction coefficient in 1/Pa:
  DATA zlwemn_loc(:,2) / 2.000,  2.000,  2.000,  2.000,  2.000,  2.000,  2.000,  2.0000 /
  DATA zlwemx_loc(:,2) /50.000, 50.000, 50.000, 50.000, 50.000, 50.000, 50.000, 50.0000 /

  ! for iradpar_cloud = 3:
  DATA zlwemn_loc(:,3) / 2.000,  2.000,  2.000,  2.000,  2.000,  2.000,  2.000,  2.0000 /
  DATA zlwemx_loc(:,3) /50.000, 50.000, 50.000, 50.000, 50.000, 50.000, 50.000, 50.0000 /


! Optical properties of ice clouds for all 8 spectral intervals (solar and thermal)
! --------------------------------------------------------------------------------------

  ! .. data for iradpar_cloud == 2 :
  !----------------------------------

  ! Fu et al. (1998) for IR bands for randomly oriented hexagonal columns
  ! Key et al. (2002) for VIS bands for hexagonal plates whose
  ! hexagonal faces are oriented perpendicular to the incident light.
  ! The zeros are dummy values for the Fu-parameterization,
  ! where no data is needed

  ! Number of habits and coefficients for ice cloud radiative feedback based on r_eff:
  INTEGER, PARAMETER :: nhabits_ice = 2
  INTEGER, PARAMETER :: nparams_per_habit_reff_so = 2
  INTEGER, PARAMETER :: nparams_per_habit_reff_th = 4

  ! Coefficients for the fit functions of the optical properties of cloud ice
  ! as function of eff. radius in case:
  REAL  (KIND=wp)  ::  &
       zgi(4,jpspec,nhabits_ice),  &  ! coefficients for asymmetry factor for iradpar_cloud=2
       zwi(4,jpspec,nhabits_ice),  &  ! coefficients for single scat. albedo for iradpar_cloud=2
       zei(4,jpspec,nhabits_ice)      ! coefficients for extinction for iradpar_cloud=2


  ! .. coeffs for extinction coeff.:
  DATA zei(:,:,1)  /  0.01042,1624, 158.1,-392, &  ! Key, hexagonal plates
                     -0.1285,1647,-216.6,797.6, &  ! Key, hexagonal plates
                     -0.4046,1648,-83.84,229.9, &  ! Key, hexagonal plates
                     -0.0022,3.0410,-18.89,0.0, &  ! Fu, hexagonal columns
                     -0.0081,3.4450,-9.748,0.0, &  ! Fu, hexagonal columns
                     -0.0016,2.7600,-8.145,0.0, &  ! Fu, hexagonal columns
                     -0.0082,3.4300,-14.76,0.0, &  ! Fu, hexagonal columns
                     -0.0044,3.0280,-3.213,0.0  /  ! Fu, hexagonal columns

  DATA zei(:,:,2)  /  0.01042,1624, 158.1,-392, &  ! At the moment equal to zei(:,:,1)
                     -0.1285,1647,-216.6,797.6, &  !==================================
                     -0.4046,1648,-83.84,229.9, &  ! -> the plan is to have
                     -0.0022,3.0410,-18.89,0.0, &  !     zei(:,:,1) for hex plates
                     -0.0081,3.4450,-9.748,0.0, &  !    and
                     -0.0016,2.7600,-8.145,0.0, &  !     zei(:,:,2) for hex columns
                     -0.0082,3.4300,-14.76,0.0, &  !==================================
                     -0.0044,3.0280,-3.213,0.0  /

  ! .. coeffs for single scattering albedo:
  DATA zwi(:,:,1)  / 0.9506,-0.003774,6.022e-05, -4.839e-07, &  ! analogous to zei(:,:,1)
                     1.0000,-0.0003628,4.163e-06,-2.824e-08, &
                     1.0000,-3.35e-07,2.0700e-09,-1.592e-11, &
                     0.4099,0.021000,-0.00020820,0.0000007246, &
                     1.0880,0.008640,-0.00012680,0.0000005281, &
                     1.2120,0.003205,-0.00005431,0.0000002381, &
                     0.5673,0.018860,-0.00019950,0.0000007187, &
                     0.7483,0.015020,-0.00017360,0.0000006603 /

  DATA zwi(:,:,2)  / 0.9506,-0.003774,6.022e-05, -4.839e-07, &  ! analogous to zei(:,:,2)
                     1.0000,-0.0003628,4.163e-06,-2.824e-08, &
                     1.0000,-3.35e-07,2.0700e-09,-1.592e-11, &
                     0.4099,0.021000,-0.00020820,0.0000007246, &
                     1.0880,0.008640,-0.00012680,0.0000005281, &
                     1.2120,0.003205,-0.00005431,0.0000002381, &
                     0.5673,0.018860,-0.00019950,0.0000007187, &
                     0.7483,0.015020,-0.00017360,0.0000006603 /

  ! .. coeffs for asymmetry factor:
  DATA zgi(:,:,1)  / 0.6489,0.01577,-0.0003041,2.208e-06, &  ! analogous to zei(:,:,1)
                     0.6005,0.02044,-0.0004684,3.973e-06, &
                     0.6679,0.01757,-0.0004504,4.257e-06, &
                     0.6430,0.006898,-0.00006949,0.00000025220, &
                     0.7611,0.004264,-0.00004145,0.00000014410, &
                     0.8663,0.002787,-0.00003174,0.00000012120, &
                     0.8921,0.001852,-0.00001680,0.00000005662, &
                     0.8574,0.002483,-0.00002189,0.00000007036 /

  DATA zgi(:,:,2)  / 0.6489,0.01577,-0.0003041,2.208e-06, &  ! analogous to zei(:,:,2)
                     0.6005,0.02044,-0.0004684,3.973e-06, &
                     0.6679,0.01757,-0.0004504,4.257e-06, &
                     0.6430,0.006898,-0.00006949,0.00000025220, &
                     0.7611,0.004264,-0.00004145,0.00000014410, &
                     0.8663,0.002787,-0.00003174,0.00000012120, &
                     0.8921,0.001852,-0.00001680,0.00000005662, &
                     0.8574,0.002483,-0.00002189,0.00000007036 /

  DATA ziwemn_loc(:,2)/ 2.000, 2.000, 2.000, 2.000, 2.000, 2.000, 2.000, 2.0000 /
  DATA ziwemx_loc(:,2)/30.000,30.000,30.000,30.000,30.000,30.000,30.000, 30.000 /


  ! .. data for iradpar_cloud == 3 :
  !----------------------------------

  ! Fu et al. (1996/1998) for VIS and IR bands for randomly oriented hexagonal columns

  ! Fits of B. Ritter to Fu et al. (1996) for solar spectrum:
  !
  ! solar   spectral intervals (FU,1996 Eq.: 3.9a-d):
  !
  ! beta_total=IWC     *(a0+a1/D_ge+a2/D_ge^2)  (NOTE: original eq. is "a0+a1/D_ge")
  !
  ! 1-omega  =          (b0+b1*D_ge+b2*D_ge^2+b3*D_ge^3)
  !
  ! g         =         (c0+c1*D_ge+c2*D_ge^2+c3*D_ge^3)
  !
  ! f         =         (d0+d1*D_ge+d2*D_ge^2+d3*D_ge^3)
  !
  ! thermal spectral intervals (FU et al.,1998, Eq.3.1-3.3):
  !
  ! beta_total=IWC     *(a0+a1/D_ge+a2/D_ge^2)
  !
  ! beta_abs  =IWC/D_ge*(b0+b1*D_ge+b2*D_ge^2+b3*D_ge^3)
  !
  ! g         =         (c0+c1*D_ge+c2*D_ge^2+c3*D_ge^3)
  !

  ! Coefficients for the fit functions of the optical properties of cloud ice
  ! as function of eff. radius:

  REAL  (KIND=wp)  ::  &
       zfuia(3,jpspec,nhabits_ice), &  ! coefficients for extinction for iradpar_cloud=3
       zfuib(4,jpspec,nhabits_ice), &  ! coefficients for single scat. albedo for iradpar_cloud=3
       zfuic(4,jpspec,nhabits_ice), &  ! coefficients for asymmetry factor for iradpar_cloud=3
       zfuid(4,jpspec,nhabits_ice)     ! coefficients for forward delta fraction for iradpar_cloud=3

  ! ihabit_ice = 1:  "hexagonal plates"  (NOTE: Fu only give values for hexagonal columns,
  !                                             so we set the values equal to that for
  !                                             columns ihabit_ice = 2 below!
  !                                             We keep the distinction for consistency with the
  !                                             coefficients of Key et al. for iradpar_cloud = 2.)

  ! Extinction fit coefficients for solar RG92 interval no.:       1    (solar)
  DATA zfuia(1:3,1,1)/  1.6621080E-04,  2.5032640E+00,  1.4060140E-03/
  ! coalbedo fit coefficients for solar RG92 interval no.:       1
  DATA zfuib(1:4,1,1)/  6.5663517E-02,  2.0028187E-03, -1.1368109E-05,  3.4843922E-08/
  ! asymetry factor fit coefficients for RG92 interval no.:       1
  DATA zfuic(1:4,1,1)/  7.8365010E-01,  2.0741818E-03, -1.3918600E-05,  3.9658062E-08/
  ! forward scattered fraction fit coefficients for RG92 interval no.:       1
  DATA zfuid(1:4,1,1)/  9.6984804E-02, -2.5914644E-04,  3.3848764E-06, -1.5379271E-08/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for solar RG92 interval no.:       2    (solar)
  DATA zfuia(1:3,2,1)/ -7.6867611E-05,  2.5250466E+00, -4.0746778E-03/
  ! coalbedo fit coefficients for solar RG92 interval no.:       2
  DATA zfuib(1:4,2,1)/  3.4693629E-05,  1.3240722E-04, -4.6279308E-07,  1.2501071E-09/
  ! asymetry factor fit coefficients for RG92 interval no.:       2
  DATA zfuic(1:4,2,1)/  7.5322860E-01,  1.0940240E-03, -2.3251764E-06, -4.5423878E-09/
  ! forward scattered fraction fit coefficients for RG92 interval no.:       2
  DATA zfuid(1:4,2,1)/  1.1428944E-01,  2.0835896E-04,  2.3070838E-06, -1.4100462E-08/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for solar RG92 interval no.:       3    (solar)
  DATA zfuia(1:3,3,1)/ -2.5337780E-05,  2.5189104E+00,  6.3517094E-03/
  ! coalbedo fit coefficients for solar RG92 interval no.:       3
  DATA zfuib(1:4,3,1)/  2.5592090E-07,  9.4888946E-08,  4.1502801E-11, -1.7501278E-13/
  ! asymetry factor fit coefficients for RG92 interval no.:       3
  DATA zfuic(1:4,3,1)/  7.4874061E-01,  8.9719845E-04, -3.2262585E-07, -1.1357892E-08/
  ! forward scattered fraction fit coefficients for RG92 interval no.:       3
  DATA zfuid(1:4,3,1)/  1.1619963E-01,  2.3595581E-04,  2.2354652E-06, -1.3883747E-08/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       4    (infrared)
  DATA zfuia(1:3,4,1)/ -2.1578663E-03,  3.0456934E+00, -1.9152817E+01/
  ! Absorption fit coefficients for RG92 interval no.:       4
  DATA zfuib(1:4,4,1)/  3.8558248E-01,  2.0805331E-02, -2.0117503E-04,  6.9004977E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       4
  DATA zfuic(1:4,4,1)/  6.4654177E-01,  6.7071076E-03, -6.7098299E-05,  2.4330535E-07/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,4,1)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       5    (infrared)
  DATA zfuia(1:3,5,1)/ -7.9505788E-03,  3.4304042E+00, -9.4157362E+00/
  ! Absorption fit coefficients for RG92 interval no.:       5
  DATA zfuib(1:4,5,1)/  1.1074280E+00,  8.1598461E-03, -1.2224058E-04,  5.1250163E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       5
  DATA zfuic(1:4,5,1)/  7.6095015E-01,  4.3195440E-03, -2.7272683E-05,  1.4853561E-07/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,5,1)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       6    (infrared)
  DATA zfuia(1:3,6,1)/ -2.8788764E-03,  2.8776402E+00, -8.2543716E+00/
  ! Absorption fit coefficients for RG92 interval no.:       6
  DATA zfuib(1:4,6,1)/  1.0815326E+00,  6.2126890E-03, -8.0646088E-05,  3.2008128E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       6
  DATA zfuic(1:4,6,1)/  8.6676204E-01,  2.6379775E-03, -2.8448776E-05,  1.0545019E-07/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,6,1)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       7    (infrared)
  DATA zfuia(1:3,7,1)/ -8.5133482E-03,  3.4710600E+00, -1.5376632E+01/
  ! Absorption fit coefficients for RG92 interval no.:       7
  DATA zfuib(1:4,7,1)/  5.2922714E-01,  1.9775582E-02, -2.0761121E-04,  7.4459746E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       7
  DATA zfuic(1:4,7,1)/  8.9437252E-01,  1.7735233E-03, -1.5757996E-05,  5.2426117E-08/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,7,1)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       8    (infrared)
  DATA zfuia(1:3,8,1)/ -3.9731851E-03,  2.9889402E+00, -2.6700449E+00/
  ! Absorption fit coefficients for RG92 interval no.:       8
  DATA zfuib(1:4,8,1)/  7.6872569E-01,  1.4355695E-02, -1.6627095E-04,  6.3322750E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       8
  DATA zfuic(1:4,8,1)/  8.5852456E-01,  2.4808683E-03, -2.2113087E-05,  7.1682649E-08/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,8,1)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------

  ! ihabit_ice = 2:  "hexagonal columns"  (NOTE: Fu only give values for hexagonal columns!)

  ! Extinction fit coefficients for solar RG92 interval no.:       1    (solar)
  DATA zfuia(1:3,1,2)/  1.6621080E-04,  2.5032640E+00,  1.4060140E-03/
  ! coalbedo fit coefficients for solar RG92 interval no.:       1
  DATA zfuib(1:4,1,2)/  6.5663517E-02,  2.0028187E-03, -1.1368109E-05,  3.4843922E-08/
  ! asymetry factor fit coefficients for RG92 interval no.:       1
  DATA zfuic(1:4,1,2)/  7.8365010E-01,  2.0741818E-03, -1.3918600E-05,  3.9658062E-08/
  ! forward scattered fraction fit coefficients for RG92 interval no.:       1
  DATA zfuid(1:4,1,2)/  9.6984804E-02, -2.5914644E-04,  3.3848764E-06, -1.5379271E-08/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for solar RG92 interval no.:       2    (solar)
  DATA zfuia(1:3,2,2)/ -7.6867611E-05,  2.5250466E+00, -4.0746778E-03/
  ! coalbedo fit coefficients for solar RG92 interval no.:       2
  DATA zfuib(1:4,2,2)/  3.4693629E-05,  1.3240722E-04, -4.6279308E-07,  1.2501071E-09/
  ! asymetry factor fit coefficients for RG92 interval no.:       2
  DATA zfuic(1:4,2,2)/  7.5322860E-01,  1.0940240E-03, -2.3251764E-06, -4.5423878E-09/
  ! forward scattered fraction fit coefficients for RG92 interval no.:       2
  DATA zfuid(1:4,2,2)/  1.1428944E-01,  2.0835896E-04,  2.3070838E-06, -1.4100462E-08/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for solar RG92 interval no.:       3    (solar)
  DATA zfuia(1:3,3,2)/ -2.5337780E-05,  2.5189104E+00,  6.3517094E-03/
  ! coalbedo fit coefficients for solar RG92 interval no.:       3
  DATA zfuib(1:4,3,2)/  2.5592090E-07,  9.4888946E-08,  4.1502801E-11, -1.7501278E-13/
  ! asymetry factor fit coefficients for RG92 interval no.:       3
  DATA zfuic(1:4,3,2)/  7.4874061E-01,  8.9719845E-04, -3.2262585E-07, -1.1357892E-08/
  ! forward scattered fraction fit coefficients for RG92 interval no.:       3
  DATA zfuid(1:4,3,2)/  1.1619963E-01,  2.3595581E-04,  2.2354652E-06, -1.3883747E-08/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       4    (infrared)
  DATA zfuia(1:3,4,2)/ -2.1578663E-03,  3.0456934E+00, -1.9152817E+01/
  ! Absorption fit coefficients for RG92 interval no.:       4
  DATA zfuib(1:4,4,2)/  3.8558248E-01,  2.0805331E-02, -2.0117503E-04,  6.9004977E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       4
  DATA zfuic(1:4,4,2)/  6.4654177E-01,  6.7071076E-03, -6.7098299E-05,  2.4330535E-07/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,4,2)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       5    (infrared)
  DATA zfuia(1:3,5,2)/ -7.9505788E-03,  3.4304042E+00, -9.4157362E+00/
  ! Absorption fit coefficients for RG92 interval no.:       5
  DATA zfuib(1:4,5,2)/  1.1074280E+00,  8.1598461E-03, -1.2224058E-04,  5.1250163E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       5
  DATA zfuic(1:4,5,2)/  7.6095015E-01,  4.3195440E-03, -2.7272683E-05,  1.4853561E-07/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,5,2)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       6    (infrared)
  DATA zfuia(1:3,6,2)/ -2.8788764E-03,  2.8776402E+00, -8.2543716E+00/
  ! Absorption fit coefficients for RG92 interval no.:       6
  DATA zfuib(1:4,6,2)/  1.0815326E+00,  6.2126890E-03, -8.0646088E-05,  3.2008128E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       6
  DATA zfuic(1:4,6,2)/  8.6676204E-01,  2.6379775E-03, -2.8448776E-05,  1.0545019E-07/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,6,2)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       7    (infrared)
  DATA zfuia(1:3,7,2)/ -8.5133482E-03,  3.4710600E+00, -1.5376632E+01/
  ! Absorption fit coefficients for RG92 interval no.:       7
  DATA zfuib(1:4,7,2)/  5.2922714E-01,  1.9775582E-02, -2.0761121E-04,  7.4459746E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       7
  DATA zfuic(1:4,7,2)/  8.9437252E-01,  1.7735233E-03, -1.5757996E-05,  5.2426117E-08/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,7,2)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------
  ! Extinction fit coefficients for RG92 interval no.:       8    (infrared)
  DATA zfuia(1:3,8,2)/ -3.9731851E-03,  2.9889402E+00, -2.6700449E+00/
  ! Absorption fit coefficients for RG92 interval no.:       8
  DATA zfuib(1:4,8,2)/  7.6872569E-01,  1.4355695E-02, -1.6627095E-04,  6.3322750E-07/
  ! Asymetry factor fit coefficients for RG92 interval no.:       8
  DATA zfuic(1:4,8,2)/  8.5852456E-01,  2.4808683E-03, -2.2113087E-05,  7.1682649E-08/
  ! Dummies for infrared, because here no forward scattered delta fraction is considered.
  DATA zfuid(1:4,8,2)/  0.0, 0.0, 0.0, 0.0/
  ! ----------------------------------------------------------------------------

  ! for iradpar_cloud = 3:
  DATA ziwemn_loc(:,3)/ 2.000, 2.000, 2.000, 2.000, 2.000, 2.000, 2.000, 2.0000 /
  DATA ziwemx_loc(:,3)/30.000,30.000,30.000,30.000,30.000,30.000,30.000, 30.000 /

!==============================================================================
!==============================================================================

LOGICAL, SAVE :: FIRSTCALL=.TRUE.

INTEGER (KIND=iintegers) , PARAMETER :: jloc=1

!------------------------------------------------
! ihabit_ice = 2:  "hexagonal columns" 
! use only ihabit_ice = 2 in the following 
! ihabit=1 would assume hexagonal plates, but we lack the fits for it ...
!-------------------------------------------------
INTEGER (KIND=iintegers), PARAMETER  ::      ihabit = 2

CONTAINS

!==============================================================================
!==============================================================================

SUBROUTINE reff_for_rad(zclc,zciwc,iradpar_cloud)


USE data_modelconfig, ONLY :   &

! horizontal and vertical sizes of the fields and related variables
! --------------------------------------------------------------------
    idt_qc,       & ! index of hydrometeor tracer
    idt_qi,       & ! -"-
    istart,iend,jstart,jend

!------------------------------------------------------------------------------

USE data_fields     , ONLY :   &
    reffc_out,                 & ! effective radius of cloud droplets ( m )
    reffi_out,                 & ! effective radius of ice droplets   ( m ) 
    t                            ! temperature                        ( K )

USE data_runcontrol , ONLY :   &
    nold  ,       & 
    nnow  ,       & 
    nnew  ,       &
    l2tls            

USE data_constants  , ONLY :   &
  t0_melt     ! melting temperature of ice


!------------------------------------------------------------------------------

!$$TS
USE data_io,         ONLY:     &
   undefncdf
!$$TS

USE src_twomom_sb_interface, ONLY :   &
           calc_reff_rad_2mom,        &
           prefactors_reff_1mom_icemono, &
           refft,                        &
           ice_nuclei_number

!------------------------------------------------------------------------------

USE src_tracer,         ONLY: trcr_get, trcr_errorstr

!------------------------------------------------------------------------------

USE environment,              ONLY :  &
  model_abort


!------------------------------------------------------------------------------


IMPLICIT NONE

!==============================================================================
! Declaration
!------------------------------------------------------------------------------

INTEGER (KIND=iintegers)  :: i, j, k,  jspec, nzx,  izerror,iradpar_cloud

REAL (KIND=wp) :: zqi_gridscale_thresh,  &
                      zr_effc_so,            &
                      zr_effi_so,            &
                      zr_effc_th,            &
                      zr_effi_th,            &
                      zclc   (ie,je,ke ),    &  ! Cloud cover in each layer
                      zciwc  (ie,je,ke ),    &  ! ice mixing ratio
                      r_effc_so(ie,je,ke ),    &
                      r_effc_th(ie,je,ke ),    &
                      r_effi_so(ie,je,ke,2 ),    & ! 2 is number of ice habits
                      r_effi_th(ie,je,ke,2 )


REAL    (KIND=wp   ) ::  &
    ! individual optical properties of liquid water and ice 
    z_lwe, z_iwe,          & ! extinction coefficient
    z_lww, z_iww,          & ! single scattering coefficient
    z_lwg, z_iwg,          & ! asymetry factor
    z_lwf, z_iwf,          & ! forward scattered fraction 
    zzg

REAL (kind=wp) :: zxi, &
                      c_reffc_sub, &
                      c_reffi_so_sub(nhabits_ice,nparams_per_habit_reff_so), &
                      c_reffi_th_sub(nhabits_ice,nparams_per_habit_reff_th)


CHARACTER (LEN=80)          :: yzerrmsg
CHARACTER (LEN=80)          :: yzroutine

REAL    (KIND=wp   ), PARAMETER ::  &
     z1dg   = 1.0/9.80665, & ! 1./g
     z1d8   = 0.125      , & ! 1./8 
     zepssa = 1.0E-6         ! Security constant for single scattering albedo

!$$TS
!$$TS

! Tracer pointers
!----------------
REAL (KIND=wp), POINTER :: &
    qc  (:,:,:) => NULL(),     &     ! QC at nx
    qi  (:,:,:) => NULL()            ! QI at nx


  IF(FIRSTCALL) THEN

    WRITE(*,*) 'ALLOCATION OF VARIABLES FOR CLOUD OPTICAL PROPERTIES BASED ON REFF'

    ! eff. radii of cloud droples and cloud ice, initialization with reasonable non-zero values:
    ALLOCATE ( refft%r_effc (ie,je,ke) ) ; refft%r_effc  = 10.0e-6_wp 
    ALLOCATE ( refft%r_effi_so (ie,je,ke,nhabits_ice))
               refft%r_effi_so  = 20.0e-6_wp
    ALLOCATE ( refft%r_effi_th (ie,je,ke,nhabits_ice) )
               refft%r_effi_th  = 20.0e-6_wp
    ALLOCATE ( refft%odepthw_so (ie,je,ke) ) ; refft%odepthw_so = 0.0_wp
    ALLOCATE ( refft%odepthi_so (ie,je,ke) ) ; refft%odepthi_so = 0.0_wp
    ALLOCATE ( refft%odepthw_th (ie,je,ke) ) ; refft%odepthw_th = 0.0_wp
    ALLOCATE ( refft%odepthi_th (ie,je,ke) ) ; refft%odepthi_th = 0.0_wp
  
  
    ALLOCATE ( &
    zlwoda_so_prefac(ie,jloc,ke,jpspec),          &
    zlwods_so_prefac(ie,jloc,ke,jpspec),          &
    zlwb0_so(ie,jloc,ke,jpspec),                  &
    zlwb_so_prefac(ie,jloc,ke,jpspec),            &
    ziwoda_so_prefac(ie,jloc,ke,jpspec),          &
    ziwods_so_prefac(ie,jloc,ke,jpspec),          &
    ziwb0_so(ie,jloc,ke,jpspec),                  &
    ziwb_so_prefac(ie,jloc,ke,jpspec),            &
    zlwoda_th_prefac(ie,jloc,ke,jpspec),          &
    zlwods_th_prefac(ie,jloc,ke,jpspec),          &
    zlwb0_th(ie,jloc,ke,jpspec),                  &
    zlwb_th(ie,jloc,ke,jpspec),                   &
    ziwoda_th_prefac(ie,jloc,ke,jpspec),          &
    ziwods_th_prefac(ie,jloc,ke,jpspec),          &
    ziwb0_th(ie,jloc,ke,jpspec),                  &
    w_extinct(ie,jloc,ke,jpspec),          &
    i_extinct(ie,jloc,ke,jpspec)           &
     )
    FIRSTCALL=.FALSE.
  ENDIF


  yzroutine = 'calc_cloud_opt'

    ! select time level according to the integration scheme used
    IF ( l2tls ) THEN
      nzx  = nnow
    ELSE
      nzx  = nold
    ENDIF

   ! retrieve the required microphysics tracers (at corresponding timelevel nnew)
    yzerrmsg(:) = ' '

!$$TS
    CALL trcr_get(izerror, 'QC', ptr_tlev = nzx, ptr = qc)
    IF (izerror /= 0) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
!$$TS
    CALL trcr_get(izerror, 'QI', ptr_tlev = nzx, ptr = qi)
    IF (izerror /= 0) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF


    refft%r_effc = 5.0e-6_wp  ! 5  micrometer  IMPORTANT default for sub-grid clouds
    refft%r_effi_th = 20.0e-6_wp ! 20 micrometer  NOT USED
    refft%r_effi_so =  20.0e-6_wp ! 20 micrometer  NOT USED

    zqi_gridscale_thresh=0.0_wp

        CALL calc_reff_rad_2mom (ntlev=nzx, qc_thresh=0.0_wp, qi_thresh=zqi_gridscale_thresh)

        reffc_out = refft%r_effc
        reffi_out = refft%r_effi_so(:,:,:,ihabit)
        
        WHERE(reffc_out ==  1e-05_wp .AND. qc <= 0.0_wp) reffc_out = undefncdf
        WHERE(reffi_out == 20.e-6_wp .AND. qi <= 0.0_wp) reffi_out = undefncdf      
 
      ! .. Effective radii for pure subgrid scale ice clouds (water clouds simply take on the initial value
      !    of r_effc, which is 5 mikrometers):
        CALL prefactors_reff_1mom_icemono (pi=4.0_wp*ATAN(1.0_wp), nue_c=5.0_wp, mue_c=1.0_wp, &
             ageo_i=130.0_wp, bgeo_i=3.0_wp, rho_ice=920.0_wp, &
             rho_w=1000.0_wp, ldebug=.FALSE., &
             c_reffc=c_reffc_sub, c_reffi_so=c_reffi_so_sub, &
             c_reffi_th=c_reffi_th_sub)


        DO  k = 1, ke
          DO j = jstart, jend       
            DO  i = istart, iend
              IF (qi(i,j,k) <= zqi_gridscale_thresh  &
                 .AND. zciwc(i,j,k) > 1.0e-20_wp &
                 .AND. t(i,j,k,nzx) <= t0_melt)        THEN   

                zxi = zciwc(i,j,k) / MAX( zclc(i,j,k) * ice_nuclei_number(t(i,j,k,nzx),t0_melt), 1.0e-20_wp )
                refft%r_effi_so(i,j,k,ihabit) = c_reffi_so_sub(ihabit,1) * zxi**c_reffi_so_sub(ihabit,2)
                refft%r_effi_th(i,j,k,ihabit) = 0.5 / ( &
                                                      c_reffi_th_sub(ihabit,1)*zxi**c_reffi_th_sub(ihabit,2) + &
                                                      c_reffi_th_sub(ihabit,3)*zxi**c_reffi_th_sub(ihabit,4)   )
              END IF
            END DO
          END DO
        END DO


END SUBROUTINE reff_for_rad


SUBROUTINE calc_cloud_opt(jindex,iradpar_cloud)

USE src_twomom_sb_interface, ONLY :   &
           refft

!------------------------------------------------------------------------------


IMPLICIT NONE

!==============================================================================
! Declaration
!------------------------------------------------------------------------------


INTEGER (KIND=iintegers)  :: i, j,jindex, k, jspec,iradpar_cloud

REAL (KIND=wp) :: zqi_gridscale_thresh,  &
                      zr_effc_so,            &
                      zr_effi_so,            &
                      zr_effc_th,            &
                      zr_effi_th,            &
                      zclc   (ie,jloc,ke ),    &  ! Cloud cover in each layer
                      r_effc_so(ie,jloc,ke ),    &
                      r_effc_th(ie,jloc,ke ),    &
                      r_effi_so(ie,jloc,ke,2 ),    & ! 2 is number of ice habits
                      r_effi_th(ie,jloc,ke,2 )


REAL    (KIND=wp   ) ::  &
    ! individual optical properties of liquid water and ice 
    z_lwe, z_iwe,          & ! extinction coefficient
    z_lww, z_iww,          & ! single scattering coefficient
    z_lwg, z_iwg,          & ! asymetry factor
    z_lwf, z_iwf,          & ! forward scattered fraction 
    zzg

REAL    (KIND=wp   ), PARAMETER ::  &
     z1dg   = 1.0/9.80665, & ! 1./g
     z1d8   = 0.125      , & ! 1./8 
     zepssa = 1.0E-6         ! Security constant for single scattering albedo


  !--------------------------------------
  ! copy global effective radii to local arrays

    r_effc_so(:,jloc,:) = refft%r_effc(:,jindex,:)
    r_effc_th = r_effc_so       ! use same definition for so and th
    r_effi_so(:,jloc,:,:) = refft%r_effi_th(:,jindex,:,:)
    r_effi_th = r_effi_so       ! use same definition for so and th

  !--------------------------------------

  !-------------
  ! Optical properties of liquid water and ice as function of the specific
  ! liquid water and ice content
  !
  ! Liquid water:
  !
  ! Hu and Stamnes (1993) - spectrally remapped by Elias Zubler
  !
  ! Cloud ice:
  !
  ! Solar Fu et al. (1996) - spectrally remapped by Bodo Ritter
  ! IR Fu et al. (1998) - spectrally remapped by Bodo Ritter
  !-------------

  ! Loop over solar spectral intervals
  ! ===============================================================
  DO jspec = 1, jpsol
  ! ===============================================================
    DO k = 1, ke
      DO j = 1, jloc
        DO i = 1, ie

      ! Liquid water

      ! Parameterization according to Hu & Stamnes (1993), which is based in parts
      !   on Ackerman and Stephens (1987), Tsay (1989)
      ! z_lwe = extinction coefficient / LWC in [km-1 g-1 m^3] ->
      !   ext. coeff[1/Pa] = ext. coeff[1/m] / (g[m s^-2] * LWC[kg m^-3]) = z_lwe / g * 10^-3 * 10^3
      ! coeffs from Elias Zubler, ETHZ

        zr_effc_so = MIN(MAX(1.e6_wp*r_effc_so(i,j,k),5.0_wp), 60.0_wp)   ! meters -> micrometers

        IF ( zr_effc_so .LT. 12.5 ) THEN
          z_lwg    = zgc1(1,jspec) * zr_effc_so**zgc1(2,jspec) + zgc1(3,jspec)
          z_lwg    = MAX (0.0_wp,MIN(1.0_wp,z_lwg))
          z_lwf    = z_lwg*z_lwg
          ! single scattering albedo
          z_lww    = 1.0_wp - zwc1(1,jspec)*zr_effc_so**zwc1(2,jspec) - zwc1(3,jspec)
          z_lww    = MAX(zepssa,MIN(1.0_wp,z_lww))
          ! extinction coefficient
          z_lwe    = z1dg * ( zec1(1,jspec)*zr_effc_so**zec1(2,jspec) + zec1(3,jspec) )
          z_lwe    = MAX(zlwemn_loc(jspec,iradpar_cloud),MIN(zlwemx_loc(jspec,iradpar_cloud),z_lwe))
        ELSEIF ( zr_effc_so .LT. 30.0 ) THEN
          z_lwg    = zgc2(1,jspec) * zr_effc_so**zgc2(2,jspec) + zgc2(3,jspec)
          z_lwg    = MAX (0.0_wp,MIN(1.0_wp,z_lwg))
          z_lwf    = z_lwg*z_lwg
        ! single scattering albedo
          z_lww    = 1.0_wp - zwc2(1,jspec)*zr_effc_so**zwc2(2,jspec) - zwc2(3,jspec)
          z_lww    = MAX(zepssa,MIN(1.0_wp,z_lww))
          ! extinction coefficient
          z_lwe    = z1dg * ( zec2(1,jspec)*zr_effc_so**zec2(2,jspec) + zec2(3,jspec) )
          z_lwe    = MAX(zlwemn_loc(jspec,iradpar_cloud),MIN(zlwemx_loc(jspec,iradpar_cloud),z_lwe))
        ELSE
          z_lwg    = zgc3(1,jspec) * zr_effc_so**zgc3(2,jspec) + zgc3(3,jspec)
          z_lwg    = MAX (0.0_wp,MIN(1.0_wp,z_lwg))
          z_lwf    = z_lwg*z_lwg
          ! single scattering albedo
          z_lww    = 1.0_wp - zwc3(1,jspec)*zr_effc_so**zwc3(2,jspec) - zwc3(3,jspec)
          z_lww    = MAX(zepssa,MIN(1.0_wp,z_lww))
          ! extinction coefficient
          z_lwe    = z1dg * ( zec3(1,jspec)*zr_effc_so**zec3(2,jspec) + zec3(3,jspec) )
          z_lwe    = MAX(zlwemn_loc(jspec,iradpar_cloud),MIN(zlwemx_loc(jspec,iradpar_cloud),z_lwe))
        ENDIF

        zlwoda_so_prefac(i,j,k,jspec)= z_lwe  * (1.-z_lww)
        zlwods_so_prefac(i,j,k,jspec)= z_lwe  *     z_lww  * (1.-z_lwf)
        zlwb0_so (i,j,k,jspec)= z1d8*(4.+z_lwg)/(1.+z_lwg)
!        zlwb_so  (i,j,k) = 0.5-0.75*psmu0(i,j)*z_lwg/(1.+z_lwg)    
        zlwb_so_prefac  (i,j,k,jspec) = 0.5-0.75*z_lwg/(1.+z_lwg)    

!      ! optical thickness    do it in opt_XX because water content is used
      w_extinct(i,j,k,jspec) = z_lwe ! * z_lww  ! ???



      ! Cloud ice
      !----------

      ! Parameterization following Fu et al. (Journal of Climate, 1996)
      !  for his hexagonal columns.
      ! coeffs from Bodo Ritter, also used in GME
      ! for IWC in g/m^3, r_effi_so in um


      ! limits of Fu et al. (1996) parameterization:
      ! limits for D_eff correspond to 11 um and 129.6 um
      zr_effi_so = MAX(MIN(1.e6_wp * r_effi_so(i,j,k,ihabit),70.0_wp),5.5_wp)   ! meters -> micrometers

      ! From GME CODE, slightly modified to COSMO conventions
      ! Fu Eq. 3.9 a)  (modified, added quadratic term)
      ! Coeffs only valid for ihabit = 2 !!!
      z_iwe   =   zfuia(1,jspec,ihabit)                     + &
                  zfuia(2,jspec,ihabit)/(2.0*zr_effi_so)    + &
                  zfuia(3,jspec,ihabit)/(2.0*zr_effi_so)**2
      ! convert extinction coefficient to 1/Pa ( = extinction coefficient[1/m] / (g*IWC[g/m3]) * 10^3 ):
      z_iwe    =  z_iwe * z1dg * 1e3_wp
      ! limit extinction coefficient to allowed range:
      z_iwe    = MAX(ziwemn_loc(jspec,iradpar_cloud),MIN(ziwemx_loc(jspec,iradpar_cloud),z_iwe))

      ! From Fu Eq. 3.9 b) for omega
      ! Original fit is for coalbedo. To get single scat. albedo, substract it from 1.0
      ! Coeffs only valid for ihabit = 2 !!!
      z_iww   =  1.0 - ( &
                  zfuib(1,jspec,ihabit)                     + &
                  zfuib(2,jspec,ihabit)*(2.0*zr_effi_so)    + &
                  zfuib(3,jspec,ihabit)*(2.0*zr_effi_so)**2 + &
                  zfuib(4,jspec,ihabit)*(2.0*zr_effi_so)**3   &
                  )
      z_iww    = MAX(1.E-12_wp,MIN(1.0_wp,z_iww) )

      ! Fu Eq. 3.9 c) for g
      ! Coeffs only valid for ihabit = 2 !!!
      z_iwg   =   zfuic(1,jspec,ihabit)                     + &
                  zfuic(2,jspec,ihabit)*(2.0*zr_effi_so)    + &
                  zfuic(3,jspec,ihabit)*(2.0*zr_effi_so)**2 + &
                  zfuic(4,jspec,ihabit)*(2.0*zr_effi_so)**3
      z_iwg    = MAX (0.0_wp,MIN(1.0_wp,z_iwg))

     ! Fu Eq. 3.9 d) for f_delta
      ! Coeffs only valid for ihabit = 2 !!!
      z_iwf   =   zfuid(1,jspec,ihabit)                          + &
                  zfuid(2,jspec,ihabit)*(2.0_wp*zr_effi_so)    + &
                  zfuid(3,jspec,ihabit)*(2.0_wp*zr_effi_so)**2 + &
                  zfuid(4,jspec,ihabit)*(2.0_wp*zr_effi_so)**3

      ! In this case, Fu gives an explicit parameterization of the forward
      ! scattering peak fraction f. We use it here explicitly instead
      ! of the f = g^2 approximation in other parts of the code:
      !
      !   f = 1 / (2*omega) + f_delta
      !      (f_delta=z_iwf as above, omega=z_iww as above)
      !
      ! (cf. Fu (1996), Eq. 3.8 and comment on next page top left)
      z_iwf  = MIN ( MAX(0.5/z_iww+z_iwf, 0.0_wp), z_iwg)

      ziwoda_so_prefac(i,j,k,jspec) = z_iwe  * (1.0-z_iww)
      ziwods_so_prefac(i,j,k,jspec) = z_iwe  *      z_iww *(1.-z_iwf)
      ziwb0_so (i,j,k,jspec) = z1d8  * (4.+z_iwg)       / (1.+z_iwg)
!      ziwb_so  (i,j,k) = 0.5-0.75*psmu0(i,j)*z_iwg/(1.+z_iwg)
      ziwb_so_prefac  (i,j,k,jspec) = 0.5-0.75*z_iwg/(1.+z_iwg)

      i_extinct(i,j,k,jspec) = z_iwe ! * z_iww  ! ???

        ENDDO
      ENDDO
    ENDDO
  ENDDO

  ! Loop over thermal spectral intervals
  !================================================================
  DO jspec=jpsol+1,jpspec
  !================================================================
    DO k = 1, ke
      DO j = 1, jloc
        DO i = 1, ie

        ! Liquid water

        ! Parameterization according to Hu & Stamnes (1993), which is based in parts
        !   on Ackerman and Stephens (1987), Tsay (1989)
        ! z_lwe = extinction coefficient / LWC in [km-1 g-1 m^3] ->
        !   ext. coeff[1/Pa] = ext. coeff[1/m] / (g[m s^-2] * LWC[kg m^-3]) = z_lwe / g * 10^-3 * 10^3

        zr_effc_th = MIN(MAX(1.e6_wp*r_effc_th(i,j,k),5.0_wp), 60.0_wp)   ! meters -> micrometers

        IF ( zr_effc_th .LT. 12.5 ) THEN
          z_lwg    = zgc1(1,jspec) * zr_effc_th**zgc1(2,jspec) + zgc1(3,jspec)
          z_lwg    = MAX (0.0_wp,MIN(1.0_wp,z_lwg))
          z_lwf    = z_lwg*z_lwg
          ! single scattering albedo
          z_lww    = 1.0_wp - zwc1(1,jspec)*zr_effc_th**zwc1(2,jspec) - zwc1(3,jspec)
          z_lww    = MAX(zepssa,MIN(1.0_wp,z_lww))
          ! extinction coefficient [1/Pa]:
          z_lwe    = z1dg * ( zec1(1,jspec)*zr_effc_th**zec1(2,jspec) + zec1(3,jspec) )
          z_lwe    = MAX(zlwemn_loc(jspec,iradpar_cloud),MIN(zlwemx_loc(jspec,iradpar_cloud),z_lwe))
        ELSEIF ( zr_effc_th .LT. 30.0 ) THEN
          z_lwg    = zgc2(1,jspec) * zr_effc_th**zgc2(2,jspec) + zgc2(3,jspec)
          z_lwg    = MAX (0.0_wp,MIN(1.0_wp,z_lwg))
          z_lwf    = z_lwg*z_lwg
          ! single scattering albedo
          z_lww    = 1.0_wp - zwc2(1,jspec)*zr_effc_th**zwc2(2,jspec) - zwc2(3,jspec)
          z_lww    = MAX(zepssa,MIN(1.0_wp,z_lww))
          ! extinction coefficient [1/Pa]:
          z_lwe    = z1dg * ( zec2(1,jspec)*zr_effc_th**zec2(2,jspec) + zec2(3,jspec) )
          z_lwe    = MAX(zlwemn_loc(jspec,iradpar_cloud),MIN(zlwemx_loc(jspec,iradpar_cloud),z_lwe))
        ELSE
          z_lwg    = zgc3(1,jspec) * zr_effc_th**zgc3(2,jspec) + zgc3(3,jspec)
          z_lwg    = MAX (0.0_wp,MIN(1.0_wp,z_lwg))
          z_lwf    = z_lwg*z_lwg
          ! single scattering albedo
          z_lww    = 1.0_wp - zwc3(1,jspec)*zr_effc_th**zwc3(2,jspec) - zwc3(3,jspec)
          z_lww    = MAX(zepssa,MIN(1.0_wp,z_lww))
          ! extinction coefficient [1/Pa]:
          z_lwe    = z1dg * ( zec3(1,jspec)*zr_effc_th**zec3(2,jspec) + zec3(3,jspec) )
          z_lwe    = MAX(zlwemn_loc(jspec,iradpar_cloud),MIN(zlwemx_loc(jspec,iradpar_cloud),z_lwe))
        ENDIF

        zlwoda_th_prefac(i,j,k,jspec)= z_lwe  * (1.-z_lww)
        zlwods_th_prefac(i,j,k,jspec)= z_lwe  *     z_lww  * (1.-z_lwf)
        zlwb0_th (i,j,k,jspec)= z1d8*(4.+z_lwg)/(1.+z_lwg)

        w_extinct(i,j,k,jspec) = z_lwe ! * z_iww  ! ???


        ! Cloud ice
        ! ----

        ! Parameterization following Fu et al. (1998) for his hexagonal columns
        !   - r_effi_th in mu-m, IWC in g/m^3, then beta in [1/m] (therefore factor 1000 in z_iwe)
        !   - conversion of z_iwe to 1/Pa
        ! coeffs from Bodo Ritter, also used in GME

        ! limits of Fu et al. (1998) parameterization:
        ! limits for D_eff correspond to 11 um and 129.6 um
        zr_effi_th = MAX(MIN(1.e6_wp * r_effi_th(i,j,k,ihabit),70.0_wp),5.5_wp)   ! meters -> micrometers

        ! Coeffs only valid for ihabit = 2 !!!

        z_iwg    = zfuic(1,jspec,ihabit)                     + &
                   zfuic(2,jspec,ihabit)*(2.0*zr_effi_th)    + &
                   zfuic(3,jspec,ihabit)*(2.0*zr_effi_th)**2 + &
                   zfuic(4,jspec,ihabit)*(2.0*zr_effi_th)**3       ! asymmetry coefficient g
        z_iwg    = MAX (0.0_wp,MIN(1.0_wp,z_iwg))
        z_iwf    = z_iwg*z_iwg

        z_iwe    = zfuia(1,jspec,ihabit) +                       &
                   zfuia(2,jspec,ihabit) / (2.0*zr_effi_th)    + &
                   zfuia(3,jspec,ihabit) / (2.0*zr_effi_th)**2     ! extinction coefficient / IWC [m-1 m3 g-1]

        ! Original fit is for absorption coefficient. To get single scat. albedo, divide by ext. coeff z_iwe
        !   and substract it from 1.0
        z_iww    = 1.0 - (                                             &
                   zfuib(1,jspec,ihabit) / (2.0_wp*zr_effi_th)  + &
                   zfuib(2,jspec,ihabit) +                            &
                   zfuib(3,jspec,ihabit)*(2.0_wp*zr_effi_th)    + &
                   zfuib(4,jspec,ihabit)*(2.0_wp*zr_effi_th)**2   &
                   ) / z_iwe
        z_iww    = MAX(1.E-12_wp , MIN(1.0_wp,z_iww))     ! single scattering albedo

        ! convert extinction coefficient to 1/Pa ( = extinction coefficient[1/m] / (g*IWC[g/m3]) * 10^3 ):
        z_iwe    =  z_iwe * z1dg * 1e3_wp
        ! limit extinction coefficient to allowed range:
        z_iwe    = MAX(ziwemn_loc(jspec,iradpar_cloud),MIN(ziwemx_loc(jspec,iradpar_cloud),z_iwe))

        ziwoda_th_prefac(i,j,k,jspec) = z_iwe  * (1.-z_iww)
        ziwods_th_prefac(i,j,k,jspec) = z_iwe  *     z_iww  * (1.-z_iwf)
        ziwb0_th (i,j,k,jspec) = z1d8  * (4.+z_iwg)       / (1.+z_iwg)

        i_extinct(i,j,k,jspec) = z_iwe ! * z_iww  ! ???


        ENDDO
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE calc_cloud_opt

SUBROUTINE calc_optthick_cloud(js,dp0,zclwc,zciwc,lsolar)

USE src_twomom_sb_interface, ONLY :   &
           refft

USE data_fields,             ONLY:    &
   odepthw_so,    &
   odepthi_so,    &
   odepthw_th,    &
   odepthi_th


IMPLICIT NONE

INTEGER (KIND=iintegers) ::  &
   js

REAL (KIND=wp) ::        &
   dp0(ie,ke),      &
!   zclc(ie,ke),     &
   zclwc(ie,ke),    &
   zciwc(ie,ke)

LOGICAL :: lsolar

   refft%odepthw_so(:,js,:) = 0.0_wp
   refft%odepthi_so(:,js,:) = 0.0_wp
   refft%odepthw_th(:,js,:) = 0.0_wp
   refft%odepthi_th(:,js,:) = 0.0_wp

   IF (lsolar) THEN
     ! optical thickness in spectral band Nr. 2:
     refft%odepthw_so(istartpar:iendpar,js,:) = dp0(istartpar:iendpar,:)       &
                                              * w_extinct(istartpar:iendpar,1,:,2) &
                                              * zclwc(istartpar:iendpar,:)

    refft%odepthi_so(istartpar:iendpar,js,:) = dp0(istartpar:iendpar,:)        &
                                             * i_extinct(istartpar:iendpar,1,:,2)  &
                                             * zciwc(istartpar:iendpar,:)
   ELSE
     ! if .not. lsolar, then no solar optical properties
     ! of clouds have been calculated in fesft, therefore set missing value:
     refft%odepthw_so(:,js,:) = -999.99_wp
     refft%odepthi_so(:,js,:) = -999.99_wp
   END IF

   ! optical thickness in spectral band Nr. jpsol + 2:
     refft%odepthw_th(istartpar:iendpar,js,:) = dp0(istartpar:iendpar,:)       &
                                              * w_extinct(istartpar:iendpar,1,:,jpsol+2) &
                                              * zclwc(istartpar:iendpar,:)

    refft%odepthi_th(istartpar:iendpar,js,:) = dp0(istartpar:iendpar,:)        &
                                             * i_extinct(istartpar:iendpar,1,:,jpsol+2)  &
                                             * zciwc(istartpar:iendpar,:)

    
    odepthw_so = refft%odepthw_so
    odepthi_so = refft%odepthi_so
    odepthw_th = refft%odepthw_th
    odepthi_th = refft%odepthi_th

END SUBROUTINE calc_optthick_cloud


END MODULE src_cloud_opt_reff
#endif
