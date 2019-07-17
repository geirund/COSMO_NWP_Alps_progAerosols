!+ Source Module for setting up the variable table
!------------------------------------------------------------------------------

MODULE src_setup_vartab

!------------------------------------------------------------------------------
!
! Description:
!   This module initializes the LM table of all GRIB variables used.
!   Because this takes a long time to compile, it has been taken out of the
!   external subroutine organize_data.
!
! Method:
!
! Current Code Owner: DWD, Ulrich Schaettler
!  phone:  +49  69  8062 2739
!  fax:    +49  69  8062 3721
!  email:  ulrich.schaettler@dwd.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 3.13       2004/12/03 Ulrich Schaettler
!  Initial release
! 3.16       2005/07/22 Ulrich Schaettler
!  Added fields to the table: for_e, for_d, clc_con, clw_con
!  Added an additional entry in the list description (idef_stat), to indicate
!  whether a corresponding field is allocated
! 3.17       2005/12/12 Ulrich Schaettler
!  Bug correction for pointer connection to for_e, for_d
!  Added fields rho_snow (prognostic snow density), h_snow (snow height)
! 3.18       2006/03/03 Ulrich Schaettler
!  Introduction of NetCDF-variables to structure ar_des (for every variable)
!  Introduction of new fields (for CLM Version)
!  Added fields for the lake model FLake (Dmitrii Mironov)
! 3.19       2006/04/25 Ulrich Schaettler
!  Added field t_s_lake for the lake model
! 3.21       2006/12/04 Burkhardt Rockel, Ulrich Schaettler, Christoph Schraff
!  Renamed sunshhrs to dursun; corrected 2m-coding for QV_2M;
!  Corrected several netCDF standard_names
!  All variable names in uppercase letters (dBZ -> DBZ)
!  Bug correction: pointers u_10m_av, v_10m_av were not set correct
!  Set pointers for qr_bd, qs_bd, qg_bd
!  Time integrated analysis increment fields introduced.
! V3_23        2007/03/30 Ulrich Schaettler
!  Adaptation for Restart in case of ldiabf_lh is true.
!  Added new variable CEILING in the table (M. Baldauf)
! V3_24        2007/04/26 Ulrich Schaettler
!  Corrected CF name, description and unit for TKE
! V3_25        2007/05/21 Ulrich Schaettler
!  Corrected CF units for W_I and QCVG_CON
! V4_1         2007/12/04 Ulrich Schaettler
!  Two New fields for SDI (Supercell detection index)
! V4_3         2008/02/25 Matthias Raschendorfer, et al.
!  Added new fields: edr, qh, qnc, qnr, qni, qns, qng, qnh, prh_gsp, hail_gsp
!  Changed some numbers, which are now official: horizon, swdir_cor, slo_ang,
!   slo_asp, skyview, aswdir_s, aswdifd_s, aswdifu_s, alwd_s, alwu_s, tinc_lh
! V4_4         2008/07/16 Ulrich Schaettler, Jan-Peter Schulz
!  2 new fields for special SWISS indices
!  Activated external parameter fields for sso scheme
! V4_5         2008/09/10 Simone Campagna
!  Corrected some grib numbers in the last section (check_alloc)
! V4_7         2008/12/12 Ulrich Schaettler
!  Fields AVDISSSO, AUSTRSSO, AVSTRSSO get time range indicator 3
! V4_8         2009/02/16 Ulrich Schaettler
!  Implemented additional fields 
!   5 ordered from MeteoSwiss for additional convective indices
!   extra fields for dynamical and convective gusts
!   extra output fields for radiation ordered by CLM; add lsm-flag to FRESHSNW
!   Fields qct_conv, qit_conv for restarts with additional convective tendencies
! V4_9         2009/07/16 Heike Vogel, Ulrich Schaettler
!  Added new GRIB tables for COSMO_ART
! V4_10        2009/09/11 Heike Vogel, Ulrich Schaettler
!  Added new entry for fields BRN and snow_melt
! V4_11        2009/11/30 Ekaterina Machulskaya, Juergen Helmert, Lucio Torrisi
!  Additional fields for multi-layer snow model (EM)
!  Additional fields for aerosols, external emissivity map, stomata resistance (JH)
!  Additional fields for fluxes and stomata resistance
! V4_12        2010/05/11 Michael Baldauf, Ulrich Schaettler
!  New output variables potential vorticity and (relative) vorticity
!  Changed shortnames for QNC, QNR,...,QNH and PRS_MIN because of conflicting
!  names
!  Additional fields for sunshine duration (Oli Fuhrer)
! V4_13        2010/05/11 Michael Gertz
!  Adaptions to SVN
! V4_15        2010/11/19 Ulrich Schaettler
!  The field t_s_lake is removed again after adaptations in the SST Analysis
! V4_18        2011/05/26 Ulrich Schaettler
!  Eliminated Pollen and ART tables, which are put to special COSMO-ART modules
!   (Christoph Knote)
!  Eliminated synsat-variables from the tables, because they are not allocated
!  when initializing the table (and they are not needed in the current implementation)
!  Introduced 4 additional fields for each group of products for syn sat data
!   (Anne Roches et al.)
!  Added instantaneous values of shfl_s, lhfl_s, umfl_s, vmfl_s to the vartab
!   (Uli Blahak)
! V4_20        2011/08/31 Ulrich Blahak
!  Added instantaneous values of swdir_s, swdifd_s, swdifu_s, lwd_s, lwu_s
!    to the vartab
! V4_23        2012/05/10 Ulrich Schaettler, for CLM
!  New fields defined by CLM Community (vabsmx_10m, alb_dry, alb_sat, alb_dif)
!  Definition of additional fields necessary for Restart files and / or usual I/O
!   (ustr_sso, vstr_sso, vdis_sso, du_sso, dv_sso, dt_sso, dursun_m, dursun_r)
!  Change of flag for FR_LAKE; has to be empty ' ' instead of 'l'
!  Add unit W m-2 to SOD_T and ASOD_T
!  Changes to allow rho_snow_mult of the multi layer snow model in netCDF output
!  New flag "i" to distinguish between ocean and inland water (lakes) quantities 
!     (for netCDF output only)
!  Several corrections for netCDF definitions
!  Implementation of time dependent boundary values for aerosol optical depths
!  All SSO related variables got the flag 'l' for land
! V4_25        2012/09/28 Anne Roches, Oliver Fuhrer, Ulrich Blahak, Carlos Osuna
!  Replaced qx-variables by using them from the tracer module
!  For the 2-moment scheme, added prh_gsp, hail_gsp, TQH to the vartab.
!  Removed QNxx, which are now in the tracer definition
!  Add length of K dimension to var table. Netcdf asyn I/O PE need to  
!   know this information for every field, which are not allocated in
!   I/O PE. This static information is used to write netcdf headers. 
!   Using -1 for undefined value.  (Carlos Osuna)
! V4_26        2012/12/06 Hans-Juergen Panitz, Ulrich Schaettler,
!                         Matthias Raschendorfer
!  Change leveltype for SOD_T and ASOD_T from 1 to 8 (HJP)
!  Adapted variable names of multi-layer snow model to corresponding
!   short names for I/O (US)
!  Introduction of the additional output fields 'DTKE_(SSO, HSH, CON)'
!   based on 'tket_(sso, hshr, conv)'  (MR)
! V4_27        2013-03-19 Burkhardt Rockel, Ulrich Schaettler, Astrid Kerkweg
!  Introduction of official CF standard names for high, medium and low cloud cover
!  Corrected Grib scaling factor for new albedo values to 100.0, because these
!    values are given in percent (JH)
!  MESSy interface introduced
! V4_28        2013/07/12 Ulrich Schaettler
!  Added new fiels SP_10M for output of near-surface analysis fields
! V4_29        2013/10/04 Astrid Kerkweg, Ulrich Schaettler
!  Excluded O3 from vartab table for MESSy because of conflicting names
! VX_XX        2014/03/26 Ulrich Blahak
!  Added extra output field tket_adv (adv. tend. of TKE). Needed for restart.
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================

USE data_parameters,    ONLY:  wp, iintegers

!------------------------------------------------------------------------------

USE data_parallel,      ONLY:                              &
    my_cart_id         ! rank of this subdomain in the cartesian communicator

!------------------------------------------------------------------------------

USE data_runcontrol , ONLY :   &
    nhori,         &! number of sectors for the horizont array by the topographic
                    ! correction of the radiation

!OCH >>
    lcmprates
!OCH <<

!------------------------------------------------------------------------------

USE data_io,            ONLY:                              &
    num_gribtabs, & ! number of GRIB tables used in LM variable table
    ar_des,       & ! structure for LM variable table
    var             ! array for LM variable table

!(WL;2010)b
USE data_runcontrol,    ONLY:                              &
    l3dturb
!(WL;2010)e

!------------------------------------------------------------------------------

USE data_modelconfig,   ONLY :           &
    ke,           & ! number of grid points in vertical direction
    ke_soil,      & ! number of layers in the multi-layer soil model
    ke_snow,      & ! number of layers in multi-layer snow model
    ke1             ! KE+1

!------------------------------------------------------------------------------

USE data_fields,        ONLY:                              &

! 1. constant fields for the reference atmosphere                     (unit)
! -----------------------------------------------
    hhl        ,    & ! geometical height of half levels              ( m   )

! 2. external parameter fields                                        (unit)
! ----------------------------
    hsurf      ,    & ! geometical heigt of surface topography        (  m  )
    sso_stdh   ,    & ! standard deviation of sub-grid scale orography( m   )
    sso_gamma  ,    & ! anisotropy of sub-grid scale orography          --
    sso_theta  ,    & ! angle betw. principal axis of orography and E ( rad )
    sso_sigma  ,    & ! mean slope of sub-grid scale orography          --
    gz0        ,    & ! surface roughness  * g                        (m2/s2)
    fr_land    ,    & ! fraction of land in a grid element            ( --  )
    soiltyp    ,    & ! type of the soil (keys 0-9)                   ( --  )
    vio3       ,    & ! vertical integrated ozone contents            (pa O3)
    hmo3       ,    & ! ozone maximum                                 ( pa  )
    rlat       ,    & ! geographical latitude                         ( rad )
    rlon       ,    & ! geographical longitude                        ( rad )
    fc         ,    & ! coriolis-parameter                            ( 1/s )
    plcov      ,    & ! fraction of plant cover                         --
    lai        ,    & ! leaf area index of plants                       --
    rootdp     ,    & ! depth of the roots                            (  m  )
    for_e      ,    & ! ground fraction covered by evergreen forest     --
    for_d      ,    & ! ground fraction covered by deciduous forest     --
    aer_su     ,    & ! monthly aerosol climatology sulfate drops     (0 - 1)
    aer_du     ,    & ! monthly aerosol climatology total dust        (0 - 1)
    aer_or     ,    & ! monthly aerosol climatology organic (water sol.)(0-1)
    aer_bc     ,    & ! monthly aerosol climatology black carbon      (0 - 1)
    aer_ss     ,    & ! monthly aerosol climatology sea salt          (0 - 1)
    emis_rad   ,    & ! external thermal emissivity                   (0 - 1)
    rsmin2d    ,    & ! minimum stomata resistance                    ( s/m )
    fr_lake    ,    & ! lake fraction in a grid element [0,1]         (  -  )
    depth_lk   ,    & ! lake depth                                    (  m  )
    fetch_lk   ,    & ! wind fetch over lake                          (  m  )
    dp_bs_lk   ,    & ! thickness of the thermally active layer
                      ! of bottom sediments                           (  m  )
    t_bs_lk    ,    & ! climatological temperature at the bottom of
                      ! the thermally active layer of sediments       (  K  )
    gamso_lk   ,    & ! attenuation coefficient for
                      ! solar radiation in lake water                 ( 1/m )
    alb_dry    ,    & ! surface albedo field for dry soil
    alb_sat    ,    & ! surface albedo field for saturated soil
    alb_dif           ! diffuse albedo field 

USE data_fields,        ONLY:                              &

! 3. prognostic variables                                             (unit)
! -----------------------
    u          ,    & ! zonal wind speed                              ( m/s )
    v          ,    & ! meridional wind speed                         ( m/s )
    w          ,    & ! vertical wind speed (defined on half levels)  ( m/s )
    t          ,    & ! temperature                                   (  K  )
    pp         ,    & ! deviation from the reference pressure         ( pa  )

! fields of the turbulence scheme defined on half-levels:

    tke        ,    & ! SQRT(2 * turbulent kinetik energy)            ( m/s )
    edr        ,    & ! eddy dissipation rate of TKE (EDR)            ( m2/s3)
    tketens    ,    & ! tendency of TKE
    tket_adv   ,    & ! pure advective tendency of SQRT(2*TKE)        ( m/s2)
    tket_conv  ,    & ! TKE-tendency due to convective buoyancy       ( m2/s3)
    tket_hshr  ,    & ! TKE-tendency due to (sep.) horiz. shear       ( m2/s3)
    tket_sso          ! TKE-tendency due to SSO wake production       ( m2/s3)

USE data_fields,        ONLY:                              &

! 5. fields for surface values and soil model variables               (unit )
! -----------------------------------------------------

    ps        ,     & ! surface pressure                              ( pa  )
    t_snow    ,     & ! temperature of the snow-surface               (  K  )
    t_snow_mult,    & ! temperature of the snow-surface               (  K  )
    wliq_snow ,     & ! liquid water content in the snow              (m H2O)
    w_snow_mult ,   & ! total (liquid + solid) water content of snow  (m H2O)
    dzh_snow_mult,  & ! layer thicknesses between half levels in snow (  m  )
    t_s       ,     & ! temperature of the ground surface             (  K  )
    t_g       ,     & ! weighted surface temperature                  (  K  )
    qv_s      ,     & ! specific water vapor content on the surface   (kg/kg)
    t_m       ,     & ! temperature between upper and medium
                      ! soil layer                                    (  K  )
    t_cl      ,     & ! temperature between medium and lower
                      ! soil layer                                    (  K  )
    t_so      ,     & ! multi-layer soil temperature                  (  K  )
    w_snow    ,     & ! water content of snow                         (m H2O)
    rho_snow  ,     & ! prognostic density of snow                    (kg/m3)
    rho_snow_mult,  & ! prognostic density of snow                    (kg/m3)
    h_snow    ,     & ! snow height                                   (  m  )
    w_i       ,     & ! water content of interception water           (m H2O)
    w_g1      ,     & ! water content of the upper soil layer         (m H2O)
    w_g2      ,     & ! water content of the medium soil layer        (m H2O)
    w_g3      ,     & ! water content of the lower soil layer         (m H2O)
                      ! (if NLWB=3, unused otherwise)
    w_so      ,     & ! multi-layer soil moisture                     (m H2O)
    w_so_ice  ,     & ! multi-layer soil ice                          (m H2O)
    w_cl      ,     & ! climatological water content                  (m H2O)
! LS, b
    s_so      ,     &
! LS, e
    freshsnow ,     & !
    snow_melt ,     & ! snow melt amount                              (kg/m2)
    fr_ice    ,     & ! ice fraction for ocean/lake surfaces          (  -  )
    t_ice     ,     & ! temperature at the snow-ice or
                      ! air-ice interface                             (  K  )
    t_mnw_lk  ,     & ! mean temperature of the water column          (  K  )
    t_wml_lk  ,     & ! mixed-layer temperature                       (  K  )
    t_bot_lk  ,     & ! temperature at the water-bottom sediment
                      ! interface                                     (  K  )
    t_b1_lk   ,     & ! temperature at the bottom of the upper layer
                      ! of the sediments                              (  K  )
    c_t_lk    ,     & ! shape factor with respect to the
                      ! temperature profile in lake thermocline       (  -  )
    h_ice     ,     & ! ice thickness                                 (  m  )
    h_ml_lk   ,     & ! thickness of the mixed-layer                  (  m  )
    h_b1_lk           ! thickness of the upper layer
                      ! of bottom sediments                           (  m  )

USE data_fields,        ONLY:                              &

! 6. fields that are computed in the parametrization and dynamics     (unit )
! ---------------------------------------------------------------
    qc_rad     ,    & ! subgrid-scale specific cloud water            (kg/kg)
    qi_rad     ,    & ! subgrid-scale specific ice water              (kg/kg)
    tinc_lh    ,    & ! temperature increment due to latent heat      (  K  )
    sohr       ,    & ! rate of solar heating                         ( k/s )
    thhr       ,    & ! rate of thermal heating                       ( k/s )
    clc_sgs    ,    & ! subgrid-scale stratiform cloud cover            --
    alb_rad    ,    & ! albedo of the ground                            --
    sobs       ,    & ! solar radiation at the ground                 ( w/m2)
    thbs       ,    & ! thermal radiation at the ground               ( w/m2)
    pabs       ,    & ! photosynthetic active radiation at the ground ( w/m2)
    sobt       ,    & ! solar radiation at the upper boundary         ( w/m2)
                      ! of the atmosphere
    thbt       ,    & ! thermal radiation at the upper boundary       ( w/m2)
                      ! of the atmosphere
!cloud forcing>
    tcft        ,   & ! thermal cloud forcing at the upper boundary   ( w/m2)
    scft        ,   & ! solar cloud forcing at the upper boundary   ( w/m2)
    scfs        ,   & ! solar cloud forcing at the lower boundary     ( w/m2)
    tcfs        ,   & ! thermal cloud forcing at the lower boundary   ( w/m2)
!cloud forcing<
    sobcs      ,    & ! clear-sky solar radiation at the ground       ( w/m2)
    thbcs      ,    & ! clear-sky thermal radiation at the ground     ( w/m2)
    sobct      ,    & ! clear-sky solar radiation at upper boundary   ( w/m2)
    thbct      ,    & ! clear-sky thermal radiation at upper boundary ( w/m2)
    clch       ,    & ! cloud cover with high clouds                    --
    clcm       ,    & ! cloud cover with medium clouds                  --
    clcl       ,    & ! cloud cover with low clouds                     --
    clct       ,    & ! total cloud cover                               --
    tkvm       ,    & ! turbulent diffusion coefficient for momentum  (m2/s)
    tkvh       ,    & ! turbulent diffusion coefficient for heat      (m2/s)
    !(WL;2010)b
    tkhm       ,    & ! turbulent hor. diffusion coefficient for momentum  (m2/s)
    tkhh       ,    & ! turbulent hor. diffusion coefficient for heat      (m2/s)
    !(WL;2010)e
    tcm        ,    & ! transfer coefficient for momentum             ( -- )
    tch        ,    & ! transfer coefficient for heat and moisture    ( -- )
    rcld              ! standard deviation of the saturation deficit    --

USE data_fields,        ONLY:                              &

    clc_con    ,    & ! cloud cover due to convection                   --
    clw_con    ,    & ! convective cloud liquid water
    prr_con    ,    & ! precipitation rate of rain, convective        (kg/m2*s)
    prs_con    ,    & ! precipitation rate of snow, convective        (kg/m2*s)
    bas_con    ,    & ! level index of convective cloud base            --
    top_con    ,    & ! level index of convective cloud top             --
    mflx_con   ,    & ! cloud base massflux                           (kg/m2*s)
    cape_con   ,    & ! convective available energy                   (   J/kg)
    tke_con    ,    & ! convective turbulent energy                   (   J/kg)
    qcvg_con   ,    & ! moisture convergence for Kuo-type closure     (    1/s)
    ut_conv    ,    & ! u-tendency due to convection                  ( m/s^2)
    vt_conv    ,    & ! v-tendency due to convection                  ( m/s^2)
    tt_conv    ,    & ! temperature tendency due to convection        ( K/s  )
    qvt_conv   ,    & ! humidity    tendency due to convection        ( 1/s  )
    qct_conv   ,    & ! qc tendency due to convection                 ( 1/s  )
    qit_conv   ,    & ! qi tendency due to convection                 ( 1/s  )
    qrs        ,    & ! specific precip. water content                (kg/kg)
    prr_gsp    ,    & ! precipitation rate of rain, grid-scale        (kg/m2*s)
    prs_gsp    ,    & ! precipitation rate of snow, grid-scale        (kg/m2*s)
    prg_gsp    ,    & ! precipitation rate of graupel, grid-scale     (kg/m2*s)
    prh_gsp           ! precipitation rate of hail, grid-scale        (kg/m2s)

USE data_fields,        ONLY:                              &

    dqvdt      ,    & ! threedimensional moisture convergence         ( 1/s )
    qvsflx     ,    & ! surface flux of water vapour                  (kg/m2s)
    dpsdt      ,    & ! tendency of the surface pressure              ( pa/s)
    aumfl_s    ,    & ! average u-momentum flux (surface)             ( N/m2)
    umfl_s     ,    & ! u-momentum flux (surface)                     ( N/m2)
    avmfl_s    ,    & ! average v-momentum flux (surface)             ( N/m2)
    vmfl_s     ,    & ! v-momentum flux (surface)                     ( N/m2)
    def13     ,     & ! deformation tensor comp. 13                   ( m2/s)
    def23     ,     & ! deformation tensor comp. 23                   ( m2/s)
    def33     ,     & ! deformation tensor comp. 23                   ( m2/s)
    def12     ,     & ! deformation tensor comp. 23                   ( m2/s)
    def11     ,     & ! deformation tensor comp. 23                   ( m2/s)
    def22     ,     & ! deformation tensor comp. 23                   ( m2/s)
    ashfl_s    ,    & ! average sensible heat flux (surface)          ( W/m2)
    shfl_s     ,    & ! sensible heat flux (surface)                  ( W/m2)
    alhfl_s    ,    & ! average latent heat flux (surface)            ( W/m2)
    lhfl_s     ,    & ! latent heat flux (surface)                    ( W/m2)
    swdir_s  ,      & ! direct comp. of solar radiative flux at surface ( W/m2)
    swdifd_s ,      & ! diffuse downward comp. of short wave rad. flux  ( W/m2)
    swdifu_s ,      & ! diffuse upward   comp. of short wave rad. flux  ( W/m2)
    lwd_s    ,      & !         downward comp. of long  wave rad. flux  ( W/m2)
    lwu_s    ,      & !         upward   comp. of long  wave rad. flux  ( W/m2)
    aswdir_s   ,    & ! direct comp. of solar radiative flux at surface ( W/m2)
    aswdifd_s  ,    & ! diffuse downward comp. of short wave rad. flux  ( W/m2)
    aswdifu_s  ,    & ! diffuse upward   comp. of short wave rad. flux  ( W/m2)
    alwd_s     ,    & !         downward comp. of long  wave rad. flux  ( W/m2)
    alwu_s     ,    & !         upward   comp. of long  wave rad. flux  ( W/m2)
    swdir_cor  ,    & ! direct short wave radiation correction factor actual value
    skyview    ,    & ! sky view
    slo_asp    ,    & ! slope aspect
    slo_ang    ,    & ! slope angle
    horizon    ,    & ! horizon
    austr_sso  ,    & ! average of ustr_sso                           ( N/m2)
    avstr_sso  ,    & ! average of vstr_sso                           ( N/m2)
    avdis_sso  ,    & ! average of vdis_sso                           ( W/m2)
    ustr_sso   ,    & ! u-stress due to sso                           ( N/m2)
    vstr_sso   ,    & ! v-stress due to sso                           ( N/m2)
    vdis_sso   ,    & ! vert. integrated kinetic energy due to sso    ( W/m2)
    ut_sso     ,    & ! u-tendency due to sso                         ( m/s2)
    vt_sso     ,    & ! v-tendency due to sso                         ( m/s2)
    tt_sso            ! t-tendency due to sso                         ( K/s )

USE data_fields,        ONLY:                              &

! 7. fields for model output and diagnostics                          (unit )
! ------------------------------------------
    t_2m       ,    & ! temperature in 2m                             (  K  )
    t_2m_av    ,    & ! time mean temperature in 2m                   (  K  )
    qv_2m      ,    & ! specific water vapor content in 2m            (kg/kg)
    td_2m      ,    & ! dew-point in 2m                               (  K  )
    td_2m_av   ,    & ! dew-point in 2m                               (  K  )
    rh_2m      ,    & ! relative humidity in 2m                       (  %  )
    u_10m      ,    & ! zonal wind in 10m                             ( m/s )
    u_10m_av   ,    & ! time_mean zonal wind in 10m                   ( m/s )
    v_10m      ,    & ! meridional wind in 10m                        ( m/s )
    v_10m_av   ,    & ! time mean meridional wind in 10m              ( m/s )
    tmin_2m    ,    & ! minimum temperature in 2m                     (  K  )
    tmax_2m    ,    & ! maximum temperature in 2m                     (  K  )
    vmax_10m   ,    & ! maximal windspeed in 10m                      ( m/s )
    vabsmx_10m ,    & ! maximal windspeed in 10m without gust         ( m/s )
    vgust_dyn  ,    & ! maximal dynamical wind gust in 10m            ( m/s )
    vgust_con  ,    & ! maximal convective wind gust in 10m           ( m/s )
    asob_s     ,    & ! average solar radiation budget (surface)      ( W/m2)
    athb_s     ,    & ! average thermal radiation budget (surface)    ( W/m2)
    apab_s     ,    & ! average photosynthetic active radiation (sfc) ( W/m2)
    asob_t     ,    & ! average solar radiation budget (model top)    ( W/m2)
    athb_t     ,    & ! average thermal radiation budget (model top)  ( W/m2)
     sod_t     ,    & ! solar downward radiation at top of atmosphere (     )
    asod_t     ,    & ! averaged solar downward radiation at top      (     )
    asobc_s      ,  & ! averaged clear-sky solar radiation at the ground       ( w/m2)
    athbc_s      ,  & ! averaged clear-sky thermal radiation at the ground     ( w/m2)
    asobc_t      ,  & ! averaged clear-sky solar radiation at upper boundary   ( w/m2)
    athbc_t      ,  & ! averaged clear-sky thermal radiation at upper boundary ( w/m2)

    dursun     ,    & ! sunshine duration                             (  s  )
    dursun_m   ,    & ! maximum possible sunshine duration             (  s  )
    dursun_r   ,    & ! relative sunshine duration                     (  s  )
    rain_gsp   ,    & ! amount of rain from grid-scale precip. (sum)  (kg/m2)
    snow_gsp   ,    & ! amount of snow from grid-scale precip. (sum)  (kg/m2)
    grau_gsp   ,    & ! amount of graupel from grid-scale precip. (sum)  (kg/m2)
    hail_gsp   ,    & ! amount of hail from grid-scale prec.    (sum) (kg/m2 )
    rain_con   ,    & ! amount of rain from convective precip. (sum)  (kg/m2)
    snow_con   ,    & ! amount of snow from convective precip. (sum)  (kg/m2)
    runoff_s   ,    & ! surface water runoff; sum over forecast       (kg/m2)
    runoff_g   ,    & ! soil water runoff; sum over forecast          (kg/m2)
    tdiv_hum   ,    & ! vertical integral divergence of humidity      (kg/m2)
    rstom      ,    & ! stomata resistance                            ( s/m )
    alhfl_bs   ,    & ! average latent heat flux from bare soil evap. ( W/m2)
    alhfl_pl   ,    & ! average latent heat flux from plants          ( W/m2)
    aevap_s           ! accumulated surface moisture flux             (kg/m2)

#ifdef TEND
USE data_fields,        ONLY:                              &
! Variables for tendency-sum output (dmaurer)
    ttend_rad  ,    & ! Radiation Tendency T                          (K/s)
    ttend_sso  ,    & ! SSO Tendency T                                (K/s)
    ttend_tur  ,    & ! Turbulence Tendency T                         (K/s)
    ttend_tmp1 ,    & ! Turbulence Tendency T tmp1                    (K/s)
    ttend_tmp2 ,    & ! Turbulence Tendency T tmp2                    (K/s)
    ttend_con  ,    & ! Shallow convection Tendency T                 (K/s)
    ttend_hyd  ,    & ! Hydci_pp_gr Tendency T                        (K/s)
    ttend_tot  ,    & ! Total Tendency T                              (K/s)
    qvten_tur  ,    & ! Turbulence Tendency QV                        (kg/kg/s)
    qvten_con  ,    & ! Shallow convection Tendency QV                (kg/kg/s)
    qvten_hyd  ,    & ! Hydci_pp_gr Tendency QV                       (kg/kg/s)
    qvten_tot  ,    & ! Total Tendency QV                             (kg/kg/s)
    utend_sso  ,    & ! SSO Tendency U                                (m/s2)
    utend_tur  ,    & ! Turbulence Tendency U                         (m/s2)
    utend_tmp1 ,    & ! Turbulence Tendency U tmp1                    (m/s2)
    utend_tmp2 ,    & ! Turbulence Tendency U tmp2                    (m/s2)
    utend_tot  ,    & ! Total Tendency U                              (m/s2)
    vtend_sso  ,    & ! SSO Tendency V                                (m/s2)
    vtend_tur  ,    & ! Turbulence Tendency V                         (m/s2)
    vtend_tmp1 ,    & ! Turbulence Tendency V tmp1                    (m/s2)
    vtend_tmp2 ,    & ! Turbulence Tendency V tmp2                    (m/s2)
    vtend_tot  ,    & ! Total Tendency V                              (m/s2)
    ttend_tmp3,     & ! T tendency before turbulence scheme           (K/s)
    utend_tmp3,     & ! U tendency before turbulence scheme           (m/s2)
    vtend_tmp3,     & ! V tendency before turbulence scheme           (m/s2)
    qvten_tmp3,     & ! QV tendency before turbulence scheme          (kg/kg/s)
    ttend_tmp4,     & ! T tendency after turbulence scheme            (K/s)
    utend_tmp4,     & ! U tendency after turbulence scheme            (m/s2)
    vtend_tmp4,     & ! V tendency after turbulence scheme            (m/s2)
    qvten_tmp4,     & ! QV tendency after turbulence scheme           (kg/kg/s)
    ttend_tur2,     & ! T tendency turbulence scheme 2                (K/s)
    utend_tur2,     & ! U tendency turbulence scheme 2                (m/s2)
    vtend_tur2,     & ! V tendency turbulence scheme 2                (m/s2)
    qvten_tur2,     & ! QV tendency turbulence scheme 2               (kg/kg/s)
    countref,       & ! Counter reference
    counter           ! Counter
#endif

USE data_fields,        ONLY:                              &

! 8. fields for the boundary values                                   (unit )
! ---------------------------------
    u_bd          , & ! b undary field for u                          ( m/s )
    v_bd          , & ! boundary field for v                          ( m/s )
    w_bd          , & ! boundary field for w                          ( m/s )
    t_bd          , & ! boundary field for t                          (  k  )
    pp_bd         , & ! boundary field for pp                         (  pa )
    qv_s_bd       , & ! boundary field for qv_s                       (kg/kg)
    t_snow_bd     , & ! boundary field for t_snow                     (  k  )
    t_s_bd        , & ! boundary field for t_s                        (  k  )
    t_m_bd        , & ! boundary field for t_m                        (  k  )
    w_snow_bd     , & ! boundary field for w_snow                     (m H2O)
    w_g1_bd       , & ! boundary field for w_g1                       (m H2O)
    w_g2_bd       , & ! boundary field for w_g2                       (m H2O)
    w_g3_bd       , & ! boundary field for w_g3                       (m H2O)
    hmo3_bd       , & ! boundary field for hmo3                       (m    )
    vio3_bd       , & ! boundary field for vio3                       (pa O3)
    w_cl_bd       , & ! boundary field for w_cl                       (m H2O)
    t_cl_bd       , & ! boundary field for t_cl                       (  K  )
    lai_bd        , & ! boundary field for lai                        ( --  )
    rootdp_bd     , & ! boundary field for rootdp                     (m    )
    plcov_bd      , & ! boundary field for plcov                      ( --  )
    aer_su_bd     , & ! boundary field for aer_su                     (0 - 1)
    aer_bc_bd     , & ! boundary field for aer_bc                     (0 - 1)
    aer_du_bd     , & ! boundary field for aer_du                     (0 - 1)
    aer_or_bd     , & ! boundary field for aer_or                     (0 - 1)
    aer_ss_bd     , & ! boundary field for aer_ss                     (0 - 1)

! 10. time integrated analysis increment fields
! ---------------------------------------------

    ff_anai       , & ! wind velocity                                 ( m/s )
    dd_anai       , & ! wind direction                                ( rad )
    t_anai        , & ! temperature                                   (  k  )
    p_anai        , & ! deviation from the reference pressure         ( Pa  )
    qv_anai       , & ! specific water vapor content                  (kg/kg)
    qc_anai       , & ! specific cloud water content (via saturation adjustm)
    !(WL;2010)b
    hflux         , &
    eflux         , &
    ahflux        , &
    aeflux        , & 
    !(WL;2010)e

!WL2011b
! 11. fields for budget analysis 
! ---------------------------------------------
    tt_mic        , & ! 
    tt_rad        , & ! 
    tt_con        , & ! 
    tt_turb       , & ! 
    tt_adv        , & ! 
    tt_rlx        , & ! 
    tt_tot        , & ! 
    tt_ssowl      , & !    
    qvt_mic       , & ! 
    qcit_mic      , & !
    qrsgt_mic     , & !
    qvt_adv       , & ! 
    qvt_rlx       , & ! 
    qcit_adv      , & !
    qcit_rlx      , & !
    qrsgt_adv     , & !
    qrsgt_rlx     , & !
    qvt_turb      , & ! 
    qcit_turb     , & !
    qvt_tot       , & !
    qcit_tot      , & !
    qrsgt_tot     , & !
    tt_hd         , & !
    qcit_hd       , & !
    qvt_hd        , & !
    tt_zadv       , & !
    tt_hadv       , & !
    qvt_zadv      , & !
    qcit_zadv     , & !
    qrsgt_zadv    , & !
    tt_dpdt       , & !

    att_mic        , & !
    att_rad        , & !
    att_con        , & !
    att_turb       , & !
    att_adv        , & !
    att_rlx        , & !
    att_tot        , & !
    att_ssowl      , & !
    aqvt_mic       , & !
    aqcit_mic      , & !
    aqrsgt_mic     , & !
    aqvt_adv       , & !
    aqvt_rlx       , & !
    aqvt_con       , & !
    aqcit_adv      , & !
    aqcit_rlx      , & !
    aqcit_con      , & !
    aqrsgt_adv     , & !
    aqrsgt_rlx     , & !
    aqrsgt_con     , & !
    aqvt_turb      , & !
    aqcit_turb     , & !
    aqvt_tot       , & !
    aqcit_tot      , & !
    aqrsgt_tot     , & !
    att_hd         , & !
    aqcit_hd       , & !
    aqvt_hd        , & !
    att_zadv       , & !
    att_hadv       , & !
    aqvt_zadv      , & !
    aqcit_zadv     , & !
    aqrsgt_zadv    , & !
    att_dpdt       , & !

!WL2011e
!LS, b
    tt_relax       , & !
    qvt_relax      , & !
    ut_relax       , & !
    vt_relax       , & !
    att_relax      , & !
    aqvt_relax     , & !
    aut_relax      , & !
    avt_relax      , & !
    wso_relax      , & !
    awso_relax     , &
! LS, e
! SBoeing, b
    tt_lsf         , & !
    qvt_lsf        , & !
    qcit_lsf       , & !
    ut_lsf         , & !
    vt_lsf         , & !
    att_lsf        , & !
    aqvt_lsf       , & !
    aqcit_lsf      , & !
    aut_lsf        , & !
    avt_lsf        , & !
! Sboeing, e
!------------------------------------------------------------------------------
!OCH>>
    cmp_fr_qi       , & ! cloud freezing
    cmp_nuc_qi      , & ! ice nucleation
    cmp_dep_qi      , & ! ice growth
    cmp_dep_qc      , & ! water growth
    cmp_act_qc      , & ! droplet activation
    cmp_self_qi     , & ! selfcollection of qi (aggregation to snow)
    cmp_fr_qni      , & ! cloud freezing (immersionsgefrieren)
    cmp_nuc_qni     , & ! ice nucleation
    cmp_act_qnc     , & ! droplet activation
    cmp_self_qni    , & ! self collection of qni (aggregation to snow)
    cmp_self_qns    , & ! self collection of qns
    cmp_self_qng    , & ! self collection of qng
    cmp_self_qnc    , &
    cmp_rime_qi_qc  , & ! riming on ice particles
    cmp_rime_qs_qc  , & ! riming on snow particles
    cmp_rime_qi_qr  , & ! riming on ice particles
    cmp_rime_qs_qr  , & ! riming on snow particles
    cmp_melt_qs_qr  , & ! melting of snow to rain
    cmp_melt_qi_qr  , & ! melting of ice to rain
    cmp_rime_qg_qc  , & ! riming on graupel particles
    cmp_rime_ice    , & ! cloud droplet reduction due to riming on ice
    cmp_rime_snow   , & ! cloud droplet reduction due to riming on snow
    cmp_rime_graup  , & ! cloud droplet reduction due to riming on graupel
    cmp_coll_qi_qs  , &
    cmp_conv_qi_qg  , & ! Conversion rates from ice to graupel 
    cmp_conv_qni_qng, & !Conversion rates from ice to graupel 
    cmp_conv_qs_qg  , & ! Conversion rates from snow to graupel 
    cmp_conv_qns_qng, & !Conversion rates from snow to graupel 
    cmp_conv_qc_qr,   & ! Autoconversion of cloud droplets
    cmp_conv_qnc_qnr, & ! 
    cmp_coll_qni_qns, &
    cmp_coll_qc_qr,   & ! Accretion 
    cmp_coll_qnc_qnr, &
    cmp_mult_ice    , & ! HM processes due to ice riming
    cmp_mult_snow   , & ! HM processes due to snow riming
    cmp_mult_graup  , & ! HM processes due to graupel riming
    cmp_hom_sol_qi  , & ! homogenous freezing of solution droplet
    cmp_hom_sol_qni , & !
    cmp_hom_qi      , & ! homogeneous freezing
    cmp_hom_qni     , &
    cmp_dep_qs      , & ! depositional growth of snow
    cmp_sub_qi      , & ! negative dep growth of ice 
    cmp_sub_qs      , & ! ... of snow 
    cmp_evap_qc     , &
    cmp_cond_qc     , &
    cmp_sedi_qg, &
    cmp_sedi_qng, &
    cmp_sedi_qs, &
    cmp_sedi_qns, &
    cmp_sedi_qi, &
    cmp_sedi_qni, &
    cmp_sedi_qr,  &
    cmp_sedi_qnr
!OCH<<

USE data_runcontrol , ONLY :   &
!WL2011b
    lbud, lbud_avg
!WL2011e

!------------------------------------------------------------------------------

USE environment,        ONLY: model_abort

#ifdef TWOMOM_SB
! 11. fields for 2 moment-scheme output
USE data_fields,        ONLY:                   &
    reffc_out,      & ! effective radius of cloud droplets            ( m )
    reffi_out,      & ! effective radius of ice particles             ( m )
    odepthw_so ,    & ! optical depth of cloud droplets  ! ???
    odepthi_so ,    & ! optical depth of ice particles   ! ???
    odepthw_th ,    & ! optical depth of cloud droplets  ! ???
    odepthi_th        ! optical depth of ice particles   ! ???
#endif
!==============================================================================

IMPLICIT NONE

!==============================================================================

!------------------------------------------------------------------------------
!- End of header
!------------------------------------------------------------------------------
 
CONTAINS

!==============================================================================
!+ Creates the LM variable table for Grib 1
!------------------------------------------------------------------------------

SUBROUTINE setup_vartab

!------------------------------------------------------------------------------
!
! Description:
!  The table that is created in this routine is necessary for dealing with
!  Grib I/O. For every variable it defines the name, grib table type, level
!  type, grib number, top and bottom level, grib factor, bias, data type,
!  time range indicator, rank and corresponding targets (i.e. the arrays for
!  the variables).
!
! Method:
!  Simple specifications.
!
!==============================================================================
!
! Declarations

REAL (KIND=wp),     POINTER :: dum4   (:,:,:,:)
REAL (KIND=wp),     POINTER :: dum3   (:,:,:)
REAL (KIND=wp),     POINTER :: dum2   (:,:)
INTEGER (KIND=iintegers)   :: istat
LOGICAL                    :: lcheck_alloc

!- End of header
!==============================================================================

!------------------------------------------------------------------------------
! Begin Subroutine setup_vartab
!------------------------------------------------------------------------------

lcheck_alloc = .FALSE.

IF ( .NOT. ALLOCATED( var ) ) THEN
  ! Allocate the array var
  istat = 0
  ALLOCATE(var(4,0:255,num_gribtabs), STAT=istat)
  lcheck_alloc = .TRUE.   ! after the first call to setup_vartab, the
                          ! allocation status of all variables are checked
  IF(istat /= 0) THEN
    CALL model_abort(my_cart_id, 2031, 'Error allocating var', 'setup_vartab')
  ENDIF
ENDIF

! Preset var
! Note:
!       Units must be a string that can be recognized by UNIDATA's Udunits package.
!       standard_name must fulfill netCDf CF conventions
NULLIFY (dum4, dum3, dum2)
var(:,:,:) = ar_des('          ', 0, 0, 0, 1.0_wp, 0.0_wp, 0, 0, 0, dum4, dum4, dum3, dum3, dum2, 0,           &
                    '                    ',                                                              &
                    '                                                                                ',  &
                    '                                                                                ',  &
                    ' ')

! Now set values for the LM fields
!                     name       ,levtyp,lev,lev,factor ,bias,ntri,rank, dimvert, p4  , p4_bd,    p3,  p3_bd,   p2, idef_stat,
!                                        top,bot,
!     units,          standard_name,                                    long_name,                                 land/sea
var(1,  1,1)= ar_des('PS        ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,    ps,   dum3,    dum2, 1,&
      'Pa          ','surface_air_pressure',                           'surface pressure',                          ' ')
var(2,  1,1)= ar_des('P         ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'Pa          ','air_pressure',                                   'pressure',                                  ' ')
var(1,  2,1)= ar_des('PMSL      ',   102,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'Pa          ','air_pressure_at_sea_level',                      'mean sea level pressure',                   ' ')
var(1,  3,1)= ar_des('DPSDT     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   dpsdt, 1,& 
      'Pa s-1      ','tendency_of_surface_air_pressure',               'surface pressure change',                   ' ')
var(1,  4,1)= ar_des('POT_VORTIC',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'K m2 kg-1 s-1','-'                ,      'Potential Vorticity',                                                ' ')
var(1,  6,1)= ar_des('FIS       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm2 s-2      ','surface_geopotential',                           'surface geopotential',                      ' ')
var(2,  6,1)= ar_des('FI        ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm2 s-2      ','geopotential',                                   'geopotential',                              ' ')
var(1,  8,1)= ar_des('HSURF     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   hsurf, 1,&
      'm           ','surface_altitude',                                'surface height',                           ' ')
var(2,  8,1)= ar_des('HHL       ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke1,    dum4,  dum4,   hhl,   dum3,    dum2, 1,&
      'm           ','altitude',                                        'height',                                   ' ')
var(1, 10,1)= ar_des('TO3       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'Dobson      ','equivalent_thickness_at_stp_of_atmosphere_ozone_content','vertical integrated ozone content', ' ')
var(1, 11,1)= ar_des('T_G       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  0,      dum4,  dum4,   t_g,   dum3,    dum2, 1,&
      'K           ','surface_temperature',                             'grid mean surface temperature',            ' ')
var(2, 11,1)= ar_des('T_2M      ',   105,  0,  2,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    t_2m, 1,&
      'K           ','air_temperature',                                 '2m temperature',                           ' ')
var(3, 11,1)= ar_des('T_2M_AV  ' ,   105,  0,  2,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,  t_2m_av,1,&
      'K           ','air_temperature',                                 '2m temperature',                           ' ')
var(4, 11,1)= ar_des('T         ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   4,  ke,        t,  t_bd,  dum3,   dum3,    dum2, 1,&
      'K           ','air_temperature',                                 'temperature',                              ' ')
var(1, 15,1)= ar_des('TMAX_2M   ',   105,  0,  2,   1.0_wp    , 0.0_wp,   2,   2,  -1,     dum4,  dum4,  dum3,   dum3, tmax_2m, 1,&
      'K           ','air_temperature',                                 '2m maximum temperature',                   ' ')
var(1, 16,1)= ar_des('TMIN_2M   ',   105,  0,  2,   1.0_wp    , 0.0_wp,   2,   2,  -1,     dum4,  dum4,  dum3,   dum3, tmin_2m, 1,&
      'K           ','air_temperature',                                 '2m minimum temperature',                   ' ')
var(1, 17,1)= ar_des('TD_2M     ',   105,  0,  2,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   td_2m, 1,&
      'K           ','dew_point_temperature',                           '2m dew point temperature',                 ' ')
var(2, 17,1)= ar_des('TD_2M_AV ',    105,  0,  2,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3, td_2m_av,1,&
      'K           ','dew_point_temperature',                           '2m dew point temperature',                 ' ')
var(1, 32,1)= ar_des('SP_10M    ',   105,  0, 10,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm s-1       ','wind_speed',                                      'wind speed of 10m wind',                   ' ')
var(1, 33,1)= ar_des('U_10M     ',   105,  0, 10,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   u_10m, 1,&
      'm s-1       ','grid_eastward_wind',                              'U-component of 10m wind',                  ' ')
var(2, 33,1)= ar_des('U_10M_AV  ',   105,  0, 10,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,u_10m_av, 1,&
      'm s-1       ','grid_eastward_wind',                              'U-component of 10m wind',                  ' ')
var(3, 33,1)= ar_des('U         ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   4,  ke,        u,  u_bd,  dum3,   dum3,    dum2, 1,&
      'm s-1       ','grid_eastward_wind',                              'U-component of wind',                      ' ')
var(1, 34,1)= ar_des('V_10M     ',   105,  0, 10,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   v_10m, 1,&
      'm s-1       ','grid_northward_wind',                             'V-component of 10m wind',                  ' ')
var(2, 34,1)= ar_des('V_10M_AV  ',   105,  0, 10,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,v_10m_av, 1,&
      'm s-1       ','grid_northward_wind',                             'V-component of 10m wind',                  ' ')
var(3, 34,1)= ar_des('V         ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   4,  ke,        v,  v_bd,  dum3,   dum3,    dum2, 1,&
      'm s-1       ','grid_northward_wind',                             'V-component of wind',                      ' ')
var(1, 39,1)= ar_des('OMEGA     ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'Pa s-1      ','lagrangian_tendency_of_air_pressure',             'omega',                                    ' ')
var(1, 40,1)= ar_des('W         ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   4,  ke1,       w,  w_bd,  dum3,   dum3,    dum2, 1,&
      'm s-1       ','upward_air_velocity',                             'vertical wind velocity',                   ' ')
var(1, 43,1)= ar_des('VORTIC_W  ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      '1/s         ','-'                 ,      'relative vorticity, vertical component',                          ' ')
var(1, 51,1)= ar_des('QV_S      ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,  qv_s,qv_s_bd,    dum2, 1,& 
      'kg kg-1     ','surface_specific_humidity',                       'surface specific humidity',                ' ')
var(2, 51,1)= ar_des('QV_2M     ',   105,  0,  2,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   qv_2m, 1,& 
      'kg kg-1     ','specific_humidity',                               '2m specific humidity',                     ' ')
var(1, 52,1)= ar_des('RELHUM    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '%           ','relative_humidity',                               'relative humidity',                        ' ')
var(2, 52,1)= ar_des('RELHUM_2M ',   105,  0,  2,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   rh_2m, 1,& 
      '%           ','relative_humidity',                               '2m relative humidity',                     ' ')
var(1, 54,1)= ar_des('TQV       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','atmosphere_water_vapor_content',                  'precipitable water',                       ' ')
var(1, 57,1)= ar_des('AEVAP_S   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3, aevap_s, 1,& 
      'kg m-2      ','water_evaporation_amount',                        'surface evaporation',                      ' ')
var(1, 58,1)= ar_des('TQI       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','atmosphere_cloud_ice_content',                    'vertical integrated cloud ice',            ' ')
var(1, 59,1)= ar_des('TOT_PR    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'kg m-2 s-1  ','precipitation_rate',                               'total precipitation rate',                ' ')
var(1, 61,1)= ar_des('TOT_PREC  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','precipitation_amount',                            'total precipitation amount',               ' ')
var(1, 65,1)= ar_des('W_SNOW    ',     1,  0,  0,1000.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,w_snow,w_snow_bd,  dum2, 1,& 
      'm           ','lwe_thickness_of_surface_snow_amount',            'surface snow amount',                      'l')
var(2, 65,1)= ar_des('W_SNOW_M  ',   211,  0,  0,   1.0_wp    , 0.0_wp,   0,   4, ke_snow, w_snow_mult, dum4, dum3, dum3, dum2, 1,&
      'm           ','snow_total_water_content',                'snow total water content',                         'l')
var(1, 66,1)= ar_des('H_SNOW    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,h_snow,   dum3,    dum2, 1,& 
      'm           ','surface_snow_thickness',                          'thickness of snow',                           'l')
var(2, 66,1)= ar_des('H_SNOW_M  ',   211,  0,  0,   1.0_wp    , 0.0_wp,   0,   4, ke_snow, dzh_snow_mult,dum4,dum3, dum3, dum2, 1,&
      'm           ','snow_layers_height_where_snow',                'snow layers height',                          'l')
var(1, 71,1)= ar_des('CLCT      ',     1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    clct, 1,& 
      '1           ','cloud_area_fraction',                             'total cloud cover',                        ' ')
var(1, 72,1)= ar_des('CLC_CON   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,clc_con,  dum3,    dum2, 1,& 
      '1           ','convective_cloud_area_fraction_in_atmosphere_layer', 'convective cloud area fraction',        ' ')
var(1, 73,1)= ar_des('CLCL      ',     1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    clcl, 1,& 
      '1           ','low_type_cloud_area_fraction',         'low cloud cover',                          ' ')
var(1, 74,1)= ar_des('CLCM      ',     1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    clcm, 1,& 
      '1           ','medium_type_cloud_area_fraction',         'medium cloud cover',                       ' ')
var(1, 75,1)= ar_des('CLCH      ',     1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    clch, 1,& 
      '1           ','high_type_cloud_area_fraction',         '  high cloud cover',                        ' ')
var(1, 76,1)= ar_des('TQC       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','atmosphere_cloud_liquid_water_content',           'vertical integrated cloud water',          ' ')
var(1, 78,1)= ar_des('SNOW_CON  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,snow_con, 1,& 
      'kg m-2      ','convective_snowfall_amount',                      'convective snowfall',                      ' ')
var(1, 79,1)= ar_des('SNOW_GSP  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,snow_gsp, 1,& 
      'kg m-2      ','large_scale_snowfall_amount',                     'large scale snowfall',                     ' ')
var(1, 81,1)= ar_des('FR_LAND   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, fr_land, 1,& 
      '1           ','land_area_fraction',                              'land-sea fraction',                        ' ')
var(1, 83,1)= ar_des('Z0        ',     1,  0,  0,   0.10197_wp, 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,     gz0, 1,& 
      'm           ','surface_roughness_length',                        'surface roughness length',                 ' ')
var(1, 84,1)= ar_des('ALB_RAD   ',     1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, alb_rad, 1,& 
      '1           ','surface_albedo',                                  'surface albedo',                           ' ')
var(1, 85,1)= ar_des('T_S       ',   111,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,   t_s, t_s_bd,    dum2, 1,& 
      'K           ','-',                                               'soil surface temperature',                 ' ')
var(2, 85,1)= ar_des('T_M       ',   111,  0,  9,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,   t_m, t_m_bd,    dum2, 1,& 
      'K           ','soil_temperature',                                'temperature of 1. soil layer',             'l')
var(3, 85,1)= ar_des('T_CL      ',   111,  0, 41,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,t_cl_bd,    t_cl, 1,& 
      'K           ','soil_temperature',                                'deep soil temperature',                    'l')
var(1, 86,1)= ar_des('W_G1      ',   112,  0, 10,1000.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,  w_g1,w_g1_bd,    dum2, 1,& 
      'm           ','lwe_thickness_of_moisture_content_of_soil_layer', 'water content of 1. soil layer',           'l')
var(2, 86,1)= ar_des('W_G2      ',   112, 10,100,1000.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,  w_g2,w_g2_bd,    dum2, 1,& 
      'm           ','lwe_thickness_of_moisture_content_of_soil_layer', 'water content of 2. soil layer',           'l')
var(3, 86,1)= ar_des('W_G3      ',   112, 10,100,1000.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,  w_g3,w_g3_bd,    dum2, 1,& 
      'm           ','lwe_thickness_of_moisture_content_of_soil_layer', 'water content of 3. soil layer',           'l')
var(4, 86,1)= ar_des('W_CL      ',   112,100,190,1000.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,w_cl_bd,    w_cl, 1,& 
      'm           ','lwe_thickness_of_moisture_content_of_soil_layer', 'deep soil water',                          'l')
var(1, 87,1)= ar_des('PLCOV     ',     1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,plcov_bd,  plcov, 1,& 
      '1           ','vegetation_area_fraction',                        'vegetation area fraction',                 'l')
var(1, 90,1)= ar_des('RUNOFF_S  ',   112,  0, 10,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,runoff_s, 1,& 
      'kg m-2      ','surface_runoff_amount',                           'surface runoff',                           'l')
var(2, 90,1)= ar_des('RUNOFF_G  ',   112, 10,190,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,runoff_g, 1,& 
      'kg m-2      ','subsurface_runoff_amount',                        'subsurface runoff',                        'l')
var(1, 91,1)= ar_des('FR_ICE    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 1,& 
      '1           ','sea_ice_area_fraction',                       'sea ice area fraction',                        's')
var(1, 92,1)= ar_des('H_ICE     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4, h_ice,   dum3,    dum2, 1,& 
      'm           ','sea_ice_thickness',                           'sea ice thickness',                            's')
var(1, 99,1)= ar_des('SNOW_MELT ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,snow_melt,1,&
      'kg m-2      ','surface_snow_melt_amount',                    'snow melt',                                    'l')
var(1,111,1)= ar_des('SOBS_RAD  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    sobs, 1,&
      'W m-2       ','surface_net_downward_shortwave_flux',             'surface net downward shortwave radiation', ' ')
var(2,111,1)= ar_des('ASOB_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,  asob_s, 1,& 
      'W m-2       ','surface_net_downward_shortwave_flux','averaged surface net downward shortwave radiation',' ')
var(1,112,1)= ar_des('THBS_RAD  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    thbs, 1,& 
      'W m-2       ','surface_net_downward_longwave_flux','surface net downward longwave radiation',  ' ')
var(2,112,1)= ar_des('ATHB_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,  athb_s, 1,& 
      'W m-2       ','surface_net_downward_longwave_flux','averaged surface net downward longwave radiation',  ' ')
var(1,113,1)= ar_des('SOBT_RAD  ',     8,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    sobt, 1,& 
      'W m-2       ','net_downward_shortwave_flux_in_air',  'TOA net downward shortwave radiation',     ' ')
var(2,113,1)= ar_des('ASOB_T    ',     8,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,  asob_t, 1,& 
      'W m-2       ','net_downward_shortwave_flux_in_air',  'averaged TOA net downward shortwave radiation',     ' ')
var(1,114,1)= ar_des('THBT_RAD  ',     8,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    thbt, 1,& 
      'W m-2       ','net_downward_longwave_flux_in_air',     'TOA outgoing longwave radiation',          ' ')
var(2,114,1)= ar_des('ATHB_T    ',     8,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,  athb_t, 1,& 
      'W m-2       ','net_downward_longwave_flux_in_air',     'averaged TOA outgoing longwave radiation',          ' ')
var(1,115,1)= ar_des('SOBCS_RAD ',     1,     0,     0,   1.0_wp    , 0.0_wp,   0,   2,  -1, dum4,  dum4,  dum3,   dum3,    sobcs, 1,&
      'W m-2       ','clear_sky_surface_net_downward_shortwave_flux', 'clear-sky surface net downward shortwave radiation', ' ')
var(2,115,1)= ar_des('ASOBC_S   ',     1,     0,     0,   1.0_wp    , 0.0_wp,   3,   2,  -1, dum4,  dum4,  dum3,   dum3,    asobc_s, 1,&
      'W m-2       ','clear_sky_surface_net_downward_shortwave_flux', 'averaged clear-sky surface net downward shortwave radiation', ' ')
var(1,116,1)= ar_des('THBCS_RAD ',     1,     0,     0,   1.0_wp    , 0.0_wp,   0,   2,  -1, dum4,  dum4,  dum3,   dum3,    thbcs, 1,&
      'W m-2       ','clear_sky_surface_net_downward_longwave_flux','clear-sky surface net downward longwave radiation',  ' ')
var(2,116,1)= ar_des('ATHBC_S   ',     1,     0,     0,   1.0_wp    , 0.0_wp,   3,   2,  -1, dum4,  dum4,  dum3,   dum3,    athbc_s, 1,&
      'W m-2       ','clear_sky_surface_net_downward_longwave_flux','averaged clear-sky surface net downward longwave radiation',  ' ')
var(1,117,1)= ar_des('SOBCT_RAD ',     8,     0,     0,   1.0_wp    , 0.0_wp,   0,   2,  -1, dum4,  dum4,  dum3,   dum3,    sobct, 1,&
      'W m-2       ','clear_sky_net_downward_shortwave_flux_in_air', 'TOA clear-sky net downward shortwave radiation',     ' ')
var(2,117,1)= ar_des('ASOBC_T   ',     8,     0,     0,   1.0_wp    , 0.0_wp,   3,   2,  -1, dum4,  dum4,  dum3,   dum3,    asobc_t, 1,&
      'W m-2       ','clear_sky_net_downward_shortwave_flux_in_air', 'averaged TOA clear-sky net downward shortwave radiation',     ' ')
var(1,118,1)= ar_des('THBCT_RAD ',     8,     0,     0,   1.0_wp    , 0.0_wp,   0,   2,  -1, dum4,  dum4,  dum3,   dum3,    thbct, 1,&
      'W m-2       ','clear_sky_net_downward_longwave_flux_in_air', 'TOA clear-sky outgoing longwave radiation',          ' ')
var(2,118,1)= ar_des('ATHBC_T   ',     8,     0,     0,   1.0_wp    , 0.0_wp,   3,   2,  -1, dum4,  dum4,  dum3,   dum3,    athbc_t, 1,&
      'W m-2       ','clear_sky_net_downward_longwave_flux_in_air', 'averaged TOA clear-sky outgoing longwave radiation',          ' ')
var(1,121,1)= ar_des('LHFL_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,  lhfl_s, 1,&
      'W m-2       ','surface_downward_latent_heat_flux',  'surface latent heat flux',               ' ')
var(2,121,1)= ar_des('ALHFL_S   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3, alhfl_s, 1,&
      'W m-2       ','surface_downward_latent_heat_flux',  'averaged surface latent heat flux',               ' ')
var(1,122,1)= ar_des('SHFL_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,  shfl_s, 1,&
      'W m-2       ','surface_downward_sensible_heat_flux','surface sensible heat flux',             ' ')
var(2,122,1)= ar_des('ASHFL_S   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3, ashfl_s, 1,&
      'W m-2       ','surface_downward_sensible_heat_flux','averaged surface sensible heat flux',             ' ')
var(1,124,1)= ar_des('UMFL_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,  umfl_s, 1,&
      'Pa          ','surface_downward_eastward_stress',   'eastward stress',                        ' ')
var(2,124,1)= ar_des('AUMFL_S   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3, aumfl_s, 1,&
      'Pa          ','surface_downward_eastward_stress',   'averaged eastward stress',                        ' ')
var(1,125,1)= ar_des('VMFL_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,  vmfl_s, 1,&
      'Pa          ','surface_downward_northward_stress',  'northward stress',                       ' ')
var(2,125,1)= ar_des('AVMFL_S   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3, avmfl_s, 1,&
      'Pa          ','surface_downward_northward_stress',  'averaged northward stress',                       ' ')
var(1,241,4)= ar_des('HFLUX     ',   109,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, -1, dum4  ,dum4,hflux    ,  dum3,    dum2,1,&
      'W m-2     ','sensible heat flux            ',              'sensible heat flux',                 ' ')
var(1,242,4)= ar_des('EFLUX     ',   109,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  -1, dum4  ,dum4,eflux    ,  dum3,    dum2,1,&
      'W m-2     ','latent heat flux            ',              'latent heat flux',                 ' ')
var(1,243,4)= ar_des('AHFLUX     ',   109,     0,     0,   1.0_wp    , 0.0_wp,   3,   3,  -1, dum4  ,dum4,ahflux    ,  dum3,    dum2,1,&
      'W m-2     ','sensible heat flux            ',              'averaged sensible heat flux',                 ' ')
var(1,244,4)= ar_des('AEFLUX     ',   109,     0,     0,   1.0_wp    , 0.0_wp,   3,   3,  -1, dum4  ,dum4,aeflux    ,  dum3,    dum2,1,&
      'W m-2     ','latent heat flux            ',              'averaged latent heat flux',                 ' ')
var(1,130,1)= ar_des('DEF13        ',   110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  -1, dum4,  dum4,  def13,   dum3,    dum2, 0,& 
      'm2/s          ','deformation 13',                                   'def 13',                                  ' ')
var(1,131,1)= ar_des('DEF23        ',   110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  -1, dum4,  dum4,  def23,   dum3,    dum2, 0,& 
      'm2/s          ','deformation 23',                                   'def 23',                                  ' ')
var(1,132,1)= ar_des('DEF12        ',   110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  -1, dum4,  dum4,  def12,   dum3,    dum2, 0,&
      'm2/s          ','deformation 12',                                   'def 12',                                  ' ')
var(1,133,1)= ar_des('DEF11        ',   110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  -1, dum4,  dum4,  def11,   dum3,    dum2, 0,&
      'm2/s          ','deformation 11',                                   'def 11',                                  ' ')
var(1,134,1)= ar_des('DEF22        ',   110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  -1, dum4,  dum4,  def22,   dum3,    dum2, 0,&
      'm2/s          ','deformation 22',                                   'def 22',                                  ' ')
var(1,135,1)= ar_des('DEF33        ',   110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  -1, dum4,  dum4,  def33,   dum3,    dum2, 0,&
      'm2/s          ','deformation 33',                                   'def 33',                                  ' ')

!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------

!                     name       ,levtyp,lev,lev,factor ,bias,ntri,rank, dimvert, p4  , p4_bd,    p3,  p3_bd,   p2, idef_stat,
!                                        top,bot,

var(1,  5,2)= ar_des('PABS_RAD  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    pabs, 1,& 
      'W m-2       ','surface_downwelling_photosynthetic_radiative_flux_in_air',                                          &
                                                                 'surface photosynthetic active radiation', ' ')
var(2,  5,2)= ar_des('APAB_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,  apab_s, 1,& 
      'W m-2       ','surface_downwelling_photosynthetic_radiative_flux_in_air',                                          &
                                                                'averaged surface photosynthetic active radiation', ' ')
var(1, 13,2)= ar_des('SOHR_RAD  ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,  sohr,   dum3,    dum2, 1,& 
      'K s-1       ','tendency_of_air_temperature_due_to_shortwave_heating',          &
                                                                   'solar radiation heating rate in the atmosphere',' ')
var(1, 14,2)= ar_des('THHR_RAD  ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,  thhr,   dum3,    dum2, 1,& 
      'K s-1       ','tendency_of_air_temperature_due_to_longwave_heating',                                               &
                                                                   'thermal radiation heating rate in the atmosphere', ' ')
var(1, 18,2)= ar_des('ALHFL_BS  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,   0,     dum4,  dum4,  dum3,   dum3,alhfl_bs, 1,&
      'W m-2       ','-',                                       'averaged latent heat flux from bare soil evaporation',' ')
var(1, 19,2)= ar_des('ALHFL_PL  ',   111,  0,  0,   1.0_wp    , 0.0_wp,   3,   3, ke_soil, dum4,  dum4,alhfl_pl, dum3,    dum2, 1,&
      'W m-2       ','-',                          'averaged latent heat flux from plants',                            ' ')
var(1, 20,2)= ar_des('DURSUN   ',      1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,  dursun, 1,& 
      's           ','duration_of_sunshine',                       'duration of sunshine',                             ' ')
var(1, 21,2)= ar_des('RSTOM    ',      1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   rstom, 1,&
       's/m         ','-',                                           'stomata resistance',                             ' ')
var(1, 22,2)= ar_des('SWDIR_S   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,  swdir_s,1,&
      'W m-2       ','-',                                   'direct downward sw radiation at the surface',            ' ')
var(2, 22,2)= ar_des('ASWDIR_S  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3, aswdir_s,1,&
      'W m-2       ','-',                                   'averaged direct downward sw radiation at the surface',   ' ')
var(1, 23,2)= ar_des('SWDIFD_S  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, swdifd_s,1,&
      'W m-2       ','-',                                   'diffuse downward sw radiation at the surface',           ' ')
var(2, 23,2)= ar_des('ASWDIFD_S ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,aswdifd_s,1,&
      'W m-2       ','-',                                   'averaged diffuse downward sw radiation at the surface',  ' ')
var(1, 24,2)= ar_des('SWDIFU_S  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, swdifu_s,1,&
      'W m-2       ','-',                                   'diffuse upwnward sw radiation at the surface',           ' ')
var(2, 24,2)= ar_des('ASWDIFU_S ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3,aswdifu_s,1,&
      'W m-2       ','-',                                   'averaged diffuse upwnward sw radiation at the surface',  ' ')
var(1, 25,2)= ar_des('LWD_S     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,  lwd_s , 1,&
      'W m-2       ','-',                                   'downward lw radiation at the surface',                   ' ')
var(2, 25,2)= ar_des('ALWD_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3, alwd_s , 1,&
      'W m-2       ','-',                                   'averaged downward lw radiation at the surface',          ' ')
var(1, 26,2)= ar_des('LWU_S     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,  lwu_s , 1,&
      'W m-2       ','-',                                   'upward lw radiation at the surface',                     ' ')
var(2, 26,2)= ar_des('ALWU_S    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,   dum3, alwu_s , 1,&
      'W m-2       ','-',                                   'averaged upward lw radiation at the surface',            ' ')
var(1, 27,2)= ar_des('SOD_T     ',     8,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,    dum3, dum3,   sod_t, 1,&
      'W m-2       ','-',                           'solar downward radiation at top',                              ' ')
var(2, 27,2)= ar_des('ASOD_T    ',     8,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,    dum3, dum3,  asod_t, 1,&
      'W m-2       ','-',                           'averaged solar downward radiation at top',                     ' ')
var(1, 29,2)= ar_des('CLC       ',   110,  0,  0, 100.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '1           ','cloud_area_fraction_in_atmosphere_layer',          'cloud area fraction',                     ' ')
var(1, 30,2)= ar_des('CLC_SGS   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,clc_sgs,  dum3,    dum2, 1,& 
      '1           ','stratiform_cloud_area_fraction_in_atmosphere_layer',  'grid scale cloud area fraction',       ' ')
var(1, 37,2)= ar_des('TQR       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','atmosphere_rain_content',                    'total rain water content vertically integrated', ' ')
var(1, 38,2)= ar_des('TQS       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','atmosphere_snow_content',                          'total snow content vertically integrated', ' ')
var(1, 40,2)= ar_des('TQG       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','atmosphere_graupel_content',                     'total graupel content vertically integrated', ' ')
var(1, 41,2)= ar_des('TWATER    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','atmosphere_water_content ',                    'total water content',                           ' ')
var(1, 42,2)= ar_des('TDIV_HUM  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,tdiv_hum, 1,& 
      'kg m-2      ','change_over_time_in_atmospheric_water_content_due_to_advection', 'atmosphere water divergence',' ')
var(1, 43,2)= ar_des('QC_RAD    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,qc_rad,   dum3,    dum2, 1,& 
      'kg kg-1     ','mass_fraction_of_cloud_liquid_water_in_air','sub scale specific cloud liquid water content', ' ')
var(1, 44,2)= ar_des('QI_RAD    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,qi_rad,   dum3,    dum2, 1,& 
      'kg kg-1     ','mass_fraction_of_cloud_ice_in_air',      'sub scale specific cloud ice content',    ' ')
var(1, 58,2)= ar_des('HBAS_SC   ',     2,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','convective_cloud_base_altitude',             'height of shallow convective cloud base',         ' ')
var(1, 59,2)= ar_des('HTOP_SC   ',     3,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','convective_cloud_top_altitude',              'height of shallow convective cloud top',          ' ')
var(1, 61,2)= ar_des('CLW_CON   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,clw_con,  dum3,    dum2, 1,&
      '1           ','mass_fraction_of_convective_cloud_liquid_water_in_air','convective cloud liquid water',         ' ')
var(1, 68,2)= ar_des('HBAS_CON  ',     2,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','convective_cloud_base_altitude',                   'height of convective cloud base',         ' ')
var(1, 69,2)= ar_des('HTOP_CON  ',     3,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','convective_cloud_top_altitude',                    'height of convective cloud top',          ' ')
var(1, 72,2)= ar_des('BAS_CON   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, bas_con, 1,&
      '1           ','model_level_number_at_convective_cloud_base',      'index of convective cloud base',          ' ')
var(1, 73,2)= ar_des('TOP_CON   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, top_con, 1,&
      '1           ','model_level_number_at_convective_cloud_top',       'index of convective cloud top',           ' ')
var(1, 74,2)= ar_des('DT_CON    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,tt_conv,  dum3,    dum2, 1,&
      'K s-1       ','tendency_of_air_temperature_due_to_moist_convection','convective tendency of temperature',      ' ')
var(1, 75,2)= ar_des('DQV_CON   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,qvt_conv, dum3,    dum2, 1,&
      's-1         ','tendency_of_specific_humidity_due_to_convection',    'convective tendency of specific humidity',' ')
var(1, 78,2)= ar_des('DU_CON    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,ut_conv,  dum3,    dum2, 1,&
      'm s-2       ','tendency_of_eastward_wind_due_to_convection',        'convective tendency of u-wind component', ' ')
var(1, 79,2)= ar_des('DV_CON    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,vt_conv,  dum3,    dum2, 1,&
      'm s-2       ','tendency_of_northward_wind_due_to_convection',       'convective tendency of v-wind component', ' ')
var(1, 82,2)= ar_des('HTOP_DC   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','altitude_at_top_of_dry_convection',                'height of dry convection top',            ' ')
var(1, 84,2)= ar_des('HZEROCL   ',     4,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','freezing_level_altitude',                          'height of freezing level',                ' ')
var(1, 85,2)= ar_des('SNOWLMT   ',     4,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','altitude',                 'height of the snow fall limit in m above sea level', ' ')
var(1, 88,2)= ar_des('DQC_CON   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,qct_conv, dum3,    dum2, 1,&
      's-1         ','tendency_of_specific_cloud_water_due_to_convection','convective tendency of specific cloud water',' ')
var(1, 89,2)= ar_des('DQI_CON   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,qit_conv, dum3,    dum2, 1,&
      's-1         ','tendency_of_specific_cloud_ice_due_to_convection','convective tendency of specific cloud ice',' ')
var(1, 91,2)= ar_des('C_T_LK    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,c_t_lk,   dum3,    dum2, 1,&
      '1           ','-',              'shape factor of temperature profile in lake thermocline',                   'i')
var(1, 92,2)= ar_des('GAMSO_LK  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,gamso_lk, 1,&
      'm-1         ','-',              'attenuation coefficient for solar radiation in lake water',                 'i')
var(1, 93,2)= ar_des('DP_BS_LK  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,dp_bs_lk, 1,&
      'm           ','-',              'thickness of thermally active layer of bottom sediments',                   'i')
var(1, 94,2)= ar_des('H_B1_LK   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,h_b1_lk,  dum3,    dum2, 1,&
      'm           ','-',              'thickness of the upper layer of bottom sediments',                          'i')
var(1, 95,2)= ar_des('H_ML_LK   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,h_ml_lk,  dum3,    dum2, 1,&
      'm           ','-',              'thickness of mixed layer',                                                  'i')
var(1, 96,2)= ar_des('DEPTH_LK  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,depth_lk, 1,&
      'm           ','sea_floor_depth_below_sea_level',                  'lake depth',                              'i')
var(1, 97,2)= ar_des('FETCH_LK  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,fetch_lk, 1,&
      'm           ','-',                                                'wind fetch over lake',                    'i')
var(1, 99,2)= ar_des('QRS       ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,   qrs,   dum3,    dum2, 1,&
      '1           ','mass_fraction_of_precipitation_in_air',            'precipitation water (water loading)',     ' ')
var(2, 99,2)= ar_des('Q_SEDIM   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 1,&
      '1           ','mass_fraction_of_precipitation_in_air',            'precipitation water (water loading)',     ' ')
var(1,100,2)= ar_des('PRR_GSP   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, prr_gsp, 1,&
      'kg m-2 s-1  ','large_scale_rainfall_flux',                      'mass flux density of large scale rainfall', ' ')
var(1,101,2)= ar_des('PRS_GSP   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, prs_gsp, 1,&
      'kg m-2 s-1  ','large_scale_snowfall_flux',                      'mass flux density of large scale snowfall', ' ')
var(1,102,2)= ar_des('RAIN_GSP  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,rain_gsp, 1,&
      'kg m-2      ','large_scale_rainfall_amount',                      'large scale rainfall',                    ' ')
var(1,111,2)= ar_des('PRR_CON   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, prr_con, 1,&
      'kg m-2 s-1  ','convective_rainfall_flux',                        'mass flux density of convective rainfall', ' ')
var(1,112,2)= ar_des('PRS_CON   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, prs_con, 1,&
      'kg m-2 s-1  ','convective_snowfall_flux',                        'mass flux density of convective snowfall', ' ')
var(1,113,2)= ar_des('RAIN_CON  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,rain_con, 1,&
      'kg m-2      ','convective_rainfall_amount',                       'convective rainfall',                     ' ')
var(1,129,2)= ar_des('FRESHSNW  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,freshsnow,1,&
      '1           ','-',                                              'freshness of snow',                         'l')
var(1,131,2)= ar_des('PRG_GSP   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, prg_gsp, 1,&
      'kg m-2 s-1  ','large_scale_graupel_flux',                       'mass flux density of large scale graupel',  ' ')
var(1,132,2)= ar_des('GRAU_GSP  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,grau_gsp, 1,&
      'kg m-2      ','large_scale_graupel_amount',                       'large scale graupel',                     ' ')
var(1,133,2)= ar_des('RHO_SNOW  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,rho_snow, dum3,    dum2, 1,&
      'kg m-3      ','surface_snow_density',                             'density of snow',                         ' ')
var(2,133,2)= ar_des('RHO_SNOW_M',   211,  0,  0,   1.0_wp    , 0.0_wp,   0,   4, ke_snow, rho_snow_mult,dum4,dum3,dum3,  dum2, 1,&
      'kg m-3      ','surface_snow_density',                             'density of snow',                         'l')
var(1,134,2)= ar_des('PRH_GSP   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, prh_gsp, 1,&
      'kg m-2 s-1  ','large_scale_hail_flux',                       'mass flux density of large scale hail',   ' ')
var(1,135,2)= ar_des('HAIL_GSP  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3,hail_gsp, 1,&
      'kg m-2      ','large_scale_hail_amount',                       'large scale hail',                     ' ')
var(1,136,2)= ar_des('TQH       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','atmosphere_hail_content',                         'total hail content vertically integrated', ' ')
var(1,137,2)= ar_des('WLIQ_SNOW ',   211,  0,  0,   1.0_wp    , 0.0_wp,   0,   4, ke_snow, wliq_snow,  dum4, dum3, dum3,  dum2, 1,&
      'm           ','snow_liquid_water_content',              'snow liquid water content',                         'l')
var(1,139,2)= ar_des('PP        ',   110,  0,  0,   0.01_wp   , 0.0_wp,   0,   4,  ke,       pp, pp_bd,  dum3,   dum3,    dum2, 1,&
      'Pa          ','difference_of_air_pressure_from_model_reference',  'deviation from reference pressure',       ' ')
var(1,140,2)= ar_des('RCLD      ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke1,    dum4,  dum4,  rcld,   dum3,    dum2, 1,&
      '-           ','-',                                'standard deviation of saturation deficit',                ' ')
var(1,141,2)= ar_des('SDI_1     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      '1/s         ','-'                 ,      'Supercell detection index 1 (rotating up-/downdrafts',             ' ')
var(1,142,2)= ar_des('SDI_2     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      '1/s         ','-'                 ,      'Supercell detection index 2 (rotating updrafts, only',             ' ')
var(1,143,2)= ar_des('CAPE_MU   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 1,&
      'J kg-1      ','-',                                                'cape of most unstable parcel',            ' ')
var(1,144,2)= ar_des('CIN_MU    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 1,&
      'J kg-1      ','-',                               'convective inhibition of most unstable parcel',            ' ')
var(1,145,2)= ar_des('CAPE_ML   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 1,&
      'J kg-1      ','-',                                           'cape of mean surface layer parcel',            ' ')
var(1,146,2)= ar_des('CIN_ML    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 1,&
      'J kg-1      ','-',                          'convective inhibition of mean surface layer parcel',            ' ')
var(1,147,2)= ar_des('TKE_CON   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, tke_con, 1,& 
      'J kg-1      ','-',              'convective turbulent kinetic energy',     ' ')
var(1,148,2)= ar_des('TKETENS   ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke1,    dum4,  dum4, tketens, dum3,    dum2, 1,&
      'm s-1       ','-',                                                'tendency of turbulent kinetic energy',    ' ')
var(1,151,2)= ar_des('EDR       ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke1,    dum4,  dum4,  edr,    dum3,    dum2, 1,&
      'm2 s-3      ','-'                             ,      'dissipation rate of turbulent kinetic energy',         ' ')
var(1,152,2)= ar_des('TKE       ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   4,  ke1,     tke,  dum4,  dum3,   dum3,    dum2, 1,&
      'm2 s-2      ','specific_kinetic_energy_of_air',     'turbulent kinetic energy',                              ' ')
var(1,153,2)= ar_des('TKVM      ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke1,    dum4,  dum4,  tkvm,   dum3,    dum2, 1,&
      'm2 s-1      ','atmosphere_momentum_diffusivity',                'diffusion coefficient of momentum',        ' ')
var(1,154,2)= ar_des('TKVH      ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke1,    dum4,  dum4,  tkvh,   dum3,    dum2, 1,&
      'm2 s-1      ','atmosphere_heat_diffusivity',                     'diffusion coefficient of heat',           ' ')
var(1,155,2)= ar_des('DTKE_SSO  ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3, ke1,     dum4,  dum4,tket_sso, dum3,    dum2, 1,&
      'm2 s-3      ','-',                                              'TKE tendency due to SSO wakes',           ' ')
var(1,156,2)= ar_des('DTKE_HSH  ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3, ke1,     dum4,  dum4,tket_hshr,dum3,    dum2, 1,&
      'm2 s-3      ','-',                                              'TKE tendency due to horizontal shear',    ' ')
var(1,157,2)= ar_des('DTKE_CON  ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3, ke1,     dum4,  dum4,tket_conv,dum3,    dum2, 1,&
      'm2 s-3      ','-',                                              'TKE tendency due to convective buoyancy', ' ')
!(WL;2010)b
IF (l3dturb) THEN 
var(1,158,2)= ar_des('TKHM      ',   110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  ke1, dum4,  dum4,  tkhm,   dum3,    dum2, 1,&
      'm2 s-1      ','atmosphere_momentum_diffusivity',                'hor. diffusion coefficient of momentum',        ' ')
var(1,159,2)= ar_des('TKHH      ',   110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3,  ke1, dum4,  dum4,  tkhh,   dum3,    dum2, 1,&
      'm2 s-1      ','atmosphere_heat_diffusivity',                     'hor. diffusion coefficient of heat',           ' ')
END IF
!(WL;2010)e
var(1,170,2)= ar_des('TCM       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,     tcm, 1,&
      '1           ','surface_drag_coefficient_for_momentum_in_air ',  'drag coefficient of momentum',             ' ')
var(1,171,2)= ar_des('TCH       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,     tch, 1,&
      '1           ','surface_drag_coefficient_for_heat_in_air',      'drag coefficient of heat',                  ' ')
var(1,187,2)= ar_des('VMAX_10M  ',   105,  0, 10,   1.0_wp    , 0.0_wp,   2,   2,  -1,     dum4,  dum4,  dum3,   dum3,vmax_10m, 1,&
      'm s-1       ','wind_speed_of_gust',                               'maximum 10m wind speed',                  ' ')
var(1,190,2)= ar_des('T_BS_LK   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, t_bs_lk, 1,&
      'K           ','-',            'climatological temperature at bottom of thermally active layer of sediments', 'i')
var(1,191,2)= ar_des('T_BOT_LK  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,t_bot_lk, dum3,    dum2, 1,&
      'K           ','-',            'temperature at water bottom sediment interface',                              'i')
var(1,192,2)= ar_des('T_B1_LK   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,t_b1_lk,  dum3,    dum2, 1,&
      'K           ','-',            'temperature at bottom of upper layer of sediments',                           'i')
var(1,193,2)= ar_des('T_WML_LK  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,t_wml_lk, dum3,    dum2, 1,&
      'K           ','-',                          'mixed layer temperature',                 'i')
var(1,194,2)= ar_des('T_MNW_LK  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,t_mnw_lk, dum3,    dum2, 1,&
      'K           ','-',                 'mean temperature of water column',        'i')
var(1,197,2)= ar_des('T_SO      ',   111,  0,  0,   1.0_wp    , 0.0_wp,   0,   4, ke_soil+1, t_so,  dum4,  dum3,   dum3,  dum2, 1,&
      'K           ','soil_temperature',                                 'soil temperature',                        'l')
var(1,198,2)= ar_des('W_SO      ',   111,  0,  0,1000.0_wp    , 0.0_wp,   0,   4, ke_soil+1, w_so,  dum4,  dum3,   dum3,  dum2, 1,&
      'm           ','lwe_thickness_of_moisture_content_of_soil_layer',  'soil water content',                      'l')
var(1,199,2)= ar_des('W_SO_ICE  ',   111,  0,  0,1000.0_wp    , 0.0_wp,   0,   4, ke_soil+1,w_so_ice,dum4, dum3,   dum3,  dum2, 1,&
      'm           ','lwe_thickness_of_frozen_water_content_of_soil_layer', 'soil frozen water content',            'l')
var(1,200,2)= ar_des('W_I       ',     1,  0,  0,1000.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,   w_i,   dum3,    dum2, 1,&
      'm           ','lwe_thickness_of_canopy_water_amount',             'canopy water amount',                     'l')
! LS, b
var(1,201,2)= ar_des('S_SO      ',   111,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, 0,  dum4,  dum4,  s_so,   dum3,    dum2, 1,&
      '1           ','soil_moisture_saturation',  'soil moisture saturation',                      'l')
! LS, e
var(1,203,2)= ar_des('T_SNOW    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4,t_snow,t_snow_bd,  dum2, 1,& 
      'K           ','surface_temperature_where_snow',                'snow surface temperature',                   'l')
var(2,203,2)= ar_des('T_SNOW_M  ',   211,  0,  0,   1.0_wp    , 0.0_wp,   0,   4, ke_snow,t_snow_mult, dum4, dum3, dum3,  dum2, 1,&
      'K           ','snow_temperature_where_snow',                'snow temperature',                              'l')
var(1,212,2)= ar_des('RSMIN     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,rsmin2d,  1,&
      's m-1       ','-',                                                'minimum stomata resistance',           'l')
var(1,215,2)= ar_des('T_ICE     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,   0,     dum4,  dum4, t_ice,   dum3,    dum2, 1,& 
      'K           ','-',                                               'temperature of ice upper surface',         's')
var(1,216,2)= ar_des('VABSMX_10M',   105,  0, 10,   1.0_wp    , 0.0_wp,   2,   2,  -1,     dum4,  dum4,  dum3,  dum3,vabsmx_10m,1,&
      'm s-1       ','wind_speed',                            'maximum 10m wind speed without gust',                ' ')
var(1,218,2)= ar_des('VGUST_DYN ',   105,  0, 10,   1.0_wp    , 0.0_wp,   2,   2,  -1,     dum4,  dum4,  dum3,   dum3,vgust_dyn,1,&
      'm s-1       ','wind_speed_of_gust',                            'maximum 10m dynamical gust',              ' ')
var(1,219,2)= ar_des('VGUST_CON ',   105,  0, 10,   1.0_wp    , 0.0_wp,   2,   2,  -1,     dum4,  dum4,  dum3,   dum3,vgust_con,1,&
      'm s-1       ','wind_speed_of_gust',                            'maximum 10m convective gust',             ' ')
var(1,230,2)= ar_des('DBZ       ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '1           ','-',                         'unattenuated radar reflectivity in Rayleigh approximation',      'l')
var(2,230,2)= ar_des('DBZ_850   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '1           ','-',              'unattenuated radar reflectivity in Rayleigh approximation in 850 hPa',      'l')
var(3,230,2)= ar_des('DBZ_CMAX  ',   200,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '1           ','-',         'unattenuated radar reflectivity in Rayleigh approximation: column maximum',      'l')
var(2,236,2)= ar_des('EVATRA_SUM',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','-',              'sum of contributions to evapotranspiration',                                'l')
var(2,237,2)= ar_des('TRA_SUM   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','-',              'total transpiration from all soil layers',                                  'l')
var(2,238,2)= ar_des('TOTFORCE_S',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'W  m-2      ','-',              'total forcing at soil surface',                                             'l')
var(2,239,2)= ar_des('RESID_WSO ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg m-2      ','-',              'residuum of soil moisture',                                                 'l')
var(1,240,2)= ar_des('MFLX_CON  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,mflx_con, 1,& 
      'kg m-2 s-1  ','atmosphere_convective_mass_fluxx',                             'convective mass flux density', ' ')
var(1,241,2)= ar_des('CAPE_CON  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,cape_con, 1,& 
      'J kg-1      ','atmosphere_specific_convective_available_potential_energy ',                                        &
                                                                 'specific convectively available potential energy', ' ')
var(1,243,2)= ar_des('QCVG_CON  ',     1,  0,  0,1000.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,qcvg_con, 1,& 
      's-1         ','-',                                    'moisture convergence in the air for kuo type closure', ' ')

!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------

!                     name       ,levtyp,lev,lev,factor ,bias,ntri,rank, dimvert, p4  , p4_bd,    p3,  p3_bd,   p2, idef_stat,
!                                        top,bot,

var(1, 32,3)= ar_des('TINC_LH   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, tinc_lh, dum3,    dum2, 1,&
      'K           ','-',                           'temperature increment due to latent heat',                     ' ')
var(1, 43,3)= ar_des('DT_SSO    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, tt_sso,  dum3,    dum2, 1,&
      'K s-1       ','-'                            , 'tendency of t due to SSO',                                   ' ')  
var(1, 44,3)= ar_des('DU_SSO    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, ut_sso,  dum3,    dum2, 1,&
      'm s-2       ','-'                            , 'tendency of u due to SSO',                                   ' ')
var(1, 45,3)= ar_des('DV_SSO    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, vt_sso,  dum3,    dum2, 1,&
      'm s-2       ','-'                            , 'tendency of v due to SSO',                                   ' ')
var(1, 46,3)= ar_des('SSO_STDH  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,sso_stdh, 1,& 
      'm           ','-',                                         'standard deviation of sub-grid scale orography', 'l')
var(1, 47,3)= ar_des('SSO_GAMMA ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,sso_gamma,1,& 
      '-           ','-',                                                 'anisotropy of sub-grid scale orography', 'l')
var(1, 48,3)= ar_des('SSO_THETA ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,sso_theta,1,& 
      'rad         ','-',                                     'angle between principal axis of orography and east', 'l')
var(1, 49,3)= ar_des('SSO_SIGMA ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,sso_sigma,1,& 
      '-           ','-',                                                 'mean slope of sub-grid scale orography', 'l')
var(1, 55,3)= ar_des('FR_LAKE   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, fr_lake, 1,& 
      '1           ','-',                                                'fraction of inland lake water',           ' ')
var(1, 56,3)= ar_des('EMIS_RAD  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,emis_rad, 1,&
      '-           ','-',                                                'thermal surface emissivity',           'l')
var(1, 57,3)= ar_des('SOILTYP   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, soiltyp, 1,& 
      '1           ','soil_type',                                        'soil type',                               ' ')
var(1, 61,3)= ar_des('LAI       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3, lai_bd,     lai, 1,& 
      '1           ','leaf_area_index',                                  'leaf area index',                         'l')
var(1, 62,3)= ar_des('ROOTDP    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,rootdp_bd,rootdp, 1,& 
      'm           ','root_depth',                                       'root depth',                              'l')
var(1, 64,3)= ar_des('HMO3      ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,hmo3_bd,    hmo3, 1,& 
      'Pa          ','air_pressure',                    'air pressure at ozone maximum',           ' ')
var(1, 65,3)= ar_des('VIO3      ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,vio3_bd,    vio3, 1,& 
      'Pa          ','equivalent_pressure_of_atmosphere_ozone_content',  'vertical integrated ozone amount',        ' ')
var(1, 67,3)= ar_des('PLCOV_MX  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '1           ','vegetation_area_fraction',                        'vegetation area fraction maximum',         'l')
var(1, 68,3)= ar_des('PLCOV_MN  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '1           ','vegetation_area_fraction',                        'vegetation area fraction minimum',         'l')
var(1, 69,3)= ar_des('LAI_MX    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '1           ','leaf_area_index',                                  'leaf area index maximum',                 'l')
var(1, 70,3)= ar_des('LAI_MN    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '1           ','leaf_area_index',                                  'leaf area index minimum',                 'l')
var(1, 75,3)= ar_des('FOR_E     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   for_e, 0,& 
      '-           ','-',  'ground fraction covered by evergreen forest',                                           'l')
var(1, 76,3)= ar_des('FOR_D     ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,   for_d, 0,& 
      '-           ','-',  'ground fraction covered by deciduous forest',                                           'l')
var(1, 84,3)= ar_des('AER_SO4   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4, dum4, dum3, aer_su_bd, aer_su, 1,&
      '1          ','-',                                   'aerosol sulfat',                                        ' ')
var(1, 86,3)= ar_des('AER_DUST   ',    1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4, dum4, dum3, aer_du_bd, aer_du, 1,&
      '1          ','-',                                   'aerosol dust',                                          ' ')
var(1, 91,3)= ar_des('AER_ORG   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4, dum4, dum3, aer_or_bd, aer_or, 1,&
      '1          ','-',                                   'aerosol organic',                                       ' ')
var(1, 92,3)= ar_des('AER_BC   ',      1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4, dum4, dum3, aer_bc_bd, aer_bc, 1,&
      '1          ','-',                                   'aerosol black carbon',                                  ' ')
var(1, 93,3)= ar_des('AER_SS   ',      1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4, dum4, dum3, aer_ss_bd, aer_ss, 1,&
      '1          ','-',                                   'aerosol sea salt',                                      ' ')
var(1, 96,3)= ar_des('HORIZON   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  nhori,  dum4,  dum4,  horizon,dum3,   dum2 , 1,&
      '           ','-',                                   'horizon angle - topography',                              ' ')
var(1, 97,3)= ar_des('SWDIR_COR ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,swdir_cor,1,&
      '1           ','-',                                   'topo correction of direct solar radiation',            ' ')
var(1, 98,3)= ar_des('SLO_ANG   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, slo_ang, 1,&
      '           ','-',                                   'slope angle - topography',                              ' ')
var(1, 99,3)= ar_des('SLO_ASP   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, slo_asp, 1,&
      '           ','-',                                   'slope aspect - topography',                             ' ')
var(1,100,3)= ar_des('SKYVIEW   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, skyview, 1,&
      '1           ','-',                                   'sky-view factor',                                      ' ')
var(1,104,3)= ar_des('DQVDT     ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, dqvdt,   dum3,    dum2, 1,& 
      's-1         ','tendency_of_specific_humidity',                    'tendency of water vapor',                 ' ')
var(1,105,3)= ar_des('QVSFLX    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,  qvsflx, 1,& 
      's-1m-2      ','-',                                                'surface flux of water vapour',            ' ')
var(1,113,3)= ar_des('FC        ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,      fc, 1,& 
      's-1         ','coriolis_parameter',                               'coriolis parameter',                      ' ')
var(1,114,3)= ar_des('RLAT      ',     1,  0,  0,  57.296_wp  , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    rlat, 1,& 
      'radian      ','latitude',                                         'latitude',                                ' ')
var(1,115,3)= ar_des('RLON      ',     1,  0,  0,  57.296_wp  , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    rlon, 1,& 
      'radian      ','longitude',                                        'longitude',                               ' ')
var(1,121,3)= ar_des('ZTD       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '            ','-',                                               'integrated total atmospheric refractivity',' ')
var(1,122,3)= ar_des('ZWD       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '            ','-',                                                'integrated wet atmospheric refractivity', ' ')
var(1,123,3)= ar_des('ZHD       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      '            ','-',                                                'integrated dry atmospheric refractivity', ' ')
var(1,127,3)= ar_des('ALB_DRY   ',     1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, alb_dry, 1,&
      '1           ','-',                                              'dry soil albedo',                           'l')
var(1,128,3)= ar_des('ALB_SAT   ',     1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, alb_sat, 1,&
      '1           ','-',                                        'saturated soil albedo',                           'l')
var(1,129,3)= ar_des('ALB_DIF    ',    1,  0,  0, 100.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3, alb_dif, 1,&
      '1           ','-',                                       'diffuse solar albedo',                             'l')
var(1,133,3)= ar_des('VORTIC_U  ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      '1/s         ','-'                 ,      'relative vorticity, u-component in rotated grid',                 ' ')
var(1,134,3)= ar_des('VORTIC_V  ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      '1/s         ','-'                 ,      'relative vorticity, v-component in rotated grid',                 ' ')
#ifndef MESSY
var(1,180,3)= ar_des('O3        ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,& 
      'kg kg-1     ','mass_fraction_of_ozone_in_air',                    'ozone mass mixing ratio',                 ' ')
#endif
var(1,231,3)= ar_des('USTR_SSO  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,  dum3,ustr_sso, 1,&
      'N m-2       ','-'                            , '           u-stress due to SSO',                             'l')
var(2,231,3)= ar_des('AUSTRSSO  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,  dum3,austr_sso, 1,& 
      'N m-2       ','-'                            , 'average of u-stress due to SSO',                             'l')
var(1,232,3)= ar_des('VSTR_SSO  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,  dum3,vstr_sso, 1,&
      'N m-2       ','-'                            , '           v-stress due to SSO',                             'l')
var(2,232,3)= ar_des('AVSTRSSO  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,  dum3,avstr_sso, 1,& 
      'N m-2       ','-'                            , 'average of v-stress due to SSO',                             'l')
var(1,233,3)= ar_des('VDIS_SSO  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,  dum3,vdis_sso, 1,&
      'W m-2       ','-'             , '           vertical integrated dissipation of kinetic energy due to SSO',   'l')
var(2,233,3)= ar_des('AVDISSSO  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   3,   2,  -1,     dum4,  dum4,  dum3,  dum3,avdis_sso, 1,& 
      'W m-2       ','-'             , 'average of vertical integrated dissipation of kinetic energy due to SSO',   'l')

!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------

!                     name       ,levtyp,lev,lev,factor ,bias,ntri,rank, dimvert, p4  , p4_bd,    p3,  p3_bd,   p2, idef_stat,
!                                        top,bot,

var(1, 50,4)= ar_des('TKET_ADV  ',   109,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke1,    dum4,  dum4,tket_adv, dum3,    dum2, 1,&
      'm s-1       ','-',                                 'pure advective tendency of turbulent kinetic energy',    ' ')
var(1,135,4)= ar_des('LCL_ML    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','-',                           'lifting condensation level of mean layer parcel',              ' ')
var(1,136,4)= ar_des('LFC_ML    ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','-',                           'level of free convection of mean layer parcel',                ' ')
var(1,137,4)= ar_des('CAPE_3KM  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'J kg-1      ','-',                           'CAPE of mean layer parcel in the lowest 3 km',                 ' ')
var(1,138,4)= ar_des('SWISS00   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'K           ','-',                           'showalter index with windshear',                               ' ')
var(1,139,4)= ar_des('SWISS12   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'K           ','-',                           'surface lifted index with windshear',                          ' ')
var(1,147,4)= ar_des('SLI       ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'K           ','-',                           'surface lifted index',                                         ' ')
var(1,149,4)= ar_des('SI        ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'K           ','-',                           'showalter index',                                              ' ')
var(1,155,4)= ar_des('BRN       ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      '1           ','-'         ,                  'Bulk Richardson Number',                                       ' ')
var(1,156,4)= ar_des('HPBL      ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','-'         ,                  'Height of boundary layer',                                     ' ')
var(1,157,4)= ar_des('CEILING   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      'm           ','-'         ,                  'Cloud ceiling height (above MSL)',                             ' ')
var(1,203,4)= ar_des('CLDEPTH   ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      '1           ','-',                           'normalized cloud depth',                                       ' ')
var(1,204,4)= ar_des('CLCT_MOD  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,  dum3,   dum3,    dum2, 0,&
      '1           ','-',                     'modified_total_cloud_cover',                                         ' ')

!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------

var(1, 21,5)= ar_des('FF_ANAI   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, ff_anai, dum3,    dum2, 1,& 
      'm s-1       ','-',                           'wind speed analysis increment',                                ' ')
var(1, 22,5)= ar_des('DD_ANAI   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, dd_anai, dum3,    dum2, 1,& 
      'degree      ','-',                           'wind direction analysis increment',                            ' ')
var(1, 23,5)= ar_des('T_ANAI    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,  t_anai, dum3,    dum2, 1,& 
      'K           ','-',                           'temperature analysis increment',                               ' ')
var(1, 24,5)= ar_des('FI_ANAI   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,    dum3, dum3,    dum2, 0,& 
      'm2 s-2       ','-',                          'geopotential analysis increment',                              ' ')
var(1, 25,5)= ar_des('P_ANAI    ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4,  p_anai, dum3,    dum2, 1,& 
      'Pa          ','-',                           'pressure analysis increment',                                  ' ')
var(1, 26,5)= ar_des('PMSL_ANAI ',   102,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,    dum3, dum3,    dum2, 0,& 
      'Pa          ','-',                           'msl pressure analysis increment',                              ' ')
var(1, 27,5)= ar_des('QV_ANAI   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, qv_anai, dum3,    dum2, 1,& 
      'kg kg-1     ','-',                           'specific humidity analysis increment',                         ' ')
var(1, 28,5)= ar_des('QC_ANAI   ',   110,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  ke,     dum4,  dum4, qc_anai, dum3,    dum2, 1,& 
      'kg kg-1     ','-',                           'specific cloud water content analysis increment',              ' ')
var(1, 29,5)= ar_des('TQV_ANAI  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,    dum3, dum3,    dum2, 0,& 
      'kg m-2      ','-',                           'precipitable water analysis increment',                        ' ')
var(1, 30,5)= ar_des('TQC_ANAI  ',     1,  0,  0,   1.0_wp    , 0.0_wp,   0,   2,  -1,     dum4,  dum4,    dum3, dum3,    dum2, 0,& 
      'kg m-2      ','-',                           'cloud water analysis increment',                               ' ')

!$$TS  effective radii for 2MOM
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------

#ifdef TWOMOM_SB
var(1,103,2)= ar_des('TAU_I_SO',     110,  0,  0,   1.0    , 0.0,   0, 3,    ke,     dum4,  dum4,  odepthi_so,   dum3, dum2, 1,&
      '1     ','optical_depth_cloud_ice',                    'optical depth of cloud ice',            ' ')
var(1,104,2)= ar_des('TAU_I_TH',     110,  0,  0,   1.0    , 0.0,   0, 3,    ke,     dum4,  dum4,  odepthi_th,   dum3, dum2, 1,&
      '1     ','optical_depth_cloud_ice',                    'optical depth of cloud ice',            ' ')
var(1,105,2)= ar_des('TAU_C_SO',     110,  0,  0,   1.0    , 0.0,   0, 3,    ke,     dum4,  dum4,  odepthw_so,   dum3, dum2, 1,&
      '1     ','optical_depth_cloud_water',                    'optical depth of cloud water',            ' ')
var(1,106,2)= ar_des('TAU_C_TH',     110,  0,  0,   1.0    , 0.0,   0, 3,    ke,     dum4,  dum4,  odepthw_th,   dum3, dum2, 1,&
      '1     ','optical_depth_cloud_water',                    'optical depth of cloud water',            ' ')
var(1,252,2)= ar_des('REFF_C    ',   110,  0,  0,   1.0    , 0.0,   0, 3,    ke,     dum4,  dum4, reffc_out,   dum3,    dum2, 1,&
      'm     ','reff_cloud     ',     'effective radius of cloud droplets',                                             ' ')
var(1,253,2)= ar_des('REFF_I    ',   110,  0,  0,   1.0    , 0.0,   0, 3,    ke,     dum4,  dum4, reffi_out,   dum3,    dum2, 1,&
      'm     ','reff_ice       ',                'effective radius of cloud ice',              ' ')
#endif


!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------

var(1,  1,6)= ar_des('SYNME5    ',   222,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,   dum3,  dum3,    dum2, 1,& 
      '-           ','-',             'synthetic satellite images Meteosat5',                                       ' ')
var(1,  2,6)= ar_des('SYNME6    ',   222,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,   dum3,  dum3,    dum2, 1,& 
      '-           ','-',             'synthetic satellite images Meteosat6',                                       ' ')
var(1,  3,6)= ar_des('SYNME7    ',   222,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,   dum3,  dum3,    dum2, 1,& 
      '-           ','-',             'synthetic satellite images Meteosat7',                                       ' ')
var(1,  4,6)= ar_des('SYNMSG    ',   222,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4,   dum3,  dum3,    dum2, 1,& 
      '-           ','-',                   'synthetic satellite images MSG',                                       ' ')
var(1,  5,6)= ar_des('MSG_TB    ',   222,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4, dum3, dum3,     dum2, 0,&
      'K           ','-',             'MSG brightness temperature',                                                 ' ')
var(1,  6,6)= ar_des('MSG_TBC  ' ,   222,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4, dum3, dum3,     dum2, 0,&
      'K           ','-',             'MSG clear-sky brightness temperature',                                       ' ')
var(1,  7,6)= ar_des('MSG_RAD   ',   222,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4, dum3, dum3,     dum2, 0,&
      'W sr-1 m-2  ','-',             'MSG radiance',                                                               ' ')
var(1,  8,6)= ar_des('MSG_RADC  ',   222,  0,  0,   1.0_wp    , 0.0_wp,   0,   3,  -1,     dum4,  dum4, dum3, dum3,     dum2, 0,&
      'W sr-1 m-2  ','-',             'MSG clear-sky radiance',                                                     ' ')

!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------

! MeteoSwiss local grib table (version 250)
! the multi-layer snow model fields are in this table

var(1, 25,11)= ar_des('DURSUN_M ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3, dursun_m, 1,&
      's           ','duration_of_sunshine',                       'maximum duration of sunshine',                     ' ')
var(1, 26,11)= ar_des('DURSUN_R ',     1,  0,  0,   1.0_wp    , 0.0_wp,   4,   2,  -1,     dum4,  dum4,  dum3,   dum3, dursun_r, 1,&
      's           ','duration_of_sunshine',                       'relative duration of sunshine',                    ' ')

#ifdef TEND
! Variables for tendency-sum output (dmaurer)
var(1, 31,11)= ar_des('TTEND_RAD',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_rad,dum3,dum2, 1,&
      'K s-1       ','t-tendency-radiation',                       't tendency radiation',                             ' ')
var(1, 32,11)= ar_des('TTEND_SSO',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_sso,dum3,dum2, 1,&
      'K s-1       ','t-tendency-sso',                             't tendency sso',                                   ' ')
var(1, 33,11)= ar_des('TTEND_TUR',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_tur,dum3,dum2, 1,&
      'K s-1       ','t-tendency-turbulence',                      't tendency turbulence',                            ' ')
var(1, 34,11)= ar_des('TTEND_TMP1',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_tmp1,dum3,dum2, 1,&
      'K s-1       ','t-tendency-turbulence-tmp1',                 't tendency turbulence tmp1',                       ' ')
var(1, 35,11)= ar_des('TTEND_TMP2',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_tmp2,dum3,dum2, 1,&
      'K s-1       ','t-tendency-turbulence-tmp2',                 't tendency turbulence tmp2',                       ' ')
var(1, 36,11)= ar_des('TTEND_CON',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_con,dum3,dum2, 1,&
      'K s-1       ','t-tendency-shallow-convection',              't tendency shallow convection',                    ' ')
var(1, 37,11)= ar_des('TTEND_HYD',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_hyd,dum3,dum2, 1,&
      'K s-1       ','t-tendency-hydci-pp-gr',                     't tendency hydci pp gr',                           ' ')
var(1, 38,11)= ar_des('TTEND_TOT',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_tot,dum3,dum2, 1,&
      'K s-1       ','total-t-tendency',                           'total t tendency',                                 ' ')
var(1, 39,11)= ar_des('QVTEN_TUR',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,qvten_tur,dum3,dum2, 1,&
      'kg kg-1 s-1 ','qv-tendency-turbulence',                     'qv tendency turbulence',                           ' ')
var(1, 40,11)= ar_des('QVTEN_CON',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,qvten_con,dum3,dum2, 1,&
      'kg kg-1 s-1 ','qv-tendency-shallow-convection',             'qv tendency shallow convection',                   ' ')
var(1, 41,11)= ar_des('QVTEN_HYD',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,qvten_hyd,dum3,dum2, 1,&
      'kg kg-1 s-1 ','qv-tendency-hydci-pp-gr',                    'qv tendency hydci pp gr',                          ' ')
var(1, 42,11)= ar_des('QVTEN_TOT',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,qvten_tot,dum3,dum2, 1,&
      'kg kg-1 s-1 ','total-qv-tendency',                          'total qv tendency',                                ' ')
var(1, 43,11)= ar_des('UTEND_SSO',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,utend_sso,dum3,dum2, 1,&
      'm s-2       ','u-tendency-sso',                             'u tendency sso',                                   ' ')
var(1, 44,11)= ar_des('UTEND_TUR',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,utend_tur,dum3,dum2, 1,&
      'm s-2       ','u-tendency-turbulence',                      'u tendency turbulence',                            ' ')
var(1, 45,11)= ar_des('UTEND_TMP1',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,utend_tmp1,dum3,dum2, 1,&
      'm s-2       ','u-tendency-turbulence-tmp1',                 'u tendency turbulence tmp1',                       ' ')
var(1, 46,11)= ar_des('UTEND_TMP2',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,utend_tmp2,dum3,dum2, 1,&
      'm s-2       ','u-tendency-turbulence-tmp2',                 'u tendency turbulence tmp2',                       ' ')
var(1, 47,11)= ar_des('UTEND_TOT',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,utend_tot,dum3,dum2, 1,&
      'm s-2       ','total-u-tendency',                           'total u tendency',                                 ' ')
var(1, 48,11)= ar_des('VTEND_SSO',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,vtend_sso,dum3,dum2, 1,&
      'm s-2       ','v-tendency-sso',                             'v tendency sso',                                   ' ')
var(1, 49,11)= ar_des('VTEND_TUR',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,vtend_tur,dum3,dum2, 1,&
      'm s-2       ','v-tendency-turbulence',                      'v tendency turbulence',                            ' ')
var(1, 50,11)= ar_des('VTEND_TMP1',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,vtend_tmp1,dum3,dum2, 1,&
      'm s-2       ','v-tendency-turbulence-tmp1',                 'v tendency turbulence tmp1',                       ' ')
var(1, 51,11)= ar_des('VTEND_TMP2',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,vtend_tmp2,dum3,dum2, 1,&
      'm s-2       ','v-tendency-turbulence-tmp2',                 'v tendency turbulence tmp2',                       ' ')
var(1, 86,11)= ar_des('VTEND_TOT',   110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,vtend_tot,dum3,dum2, 1,&
      'm s-2       ','total-v-tendency',                           'total v tendency',                                 ' ')
var(1, 87,11)= ar_des('TTEND_TMP3',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_tmp3,dum3,dum2, 1,&
      'K s-1       ','tmp3-t-tendency',                           'tmp3 t tendency',                                 ' ')
var(1, 88,11)= ar_des('UTEND_TMP3',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,utend_tmp3,dum3,dum2, 1,&
      'm s-2       ','tmp3-u-tendency',                           'tmp3 u tendency',                                 ' ')
var(1, 89,11)= ar_des('VTEND_TMP3',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,vtend_tmp3,dum3,dum2, 1,&
      'm s-2       ','tmp3-v-tendency',                           'tmp3 v tendency',                                 ' ')
var(1, 90,11)= ar_des('QVTEN_TMP3',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,qvten_tmp3,dum3,dum2, 1,&
      'kg kg-1 s-1 ','tmp3-qv-tendency',                          'tmp3 qv tendency',                                 ' ')
var(1, 91,11)= ar_des('TTEND_TMP4',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_tmp4,dum3,dum2, 1,&
      'K s-1       ','tmp4-t-tendency',                           'tmp4 t tendency',                                 ' ')
var(1, 92,11)= ar_des('UTEND_TMP4',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,utend_tmp4,dum3,dum2, 1,&
      'm s-2       ','tmp4-u-tendency',                           'tmp4 u tendency',                                 ' ')
var(1, 93,11)= ar_des('VTEND_TMP4',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,vtend_tmp4,dum3,dum2, 1,&
      'm s-2       ','tmp4-v-tendency',                           'tmp4 v tendency',                                 ' ')
var(1, 94,11)= ar_des('QVTEN_TMP4',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,qvten_tmp4,dum3,dum2, 1,&
      'kg kg-1 s-1 ','tmp4-qv-tendency',                          'tmp4 qv tendency',                                 ' ')
var(1, 95,11)= ar_des('TTEND_TUR2',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,ttend_tur2,dum3,dum2, 1,&
      'K s-1       ','tur2-t-tendency',                           'tur2 t tendency',                                 ' ')
var(1, 96,11)= ar_des('UTEND_TUR2',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,utend_tur2,dum3,dum2, 1,&
      'm s-2       ','tur2-u-tendency',                           'tur2 u tendency',                                 ' ')
var(1, 97,11)= ar_des('VTEND_TUR2',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,vtend_tur2,dum3,dum2, 1,&
      'm s-2       ','tur2-v-tendency',                           'tur2 v tendency',                                 ' ')
var(1, 98,11)= ar_des('QVTEN_TUR2',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,qvten_tur2,dum3,dum2, 1,&
      'kg kg-1 s-1 ','tur2-qv-tendency',                          'tur2 qv tendency',                                 ' ')
var(1, 99,11)= ar_des('COUNTREF',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,countref,dum3,dum2, 1,&
      'n       ','countref',                           'counter reference',                                 ' ')
var(1,100,11)= ar_des('COUNTER',  110,  0,  0,   1.0_wp    , 0.0_wp,   4,   3,  ke, dum4,dum4,counter,dum3,dum2, 1,&
      'n ','counter',                          'counter',                                 ' ')
#endif

!WL2011b
IF (lbud) THEN
! Now set values for the budget fields
!                     name       ,levtyp,lev,lev,factor ,bias,ntri,rank, dimvert, p4  , p4_bd,    p3,  p3_bd,   p2, idef_stat,
!                                        top,bot,
!     units,          standard_name,                                    long_name,                                 land/sea
var(1,  1,12)= ar_des('TT_MIC      ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,      tt_mic,   dum3,    dum2, 1,&
      'K s-1'  ,  'microphy. temp. tend.',                           'temperature tendency due to microphysics',                          ' ')
var(1,  2,12)= ar_des('TT_RAD      ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,      tt_rad,   dum3,    dum2, 1,&
      'K s-1'  ,  'radiation temp. tend.',                           'temperature tendency due to radiation',                          ' ')
var(1,  3,12)= ar_des('TT_TURB     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     tt_turb,   dum3,    dum2, 1,&
      'K s-1'  ,  'turbulence temp. tend.',                           'temperature tendency due to turbulence',                          ' ')
var(1,  4,12)= ar_des('TT_ADV      ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,      tt_adv,   dum3,    dum2, 1,&
      'K s-1'  ,  '3D advection temp. tend.',                           'temperature tendency due to 3D advection',                          ' ')
var(1,  5,12)= ar_des('TT_HD       ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,       tt_hd,   dum3,    dum2, 1,&
      'K s-1'  ,  'comp. diff. temp. tend.',                           'temperature tendency due to computational diffusion',                          ' ')
var(1,  6,12)= ar_des('TT_TOT      ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,      tt_tot,   dum3,    dum2, 1,&
      'K s-1'  ,  'total temp. tend.',                           'total temperature tendency',                          ' ')
var(1,  7,12)= ar_des('QVT_MIC     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     qvt_mic,   dum3,    dum2, 1,&
      's-1  '  ,  'microphy. qv-tend.',                           'qv-tendency from microphysics',                          ' ')
var(1,  8,12)= ar_des('QCIT_MIC    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,    qcit_mic,   dum3,    dum2, 1,&
      's-1  '  ,  'microphy. qc,qi-tend.',                           'qc,qi-tendency from microphysics',                          ' ')
var(1,  9,12)= ar_des('QRSGT_MIC   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   qrsgt_mic,   dum3,    dum2, 1,&
      's-1  '  ,  'microphy. qr,qs,qg-tend.',                           'qr,qs,qg-tendency from microphysics',                          ' ')
var(1,  10,12)= ar_des('QVT_ADV    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     qvt_adv,   dum3,    dum2, 1,&      
      's-1  '  ,  'advection qv-tend.',                           'qv-tendency from advection',                          ' ')
var(1,  11,12)= ar_des('QCIT_ADV   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,    qcit_adv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qc,qi-tend.',                           'qc,qi-tendency from advection',                          ' ')
var(1,  12,12)= ar_des('QRSGT_ADV  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   qrsgt_adv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qr,qs,qg-tend.',                           'qr,qs,qg-tendency from advection',                          ' ')
var(1,  13,12)= ar_des('QVT_TURB   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,    qvt_turb,   dum3,    dum2, 1,& 
      's-1  '  ,  'turbulence qv-tend.',                           'qv-tendency from turbulence',                          ' ')
var(1,  14,12)= ar_des('QCIT_TURB  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   qcit_turb,   dum3,    dum2, 1,&
      's-1  '  ,  'turbulence qc,qi-tend.',                           'qc,qi-tendency from turbulence',                          ' ')
var(1,  15,12)= ar_des('QVT_TOT    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     qvt_tot,   dum3,    dum2, 1,& 
      's-1  '  ,  'difference qv-tend.',                           'qv-tendency difference',                          ' ')
var(1,  16,12)= ar_des('QCIT_TOT   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,    qcit_tot,   dum3,    dum2, 1,&
      's-1  '  ,  'difference qc,qi-tend.',                           'qc,qi-tendency difference',                          ' ') 
var(1,  17,12)= ar_des('QRSGT_TOT  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   qrsgt_tot,   dum3,    dum2, 1,&
      's-1  '  ,  'difference qr,qs,qg-tend.',                           'qr,qs,qg-tendency difference',                          ' ')
var(1,  18,12)= ar_des('QVT_HD     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,      qvt_hd,   dum3,    dum2, 1,&  
      'K s-1'  ,  'hor. diff. qv-tend.',                           'comput. horizontal diffusion qv-tendency',                         ' ')
var(1,  19,12)= ar_des('QCIT_HD    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     qcit_hd,   dum3,    dum2, 1,&       
      'K s-1'  ,  'hor. diff. qc,qi-tend.',                           'comput. horizontal diffusion qc,qi-tendency',                          ' ')
var(1,  20,12)= ar_des('QVT_ZADV   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,    qvt_zadv,   dum3,    dum2, 1,&            
      's-1  '  ,  'advection qv-tend.',                           'qv-tendency from advection',                          ' ')
var(1,  21,12)= ar_des('QCIT_ZADV  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   qcit_zadv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qc,qi-tend.',                           'qc,qi-tendency from advection',                          ' ')
var(1,  22,12)= ar_des('QRSGT_ZADV ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,  qrsgt_zadv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qr,qs,qg-tend.',                           'average qr,qs,qg-tendency from advection',                          ' ')
var(1,  23,12)= ar_des('TT_ZADV    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     tt_zadv,   dum3,    dum2, 1,&
      's-1  '   ,  'advection t-tend.',                           't-tendency from advection',                          ' ')
var(1,  24,12)= ar_des('TT_DPDT    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     tt_dpdt,   dum3,    dum2, 1,&
      'K s-1'  ,  'total temp. tend.',                           'total temperature tendency',                          ' ')
var(1,  25,12)= ar_des('TT_CON     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,      tt_con,   dum3,    dum2, 1,&
      'K s-1'  ,  'convection temp. tend.',                           'temperature tendency due to convection',                          ' ')
var(1,  26,12)= ar_des('TT_RLX     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,      tt_rlx,   dum3,    dum2, 1,&
      'K s-1'  ,  'relaxation t-tend.',                           't-tendency due to relaxation',                          ' ')
var(1,  27,12)= ar_des('QVT_RLX    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     qvt_rlx,   dum3,    dum2, 1,&
      'g g-1'  ,  'relaxation qv-tend.',                           'qv-tendency due to relaxation',                          ' ')
var(1,  28,12)= ar_des('QCIT_RLX   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,    qcit_rlx,   dum3,    dum2, 1,&
      'g g-1'  ,  'relaxation qci-tend.',                           'qci-tendency due to relaxation',                          ' ')
var(1,  29,12)= ar_des('QRSGT_RLX  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   qrsgt_rlx,   dum3,    dum2, 1,&
      'g g-1'  ,  'relaxation qrsg-tend.',                           'qrsg-tendency due to relaxation',                          ' ')
var(1,  30,12)= ar_des('TT_SSOWL   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,      tt_ssowl,   dum3,    dum2, 1,&
      'K s-1'  ,  'sso t-tend.',                           't-tendency due to sso',                          ' ')
var(1,  31,12)= ar_des('TT_HADV    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,     tt_hadv,   dum3,    dum2, 1,&
      's-1  '   ,  'hori advection t-tend.',                           't-tendency from hori advection',                          ' ')
! Linda
var(1, 32,12)= ar_des('TT_RELAX ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   tt_relax,   dum3,    dum2, 1,&
      'K s-1'  ,  'relaxation temp. tend.',                           'temperature tendency due to relaxation',                          ' ')
var(1, 33,12) = ar_des('QVT_RELAX       ',    110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,qvt_relax   ,   dum3,    dum2, 1,&
      's-1       ','relaxation qv-tend.         ', 'qv-tendency due to relaxation',      ' ')
var(1, 34,12)= ar_des('UT_RELAX ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   ut_relax,   dum3,    dum2, 1,&
      'm s-2'  ,  'relaxation u. tend.',                           'u tendency due to relaxation',                          ' ')
var(1, 35,12)= ar_des('VT_RELAX ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   vt_relax,   dum3,    dum2, 1,&
      'm s-2'  ,  'relaxation v. tend.',                           'v tendency due to relaxation',                          ' ')
var(1,36,12)= ar_des('WSO_RELAX ',   111,     0,     0,1.0_wp    , 0.0_wp,   0,   3, 0, dum4,  dum4,  wso_relax,   dum3,    dum2, 1,&
      '           ','relaxation_of_moisture_content_of_soil_layer',  'relaxation of soil water content',                      'l')
! SBoeing, b
var(1, 37,12)= ar_des('TT_LSF ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   tt_lsf,   dum3,    dum2, 1,&
      'K s-1'  ,  'lsf temp. tend.',                           'temperature tendency due to lsf',                          ' ')
var(1, 38,12) = ar_des('QVT_LSF ',    110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,qvt_lsf   ,   dum3,    dum2, 1,&
      's-1       ','lsf qv-tend.         ', 'qv-tendency due to lsf',      ' ')
var(1, 39,12) = ar_des('QCIT_LSF ',    110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,qcit_lsf   ,   dum3,    dum2, 1,&
      's-1       ','lsf qci-tend.         ', 'qci-tendency due to lsf',      ' ')
var(1, 40,12)= ar_des('UT_LSF ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   ut_lsf,   dum3,    dum2, 1,&
      'm s-2'  ,  'lsf u tend.',                           'u tendency due to lsf',                          ' ')
var(1, 41,12)= ar_des('VT_LSF ',     110,     0,     0,   1.0_wp    , 0.0_wp,   0,   3, ke, dum4,  dum4,   vt_lsf,   dum3,    dum2, 1,&
      'm s-2'  ,  'lsf v tend.',                           'v tendency due to lsf',                          ' ')
! SBoeing, e


IF (lbud_avg) THEN
  var(1,  1,13)= ar_des('ATT_MIC      ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,      att_mic,   dum3,    dum2, 1,&
      'K s-1'  ,  'microphy. temp. tend.',                           'temperature tendency due to microphysics',                          ' ')
  var(1,  2,13)= ar_des('ATT_RAD      ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,      att_rad,   dum3,    dum2, 1,&
      'K s-1'  ,  'radiation temp. tend.',                           'temperature tendency due to radiation',                          ' ')
  var(1,  3,13)= ar_des('ATT_TURB     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     att_turb,   dum3,    dum2, 1,&
      'K s-1'  ,  'turbulence temp. tend.',                           'temperature tendency due to turbulence',                          ' ')
  var(1,  4,13)= ar_des('ATT_ADV      ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,      att_adv,   dum3,    dum2, 1,&
      'K s-1'  ,  '3D advection temp. tend.',                           'temperature tendency due to 3D advection',                          ' ')
  var(1,  5,13)= ar_des('ATT_HD       ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,       att_hd,   dum3,    dum2, 1,&
      'K s-1'  ,  'comp. diff. temp. tend.',                           'temperature tendency due to computational diffusion',                          ' ')
  var(1,  6,13)= ar_des('ATT_TOT      ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,      att_tot,   dum3,    dum2, 1,&
      'K s-1'  ,  'total temp. tend.',                           'total temperature tendency',                          ' ')
  var(1,  7,13)= ar_des('AQVT_MIC     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     aqvt_mic,   dum3,    dum2, 1,&
      's-1  '  ,  'microphy. qv-tend.',                           'qv-tendency from microphysics',                          ' ')
  var(1,  8,13)= ar_des('AQCIT_MIC    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    aqcit_mic,   dum3,    dum2, 1,&
      's-1  '  ,  'microphy. qc,qi-tend.',                           'qc,qi-tendency from microphysics',                          ' ')
  var(1,  9,13)= ar_des('AQRSGT_MIC   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqrsgt_mic,   dum3,    dum2, 1,&
      's-1  '  ,  'microphy. qr,qs,qg-tend.',                           'qr,qs,qg-tendency from microphysics',                          ' ')
  var(1,  10,13)= ar_des('AQVT_ADV    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     aqvt_adv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qv-tend.',                           'qv-tendency from advection',                          ' ')
  var(1,  11,13)= ar_des('AQCIT_ADV   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    aqcit_adv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qc,qi-tend.',                           'qc,qi-tendency from advection',                          ' ')
  var(1,  12,13)= ar_des('AQRSGT_ADV  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqrsgt_adv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qr,qs,qg-tend.',                           'qr,qs,qg-tendency from advection',                          ' ')
  var(1,  13,13)= ar_des('AQVT_TURB   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    aqvt_turb,   dum3,    dum2, 1,&
      's-1  '  ,  'turbulence qv-tend.',                           'qv-tendency from turbulence',                          ' ')
  var(1,  14,13)= ar_des('AQCIT_TURB  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqcit_turb,   dum3,    dum2, 1,&
      's-1  '  ,  'turbulence qc,qi-tend.',                           'qc,qi-tendency from turbulence',                          ' ')
  var(1,  15,13)= ar_des('AQVT_TOT    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     aqvt_tot,   dum3,    dum2, 1,&
      's-1  '  ,  'difference qv-tend.',                           'qv-tendency difference',                          ' ')
  var(1,  16,13)= ar_des('AQCIT_TOT   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    aqcit_tot,   dum3,    dum2, 1,&
      's-1  '  ,  'difference qc,qi-tend.',                           'qc,qi-tendency difference',                          ' ')
  var(1,  17,13)= ar_des('AQRSGT_TOT  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqrsgt_tot,   dum3,    dum2, 1,&
      's-1  '  ,  'difference qr,qs,qg-tend.',                           'qr,qs,qg-tendency difference',                          ' ')
  var(1,  18,13)= ar_des('AQVT_HD     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,      aqvt_hd,   dum3,    dum2, 1,&
      'K s-1'  ,  'hor. diff. qv-tend.',                           'comput. horizontal diffusion qv-tendency',                         ' ')
  var(1,  19,13)= ar_des('AQCIT_HD    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     aqcit_hd,   dum3,    dum2, 1,&
      'K s-1'  ,  'hor. diff. qc,qi-tend.',                           'comput. horizontal diffusion qc,qi-tendency',                          ' ')
  var(1,  20,13)= ar_des('AQVT_ZADV   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    aqvt_zadv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qv-tend.',                           'average qv-tendency from advection',                          ' ')
  var(1,  21,13)= ar_des('AQCIT_ZADV  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqcit_zadv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qc,qi-tend.',                           'average qc,qi-tendency from advection',                          ' ')
  var(1,  22,13)= ar_des('AQRSGT_ZADV ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,  aqrsgt_zadv,   dum3,    dum2, 1,&
      's-1  '  ,  'advection qr,qs,qg-tend.',                           'average qr,qs,qg-tendency from advection',                          ' ')
  var(1,  23,13)= ar_des('ATT_ZADV    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     att_zadv,   dum3,    dum2, 1,&
      's-1  '   ,  'advection theta-tend.',                           'average theta-tendency from advection',                          ' ')
  var(1,  24,13)= ar_des('ATT_DPDT    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     att_dpdt,   dum3,    dum2, 1,&
      'K s-1'  ,  'total temp. tend.',                           'total temperature tendency',                          ' ')
  var(1,  25,13)= ar_des('ATT_CON     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,      att_con,   dum3,    dum2, 1,&
      'K s-1'  ,  'convection temp. tend.',                           'temperature tendency due to convection',                          ' ')
  var(1,  26,13)= ar_des('ATT_RLX     ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,      att_rlx,   dum3,    dum2, 1,&
      'K s-1'  ,  'relaxation t-tend.',                           't-tendency due to relaxation',                          ' ')
  var(1,  27,13)= ar_des('AQVT_RLX    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     aqvt_rlx,   dum3,    dum2, 1,&
      'g g-1'  ,  'relaxation qv-tend.',                           'qv-tendency due to relaxation',                          ' ')
  var(1,  28,13)= ar_des('AQCIT_RLX   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    aqcit_rlx,   dum3,    dum2, 1,&
      'g g-1'  ,  'relaxation qci-tend.',                           'qci-tendency due to relaxation',                          ' ')
  var(1,  29,13)= ar_des('AQRSGT_RLX  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqrsgt_rlx,   dum3,    dum2, 1,&
      'g g-1'  ,  'relaxation qrsg-tend.',                           'qrsg-tendency due to relaxation',                          ' ')
  var(1,  30,13)= ar_des('ATT_SSOWL   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    att_ssowl,   dum3,    dum2, 1,&
      'K s-1'  ,  'sso t-tend.',                           't-tendency due to sso',                          ' ')
  var(1,  31,13)= ar_des('AQVT_CON    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     aqvt_con,   dum3,    dum2, 1,&
      'K s-1'  ,  'convection temp. tend.',                           'temperature tendency due to convection',                          ' ')
  var(1,  32,13)= ar_des('AQCIT_CON   ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    aqcit_con,   dum3,    dum2, 1,&
      'K s-1'  ,  'convection temp. tend.',                           'temperature tendency due to convection',                          ' ')
  var(1,  33,13)= ar_des('AQRSGT_CON  ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,    aqrsgt_con,   dum3,    dum2, 1,&
      'K s-1'  ,  'convection temp. tend.',                           'temperature tendency due to convection',                          ' ')
  var(1,  34,13)= ar_des('ATT_HADV    ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,     att_hadv,   dum3,    dum2, 1,&
      's-1  '   ,  'hori advection tend.',                           'average tendency from hori advection',                          ' ')
! Linda
 var(1,  35,13)= ar_des('ATT_RELAX ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3,  ke,dum4,  dum4,   att_relax,   dum3,    dum2, 1,&
       'K s-1'  ,  'relaxation temp. tend.',                           'average temperature tendency due to relaxation',                          ' ')
 var(1,  36,13)= ar_des('AQVT_RELAX ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqvt_relax,   dum3,    dum2, 1,&
       's-1'  ,  'relaxation qv-tend.',                           'average qv-tendency due to relaxation',                          ' ')
 var(1,  37,13)= ar_des('AUT_RELAX ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aut_relax,   dum3,    dum2, 1,&
       'K s-1'  ,  'relaxation temp. tend.',                           'average temperature tendency due to relaxation',                          ' ')
 var(1,  38,13)= ar_des('AVT_RELAX ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   avt_relax,   dum3,    dum2, 1,&
       'K s-1'  ,  'relaxation temp. tend.',                           'average temperature tendency due to relaxation',                          ' ')
 var(1,  39,13)= ar_des('AWSO_RELAX      ',   111,     0,     0,1.0_wp    , 0.0_wp,   3,   3, 0, dum4,  dum4,  awso_relax,   dum3,    dum2, 1,&
      '           ','avg relaxation_of_moisture_content_of_soil_layer',  'avg relaxation of soil water content',                      'l')
! Sboeing, b 
 var(1,  40,13)= ar_des('ATT_LSF ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3,  ke,dum4,  dum4,   att_lsf,   dum3,    dum2, 1,&
       'K s-1'  ,  'lsf temp. tend.',                           'average temperature tendency due to lsf',                          ' ')
 var(1,  41,13)= ar_des('AQVT_LSF ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqvt_lsf,   dum3,    dum2, 1,&
       's-1'  ,  'lsf qv-tend.',                           'average qv-tendency due to lsf',                          ' ')
 var(1,  42,13)= ar_des('AQCIT_LSF ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aqcit_lsf,   dum3,    dum2, 1,&
       's-1'  ,  'lsf qci-tend.',                           'average qci-tendency due to lsf',                          ' ')
 var(1,  43,13)= ar_des('AUT_LSF ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   aut_lsf,   dum3,    dum2, 1,&
       'K s-1'  ,  'lsf u tend.',                           'average u tendency due to lsf',                          ' ')
 var(1,  44,13)= ar_des('AVT_LSF ',     110,     0,     0,   1.0_wp    , 0.0_wp,   3,   3, ke, dum4,  dum4,   avt_lsf,   dum3,    dum2, 1,&
       'K s-1'  ,  'lsf v tend.',                           'average v tendency due to lsf',                          ' ')
! SBoeing, e
END IF !lbuid_avg
ENDIF !lbud
!cloud forcing>
var(1,111,9)= ar_des('SCFS_RAD  ',     1,     0,     0,   1.0_wp    , 0.0_wp,   3,   2,  -1,dum4,  dum4,  dum3,   dum3,    scfs, 1,&
      'W m-2       ','surface_shortwave_cloud_radiative_forcing',        'surface shortwave cloud radiative forcing', ' ')
var(1,112,9)= ar_des('TCFS_RAD  ',     1,     0,     0,   1.0_wp    , 0.0_wp,   3,   2,  -1,dum4,  dum4,  dum3,   dum3,    tcfs, 1,&
      'W m-2       ','surface_longwave_cloud_radiative_forcing',         'surface longwave cloud radiative forcing',  ' ')
var(1,113,9)= ar_des('SCFT_RAD  ',     1,     0,     0,   1.0_wp    , 0.0_wp,   3,   2,  -1,dum4,  dum4,  dum3,   dum3,    scft, 1,&
      'W m-2       ','toa_shortwave_cloud_radiative_forcing',            'toa shortwave cloud radiative forcing',     ' ')
var(1,114,9)= ar_des('TCFT_RAD  ',     1,     0,     0,   1.0_wp    , 0.0_wp,   3,   2,  -1, dum4,  dum4,  dum3,   dum3,    tcft, 1,&
      'W m-2       ','toa_longwave_cloud_radiative_forcing',             'toa longwave cloud radiative forcing',      ' ')
!cloud forcing<
!OCH>>
IF (lcmprates) THEN
!var(1,  50,12)= ar_des('QC_ACT     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_act_qc ,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'activation of qc',                           'activation of cloud droplets',                          ' ')
!var(1,  51,12)= ar_des('QNC_ACT     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_act_qnc,  dum3,    dum2, 1,&
!      'm-3'  ,  'activation of qnc',                           'activation of cloud droplets',                          ' ')
!var(1,  52,12)= ar_des('QI_IMFR     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_fr_qi  ,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'immersion freezing',                           'immersion freezing of ice particles',      ' ')
!var(1,  53,12)= ar_des('QNI_IMFR     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,   cmp_fr_qni,  dum3,    dum2, 1,&
!      'm-3'  ,  'imm fr of qni',                           'immersion freezing of ice numer',                          ' ')
!var(1,  54,12)= ar_des('QI_NUC     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_nuc_qi ,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'nucleation of qi',                           'nucleation of cloud ice mass',                          ' ')
!var(1,  55,12)= ar_des('QNI_NUC     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_nuc_qni,  dum3,    dum2, 1,&
!      'm-3'  ,  'nucleation of qni',                           'nucleation of cloud ice number',                          ' ')
var(1,  56,12)= ar_des('QI_DEP     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_dep_qi,   dum3,    dum2, 1,&
      'kg m-3'  ,  'depositional growth of qi',                           'depositional growth of ice',                          ' ')
!var(1,  57,12)= ar_des('QC_DEP     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_dep_qc ,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'dep growth of qc',                           'depositional growth od water dropets',      ' ')
var(1,  58,12)= ar_des('QI_SELF     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_self_qi,  dum3,    dum2, 1,&
      'kg m-3'  ,  'selfcollection ice',                           'mass of two colliding ice particles that is transfered two snow',    ' ')
var(1,  59,12)= ar_des('QNI_SELF     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,   cmp_self_qni,  dum3,    dum2, 1,&
      'm-3'  ,  'selfcollection qni',                           'number transfered to snow due to two colliding ice particles',       ' ')
!var(1,  60,12)= ar_des('QNS_SELF     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,   cmp_self_qns,  dum3,    dum2, 1,&
!      'm-3'  ,  'selfcollection qns',                           'snow crystal number reduced due to selfcollection',               ' ')
!var(1,  61,12)= ar_des('QNG_SELF     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,   cmp_self_qng,  dum3,    dum2, 1,&
!      'm-3'  ,  'selfcollection qng',                           'graupel number reduced due to selfcollection',               ' ')
var(1,  62,12)= ar_des('RIME_QI_QC  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_qi_qc,  dum3,    dum2, 1,&
      'kg m-3'  ,  'riming of qc on qi',                           'Riming on ice particles with cloud drops',    ' ')
var(1,  63,12)= ar_des('RIME_QS_QC  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_qs_qc,  dum3,    dum2, 1,&
      'kg m-3'  ,  'riming of qc on qs',                           'Riming on snow particles with cloud drops',    ' ')
!var(1,  64,12)= ar_des('RIME_QG  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_qg_qc,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'riming on qg',                           'Riming on graupel particles',    ' ')
var(1,  65,12)= ar_des('RIME_ICE  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_ice,  dum3,    dum2, 1,&
      'm-3'  ,  'dqn due to riming on qi',                           'Cloud number loss due to riming on ice',    ' ')
var(1,  66,12)= ar_des('RIME_SNOW  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_snow,  dum3,    dum2, 1,&
      'm-3'  ,  'dqn due to riming on qs',                           'Cloud number loss due to riming on snow',    ' ')
!var(1,  67,12)= ar_des('RIME_GRAUPEL  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_graup,  dum3,    dum2, 1,&
!      'm-3'  ,  'dqn due to riming on qs',                           'Cloud number loss due to riming on graupel',    ' ')
var(1,  68,12)= ar_des('COLL_QI_QS  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_coll_qi_qs,  dum3,    dum2, 1,&
      'm-3'  ,  'coll of qi by qs',                           'collection of cloud ice by snow',    ' ')
!var(1,  69,12)= ar_des('COLL_QNI_QNS  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_coll_qni_qns,  dum3,    dum2, 1,&
!      'm-3'  ,  'coll of qni and qns ',                           'collection of cloud ice by snow',    ' ')
!var(1,  70,12)= ar_des('CONV_QI_QG  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_conv_qi_qg,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'con from qi to qg',                           'Conversion rates from ice to graupel',    ' ')
!var(1,  71,12)= ar_des('CONV_QNI_QNG  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_conv_qni_qng,  dum3,    dum2, 1,&
!      'm-3'  ,  'con from qi to qg',                           'Conversion rates from ice to graupel',    ' ')
!var(1,  72,12)= ar_des('CONV_QS_QG  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_conv_qs_qg,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'con from qs to qg',                           'Conversion rates from snow to graupel',    ' ')
!var(1,  73,12)= ar_des('CONV_QNS_QNG  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_conv_qns_qng,  dum3,    dum2, 1,&
!      'm-3'  ,  'con from qs to qg',                           'Conversion rates from snow to graupel',    ' ')
!var(1,  74,12)= ar_des('MULT_ICE  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_mult_ice,  dum3,    dum2, 1,&
!      'm-3'  ,  'hm ice riming',                           'Hallet Mossop ice multiplication caused by riming on ice',    ' ')
!var(1,  75,12)= ar_des('MULT_SNOW  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_mult_snow,  dum3,    dum2, 1,&
!      'm-3'  ,  'hm snow riming',                           'Hallet Mossop ice multiplication caused by riming on snow',    ' ')
!var(1,  76,12)= ar_des('MULT_GRAUPEL  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_mult_graup,  dum3,    dum2, 1,&
!      'm-3'  ,  'hm graupel riming',                           'Hallet Mossop ice multiplication caused by riming on graupel',    ' ')
!var(1,  77,12)= ar_des('QI_HOM_SOL  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_hom_sol_qi,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'hom freezing of sol.',                           'Homogenous freezing of solution droplets',    ' ')
!var(1,  78,12)= ar_des('QNI_HOM_SOL  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_hom_sol_qni,  dum3,    dum2, 1,&
!      'm-3'  ,  'hom freezing of sol.',                           'Homogenous freezing of solution droplets',    ' ')
!var(1,  79,12)= ar_des('QI_HOM  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_hom_qi,  dum3,    dum2, 1,&
!      'kg m-3'  ,  'hom freezing of water.',                           'Homogenous freezing of cloud droplets',    ' ')
!var(1,  80,12)= ar_des('QNI_HOM  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_hom_qni,  dum3,    dum2, 1,&
!      'm-3'  ,  'hom freezing of water.',                           'Homogenous freezing of cloud droplets',    ' ')
var(1,  81,12)= ar_des('QS_DEP     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_dep_qs,   dum3,    dum2, 1,&
      'kg m-3'  ,  'depositional growth of qs',                           'depositional growth of snow',                          ' ')
!var(1,  82,12)= ar_des('QI_SUB     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_sub_qi,   dum3,    dum2, 1,&
!      'kg m-3'  ,  'sub of qi',                           'sublimation of ice',                          ' ')
!var(1,  83,12)= ar_des('QS_SUB     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_sub_qs,   dum3,    dum2, 1,&
!      'kg m-3'  ,  'sub of qs',                           'sublimation of snow',                          ' ')
var(1,  84,12)= ar_des('QNC_SELF     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_self_qnc,   dum3,    dum2, 1,&
      'm-3'  ,  'selfcollection qc',                           'selfcollection of cloud droplets',                          ' ')
var(1,  85,12)= ar_des('CONV_QC_QR     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_conv_qc_qr,   dum3,    dum2, 1,&
      'kg m-3'  ,  'autocon qc',                           'autoconversion of cloud droplets',                          ' ')
!var(1,  86,12)= ar_des('CONV_QNC_QNR     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_conv_qnc_qnr,   dum3,    dum2, 1,&
!      'm-3'  ,  'autocon qc',                           'autoconversion of cloud droplets',                          ' ')
!var(1,  87,12)= ar_des('COLL_QC_QR     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_coll_qc_qr,   dum3,    dum2, 1,&
!      'kg m-3'  ,  'accreation qc',                           'accreation cloud droplets',                          ' ')
!var(1,  88,12)= ar_des('COLL_QNC_QNR     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_coll_qnc_qnr,   dum3,    dum2, 1,&
!      'm-3'  ,  'accreation qc',                           'accreation of cloud droplets',                          ' ')
!var(1,  89,12)= ar_des('EVAP_QC     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_evap_qc,   dum3,    dum2, 1,&
!      'kg m-3'  ,  'evaporation of QC after satad in sb_interface',                           'evaporation of QC after satad in sb_interface',                          ' ')
!var(1,  90,12)= ar_des('COND_QC     ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,     cmp_cond_qc,   dum3,    dum2, 1,&
!      'kg m-3'  ,  'condensation of QC after satad in sb_interface',                           'condensation of QC after satad in sb_interface',                          ' ')
var(1,  91,12)= ar_des('RIME_QI_QR  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_qi_qc,  dum3,    dum2, 1,&
      'kg m-3'  ,  'riming of qr on qi',                           'Riming on ice particles with rain',    ' ')
var(1,  92,12)= ar_des('RIME_QS_QR  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_qs_qc,  dum3,    dum2, 1,&
      'kg m-3'  ,  'riming of qr on qs',                           'Riming on snow particles with rain',    ' ')
var(1,  93,12)= ar_des('MELT_QI_QR  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_qi_qc,  dum3,    dum2, 1,&
      'kg m-3'  ,  'melting of qi to qr',                           'Melting of ice to rain',    ' ')
var(1,  94,12)= ar_des('MELT_QS_QR  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_rime_qs_qc,  dum3,    dum2, 1,&
      'kg m-3'  ,  'melting of qs to qr',                           'Melting of snow to rain',    ' ')
var(1,  95,12)= ar_des('QG_SEDI  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_sedi_qg,  dum3,    dum2, 1,&
      'kg kg-1'  ,  'graupel sedimentation',                           'graupel sedimentation',    ' ')
var(1,  96,12)= ar_des('QNG_SEDI  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_sedi_qng,  dum3,    dum2, 1,&
      'kg-1'  ,  'graupel sedimentation',                           'graupel sedimentation',    ' ')
var(1,  97,12)= ar_des('QS_SEDI  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_sedi_qs,  dum3,    dum2, 1,&
      'kg kg-1'  ,  'snow sedimentation',                           'snow sedimentation',    ' ')
var(1,  98,12)= ar_des('QNS_SEDI  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_sedi_qns,  dum3,    dum2, 1,&
      'kg-1'  ,  'snow sedimentation',                           'snow sedimentation',    ' ')
var(1,  99,12)= ar_des('QI_SEDI  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_sedi_qi,  dum3,    dum2, 1,&
      'kg kg-1'  ,  'ice sedimentation',                           'ice sedimentation',    ' ')
var(1,  100,12)= ar_des('QNI_SEDI  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_sedi_qni,  dum3,    dum2, 1,&
      'kg-1'  ,  'ice sedimentation',                           'ice sedimentation',    ' ')
var(1,  101,12)= ar_des('QR_SEDI  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_sedi_qr,  dum3,    dum2, 1,&
      'kg kg-1'  ,  'rain sedimentation',                           'rain sedimentation',    ' ')
var(1,  102,12)= ar_des('QNR_SEDI  ',     110,     0,     0,   1.0    , 0.0,   0,   3, ke, dum4,  dum4,    cmp_sedi_qnr,  dum3,    dum2, 1,&
      'kg-1'  ,  'rain sedimentation',                           'rain sedimentation',    ' ')
END IF
!<<OCH

!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------------------

IF (lcheck_alloc) THEN
#ifndef MESSY
IF (.NOT. ALLOCATED(ps       ))  var(1,  1,1)%idef_stat = -1
IF (.NOT. ALLOCATED(dpsdt    ))  var(1,  3,1)%idef_stat = -1
IF (.NOT. ALLOCATED(hsurf    ))  var(1,  8,1)%idef_stat = -1
IF (.NOT. ALLOCATED(hhl      ))  var(2,  8,1)%idef_stat = -1
IF (.NOT. ALLOCATED(t_g      ))  var(1, 11,1)%idef_stat = -1
IF (.NOT. ALLOCATED(t_2m     ))  var(2, 11,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_2m_av  ))  var(3, 11,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(t        ))  var(4, 11,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(tmax_2m  ))  var(1, 15,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(tmin_2m  ))  var(1, 16,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(td_2m    ))  var(1, 17,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(td_2m_av ))  var(2, 17,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(u_10m    ))  var(1, 33,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(u_10m_av ))  var(2, 33,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(u        ))  var(3, 33,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(v_10m    ))  var(1, 34,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(v_10m_av ))  var(2, 34,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(v        ))  var(3, 34,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(w        ))  var(1, 40,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(qv_s     ))  var(1, 51,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(qv_2m    ))  var(2, 51,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(rh_2m    ))  var(2, 52,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(aevap_s  ))  var(1, 57,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_snow   ))  var(1, 65,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_snow_mult))var(2, 65,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(h_snow   ))  var(1, 66,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(dzh_snow_mult)) var(2, 66,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(clct     ))  var(1, 71,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(clc_con  ))  var(1, 72,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(clcl     ))  var(1, 73,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(clcm     ))  var(1, 74,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(clch     ))  var(1, 75,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(snow_con ))  var(1, 78,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(snow_gsp ))  var(1, 79,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(fr_land  ))  var(1, 81,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(gz0      ))  var(1, 83,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(alb_rad  ))  var(1, 84,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_s      ))  var(1, 85,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_m      ))  var(2, 85,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_cl     ))  var(3, 85,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_g1     ))  var(1, 86,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_g2     ))  var(2, 86,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_g3     ))  var(3, 86,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_cl     ))  var(4, 86,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(plcov    ))  var(1, 87,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(runoff_s ))  var(1, 90,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(runoff_g ))  var(2, 90,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(fr_ice   ))  var(1, 91,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(h_ice    ))  var(1, 92,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(sobs     ))  var(1,111,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(asob_s   ))  var(2,111,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(thbs     ))  var(1,112,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(athb_s   ))  var(2,112,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(sobt     ))  var(1,113,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(asob_t   ))  var(2,113,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(thbt     ))  var(1,114,1)%idef_stat = -1
!cloud forcing>
  IF (.NOT. ALLOCATED(scfs     ))  var(1,111,9)%idef_stat = -1
  IF (.NOT. ALLOCATED(tcfs     ))  var(1,112,9)%idef_stat = -1
  IF (.NOT. ALLOCATED(scft     ))  var(1,113,9)%idef_stat = -1
  IF (.NOT. ALLOCATED(tcft     ))  var(1,114,9)%idef_stat = -1
!cloud forcing<
  IF (.NOT. ALLOCATED(athb_t   ))  var(2,114,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(sobcs    ))  var(1,115,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(thbcs    ))  var(1,116,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(sobct    ))  var(1,117,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(thbct    ))  var(1,118,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(asobc_s  ))  var(2,115,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(athbc_s  ))  var(2,116,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(asobc_t  ))  var(2,117,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(athbc_t  ))  var(2,118,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(lhfl_s   ))  var(1,121,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(alhfl_s  ))  var(2,121,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(shfl_s   ))  var(1,122,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(ashfl_s  ))  var(2,122,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(umfl_s   ))  var(1,124,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(aumfl_s  ))  var(2,124,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(vmfl_s   ))  var(1,125,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(avmfl_s  ))  var(2,125,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(pabs     ))  var(1,  5,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(apab_s   ))  var(2,  5,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(sohr     ))  var(1, 13,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(thhr     ))  var(1, 14,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(alhfl_bs ))  var(1, 18,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(alhfl_pl ))  var(1, 19,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(dursun   ))  var(1, 20,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(rstom    ))  var(1, 21,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(swdir_s  ))  var(1, 22,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(swdifd_s ))  var(1, 23,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(swdifu_s ))  var(1, 24,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(lwd_s    ))  var(1, 25,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(lwu_s    ))  var(1, 26,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(aswdir_s ))  var(2, 22,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(aswdifd_s))  var(2, 23,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(aswdifu_s))  var(2, 24,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(alwd_s   ))  var(2, 25,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(alwu_s   ))  var(2, 26,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(sod_t    ))  var(1, 27,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(asod_t   ))  var(2, 27,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(clc_sgs  ))  var(1, 30,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tdiv_hum ))  var(1, 42,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(clw_con  ))  var(1, 61,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(bas_con  ))  var(1, 72,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(top_con  ))  var(1, 73,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_conv  ))  var(1, 74,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_conv ))  var(1, 75,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(ut_conv  ))  var(1, 78,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(vt_conv  ))  var(1, 79,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(qct_conv ))  var(1, 88,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(qit_conv ))  var(1, 89,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(c_t_lk   ))  var(1, 91,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(gamso_lk ))  var(1, 92,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(dp_bs_lk ))  var(1, 93,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(h_b1_lk  ))  var(1, 94,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(h_ml_lk  ))  var(1, 95,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(depth_lk ))  var(1, 96,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(fetch_lk ))  var(1, 97,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(qrs      ))  var(1, 99,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(prr_gsp  ))  var(1,100,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(prs_gsp  ))  var(1,101,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(rain_gsp ))  var(1,102,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(prr_con  ))  var(1,111,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(prs_con  ))  var(1,112,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(rain_con ))  var(1,113,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(freshsnow))  var(1,129,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(prg_gsp  ))  var(1,131,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(grau_gsp ))  var(1,132,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(prh_gsp  ))  var(1,134,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(hail_gsp ))  var(1,135,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(rho_snow ))  var(1,133,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(rho_snow_mult)) var(2,133,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(wliq_snow))  var(1,137,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(pp       ))  var(1,139,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(rcld     ))  var(1,140,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tke_con  ))  var(1,147,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tketens  ))  var(1,148,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(edr      ))  var(1,151,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tke      ))  var(1,152,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tkvm     ))  var(1,153,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tkvh     ))  var(1,154,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tket_sso ))  var(1,155,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tket_hshr))  var(1,156,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tket_conv))  var(1,157,2)%idef_stat = -1
  !(WL;2010)b
  IF (.NOT. ALLOCATED(tkhm     ))  var(1,155,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tkhh     ))  var(1,156,2)%idef_stat = -1
  !(WL;2010)e
  IF (.NOT. ALLOCATED(tcm      ))  var(1,170,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(tch      ))  var(1,171,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(vmax_10m ))  var(1,187,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_bs_lk  ))  var(1,190,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_bot_lk ))  var(1,191,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_b1_lk  ))  var(1,192,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_wml_lk ))  var(1,193,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_mnw_lk ))  var(1,194,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_so     ))  var(1,197,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_so     ))  var(1,198,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_so_ice ))  var(1,199,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(w_i      ))  var(1,200,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(s_so     ))  var(1,201,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_snow   ))  var(1,203,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_snow_mult))var(2,203,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(rsmin2d  ))  var(1,212,2)%idef_stat = -1  
  IF (.NOT. ALLOCATED(t_ice    ))  var(1,215,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(vabsmx_10m)) var(1,216,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(vgust_dyn))  var(1,218,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(vgust_con))  var(1,219,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(mflx_con ))  var(1,240,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(cape_con ))  var(1,241,2)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcvg_con ))  var(1,243,2)%idef_stat = -1

  IF (.NOT. ALLOCATED(tt_sso))     var(1, 43,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(ut_sso))     var(1, 44,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(vt_sso))     var(1, 45,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(sso_stdh ))  var(1, 46,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(sso_gamma))  var(1, 47,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(sso_theta))  var(1, 48,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(sso_sigma))  var(1, 49,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(fr_lake  ))  var(1, 55,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(emis_rad ))  var(1, 56,3)%idef_stat = -1   
  IF (.NOT. ALLOCATED(soiltyp  ))  var(1, 57,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(lai      ))  var(1, 61,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(rootdp   ))  var(1, 62,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(hmo3     ))  var(1, 64,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(vio3     ))  var(1, 65,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(for_e    ))  var(1, 75,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(for_d    ))  var(1, 76,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(aer_su   ))  var(1, 84,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(aer_du   ))  var(1, 86,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(aer_or   ))  var(1, 91,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(aer_bc   ))  var(1, 92,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(aer_ss   ))  var(1, 93,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(horizon  ))  var(1, 96,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(swdir_cor))  var(1, 97,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(slo_ang  ))  var(1, 98,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(slo_asp  ))  var(1, 99,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(skyview  ))  var(1,100,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(fc       ))  var(1,113,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(rlat     ))  var(1,114,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(rlon     ))  var(1,115,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(alb_dry  ))  var(1,127,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(alb_sat  ))  var(1,128,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(alb_dif  ))  var(1,129,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(ustr_sso ))  var(1,231,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(austr_sso))  var(2,231,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(vstr_sso ))  var(1,232,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(avstr_sso))  var(2,232,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(vdis_sso ))  var(1,233,3)%idef_stat = -1
  IF (.NOT. ALLOCATED(avdis_sso))  var(2,233,3)%idef_stat = -1

  !(WL;2010)b
  IF (.NOT. ALLOCATED(hflux    ))  var(1, 241,4)%idef_stat = -1
  IF (.NOT. ALLOCATED(eflux    ))  var(1, 242,4)%idef_stat = -1
  IF (.NOT. ALLOCATED(ahflux   ))  var(1, 243,4)%idef_stat = -1
  IF (.NOT. ALLOCATED(aeflux   ))  var(1, 244,4)%idef_stat = -1
  IF (.NOT. ALLOCATED(def13    ))  var(1, 130,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(def23    ))  var(1, 131,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(def12    ))  var(1, 132,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(def11    ))  var(1, 133,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(def22    ))  var(1, 134,1)%idef_stat = -1
  IF (.NOT. ALLOCATED(def33    ))  var(1, 135,1)%idef_stat = -1
  !(WL;2010)e

  IF (.NOT. ALLOCATED(tinc_lh  ))  var(1,  1,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(ff_anai  ))  var(1, 21,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(dd_anai  ))  var(1, 22,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(t_anai   ))  var(1, 23,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(p_anai   ))  var(1, 25,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(qv_anai  ))  var(1, 27,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(qc_anai  ))  var(1, 28,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(p_anai   ))  var(1, 24,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(p_anai   ))  var(1, 26,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(qv_anai  ))  var(1, 29,5)%idef_stat = -1
  IF (.NOT. ALLOCATED(qc_anai  ))  var(1, 30,5)%idef_stat = -1

  IF (.NOT. ALLOCATED(dursun_m ))  var(1, 25,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(dursun_r ))  var(1, 26,11)%idef_stat = -1

#ifdef TEND
! Variables for tendency-sum output (dmaurer)
  IF (.NOT. ALLOCATED(ttend_rad))  var(1, 31,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_sso))  var(1, 32,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_tur))  var(1, 33,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_tmp1)) var(1, 34,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_tmp2)) var(1, 35,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_con))  var(1, 36,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_hyd))  var(1, 37,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_tot))  var(1, 38,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvten_tur))  var(1, 39,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvten_con))  var(1, 40,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvten_hyd))  var(1, 41,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvten_tot))  var(1, 42,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(utend_sso))  var(1, 43,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(utend_tur))  var(1, 44,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(utend_tmp1)) var(1, 45,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(utend_tmp2)) var(1, 46,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(utend_tot))  var(1, 47,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(vtend_sso))  var(1, 48,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(vtend_tur))  var(1, 49,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(vtend_tmp1)) var(1, 50,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(vtend_tmp2)) var(1, 51,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(vtend_tot))  var(1, 86,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_tmp3)) var(1, 87,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(utend_tmp3)) var(1, 88,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(vtend_tmp3)) var(1, 89,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvten_tmp3)) var(1, 90,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_tmp4)) var(1, 91,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(utend_tmp4)) var(1, 92,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(vtend_tmp4)) var(1, 93,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvten_tmp4)) var(1, 94,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(ttend_tur2)) var(1, 95,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(utend_tur2)) var(1, 96,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(vtend_tur2)) var(1, 97,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvten_tur2)) var(1, 98,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(countref)) var(1, 99,11)%idef_stat = -1
  IF (.NOT. ALLOCATED(counter)) var(1,100,11)%idef_stat = -1
#endif

  !(WL;2009)b
  IF (.NOT. ALLOCATED(tt_mic   ))  var(1,  1,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_rad   ))  var(1,  2,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_turb  ))  var(1,  3,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_adv   ))  var(1,  4,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_hd    ))  var(1,  5,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_tot   ))  var(1,  6,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_mic  ))  var(1,  7,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcit_mic ))  var(1,  8,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qrsgt_mic))  var(1,  9,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_adv  ))  var(1,  10,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcit_adv ))  var(1,  11,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qrsgt_adv))  var(1,  12,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_turb ))  var(1,  13,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcit_turb))  var(1,  14,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_tot  ))  var(1,  15,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcit_tot ))  var(1,  16,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qrsgt_tot))  var(1,  17,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_hd   ))  var(1,  18,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcit_hd  ))  var(1,  19,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_zadv  ))  var(1,  20,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcit_zadv ))  var(1,  21,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qrsgt_zadv))  var(1,  22,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_zadv   ))  var(1,  23,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_dpdt   ))  var(1,  24,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_con    ))  var(1,  25,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_rlx    ))  var(1,  26,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_rlx   ))  var(1,  27,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcit_rlx  ))  var(1,  28,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qrsgt_rlx ))  var(1,  29,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_ssowl  ))  var(1,  30,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(tt_hadv   ))  var(1,  31,12)%idef_stat = -1
  ! relax
  IF (.NOT. ALLOCATED(tt_relax  ))  var(1,  32,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_relax ))  var(1,  33,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(ut_relax  ))  var(1,  34,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(vt_relax  ))  var(1,  35,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(wso_relax ))  var(1,  36,12)%idef_stat = -1
  ! Sboeing, b
  IF (.NOT. ALLOCATED(tt_lsf  ))  var(1,  37,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qvt_lsf ))  var(1,  38,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(qcit_lsf )) var(1,  39,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(ut_lsf  ))  var(1,  40,12)%idef_stat = -1
  IF (.NOT. ALLOCATED(vt_lsf  ))  var(1,  41,12)%idef_stat = -1
  ! Sboeing, e

  IF (.NOT. ALLOCATED(att_mic   ))  var(1,  1,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_rad   ))  var(1,  2,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_turb  ))  var(1,  3,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_adv   ))  var(1,  4,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_hd    ))  var(1,  5,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_tot   ))  var(1,  6,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_mic  ))  var(1,  7,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_mic ))  var(1,  8,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqrsgt_mic))  var(1,  9,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_adv  ))  var(1,  10,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_adv ))  var(1,  11,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqrsgt_adv))  var(1,  12,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_turb ))  var(1,  13,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_turb))  var(1,  14,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_tot  ))  var(1,  15,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_tot ))  var(1,  16,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqrsgt_tot))  var(1,  17,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_hd   ))  var(1,  18,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_hd  ))  var(1,  19,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_zadv  ))  var(1,  20,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_zadv ))  var(1,  21,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqrsgt_zadv))  var(1,  22,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_zadv   ))  var(1,  23,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_dpdt   ))  var(1,  24,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_con    ))  var(1,  25,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_rlx    ))  var(1,  26,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_rlx   ))  var(1,  27,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_rlx  ))  var(1,  28,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqrsgt_rlx ))  var(1,  29,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_ssowl  ))  var(1,  30,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_con   ))  var(1,  31,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_con  ))  var(1,  32,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqrsgt_con ))  var(1,  33,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_hadv   ))  var(1,  34,13)%idef_stat = -1
  !(WL;2009)e
  ! relax
  IF (.NOT. ALLOCATED(att_relax  ))  var(1,  35,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_relax ))  var(1,  36,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aut_relax  ))  var(1,  37,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(avt_relax  ))  var(1,  38,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(awso_relax  )) var(1,  39,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(att_lsf    ))  var(1,  40,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqvt_lsf   ))  var(1,  41,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aqcit_lsf   )) var(1,  42,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(aut_lsf    ))  var(1,  43,13)%idef_stat = -1
  IF (.NOT. ALLOCATED(avt_lsf    ))  var(1,  44,13)%idef_stat = -1

#endif
  lcheck_alloc = .FALSE.
ENDIF

END SUBROUTINE setup_vartab

!------------------------------------------------------------------------------
! End of Module
!------------------------------------------------------------------------------

END MODULE src_setup_vartab
