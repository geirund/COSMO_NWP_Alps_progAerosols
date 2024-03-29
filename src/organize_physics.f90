!+ External procedure for organizing the calls to the physics packages
!------------------------------------------------------------------------------

SUBROUTINE organize_physics (yaction, ierror, yerrmsg)

!------------------------------------------------------------------------------
!
! Description:
!   This procedure is the driving routine for calling the physical
!   parametrizations. It is called first at the beginning of the program just
!   to initialize the physical packages. Later it is called during the 
!   time-stepping in the initialization and the main program.
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
! 1.29       1999/05/11 Ulrich Schaettler
!  Initial release
! 1.30       1999/06/24 Matthias Raschendorfer
!  Including the call of the new turbulence routines: turbdiff, turbtran,
!  canopy_source, turbdiff_hori                 
! 1.33       1999/10/14 Matthias Raschendorfer
!  Introduction of the LOGICAL lstfnct, controlling the calculation of a
!  new stability function in sub. turbdiff.
! 1.34       1999/12/10 Ulrich Schaettler
!  Changed Calls to timing routines and included Call to hydci (lmorg before)
! 1.39       2000/05/03 Ulrich Schaettler
!  Included subroutine for Namelist input and some technical changes.
! 2.2        2000/08/18 Guenther Doms
!  Introduction of the logical switch 'lconf_avg' on Namelist input to
!  enable (default) or disable a horizontal averaging of the convective
!  forcing functions. Also, two new REAL namelist input parameters
!  'c_soil' and 'e_surf' to control surface fluxes have been introduced. 
!  An error in the check of namelist input parameters was corrected.
! 2.3        2000/11/15 Guenther Doms
!  An error in the call of the TKE-subroutines 'turbtran.incf' and
!  'turbdiff.incf' was corrected.
! 2.8        2001/07/06 Ulrich Schaettler
!  Eliminated non-necessary variables from the USE-lists
!  Introduced new NAMELIST variables for multi-layer soil-model 
!         (ke_soil, isoillevels, lmulti_layer, lmelt)
! 2.11       2001/09/28 Ulrich Schaettler
!  Renamed a variable for the multi-layer soil model and added another one
! 2.15       2002/03/20 Matthias Raschendorfer
!  Introduction of the roughness length for a typical SYNOP-station (z0m_dia)
! 2.17       2002/05/08 Ulrich Schaettler
!  Adaptations to use the new parameterization schemes
! 2.18       2002/07/16 Ulrich Schaettler
!  Added error variables in call to terra2_multlay; extended range for
!  itype_gscp for prognostic rain.
! 2.19       2002/10/24 Ulrich Schaettler
!  Eliminated call to vertical_diffusion_impl in case of 2 time level scheme
! 3.2        2003/02/07 Ulrich Schaettler
!  Moved some communications from the Convection scheme to organize_physics.
! 3.5        2003/09/02 Ulrich Schaettler
!  Adaptations for the interface of exchg_boundaries
! 3.6        2003/12/11 Reinhold Schrodin
!  Adaptations for the multi-layer soil model;
!  Modifications for checking the IOSTAT-value when reading the NAMELIST
! 3.7        2004/02/18 Ulrich Schaettler
!  Namelist Input for new variable lprogprec; ltrans_prec; rat_sea
!  Changed selection of precipitation module
! 3.8        2004/03/23 Jochen Foerstner
!  For the 2 timelevel scheme and prognostic precipitation, the precipitation
!  scheme (renamed to hydci_pp) is now also called after the dynamics.
! 3.12       2004/09/15 Christoph Schraff
!  New Namelist variable 'ldiniprec'.
! 3.13       2004/12/03 Jochen Foerstner / Thorsten Reinhardt
!  New Namelist variables for 3D turbulence
! 3.15       2005/03/03 Ulrich Schaettler
!  Editorial changes
! 3.16       2005/07/22 M. Raschendorfer, J. Foerstner, R. Schrodin
!  New Namelist variables for Physics, Turbulence scheme and shallow convection
!  Multi layer soil model completely called before convection
! 3.18       2006/03/03 Ulrich Schaettler, Dmitrii Mironov
!  Added variables for CLM version (ico2_rad, czbot_w_so, ibot_w_so)
!  Added switch llake and calls to the routines of the lake model FLake
!  Added switch l3dturb_metr for using metric terms for 3D-turbulence
!  Determination of ntke in case of restarts
! 3.19       2006/04/25 Ulrich Schaettler
!  Provide the depths of soil half levels for NetCDF IO
! 3.21       2006/12/04 Ulrich Schaettler, Burkhardt Rockel
!  Bug correction in distributing Namelist input
!  Maximum value for ico2_rad set to 6
!  In case of climate runs, call init_canopy every time step
!  Removed several NL variables and put them to TUNING (in src_setup)
!  Added NL variable lbechtold and call to Bechtold scheme (MeteoSwiss)
! 3.22       2007/01/24 Jochen Foerstner
!  Corrections for Runge-Kutta Restart: TKE has to be put to "nnew"
! V3_23        2007/03/30 Ulrich Schaettler
!  Introduction of idbg_level;
!  Eliminated hincconv, hinctura (these values can only be set in time steps)
!  Changed determination when to call the radiation scheme.
!  Changed default settings of some namelist variables
!  Added new NL variables lradtopo, nhori (Matteo Buzzi)
! V3_26        2007/05/29 Ulrich Schaettler
!  Bug correction when running with nincrad = 1
! V4_4         2008/07/16 Ulrich Schaettler, Jan-Peter Schulz
!  Adapted interface of get_timings
!  Added NL variable lsso
!  Replaced logical switches for convection (ltiedtke...) by itype_conv
!  Initialize nincrad, if only hincrad is given in NL input
! V4_5         2008/09/10 Jan-Peter Schulz
!  Activation of SSO scheme with additional Namelist parameter nincsso
!  Add possibility to use turbulence scheme without surface friction (G. Zaengl)
! V4_9         2009/07/16 Ulrich Schaettler
!  Corrected a comment for CO2
!  Check calls to get_timings
!  Adapted a check for ltrans_prec and l2tls
!  Included boundary exchange necessary for radiation averaging
! V4_10        2009/09/11 Matthias Raschendorfer, Jan-Peter Schulz
!  Introduction of LOGICALs limpltkediff, ltkesso and removing lturhor.
!  Introduction of INTEGER itype_shar
!  Moving the call of 'organize_sso' to be executed before the call of turbulence.
!  Introduction of sea-ice model
! V4_11        2009/11/30 Ekaterina Machulskaya, Juergen Helmert
!  Read Namelist switches lmulti_snow, ke_snow  (EM)
!  Read Namelist switches itype_aerosol, itype_root, itype_heatcond, 
!       itype_hydbound, lemiss, lstomata
! V4_12        2010/05/11 Michael Baldauf
!  set flag l_dzeta_d_needed, if 3D Turbulence is used
! V4_13        2010/05/11 Michael Gertz
!  Adaptions to SVN
! V4_17        2011/02/24 Ulrich Blahak
!  - Adapted interface of exchg_boundaries;
!  - corrected kzdims(1:20) -> kzdims(1:24);
!  - eliminated my_peri_neigh;
!  - eliminated lfreeslip_surf (free-slip BC and/or no-surface-heat/moisture-flux
!    conditions may now be imposed by new switches lnosurffluxes_m/h in namelist IDEAL);
!  - introduced new itype_turb=100 to specify the fixed diffusion coefficients
!    tkvhfix, tkhhfix, tkvmfix, tkhmfix in namelist IDEAL (mainly intended for idealized runs, but may also
!    be used for real cases); 
!  - added some missing communication for tkhm, tkhh, tke, tketens.
! V4_18        2011/05/26 Michael Baldauf
!  Introduced new NL switch y_scalar_advect (which replaces lsl_adv_qx and yef_adv_qx)
!  Restrict use of lradtopo to nradcoarse=1 (US)
! V4_20        2011/08/31 Ulrich Schaettler
!  Introduced call to new subroutine organize_turbulence which organizes the
!    calls to the packages for turbulence parameterization (therefore removed
!    calls to routines from src_turbulence and src_turbdiff)
!  Re-organized 'init'-phase for the turbulence (now calls init_volume_canopy
!    and init_surface_canopy
!  Introduction of NAMELIST parameter 'ltkecon' (Raschendorfer)
! Setting 'lctke=F' in case of 'ltur=F'.
! V4_21        2011/12/06 Ulrich Schaettler
!  Additional debug output
! V4_22        2012/01/31 Thorsten Reinhardt
!  Adaptations for solar zenith angle updating in every timestep.
! V4_23        2012/05/10 Ulrich Schaettler, Oli Fuhrer, CLM
!  Removed src_2timelevel and related stuff
!  Removed switches lprogprec, ltrans_prec
!  Removed Kain-Fritsch convection scheme 
!    only possible at the moment: itype_conv = 0 / 3
!  Added computation of total physical tendencies at the end of action='compute'.
!    These computations have been moved from the dynamical cores, to clean up
!    the code.
!  New Namelist parameters iy_co2_stab, lco2_stab; extended range of ico2_rad to 10
!  Introduction of prescribed soil albedo: new namelist parameter itype_albedo
!     (CLM)
! V4_25        2012/09/28 Anne Roches, Oliver Fuhrer, Ulrich Blahak
!                         Hans-Juergen Panitz (CLM)
!  Added part for the initialization of the tracer module
!  Replaced qx-variables by using them from the tracer module
!  UB:
!  Added additional tracers for the 2-moment scheme.
!  For 2-moment microphysics: added calls to init_seifert and seifert_pp;
!    added consistency checks with relevant namelist switches.
!  Implemented internal switch "l2mom_satads" to be able to switch on the 
!    extra saturation adjustments outside the microphysics parts.
!  Added plausibility checks regarding lrad and itype_aerosols in combination
!   with periodic BCs.
!  Prohibit concurrent usage of nboundlines > 3 and nradcoarse > 2, 
!    which is not working (Uli Schaettler)
!  Implement IFS convection for CLM with conditional compilation (ifdef CLM)
!  HJP for CLM:
!  Allow a new value for itype_conv (=2) to choose IFS convection
!  Introduced a warning in case itype_conv=2 and lconf_avg=TRUE (will be set
!    to FALSE then)  
!  Introduced nexch_tag for MPI boundary exchange tag to replace ntstep (HJP)
! V4_26        2012/12/06 Hans-Juergen Panitz, Ulrich Schaettler, Anne Roches
!  Computation of physical forcings (tendencies):
!   Delete lines related to itpye_conv=1 since itype_conv=1 is not valid anymore
!  Read ntke in case of restarts from restart file instead of recomputing
!  Changes and technical adaptations to the tracer handling (by Anne Roches)
! V4_27        2013/03/19 Astrid Kerkweg
!  MESSy interface introduced
!  bugfix for tke vertical dimension in exchg_boundaries call in case of l3dturb
!  Modified default values of some tuning constants to reflect settings of COSMO-EU (US)
! V4_28        2013/07/12 Ulrich Schaettler, KIT
!  Compute depth of soil half layers (czhls) in all subdomains (US)
!  Changes to adapt COSMO-ART to new tracer module
! V4_29        2013-10-02 Astrid Kerkweg, Ulrich Schaettler
!  Unification of MESSy interfaces and COSMO Tracer structure
!  Allocation and boundary exchange for tkvm, tkvh, tkhm, tkhh for 1:ke1 
!   in the vertical
! V4_30        2013-11-08 Lucio Torrisi
!  Set dt2, which is needed for a diagnostic initialization calling hydci
! @VERSION@    @DATE@     Ulrich Blahak
!  Enabled lprog_tke=.true. also for M. Raschendorfers turbulence scheme.
!  Catched setting of lprog_tke in case of ltur=.false. or lphys=.false.
!  
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================

USE data_parameters,    ONLY:  wp, iintegers

USE  data_modelconfig,   ONLY:                                                &
       ie, je, ke, ke_soil, jstartpar, jendpar, dt, dt2, czmls, msoilgrib,   &
       czhls, istartu, iendu, jstartu, jendu, istartv, iendv, jstartv, jendv,&
       ke_snow, ke1, kcm, istartpar, iendpar, istart, iend, jstart, jend,    &
       idt_qv, idt_qc,  idt_qi,  idt_qr,  idt_qs,  idt_qg,  idt_qh,          &
               idt_qnc, idt_qnr, idt_qni, idt_qns, idt_qng, idt_qnh,         &
       idt_T1, idt_T2, idt_T3, idt_T4, idt_T5, idt_T6, idt_T7, idt_T8,       &
       idt_T9, idt_T10, idt_T11, idt_T12, idt_T13, idt_T14, idt_T15,         &
       iccn1, iccn2, iccn3

USE data_runcontrol,    ONLY:                                                &
       lphys, lgsp, lINprog, lCCNprog, lrad, ltur, lconv, itype_conv, lsoil, lmelt, lmelt_var,  &
       lmulti_layer, ldiniprec, lprog_qi, nradcoarse,                        &
       lradf_avg, nincrad, nextrad, hincrad, hnextrad, ninctura, nincconv,   &
       itype_tran, itype_turb, itype_gscp, itype_synd, itype_wcld,           &
       imode_tran, imode_turb, icldm_rad, icldm_tran, icldm_turb, nstart,    &
       ntstep, l2tls, ltime, nold, nnow, nnew, nuspecif,                     &
       lexpcor, lnonloc, lcpfluc, lconf_avg, lcape, lctke, ltmpcor, lprfcor, &
       itype_trvg, itype_evsl, nlgw, nbl_exchg, l3dturb,                     &
       lprog_tke, lconv_inst, lforest, luse_rttov, ntke, lseaice, llake,     &
!cloud forcing>
       lcrf,                                                                 &
!cloud forcing<
!<FG: 2M-scheme
       iradpar_cloud,                                                        &
!FG: 2M-scheme>
       Tdiff_IN, Tmax_IN, Na_IN,CCN_ship_N, tperturb, Na_IN_pert,           &
       ico2_rad, ibot_w_so, czbot_w_so, l3dturb_metr, idbg_level,            &
       lprintdeb_all, lradtopo, nhori, hstart, lsso, nincsso,                &
       itype_sher, limpltkediff, ltkesso, ltkecon, lmulti_snow, lemiss,      &
       lradcst, radlon, radlat,                                              &
       lstomata, itype_aerosol, itype_root, itype_heatcond, itype_hydbound,  &
       l_dzeta_d_needed, lperi_x, lperi_y, l2dim, nbl_exchg,                 &
       lartif_data, y_scalar_advect, ldiabf_lh, lco2_stab, iy_co2_stab,      &
       itype_albedo, itype_lbc_qrsg, itype_outflow_qrsg, lspubc,             &
       itype_spubc, l2mom_satads, l_cosmo_art, l_pollen, l_2mom,             &
       !(WL;2010)b
       lkhdef3d, lisotrop, lneutral,                                         &
       !(WL;2010)e
!WL2011b
       lbud, CCN_1_N, CCN_2_N, CCN_3_N                             

!WL2011e

USE data_parallel,      ONLY:                                                &
       num_compute, my_cart_id, icomm_cart, sendbuf, isendbuflen, iexch_req, &
       imp_reals, imp_integers, imp_logical, nboundlines, my_cart_neigh,     &
       nproc, my_world_id, icomm_world, intbuf, realbuf, logbuf, ncomm_type, &
       ldatatypes, ltime_barrier, nexch_tag

USE data_fields,        ONLY:                                                &
       tkvm, tkvh, tcm, tch, qrs, t_s, t_g, t_snow, w_snow, freshsnow, qv_s, &
       w_so, w_g1, ut_conv, vt_conv, clc_con, ut_sso, vt_sso, fr_land, d_pat,&
       c_big, c_sml, r_air, plcov, sai, tai, lai, eai, hhl, h_can,           &
       tkhm, tkhh, tke, tketens, ttens, utens, vtens,                        &
       sohr, thhr, tt_conv, qvt_conv, qct_conv, qit_conv, ut_conv, vt_conv,  &
       ut_sso, vt_sso, tt_sso, tinc_lh,                                      &
!WL2011b
       qvt_turb, tt_rad, tt_ssowl, tt_con, ttens      
!WL2011e

#ifdef TEND
! Variables for tendency-sum output (dmaurer)
USE data_fields     , ONLY :   &
    ttend_rad  ,    & ! Radiation Tendency T                          (K/s)
    ttend_sso  ,    & ! SSO Tendency T                                (K/s)
    ttend_con  ,    & ! Shallow convection Tendency T                 (K/s)
    qvten_con  ,    & ! Shallow convection Tendency QV                (kg/kg/s)
    utend_sso  ,    & ! SSO Tendency U                                (m/s2)
    vtend_sso  ,    & ! SSO Tendency V                                (m/s2)
    ttend_tmp3 ,    & ! T tendency before turbulence scheme           (K/s)
    utend_tmp3 ,    & ! U tendency before turbulence scheme           (m/s2)
    vtend_tmp3 ,    & ! V tendency before turbulence scheme           (m/s2)
    qvten_tmp3 ,    & ! QV tendency before turbulence scheme          (kg/kg/s)
    ttend_tmp4 ,    & ! T tendency after turbulence scheme            (K/s)
    utend_tmp4 ,    & ! U tendency after turbulence scheme            (m/s2)
    vtend_tmp4 ,    & ! V tendency after turbulence scheme            (m/s2)
    qvten_tmp4 ,    & ! QV tendency after turbulence scheme           (kg/kg/s)
    ttend_tur2 ,    & ! T tendency turbulence scheme 2                (K/s)
    utend_tur2 ,    & ! U tendency turbulence scheme 2                (m/s2)
    vtend_tur2 ,    & ! V tendency turbulence scheme 2                (m/s2)
    qvten_tur2        ! QV tendency turbulence scheme 2               (kg/kg/s)
#endif

USE data_io,            ONLY:  ydate_ini, lbdclim, lana_qi, llb_qi, lana_qg, &
                               llb_qg, lana_qr_qs, llb_qr_qs

USE data_tracer,        ONLY:  T_ADV_OFF , T_ADV_ON  , T_DIFF_OFF, T_DIFF_ON, &
                               T_TURB_OFF, T_TURB_1D , T_CONV_OFF, T_CONV_ON, &
                               T_INI_ZERO, T_INI_FILE, T_LBC_ZERO, T_LBC_FILE,&
                               T_LBC_ZEROGRAD        , T_LBC_CST             ,&
                               T_BBC_ZEROFLUX        , T_BBC_ZEROVAL         ,&
                               T_BBC_SURF_VAL        , T_RELAX_OFF           ,&
                               T_RELAX_FULL          , T_RELAX_INFLOW        ,&
                               T_DAMP_OFF            , T_DAMP_ON             ,&
                               T_CLP_OFF             , T_CLP_ON              ,&
                               T_ERR_NOTFOUND        , T_TURB_3D

USE environment,        ONLY:  exchg_boundaries, comm_barrier, model_abort
USE parallel_utilities, ONLY:  distribute_values
USE time_utilities,     ONLY:  get_timings, i_precipitation, i_radiation,    &
                               i_turbulence, i_convection, i_soil_model,     &
                               i_communications_phy, i_barrier_waiting_phy,  &
                               i_add_computations, i_sso

USE src_gscp,           ONLY:  hydci_pp, hydci_pp_gr,  kessler_pp, hydor_pp, &
                               hydci
USE src_radiation,      ONLY:  init_radiation, organize_radiation
USE turbulence_utilities, ONLY: init_volume_canopy, init_surface_canopy
USE turbulence_interface, ONLY: organize_turbulence 
USE src_conv_tiedtke,   ONLY:  organize_conv_tiedtke
!not yet:  USE src_conv_bechtold,  ONLY:  organize_conv_becht
USE src_conv_shallow,   ONLY:  organize_conv_shallow
USE src_soil,           ONLY:  terra1, terra2
USE src_soil_multlay,   ONLY:  terra_multlay
USE src_flake,          ONLY:  flake_init, flake_interface
USE src_sso,            ONLY:  organize_sso
USE src_seaice,         ONLY:  seaice
USE src_artifdata,      ONLY:  tkvhfix, tkhhfix, tkvmfix, tkhmfix, &
                               lnosurffluxes_m, lnosurffluxes_h

USE src_tracer,         ONLY: trcr_new, trcr_errorstr, trcr_meta_define,      &
                              trcr_meta_set, trcr_get

#ifdef TWOMOM_SB
USE src_twomom_sb_interface,   ONLY:  seifert_pp
USE wolken_konstanten,         ONLY:  init_seifert
#endif

#ifdef CLM
USE src_conv_ifs,       ONLY:  organize_conv_ifs
#endif

!==============================================================================

IMPLICIT NONE

!==============================================================================
! Parameter list:
CHARACTER (LEN= *),       INTENT(IN)            ::                      &
  yaction      ! action to be performed

INTEGER (KIND=iintegers), INTENT(OUT)           ::                      &
  ierror       ! error status

CHARACTER (LEN= *),       INTENT(OUT)           ::                      &
  yerrmsg      ! error message

! Local variables: 
INTEGER (KIND=iintegers)   ::   izerrstat, izerr,                       &
                                nuin, izerror, nx, i, j, k, kzdims(24), &
                                izdebug, ist, izl, izic_qi, izlbc_qi, izic_qg,&
                                izlbc_qg, izic_qr, izlbc_qr, izic_qs,         &
                                izlbc_qs, izrelax_qr, izrelax_qs, izrelax_qg, &
                                izdamp_qv, izdamp_qc, izdamp_qi, izdamp_qr,   &
                                izdamp_qs, izdamp_qg, isp, izdamp_in

#ifdef TWOMOM_SB
INTEGER (KIND=iintegers)   ::   izdamp_qh, izic_qh, izlbc_qh
#endif

LOGICAL                    ::   lstfnct, & ! calculation of a new stability
                                           ! function in sub. turbdiff
                                lzconv     ! if convection is computed

CHARACTER (LEN=  9)        ::   yinput             ! Namelist INPUT file
CHARACTER (LEN=130)        ::   yzerror
CHARACTER (LEN= 25)        ::   yzroutine

REAL (KIND=wp)             ::   zdt, zdt_orig

! Tracer pointers:
REAL (KIND=wp),     POINTER:: &
  qv_tens(:,:,:) => NULL()  , &
#ifdef TEND
  qv_tens_tmp(:,:,:) => NULL()  , &
#endif
  qc_tens(:,:,:) => NULL()  , &
  qi_tens(:,:,:) => NULL()  , &
  ccn1  (:,:,:)=> NULL() ,&  ! CCN1 at itl
  ccn2  (:,:,:)=> NULL() ,&  ! CCN2 at itl
  ccn3  (:,:,:)=> NULL() ,&  ! CCN3 at itl
  ccn1_bd  (:,:,:,:)=> NULL() ,&  ! CCN1 at itl
  ccn2_bd  (:,:,:,:)=> NULL() ,&  ! CCN2 at itl
  ccn3_bd  (:,:,:,:)=> NULL()     ! CCN3 at itl

!ZD >>>>
!if (lINprog) then
REAL(KIND=wp),     POINTER :: &
  in_t1  (:,:,:)=> NULL() ,&  ! IN_T1 at itl
  in_t2  (:,:,:)=> NULL() ,&  !IN_T2 at itl
  in_t3  (:,:,:)=> NULL() ,&  !IN_T3 at itl
  in_t4  (:,:,:)=> NULL() ,&  !IN_T4 at itl
  in_t5  (:,:,:)=> NULL() ,&  !IN_T5 at itl
  in_t6  (:,:,:)=> NULL() ,&  !IN_T6 at itl
  in_t7  (:,:,:)=> NULL() ,&  !IN_T7 at itl
  in_t8  (:,:,:)=> NULL() ,&  !IN_T8 at itl
  in_t9  (:,:,:)=> NULL() ,&  !IN_T9 at itl
  in_t10 (:,:,:)=> NULL() ,&  !IN_T10 at itl
  in_t11 (:,:,:)=> NULL() ,&  !IN_T11 at itl
  in_t12 (:,:,:)=> NULL() ,&  !IN_T12 at itl
  in_t13 (:,:,:)=> NULL() ,&  !IN_T13 at itl
  in_t14 (:,:,:)=> NULL() ,&  !IN_T14 at itl
  in_t15 (:,:,:)=> NULL() ,&  !IN_T15 at itl
  in_t1_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T1
  in_t2_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T2
  in_t3_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T3
  in_t4_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T4
  in_t5_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T5
  in_t6_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T6
  in_t7_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T7
  in_t8_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T8
  in_t9_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T9
  in_t10_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T10
  in_t11_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T11
  in_t12_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T12
  in_t13_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T13
  in_t14_bd  (:,:,:,:)=> NULL() ,&  !BC for IN_T14
  in_t15_bd  (:,:,:,:)=> NULL()   !BC for IN_T15
!endif
!ZD <<<<

!------------------------------------------------------------------------------
!- End of header
!------------------------------------------------------------------------------
 
!------------------------------------------------------------------------------
!- Begin Subroutine organize_physics
!------------------------------------------------------------------------------

yzroutine = 'organize_physics'
ierror    = 0_iintegers
izerr     = 0_iintegers
izerrstat = 0_iintegers
lzconv    = .FALSE.
kzdims(:) = 0_iintegers

! Initialize, whether debug output shall be done
IF (lprintdeb_all) THEN
  izdebug = idbg_level
ELSE
  IF (my_cart_id == 0) THEN
    izdebug = idbg_level
  ELSE
    izdebug = 0
  ENDIF
ENDIF

!------------------------------------------------------------------------------
! Section 1: Input of the Namelist
!------------------------------------------------------------------------------

IF (yaction == 'input') THEN

  ! Open NAMELIST-INPUT file
  IF (my_world_id == 0) THEN
    IF (idbg_level > 0) THEN
      PRINT *,'    INPUT OF THE NAMELISTS FOR PHYSICS'
    ENDIF
    yinput   = 'INPUT_PHY'
    nuin     =  1
    OPEN(nuin   , FILE=yinput  , FORM=  'FORMATTED', STATUS='UNKNOWN',  &
         IOSTAT=izerrstat)
    IF(izerrstat /= 0) THEN
      yerrmsg  = ' ERROR    *** Error while opening file INPUT_PHY *** '
      ierror   = 2001
      RETURN
    ENDIF
  ENDIF

  ! Read all NAMELIST-groups
  CALL input_phyctl (nuspecif, nuin, izerrstat)

  IF (my_world_id == 0) THEN
    ! Close file for input of the NAMELISTS
    CLOSE (nuin    , STATUS='KEEP')
  ENDIF

  IF (izerrstat < 0) THEN
    yerrmsg = 'ERROR *** while reading NAMELIST Group /PHYCTL/ ***'
    ierror  = 2003
  ELSEIF (izerrstat > 0) THEN
    yerrmsg = 'ERROR *** Wrong values occured in NAMELIST INPUT_PHY ***'
    ierror  = 2004
  ENDIF

!------------------------------------------------------------------------------
! Section 1.5: Definition of required tracers by all physics modules
!------------------------------------------------------------------------------

ELSEIF (yaction == 'tracer') THEN

  ! init error value
  izerr = 0_iintegers

  ! Define all tracers required by the physics
  ! ------------------------------------------

  ! define QV
  IF (lspubc) THEN
    izdamp_qv = T_DAMP_ON
  ELSE
    izdamp_qv = T_DAMP_OFF
  ENDIF 

  CALL trcr_new(                                                              &
    ierr           = izerr,                                                   &
    yshort_name    = 'QV',                                                    &
    igribparam     = 51,                                                      &
    igribtable     = 1,                                                       &
    yparent        = 'organize_physics',                                      &
    yunits         = 'kg kg-1',                                               &
    ystandard_name = 'specific_humidity',                                     &
    ylong_name     = 'specific humidity',                                     &
    itype_adv      = T_ADV_ON,                                                &
    itype_diff     = T_DIFF_ON,                                               &
    itype_turbmix  = T_TURB_1D,                                               &
    itype_passconv = T_CONV_OFF,                                              &
    itype_ini      = T_INI_FILE,                                              &
    itype_lbc      = T_LBC_FILE,                                              &
    itype_bbc      = T_BBC_SURF_VAL,                                          &
    itype_relax    = T_RELAX_FULL,                                            & 
    itype_damp     = izdamp_qv,                                               &
    itype_clip     = T_CLP_ON,                                                &
    idx_trcr       = idt_qv)

  IF (izerr /= 0_iintegers) THEN
    ierror  = izerr
    yerrmsg = trcr_errorstr(izerr)
    RETURN
  ENDIF

  ! define QC
  IF (lspubc) THEN
    izdamp_qc = T_DAMP_ON
  ELSE
    izdamp_qc = T_DAMP_OFF
  ENDIF 

  CALL trcr_new(                                                              &
    ierr           = izerr,                                                   &
    yshort_name    = 'QC',                                                    &
    igribparam     = 31,                                                      &
    igribtable     = 2,                                                       &
    yparent        = 'organize_physics',                                      &
    yunits         = 'kg kg-1',                                               &
    ystandard_name = 'mass_fraction_of_cloud_liquid_water_in_air',            &
    ylong_name     = 'specific cloud liquid water content',                   &
    itype_adv      = T_ADV_ON,                                                &
    itype_diff     = T_DIFF_ON,                                               &
    itype_turbmix  = T_TURB_1D,                                               &
    itype_passconv = T_CONV_OFF,                                              &
    itype_ini      = T_INI_FILE,                                              &
    itype_lbc      = T_LBC_FILE,                                              &
    itype_bbc      = T_BBC_ZEROFLUX,                                          &
    itype_relax    = T_RELAX_FULL,                                            &
    itype_damp     = izdamp_qc,                                               &
    itype_clip     = T_CLP_ON,                                                &
    idx_trcr       = idt_qc)

  IF (izerr /= 0_iintegers) THEN
    ierror  = izerr
    yerrmsg = trcr_errorstr(izerr)
    RETURN
  ENDIF

  ! define QI 
  IF (lprog_qi) THEN
    IF (lspubc) THEN
      izdamp_qi = T_DAMP_ON
    ELSE
      izdamp_qi = T_DAMP_OFF
    ENDIF 
    IF (lana_qi) THEN
      izic_qi = T_INI_FILE
    ELSE
      izic_qi = T_INI_ZERO
    ENDIF
    IF (llb_qi) THEN
      izlbc_qi = T_LBC_FILE
    ELSE
      izlbc_qi = T_LBC_ZERO     ! this might be changed by the user 
                                ! to T_LBC_CST or T_LBC_ZEROGRAD
    ENDIF

    CALL trcr_new(                                                            &
      ierr           = izerr,                                                 &
      yshort_name    = 'QI',                                                  &
      igribparam     = 33,                                                    &
      igribtable     = 2,                                                     &
      yparent        = 'organize_physics',                                    &
      yunits         = 'kg kg-1',                                             &
      ystandard_name = 'mass_fraction_of_cloud_ice_in_air',                   &
      ylong_name     = 'specific cloud ice content',                          &
      itype_adv      = T_ADV_ON,                                              &
      itype_diff     = T_DIFF_OFF,                                            &
      itype_turbmix  = T_TURB_1D,                                             &
      itype_passconv = T_CONV_OFF,                                            &
      itype_ini      = izic_qi,                                               &
      itype_lbc      = izlbc_qi,                                              &
      itype_bbc      = T_BBC_ZEROFLUX,                                        &
      itype_relax    = T_RELAX_FULL,                                          &
      itype_damp     = izdamp_qi,                                             &
      itype_clip     = T_CLP_ON,                                              &
      idx_trcr       = idt_qi)

    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
  ENDIF

  ! define QG
  IF (itype_gscp >= 4) THEN
    IF (lspubc) THEN
      IF (itype_spubc == 1_iintegers) THEN
        izdamp_qg = T_DAMP_OFF
      ELSE
        izdamp_qg = T_DAMP_ON
      ENDIF
    ELSE
      izdamp_qg = T_DAMP_OFF
    ENDIF 
    IF (lana_qg) THEN
      izic_qg = T_INI_FILE
    ELSE
      izic_qg = T_INI_ZERO
    ENDIF

    IF (llb_qg) THEN
      izlbc_qg = T_LBC_FILE
      IF (itype_outflow_qrsg == 1) THEN
        izrelax_qg = T_RELAX_FULL
      ELSEIF (itype_outflow_qrsg == 2) THEN
        izrelax_qg = T_RELAX_INFLOW
      ENDIF
    ELSE
      izrelax_qg = T_RELAX_OFF
      IF (itype_lbc_qrsg == 1) THEN 
        izlbc_qg = T_LBC_ZEROGRAD 
      ELSEIF (itype_lbc_qrsg == 2) THEN
        izlbc_qg = T_LBC_ZERO
      ENDIF
    ENDIF
 
    CALL trcr_new(                                                            &
      ierr           = izerr,                                                 &
      yshort_name    = 'QG',                                                  &
      igribparam     = 39,                                                    &
      igribtable     = 2,                                                     &
      yparent        = 'organize_physics',                                    &
      yunits         = 'kg kg-1',                                             &
      ystandard_name = 'mass_fraction_of_graupel_in_air',                     &
      ylong_name     = 'specific graupel content',                            &
      itype_adv      = T_ADV_ON,                                              &
      itype_diff     = T_DIFF_OFF,                                            &
      itype_turbmix  = T_TURB_OFF,                                            &
      itype_passconv = T_CONV_OFF,                                            &
      itype_ini      = izic_qg,                                               &
      itype_lbc      = izlbc_qg,                                              &
      itype_bbc      = T_BBC_ZEROFLUX,                                        &
      itype_relax    = izrelax_qg,                                            &
      itype_damp     = izdamp_qg,                                             &
      itype_clip     = T_CLP_ON,                                              &
      idx_trcr       = idt_qg)

    ! check for errors
    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
  ENDIF

  ! define QR 
  IF (lspubc) THEN
    IF (itype_spubc == 1_iintegers) THEN
      izdamp_qr = T_DAMP_OFF
    ELSE
      izdamp_qr = T_DAMP_ON
    ENDIF
  ELSE
    izdamp_qr = T_DAMP_OFF
  ENDIF 
  IF (lana_qr_qs) THEN
    izic_qr = T_INI_FILE
  ELSE
    izic_qr = T_INI_ZERO
  ENDIF
  IF (llb_qr_qs) THEN
    izlbc_qr = T_LBC_FILE
    IF (itype_outflow_qrsg == 1) THEN
      izrelax_qr = T_RELAX_FULL
    ELSEIF (itype_outflow_qrsg == 2) THEN
      izrelax_qr = T_RELAX_INFLOW
    ENDIF
  ELSE
    izrelax_qr = T_RELAX_OFF
    IF (itype_lbc_qrsg == 1) THEN 
      izlbc_qr = T_LBC_ZEROGRAD 
    ELSEIF (itype_lbc_qrsg == 2) THEN
      izlbc_qr = T_LBC_ZERO
    ENDIF
  ENDIF

  CALL trcr_new(                                                              &
    ierr           = izerr,                                                   &
    yshort_name    = 'QR',                                                    &
    igribparam     = 35,                                                      &
    igribtable     = 2,                                                       &
    yparent        = 'organize_physics',                                      &
    yunits         = 'kg kg-1',                                               &
    ystandard_name = 'mass_fraction_of_rain_in_air',                          &
    ylong_name     = 'specific rain content',                                 &
    itype_adv      = T_ADV_ON,                                                &
    itype_diff     = T_DIFF_OFF,                                              &
    itype_turbmix  = T_TURB_OFF,                                              &
    itype_passconv = T_CONV_OFF,                                              &
    itype_ini      = izic_qr,                                                 &
    itype_lbc      = izlbc_qr,                                                &
    itype_bbc      = T_BBC_ZEROFLUX,                                          &
    itype_relax    = izrelax_qr,                                              &
    itype_damp     = izdamp_qr,                                               &
    itype_clip     = T_CLP_ON,                                                &
    idx_trcr       = idt_qr)

  IF (izerr /= 0_iintegers) THEN
    ierror  = izerr
    yerrmsg = trcr_errorstr(izerr)
    RETURN
  ENDIF

  ! define QS
  IF (itype_gscp > 1_iintegers) THEN
    IF (lspubc) THEN
      IF (itype_spubc == 1_iintegers) THEN
        izdamp_qs = T_DAMP_OFF
      ELSE
        izdamp_qs = T_DAMP_ON
      ENDIF
    ELSE
      izdamp_qs = T_DAMP_OFF
    ENDIF 
    IF (lana_qr_qs) THEN
      izic_qs = T_INI_FILE
    ELSE
      izic_qs = T_INI_ZERO
    ENDIF
    IF (llb_qr_qs) THEN
      izlbc_qs = T_LBC_FILE
      IF (itype_outflow_qrsg == 1) THEN
        izrelax_qs = T_RELAX_FULL
      ELSEIF (itype_outflow_qrsg == 2) THEN
        izrelax_qs = T_RELAX_INFLOW
      ENDIF
    ELSE
      izrelax_qs = T_RELAX_OFF
      IF (itype_lbc_qrsg == 1) THEN 
        izlbc_qs = T_LBC_ZEROGRAD 
      ELSEIF (itype_lbc_qrsg == 2) THEN
        izlbc_qs = T_LBC_ZERO
      ENDIF
    ENDIF

    CALL trcr_new(                                                            &
      ierr           = izerr,                                                 &
      yshort_name    = 'QS',                                                  &
      igribparam     = 36,                                                    &
      igribtable     = 2,                                                     &
      yparent        = 'organize_physics',                                    &
      yunits         = 'kg kg-1',                                             &
      ystandard_name = 'mass_fraction_of_snow_in_air',                        &
      ylong_name     = 'specific snow content',                               &
      itype_adv      = T_ADV_ON,                                              &
      itype_diff     = T_DIFF_OFF,                                            &
      itype_turbmix  = T_TURB_OFF,                                            &
      itype_passconv = T_CONV_OFF,                                            &
      itype_ini      = izic_qs,                                               &
      itype_lbc      = izlbc_qs,                                              &
      itype_bbc      = T_BBC_ZEROFLUX,                                        &
      itype_relax    = izrelax_qs,                                            &
      itype_damp     = izdamp_qs,                                             &
      itype_clip     = T_CLP_ON,                                              &
      idx_trcr       = idt_qs)

    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
  ENDIF

#ifdef TWOMOM_SB
  IF ( itype_gscp >= 100 ) THEN

    ! define QNCLOUD
   
    ! Overtake value of
    !   izdamp_qc
    ! from the definition of QC above to do consistent things!
    
    CALL trcr_new(                                                                 &
         ierr           = izerr,                                                   &
         yshort_name    = 'QNCLOUD',                                               &
         igribparam     = 221,                                                     &
         igribtable     = 2,                                                       &
         yparent        = 'organize_physics',                                      &
         yunits         = 'kg-1',                                                  &
         ystandard_name = 'specif_number_of_cloud_droplets_in_air',                &
         ylong_name     = 'specific cloud droplet number',                         &
         itype_adv      = T_ADV_ON,                                                &
         itype_diff     = T_DIFF_ON,                                               &
         itype_turbmix  = T_TURB_3D,                                               &
         itype_passconv = T_CONV_OFF,                                              &
         itype_ini      = T_INI_FILE,                                              &
         itype_lbc      = T_LBC_FILE,                                              &
         itype_bbc      = T_BBC_ZEROFLUX,                                          &
         itype_relax    = T_RELAX_FULL,                                            &
         itype_damp     = izdamp_qc,                                               &
         itype_clip     = T_CLP_ON,                                                &
         idx_trcr       = idt_qnc)

    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF

    ! define QNICE 
    ! ------------

    IF (lprog_qi) THEN

      ! Overtake values of
      !   izdamp_qi
      !   izic_qi
      !   izlbc_qi
      ! from the definition of QI above to do consistent things!

      CALL trcr_new(                                                               &
           ierr           = izerr,                                                 &
           yshort_name    = 'QNICE',                                               &
           igribparam     = 223,                                                   &
           igribtable     = 2,                                                     &
           yparent        = 'organize_physics',                                    &
           yunits         = 'kg-1',                                                &
           ystandard_name = 'specif_number_of_cloud_ice_in_air',                   &
           ylong_name     = 'specific cloud ice number content',                   &
           itype_adv      = T_ADV_ON,                                              &
           itype_diff     = T_DIFF_OFF,                                            &
           itype_turbmix  = T_TURB_3D,                                             &
           itype_passconv = T_CONV_OFF,                                            &
           itype_ini      = izic_qi,                                               &
           itype_lbc      = izlbc_qi,                                              &
           itype_bbc      = T_BBC_ZEROFLUX,                                        &
           itype_relax    = T_RELAX_FULL,                                          &
           itype_damp     = izdamp_qi,                                             &
           itype_clip     = T_CLP_ON,                                              &
           idx_trcr       = idt_qni)
 
      IF (izerr /= 0_iintegers) THEN
        ierror  = izerr
        yerrmsg = trcr_errorstr(izerr)
        RETURN
      ENDIF

    ENDIF

    ! define QNRAIN
    ! -------------

    ! Overtake values of
    !    izdamp_qr
    !    izic_qr
    !    izlbc_qr
    !    izrelax_qr
    ! from the definition of QR above to do consistent things!

    CALL trcr_new(                                                                 &
         ierr           = izerr,                                                   &
         yshort_name    = 'QNRAIN',                                                &
         igribparam     = 222,                                                     &
         igribtable     = 2,                                                       &
         yparent        = 'organize_physics',                                      &
         yunits         = 'kg-1',                                                  &
         ystandard_name = 'specif_number_of_rain_drops_in_air',                    &
         ylong_name     = 'specific rain drop number',                             &
         itype_adv      = T_ADV_ON,                                                &
         itype_diff     = T_DIFF_OFF,                                              &
         itype_turbmix  = T_TURB_OFF,                                              &
         itype_passconv = T_CONV_OFF,                                              &
         itype_ini      = izic_qr,                                                 &
         itype_lbc      = izlbc_qr,                                                &
         itype_bbc      = T_BBC_ZEROFLUX,                                          &
         itype_relax    = izrelax_qr,                                              &
         itype_damp     = izdamp_qr,                                               &
         itype_clip     = T_CLP_ON,                                                &
         idx_trcr       = idt_qnr)

    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF

    ! define QNSNOW
    ! -------------

    ! Overtake values of
    !    izdamp_qg
    !    izic_qs
    !    izlbc_qs
    !    izrelax_qs
    ! from the definition of QS above to do consistent things!

    CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'QNSNOW',                                              &
         igribparam     = 224,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'kg-1',                                                &
         ystandard_name = 'specif_number_of_snow_flakes_in_air',                 &
         ylong_name     = 'specific snow flake number',                          &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_OFF,                                            &
         itype_turbmix  = T_TURB_OFF,                                            &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = izic_qs,                                               &
         itype_lbc      = izlbc_qs,                                              &
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = izrelax_qs,                                            &
         itype_damp     = izdamp_qs,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_qns)

    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF

    ! define QNGRAUPEL
    ! ------------

    ! Overtake values of
    !    izdamp_qg
    !    izic_qg
    !    izlbc_qg
    !    izrelax_qg
    ! from the definition of QG above to do consistent things!

    CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'QNGRAUPEL',                                           &
         igribparam     = 225,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'kg-1',                                                &
         ystandard_name = 'specif_number_of_graupel_in_air',                     &
         ylong_name     = 'specific graupel number',                             &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_OFF,                                            &
         itype_turbmix  = T_TURB_OFF,                                            &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = izic_qg,                                               &
         itype_lbc      = izlbc_qg,                                              &
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = izrelax_qg,                                            &
         itype_damp     = izdamp_qg,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_qng)

    ! check for errors
    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF

    ! define QH (in any case, not only for itype_gscp >= 2000)
    ! --------------------------------------------------------

    CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'QH',                                                  &
         igribparam     = 98,                                                    &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'kg kg-1',                                             &
         ystandard_name = 'mass_fraction_of_hail_in_air',                        &
         ylong_name     = 'specific hail content',                               &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_OFF,                                            &
         itype_turbmix  = T_TURB_OFF,                                            &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            &
         itype_lbc      = T_LBC_ZERO,                                            &
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_OFF,                                           &
         itype_damp     = T_DAMP_OFF,                                            &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_qh)

    ! check for errors
    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF

    ! define QNHAIL
    ! -------------

    ! Overtake values of
    !    izdamp_qg
    !    izic_qg
    !    izlbc_qg
    !    izrelax_qg
    ! from the definition of QG above to do consistent things!

    CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'QNHAIL',                                              &
         igribparam     = 226,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'kg-1',                                                &
         ystandard_name = 'specif_number_of_hail_in_air',                        &
         ylong_name     = 'specific hail number',                                &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_OFF,                                            &
         itype_turbmix  = T_TURB_OFF,                                            &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            &
         itype_lbc      = T_LBC_ZERO,                                            &
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_OFF,                                           &
         itype_damp     = T_DAMP_OFF,                                            &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_qnh)

    ! check for errors
    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF


  END IF
#endif

  if (lINprog) then
    IF (lspubc) THEN
      izdamp_in = T_DAMP_ON
    ELSE
      izdamp_in = T_DAMP_OFF
    ENDIF
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T1',                                               &
         igribparam     = 202,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T1_bin',                   &
         ylong_name     = 'IN number concentration T1 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                             &
         idx_trcr       = idt_T1)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T2',                                               &
         igribparam     = 205,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T2_bin',                   &
         ylong_name     = 'IN number concentration T2 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T2)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T3',                                               &
         igribparam     = 206,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T3_bin',                   &
         ylong_name     = 'IN number concentration T3 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T3)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T4',                                               &
         igribparam     = 207,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T4_bin',                   &
         ylong_name     = 'IN number concentration T4 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T4)
     CALL trcr_new(                                                              &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T5',                                               &
         igribparam     = 208,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T5_bin',                   &
         ylong_name     = 'IN number concentration T5 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T5)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T6',                                               &
         igribparam     = 220,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T6_bin',                   &
         ylong_name     = 'IN number concentration T6 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T6)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T7',                                               &
         igribparam     = 227,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T7_bin',                   &
         ylong_name     = 'IN number concentration T7 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T7)
     CALL trcr_new(                                                              &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T8',                                               &
         igribparam     = 229,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T8_bin',                   &
         ylong_name     = 'IN number concentration T8 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T8)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T9',                                               &
         igribparam     = 234,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T9_bin',                   &
         ylong_name     = 'IN number concentration T9 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T9)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T10',                                               &
         igribparam     = 235,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T10_bin',                   &
         ylong_name     = 'IN number concentration T10 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T10)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T11',                                               &
         igribparam     = 245,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T11_bin',                   &
         ylong_name     = 'IN number concentration T11 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T11)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T12',                                               &
         igribparam     = 246,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T12_bin',                   &
         ylong_name     = 'IN number concentration T12 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T12)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T13',                                               &
         igribparam     = 247,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T13_bin',                   &
         ylong_name     = 'IN number concentration T13 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T13)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T14',                                               &
         igribparam     = 248,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T14_bin',                   &
         ylong_name     = 'IN number concentration T14 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T14)
     CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'IN_T15',                                               &
         igribparam     = 249,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'IN_concentration_T15_bin',                   &
         ylong_name     = 'IN number concentration T15 bin',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_ON,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = idt_T15)
  endif



  if(lCCNprog) then
    IF (lspubc) THEN
      izdamp_in = T_DAMP_ON
    ELSE
      izdamp_in = T_DAMP_OFF
    ENDIF
    CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'CCN_1',                                               &
         igribparam     = 242,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'N_aero mode 1',                   &
         ylong_name     = 'N_aero concentration first lognormal mode',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_OFF,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = iccn1)
    CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'CCN_2',                                               &
         igribparam     = 243,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'N_aero mode 2',                   &
         ylong_name     = 'N_aero concentration second lognormal mode',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_OFF,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = iccn2)
    CALL trcr_new(                                                               &
         ierr           = izerr,                                                 &
         yshort_name    = 'CCN_3',                                               &
         igribparam     = 244,                                                   &
         igribtable     = 2,                                                     &
         yparent        = 'organize_physics',                                    &
         yunits         = 'm-3',                                                &
         ystandard_name = 'N_aero mode 3',                   &
         ylong_name     = 'N_aero concentration third lognormal mode',                   &
         itype_adv      = T_ADV_ON,                                              &
         itype_diff     = T_DIFF_OFF,                                            &
         itype_turbmix  = T_TURB_3D,                                             &
         itype_passconv = T_CONV_OFF,                                            &
         itype_ini      = T_INI_ZERO,                                            & !T_INI_FILE alternative
         itype_lbc      = T_LBC_ZERO,                                            & !T_LBC_FILE alternative
         itype_bbc      = T_BBC_ZEROFLUX,                                        &
         itype_relax    = T_RELAX_FULL,                                          &
         itype_damp     = izdamp_in,                                             &
         itype_clip     = T_CLP_ON,                                              &
         idx_trcr       = iccn3)
  endif

  ! Define and set all additional metadata required by the microphysics
  ! -------------------------------------------------------------------

  ! This list of metadata is considered as a list of "hacks".
  ! It is not recommended to use them for other tracers.

  ! CLP_10E-12: Remove small values after turbulent mixing in slow_tendencies()
  !             in slow_tendencies.f90 for LF core to reproduce original
  !             behaviour of QI
  !             NOTE: this is only active if T_CLP is set to T_CLP_ON
  !             NOTE: this cannot be switched on at the same time as MASSFLX_CLP
  IF ( .NOT. l2tls ) THEN
    CALL trcr_meta_define(izerr, 'CLP_10E-12', .FALSE. )
    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qi, 'CLP_10E-12', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
  ENDIF

  ! ADD_CLP_ADV: Additional clipping in advection_pd() in src_advection_rk.f90
  !              in RK core to reproduce original behaviour of QG, QR, QS
  !              but it is not clear why this is not done for QV, QC, QI
  !              NOTE: this is only active if T_ADV is set to T_ADV_ON
  IF ( l2tls ) THEN
    CALL trcr_meta_define(izerr, 'ADD_CLP_ADV', .FALSE.)
    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qr, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qs, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qg, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
#ifdef TWOMOM_SB
    CALL trcr_meta_set(izerr, idt_qh, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnc, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnr, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qni, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qns, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qng, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnh, 'ADD_CLP_ADV', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
#endif
  ENDIF

  ! BD_0GRAD_FORCED: Force a zero-gradend boundary condition in advection_pd() in 
  !                  src_advection_rk.f90 even if zero-value boundary condition
  !                  has been chosen. This is required, since QR, QS, QG and QI
  !                  have a zero-grad BC there (if the corresponding llb_qx switch
  !                  is set to False) even if the user has chosen itype_lbc_qrsg=2
  !                  which would imply a zero-value BC. This switch is only required
  !                  to reproduce the original behaviour of the RK core.
  IF ( l2tls ) THEN
    CALL trcr_meta_define(izerr, 'BD_0GRAD_FORCED', .FALSE.)
    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qi, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qr, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qs, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qg, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
#ifdef TWOMOM_SB
    CALL trcr_meta_set(izerr, idt_qh, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnc, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnr, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qni, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qns, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qng, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnh, 'BD_0GRAD_FORCED', .TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
#endif
  ENDIF

  ! SP_ADV_LF: Define advection schemes and time stepping scheme combinations
  !            in case of LF core (src_leapfrog.f90, slow_tendencies.f90,
  !            src_relaxation.f90)
  !            1: hadv_cd2 (centered differences, LF time step)
  !               and implicit advection in the vertical
  !            2: hadv_pd (positive definite, Euler forward)
  !               and vadv_pd (explicit vertical advection, positive definite)
  !            3: interpol_sl_trilin (Semi-Lagrange, LF timestep)
  ! WARNING: SP_ADV_LF must be 1 (default) for QV !!!
  ! WARNING: SP_ADV_LF = 2 or 3 is not compatible with T_ADV_OFF !!!

  IF ( .NOT. l2tls ) THEN
    CALL trcr_meta_define(izerr, 'SP_ADV_LF', 1_iintegers)
    IF (izerr /= 0_iintegers) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qi, 'SP_ADV_LF', 2_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qr, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qs, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qg, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
#ifdef TWOMOM_SB
    CALL trcr_meta_set(izerr, idt_qh, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnc, 'SP_ADV_LF', 2_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnr, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qni, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qns, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qng, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnh, 'SP_ADV_LF', 3_iintegers)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
#endif
  ENDIF

  ! BD_SET_FORCED: Force application of a boundary condition (zero gradiend or zero
  !                value) after dynamics also if boundary data from a file is available
  !                (set_trcr_special_bc() in lmorg.f90). This is to reproduce the original
  !                behaviour of QR, QS, QG.
  !                0 : do not do a forced application of a boundary condition
  !                1 : do a forced boundary condition with zero gradiend
  !                2 : do a forced boundary condition with zero value
  CALL trcr_meta_define(izerr, 'BD_SET_FORCED', 0_iintegers)
  IF (izerr /= 0_iintegers) THEN
    ierror  = izerr
    yerrmsg = trcr_errorstr(izerr)
    RETURN
  ENDIF
  IF (itype_lbc_qrsg == 1 .OR. itype_lbc_qrsg == 2) THEN
    CALL trcr_meta_set(izerr, idt_qr, 'BD_SET_FORCED', itype_lbc_qrsg)   
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qs, 'BD_SET_FORCED', itype_lbc_qrsg)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qg, 'BD_SET_FORCED', itype_lbc_qrsg)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
#ifdef TWOMOM_SB
    CALL trcr_meta_set(izerr, idt_qh, 'BD_SET_FORCED', itype_lbc_qrsg)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnr, 'BD_SET_FORCED', itype_lbc_qrsg)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qns, 'BD_SET_FORCED', itype_lbc_qrsg)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qng, 'BD_SET_FORCED', itype_lbc_qrsg)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    CALL trcr_meta_set(izerr, idt_qnh, 'BD_SET_FORCED', itype_lbc_qrsg)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
#endif
  ENDIF

  ! MASSFLX_CLP: Massflux correction clipping in both LF and RK core (after
  !              turbulent diffusion of tracers in slow_tendencies.f90 and
  !              src_slow_tendencies_rk.f90). This is to reproduce the original
  !              behaviour of QV and QC.
  !              NOTE: this is only active if T_CLP is set to T_CLP_ON
  !              NOTE: this cannot be switched on at the same time as CLP_10E-12
  CALL trcr_meta_define(izerr, 'MASSFLX_CLP', .FALSE.)
  IF (izerr /= 0_iintegers) THEN
    ierror  = izerr
    yerrmsg = trcr_errorstr(izerr)
    RETURN
  ENDIF
  CALL trcr_meta_set(izerr, idt_qv, 'MASSFLX_CLP', .TRUE.)
  IF (izerr /= 0_iintegers) THEN
    ierror  = izerr
    yerrmsg = trcr_errorstr(izerr)
    RETURN
  ENDIF
  CALL trcr_meta_set(izerr, idt_qc, 'MASSFLX_CLP', .TRUE.)
  IF (izerr /= 0_iintegers) THEN
    ierror  = izerr
    yerrmsg = trcr_errorstr(izerr)
    RETURN
  ENDIF

  ! DAMP_FORCED: Damping forced for zerogradient lateral boundary
  !              conditions in case of itype_spubc=2
  CALL trcr_meta_define(izerr, 'DAMP_FORCED', .FALSE.)
  IF (izerr /= 0_iintegers) THEN
    ierror  = izerr
    yerrmsg = trcr_errorstr(izerr)
    RETURN
  ENDIF

  IF ( lspubc .AND. itype_spubc==2 .AND. .NOT. llb_qg                         &
      .AND. itype_lbc_qrsg == 1 ) THEN
    CALL trcr_meta_set(izerr, idt_qg, 'DAMP_FORCED',.TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    IF (izdebug > 1_iintegers) THEN
      PRINT *,  ''
      PRINT *,  '  *** WARNING: Damping is forced for QG in case of zero-gradient ***'
      PRINT *,  '  (The specification of itype_damp in sd out may not be correct for QG)'
      PRINT *,  ''
    ENDIF
#ifdef TWOMOM_SB
    IF ( itype_gscp >= 100 ) THEN
      CALL trcr_meta_set(izerr, idt_qh, 'DAMP_FORCED',.TRUE.)
      IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
        ierror  = izerr
        yerrmsg = trcr_errorstr(izerr)
        RETURN
      ENDIF
      IF (izdebug > 1_iintegers) THEN
        PRINT *,  ''
        PRINT *,  '  *** WARNING: Damping is forced for QH in case of zero-gradient ***'
        PRINT *,  '  (The specification of itype_damp in sd out may not be correct for QH)'
        PRINT *,  ''
      ENDIF
      CALL trcr_meta_set(izerr, idt_qng, 'DAMP_FORCED',.TRUE.)
      IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
        ierror  = izerr
        yerrmsg = trcr_errorstr(izerr)
        RETURN
      ENDIF
      IF (izdebug > 1_iintegers) THEN
        PRINT *,  ''
        PRINT *,  '  *** WARNING: Damping is forced for QNGRAUPEL in case of zero-gradient ***'
        PRINT *,  '  (The specification of itype_damp in sd out may not be correct for QNGRAUPEL)'
        PRINT *,  ''
      ENDIF
    ENDIF
#endif
  ENDIF

  IF ( lspubc .AND. itype_spubc==2 .AND. .NOT. llb_qr_qs                      &
      .AND. itype_lbc_qrsg == 1 ) THEN
    CALL trcr_meta_set(izerr, idt_qr, 'DAMP_FORCED',.TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    IF (izdebug > 1_iintegers) THEN
      PRINT *,  ''
      PRINT *,  '  *** WARNING: Damping is forced for QR in case of zero-gradient ***'
      PRINT *,  '  (The specification of itype_damp in sd out may not be correct for QR)'
      PRINT *,  ''
    ENDIF
    CALL trcr_meta_set(izerr, idt_qs, 'DAMP_FORCED',.TRUE.)
    IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
      ierror  = izerr
      yerrmsg = trcr_errorstr(izerr)
      RETURN
    ENDIF
    IF (izdebug > 1_iintegers) THEN
      PRINT *,  ''
      PRINT *,  '  *** WARNING: Damping is forced for QS in case of zero-gradient ***'
      PRINT *,  '  (The specification of itype_damp in sd out may not be correct for QS)'
      PRINT *,  ''
    ENDIF
#ifdef TWOMOM_SB
    IF ( itype_gscp >= 100 ) THEN
      CALL trcr_meta_set(izerr, idt_qnr, 'DAMP_FORCED',.TRUE.)
      IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
        ierror  = izerr
        yerrmsg = trcr_errorstr(izerr)
        RETURN
      ENDIF
      IF (izdebug > 1_iintegers) THEN
        PRINT *,  ''
        PRINT *,  '  *** WARNING: Damping is forced for QNRAIN in case of zero-gradient ***'
        PRINT *,  '  (The specification of itype_damp in sd out may not be correct for QNRAIN)'
        PRINT *,  ''
      ENDIF
      CALL trcr_meta_set(izerr, idt_qns, 'DAMP_FORCED',.TRUE.)
      IF (izerr /= 0_iintegers .AND. izerr /= T_ERR_NOTFOUND) THEN
        ierror  = izerr
        yerrmsg = trcr_errorstr(izerr)
        RETURN
      ENDIF
      IF (izdebug > 1_iintegers) THEN
        PRINT *,  ''
        PRINT *,  '  *** WARNING: Damping is forced for QNSNOW in case of zero-gradient ***'
        PRINT *,  '  (The specification of itype_damp in sd out may not be correct for QNSNOW)'
        PRINT *,  ''
      ENDIF
    ENDIF
#endif
  ENDIF

#ifdef TWOMOM_SB
  ! Initialize hydrometeor parameters for the 2-Moment cloud microphysical
  ! scheme of Seifert & Beheng
  IF (itype_gscp >= 100 ) THEN
    CALL init_seifert(itype_gscp)
  END IF

  ! ONLY EFFECTIVE IF itype_gscp >= 100:
  ! Determine if all the satads should be done
  ! (like for the 1-moment schemes), not just the
  ! satad after the microphysics at the end of the timestep:
  l2mom_satads = .FALSE.

  ! However, l2mom_do_extra_satads = .TRUE. can only be used
  ! for itype_gscp = ??[6-9]? (e.g., 2767, but not 2737),
  ! i.e., for the Segal-Khain parameterization of cloud nucleation.
  ! For the other cloud nucleation types, we need supersaturation
  ! after the dynamics.
  IF (l2mom_satads .AND. itype_gscp >= 100) THEN
    IF (MODULO(itype_gscp/10,10) < 6) THEN
      yerrmsg = 'ERROR ** l2mom_satads=.TRUE. only possible for itype_gscp=??[6-9]? **'
      ierror  = 2005
    END IF
  END IF
#endif

!------------------------------------------------------------------------------
! Section 2: Initialization of the packages for the first call
!------------------------------------------------------------------------------

ELSEIF (yaction == 'init') THEN

  IF (izdebug > 0) THEN
    PRINT *, '    PHYSICAL PACKAGES'
  ENDIF

  ! Initialize values for the radiation and the canopy fields
  IF (lrad) CALL init_radiation
  IF (lco2_stab) THEN
    IF (izdebug > 1) THEN
      WRITE (*,'(A44,I4,A4)') '           stabilised GHG forcing from year ', iy_co2_stab, ' on!'
    ENDIF
  ENDIF

  IF (ltur .OR. lsoil) THEN
     ! Determine the index of the highest layer which still is a canopy layer

     ! h_can is a primary external parameter. The initial values of h_can are 0.
     ! If we don't change this, no canopy will be resolved in the vertical direction.
     ! Because there is no h_can available at the moment, the next lines 
     ! are not in affect

     kcm=ke
     DO WHILE (MAXVAL( h_can - hhl(:,:,kcm) + hhl(:,:,ke1) ) > 0.0_wp)
        kcm=kcm-1
     END DO

     ! Now kcm points to the lowest layer being not a canopy layer.
     ! Therefore add +1
     kcm=kcm+1

     ! Allocate the canopy fields c_big, c_sml, r_air:
     ist = 0
     izl = 0
     ALLOCATE ( c_big(ie,je,kcm:ke1), STAT=izl ); c_big = 0.0_wp; ist = ist+izl
     ALLOCATE ( c_sml(ie,je,kcm:ke1), STAT=izl ); c_sml = 0.0_wp; ist = ist+izl
     ALLOCATE ( r_air(ie,je,kcm:ke1), STAT=izl ); r_air = 0.0_wp; ist = ist+izl

     IF (ist /= 0) THEN
       yerrmsg = 'ERROR *** Allocation of canopy fields failed ***'
       ierror  = 2014
       RETURN
     ENDIF

     CALL init_volume_canopy  (ie, je, ke, ke1, kcm,                   &
                               istartpar, iendpar, jstartpar, jendpar, &
                               fr_land, d_pat, c_big, c_sml, r_air)
     CALL init_surface_canopy (ie, je, itype_tran,                     &
                               istartpar, iendpar, jstartpar, jendpar, &
                               fr_land, plcov, lai, sai, tai, eai)
  ENDIF

!AP>>
!initialise the CCN tracers
if (lCCNprog) then
  CALL trcr_get(ierror, iccn1, ptr_tlev = nnew, ptr = ccn1, ptr_bd=ccn1_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, iccn2, ptr_tlev = nnew, ptr = ccn2, ptr_bd=ccn2_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, iccn3, ptr_tlev = nnew, ptr = ccn3, ptr_bd=ccn3_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  call readCCNconc(ccn1,ccn2,ccn3,ie,je,ke)
  ccn1_bd = CCN_1_N
  ccn2_bd = CCN_2_N
  ccn3_bd = CCN_3_N
endif

!ZD>>>>
if (lINprog) then
  CALL trcr_get(ierror, idt_T1, ptr_tlev=nnew, ptr=in_t1, ptr_bd=in_t1_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T2, ptr_tlev = nnew, ptr = in_t2, ptr_bd=in_t2_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T3, ptr_tlev = nnew, ptr = in_t3, ptr_bd=in_t3_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T4, ptr_tlev = nnew, ptr = in_t4, ptr_bd=in_t4_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T5, ptr_tlev = nnew, ptr = in_t5, ptr_bd=in_t5_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T6, ptr_tlev = nnew, ptr = in_t6, ptr_bd=in_t6_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T7, ptr_tlev = nnew, ptr = in_t7, ptr_bd=in_t7_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T8, ptr_tlev = nnew, ptr = in_t8, ptr_bd=in_t8_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T9, ptr_tlev = nnew, ptr = in_t9, ptr_bd=in_t9_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T10, ptr_tlev = nnew, ptr = in_t10, ptr_bd=in_t10_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T11, ptr_tlev = nnew, ptr = in_t11, ptr_bd=in_t11_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T12, ptr_tlev = nnew, ptr = in_t12, ptr_bd=in_t12_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T13, ptr_tlev = nnew, ptr = in_t13, ptr_bd=in_t13_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T14, ptr_tlev = nnew, ptr = in_t14, ptr_bd=in_t14_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_T15, ptr_tlev = nnew, ptr = in_t15, ptr_bd=in_t15_bd)
  IF (ierror /= 0_iintegers .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF

  CALL readINconc(in_t1, in_t2, in_t3, in_t4, in_t5, in_t6, in_t7, in_t8, in_t9, &
                      in_t10, in_t11, in_t12, in_t13, in_t14, in_t15, ie, je, ke)
  
  in_t1_bd(:,:,:,1) = in_t1(:,:,:)
  in_t2_bd(:,:,:,1) = in_t2(:,:,:)
  in_t3_bd(:,:,:,1) = in_t3(:,:,:)
  in_t4_bd(:,:,:,1) = in_t4(:,:,:)
  in_t5_bd(:,:,:,1) = in_t5(:,:,:)
  in_t6_bd(:,:,:,1) = in_t6(:,:,:)
  in_t7_bd(:,:,:,1) = in_t7(:,:,:)
  in_t8_bd(:,:,:,1) = in_t8(:,:,:)
  in_t9_bd(:,:,:,1) = in_t9(:,:,:)
  in_t10_bd(:,:,:,1) = in_t10(:,:,:)
  in_t11_bd(:,:,:,1) = in_t11(:,:,:)
  in_t12_bd(:,:,:,1) = in_t12(:,:,:)
  in_t13_bd(:,:,:,1) = in_t13(:,:,:)
  in_t14_bd(:,:,:,1) = in_t14(:,:,:)
  in_t15_bd(:,:,:,1) = in_t15(:,:,:)

endif
!ZD<<<<

!AP<<

  ! initialize the time stepping for the TKE 
  ! (necessary in any case, but absolutely for restarts!!)
  IF (nstart == 0) THEN
     ntke  = 0
! do not recompute, but read it from restart-file in src_input
! ELSE
!   IF (l2tls) THEN
!     ntke = nnew  !???
!   ELSE
!     ntke = nnow  !???
!   ENDIF
  ENDIF

  !Initialize varaiables of the lake model FLake
  IF (llake) CALL flake_init

  ! Diagnostic initialisation of prognostic rain / snow
  IF (ldiniprec) THEN
    ! set the timestep for the initialization phase
    zdt_orig = dt
    IF (.NOT. l2tls) THEN
      ! for Leapfrog only dt/2 is used in the first step
      dt = 0.5_wp * dt
    ENDIF
    dt2 = 2.0_wp * dt

    CALL hydci ( ldiniprec, yzerror, izerr )
    IF (izerr /= 0_iintegers) THEN
      ierror  = 2021
      yerrmsg = yzerror
    ENDIF

    ! reset the timestep again
    IF (.NOT. l2tls) THEN
      dt = zdt_orig
    ENDIF
    dt2 = 2.0_wp * dt
  ENDIF

!------------------------------------------------------------------------------
! Section 3: Physical packages at the beginning of the time stepping
!------------------------------------------------------------------------------

ELSEIF (yaction == 'compute') THEN

  ! get the correct timelevel
  IF (l2tls) THEN
    nx = nnow
    zdt = dt
  ELSE
    nx = nold
    zdt = dt2
  ENDIF

  IF (lbdclim) THEN
    ! the canopy layer has to be initialized every step, because the
    ! leaf area index, the plant cover and the root depth are changing
    IF (ltur .OR. lsoil)                                                &
      CALL init_surface_canopy (ie, je, itype_tran,                     &
                                istartpar, iendpar, jstartpar, jendpar, &
                                fr_land, plcov, lai, sai, tai, eai)
  ENDIF

  !----------------------------------------------------------------------------
  ! Section 3.1: Precipitation
  !----------------------------------------------------------------------------

  IF (ltime) CALL get_timings (i_add_computations, ntstep, dt, izerror)

! IF (lgsp) THEN
!   IF (izdebug > 5) THEN
!     PRINT *, '      GRID SCALE PRECIPITATION'
!   ENDIF

!   CALL organize_gscp
!   IF (ltime) CALL get_timings (i_precipitation, ntstep, dt, izerror)
! ENDIF

  !----------------------------------------------------------------------------
  ! Section 3.2: Radiation
  !----------------------------------------------------------------------------

  IF (lrad) THEN
!    IF ( (ntstep < 2) .OR. (ntstep == nextrad) ) THEN
      IF (izdebug > 5) THEN
        PRINT *, '      RADIATION'
      ENDIF

      CALL organize_radiation (ydate_ini)

!!$      IF ((ntstep >= 1) .OR. (nincrad == 1)) THEN
!!$        IF (hincrad /= 0.0) THEN
!!$          hnextrad = hnextrad + hincrad
!!$          nextrad  = NINT (hnextrad * 3600.0_wp / dt)
!!$        ELSE
!!$          nextrad  = nextrad + nincrad
!!$        ENDIF
!!$      ENDIF
      IF (ltime) CALL get_timings (i_radiation, ntstep, dt, izerror)
!    ENDIF
    
  ENDIF !lrad

  !----------------------------------------------------------------------------
  ! Section 3.3: Sub-grid scale orography
  !----------------------------------------------------------------------------
       
  IF (lsso) THEN
    IF ( (ntstep <= 10) .OR. (MOD(ntstep+1,nincsso) == 0) ) THEN
      IF (izdebug > 5) THEN
        PRINT *, '      SUB-GRID SCALE OROGRAPHY'
      ENDIF

      CALL organize_sso

      IF (ltime) CALL get_timings (i_sso, ntstep, dt, izerror)
    ENDIF
  ENDIF

  !----------------------------------------------------------------------------
  ! Section 3.4: Turbulence
  !----------------------------------------------------------------------------

  IF (ltur) THEN

#ifdef TEND
! Variables for tendency-sum output (dmaurer)
  CALL trcr_get(ierror, idt_qv, ptr_tens=qv_tens_tmp)
  IF (izerror /= 0) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  ttend_tmp3 = ttens                                   ! T tendency before turbulence scheme
  utend_tmp3 = utens                                   ! U tendency before turbulence scheme
  vtend_tmp3 = vtens                                   ! V tendency before turbulence scheme
  qvten_tmp3 = qv_tens_tmp                             ! QV tendency before turbulence scheme
#endif

  CALL organize_turbulence (izerror, yerrmsg)

#ifdef TEND
! Variables for tendency-sum output (dmaurer)
  CALL trcr_get(ierror, idt_qv, ptr_tens=qv_tens_tmp)
  IF (izerror /= 0) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  ttend_tmp4 = ttens                                   ! T tendency after turbulence scheme
  utend_tmp4 = utens                                   ! U tendency after turbulence scheme
  vtend_tmp4 = vtens                                   ! V tendency after turbulence scheme
  qvten_tmp4 = qv_tens_tmp                             ! QV tendency after turbulence scheme

  ttend_tur2 = ttend_tmp4 - ttend_tmp3                 ! T tendency turbulence scheme 2
  utend_tur2 = utend_tmp4 - utend_tmp3                 ! U tendency turbulence scheme 2
  vtend_tur2 = vtend_tmp4 - vtend_tmp3                 ! V tendency turbulence scheme 2
  qvten_tur2 = qvten_tmp4 - qvten_tmp3                 ! QV tendency turbulence scheme 2
#endif


    IF (ltime) CALL get_timings (i_turbulence, ntstep, dt, izerror)

  ENDIF

  !----------------------------------------------------------------------------
  ! Section 3.5: Soil Model  ( call of 1st part of 2-layer soil model or the
  !                            complete multi layer soil model respectively)
  !              Sea ice model
  !              Lake model
  !----------------------------------------------------------------------------

  IF (lsoil) THEN
    IF (lmulti_layer) THEN
      IF (izdebug > 5) THEN
        PRINT *, '      MULTI LAYER SOIL MODEL'
      ENDIF

      CALL terra_multlay (yzerror, izerr)
    ELSE
      IF (izdebug > 5) THEN
        PRINT *, '      TWO LAYER SOIL MODEL; 1st part'
      ENDIF

      CALL terra1
    ENDIF
    IF (izerr /= 0_iintegers) THEN
      ierror  = 2009
      yerrmsg = yzerror
      RETURN
    ENDIF

    IF (lseaice) THEN
      IF (izdebug > 5) THEN
        PRINT *, '      SEA ICE MODEL'
      ENDIF

      CALL seaice
    ENDIF

    IF (llake) THEN
      IF (izdebug > 5) THEN
        PRINT *, '      FLAKE MODEL'
      ENDIF

      CALL flake_interface
    ENDIF

    IF (ltime) CALL get_timings (i_soil_model, ntstep, dt, izerror)
  ENDIF
  
  !----------------------------------------------------------------------------
  ! Section 3.6: Convection
  !----------------------------------------------------------------------------

  IF (lconv) THEN
    IF ( (ntstep < 2) .OR. (MOD(ntstep+1,nincconv) == 0) ) THEN
      lzconv = .TRUE.

      SELECT CASE (itype_conv)

      CASE (0) ! Tiedtke scheme
        IF (izdebug > 5) THEN
          PRINT *, '      TIEDTKE CONVECTION SCHEME'
        ENDIF
        CALL organize_conv_tiedtke

!no more:      CASE (1) ! Kain-Fritsch scheme
!no more:        IF (izdebug > 5) THEN
!no more:          PRINT *, '      KAIN-FRITSCH CONVECTION SCHEME'
!no more:        ENDIF
!no more:        CALL organize_conv_kainfri

#ifdef CLM
      CASE (2) ! ECMWF IFS scheme
        IF (izdebug > 5) THEN
          PRINT *, '      ECMWF IFS CONVECTION SCHEME'
        ENDIF
        CALL organize_conv_ifs
#endif

      CASE (3) ! Shallow convection scheme
        IF (izdebug > 5) THEN
          PRINT *, '      SHALLOW CONVECTION SCHEME'
        ENDIF
        CALL organize_conv_shallow

      CASE DEFAULT
        ierror = 2008
        yerrmsg = 'No valid convection scheme'
        RETURN

      END SELECT

    ENDIF

#ifdef MESSY
    CALL messy_convec
#endif

    IF (ltime) CALL get_timings (i_convection, ntstep, dt, izerror)
  ENDIF
  
  !----------------------------------------------------------------------------
  ! Section 3.7: Soil Model  ( 2nd part of the 2-layer soil model)
  !----------------------------------------------------------------------------

  IF (lsoil) THEN
    IF (.NOT. lmulti_layer) THEN
      IF (izdebug > 5) THEN
        PRINT *, '      TWO LAYER SOIL MODEL; 2nd part'
      ENDIF

      CALL terra2
    ENDIF
    IF (ltime) CALL get_timings (i_soil_model, ntstep, dt, izerror)
  ENDIF

  !----------------------------------------------------------------------------
  ! Section 3.8: Boundary exchange for the parallel program
  !----------------------------------------------------------------------------

  IF (izdebug > 5) THEN
    PRINT *, '      BOUNDARY EXCHANGE AFTER PHYSICS'
  ENDIF

  IF (ltime) THEN
    IF (ltime_barrier) THEN
      CALL comm_barrier (icomm_cart, izerror, yerrmsg)
      CALL get_timings (i_barrier_waiting_phy, ntstep, dt, izerror)
    ENDIF
  ENDIF

  IF (lsso) THEN
    IF (.NOT. lzconv) THEN
      kzdims(1:24)=(/ke1,ke1,ke,ke,ke,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      CALL exchg_boundaries                                                  &
         (nx+73,  sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,  &
          ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,        &
          my_cart_neigh, lperi_x, lperi_y, l2dim,                            &
          10000+nexch_tag, ldatatypes, ncomm_type, izerror, yerrmsg,         &
          tkvm, tkvh, qrs, ut_sso, vt_sso, t_s(:,:,nx),                      &
          qv_s(:,:,nx), tcm, tch)
    ELSE
      kzdims(1:24)=(/ke1,ke1,ke,ke,ke,ke,ke,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      CALL exchg_boundaries                                                  &
         (nx+70,  sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,  &
          ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,        &
          my_cart_neigh, lperi_x, lperi_y, l2dim,                            &
          10000+nexch_tag, ldatatypes, ncomm_type, izerror, yerrmsg,         &
          tkvm, tkvh, qrs, ut_conv, vt_conv, ut_sso, vt_sso, t_s(:,:,nx),    &
          qv_s(:,:,nx), tcm, tch)
    ENDIF
  ELSE
    IF (.NOT. lzconv) THEN
      kzdims(1:24)=(/ke1,ke1,ke,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      CALL exchg_boundaries                                                  &
         (nx+13,  sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,  &
          ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,        &
          my_cart_neigh, lperi_x, lperi_y, l2dim,                            &
          10000+nexch_tag, ldatatypes, ncomm_type, izerror, yerrmsg,         &
          tkvm, tkvh, qrs, t_s(:,:,nx), qv_s(:,:,nx), tcm, tch)
    ELSE
      kzdims(1:24)=(/ke1,ke1,ke,ke,ke,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      CALL exchg_boundaries                                                  &
         (nx+10,  sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,  &
          ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,        &
          my_cart_neigh, lperi_x, lperi_y, l2dim,                            &
          10000+nexch_tag, ldatatypes, ncomm_type, izerror, yerrmsg,         &
          tkvm, tkvh, qrs, ut_conv, vt_conv, t_s(:,:,nx), qv_s(:,:,nx),      &
          tcm, tch)
    ENDIF
  ENDIF

  IF ( ltur ) THEN

    IF ( l3dturb ) THEN

      kzdims(1:24)=(/ke1,ke1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      CALL exchg_boundaries                                                  &
           (0,  sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,    &
           ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,       &
           my_cart_neigh, lperi_x, lperi_y, l2dim,                           &
           15000+nexch_tag, .FALSE., ncomm_type, izerror, yerrmsg,           &
           tkhm, tkhh )
    END IF
    
    SELECT CASE (itype_turb )
    CASE (3)
      IF ( l3dturb .OR. lprog_tke ) THEN   ! Not sure if necessary in all cases or just in 3D-Turb.
        kzdims(1:24)=(/ke1,ke1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
        CALL exchg_boundaries                                                &
             (0, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,   &
              ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,    &
              my_cart_neigh, lperi_x, lperi_y, l2dim,                        &
              15000+nexch_tag, .FALSE., ncomm_type, izerror, yerrmsg,        &
              tke(:,:,:,ntke), tketens )
      END IF
    CASE (5:8)
      IF ( lprog_tke ) THEN
        kzdims(1:24)=(/ke1,ke1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
        CALL exchg_boundaries                                                &
             (0, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,   &
              ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,    &
              my_cart_neigh, lperi_x, lperi_y, l2dim,                        &
              15000+nexch_tag, .FALSE., ncomm_type, izerror, yerrmsg,        &
              tke(:,:,:,nnow), tketens )
      ELSE
        IF ( l3dturb ) THEN   ! Not sure if necessary in all cases or just in 3D-Turb.
          kzdims(1:24)=(/ke1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
          CALL exchg_boundaries                                              &
               (0, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute, &
                ie, je, kzdims, jstartpar, jendpar, nbl_exchg, nboundlines,  &
                my_cart_neigh, lperi_x, lperi_y, l2dim,                      &
                15000+nexch_tag, .FALSE., ncomm_type, izerror, yerrmsg,      &
                tke(:,:,:,1) )
        END IF
      END IF
    END SELECT

  END IF
    
  IF (nradcoarse > 1) THEN
    ! radiation computations on a coarser grid need additional
    ! boundary exchange

    IF ( lsoil ) THEN
      IF(lmulti_layer) THEN
        kzdims(1:24)=(/1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
       CALL exchg_boundaries                                                 &
         (70+nnew, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute, &
          ie, je, kzdims, jstartpar, jendpar, nradcoarse-1, nboundlines,     &
          my_cart_neigh, lperi_x, lperi_y, l2dim,                            &
          17000+nexch_tag, ldatatypes, ncomm_type, izerror, yerrmsg,         &
          t_g(:,:,nnew),w_so(:,:,1,nnew),t_snow(:,:,nnew),w_snow(:,:,nnew),  &
          freshsnow(:,:) )
      ELSE
        kzdims(1:24)=(/1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
        CALL exchg_boundaries                                                &
          (73+nnew, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,&
           ie, je, kzdims, jstartpar, jendpar, nradcoarse-1, nboundlines,    &
           my_cart_neigh, lperi_x, lperi_y, l2dim,                           &
           17000+nexch_tag, ldatatypes, ncomm_type, izerror, yerrmsg,        &
           t_g(:,:,nnew),w_g1(:,:,nnew),t_snow(:,:,nnew),w_snow(:,:,nnew))
      ENDIF
    ELSE ! .NOT.lsoil:
        kzdims(1:24)=(/1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
        ! exchange of t_snow probably not necessary
        CALL exchg_boundaries                                                &
          (76+nnew, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,&
           ie, je, kzdims, jstartpar, jendpar, nradcoarse-1, nboundlines,    &
           my_cart_neigh, lperi_x, lperi_y, l2dim,                           &
           17000+nexch_tag, ldatatypes, ncomm_type, izerror, yerrmsg,        &
           t_g(:,:,nnew),t_snow(:,:,nnew) )
    ENDIF

    IF (lzconv) THEN
      kzdims(1:24)=(/ke,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/)
      CALL exchg_boundaries                                                  &
        (2, sendbuf, isendbuflen, imp_reals, icomm_cart, num_compute,        &
         ie, je, kzdims, jstartpar, jendpar, nradcoarse-1, nboundlines,      &
         my_cart_neigh, lperi_x, lperi_y, l2dim,                             &
         19000+nexch_tag, ldatatypes, ncomm_type, izerror, yerrmsg,          &
         clc_con(:,:,:))
    ENDIF

  ENDIF   ! nradcoarse > 1

  IF (ltime) CALL get_timings (i_communications_phy, ntstep, dt, izerror)

  !----------------------------------------------------------------------------
  ! Section 3.9: Computation of total physical forcings
  !              (this has been in the dynamical cores before Version 4.23)
  !----------------------------------------------------------------------------

  ! retrieve the required microphysics tracers
  CALL trcr_get(ierror, idt_qv, ptr_tens=qv_tens)
  IF (ierror /= 0) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_qc, ptr_tens=qc_tens)
  IF (ierror /= 0) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  CALL trcr_get(ierror, idt_qi, ptr_tens=qi_tens)
  IF (ierror /= 0 .AND. ierror /= T_ERR_NOTFOUND) THEN
    yerrmsg = trcr_errorstr(ierror)
    CALL model_abort(my_cart_id, ierror, yerrmsg, yzroutine)
  ENDIF
  
  ! Reset error
  ierror = 0_iintegers

  ! Compute total physical forcings (tendencies)

#ifdef TEND
! Variables for tendency-sum output (dmaurer)
  ttend_rad = ttend_rad + sohr + thhr       ! Sum T tendencies of Radiation
  ttend_con = ttend_con + tt_conv           ! Sum T tendency of shallow convection
  IF (lsso) THEN
    ttend_sso = ttend_sso + tt_sso          ! Sum T tendency of SSO
  ENDIF
  qvten_con = qvten_con + qvt_conv          ! Sum QV tendency of shallow convection
#endif

  DO  k = 1 , ke
    DO    j = jstart, jend 
      DO  i = istart, iend
        ttens  (i,j,k) = ttens  (i,j,k) + sohr(i,j,k) + thhr(i,j,k)
        ttens  (i,j,k) = ttens  (i,j,k) + tt_conv (i,j,k)
        !WL2011b
        !export temperature tendency due to radiation and convection
        !we create a new variable since we might convert to pot. temp
        IF (lbud) THEN
          tt_rad (i,j,k)= sohr(i,j,k) + thhr(i,j,k)
          tt_con (i,j,k)= tt_conv (i,j,k)
        ENDIF
        !WL2011e
        qv_tens(i,j,k) = qv_tens(i,j,k) + qvt_conv(i,j,k)
!ROA bugfix: Wolfgang had the line qvt_turb(i,j,k) = qvt_turb(i,j,k) - qvt_diff(i,j,k) 
!            in src_runge_kutta.f90 (line 1025) in 4.19. The corresponding location
!            in 4.26 would be here. However, I think that it was a bug in 4.19.
!            Indeed the qvt_diff contribution is already caught in the subroutine
!            computing the explicit horizontal diffusion. I thus just ignore this line.
      ENDDO
    ENDDO

    IF (lsso) THEN
      DO    j = jstart, jend
        DO  i = istart, iend
          ttens(i,j,k) = ttens(i,j,k) + tt_sso(i,j,k)
          !WL2011b
          !export temperature tendency due sso
          IF (lbud) THEN
            tt_ssowl (i,j,k)= tt_sso(i,j,k) 
          ENDIF
          !WL2011e
        ENDDO
      ENDDO
    ENDIF

!US this is not active at the moment, but might be reconsidered in the future
!US therefore we leave these lines in the code
!   IF (itype_conv == 1) THEN
!     DO    j = jstart, jend
!       DO  i = istart, iend
!         qc_tens(i,j,k) = qc_tens(i,j,k) + qct_conv(i,j,k)
!       ENDDO
!     ENDDO
!   ENDIF

    IF ( (itype_conv == 0) .OR. (itype_conv == 2) ) THEN
      ! Add convective tendencies of qc and qi computed by Tiedtke(0) or
      ! IFS scheme (2)
      DO    j = jstart, jend
        DO  i = istart, iend
          qc_tens(i,j,k) = qc_tens(i,j,k) + qct_conv(i,j,k)
        ENDDO
      ENDDO
      IF ( ASSOCIATED(qi_tens) ) THEN
        DO    j = jstart, jend
          DO  i = istart, iend
            qi_tens(i,j,k) = qi_tens(i,j,k) + qit_conv(i,j,k)
          ENDDO
        ENDDO
      ENDIF
    ENDIF

    ! SSO and convective tendencies for u and v have been exchanged above and
    ! have to be interpolated to the u- and v-grid points
    IF (lconv) THEN
      IF (lzconv) THEN
        DO   j = jstartu, jendu
          DO i = istartu, iendu
            ut_conv(i,j,k) = 0.5_wp*( ut_conv(i+1,j,k) + ut_conv(i,j,k) )
          ENDDO
        ENDDO
      ENDIF
      DO   j = jstartu, jendu
        DO i = istartu, iendu
          utens(i,j,k) = utens(i,j,k) + ut_conv(i,j,k)
        ENDDO
      ENDDO
    ENDIF
    IF (lsso) THEN
      DO   j = jstartu, jendu
        DO i = istartu, iendu
          ut_sso(i,j,k) = 0.5_wp*( ut_sso(i+1,j,k) + ut_sso(i,j,k) )
          utens(i,j,k) = utens(i,j,k) + ut_sso(i,j,k)
        ENDDO
      ENDDO
    ENDIF
    IF (lconv) THEN
      IF (lzconv) THEN
        DO   j = jstartv, jendv
          DO i = istartv, iendv
            vt_conv(i,j,k) = 0.5_wp*( vt_conv(i,j+1,k) + vt_conv(i,j,k) )
          ENDDO
        ENDDO
      ENDIF
      DO   j = jstartv, jendv
        DO i = istartv, iendv
          vtens(i,j,k) = vtens(i,j,k) + vt_conv(i,j,k)
        ENDDO
      ENDDO
    ENDIF
    IF (lsso) THEN
      DO   j = jstartv, jendv
        DO i = istartv, iendv
          vt_sso(i,j,k) = 0.5_wp*( vt_sso(i,j+1,k) + vt_sso(i,j,k) )
          vtens(i,j,k) = vtens(i,j,k) + vt_sso(i,j,k)
        ENDDO
      ENDDO
    ENDIF

  ENDDO

#ifdef TEND
! Variables for tendency-sum output (dmaurer)
  IF (lsso) THEN
    utend_sso = utend_sso + ut_sso     ! Sum U tendency of SSO
    vtend_sso = vtend_sso + vt_sso     ! Sum V tendency of SSO
  ENDIF
#endif

  IF (izdebug > 5) THEN
    PRINT *, '      PHYSICAL COMPUTATIONS before dynamics finished '
  ENDIF

!------------------------------------------------------------------------------
! Section 4: Physical packages at the end of the time stepping
!------------------------------------------------------------------------------

ELSEIF (yaction == 'finish_compute') THEN

  IF (lgsp) THEN
    IF (izdebug > 5) THEN
      PRINT *, '      GRID SCALE PRECIPITATION after dynamics'
    ENDIF

    IF     (itype_gscp == 4) THEN
      CALL hydci_pp_gr
    ELSEIF (itype_gscp == 3) THEN
      CALL hydci_pp
    ELSEIF (itype_gscp == 2) THEN
      CALL hydor_pp
    ELSEIF (itype_gscp == 1) THEN
      CALL kessler_pp
#ifdef TWOMOM_SB
    ELSEIF (itype_gscp >= 100 ) THEN
      CALL seifert_pp
#endif
    ENDIF

    IF (izdebug > 5) THEN
      PRINT *, '      PHYSICAL COMPUTATIONS after dynamics finished '
    ENDIF

    IF (ltime) CALL get_timings (i_precipitation, ntstep, dt, izerror)
  ENDIF

!------------------------------------------------------------------------------
! Section 5: All other actions are wrong
!------------------------------------------------------------------------------

ELSE

  ierror  = 1
  yerrmsg = 'ERROR *** No valid action for the physics ***'

ENDIF

!------------------------------------------------------------------------------
! Internal Procedures
!------------------------------------------------------------------------------

CONTAINS

SUBROUTINE readCCNconc(ccn1,ccn2,ccn3,ie,je,ke)
!In this routine the total aerosol number concentrations of the lognormal modes
!prescribed for the calculation of the CCN concentrations are set.
!They correspond to N_t given below:
!       dN     N_t                 -ln²(r/r_m)
!       ---- = -------------  exp( -----------)
!       dlnr   2*Pi*ln(sigma)      2ln²(sigma)
  USE data_runcontrol ,   ONLY : CCN_1_N, CCN_2_N, CCN_3_N
  USE data_gscp, ONLY: sumCCN_t0,sumCCN1_t0,sumCCN2_t0,sumCCN3_t0
  USE data_modelconfig, ONLY: ie_tot, je_tot
  USE data_fields, ONLY: hhl

  IMPLICIT NONE
  INTEGER(KIND=iintegers), INTENT(IN) :: ie,je,ke
  REAL (KIND=wp), INTENT(INOUT) :: ccn1(ie,je,ke), ccn2(ie,je,ke), ccn3(ie,je,ke)
  INTEGER(KIND=iintegers) :: k,izerror
  REAL (KIND=wp) :: dz_dom


  CCN_1_N=0.0_wp
  CCN_2_N=250e06_wp
  CCN_3_N=0.0_wp
  ccn1(:,:,:)=CCN_1_N         !largest size mode
  ccn2(:,:,:)=CCN_2_N         !second largest size mode
  ccn3(:,:,:)=CCN_3_N      !third largest size mode

  sumCCN1_t0=0.0
  sumCCN2_t0=0.0
  sumCCN3_t0=0.0
  do k=1,ke
    dz_dom=hhl(1,1,k) - hhl(1,1,k+1)
    sumCCN1_t0=sumCCN1_t0+ccn1(1,1,k)*dz_dom*ie_tot*je_tot
    sumCCN2_t0=sumCCN2_t0+ccn2(1,1,k)*dz_dom*ie_tot*je_tot
    sumCCN3_t0=sumCCN3_t0+ccn3(1,1,k)*dz_dom*ie_tot*je_tot
  end do
  sumCCN_t0=sumCCN1_t0+sumCCN2_t0+sumCCN3_t0
END SUBROUTINE readCCNconc


!ZD >>>>
SUBROUTINE readINconc(in_t1, in_t2, in_t3, in_t4, in_t5, in_t6, in_t7, in_t8, in_t9, &
                      in_t10, in_t11, in_t12, in_t13, in_t14, in_t15, ie, je, ke)

!In this routine the DeMott (2015) formula is used to initialise the IN_T1:15 bins
!==================================================================================
!   At each Temperature the IN concentrationn (m-3) is given as:
!       IN (T)= 10^(-3)*Na^1.25*exp(0.46*(273.16-T)-11.6)
!   This formula is used with alpha=0, beta=1.25, gamma=0.46, delta=-11.6 (as in paper).

!The number of IN_k per bin k is then computed as:
!   IN_k=IN(T_k)-IN_k-1
!i.e. the IN concentration at the upper boundary temperature T_k of bin k, is given by
!the DeMott formula minus the IN concentration of the next warmer temperature bin

!The temperature bins are valid between -15.5 and -20.2 (temperature bin spacing is 0.3 K)
!boundary temperatures are:
!     T1=-15.5 deg C (257.65 K)
!     T2=-15.8 deg C (257.35 K)
!     T3=-16.1 deg C (257.05 K)
USE data_gscp, ONLY: sumIN_t0, sumIN1_t0, sumIN2_t0,sumIN3_t0,sumIN4_t0,sumIN5_t0,sumIN6_t0,&
                     sumIN7_t0,sumIN8_t0,sumIN9_t0,sumIN10_t0,sumIN11_t0,sumIN12_t0,sumIN13_t0,&
                     sumIN14_t0,sumIN15_t0
USE data_modelconfig, ONLY: ie_tot, je_tot
USE data_fields, ONLY: hhl

  IMPLICIT NONE
  INTEGER(KIND=iintegers), INTENT(IN) :: ie,je,ke  
  REAL (kind=wp), INTENT(INOUT)                :: in_t1(ie,je,ke), in_t2(ie,je,ke), in_t3(ie,je,ke), in_t4(ie,je,ke),in_t5(ie,je,ke), &
                                                  in_t6(ie,je,ke), in_t7(ie,je,ke), in_t8(ie,je,ke), in_t9(ie,je,ke),in_t10(ie,je,ke),&
                                                  in_t11(ie,je,ke), in_t12(ie,je,ke), in_t13(ie,je,ke), in_t14(ie,je,ke), in_t15(ie,je,ke)

  REAL(KIND=wp) :: Na(ie,je,ke),INsum(ie,je,ke)  !aerosol number concentration
  REAL(KIND=wp) :: Temp_bin,dz_dom
  INTEGER(KIND=iintegers) :: k,izerror
!set aerosol number concentration to Na=1.57 [cm-3], which gives the 0.16 IN per litre,
!corresponding to mean MPACE observations and MPACE intercomparison case setup
!Ref: Klein et al 2009 (Q. J. R. Met. Soc.)
  Na(:,:,:)=Na_IN
  Temp_bin=Tmax_IN
  INsum=0.0_wp

  in_t1(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp)
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t1
  in_t2(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t2
  in_t3(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t3
  in_t4(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t4
  in_t5(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t5
  in_t6(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t6
  in_t7(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t7
  in_t8(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t8
  in_t9(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t9
  in_t10(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t10
  in_t11(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t11
  in_t12(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t12
  in_t13(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t13
  in_t14(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum
  Temp_bin=Temp_bin-Tdiff_IN
  INsum=INsum+in_t14
  in_t15(:,:,:)=1e3_wp*Na(:,:,:)**1.25_wp*exp(0.46_wp*(273.16_wp-Temp_bin)-11.6_wp) - INsum

  sumIN1_t0=0.0
  sumIN2_t0=0.0
  sumIN3_t0=0.0
  sumIN4_t0=0.0
  sumIN5_t0=0.0
  sumIN6_t0=0.0
  sumIN7_t0=0.0
  sumIN8_t0=0.0
  sumIN9_t0=0.0
  sumIN10_t0=0.0
  sumIN11_t0=0.0
  sumIN12_t0=0.0
  sumIN13_t0=0.0
  sumIN14_t0=0.0
  sumIN15_t0=0.0
  do k=1,ke
    dz_dom=hhl(1,1,k) - hhl(1,1,k+1)
    sumIN1_t0=sumIN1_t0+in_t1(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN2_t0=sumIN2_t0+in_t2(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN3_t0=sumIN3_t0+in_t3(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN4_t0=sumIN4_t0+in_t4(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN5_t0=sumIN5_t0+in_t5(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN6_t0=sumIN6_t0+in_t6(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN7_t0=sumIN7_t0+in_t7(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN8_t0=sumIN8_t0+in_t8(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN9_t0=sumIN9_t0+in_t9(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN10_t0=sumIN10_t0+in_t10(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN11_t0=sumIN11_t0+in_t11(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN12_t0=sumIN12_t0+in_t12(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN13_t0=sumIN13_t0+in_t13(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN14_t0=sumIN14_t0+in_t14(1,1,k)*dz_dom*ie_tot*je_tot
    sumIN15_t0=sumIN15_t0+in_t15(1,1,k)*dz_dom*ie_tot*je_tot
  end do
  sumIN_t0=sumIN1_t0+sumIN2_t0+sumIN3_t0+sumIN4_t0+sumIN5_t0+sumIN6_t0+sumIN7_t0+sumIN8_t0+&
           sumIN9_t0+sumIN10_t0+sumIN11_t0+sumIN12_t0+sumIN13_t0+sumIN14_t0+sumIN15_t0
END SUBROUTINE readINconc

!ZD<<<<



!==============================================================================
!+ Module procedure in "setup" for the input of NAMELIST phyctl
!------------------------------------------------------------------------------

SUBROUTINE input_phyctl (nuspecif, nuin, ierrstat)

!------------------------------------------------------------------------------
! Description:
!   This subroutine organizes the input of the NAMELIST-group phyctl. 
!   The group phyctl contains variables for the organization of the physics.
!   These are logical variables whether a certain package has to be performed
!   and organizational variables that determine how often it is performed.
!
! Method:
!   All variables are initialized with default values and then read in from
!   the file INPUT. The input values are checked for errors and for
!   consistency. If wrong input values are detected the program prints
!   an error message. The program is not stopped in this routine but an
!   error code is returned to the calling routine that aborts the program after
!   reading in all other namelists.
!   In parallel mode, the variables are distributed to all nodes with the
!   environment-routine distribute_values.
!   Both, default and input values are written to the file YUSPECIF
!   (specification of the run).
!
!------------------------------------------------------------------------------

! Subroutine / Function arguments

  INTEGER   (KIND=iintegers),   INTENT (IN)      ::        &
    nuspecif,     & ! Unit number for protocolling the task
    nuin            ! Unit number for Namelist INPUT file

  INTEGER   (KIND=iintegers),   INTENT (INOUT)   ::        &
    ierrstat        ! error status variable

! Variables for default values
  REAL (KIND=wp)             ::       &
    hincrad_d,    & ! hour increment for running the radiation
    radlon_d,     & ! geographic longitude for the radiation
    radlat_d,     & ! geographic latitude for the rediation
    czbot_w_so_d, & ! depth of bottom of last hydrological active soil layer
    czml_soil_d(20), & ! depth of the main level of the soil layers (defaults)
    czml_soil  (20), &    ! depth of the main level of the soil layers (read in)
    Tdiff_IN_d,     & ! temperature interval for initialisation
    Tmax_IN_d,      & ! warmest bin temperature
    Na_IN_d,       & ! number of aerosols (DeMott 2015) in 1/cm3
    CCN_ship_N_d,  & ! number of aerosols ship perturbation in 1/m3
    Na_IN_pert_d,  & ! number perturbation for ship Na_IN in 1/cm3
    tperturb_d       ! time [h] after which ship perturbation is applied


  INTEGER (KIND=iintegers)   ::       &
    nradcoarse_d, & ! number of horiz. gridpoints for radiation on coarser grid
    nincrad_d,    & ! time step increment for running the radiation
    nincsso_d,    & ! time step increment for running the radiation
    ninctura_d,   & ! time step increment for running the vertical diffusion
    nincconv_d,   & ! time step increment for running the convection scheme 

    itype_trvg_d, & ! type of vegetation transpiration parameterization
    itype_evsl_d, & ! type of parameterization of bare soil evaporation

    itype_gscp_d, & ! type of grid-scale precipitation physics               

    itype_sher_d, & ! type of shear production for TKE
    itype_wcld_d, & ! type of water cloud diagnosis
    itype_tran_d, & ! type of surface-atmosphere transfer
    itype_turb_d, & ! type of turbulent diffusion parametrization
    itype_synd_d, & ! type of diagnosis of synop. station values

    imode_tran_d, & ! mode of surface-atmosphere transfer
    imode_turb_d, & ! mode of turbulent diffusion parametrization
!<FG: 2M-scheme
    iradpar_cloud_d, & ! type of parameterization of radiative transfer
!FG: 2M-scheme>

    ico2_rad_d,   & ! type of CO2 concentration in radiation parameterization
                    ! (used in the CLM)
    iy_co2_stab_d,& ! default stabilisation year

    icldm_rad_d,  & ! mode of cloud representation in radiation  parametr.
    icldm_tran_d, & ! mode of cloud representation in transfer  parametr.
    icldm_turb_d, & ! mode of cloud representation in turbulence parametr.
    itype_conv_d, & ! type of convection parameterization

    itype_aerosol_d, &! type of aerosol map
    itype_root_d,    &! type of root density distribution
    itype_heatcond_d,&! type of heat conductivity
    itype_hydbound_d,&! type of hydraulic lower boundary

    nhori_d,      & ! number of sectors for the horizont by the topographic radcorr
    ke_soil_d,    & ! number of layers in multi-layer soil model
    ke_snow_d,    & ! number of layers in multi-layer snow model
    nlgw_d,       & ! number of prognostic soil water levels
    itype_albedo_d  ! type of soil albedo treatment

  LOGICAL                    ::       &
    lrad_d,       & ! forecast with radiation
    lforest_d,    & ! to run with forest data (evergreen and deciduous)
    ltur_d,       & ! forecast with vertical diffusion
    lconv_d,      & ! forecast with convection
    lconv_inst_d, & ! output of instantaneous values of top_con/bas_con
                    ! instead of min/max for an output interval
    lgsp_d,       & ! forecast with grid scale precipitation
    lINprog_d,    & ! include prognostic equation for IN following Solomon & Feingold 2015 (ACP)
    lCCNprog_d,   & ! include prognostic aerosol number concentration tracer for CCN size distribution
    ldiniprec_d,  & ! diagnostic initialisation of prognostic precip (qr, qs)
    l3dturb_d,    & ! 3D-turbulence: CALL explicit_horizontal_diffusion (RK)
    l3dturb_metr_d,&! switch on/off additional metric terms for 3D-turbulence
    !(WL;2010)b
    lkhdef3d_d,   & ! switch on/off 3D calculation of deformation for horizontal diffusion
    lisotrop_d,   &
    lneutral_d,   &
    !(WL;2010)e
    lprog_tke_d,  & ! prognostic treatment of TKE (for itype_turb=5/7)
    limpltkediff_d,&! use semi_implicit TKE-diffusion
    lsoil_d,      & ! forecast with soil model
    lmelt_d,      & ! soil model with melting process
    lmelt_var_d,  & ! freezing temperature dependent on water content
    lmulti_layer_d,&! run multi-layer soil model
    lmulti_snow_d, &! run multi-layer snow model
    lseaice_d,    & ! forecast with sea ice model
    llake_d,      & ! forecast with lake model
    lsso_d,       & ! forecast with sub-grid scale orography scheme

    lemiss_d,     & ! external surface emissivity map
    lstomata_d,   & ! external minimum stomata resistance

    ltkesso_d,    & ! calculation SSO-wake turbulence production for TKE
    ltkecon_d,    & ! consider convective buoyancy production for TKE
    lexpcor_d,    & ! explicit corrections of the implicit calculated
                    ! turbulent diffusion (only if itype_turb=3)
    ltmpcor_d,    & ! consideration of thermal TKE-sourcel in the enthalpy budget
    lprfcor_d,    & ! using the profile values of the lowest main level instead of
                    ! the mean value of the lowest layer for surface flux calulations
    lnonloc_d,    & ! nonlocal calculation of vertical gradients used
                    ! for turbulent diffusion (only if itype_turb=3)
    lcpfluc_d,    & ! consideration of fluctuations of the heat capacity of air
    lconf_avg_d,  & ! average convective forcings in case of massflux closure
    lradf_avg_d,  & ! average radiative forcings when running on coarser grid
    lcape_d,      & ! convection with CAPE closure
    lctke_d,      & ! convection with turbulent convective energy closure
                    ! warning: lctke not yet fully implemented
    lradtopo_d,   & ! uses topographic correction of radiation
!cloud forcing>
    lcrf_d,       & ! if .true., cloud radiative forcing is computed
!cloud forcing<
    lco2_stab_d,  & ! a  year for CO2 stabilisation will be defined
    lradcst_d       ! use fixed geographic location for the radiation calculation


  INTEGER (KIND=iintegers)   :: i, k, invar, ierr, iz_err
  LOGICAL                    :: lzequiv

! Define the namelist group

  NAMELIST /phyctl/ lrad, ltur, lconv, itype_conv, lgsp, lINprog, lCCNprog,   &
                    lsoil, lmelt, lmelt_var, lmulti_layer, lexpcor,           &
                    ltmpcor, lprfcor, lnonloc, lcpfluc,lcape,lctke, lconf_avg,&
                    nincrad, hincrad, ninctura, nincconv,                     &
                    ldiniprec, itype_trvg, itype_evsl,                        &
                    itype_gscp, itype_wcld, itype_tran, itype_turb,itype_synd,&
                    icldm_rad, icldm_tran, icldm_turb, imode_tran, imode_turb,&
                    ke_soil, czml_soil, nlgw, l3dturb, lprog_tke,             &
                    lforest, lconv_inst, lseaice, llake, ico2_rad, czbot_w_so,&
!cloud forcing>
                    lcrf,                                                     &
!cloud forcing<
!<FG: 2M-scheme
                    iradpar_cloud,                                            &
!FG: 2M-scheme>
                    Tdiff_IN, Tmax_IN, Na_IN,CCN_ship_N, tperturb, Na_IN_pert,&
                    l3dturb_metr, nradcoarse, lradf_avg, lradtopo, nhori,     &
                    lsso, nincsso, limpltkediff, ltkesso, ltkecon,            &
                    lradcst, radlon, radlat,                                  &
                    itype_sher, lmulti_snow, ke_snow, lemiss, lstomata,       &
                    itype_aerosol, itype_root, itype_heatcond, itype_hydbound,&
                    lco2_stab, iy_co2_stab, itype_albedo,                     &
                    !(WL;2010)b 
                    lkhdef3d, lisotrop, lneutral 
                    !(WL;2010)e

!------------------------------------------------------------------------------
!- End of header -
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!- Begin SUBROUTINE input_phyctl
!------------------------------------------------------------------------------

ierrstat = 0_iintegers
iz_err   = 0_iintegers

IF (my_world_id == 0) THEN

!------------------------------------------------------------------------------
!- Section 1: Initialize the default variables
!------------------------------------------------------------------------------

  lrad_d         = .TRUE.
  lforest_d      = .TRUE.
  ltur_d         = .TRUE.
  lconv_d        = .TRUE.
  lconv_inst_d   = .FALSE.
  lgsp_d         = .TRUE.
  lINprog_d      = .FALSE.
  lCCNprog_d     = .FALSE.
  Tdiff_IN_d=0.3   ! temperature interval for initialisation
  Tmax_IN_d=257.65 ! warmest bin temperature
  Na_IN_d=1.57_wp  ! number of aerosols (DeMott 2015) in 1/cm3
  CCN_ship_N_d=0._wp    ! number of aeroosls ship perturbation in 1/m3
  Na_IN_pert_d=0._wp    ! number perturbation for ship Na_IN
  tperturb_d=1.0e+12_wp ! time [h] after which ship perturbation is applied
  ldiniprec_d    = .FALSE.
  l3dturb_d      = .FALSE.
  lkhdef3d_d     = .False.
  lisotrop_d     = .True.
  lneutral_d     = .False.
  l3dturb_metr_d = .TRUE.
  lprog_tke_d    = .FALSE.
  limpltkediff_d = .TRUE.
  lsoil_d        = .TRUE.
  lmelt_d        = .TRUE.
  lmelt_var_d    = .TRUE.
  lmulti_layer_d = .TRUE.
  lmulti_snow_d  = .FALSE.
  lseaice_d      = .TRUE.
  llake_d        = .FALSE.
  lemiss_d       = .FALSE.
  lstomata_d     = .FALSE.

  lsso_d         = .TRUE.
  ltkesso_d      = .FALSE.
  ltkecon_d      = .FALSE.
  lexpcor_d      = .FALSE.
  ltmpcor_d      = .FALSE.
  lprfcor_d      = .FALSE.
  lnonloc_d      = .FALSE.
  lcpfluc_d      = .FALSE.
  lconf_avg_d    = .TRUE. 
  lradf_avg_d    = .FALSE.
  lcape_d        = .FALSE.
  lctke_d        = .FALSE.
  lradtopo_d     = .FALSE.
!cloud forcing>
  lcrf_d         = .FALSE.
!cloud forcing<
  lco2_stab_d    = .FALSE.
  lradcst_d      = .FALSE.

  radlon_d       = 0.0_wp
  radlat_d       = 36.0_wp
  nhori_d      = 24
  nradcoarse_d = 1
  nincrad_d    = 0
  hincrad_d    = 1.0_wp
  ninctura_d   = 1
  nincconv_d   = 4
  nincsso_d    = 5

  itype_trvg_d = 2 ! vegetation transpiration using the BATS-approach
  itype_evsl_d = 2 ! bare soil evaporation using the BATS-approach

  itype_gscp_d = 3 ! grid-scale-cloud-precipitation with 'hydor'

  itype_sher_d = 1 ! only vertical shear
  itype_wcld_d = 2 ! water cloud diagnosis (in SUB cloud_diag) using statistical condensation
  itype_tran_d = 2 ! new surface-atmosphere transfer using SUB turb_tran
  itype_turb_d = 3 ! new moist scheme with prognostic tke-equation using SUB turb_diff
  itype_synd_d = 2 ! new method with resistance formulation using SUB turb_tran

  imode_tran_d = 1 ! diagnostik TKE in 'turbtran'
  imode_turb_d = 1 ! implicite calculation of turbulent diffusion using the explicit
                   ! surface flux densities as lower boundary condition
!<FG: 2M-scheme
  iradpar_cloud_d = 3 ! type of parameterization of radiative transfer
!FG:>

  ico2_rad_d   = 0 ! constant CO2 concentration (330 ppm)
  iy_co2_stab_d = 2001 ! year of CO2 stabilisation

  icldm_rad_d  = 4 ! special (old) cloud diagnosis (using relative humidity) for radiation
  icldm_tran_d = 0 ! surface-atmosphere-transfer considers no water clouds
  icldm_turb_d = 2 ! turbulence considers water clouds calculated in SUB cloud_diag
                   ! dependent on itype_wcld.

  itype_conv_d = 0 ! Tiedtke convection scheme

  itype_aerosol_d   = 1 ! fixed aerosol map
  itype_root_d      = 1 ! uniform root density distribution
  itype_heatcond_d  = 1 ! average soil moisture for heat conductivity
  itype_hydbound_d  = 1 ! drainage no diffusion

  ke_soil_d      = 7
  czml_soil_d(1)  = 0.005_wp
  czml_soil_d(2)  = 0.02_wp
  czml_soil_d(3)  = 0.06_wp
  czml_soil_d(4)  = 0.18_wp
  czml_soil_d(5)  = 0.54_wp
  czml_soil_d(6)  = 1.62_wp
  czml_soil_d(7)  = 4.86_wp
  czml_soil_d(8)  =14.58_wp
  czbot_w_so_d    = 2.5_wp

  ke_snow_d      = 2
  nlgw_d         = 2

  itype_albedo_d = 1

!------------------------------------------------------------------------------
!- Section 2: Initialize variables with defaults
!------------------------------------------------------------------------------

  lrad           = lrad_d
  lforest        = lforest_d
  ltur           = ltur_d
  lconv          = lconv_d
  lconv_inst     = lconv_inst_d
  lgsp           = lgsp_d
  lINprog        = lINprog_d
  lCCNprog       = lCCNprog_d
  Tdiff_IN       = Tdiff_IN_d
  Tmax_IN        = Tmax_IN_d
  Na_IN          = Na_IN_d
  CCN_ship_N     = CCN_ship_N_d
  Na_IN_pert     = Na_IN_pert_d
  tperturb       = tperturb_d
  ldiniprec      = ldiniprec_d
  l3dturb        = l3dturb_d
  lkhdef3d       = lkhdef3d_d
  lisotrop       = lisotrop_d
  lneutral       = lneutral_d
  l3dturb_metr   = l3dturb_metr_d
  lprog_tke      = lprog_tke_d
  limpltkediff   = limpltkediff_d
  lsoil          = lsoil_d
  lmelt          = lmelt_d
  lmelt_var      = lmelt_var_d
  lmulti_layer   = lmulti_layer_d
  lmulti_snow    = lmulti_snow_d
  lseaice        = lseaice_d
  llake          = llake_d
  lemiss         = lemiss_d
  lstomata       = lstomata_d

  lsso           = lsso_d
  ltkesso        = ltkesso_d
  ltkecon        = ltkecon_d
  lexpcor        = lexpcor_d
  ltmpcor        = ltmpcor_d
  lprfcor        = lprfcor_d
  lnonloc        = lnonloc_d
  lcpfluc        = lcpfluc_d
  lconf_avg      = lconf_avg_d
  lradf_avg      = lradf_avg_d
  lcape          = lcape_d  
  lctke          = lctke_d  
  lradtopo       = lradtopo_d
!cloud forcing>
  lcrf           = lcrf_d
!cloud forcing<
  lco2_stab      = lco2_stab_d !HJP 2011-12-19
  lradcst        = lradcst_d

  nhori          = nhori_d
  nradcoarse     = nradcoarse_d
  nincrad        = nincrad_d
  ninctura       = ninctura_d
  nincconv       = nincconv_d
  nincsso        = nincsso_d
  hincrad        = hincrad_d

  itype_trvg     = itype_trvg_d
  itype_evsl     = itype_evsl_d

  itype_gscp     = itype_gscp_d

  itype_sher     = itype_sher_d
  itype_wcld     = itype_wcld_d
  itype_tran     = itype_tran_d
  itype_turb     = itype_turb_d
  itype_synd     = itype_synd_d

  imode_tran     = imode_tran_d
  imode_turb     = imode_turb_d
!<FG: 2M-scheme
  iradpar_cloud  = iradpar_cloud_d
!FG>

  ico2_rad       = ico2_rad_d
  iy_co2_stab    = iy_co2_stab_d

  icldm_rad      = icldm_rad_d
  icldm_tran     = icldm_tran_d
  icldm_turb     = icldm_turb_d

  itype_conv     = itype_conv_d

  itype_aerosol  =  itype_aerosol_d
  itype_root     =  itype_root_d
  itype_heatcond =  itype_heatcond_d
  itype_hydbound =  itype_hydbound_d

  ke_soil        = ke_soil_d
  ke_snow        = ke_snow_d
  czml_soil(:)   = -1.0_wp
  czbot_w_so     = czbot_w_so_d
  nlgw           = nlgw_d
  radlon         = radlon_d
  radlat         = radlat_d

  itype_albedo   = itype_albedo_d

!------------------------------------------------------------------------------
!- Section 3: Input of the namelist values
!------------------------------------------------------------------------------

  READ (nuin, phyctl, IOSTAT=iz_err)
ENDIF

IF (nproc > 1) THEN
  ! distribute error status to all processors
  CALL distribute_values  (iz_err, 1, 0, imp_integers,  icomm_world, ierr)
ENDIF

IF (iz_err /= 0) THEN
  ierrstat = -1
  RETURN
ENDIF

IF (my_world_id == 0) THEN

!------------------------------------------------------------------------------
!- Section 4: Check values for errors and consistency
!------------------------------------------------------------------------------

  ! Check whether the values for the increments are given in hours 
  ! and calculate the values in time steps
  IF ( nincrad /= nincrad_d ) THEN
    ! hour values take priority over time step values
    IF ( hincrad /= hincrad_d ) THEN
      IF (hincrad <= 0.0_wp) THEN
        PRINT *, ' ERROR  *** Wrong value for hincrad: ', hincrad, ' *** '
        PRINT *, '        ***   must be > 0.0 *** '
        ierrstat = 1002
      ENDIF
      nincrad = NINT ( hincrad * 3600.0_wp/dt)
    ELSE
      hincrad = 0.0_wp
    ENDIF
  ELSE  
    IF ( hincrad /= hincrad_d ) THEN
      IF (hincrad <= 0.0_wp) THEN
        PRINT *, ' ERROR  *** Wrong value for hincrad: ', hincrad, ' *** '
        PRINT *, '        ***   must be > 0.0 *** '
        ierrstat = 1002
      ENDIF
    ENDIF
    nincrad = NINT ( hincrad * 3600.0_wp/dt)
  ENDIF
#ifndef MESSY
  hnextrad = hstart
#else
  IF (hstart == 0.0_wp) THEN
     hnextrad = hstart
  ELSE
     ! NOTE: hnextrad is restart attribute to ensure radiation trigger
     !       at exactly the same point in time
  ENDIF
#endif
  nextrad  = NINT ( hnextrad * 3600.0_wp / dt)

!cloud forcing>
  IF ( lcrf .AND. (.NOT. lrad) ) THEN
    lcrf = .FALSE.
    PRINT *, ' WARNING:  *** Radiation scheme is not used,          *** '
    PRINT *, '           *** cannot compute cloud radiative forcing *** '
    PRINT *, '           *** lcrf is automatically reset to FALSE   *** '
  ENDIF
!cloud forcing<

  IF ( (nradcoarse > nboundlines+1) .OR. (nradcoarse < 1) ) THEN
    PRINT *, ' ERROR  *** Wrong value for nradcoarse: ', nradcoarse, ' *** '
    PRINT *, '        ***   must be >= 1 and <= nboundlines+1 !*** '
    ierrstat = 1002
  ENDIF
  IF ( (nradcoarse > 1) .AND. (nboundlines > 3) ) THEN
    PRINT *, ' ERROR  *** nradcoarse > 1 and nboundlines > 3 is not working  *** '
    ierrstat = 1002
  ENDIF
  IF ( (nradcoarse < 2) .AND. lradf_avg ) THEN
    PRINT *, ' ERROR: If nradcoarse<2, lradf_avg not implemented yet. '
    ierrstat = 1002
  ELSEIF ( (nradcoarse > 1) .AND. .NOT.lradf_avg ) THEN
    PRINT *, ' WARNING: IF (nradcoarse > 1), lradf_avg==.TRUE. recommended.'
  ENDIF
  IF ( (nradcoarse > 1) .AND. lradtopo ) THEN
    PRINT *, ' ERROR: If nradcoarse > 1, lradtopo is not possible. '
    ierrstat = 1002
  ENDIF
  IF ( lperi_x .AND. lrad ) THEN
    PRINT *, ' WARNING: lrad=.true. and lperi_x=.true. => constant aerosol and zenith angle in X-dir.,'
    PRINT *, '          taken from domain reference point (as defined via pollon, pollat)!'
  ENDIF
  IF ( lperi_y .AND. lrad ) THEN
    PRINT *, ' WARNING: lrad=.true. and lperi_y=.true. => constant aerosol and zenith angle in Y-dir.,'
    PRINT *, '          taken from domain reference point (as defined via pollon, pollat)!'
  ENDIF

#ifndef TWOMOM_SB
  l_2mom = .FALSE.
  IF ( (itype_gscp < 1) .OR. (itype_gscp > 4) ) THEN
    PRINT *, ' ERROR  *** Wrong value for itype_gscp: ', itype_gscp, ' *** '
    PRINT *, '        ***   must be >= 1 and <= 4 !!!                  *** '
    PRINT *, ' NOTE:  *** No 2-moment scheme compiled in this binary   *** '
    ierrstat = 1002
  ENDIF
#else
  IF ( (itype_gscp < 1) .OR. ( itype_gscp > 4 .and. itype_gscp < 100 ) ) THEN
    PRINT *, ' ERROR  *** Wrong value for itype_gscp: ', itype_gscp, ' *** '
    PRINT *, '        ***   must be >= 1 and <= 4 or >= 100 !!!        *** '
    PRINT *, ' NOTE:  *** 2-moment scheme is compiled in this binary   *** '
    ierrstat = 1002
  ENDIF
  IF (itype_gscp >= 100) THEN
    l_2mom = .TRUE.
    IF (dt > 30.0_wp) THEN
      itype_gscp = itype_gscp_d
      PRINT *, ' ERROR  *** itype_gscp >= 100 (2mom microphysics) should only       ***'
      PRINT *, '        ***   run with dt <= 30 seconds!                            *** '
      ierrstat = 1002
    END IF
    IF (luse_rttov) THEN
      PRINT  *, ' WARNING  *** Satellite images together with 2mom microphysics',          &
                                ' not correctly implemented yet! ***'
    END IF
  ELSE
    l_2mom = .FALSE.
  END IF
#endif

  IF ( (.NOT. lgsp) .AND. (itype_gscp /= itype_gscp_d) ) THEN
    itype_gscp = itype_gscp_d
    PRINT *, ' WARNING:  *** itype_gscp is set to the default *** ',    &
             '           *** because precipitation is turned off ***'
  ENDIF
  IF ( (itype_gscp < 3) .AND. ldiniprec) THEN
    PRINT  *, ' ERROR  *** Initialization of prognostic precipitation ***'
    PRINT  *, '        *** only possible for itype_gscp >= 3          *** '
    ierrstat = 1002
  ENDIF
  IF ( (itype_gscp < 3) .AND. luse_rttov) THEN
    PRINT  *, ' ERROR  *** Without cloud ice, satellite images ',          &
                                           'cannot be computed ***'
    ierrstat = 1002
  ENDIF
  IF (ldiniprec)                                                              &
    PRINT *,' NOTE: *** ldiniprec will be set FALSE if lana_qr_qs is true *** '

  IF (itype_sher.LT.1 .OR.  itype_sher.GT.3) THEN
     itype_sher = itype_sher_d
     PRINT *,' WARNING: *** itype_sher is set to default again *** '
  END IF
  IF (itype_wcld.LT.1 .OR.  itype_wcld.GT.2) THEN
     itype_wcld = itype_wcld_d
     PRINT *,' WARNING: *** itype_wcld is set to default again *** '
  END IF
  IF (itype_tran.LT.1 .OR.  itype_tran.GT.2) THEN
     itype_tran = itype_tran_d
     PRINT *,' WARNING: *** itype_tran is set to default again *** '
  END IF

  IF (lneutral .AND. .NOT.itype_tran==1) THEN
     itype_tran=1
     PRINT *,' WARNING:  *** lneutral only possible in combination with Louise scheme *** '
     PRINT *,'           *** setting itype_tran==1 *** '
  ENDIF

  SELECT CASE( itype_turb )
  CASE( 1 )
    IF (lprog_tke) THEN
      lprog_tke  = .FALSE.
      PRINT *,' WARNING: *** lprog_tke=.true. not possible for itype_turb = 1! *** '
      PRINT *,'          *** lprog_tke is set to .false.!         *** '
    END IF
  CASE( 3 )
    IF (.NOT. l2tls .AND. lprog_tke) THEN
      lprog_tke  = .FALSE.
      PRINT *,' WARNING: *** lprog_tke=.true. only for RK-Scheme! *** '
      PRINT *,'          *** lprog_tke is set to .false.!         *** '
    END IF
  CASE( 5:8 )
    IF (.NOT. l2tls .AND. lprog_tke) THEN
      lprog_tke  = .FALSE.
      PRINT *,' WARNING: *** lprog_tke=.true. only for RK-Scheme! *** '
      PRINT *,'          *** lprog_tke is set to .false.!         *** '
    END IF
    IF (.NOT. l2tls) THEN
      itype_turb = itype_turb_d
      PRINT *,' WARNING: *** itype_turb = ',itype_turb,' only available for RK-Scheme! *** '
      PRINT *,'          *** itype_turb is set to default again *** '
    END IF
  CASE( 11 )
      lprog_tke  = .FALSE.
      l3dturb=.TRUE.
      PRINT *,' WARNING: *** itype_turb 11 sets l3dturb=.TRUE. *** '
  CASE( 100 )
    lprog_tke  = .FALSE.
  CASE default
    lprog_tke  = .FALSE.
    itype_turb = itype_turb_d
    PRINT *,' WARNING: *** no valid value of itype_turb! *** '
    PRINT *,'          *** itype_turb is set to default and lprog_tke to .false. *** '
  END SELECT

  IF (itype_synd.LT.1 .OR.  itype_synd.GT.2) THEN
     itype_synd = itype_synd_d
     PRINT *,' WARNING: *** itype_synd is set to default again *** '
  END IF
  
  IF (imode_tran.LT.1 .OR.  imode_tran.GT.2) THEN
     imode_tran = imode_tran_d
     PRINT *,' WARNING: *** imode_tran is set to default again *** '
  END IF
  IF (imode_turb.LT.0 .OR.  imode_turb.GT.4) THEN
     imode_turb = imode_turb_d
     PRINT *,' WARNING: *** imode_turb is set to default again *** '
  END IF

  IF (icldm_rad.LT.0  .OR. icldm_rad.GT.4 )  THEN
     icldm_rad  = icldm_rad_d
     PRINT *,' WARNING: *** icldm_rad is set to default again *** '
  END IF
     
  IF (icldm_tran.LT.0 .OR. icldm_tran.GT.2)  THEN
     icldm_tran = icldm_tran_d
     PRINT *,' WARNING: *** icldm_tran is set to default again *** '
  END IF
  ! if icldm_turb = -1: real dry scheme: qc is not taken into account
  IF (icldm_turb.LT.-1 .OR. icldm_turb.GT.2) THEN
     icldm_turb = icldm_turb_d
     PRINT *,' WARNING: *** icldm_turb is set to default again *** '
  END IF

  IF (ico2_rad > 10)  ico2_rad  = ico2_rad_d

  ! the prognostic TKE-scheme of Matthias Raschendorfer is not used
  IF (itype_turb.NE.3) THEN
  IF (itype_turb.NE.11) THEN ! not Smagorinsky-LES
    ! this is for 5:8
    imode_turb = 0
    itype_tran = MIN(itype_tran,2)
    itype_wcld = 1
    itype_sher = 1
    icldm_turb = 0
    !(WL;2010)b
  ELSE  !Smagorinsky-Lilly
    ! vertical diffusion has to be calculated (mostly) implicitly, for imode_turb>=2 an explicit tendency would result from turbdiff.incf, which 
    ! is still called in case of itype_tran=2, therefore set
    imode_turb = MIN(imode_turb,1)
    ! both transfer schemes are available, but for itype_tran=2 and itype_wcld=2
    ! turbdiff.incf would be needed to compute rcld, thus this combination will not work
    ! setting
    itype_tran = MIN(itype_tran,2)
    IF (itype_tran==2 .and. itype_wcld==2) THEN
      PRINT *,' WARNING: *** itype_wcld is set to 1 *** '
      itype_wcld=1
    ENDIF
  ENDIF
    !(WL;2010)e
  END IF

!WL2011b
 ! remove these forcings of the switches
  ! the Louis-scheme for transfer is used
!  IF (itype_tran.EQ.1) THEN
!    imode_tran = 1
!    icldm_tran = 0
!    itype_synd = 1
!  ENDIF
!WL2011e

  IF (.NOT. lmulti_layer) THEN
    PRINT *,' ERROR  *** lmulti_layer=.false. no longer supported    *** '
    PRINT *,' ERROR  *** If lmulti_layer=.false. is desired,         *** '
    PRINT *,' ERROR  *** you will have to switch off this error      *** '
    PRINT *,' ERROR  *** check manually in organize_physics.f90!     *** '
    ierrstat = 1099
  END IF
  
  IF (ltkesso .AND. .NOT.lsso) THEN
    PRINT *,' WARNING: *** ltkesso cannot be active, since SSO scheme is not running *** '
  END IF

  IF (ltkecon) THEN
    IF (.NOT. lconv) THEN
      PRINT *,' WARNING: *** ltkeconv cannot be active, since convection is not running *** '
    ELSEIF (itype_conv == 2 .OR. itype_conv == 3) THEN
      PRINT *,' WARNING: *** ltkeconv is not yet supported by convection according IFS or Kain-Fritsch *** '
    END IF
  END IF

  IF (lmulti_layer) THEN
    ALLOCATE (czmls(1:ke_soil+1), STAT=ierr)
    ALLOCATE (czhls(0:ke_soil+1), STAT=ierr)
    ALLOCATE (msoilgrib(0:ke_soil+1), STAT=ierr)
                    ! (careful: the level k=1 will be coded with 1,
                    !           but is in the depth of 0.5 cm!)

    ! Check, how many soil levels have been specified
    invar = COUNT(czml_soil(:) /= -1.0_wp)
    IF (invar == 0) THEN
      ! no level specifications have been read
      IF (ke_soil == ke_soil_d) THEN
        ! use the default
        PRINT *,'  *** Default specifications of soil main levels are used    *** '
        czmls(1:ke_soil+1) = czml_soil_d(1:ke_soil+1)
      ELSE
        PRINT *,' ERROR  *** no specifications of soil levels,    *** '
        PRINT *,' ERROR  *** but using wrong default              *** ', &
                  ke_soil_d, ke_soil
        ierrstat = 1002
      ENDIF
    ELSE
      IF (ke_soil+1 == invar) THEN
        lzequiv = .TRUE.
        DO k = 1, ke_soil+1
          IF (czml_soil(k) /= czml_soil_d(k)) THEN
            lzequiv = .FALSE.
          ENDIF
        ENDDO
        IF (lzequiv) THEN
          PRINT *,'  *** Default specifications of soil main levels are used *** '
          czmls(1:ke_soil+1) = czml_soil_d(1:ke_soil+1)
        ELSE
          PRINT *,'  *** WARNING: Own specifications of soil main levels are used *** '
          PRINT *,'  ***          These have to correspond to the levels  of the  *** '
          PRINT *,'  ***          coarse grid model!                              *** '
          czmls(1:ke_soil+1) = czml_soil(1:ke_soil+1)
        ENDIF
      ELSE
        PRINT *,' ERROR  *** wrong number of specifications ',           &
                'for soil levels  *** ', ke_soil, invar
        ierrstat = 1002
      ENDIF
    ENDIF

    IF (ierrstat == 0) THEN
      ! compute grib coded values of the depth of main soil levels
      msoilgrib(0) = 0_iintegers
      DO i = 1, ke_soil+1
        msoilgrib(i) = NINT (100 * czmls(i)+1.0E-7_wp)
      ENDDO
    ENDIF

    ! determine depth of half levels out of czmls
    czhls(0) = 0.0_wp
    DO k = 1, ke_soil+1
      czhls(k) = 2.0_wp * czmls(k) - czhls(k-1)
    ENDDO

    czbot_w_so = MIN(czbot_w_so, czhls(ke_soil))
    ibot_w_so = ke_soil
    DO i=1,ke_soil+1
      IF (czhls(i) <= czbot_w_so) ibot_w_so=i
    ENDDO
  ENDIF

  IF (lconv) THEN
#ifdef CLM
    IF ( (itype_conv /= 0) .AND. (itype_conv /= 2) .AND. (itype_conv /= 3) ) THEN
      PRINT  *, ' ERROR  *** Wrong type of convection scheme: ', itype_conv
      PRINT  *, '        *** Must be 0, 2 or 3!'
      ierrstat = 1002
    ENDIF
    IF ( itype_conv == 2 .and. lconf_avg ) THEN
      lconf_avg = .FALSE.
      PRINT  *
      PRINT *, '*** WARNING: Namelist parameter lconf_avg has a different effect ***'
      PRINT *, '***          on the IFS scheme than with the Tiedtke scheme.     ***'
      PRINT *, '***          Only lconf_avg=.FALSE. has yet been tested.         ***'
      PRINT *, '***          Thus, lconf_avg has been set to .FALSE.             ***'
    ENDIF
#else
    IF ( (itype_conv /= 0) .AND. (itype_conv /= 3) ) THEN
      PRINT  *, ' ERROR  *** Wrong type of convection scheme: ', itype_conv
      PRINT  *, '        *** Must be 0 or 3!'
      ierrstat = 1002
    ENDIF
#endif
    IF (.NOT.ltur) THEN
       lctke=.FALSE.
    END IF
  ENDIF

  IF ( (itype_albedo  < 1) .OR. (itype_albedo  > 4) ) THEN
    PRINT  *, ' ERROR  *** Wrong type of albedo scheme: ', itype_albedo
    PRINT  *, '        *** Must be between 1 and 4!'
    ierrstat = 1002
  ENDIF

  IF ( (itype_albedo == 4) .AND. (.NOT. lforest) ) THEN
    PRINT  *, ' ERROR  *** albedo scheme 4 only possible with lforest=.TRUE.!'
    ierrstat = 1002
  ENDIF

  IF ( (itype_aerosol < 1) .OR. (itype_aerosol > 2) ) THEN
    PRINT  *, ' ERROR  *** Wrong type of aerosol scheme: ', itype_aerosol
    PRINT  *, '        *** Must be between 1 and 2!'
    ierrstat = 1002
  ENDIF

  IF ( (lperi_x .OR. lperi_y) .AND. itype_aerosol > 1 ) THEN
    PRINT  *, ' ERROR  *** Wrong type of aerosol scheme: ', itype_aerosol
    PRINT  *, '        *** For periodic BCs only itype_aerosol = 1 possible!'
    ierrstat = 1002
  ENDIF

  IF ( (itype_root    < 1) .OR. (itype_root    > 2) ) THEN
    PRINT  *, ' ERROR  *** Wrong type of root distribution: ', itype_root
    PRINT  *, '        *** Must be between 1 and 2!'
    ierrstat = 1002
  ENDIF

  IF ( (itype_heatcond < 1) .OR. (itype_heatcond > 2) ) THEN
    PRINT  *, ' ERROR  *** Wrong type of soil heat conductivity: ', itype_heatcond
    PRINT  *, '        *** Must be between 1 and 2!'
    ierrstat = 1002
  ENDIF

  IF ( (itype_hydbound /= 1) .AND. (itype_hydbound /= 3) ) THEN
    PRINT  *, ' ERROR  *** Wrong type of hydraulic lower boundary: ', itype_hydbound
    PRINT  *, '        *** Must be 1 or 3! (2 is not implemented yet)'
    ierrstat = 1002
  ENDIF

  IF (ltur .AND. l3dturb) THEN
    ! Add a warning, because field qvt_diff has been eliminated in Version 4.23!!
    PRINT *,'  *** WARNING: Explicit turbulent mixing tendencies from       *** '
    PRINT *,'  ***          horizontal mixing (l3dturb=TRUE) not accounted  *** '
    PRINT *,'  ***          for moisture divergence (dqvdt)!                *** '
  ENDIF

ENDIF

!------------------------------------------------------------------------------
!- Section 5: Distribute variables to all nodes
!------------------------------------------------------------------------------

IF (nproc > 1) THEN

  IF (my_world_id == 0) THEN
    intbuf ( 1) = nincrad
    intbuf ( 2) = ninctura
    intbuf ( 3) = nincconv
    intbuf ( 4) = nlgw

    intbuf ( 5) = itype_trvg
    intbuf ( 6) = itype_evsl

    intbuf ( 7) = itype_gscp

    intbuf ( 8) = itype_sher
    intbuf ( 9) = itype_wcld
    intbuf (10) = itype_tran
    intbuf (11) = itype_turb
    intbuf (12) = itype_synd

    intbuf (13) = imode_tran
    intbuf (14) = imode_turb

    intbuf (15) = icldm_rad
    intbuf (16) = icldm_tran
    intbuf (17) = icldm_turb
    intbuf (18) = ke_soil
    intbuf (19) = ico2_rad
    intbuf (20) = ibot_w_so
    intbuf (21) = nradcoarse
    intbuf (22) = nhori
    intbuf (23) = nextrad
    intbuf (24) = itype_conv
    intbuf (25) = nincsso
    intbuf (26) = ke_snow
    intbuf (27) = itype_aerosol
    intbuf (28) = itype_root
    intbuf (29) = itype_heatcond
    intbuf (30) = itype_hydbound
    intbuf (31) = iy_co2_stab
    intbuf (32) = itype_albedo
    !<FG: 2M-scheme
    intbuf (33) = iradpar_cloud
    !FG>
    logbuf ( 1) = lrad
    logbuf ( 2) = ltur
    logbuf ( 3) = lconv
    logbuf ( 4) = lgsp
    logbuf ( 5) = lsoil
    logbuf ( 6) = ltkecon
    logbuf ( 7) = lexpcor
    logbuf ( 8) = ltmpcor
    logbuf ( 9) = lprfcor
    logbuf (10) = lnonloc
    logbuf (11) = lcpfluc
    logbuf (12) = lcape
    logbuf (13) = lctke
    logbuf (14) = lconf_avg
    logbuf (15) = lmelt
    logbuf (16) = lmelt_var
    logbuf (17) = lmulti_layer
    logbuf (18) = ldiniprec
    logbuf (19) = l3dturb
    logbuf (20) = lprog_tke
    logbuf (21) = limpltkediff
    logbuf (22) = lforest
    logbuf (23) = lconv_inst
    logbuf (24) = llake
    logbuf (25) = l3dturb_metr
    logbuf (26) = lradf_avg
    logbuf (27) = lradtopo
    logbuf (28) = lsso
    logbuf (29) = ltkesso
    logbuf (30) = lseaice
    logbuf (31) = lmulti_snow
    logbuf (32) = lemiss
    logbuf (33) = lstomata
    logbuf (34) = lco2_stab
    logbuf (35) = l_2mom
    logbuf (36) = lradcst
    logbuf (37) = lkhdef3d
    logbuf (38) = lisotrop
    logbuf (39) = lneutral
    !cloud forcing>
    logbuf (40) = lcrf
    !cloud forcing<
    logbuf (41) = lINprog
    logbuf (42) = lCCNprog

    realbuf( 1) = czbot_w_so
    realbuf( 2) = hincrad
    realbuf( 3) = hnextrad
    realbuf( 4) = radlon
    realbuf( 5) = radlat
    realbuf( 6) = Tdiff_IN
    realbuf( 7) = Tmax_IN
    realbuf( 8) = Na_IN
    realbuf( 9) = CCN_ship_N
    realbuf(10) = Na_IN_pert
    realbuf(11) = tperturb
    IF (lmulti_layer) THEN
      DO i = 1, ke_soil+1
        realbuf(11+i) = czmls(i)
      ENDDO
      DO i = 0, ke_soil+1
        realbuf(13+ke_soil+i) = czhls(i)
      ENDDO
    ELSE
      realbuf( 12:43) = 0.0_wp
    ENDIF
    
  ENDIF

  CALL distribute_values (intbuf, 33, 0, imp_integers, icomm_world, ierr)
  CALL distribute_values (logbuf, 42, 0, imp_logical,  icomm_world, ierr)
  CALL distribute_values (realbuf,43, 0, imp_reals,    icomm_world, ierr)

  IF (my_world_id /= 0) THEN
    nincrad      = intbuf ( 1)
    ninctura     = intbuf ( 2)
    nincconv     = intbuf ( 3)
    nlgw         = intbuf ( 4)

    itype_trvg   = intbuf ( 5)
    itype_evsl   = intbuf ( 6)

    itype_gscp   = intbuf ( 7)

    itype_sher   = intbuf ( 8)
    itype_wcld   = intbuf ( 9)
    itype_tran   = intbuf (10)
    itype_turb   = intbuf (11)
    itype_synd   = intbuf (12)

    imode_tran   = intbuf (13)
    imode_turb   = intbuf (14)

    icldm_rad    = intbuf (15)
    icldm_tran   = intbuf (16)
    icldm_turb   = intbuf (17)
    ke_soil      = intbuf (18)
    ico2_rad     = intbuf (19)
    ibot_w_so    = intbuf (20)
    nradcoarse   = intbuf (21)
    nhori        = intbuf (22)
    nextrad      = intbuf (23)
    itype_conv   = intbuf (24)
    nincsso      = intbuf (25)
    ke_snow      = intbuf (26)
    itype_aerosol= intbuf (27)
    itype_root   = intbuf (28)
    itype_heatcond=intbuf (29)
    itype_hydbound=intbuf (30)
    iy_co2_stab  = intbuf (31)
    itype_albedo = intbuf (32)
!<FG: 2M-scheme 
    iradpar_cloud= intbuf (33)
!FG>

    lrad         = logbuf ( 1)
    ltur         = logbuf ( 2)
    lconv        = logbuf ( 3)
    lgsp         = logbuf ( 4)
    lsoil        = logbuf ( 5)

    ltkecon      = logbuf ( 6)
    lexpcor      = logbuf ( 7)
    ltmpcor      = logbuf ( 8)
    lprfcor      = logbuf ( 9)
    lnonloc      = logbuf (10)
    lcpfluc      = logbuf (11)

    lcape        = logbuf (12)
    lctke        = logbuf (13)
    lconf_avg    = logbuf (14)
    lmelt        = logbuf (15)
    lmelt_var    = logbuf (16)
    lmulti_layer = logbuf (17)
    ldiniprec    = logbuf (18)
    l3dturb      = logbuf (19)
    lprog_tke    = logbuf (20)
    limpltkediff = logbuf (21)
    lforest      = logbuf (22)
    lconv_inst   = logbuf (23)
    llake        = logbuf (24)
    l3dturb_metr = logbuf (25)
    lradf_avg    = logbuf (26)
    lradtopo     = logbuf (27)
    lsso         = logbuf (28)
    ltkesso      = logbuf (29)
    lseaice      = logbuf (30)
    lmulti_snow  = logbuf (31)
    lemiss       = logbuf (32)
    lstomata     = logbuf (33)
    lco2_stab    = logbuf (34)
    l_2mom       = logbuf (35)
    lradcst      = logbuf (36)
    lkhdef3d     = logbuf (37)
    lisotrop     = logbuf (38)
    lneutral     = logbuf (39)
!cloud forcing>
    lcrf                  = logbuf (40)
!cloud forcing<
    lINprog      = logbuf (41)
    lCCNprog     = logbuf (42)

    czbot_w_so   = realbuf( 1)
    hincrad      = realbuf( 2)
    hnextrad     = realbuf( 3)
    radlon       = realbuf( 4)
    radlat       = realbuf( 5)
    Tdiff_IN     = realbuf( 6)
    Tmax_IN      = realbuf( 7)
    Na_IN        = realbuf( 8)
    CCN_ship_N   = realbuf( 9)
    Na_IN_pert   = realbuf(10)
    tperturb     = realbuf(11)

    IF (lmulti_layer) THEN
      ALLOCATE (czmls(1:ke_soil+1), STAT=ierr)
      ALLOCATE (czhls(0:ke_soil+1), STAT=ierr)
      ALLOCATE (msoilgrib(0:ke_soil+1), STAT=ierr)

      ! determine depth of half levels out of czmls
      msoilgrib(0) = 0_iintegers
      czhls    (0) = 0.0_wp
      DO i = 1, ke_soil+1
        czmls(i) = realbuf(11+i)
        czhls(i) = 2.0_wp * czmls(i) - czhls(i-1)
        msoilgrib(i) = NINT (100 * czmls(i)+1.0E-7_wp)
      ENDDO
      DO i = 0, ke_soil+1
        czhls(i) = realbuf(13+ke_soil+i)
      ENDDO
    ENDIF
  ENDIF

ENDIF

! Set flag l_dzeta_d_needed
IF (l3dturb_metr) THEN
  l_dzeta_d_needed = .TRUE.
END IF

! Set lprog_qi now internally, depending on itype_gscp
IF (itype_gscp < 3) THEN
  lprog_qi = .FALSE.
ELSE
  lprog_qi = .TRUE.
ENDIF

!------------------------------------------------------------------------------
!- Section 6: Output of the namelist variables and their default values
!------------------------------------------------------------------------------

IF (my_world_id == 0) THEN

  WRITE (nuspecif, '(A2)')  '  '
  WRITE (nuspecif, '(A23)') '0     NAMELIST:  phyctl'
  WRITE (nuspecif, '(A23)') '      -----------------'
  WRITE (nuspecif, '(A2)')  '  '
  WRITE (nuspecif, '(T7,A,T21,A,T39,A,T58,A)') 'Variable', 'Actual Value',   &
                                               'Default Value', 'Format'

  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lgsp'   ,lgsp   ,lgsp_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lINprog'   ,lINprog   ,lINprog_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &                  
                                           'lCCNprog'   ,lCCNprog   ,lCCNprog_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                           'Tdiff_IN',Tdiff_IN,Tdiff_IN_d,' R '
  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                           'Tmax_IN',Tmax_IN,Tmax_IN_d,' R '
  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                           'Na_IN',Na_IN,Na_IN_d,' R '
  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                           'tperturb',tperturb,tperturb_d,' R '
  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                           'Na_IN_pert',Na_IN_pert,Na_IN_pert_d,' R '
  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                           'CCN_ship_N',CCN_ship_N,CCN_ship_N_d,' R '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                            'ldiniprec'   ,ldiniprec   ,ldiniprec_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lrad',   lrad,   lrad_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                       'itype_aerosol', itype_aerosol, itype_aerosol_d,' I '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                     'lemiss   ',lemiss   ,lemiss_d   ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lforest',lforest,lforest_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'ltur   ',ltur   ,ltur_d   ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                      'l3dturb', l3dturb, l3dturb_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                       'lkhdef3d', lkhdef3d, lkhdef3d_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                       'lisotrop', lisotrop, lisotrop_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                       'lneutral', lneutral, lneutral_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                       'l3dturb_metr', l3dturb_metr, l3dturb_metr_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                'lprog_tke', lprog_tke, lprog_tke_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                       'limpltkediff', limpltkediff, limpltkediff_d,   ' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lconv  ',lconv  ,lconv_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                'itype_conv', itype_conv, itype_conv_d,' I '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                  'lconv_inst',lconv_inst,lconv_inst_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lsoil  ',lsoil  ,lsoil_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lseaice',lseaice,lseaice_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'llake  ',llake  ,llake_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lsso   ',lsso   ,lsso_d   ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lmelt  ',lmelt  ,lmelt_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                 'lmelt_var',lmelt_var  ,lmelt_var_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                         'lmulti_layer ',lmulti_layer,lmulti_layer_d  ,' L '

  IF (lmulti_layer) THEN
    ! Write specification for soil levels
    WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                    &
                                        'ke_soil ',ke_soil ,ke_soil_d, ' I '
    WRITE (nuspecif, '(T10,A)') 'Main levels of the soil layers'
    WRITE (nuspecif, '(T10,A)') '                  (m)           (cm)'
    WRITE (nuspecif, '(A,I12)') '              0:               ', msoilgrib(0)
    DO i = 1, ke_soil+1
      WRITE (nuspecif, '(I15,A,F12.4,I12)') i, ':   ',czmls(i), msoilgrib(i)
    ENDDO
    WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                    &
                                  'czbot_w_so',czbot_w_so,czbot_w_so_d,' R '
  ENDIF

  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                         'lmulti_snow  ',lmulti_snow,  lmulti_snow_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                        'ke_snow ',ke_snow ,ke_snow_d, ' I '
  WRITE (nuspecif, '(T8,A,T27,I6   ,T40,I12  ,T59,A3)')                      &
              'itype_heatcond',itype_heatcond  ,itype_heatcond_d   ,   ' I '
  WRITE (nuspecif, '(T8,A,T27,I6   ,T40,I12  ,T59,A3)')                      &
              'itype_hydbound',itype_hydbound  ,itype_hydbound_d   ,   ' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                          'itype_root',itype_root  ,itype_root_d   ,   ' I '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                'lstomata   ',lstomata   ,lstomata_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'ltkesso',ltkesso,ltkesso_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'ltkecon',ltkecon,ltkecon_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lexpcor',lexpcor,lexpcor_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'ltmpcor',ltmpcor,ltmpcor_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lprfcor',lprfcor,lprfcor_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lnonloc',lnonloc,lnonloc_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lcpfluc',lcpfluc,lcpfluc_d,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                   'lconf_avg',lconf_avg,lconf_avg_d  ,' L '
!cloud forcing>
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                   'lcrf',lcrf,lcrf_d  ,' L '
!cloud forcing<
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                   'lradf_avg',lradf_avg,lradf_avg_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lcape  ',lcape  ,lcape_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                           'lctke  ',lctke  ,lctke_d  ,' L '

  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                           'hincrad',hincrad,hincrad_d,' R '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                           'nincrad',nincrad,nincrad_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                           'nradcoarse',nradcoarse,nradcoarse_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                        'ninctura',ninctura,ninctura_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                        'nincconv',nincconv,nincconv_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                           'nincsso',nincsso,nincsso_d,' I '

  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'itype_trvg',itype_trvg,itype_trvg_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'itype_evsl',itype_evsl,itype_evsl_d,' I '

  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'itype_gscp',itype_gscp,itype_gscp_d,' I '

  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'itype_sher',itype_sher,itype_sher_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'itype_wcld',itype_wcld,itype_wcld_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'itype_tran',itype_tran,itype_tran_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'itype_turb',itype_turb,itype_turb_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'itype_synd',itype_synd,itype_synd_d,' I '

  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'imode_tran',imode_tran,imode_tran_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'imode_turb',imode_turb,imode_turb_d,' I '
!<FG:2M-scheme
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'iradpar_cloud',iradpar_cloud,iradpar_cloud_d,' I '
!FG>
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'ico2_rad',ico2_rad,ico2_rad_d,      ' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'iy_co2_stab',iy_co2_stab,iy_co2_stab_d,' I '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                    'lco2_stab',lco2_stab,lco2_stab_d,   ' L '

  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'icldm_rad',icldm_rad,icldm_rad_d,   ' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'icldm_tran',icldm_tran,icldm_tran_d,' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                    'icldm_turb',icldm_turb,icldm_turb_d,' I '

  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                        'nlgw    ',nlgw    ,nlgw_d,    ' I '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                      'lradtopo',lradtopo,lradtopo_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,L12  ,T40,L12  ,T59,A3)')                      &
                                      'lradcst',lradcst,lradcst_d  ,' L '
  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                  'radlon',radlon,radlon_d,' R '
  WRITE (nuspecif, '(T8,A,T21,F12.4,T40,F12.4,T59,A3)')                      &
                                  'radlat',radlat,radlat,' R '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                                        'nhori', nhori, nhori_d,       ' I '
  WRITE (nuspecif, '(T8,A,T21,I12  ,T40,I12  ,T59,A3)')                      &
                       'itype_albedo', itype_albedo, itype_albedo_d,' I '
  WRITE (nuspecif, '(A2)')  '  '

ENDIF

!------------------------------------------------------------------------------
!- End of the Subroutine
!------------------------------------------------------------------------------

END SUBROUTINE input_phyctl

!==============================================================================

!------------------------------------------------------------------------------
! End of module procedure organize_physics
!------------------------------------------------------------------------------

END SUBROUTINE organize_physics
