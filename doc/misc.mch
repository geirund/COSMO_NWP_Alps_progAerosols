5.0n5                                                      01.05.2014
=====================================================================

- bugfixes in data assimilation
- bugfixes for input prefetching
- small technical changes in lhn

  Tag: cosmo5.0.3.2
  Results: bit-identical results except for rare cases when using
           data assimilation
  Source: src_mult_local.f90
          src_obs_operator_conv.f90
          src_obs_qc_conv.f90
          src_lheat_nudge.f90
          organize_data.f90

- Consolidation of single precision version
  Tag: <none>
  Source: <changes in many source files>
  Results: Small changes due to change of floating point type
           at several places. According to Dani, no meteorologically
           relevant differences.

5.0n3                                                      02.04.2014
=====================================================================

- Implementation of advection of TKE
  Tag: <none>
  Source: src_advection_rk.f90
          lmorg.f90
          turbulence_interface.f90
          src_allocation.f90
          turbulence_diff.f90
          src_slow_tendencies_rk.f90
          data_fields.f90
          organize_physics.f90
  Results: No changes (if disabled, i.e. lprog_tke=.FALSE.)

- Implementation of SPPT (stochastic perturbation of physical tendencies)
  in this version of code.
  
  Tag: <none>
  Source: data_fields.f90
          data_runcontrol.f90
          lmorg.f90
          organize_eps.f90
          organize_physics.f90
          src_allocation.f90
          src_gscp.f90
          src_obs_cdfin_print.f90
          src_runge_kutta.f90
          src_setup.f90
          src_setup_vartab.f90
          src_slow_tendencies_rk.f90
          src_stoch_physics.f90
  Results: No changes (if disabled, i.e. lsppt=.FALSE.)

5.0n2                                                      04.03.2014
=====================================================================

- c_int in environment.f90 needed a USE statement from iso_c_binding
  Tag: <none>                                 Source: environment.f90
  Results: No changes

5.0n1                                                      03.02.2014
=====================================================================

NOTE: This version has been compiled with PGI, GNU and Cray and the PGI debugging
      version has been tested for a COSMO-7 and COSMO-2 simulation.

- print a WARNING if advection limiter is active and report the number of
  gridpoints which have been affected by the limiter.
  Tag: <none>
  Source: src_advection_rk.f90
  Results: No changes

- implement targeted diffusion to eliminate cold pools (after Michael Baldauf)
  using a new namelist switch in the DYNCTL namelist l_diff_cold_pools=.TRUE./.FALSE.
  which is off by default. If switched on, a diffusion operator is applied to
  any gridpoints differing more than a threshold of 10 K to the average of its
  neighbouring 4 gridpoints.
  Tag: <none>
  Source: organize_dynamics.f90
          src_advection_rk.f90
          hori_diffusion.f90
          data_runcontrol.f90
  Results: Change if l_diff_cold_pools=.TRUE. and diffusion is actived

- implement lock file mechanism for OSM. A new namelist flag in the IOCTL block
  named llockfiles=.true./.false. has been introduced. Note that lock files are
  only used for standard grib1 model output (model-, p-, z-, satellite-level output
  and that all other I/O (e.g. latent heat nudging, grib_api, ...) will not use lock
  files
  Tag: <none>
  Source: netcdf_io.f90
          src_output.f90
          src_input.f90
          mpe_io2.f90
          data_io.f90
          io_utilities.f90
          src_lheat_nudge.f90
          organize_data.f90
          src_sfcana.f90
  Results: No changes

- implement cold pool check (warnings are issued if model temperatures
  differ more than a specified threhold from the 1976 standard atmosphere)
  Tag: <none>
  Source: utilities.f90
          lmorg.f90
  Results: No changes (only to stdout)

- implement wrapper subroutines for all RTTOV7 subroutines
  - except RTTOV7_MPI_settings (but replace imp_reals by imp_double)
  - wrapper subroutines reside in new file rttov7_dp_wrapper.f90
    (only code so far, no header comments etc.)
  - wrapper subroutines convert in- and output data to and from double
    to working precision, calling the RTTOV7 subroutine in double precision
  Tag: <none>
  Source: rttov7_dp_wrapper.f90
          data_satellites.f90
          organize_satellites.f90
          src_sat_tbs.f90
          src_setup.f90
  Results: No changes (only for RTTOV7 output fields)

- add parameters imp_single and imp_double (SP and DP REAL kind for MPI)
  Tag: <none>
  Source: data_parallel.f90
          environment.f90
  Results: No changes

- Added option to perturb prognostic fields at start or on every timestep
  using a specified amplitude (two new namelist parameters itype_pert and
  rperturb). See m_perturb.f90 for more information.
  Tag: <none>
  Source: src_setup.f90
          lmorg.f90
          data_runcontrol.f90
          m_perturb.f90
  Results: No changes (in case itype_pert=0, which is the default)

- Modified lmorg.f90 to give correct executable name
  Tag: <none>  
  Source: lmorg.f90
  Results: No changes

- Bugfix in trcr_setup_vartab method in src_tracer.f90. Wrong index for
  accessing the data field has been used (this only worked due to the current
  correspondence between tracer ID and data index)
  Tag: <none>
  Source: src_tracer.f90
  Results: no changes

- Implement SLEVE2 vertical coordinate transformation (ivctype=4)
  Tag: <none>
  Source: io_metadata.f90
          netcdf_io.f90
          organize_data.f90
          src_artifdata.f90
          src_input.f90
          src_output.f90
          vgrid_refatm_utils.f90
  Results: no changes

- Implement a version capable of running both in single and double precision
  Tag: <none>
  Source: <almost all source files concerned>
  Results: Changes in all fields

- Check for not a number (NaN) values on every timestep and abort model simulation
  if such a value is encountered
  Tag: <none>
  Source: lmorg.f90
          utilities.f90
  Results: no changes

- Bugfix for writing analysis in NetCDF format with correct timestamp
  Tag: <none>
  Source: netcdf_io.f90
  Results: Timestamp is different in case of yform_write='ncdf' and lanalysis=.TRUE.

- Modularize the waiting function (from the ready mechanism in order to prepare
  for a later extension to lock-files.
  Tag: <none>
  Source: src_input.f90
  Results: no changes

- Remove fsleep() call to DWDGRIBLIB and add an C-interface in utilities.f90
  in order to call the system sleep() call via a wrapper.
  Tag: <none>
  Source: utilities.f90
          environemnt.f90
          src_input.f90
  Results: no changes

- Remove explicit calls to open files from LHN and SFCANA code and use 
  io_utilities.f90 instead
  Tag: <none>
  Source: src_lheat_nudge.f90
          src_sfcana.f90
  Results: no changes

- Introduce temperature advection limiter also for p'-T'-dynamics as
  well as for Theta'-dynamics (it now works for all settings of itheta_adv=0,1,2)
  (Note: This is currently in use for COSMO-1)
  Tag: fuo_tadvlimit
  Source: organize_dynamics.f90
          src_advection_rk.f90
  Results: no changes (if ltadv_limiter=F or itheta_adv = 2)

- Overwrite old restart files
  Tag: fuo_restart 
  Source: io_utilities.f90
  Results: No changes

- Updates of Makefile and Options files

- Added missing prototypes in dummy_mpi.f90 in order to enable compilation
  without MPI implementation
  Tag: <none>
  Source: dummy_mpi.f90
  Results: no changes

- Bugfix in check whether vertical coordinate parameters in gds are stored
  as coded reals or directly in form of integers.
  Tag: <none>
  Source: src_input.f90
          io_utilities.f90
  Results: no changes

- Disable abort upon error in trying to open LHN data files (since LHN data
  files are generated on-the-fly at MeteoSwiss, this modification is required)
  Tag: <none>
  Source: src_lheat_nudge.f90
  Results: no changes

- Bugfix in implicit conversion from real to integer and missing _ireals in
  the subroutine sum_DDI
  Tag: <none>
  Source: numeric_utilities_rk.f90
  Results: no changes (for applications which don't use SL-advection)

- Removed superfluous code lines which have been forgotten by Astrid Kerkweg
  and commented out some print statements which lead to excessive stdout on
  massively parallel architectures
  Tag: <none>
  Source: src_tracer.f90
          vgrid_refatm_utils.f90

- Introduced bug in fast_waves_rk.f90 which only accounts for 50% of the terrain
  heigh in the computation of the divergence in the fast-waves solver. This is
  required for COSMO-2, since the model would crash with the bug-free version.
  Tag: fuo_hack
  Source: fast_waves_rk.f90

