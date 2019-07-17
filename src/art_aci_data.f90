!
! Data and types for aerosol cloud interaction
!
!------------------------------------------------------------------------------

MODULE art_aci_data

!------------------------------------------------------------------------------
!
! Description: As the data types declared here are commonly used by the modules
!              art_aci and art_m7_aci, they needed to be stored in a separate
!              module.
!
!  Current Code Owner: Franziska Glassmeier (IACETH)
!  phone:  
!  fax:    
!  email:  franziska.glassmeier@env.ethz.ch
!
! History:
! Version    Date       Name
! ---------- ---------- ----
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================

USE data_parameters,    ONLY: wp, iintegers

IMPLICIT NONE

INTEGER (KIND=iintegers), PARAMETER   ::  &
         ! nmodes=16,                      &
          !GE
          nmodes=17,                      &
          !GE
          CLOUD=13,                       &
          MINCCN=14,                      &
          CCN_A=15,                       &
          CCN_B=16,                       &
          CCN_C=17

INTEGER (KIND=iintegers) :: act_indices(nmodes)

TYPE t_mode_aci_data
  LOGICAL :: do_activ
  LOGICAL :: l_koehler

  REAL  (KIND=wp) :: sigma

  REAL  (KIND=wp) :: number        & 
                       ,diameter       &
                       ,surface        &
                       ,dissfac_mean   &
                       ,rho_mean       &
                       ,molweight_mean &
                       ,solmassfr      &
                       ,rhosol_mean    &
                       ,rhoinsol_mean
END TYPE t_mode_aci_data

END MODULE art_aci_data
