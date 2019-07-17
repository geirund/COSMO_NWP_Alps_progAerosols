MODULE m_perturb
!-------------------------------------------------------------------------------
!
! Description:
!   This module provides tools to add random perturbation to the prognostic
!   fields. This is usefull to define threashold values for rounding error
!
! Current Code Owner: MeteoSwiss, Oliver Fuhrer
!  phone:  +41  44  256 9359
!  fax:    +XX  XX  XXX XXXX
!  email:  oliver.fuhrer@meteoswiss.ch
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! VX_XX        2013/10/07 Xavier Lapillonne
!  First version, adapted from Oliver's ensemble module
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================

! Modules used:
USE data_parameters,   ONLY :   &
    wp,    & ! KIND-type parameter for real variables
    iintegers, & ! KIND-type parameter for standard integer variables
    rprecision   ! precision of 1.0 in current floating point format

USE data_parallel,     ONLY :   &
     my_cart_id

USE data_modelconfig,  ONLY :   &
  idt_qv, idt_qc, idt_qr, idt_qs, idt_qi, idt_qg, idt_qh

USE data_fields,       ONLY :   &
  ! prognostic variables & microphysics
  u,              & ! zonal wind speed                              ( m/s )
  v,              & ! meridional wind speed                         ( m/s )
  w,              & ! vertical wind speed (defined on half levels)  ( m/s )
  t,              & ! temperature                                   (  k  )
  pp,             & ! deviation from the reference pressure         ( pa  )
  tke               ! turbulent kinetic energy (on half levels)     (m2/s2)

USE environment,              ONLY :  &
  model_abort

USE data_tracer ,             ONLY :  &
  T_ERR_NOTFOUND

USE src_tracer ,              ONLY :  &
  trcr_get, trcr_errorstr

IMPLICIT NONE

! Global module variables
LOGICAL, SAVE :: lseedinit=.FALSE.

!==============================================================================
! Module procedures
!==============================================================================

INTERFACE fld_perturb
   MODULE PROCEDURE fld_perturb_1d, fld_perturb_2d, fld_perturb_3d
END INTERFACE

!==============================================================================

CONTAINS

SUBROUTINE perturb_prognostic_fields(ntstep, itype_perturb, rperturb, nx, idbg)  

  ! Description
  !   This routine perturbs the prognostic fields with a perturbation of the following style
  !
  !     field = (1 + epsilon) * field
  !   where epsilon can either be chose relative to the least significant bit (rperturb<0.0)
  !   or as an absolute perturbation value (rperturb>0.0).
  !
  IMPLICIT NONE
  ! routine arguments
  INTEGER(KIND=iintegers), INTENT(IN) :: &
       ntstep,         &   ! current time step
       nx,             &   ! time level to be used       
       itype_perturb,  &   ! perturbation type
       idbg                ! debug level

  REAL(KIND=wp), INTENT(IN) :: rperturb

  ! local variables
  REAL(KIND=wp)             :: zrperturb
  REAL(KIND=wp), POINTER    :: ztrcr(:,:,:)=> NULL()
  INTEGER (KIND=iintegers)  :: izerror
  CHARACTER (LEN=80)        :: yzerrmsg
  CHARACTER (LEN=25)        :: yzroutine = 'perturb_prognostic_fields'

  ! Warn user that this is a perturbed run
  IF ( ANY(itype_perturb == (/1,2/)) .AND. my_cart_id == 0 .AND. ntstep == 0 ) THEN
    WRITE(*,*) ''
    WRITE(*,*) '*******************************************************'
    WRITE(*,*) '*       ==>  THIS IS A PERTURBED RUN <==              *'
    WRITE(*,*) '*******************************************************'
    WRITE(*,*) ''
  ENDIF

  IF ( (itype_perturb == 1 .AND. ntstep == 0) .OR.  &
       (itype_perturb == 2) ) THEN

    ! generate epsilon
    IF (rperturb > 0.0_wp) THEN
      ! relative perturbation with a specified magnitude
      ! Example: if rperturb = 1.0e-5 then perturbation will be ...
      !    field = field * (1 + 1.0e-5 * R) in double precision
      !    field = field * (1 + 1.0e-5 * R) in single precision
      !  ... where R is a random number from -1.0 to 1.0
      zrperturb = rperturb
    ELSE
      ! relative perturbation with a magnitude relative to precision
      ! Example: if rperturb = -10.0 then perturbation will be ...
      !    field = field * (1 + 10.0 * 1e-16 * R) in double precision
      !    field = field * (1 + 10.0 * 1e-7 * R) in single precision
      !  ... where R is a random number from -1.0 to 1.0
      zrperturb = - rperturb * rprecision
    ENDIF

    ! Always print this info
    IF (my_cart_id == 0) THEN
      WRITE(*,*) '      APPLYING RELATIVE PERTURBATION : ', zrperturb
    ENDIF

    ! perturb the given data fields with coefficient rperturb
    CALL fld_perturb('u', u(:,:,:,nx), zrperturb)
    CALL fld_perturb('v', v(:,:,:,nx), zrperturb)
    CALL fld_perturb('w', w(:,:,:,nx), zrperturb)
    CALL fld_perturb('t', t(:,:,:,nx), zrperturb)
    CALL fld_perturb('pp', pp(:,:,:,nx), zrperturb)
    CALL trcr_get(izerror, idt_qv, ptr_tlev = nx, ptr = ztrcr)
    IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
    IF (ASSOCIATED(ztrcr)) CALL fld_perturb('qv', ztrcr(:,:,:), zrperturb)
    CALL trcr_get(izerror, idt_qc, ptr_tlev = nx, ptr = ztrcr)
    IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
    IF (ASSOCIATED(ztrcr)) CALL fld_perturb('qc', ztrcr(:,:,:), zrperturb)
    CALL trcr_get(izerror, idt_qr, ptr_tlev = nx, ptr = ztrcr)
    IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
    IF (ASSOCIATED(ztrcr)) CALL fld_perturb('qr', ztrcr(:,:,:), zrperturb)
    CALL trcr_get(izerror, idt_qi, ptr_tlev = nx, ptr = ztrcr)
    IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
    IF (ASSOCIATED(ztrcr)) CALL fld_perturb('qi', ztrcr(:,:,:), zrperturb)
    CALL trcr_get(izerror, idt_qs, ptr_tlev = nx, ptr = ztrcr)
    IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
    IF (ASSOCIATED(ztrcr)) CALL fld_perturb('qs', ztrcr(:,:,:), zrperturb)
    CALL trcr_get(izerror, idt_qg, ptr_tlev = nx, ptr = ztrcr)
    IF (izerror /= 0 .AND. izerror /= T_ERR_NOTFOUND) THEN
      yzerrmsg = trcr_errorstr(izerror)
      CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
    ENDIF
    IF (ASSOCIATED(ztrcr)) CALL fld_perturb('qg', ztrcr(:,:,:), zrperturb)
    IF (ALLOCATED(tke)) CALL fld_perturb('tke', tke(:,:,:,nx), zrperturb)
    
  ENDIF

END SUBROUTINE perturb_prognostic_fields


!==============================================================================
!+ Subroutines that perturb a field randomly 
!------------------------------------------------------------------------------

SUBROUTINE fld_perturb_1d( fld_name, fld_data, rperturb )

IMPLICIT NONE

! Description
!   This routine perturbs a field with a perturbation of the following style
!
!     field = (1 + epsilon) * field
!     epsilon is a random perturbation with mean(epsilon)=0
!   Note: this type of perturbation does not alter zero's and conserves the positive
!         definitiveness of a field

! Subroutine arguments
CHARACTER(LEN=*), INTENT(IN) ::    &
  fld_name           ! name of field

REAL (KIND=wp), INTENT(INOUT) ::  &
  fld_data(:)        ! field data

REAL (KIND=wp), INTENT(IN) ::  &
  rperturb

! Local variables
INTEGER (KIND=iintegers) ::        &
  ni,              & ! field dimensions
  ierr               ! error code

REAL (KIND=wp), ALLOCATABLE ::  &
  repsmat(:)            !           


  ! get dimensions
  ni = SIZE(fld_data, DIM=1)

  ! init seed if first call
   IF (.NOT. lseedinit) CALL init_seed

  ! allocate memory
  ALLOCATE(repsmat(ni))

  ! generate random numbers 
  CALL RANDOM_NUMBER(repsmat) ! repsmat between 0 and 1

  ! apply perturbation
  repsmat = rperturb * (2.0_wp * repsmat - 1.0_wp)   ! repsmat between -rperturb and rperturb
  fld_data = fld_data * (1.0_wp + repsmat)
  
  ! deallocate
  DEALLOCATE(repsmat)

END SUBROUTINE fld_perturb_1d

!------------------------------------------------------------------------------

SUBROUTINE fld_perturb_2d( fld_name, fld_data, rperturb )

IMPLICIT NONE

! Description
!   This routine perturbs a field with a perturbation of the following style
!
!     field = (1 + epsilon) * field
!
!   where epsilon can either be chose relative to the least significant bit (rperturb<0.0)
!   or as an absolute perturbation value (rperturb>0.0).
!
!   Note: this type of perturbation does not alter zero's and conserves the positive
!         definitiveness of a field

! Subroutine arguments
CHARACTER(LEN=*), INTENT(IN) ::    &
  fld_name           ! name of field

REAL (KIND=wp), INTENT(INOUT) ::  &
  fld_data(:,:)      ! field data

REAL (KIND=wp), INTENT(IN) ::  &
  rperturb

! Local variables
INTEGER (KIND=iintegers) ::        &
  ni, nj,          & ! field dimensions
  ierr               ! error code

REAL (KIND=wp), ALLOCATABLE ::  &
  repsmat(:,:)         ! field data

  ! get dimensions
  ni = SIZE(fld_data, DIM=1)
  nj = SIZE(fld_data, DIM=2)

  ! allocate memory
  ALLOCATE(repsmat(ni, nj))

  ! init seed if first call
  IF (.NOT. lseedinit) CALL init_seed

  ! generate random numbers 
  CALL RANDOM_NUMBER(repsmat) ! repsmat between 0 and 1

  ! apply perturbation
  repsmat = rperturb * (2.0_wp * repsmat - 1.0_wp)   ! repsmat between -rperturb and rperturb
  fld_data = fld_data * (1.0_wp + repsmat)

  ! deallocate
  DEALLOCATE(repsmat)

END SUBROUTINE fld_perturb_2d

!------------------------------------------------------------------------------

SUBROUTINE fld_perturb_3d( fld_name, fld_data, rperturb )

IMPLICIT NONE

! Description
!   This routine perturbs a field with a perturbation of the following style
!
!     field = (1 + epsilon) * field
!
!   where epsilon can either be chose relative to the least significant bit (rperturb<0.0)
!   or as an absolute perturbation value (rperturb>0.0).
!
!   Note: this type of perturbation does not alter zero's and conserves the positive
!         definitiveness of a field

! Subroutine arguments
CHARACTER(LEN=*), INTENT(IN) ::    &
  fld_name           ! name of field

REAL (KIND=wp), INTENT(INOUT) ::  &
  fld_data(:,:,:)    ! field data

REAL (KIND=wp), INTENT(IN) ::  &
  rperturb

! Local variables
INTEGER (KIND=iintegers) ::        &
  ni, nj, nk,      & ! field dimensions
  ierr               ! error code

REAL (KIND=wp) ::  &
  reps

REAL (KIND=wp), ALLOCATABLE ::  &
  repsmat(:,:,:)       ! field data

  ! get dimensions
  ni = SIZE(fld_data, DIM=1)
  nj = SIZE(fld_data, DIM=2)
  nk = SIZE(fld_data, DIM=3)

  ! allocate memory
  ALLOCATE(repsmat(ni, nj, nk))

  ! init seed if first call
  IF (.NOT. lseedinit) CALL init_seed

  ! generate random numbers 
  CALL RANDOM_NUMBER(repsmat) !repsmat between 0 and 1

  ! apply perturbation
  repsmat = rperturb * (2.0_wp * repsmat - 1.0_wp)   ! repsmat between -rperturb and rperturb
  fld_data = fld_data * (1.0_wp + repsmat)

  ! deallocate
  DEALLOCATE(repsmat)

END SUBROUTINE fld_perturb_3d


!==============================================================================
! init seed for random generator.
! Two subsequent runs should give use 
! different sequence
!------------------------------------------------------------------------------
SUBROUTINE init_seed
INTEGER(KIND=iintegers) , ALLOCATABLE :: seed(:)
REAL(KIND=wp), ALLOCATABLE :: Rseed(:)
INTEGER(KIND=iintegers) :: dt(8), n, dsum, i 

  ! Use the default seed to generate a first random list 
  CALL RANDOM_SEED(size = n)
  ALLOCATE(seed(n), Rseed(n))
  CALL RANDOM_NUMBER(Rseed)


  ! Generate a number for the seed which should differ for every run.
  ! Concatenate hours (H), minutes (M), second (S), milliseconds (m)
  ! as follow : dsum = mmmmSSMMHH
  ! Every run on a given day are garantee to get a different dsum.
  ! Over several days it is very unlikely to get two times the same number
  ! as we would need to run at the exact same time of the day in millisecond. 
  ! Note : adding days, month, year to dsum seems to make the number to 
  ! large for RANDOM_SEED(PUT...) and should be avoided.
  CALL DATE_AND_TIME(values=dt)
  dsum= dt(5)+dt(6)*1e2+dt(7)*1e4+dt(8)*1e6 
  
  ! Combine dsum with the default random list
  seed(:)=INT(Rseed*dsum, 4)
  CALL RANDOM_SEED(PUT = seed)

  ! set lseedinit
  lseedinit=.TRUE.

  DEALLOCATE(seed, rseed) 



END SUBROUTINE init_seed



END MODULE m_perturb
