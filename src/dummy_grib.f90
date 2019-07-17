! File %M% from Library %Q%
! Version %I% from %G% extracted: %H%
!+ Dummy interfaces for MPI routines
!-------------------------------------------------------------------------------
!
! Description:
!   This file provides dummy interfaces for the calls to the grib-library.
!   If the library is not available, these dummy interfaces have to be compiled
!   and linked with the LM in order to avoid "unresolved symbols" error while
!   linking the LM.
!
! Current Code Owner: DWD, Ulrich Schaettler
!  phone:  +49  69  8062 2739
!  fax:    +49  69  8062 3721
!  email:  uschaettler@dwd.d400.de
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! !VERSION!  !DATE!     Oliver Fuhrer
!  Initial release
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!=======================================================================

!-------------------------------------------------------------------------------

SUBROUTINE copen(iun,infile,modus,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iun,irm
CHARACTER(len=*) :: infile,modus
irm=1_intgribf
WRITE(0,*) 'WARNING: call to copen but compiled with dummy_grib.f90'
END SUBROUTINE

SUBROUTINE cclose(iun,modus,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iun,irm
CHARACTER(len=*) :: modus
irm=1_intgribf
WRITE(0,*) 'WARNING: call to cclose but compiled with dummy_grib.f90'
END SUBROUTINE cclose

SUBROUTINE cinquire(iun,infile,iexi,iopn,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iun,iexi,iopn,irm
CHARACTER (len=*) :: infile
irm=1_intgribf
WRITE(0,*) 'WARNING: call to comquire but compiled with dummy_grib.f90'
END SUBROUTINE cinquire

SUBROUTINE crewind(iun,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iun,irm
irm=1_intgribf
WRITE(0,*) 'WARNING: call to crewind but compiled with dummy_grib.f90'
END SUBROUTINE crewind

SUBROUTINE cuegex(iun,ifeld,lflb,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iun,lflb,irm
INTEGER (kind=intgribf) :: ifeld(*)
irm=1_intgribf
WRITE(0,*) 'WARNING: call to cuegex but compiled with dummy_grib.f90'
END SUBROUTINE cuegex

SUBROUTINE cuegin(iun,lfdb,ifeld,lflb,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iun,lfdb,lflb,irm
INTEGER (kind=intgribf) :: ifeld(*)
irm=1_intgribf
WRITE(0,*) 'WARNING: call to cuegin but compiled with dummy_grib.f90'
END SUBROUTINE cuegin

FUNCTION irefts(refw)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: irefts
REAL (kind=irealgrib) :: refw
irefts=0_intgribf
WRITE(0,*) 'WARNING: call to irefts but compiled with dummy_grib.f90'
END FUNCTION irefts

SUBROUTINE grbex1(iednr,ips,undef,ndims,idims,iwpds,iwgds,iwbms,iwbds,ibmap,dsup,ds,iblock,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
INTEGER (kind=intgribf) :: iednr,ips,ndims,idims(*),iwpds(*),iwgds(*),iwbms(*),iwbds(*),ibmap(*),iblock(*),irm
REAL (kind=irealgrib) :: undef,dsup(*),ds(*)
irm=1_intgribf
WRITE(0,*) 'WARNING: call to grbex1 but compiled with dummy_grib.f90'
END SUBROUTINE grbex1

SUBROUTINE grbin1(iednr,undef,ndims,idims,iblock,ibmap,iwpds,iwgds,iwbms,iwbds,dsup,ds,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iednr,ndims,idims(*),iwpds(*),iwgds(*),iwbms(*),iwbds(*),ibmap(*),iblock(*),irm
REAL (kind=irealgrib) :: undef,dsup(*),ds(*)
irm=1_intgribf
WRITE(0,*) 'WARNING: call to grbin1 but compiled with dummy_grib.f90'
END SUBROUTINE grbin1

SUBROUTINE getbd1(iednr,ibyta,ibdim,iblock,iwert,ibmp2,iwidth,dsup,idanz,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iednr,ibyta,ibdim(*),iblock(*),iwert(*),ibmp2(*),iwidth(*),idanz(*),irm
REAL (kind=irealgrib) dsup(*)
irm=1_intgribf
WRITE(0,*) 'WARNING: call to getbd1 but compiled with dummy_grib.f90'
END SUBROUTINE getbd1

SUBROUTINE getpd1(iednr,ibyta,iundef,nblock,nwert,iblock,iwert,ianz,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iednr,ibyta,iundef,nblock,nwert,iblock(*),iwert(*),ianz,irm
irm=1_intgribf
WRITE(0,*) 'WARNING: call to getpd1 but compiled with dummy_grib.f90'
END SUBROUTINE getpd1

SUBROUTINE getgd1(iednr,ibyta,iundef,nblock,nwert,iblock,iwert,ianz,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: iednr,ibyta,iundef,nblock,nwert,iblock(*),iwert(*),ianz,irm
irm=1_intgribf
WRITE(0,*) 'WARNING: call to getgd1 but compiled with dummy_grib.f90'
END SUBROUTINE getgd1

SUBROUTINE difmin(ijahr,imonat,itag,istund,iminut,njahr,nmonat,ntag,nstund,nminut,imdif,irm)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: ijahr,imonat,itag,istund,iminut,njahr,nmonat,ntag,nstund,nminut,imdif,irm
irm=1_intgribf
WRITE(0,*) 'WARNING: call to difmin but compiled with dummy_grib.f90'
END SUBROUTINE difmin

FUNCTION refstf(isedem)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
REAL (kind=irealgrib) :: refstf
INTEGER (kind=intgribf) :: isedem
refstf=0.0_irealgrib
WRITE(0,*) 'WARNING: call to refstf but compiled with dummy_grib.f90'
END FUNCTION refstf

SUBROUTINE atmhp(yupabs,irm,yatmod,ein,aus)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
CHARACTER(len=6) :: yupabs,yatmod
INTEGER (kind=intgribf) :: irm
REAL (kind=irealgrib) :: ein,aus
aus=0.0_irealgrib
WRITE(0,*) 'WARNING: call to atmhp but compiled with dummy_grib.f90'
END SUBROUTINE atmhp

SUBROUTINE fsleep(isec)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: isec
WRITE(0,*) 'WARNING: call to fsleep but compiled with dummy_grib.f90'
END SUBROUTINE fsleep

SUBROUTINE check_byte_order(i)
USE data_parameters, ONLY : intgribf, intgribc, irealgrib
IMPLICIT NONE
INTEGER (kind=intgribf) :: i(*)
WRITE(0,*) 'WARNING: call to check_byte_order but compiled with dummy_grib.f90'
END SUBROUTINE check_byte_order

