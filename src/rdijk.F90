!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDIJKFILE( T_FILENAME,BUF3D,LDXX,LO,HI,ISBIN,ISHDF5 )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Read dimensionless property values from an external file for
!     IJK inputs 
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 19 March 2002.
!     Last Modified by MD White, PNNL, 19 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE SOLTN
      USE GLB_PAR
      USE GRID_MOD
      USE SIO
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT NONE
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
#ifdef USE_H5HUT
     include "mpif.h"
#endif
#include "utils.h"

!
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER(LEN=*),INTENT(IN) :: T_FILENAME
      INTEGER, INTENT(IN) :: LDXX(3)
      REAL*8,INTENT(OUT) :: BUF3D(LDXX(1),LDXX(2),LDXX(3))
      INTEGER,INTENT(IN) :: LO(3),HI(3)
      LOGICAL,INTENT(IN) :: ISBIN
      LOGICAL,INTENT(IN) :: ISHDF5

      !Other Functions
      LOGICAL, EXTERNAL :: CREATE_DBLGA

      REAL*8, ALLOCATABLE :: VAL_BUF(:),TEMP3D(:,:,:)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:)
      INTEGER  LO_PUT(3),HI_PUT(3),LSTRIDE(3)
      INTEGER G_BUF,IUNIT
      INTEGER*8 H5FILE
      LOGICAL T_OK
      INTEGER I,J,K
      INTEGER ME
      INTEGER ISTAT
      INTEGER INDX
      INTEGER NCOUNT, IJDIM, NLAYX   ! USED IF YOU WANT TO READ TEXT DATA IN SLICES
      INTEGER ICSNX
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RDIJKFILE'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(146)(1:1),'$').EQ.0 ) CVS_ID(146) = &
      '$Id: rdijk.F,v 1.8 2006/01/09 20:10:33 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      ME = GA_NODEID()
!
!--- JKI and KIJ Indexing is not currently supported ---
!
!
!--- Create temporary Global Array
!
      T_OK = CREATE_DBLGA( G_BUF,NXDIM,NYDIM,NZDIM )
!
!--- Non HDF5 Read
!
      IF (.NOT.ISHDF5) THEN
        IF( ME.EQ.0 )THEN
          T_OK = OPENFILE( T_FILENAME,IUNIT,ISBIN )
          NCOUNT = 0
          NLAYX = NZDIM
          IJDIM = NXDIM*NYDIM*NLAYX
          INDX = 3
          ALLOCATE( IDX_BUF(3,IJDIM), STAT=ISTAT )
          T_OK = CHKSTAT( 'IDX_BUF',ISTAT,INDX )
          ALLOCATE( VAL_BUF(IJDIM), STAT=ISTAT )
          T_OK = CHKSTAT( 'VAL_BUF',ISTAT,INDX )
          DO K=1,NZDIM
             DO J=1,NYDIM    
                DO I=1,NXDIM
                  NCOUNT = NCOUNT + 1
                  IDX_BUF(1,NCOUNT) = I
                  IDX_BUF(2,NCOUNT) = J
                  IDX_BUF(3,NCOUNT) = K
                ENDDO
             ENDDO
          ENDDO
          IF( ISBIN )THEN
            READ(IUNIT)VAL_BUF(1:IJDIM)
          ELSE
            READ(IUNIT,*)VAL_BUF(1:IJDIM)
          ENDIF
          CALL NGA_SCATTER(G_BUF,VAL_BUF,IDX_BUF(1,1),IJDIM)
          DEALLOCATE(IDX_BUF)
          DEALLOCATE(VAL_BUF)
          CLOSE(UNIT=IUNIT)
        ENDIF
        CALL GA_SYNC
        CALL NGA_GET(G_BUF,LO,HI,BUF3D(1,1,1),LDXX)
        istat = GA_DESTROY(G_BUF)

      ELSE IF (ISHDF5) THEN
#ifdef USE_H5HUT
        H5FILE = SIO_OPENR(TRIM( T_FILENAME ),MPI_COMM_WORLD,1)   ! 1 IS STEP
        IF (H5FILE .LT. 0) CALL GA_ERROR &
          ("UNABLE TO OPEN IJK FILE"//T_FILENAME,-2)
        LO_PUT(1) = IXMIN
        LO_PUT(2) = IYMIN
        LO_PUT(3) = IZMIN
        HI_PUT(1) = IXMAX
        HI_PUT(2) = IYMAX
        HI_PUT(3) = IZMAX
        LSTRIDE(1) = IXMAX-IXMIN+1
        LSTRIDE(2) = IYMAX-IYMIN+1
        LSTRIDE(3) = IZMAX-IZMIN+1
        ALLOCATE( TEMP3D( LSTRIDE(1),LSTRIDE(2),1:LSTRIDE(3) ),STAT=ISTAT )
        T_OK = CHKSTAT( 'TEMP3D',ISTAT,INDX )
        IF(SIO_READ_IJKD( H5FILE,TEMP3D(1,1,1), &
           LSTRIDE(1),LSTRIDE(2),LSTRIDE(3) ).LT.0 )THEN
           CALL GA_ERROR("Unable to read ijk data",-3)
        ENDIF
        CALL NGA_PUT(G_BUF,LO_PUT,HI_PUT,TEMP3D,LSTRIDE)
        CALL GA_SYNC
        CALL NGA_GET(G_BUF,LO,HI,BUF3D(1,1,1),LDXX)
        DEALLOCATE( TEMP3D )
        ISTAT = SIO_CLOSE_FILE(H5FILE)
#else
        CALL GA_ERROR &
          ("HDF support was not built into this executable",-1)

#endif
      ENDIF
!
!---  Reset subroutine name string  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDIJKFILE group  ---
!
      RETURN
      END


      !< Read ijk->value file and fill in i j k and value arrays
      !! This function is meant to read only sparse ijk->value files
      !! as the entire array is filled on proc 0 and then broadcast
      !! to the other procs.  
      !>
      logical function rd_sparse_ijkv( filename,arr,dim1,dim2,nlines) result(ok)
         implicit none
!
#include "mafdecls.fh"
#include "global.fh"

         character(*),intent(in) :: filename
         integer,intent(in)      :: dim1,dim2
         integer,intent(out)     :: arr(dim1,dim2)
         integer,intent(out)     :: nlines


         integer ierr
         ok = .true.
         nlines = 0
         if (ga_nodeid() == 0) then
            open(unit=11,file=filename, iostat=ierr,status="old")
            if (ierr == 0) then
               do while (ierr == 0)
                  nlines = nlines + 1
                  read(11,*,iostat=ierr) arr(nlines,1), arr(nlines,2), arr(nlines,3),arr(nlines,4)
               enddo
               close(11)
               nlines= nlines -1
            else
               ok = .false.
            endif
         endif
         call ga_brdcst(1, arr, sizeof(arr), 0)

      end function





!
!-ead ijk->value file and fill in i j k arrays
      !! This function is meant to read only sparse ijk indices files
      !! as the entire array is filled on proc 0 and then broadcast
      !! to the other procs.
      !>
      logical function rd_sparse_ijk( filename,arr,dim1,dim2,nlines) result(ok)
         implicit none
!
#include "mafdecls.fh"
#include "global.fh"

         character(*),intent(in) :: filename
         integer,intent(in)      :: dim1,dim2
         integer,intent(out)     :: arr(dim1,dim2)
         integer,intent(out)     :: nlines


         integer ierr
         ok = .true.
         nlines = 0
         if (ga_nodeid() == 0) then
            open(unit=11,file=filename, iostat=ierr,status="old")
            if (ierr == 0) then
               do while (ierr == 0)
                  nlines = nlines + 1
                  read(11,*,iostat=ierr) arr(nlines,1), arr(nlines,2),&
                               arr(nlines,3)
               enddo
               close(11)
               nlines= nlines -1
            else
               ok = .false.
            endif
         endif
         call ga_brdcst(1, arr, sizeof(arr), 0)

      end function
!
      LOGICAL FUNCTION RDIJK1D( T_FILENAME,LDXX,LO,HI,VAR_OUT,VARX, &
        ISBIN,ISHDF5 ) RESULT( T_OK )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Copy property values read into temporary array to permanent
!     array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNNL, 1 March 2013.
!     Last Modified by VL Freedman, PNNL, 8 March 2013.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 T_FILENAME
      INTEGER LDXX(3),LO(3),HI(3)
      REAL*8 :: BUF3D(LDXX(1),LDXX(2),LDXX(3))
      REAL*8 :: VAR_OUT( LDXX(1)*LDXX(2)*LDXX(3) )
      LOGICAL ISBIN,ISHDF5
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDIJK1D'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(146)(1:1),'$').EQ.0 ) CVS_ID(146) = &
      '$Id: rdijk.F,v 1.8 2006/01/09 20:10:33 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
      BUF3D = 0.0D0
!
!---  Read external input file ---
!
      CALL RDIJKFILE( T_FILENAME,BUF3D,LDXX,LO,HI,ISBIN,ISHDF5 )
!
!---  Copy contents of temporary array back to node field ---
!
      N = 0
      DO K = 1, LDXX(3)
        DO J = 1, LDXX(2)
          DO I = 1, LDXX(1)
            N = N + 1
            VAR_OUT(N) = BUF3D(I,J,K)*VARX
          END DO
        END DO
      END DO
      IF( N > 0 )T_OK = .TRUE.
!
!---  Reset subroutine name string  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDIJK1D group  ---
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      LOGICAL FUNCTION RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC, &
        VAR_OUT,VARX, ISBIN,ISHDF5 ) RESULT( T_OK )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Copy property values read into temporary array to permanent
!     array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNNL, 1 March 2013.
!     Last Modified by VL Freedman, PNNL, 8 March 2013.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 T_FILENAME
      INTEGER LDXX(3),LO(3),HI(3)
      REAL*8 :: BUF3D(LDXX(1),LDXX(2),LDXX(3))
      REAL*8 :: VAR_OUT( LNDX,LDXX(1)*LDXX(2)*LDXX(3) )
      LOGICAL ISBIN,ISHDF5
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDIJK2D'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(146)(1:1),'$').EQ.0 ) CVS_ID(146) = &
      '$Id: rdijk.F,v 1.8 2006/01/09 20:10:33 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
      BUF3D = 0.0D0
!
!---  Read external input file ---
!
      CALL RDIJKFILE( T_FILENAME,BUF3D,LDXX,LO,HI,ISBIN,ISHDF5 )
!
!---  Copy contents of temporary array back to node field ---
!
      N = 0
      DO K = 1, LDXX(3)
        DO J = 1, LDXX(2)
          DO I = 1, LDXX(1)
            N = N + 1
            VAR_OUT(INDC,N) = BUF3D(I,J,K)*VARX
          END DO
        END DO
      END DO
      IF( N > 0 )T_OK = .TRUE.
!
!---  Reset subroutine name string  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDIJK2D group  ---
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      LOGICAL FUNCTION RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDC, &
        LCX,ICX,VAR_OUT,VARX, ISBIN,ISHDF5 ) RESULT( T_OK )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Copy property values read into temporary array to permanent
!     array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNNL, 1 March 2013.
!     Last Modified by VL Freedman, PNNL, 8 March 2013.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 T_FILENAME
      INTEGER LDXX(3),LO(3),HI(3)
      REAL*8 :: BUF3D(LDXX(1),LDXX(2),LDXX(3))
      REAL*8 :: VAR_OUT( LCX,LNDX,LDXX(1)*LDXX(2)*LDXX(3) )
      LOGICAL ISBIN,ISHDF5
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDIJK3D'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(146)(1:1),'$').EQ.0 ) CVS_ID(146) = &
      '$Id: rdijk.F,v 1.8 2006/01/09 20:10:33 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
      BUF3D = 0.0D0
!
!---  Read external input file ---
!
      CALL RDIJKFILE( T_FILENAME,BUF3D,LDXX,LO,HI,ISBIN,ISHDF5 )
!
!---  Copy contents of temporary array back to node field ---
!
      N = 0
      DO K = 1, LDXX(3)
        DO J = 1, LDXX(2)
          DO I = 1, LDXX(1)
            N = N + 1
            VAR_OUT(ICX,INDC,N) = BUF3D(I,J,K)*VARX
          END DO
        END DO
      END DO
      IF( N > 0 )T_OK = .TRUE.
!
!---  Reset subroutine name string  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDIJK3D group  ---
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDIJKT( T_FILENAME,TBL,VARX,ITBL,NL,NT,ILOG,ISBIN, &
        ISHDF5 )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Read dimensionless property values from an external file for
!     IJK, JKI, or KIJ indexed inputs for tabular input.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 29 July 2002.
!     Last Modified by MD White, PNNL, 29 July 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE FILES
      USE TABL
      USE SIO
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
#ifdef USE_H5HUT
     include "mpif.h"
#endif
#include "utils.h"
!
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER(LEN=*),INTENT(IN) :: T_FILENAME
      CHARACTER*64 VARBX,UNTS,UNTSX
      REAL*8,ALLOCATABLE :: BUF4D(:,:,:,:)
      REAL*8 VAR(1),TBL(NUM_NODES*NL)
      REAL*8, ALLOCATABLE :: VAL_BUF(:),TEMP4D(:,:,:,:)
      INTEGER :: LO(4),HI(4),DIMS(4)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:)
      INTEGER  LO_PUT(4),HI_PUT(4),LSTRIDE(4)
      INTEGER G_BUF,IUNIT,FOUR
      INTEGER*8 H5FILE
      INTEGER ITBL(2,NUM_NODES)
      INTEGER LDXX(4)
      LOGICAL,INTENT(IN) :: ISBIN
      LOGICAL,INTENT(IN) :: ISHDF5
      LOGICAL T_OK
      
      !Other Functions
      LOGICAL, EXTERNAL :: CREATE_DBLGA
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDIJKT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(146)(1:1),'$').EQ.0 ) CVS_ID(146) = &
         '$Id: rdijk.F,v 1.8 2006/01/09 20:10:33 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Set bounds  ---
!
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      LO(4) = 1
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      HI(4) = NL
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LDXX(4) = NL
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
      DIMS(4) = NL

      ALLOCATE(BUF4D(LDXX(1),LDXX(2),LDXX(3),NL))
!
!--- Create temporary Global Array
!
      FOUR = 4
      G_BUF = GA_CREATE_HANDLE()
#ifdef USE_E4D
!-- E4D Patch
       IF (GAE4D) CALL GA_SET_PGROUP(G_BUF,GAGRP)
#endif
      CALL GA_SET_DATA(G_BUF, FOUR, DIMS, MT_DBL)
      STATUS = GA_ALLOCATE(G_BUF)
!
!--- Non HDF5 Read
!
      IF (.NOT.ISHDF5) THEN
        IF( ME.EQ.0 )THEN
          T_OK = OPENFILE( T_FILENAME,IUNIT,ISBIN )
          NCOUNT = 0
          NLAYX = NZDIM
          IJDIM = NXDIM*NYDIM*NLAYX*NL
          INDX = 3
          ALLOCATE( IDX_BUF(4,IJDIM), STAT=ISTAT )
          T_OK = CHKSTAT( 'IDX_BUF',ISTAT,INDX )
          ALLOCATE( VAL_BUF(IJDIM), STAT=ISTAT )
          T_OK = CHKSTAT( 'VAL_BUF',ISTAT,INDX )
          DO K=1,NZDIM
             DO J=1,NYDIM
                DO I=1,NXDIM
                  DO L=1,NL
                    NCOUNT = NCOUNT + 1
                    IDX_BUF(1,NCOUNT) = I
                    IDX_BUF(2,NCOUNT) = J
                    IDX_BUF(3,NCOUNT) = K
                    IDX_BUF(4,NCOUNT) = L
                    IF( MOD(NCOUNT,IJDIM).EQ.0 ) THEN
                      IF( ISBIN )THEN
                        READ(IUNIT)VAL_BUF(1:IJDIM)
                      ELSE
                        READ(IUNIT,*)VAL_BUF(1:IJDIM)
                      ENDIF
                      CALL NGA_SCATTER(G_BUF,VAL_BUF,IDX_BUF(1,1), &
                          NCOUNT)
                      NCOUNT = 0
                    ENDIF
                 ENDDO
                ENDDO
             ENDDO
          ENDDO
          IF( MOD(NCOUNT,IJDIM).GT.0 ) THEN
            IJDIM = NCOUNT
            IF( ISBIN )THEN
              READ(IUNIT)VAL_BUF(1:IJDIM)
            ELSE
              READ(IUNIT,*)VAL_BUF(1:IJDIM)
            ENDIF
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
          ENDIF
          DEALLOCATE(IDX_BUF)
          DEALLOCATE(VAL_BUF)
          CLOSE(UNIT=IUNIT)
        ENDIF
        CALL GA_SYNC
        CALL NGA_GET(G_BUF,LO,HI,BUF4D(1,1,1,1),LDXX)
        istat = GA_DESTROY(G_BUF)
      ELSE IF (ISHDF5) THEN
       CALL GA_ERROR("Tabular data unsupported with HDF5 read",-3)
#ifdef USE_H5HUT
!        H5FILE = SIO_OPENR(TRIM( T_FILENAME ),MPI_COMM_WORLD,1)   ! 1 IS STEP
!        IF (H5FILE .LT. 0) CALL GA_ERROR &
!          ("UNABLE TO OPEN IJK FILE"//T_FILENAME,-2)
!        LO_PUT(1) = IXMIN
!        LO_PUT(2) = IYMIN
!        LO_PUT(3) = IZMIN
!        LO_PUT(4) = 1
!        HI_PUT(1) = IXMAX
!        HI_PUT(2) = IYMAX
!        HI_PUT(3) = IZMAX
!        HI_PUT(4) = NL
!        LSTRIDE(1) = (IXMAX-IXMIN+1)*NL
!        LSTRIDE(2) = IYMAX-IYMIN+1
!        LSTRIDE(3) = IZMAX-IZMIN+1
!        LSTRIDE(4) = NL
!        ALLOCATE( TEMP4D( LSTRIDE(1),LSTRIDE(2),LSTRIDE(3),LSTRIDE(4) ),STAT=ISTAT )
!        T_OK = CHKSTAT( 'TEMP4D',ISTAT,INDX )
!!        IF(SIO_READ_IJKD( H5FILE,TEMP4D(1,1,1), &
!!           LSTRIDE(1),LSTRIDE(2),LSTRIDE(3),LSTRIDE(4) ).LT.0 )THEN
!!           CALL GA_ERROR("Unable to read ijk data",-3)
!!        ENDIF
!!        CALL NGA_PUT(G_BUF,LO_PUT,HI_PUT,TEMP4D,LSTRIDE)
!        CALL GA_SYNC
!        CALL NGA_GET(G_BUF,LO,HI,BUF4D(1,1,1,1),LDXX)
!        DEALLOCATE( TEMP4D )
!        ISTAT = SIO_CLOSE_FILE(H5FILE)
#else
!        CALL GA_ERROR &
!          ("HDF support was not built into this executable",-1)

#endif
      ENDIF
!
!---  Read tabular values from file
!
!         NTBV = NFLD*NL
!         IF( ISBIN ) THEN
!            READ(26) (VAR(N),N=1,NTBV)
!         ELSE
!            READ(26,*) (VAR(N),N=1,NTBV)
!         ENDIF
!
!--- IJK Indexing (JKI and KIJ Indexing is not supported) ---
!
          NT = 0
          N = 0
          DO K = 1, LDXX(3)
            DO J = 1, LDXX(2)
              DO I = 1, LDXX(1)
                N = N + 1
                ITBL(1,N) = NT + 1
                DO 90 L = 1,NL
                  NT = NT + 1
                  IF( ILOG.EQ.1 ) THEN
                     TBL(NT) = LOG( EXP(BUF4D(I,J,K,L))*VARX )
                  ELSE
                     TBL(NT) = BUF4D(I,J,K,L)*VARX
                  ENDIF
  90            CONTINUE
                ITBL(2,N) = NT
              ENDDO
            ENDDO
          ENDDO
  100    CONTINUE
      DEALLOCATE(BUF4D)
!
!---  Reset subroutine name string  ---
!
         ICSN = ICSN-ICSNX
         SUBNM = SUBNM(1:ICSN)
!
!---  End of RDIJKT group  ---
!
         RETURN
         END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDIJK( ISTART,IJK,CHDUM,UNTS,VAR_out )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Read dimensionless property values from an external file for
!     IJK, JKI, or KIJ indexed inputs
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 19 March 2002.
!     Last Modified by MD White, PNNL, 19 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE GRID
      USE FILES
      use grid_mod
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,CDUM,FDUM,VARBX,UNTS,UNTSX
      CHARACTER*512 CHDUM
!      REAL*8 VARL(NROCK),VAR(*)
!      REAL*8 VARL(NROCK),VAR(nrock)
      LOGICAL FCHK,FBIN
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3), G_BUF, THREE
      INTEGER, ALLOCATABLE :: BUF(:), VAL_BUF(:)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:)
      INTEGER, ALLOCATABLE :: BUF3(:,:,:)
      REAL*8,dimension(:), allocatable :: VARL,VAR
      real*8, dimension(num_nodes) :: var_out
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      allocate(var(num_nodes))
      var = 0.d0
      var_out = 0.d0
!
!--- create GA for zone indices ---
!
      dims(1) = nxdim
      dims(2) = nydim
      dims(3) = nzdim
      three = 3
      g_buf = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch
       IF (GAE4D) CALL GA_SET_PGROUP(G_BUF,GAGRP)
#endif
      call ga_set_data(g_buf, three, dims, MT_INT)
      status = ga_allocate(g_buf)
!
      SUBNMX = '/RDIJK'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(146)(1:1),'$').EQ.0 ) CVS_ID(146) = &
      '$Id: rdijk.F,v 1.8 2006/01/09 20:10:33 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      ISTX = ISTART
      VARBX = VARB
      IDFLT = 0
      VARB = 'External File Name'
      CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
      FBIN = .FALSE.
      IF( INDEX(ADUM(1:),'binary').NE.0 .OR. &
       INDEX(ADUM(1:),'bfile').NE.0 .OR. &
       INDEX(ADUM(1:),'b_file').NE.0 ) FBIN = .TRUE. 
!
!---  External-file input  ---
!
      IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
        ICOLON = INDEX(ADUM(1:),':') + 1
        FDUM = ADUM(ICOLON:NCHA)
        NCHF = NCHA-ICOLON+1
        INQUIRE( FILE=FDUM(1:NCHF), FORM=CDUM, EXIST=FCHK )
        IF( .NOT.FCHK ) THEN
          INDX = 4
          CHMSG = 'IJK Indexing file does not exist: ' &
           // FDUM(1:NCHF)
          CALL WRMSGS( INDX )
        ELSEIF( CDUM.EQ.'UNFORMATTED' .AND. (.NOT. FBIN) ) THEN
          INDX = 4
          CHMSG = 'Formatted IJK Indexing file is unformatted: ' &
          // FDUM(1:NCHF)
          CALL WRMSGS( INDX )
        ELSEIF( CDUM.EQ.'FORMATTED' .AND. FBIN ) THEN
          INDX = 4
          CHMSG = 'Unformatted IJK Indexing file is formatted: ' &
           // FDUM(1:NCHF)
          CALL WRMSGS( INDX )
        END IF
        IF( FBIN ) THEN
          OPEN(UNIT=26, FILE=FDUM(1:NCHF), STATUS='OLD', &
           FORM='UNFORMATTED')
        ELSE
          OPEN(UNIT=26, FILE=FDUM(1:NCHF), STATUS='OLD', &
           FORM='FORMATTED')
        ENDIF
        if(me.eq.0)WRITE(ISC,'(/,2A)') 'IJK Indexing File: ',FDUM(1:NCHF)
!
!---    Check for units  ---
!
        VARB = VARBX
        VARX = 1.D+0
        NCHU = INDEX( UNTS(1:),'  ' ) - 1
        IF( UNTS(1:NCHU).NE.'null' ) THEN
          IDFLT = 1
          UNTSX = UNTS
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTSX)
          if(me.eq.0)WRITE(ISC,'(2X,3A)') VARB(1:IVR),', ',UNTSX(1:NCH)
          INDX = 0
          CALL RDUNIT(UNTSX,VARX,INDX)
        ENDIF
        allocate(varl(lrc))
        IF( FBIN ) THEN
          READ(26) (VARL(IROCK),IROCK=1,nxdim*nydim*nzdim)
!          if(me.eq.0)then
!            READ(26) (VARL(IROCK),IROCK=1,NFLD)
!          endif
        ELSE
          READ(26,*) (VARL(IROCK),IROCK=1,nxdim*nydim*nzdim)
!          if(me.eq.0) then
!            READ(26,*) (VARL(IROCK),IROCK=1,NFLD)
!          endif
        ENDIF
!
!---    IJK indexing  ---
!
        ldxx(1) = iaxmax - iaxmin + 1
        ldxx(2) = iaymax - iaymin + 1
        ldxx(3) = iazmax - iazmin + 1
        lo(1) = iaxmin
        lo(2) = iaymin
        lo(3) = iazmin
        hi(1) = iaxmax
        hi(2) = iaymax
        hi(3) = iazmax
!
        IF( IJK.EQ.1 ) THEN
          ijdim = nxdim*nydim
          do k = lo(3),hi(3)
            do j = lo(2),hi(2)
              do i = lo(1),hi(1)
                irock = (k-1)*ijdim+(j-1)*nxdim+i
                nx = (k-lo(3))*ldxx(1)*ldxx(2)+(j-lo(2))*ldxx(1)+i-lo(1)+1
!                VAR(IROCK) = VARL(IROCK)*VARX
                VAR_out(nx) = VARL(IROCK)*VARX
              enddo
            enddo
          enddo
          deallocate(varl)
!
!---    JKI indexing  ---
!
        ELSEIF( IJK.EQ.2 ) THEN
          ijdim = nydim*nzdim
          do i = lo(1), hi(1)
            do k = lo(3), hi(3)
              do j = lo(2), hi(2)
                irock = (i-1)*ijdim+(k-1)*nydim+j
!                VAR(IROCK) = VARL(IROCK)*VARX
              end do
            end do
          end do
!
!---    KIJ indexing  ---
!
        ELSEIF( IJK.EQ.3 ) THEN
          ijdim = nxdim*nzdim
          do j = lo(2), hi(2)
            do i = lo(1), hi(1)
              do k = lo(3), hi(3)
                irock = (j-1)*ijdim+(i-1)*nzdim+k
!                VAR(IROCK) = VARL(IROCK)*VARX
              end do
            end do
          end do
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        CLOSE(UNIT=26)
!
!---  Input-file input  ---
!
      ELSE
        ISTART = ISTX
        VARB = VARBX
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VARX)
        IDFLTDX = IDFLTD
!
!---    Check for units  ---
!
        NCHU = INDEX( UNTS(1:),'  ' ) - 1
        IF( UNTS(1:NCHU).NE.'null' ) THEN
          IDFLT = 1
          UNTSX = UNTS
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTSX)
          if(me.eq.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTSX(1:NCH), &
           ': ',VARX
          INDX = 0
          CALL RDUNIT(UNTSX,VARX,INDX)
          if(me.eq.0)WRITE(ISC,'(A,1PE11.4,3A)') ' (',VARX,', ',UNTS(1:NCHU),')'
        ELSE
          if(me.eq.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',VARX
        ENDIF
        IF( IDFLTDX.EQ.0 ) THEN
          DO 400 N = 1,num_nodes
!            VAR(IZ(N)) = VARX
!            VAR(N) = VARX
            var_out(n) = varx
 400      CONTINUE
        ENDIF
      ENDIF
!      var_out = var
      deallocate(var)
!
!---  Reset subroutine name string  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDIJK group  ---
!
      RETURN
      END


!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDIJKD( ISTART,IJK,CHDUM,UNTS,VAR_out,INDC,LNDX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Read dimensionless property values from an external file for
!     IJK, JKI, or KIJ indexed inputs with double indices
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 19 March 2002.
!     Last Modified by MD White, PNNL, 19 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE GRID
      USE FILES
      use grid_mod
      use sio
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
#ifdef USE_H5HUT
     include "mpif.h"
#endif
!
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,CDUM,FDUM,VARBX,UNTS,UNTSX
      CHARACTER*512 CHDUM
      LOGICAL FCHK,FBIN
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3), G_BUF, THREE
!      REAL*8 VAR(LNDX,NROCK)
      real*8, dimension(:,:), allocatable :: var
      real*8, dimension(:), allocatable :: var_tmp
      real*8, dimension(lndx,num_nodes) :: var_out
      INTEGER, ALLOCATABLE :: BUF(:)
      double precision, allocatable ::  VAL_BUF(:)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:)
      double precision, ALLOCATABLE :: BUF3(:,:,:)
      INTEGER*8 h5file
      LOGICAL ishdf5
 
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      dims(1) = nxdim
      dims(2) = nydim
      dims(3) = nzdim
      three = 3
      g_buf = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
       IF (GAE4D) CALL GA_SET_PGROUP(G_BUF,GAGRP)
#endif
      call ga_set_data(g_buf, three, dims, MT_DBL)
      status = ga_allocate(g_buf)
      SUBNMX = '/RDIJKD'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(146)(1:1),'$').EQ.0 ) CVS_ID(146) = &
      '$Id: rdijk.F,v 1.8 2006/01/09 20:10:33 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      ISTX = ISTART
      VARBX = VARB
      IDFLT = 0
      VARB = 'External File Name'
      CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
      FBIN = .FALSE.
      ishdf5 = .FALSE.
      IF( INDEX(ADUM(1:),'binary').NE.0 .OR. &
       INDEX(ADUM(1:),'bfile').NE.0 .OR. &
       INDEX(ADUM(1:),'b_file').NE.0 ) THEN
        FBIN = .TRUE.
      elseif( INDEX(ADUM(1:),'hdffile').NE.0 )  then
        ISHDF5 = .true.
        FBIN = .false.  
      endif
!
!---  External-file input  ---
!
IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
   ICOLON = INDEX(ADUM(1:),':') + 1
   FDUM = ADUM(ICOLON:NCHA)
   NCHF = NCHA-ICOLON+1
   INQUIRE( FILE=FDUM(1:NCHF), FORM=CDUM, EXIST=FCHK )
   IF( .NOT.FCHK ) THEN
      INDX = 4
      CHMSG = 'IJK Indexing file does not exist: ' &
         // FDUM(1:NCHF)
      CALL WRMSGS( INDX )
      ELSEIF( CDUM.EQ.'UNFORMATTED' .AND. (.NOT. FBIN) ) THEN
      INDX = 4
      CHMSG = 'Formatted IJK Indexing file is unformatted: ' &
         // FDUM(1:NCHF)
      CALL WRMSGS( INDX )
      ELSEIF( CDUM.EQ.'FORMATTED' .AND. FBIN ) THEN
      INDX = 4
      CHMSG = 'Unformatted IJK Indexing file is formatted: ' &
         // FDUM(1:NCHF)
      CALL WRMSGS( INDX )
   END IF

   if(me.eq.0)WRITE(ISC,'(/,2A)') 'IJK Indexing File: ',FDUM(1:NCHF)
   !
   !---    Check for units  ---
   !
   VARB = VARBX
   VARX = 1.D+0
   NCHU = INDEX( UNTS(1:),'  ' ) - 1
   IF( UNTS(1:NCHU).NE.'null' ) THEN
      IDFLT = 1
      UNTSX = UNTS
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTSX)
      if(me.eq.0)WRITE(ISC,'(2X,3A)') VARB(1:IVR),', ',UNTSX(1:NCH)
      INDX = 0
      CALL RDUNIT(UNTSX,VARX,INDX)
   ENDIF
   !        allocate(var(lndx,nrock))
   !        var(lndx,1:nrock) = 0.d0
   ldxx(1) = iaxmax - iaxmin + 1
   ldxx(2) = iaymax - iaymin + 1
   ldxx(3) = iazmax - iazmin + 1
   lo(1) = iaxmin
   lo(2) = iaymin
   lo(3) = iazmin
   hi(1) = iaxmax
   hi(2) = iaymax
   hi(3) = iazmax
   IF( FBIN ) THEN
      !          if(me.eq.0) READ(26) (VAR(INDC,IROCK),IROCK=1,nxdim*nydim*nzdim)
      if (me.eq.0) then
         OPEN(UNIT=26, FILE=FDUM(1:NCHF), STATUS='OLD',  FORM='UNFORMATTED')
         ncount = 0
         !          nlayx = 4
         nlayx = nzdim
         ijdim = nxdim*nydim*nlayx
         !          ijdim = nxdim*nydim
         allocate(idx_buf(3,ijdim))
         allocate(val_buf(ijdim))
         ild = iaxmax - iaxmin + 1
         ijld = ild * (iaymax - iaymin + 1)
         20     CONTINUE
         do k=1,nzdim
            do j=1,nydim    
               do i=1,nxdim
                  ncount = ncount + 1
                  idx_buf(1,ncount) = i
                  idx_buf(2,ncount) = j
                  idx_buf(3,ncount) = k
                  if( mod(ncount,ijdim).eq.0 ) then
                     READ(26) val_buf(1:ijdim)
                     call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
                     ncount = 0
                  endif
               enddo
            enddo
         enddo
         if( mod(ncount,ijdim).gt.0 ) then
            ijdim = ncount
            READ(26) val_buf(1:ijdim)
            call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
            ncount = 0
         endif
         deallocate(val_buf)
         deallocate(idx_buf)
      endif
      call ga_sync
      allocate(buf3(ldxx(1),ldxx(2),ldxx(3)))
      buf3 = 0.d0
      call nga_get(g_buf,lo,hi,buf3(1,1,1),ldxx)
      !print *,'dfv',me,dfv(46)
      !          if(me.eq.0)then
      !            READ(26) (VARL(IROCK),IROCK=1,NFLD)
      !          endif


      CLOSE(UNIT=26)
   else if (ISHDF5) then
#ifdef USE_H5HUT
      allocate(buf3(ldxx(1),ldxx(2),ldxx(3)))
      buf3 = 0.d0
      write(*,*) "kls",FDUM(1:NCHF)
      h5file = sio_openr(FDUM(1:NCHF),MPI_COMM_WORLD,1)   ! 1 is step
      if (h5file .lt. 0) call ga_error("Unable to open ijk file"//FDUM(1:NCHF),-2)
      if (sio_read_ijkd(h5file,buf3(1,1,1),ldxx(1),ldxx(2),ldxx(3)) .lt. 0) then
         call ga_error("Unable to read ijk data",-3)
      endif
      istat = sio_close_file(h5file)

#else
      call ga_error("HDF support was not built into this executable",-1)

#endif
   else

      OPEN(UNIT=26, FILE=FDUM(1:NCHF), STATUS='OLD',  FORM='FORMATTED')
      allocate(var_tmp(nxdim*nydim*nzdim),STAT=ISTAT)
      IF( ISTAT.NE.0 ) THEN
         INDX = 3
         CHMSG = 'Allocation Error: VAR_TMP'
         CALL WRMSGS( INDX )
      ENDIF
      var_tmp = 0.d0
      READ(26,*) (VAR_TMP(IROCK),IROCK=1,nxdim*nydim*nzdim)
      !          if(me.eq.0) then
      !            READ(26,*) (VARL(IROCK),IROCK=1,NFLD)
      !          endif
      CLOSE(UNIT=26)
   ENDIF
   !
!---    IJK indexing  ---
!
!        ldxx(1) = iaxmax - iaxmin + 1
!        ldxx(2) = iaymax - iaymin + 1
!        ldxx(3) = iazmax - iazmin + 1
!        lo(1) = iaxmin
!        lo(2) = iaymin
!        lo(3) = iazmin
!        hi(1) = iaxmax
!        hi(2) = iaymax
!        hi(3) = iazmax
!

   IF( IJK.EQ.1 ) THEN
      if(.not. fbin .and. .not. ishdf5) then
         ijdim = nxdim*nydim
         do k = lo(3),hi(3)
            do j = lo(2),hi(2)
               do i = lo(1),hi(1)
                  irock = (k-1)*ijdim+(j-1)*nxdim+i
                  nx = (k-lo(3))*ldxx(1)*ldxx(2)+(j-lo(2))*ldxx(1)+i-lo(1)+1
                  !                VAR(IROCK) = VARL(IROCK)*VARX
                  VAR_out(indc,nx) = VAR_TMP(IROCK)*VARX
               enddo
            enddo
         enddo
         deallocate(var_tmp)
      else
         nx = 0
         do k = 1,ldxx(3)
            do j = 1,ldxx(2)
               do i = 1,ldxx(1)						      
                  nx = nx + 1
                  var_out(indc,nx) = buf3(i,j,k)*varx
               enddo
            enddo
         enddo
         deallocate(buf3)
      endif
      !---    JKI indexing  ---
      !
      ELSEIF( IJK.EQ.2 ) THEN
      ijdim = nydim*nzdim
      do i = lo(1), hi(1)
         do k = lo(3), hi(3)
            do j = lo(2), hi(2)
               irock = (i-1)*ijdim+(k-1)*nydim+j
               nx = (k-iazmin)*ldxx(1)*ldxx(2)+(j-iaymin)*ldxx(1)+i-iaxmin+1
               !                VAR(INDC,IROCK) = VAR(INDC,IROCK)*VARX
               !                var_out(indc,nx) = var(indc,irock)
            end do
         end do
      end do
      !          deallocate(var)
      !
      !---    KIJ indexing  ---
      !
      ELSEIF( IJK.EQ.3 ) THEN
      ijdim = nxdim*nzdim
      do j = lo(2), hi(2)
         do i = lo(1), hi(1)
            do k = lo(3), hi(3)
               irock = (j-1)*ijdim+(i-1)*nzdim+k
               !                VAR(INDC,IROCK) = VAR(INDC,IROCK)*VARX
            end do
         end do
      end do
      !          deallocate(var)
   ELSE
      INDX = 4
      CHMSG = 'Unrecognized Indexing Option' // ADUM(1:NCH)
      CALL WRMSGS( INDX )
   ENDIF
!
!---  Input-file input  ---
!
      ELSE
         ISTART = ISTX
         VARB = VARBX
         CALL RDDPR(ISTART,ICOMMA,CHDUM,VARX)
         IDFLTDX = IDFLTD
         !
         !---    Check for units  ---
         !
         NCHU = INDEX( UNTS(1:),'  ' ) - 1
         IF( UNTS(1:NCHU).NE.'null' ) THEN
            IDFLT = 1
            UNTSX = UNTS
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTSX)
            if(me.eq.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTSX(1:NCH), &
               ': ',VARX
            INDX = 0
            CALL RDUNIT(UNTSX,VARX,INDX)
            if(me.eq.0)WRITE(ISC,'(A,1PE11.4,3A)') ' (',VARX,', ',UNTS(1:NCHU),')'
         ELSE
            if(me.eq.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',VARX
         ENDIF
         IF( IDFLTDX.EQ.0 ) THEN
            DO 400 N = 1,num_nodes
            !            VAR(INDC,IZ(N)) = VARX
            VAR_out(INDC,N) = VARX
            400     CONTINUE
         ENDIF
      ENDIF
      !
      !---  Reset subroutine name string  ---
      !
      status = ga_destroy(g_buf)
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      !
      !---  End of RDIJKD group  ---
      !
      RETURN
      END

      !----------------------Subroutine--------------------------------------!
