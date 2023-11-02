!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSP1
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
!     Water Mode
!
!     Read input file for rock/soil saturation function information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 19, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TABL
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
      USE BUFFEREDREAD
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
#include "utils.h"
#include "gagrid.h"
!
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,RDUM,UNTS,T_FILENAME
      CHARACTER*512 CHDUM
      INTEGER :: DIM1,IRPLX,ISCHRX,ISMX
      INTEGER :: LO(3),HI(3),LDXX(3)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISLTBLX
      DOUBLE PRECISION :: SCHRX(LSCHR)
      LOGICAL T_OK,ISBIN,ISHDF5
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDSP1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  WRITE card information to ouput file  ---
!
      CARD = 'Rock/Soil Saturation Function Card'
      ICD = INDEX( CARD,'  ' )-1
      IF(ME.EQ.0) WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Set bounds, flags  ---
!
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
!
!---  Loop over the rock/soil saturation information lines  ---
!
      ISBIN = .FALSE.
      IRPLX = 0
      CALL ADD_NODE_IFIELD('ischr', IDX)
      ISCHR => I_nd_fld(idx)%p
      ISCHR = 0
      CALL ADD_NODE_IFIELD('irpl', IDX)
      IRPL => I_ND_FLD(IDX)%P
      IRPL = 0
      CALL ADD_NODE_IFIELD('ism', IDX)
      ISM => I_ND_FLD(IDX)%p
      ISM = 0
      DIM1 = LSCHR
      CALL ADD_NODE_D2FIELD('schr',DIM1, IDX)
      SCHR => D_ND_2FLD(IDX)%P
      SCHR = 0.D0
      NX = 0
      IJK = 0
      ISALLOC = 0
   10 CONTINUE
      IF( NX.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      VARB = 'Saturation Function: Rock Name: '
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  IJK indexing (property read from file)  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IJK = 1
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = 1, NROCK
        IF( RDUM.EQ.ROCK(M)) THEN
          IROCK = M
          GOTO 200
        ENDIF
  100 CONTINUE
      ISGRP = 0
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
      CALL WRMSGS( INDX )
      GOTO 10
  200 CONTINUE
!
!---  WRITE rock/soil name  ---
!
      IF(ME.EQ.0) WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      NX = NX + 1
  220 CONTINUE
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read saturation/capillary pressure function  ---
!
      LNDX = LSCHR
      VARB = 'Saturation Function Type: '
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      ISMX = 0
      IF( INDEX(ADUM(1:),'extended').NE.0 ) ISMX = 1
      IF( INDEX(ADUM(1:),'webb').NE.0 ) ISMX = 2
      IF( INDEX(ADUM(1:),'entrap').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'van genuchten').NE.0 ) THEN
          ISCHRX = 101
        ELSEIF( INDEX(ADUM(1:),'brooks').NE.0 .AND. &
         INDEX(ADUM(1:),'corey').NE.0 ) THEN
          ISCHRX = 102
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Saturation Function: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'finger').NE.0 .OR. &
       INDEX(ADUM(1:),'triple').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'van genuchten').NE.0 ) THEN
          ISCHRX = 301
        ELSEIF( INDEX(ADUM(1:),'brooks').NE.0 .AND. &
         INDEX(ADUM(1:),'corey').NE.0 ) THEN
          ISCHRX = 302
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Saturation Function: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'van genuchten').NE.0 ) THEN
        ISCHRX = 1
        IF( INDEX(RDUM(1:),'fractured').NE.0 .OR. &
         INDEX(RDUM(1:),'dp').NE.0 .OR. &
         INDEX(RDUM(1:),'dual').NE.0 ) ISCHRX = 3
      ELSEIF( INDEX(ADUM(1:),'brooks').NE.0 .AND. &
         INDEX(ADUM(1:),'corey').NE.0 ) THEN
        ISCHRX = 2
        IF( INDEX(RDUM(1:),'fractured').NE.0 .OR. &
         INDEX(RDUM(1:),'dp').NE.0 .OR. &
         INDEX(RDUM(1:),'dual').NE.0 ) ISCHRX = 4
      ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
        ISCHRX = 5
        IF( INDEX(ADUM(1:),'log').NE.0 ) ISCHRX = -5
      ELSEIF( INDEX(ADUM(1:),'well').NE.0 ) THEN
        ISCHRX = 7
        if(me.eq.0) WRITE(IWR,'(A)') 'Well Function'
      ELSEIF( INDEX(ADUM(1:),'luckner').NE.0 ) THEN
        ISCHRX = 8
      ELSEIF( INDEX(ADUM(1:),'russo').NE.0 ) THEN
        ISCHRX = 9
      ELSEIF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
        IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
            ISCHRX = 13
            IF(ME.EQ.0) WRITE(IWR,'(A)') &
              'Cubic-Spline-Log Interpolation'
          ELSE
            ISCHRX = 11
            IF(ME.EQ.0) WRITE(IWR,'(A)') 'Cubic-Spline Interpolation'
          ENDIF
        ELSE
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
            ISCHRX = 12
            IF(ME.EQ.0) WRITE(IWR,'(A)') 'Linear-Log Interpolation'
          ELSE
            ISCHRX = 10
            IF(ME.EQ.0) WRITE(IWR,'(A)') 'Linear Interpolation'
          ENDIF
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
        ISCHRX = 19
      ELSEIF( INDEX(ADUM(1:),'cambridge').NE.0 ) THEN
        ISCHRX = 41
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Saturation Function: '//ADUM
        CALL WRMSGS( INDX )
      ENDIF
      IFLAG = -1
      T_OK = COPYIJK1D_INT( ISCHR,ISCHRX,IFLAG )
      IF(ISCHRX.EQ.7)IRPLX = 7
      T_OK = COPYIJK1D_INT( IRPL,IRPLX,IFLAG )
      T_OK = COPYIJK1D_INT( ISM,ISMX,IFLAG )
!
!---  van Genuchten Function  ---
!
      IF( ISCHRX.EQ.1 .OR. ISCHRX.EQ.101 ) THEN
        IF( ISCHRX.EQ.1 ) THEN
          IF(ME.EQ.0) WRITE(IWR,'(A)') 'van Genuchten Function'
        ELSEIF( ISCHRX.EQ.101 ) THEN
          IF(ME.EQ.0) WRITE(IWR,'(A)') &
            'van Genuchten Function w/ Gas Entrapment'
        ENDIF
        VARB = 'van Genuchten (alpha)'
        LNDX = LSCHR
        INDC = 1 
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          UNTS = '1/m'
          IUNM = -1
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
            VARB(1:IVR),', ',UNTS(1:NCH), &
           ': ',SCHRX(1)
          INDX = 0
          IUNM = -1
          CALL RDUNIT(UNTS,SCHRX(1),INDX)
          IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', 1/m)'
          T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        ENDIF
        VARB = 'van Genuchten (n): '
        LNDX = LSCHR
        INDC = 3
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          UNTS = 'null'
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
          IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
          T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        ENDIF
        VARB = 'van Genuchten (residual saturation): '
        LNDX = LSCHR
        INDC = 4
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          UNTS = 'null'
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
          IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
          T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
        ENDIF
        INDC = 9
        SCHRX(9) = 1.D+20
        T_OK = COPYIJK2D( SCHR,SCHRX(9),LNDX,INDC,IFLG )
        LNDX = LSCHR
        INDC = 14
        VARB = 'van Genuchten (m)'
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          UNTS = 'null'
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(14))
          IF( ME.EQ.0 ) WRITE(IWR,'(2X,2A,1PE11.4)') &
             VARB(1:IVR),': ',SCHRX(14)
          T_OK = COPYIJK2D( SCHR,SCHRX(14),LNDX,INDC,IFLG )
        ENDIF
        IF( ISCHRX.EQ.101 ) THEN
          VARB = 'van Genuchten (Effective Gas Residual Sat.: ' // &
           'Gas-Aqueous System)'
          LNDX = LSCHR
          INDC = 15
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            UNTS = 'null'
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
              ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(15))
            IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
             SCHRX(15)
            T_OK = COPYIJK2D( SCHR,SCHRX(15),LNDX,INDC,IFLG )
          ENDIF
          VARB = 'van Genuchten (Critical Trapping Number)'
          IDFLT = 1
          LNDX = LSCHR
          INDC = 9
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            UNTS = 'null'
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
              ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(9))
            IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
             SCHRX(9)
            T_OK = COPYIJK2D( SCHR,SCHRX(9),LNDX,INDC,IFLG )
          ENDIF
        ENDIF
!
!---  van Genuchten triple curve function  ---
!
      ELSEIF( ISCHRX.EQ.301 ) THEN
        IF(ME.EQ.0) WRITE(IWR,'(A)') &
          'van Genuchten Triple Curve Function'
        VARB = 'van Genuchten (main drainage alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
           VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(1)
        INDC = 1
        T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', 1/m)'
        VARB = 'van Genuchten (main drainage n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
        INDC = 3
        T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        VARB = 'van Genuchten (main drainage residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
        INDC = 4
        T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
        SCHRX(9) = 1.D+20
        INDC = 9
        T_OK = COPYIJK2D( SCHR,SCHRX(9),LNDX,INDC,IFLG )
        VARB = 'van Genuchten (main drainage m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(14))
        IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),': ',SCHRX(14)
        INDC = 14
        T_OK = COPYIJK2D( SCHR,SCHRX(14),LNDX,INDC,IFLG )
        VARB = 'van Genuchten (boundary wetting scanning alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(12))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(12)
        INDC = 12
        T_OK = COPYIJK2D( SCHR,SCHRX(12),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHRX(12),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(12),', 1/m)'
        VARB = 'van Genuchten (main wetting alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH), ': ',SCHRX(2)
        INDC = 2
        T_OK = COPYIJK2D( SCHR,SCHRX(2),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHRX(2),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(2),', 1/m)'
        VARB = 'van Genuchten (main wetting n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(5))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(5)
        INDC = 5
        T_OK = COPYIJK2D( SCHR,SCHRX(5),LNDX,INDC,IFLG )
        VARB = 'van Genuchten (main wetting residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(6))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(6)
        INDC = 6
        T_OK = COPYIJK2D( SCHR,SCHRX(6),LNDX,INDC,IFLG )
        VARB = 'van Genuchten (main wetting m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(7))
        IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),': ',SCHRX(7)
        INDC = 7
        T_OK = COPYIJK2D( SCHR,SCHRX(7),LNDX,INDC,IFLG )
        VARB = 'van Genuchten (wetting maximum saturation)'
        SCHRX(10) = 1.D+0
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(10))
        IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),': ',SCHRX(10)
        INDC = 10
        T_OK = COPYIJK2D( SCHR,SCHRX(10),LNDX,INDC,IFLG )
        IF( SCHRX(4).LT.SCHRX(6) ) THEN
          INDX = 4
          NCH = INDEX( ROCK(IROCK),'  ' ) - 1
          CHMSG = 'Drainage SM < Imbibition SM @ Rock '// &
           ROCK(IROCK)(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Brooks and Corey Function  ---
!
      ELSEIF( ISCHRX.EQ.2 .OR. ISCHRX.EQ.102 ) THEN
        IF( ISCHRX.EQ.2 ) THEN
          IF(ME.EQ.0) WRITE(IWR,'(A)') 'Brooks and Corey Function'
        ELSEIF( ISCHRX.EQ.102 ) THEN
          IF(ME.EQ.0) WRITE(IWR,'(A)') &
          'Brooks and Corey Function w/ Gas Entrapment'
        ENDIF
        VARB = 'Brooks and Corey (psi): '
        LNDX = LSCHR
        INDC = 1
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          UNTS = 'm'
          IUNM = 1
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
           ': ',SCHRX(1)
          INDX = 0
          IUNM = 1
          CALL RDUNIT(UNTS,SCHRX(1),INDX)
          IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', m)'
          T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        ENDIF
        VARB = 'Brooks and Corey (lambda): '
        LNDX = LSCHR
        INDC = 3
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          UNTS = 'null'
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
          IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
          T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        ENDIF
        VARB = 'Brooks and Corey (residual saturation): '
        LNDX = LSCHR
        INDC = 4
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          UNTS = 'null'
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
          IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
          T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
        ENDIF
        SCHRX(9) = 1.d20
        INDC = 9
        T_OK = COPYIJK2D( SCHR,SCHRX(9),LNDX,INDC,IFLG )
        IF( ISCHRX.EQ.102 ) THEN
          VARB = 'Brooks and Corey (Eff. Gas Residual ' // &
           'Sat.: Gas-Aqueous System)'
          LNDX = LSCHR
          INDC = 15
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            UNTS = 'null'
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
              ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(15))
            IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
             SCHRX(15)
            T_OK = COPYIJK2D( SCHR,SCHRX(15),LNDX,INDC,IFLG )
          ENDIF
          VARB = 'Brooks and Corey (Critical Trapping Number)'
          LNDX = LSCHR
          INDC = 9
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            UNTS = 'null'
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,SCHR,VARX, &
              ISBIN,ISHDF5 )
          ELSE
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(9))
            IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
             SCHRX(9)
            T_OK = COPYIJK2D( SCHR,SCHRX(9),LNDX,INDC,IFLG )
          ENDIF
        ENDIF
!
!---  Brooks and Corey Triple Curve Function  ---
!
      ELSEIF( ISCHRX.EQ.302 ) THEN
        IF(ME.EQ.0) WRITE(IWR,'(A)') &
          'Brooks and Corey Triple Curve Function'
        VARB = 'Brooks and Corey (main drainage psi): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(1)
        INDC =1
        T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', m)'
        VARB = 'Brooks and Corey (main drainage lambda): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
        INDC =3
        T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        VARB = 'Brooks and Corey(main drainage residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
        INDC =4
        T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
        VARB = 'Brooks and Corey (boundary wetting scanning psi): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(12))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH), ': ',SCHRX(12)
        INDC = 12
        T_OK = COPYIJK2D( SCHR,SCHRX(12),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(12),', m)'
        VARB = 'Brooks and Corey (main wetting psi): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(2)
        INDC = 2
        T_OK = COPYIJK2D( SCHR,SCHRX(2),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(2),', m)'
        VARB = 'Brooks and Corey (main wetting lambda): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(5))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(5)
        INDC = 5
        T_OK = COPYIJK2D( SCHR,SCHRX(5),LNDX,INDC,IFLG )
        VARB = 'Brooks and Corey (main wetting residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(6))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(6)
        INDC = 6
        T_OK = COPYIJK2D( SCHR,SCHRX(6),LNDX,INDC,IFLG )
        VARB = 'Brooks and Corey (wetting maximum saturation)'
        SCHRX(10) = 1.D+0
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(10))
        IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),': ',SCHRX(10)
        INDC = 10
        T_OK = COPYIJK2D( SCHR,SCHRX(10),LNDX,INDC,IFLG )
        IF( SCHRX(4).LT.SCHRX(6) ) THEN
          INDX = 4
          NCH = INDEX( ROCK(IROCK),'  ' ) - 1
          CHMSG = 'Drainage SM < Imbibition SM @ Rock '// &
           ROCK(IROCK)(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Dual Porosity van Genuchten Function  ---
!
      ELSEIF( ISCHRX.EQ.3 ) THEN
        IF(ME.EQ.0) WRITE(IWR,'(A)') &
          'Dual Porosity van Genuchten Function'
        VARB = 'Matrix van Genuchten (alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(1)
        INDC = 1
        T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', 1/m)'
        VARB = 'Matrix van Genuchten (n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
        INDC = 3
        T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        VARB = 'Matrix van Genuchten (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
        INDC = 4
        T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
        VARB = 'Fracture van Genuchten (alpha), '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(5))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH), ': ',SCHRX(5)
        INDC = 5
        T_OK = COPYIJK2D( SCHR,SCHRX(5),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHRX(5),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(5),', 1/m)'
        VARB = 'Fracture van Genuchten (n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(6))
        INDC = 6
        T_OK = COPYIJK2D( SCHR,SCHRX(6),LNDX,INDC,IFLG )
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(6)
        VARB = 'Fracture van Genuchten (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(7))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(7)
        INDC = 7
        T_OK = COPYIJK2D( SCHR,SCHRX(7),LNDX,INDC,IFLG )
        VARB = 'Matrix van Genuchten (m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(14))
        IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),': ',SCHRX(14)
        INDC = 14
        T_OK = COPYIJK2D( SCHR,SCHRX(14),LNDX,INDC,IFLG )
        VARB = 'Fracture van Genuchten (m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(15))
        IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),': ',SCHRX(15)
        INDC = 15
        T_OK = COPYIJK2D( SCHR,SCHRX(15),LNDX,INDC,IFLG )
!
!---  Dual Porosity Brooks and Corey Function  ---
!
      ELSEIF( ISCHRX.EQ.4 ) THEN
        IF(ME.EQ.0) WRITE(IWR,'(A)') &
          'Dual Porosity Brooks and Corey Function'
        VARB = 'Matrix Brooks and Corey (psi)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(1)
        INDC = 1
        T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', m)'
        VARB = 'Matrix Brooks and Corey (lambda): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
        INDC = 3
        T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        VARB = 'Matrix Brooks and Corey (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
        INDC = 4
        T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
        VARB = 'Fracture Brooks and Corey (psi)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(5))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4)') &
          VARB(1:IVR),', ',UNTS(1:NCH), ': ',SCHRX(5)
        INDC = 5
        T_OK = COPYIJK2D( SCHR,SCHRX(5),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHRX(5),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(5),', m)'
        VARB = 'Fracture Brooks and Corey (lambda): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(6))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(6)
        INDC = 6
        T_OK = COPYIJK2D( SCHR,SCHRX(6),LNDX,INDC,IFLG )
        VARB = 'Fracture Brooks and Corey (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(7))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(7)
        INDC = 7
        T_OK = COPYIJK2D( SCHR,SCHRX(7),LNDX,INDC,IFLG )
!
!---  Haverkamp Function  ---
!
      ELSEIF( ABS(ISCHRX).EQ.5 ) THEN
        IF( ISCHRX.EQ.-5 ) THEN
          IF(ME.EQ.0) WRITE(IWR,'(A)') 'Haverkamp w/ Log Function'
        ELSE
          IF(ME.EQ.0) WRITE(IWR,'(A)') 'Haverkamp Function'
        ENDIF
        VARB = 'Haverkamp Air Entry Head (psi): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(1)
        INDC = 1
        T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', m)'
        VARB = 'Haverkamp (alpha): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(2)
        INDC = 2
        T_OK = COPYIJK2D( SCHR,SCHRX(2),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHRX(2),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(2),', m)'
        INDX = 0
        IUNM = 1
        SCHRX(5) = 1.D+0
        INDC = 5
        T_OK = COPYIJK2D( SCHR,SCHRX(5),LNDX,INDC,IFLG )
        CALL RDUNIT(UNTS,SCHRX(5),INDX)
        VARB = 'Haverkamp (beta): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
        INDC = 3
        T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        VARB = 'Haverkamp (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
        INDC = 4
        T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
!
!---  Luckner-van Genuchten-Nielsen Function  ---
!
      ELSEIF( ISCHRX.EQ.8 ) THEN
        IF(ME.EQ.0) WRITE(IWR,'(A)') &
          'Luckner-van Genuchten-Nielsen Function'
        VARB = 'Luckner et al. (alpha drainage)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(1)
        INDC = 1
        T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', 1/m)'
        VARB = 'Luckner et al. (alpha wetting)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(2)
        INDC = 2
        T_OK = COPYIJK2D( SCHR,SCHRX(2),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHRX(2),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(2),', 1/m)'
        VARB = 'Luckner et al. (n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
        INDC = 3
        T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        VARB = 'Luckner et al. (aqueous residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
        INDC = 4
        T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
        VARB = 'Luckner et al. (gas residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(5))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(5)
        INDC = 5
        T_OK = COPYIJK2D( SCHR,SCHRX(5),LNDX,INDC,IFLG )
        VARB = 'Luckner et al. (m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(14))
        IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),': ',SCHRX(14)
        INDC = 14
        T_OK = COPYIJK2D( SCHR,SCHRX(14),LNDX,INDC,IFLG )
!
!---  Russo Function  ---
!
      ELSEIF( ISCHRX.EQ.9 ) THEN
        if(me.eq.0) WRITE(IWR,'(A)') 'Russo Function'
        VARB = 'Russo (alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(1)
        INDC = 1
        T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', 1/m)'
        VARB = 'Russo (m): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
        INDC = 3
        T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        VARB = 'Russo (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
        INDC = 4
        T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
!
!---  Polynomial Function  ---
!
        ELSEIF( ISCHRX.EQ.19 ) THEN
          IF(ME.EQ.0) WRITE(IWR,'(A)') 'Polynomial Function'
          VARB = 'Number of Polynomial Function Pieces'
          CALL RDINT(ISTART,ICOMMA,CHDUM,NPLY_SL(IROCK))
          IF(ME.EQ.0) WRITE(IWR,'(2X,2A,I1)') VARB(1:IVR),': ',NPLY_SL(IROCK)
          IF( NPLY_SL(IROCK).GT.LPOLYN ) THEN
            INDX = 5
            CHMSG = 'Number of Saturation Polynomial Function ' // &
             'Pieces > Parameter LPOLYN'
            CALL WRMSGS( INDX )
          ENDIF
          DO 360 NPX = 1,NPLY_SL(IROCK)
            VARB = 'Polynomial Piece #  : '
            WRITE(VARB(14:14),'(I1)') NPX
            ISTART = 1
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            VARB = 'Number of Polynomial Coefficients'
            CALL RDINT(ISTART,ICOMMA,CHDUM,NCOEF)
            IF( (NCOEF+4).GT.LPOLYC ) THEN
              INDX = 5
              CHMSG = 'Number of Saturation Polynomial Coefficients ' // &
               ' > Parameter LPOLYC'
              CALL WRMSGS( INDX )
            ENDIF
            VARB = 'Minimum Head for Polynomial Piece'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_SL(1,NPX,IROCK))
            VARB = 'Maximum Head for Polynomial Piece'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_SL(2,NPX,IROCK))
            CPLY_SL(3,NPX,IROCK) = 0.D+0
            CPLY_SL(4,NPX,IROCK) = 0.D+0
            DO 340 NCX = 5,NCOEF+4
              VARB = 'Coefficient for Polynomial Piece'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_SL(NCX,NPX,IROCK))
!
!---          Maximum saturation for polynomial piece ---
!
              CPLY_SL(3,NPX,IROCK) = CPLY_SL(3,NPX,IROCK) +  &
               CPLY_SL(NCX,NPX,IROCK)*(LOG10(CPLY_SL(1,NPX,IROCK)) &
               **(NCX-5))
!
!---          Mininum saturation for polynomial piece ---
!
              CPLY_SL(4,NPX,IROCK) = CPLY_SL(4,NPX,IROCK) +  &
               CPLY_SL(NCX,NPX,IROCK)*(LOG10(CPLY_SL(2,NPX,IROCK)) &
               **(NCX-5))
  340       CONTINUE
            VARB = 'Head Units for Polynomial Piece'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF(ME.EQ.0) WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ',UNTS
            SCHRX(1) = 1.D+0
            INDC = 1
            T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,SCHRX(1),INDX)
            IF(ME.EQ.0) WRITE(IWR,'(2X,3A,1PE11.4,$)') &
             'Minimum Head for Polynomial Piece, ',UNTS(1:NCH), &
             ': ',CPLY_SL(1,NPX,IROCK)
            VAR = CPLY_SL(1,NPX,IROCK)*SCHRX(1)
            IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', m)'
            IF(ME.EQ.0) WRITE(IWR,'(2X,3A,1PE11.4,$)') &
             'Maximum Head for Polynomial Piece, ',UNTS(1:NCH), &
             ': ',CPLY_SL(2,NPX,IROCK)
            VAR = CPLY_SL(2,NPX,IROCK)*SCHRX(1)
            IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', m)'
            DO 350 NCX = 5,NCOEF+4
              VARB = 'Coefficient #  : '
              IF(ME.EQ.0) WRITE(VARB(14:14),'(I1)') NCX-4
              IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:17), &
               CPLY_SL(NCX,NPX,IROCK)
  350       CONTINUE
  360     CONTINUE
!
!---  Cambridge Function  ---
!
      ELSEIF( ISCHRX.EQ.41 ) THEN
        IF(ME.EQ.0) WRITE(IWR,'(A)') 'Cambridge Function'
        VARB = 'Cambridge (alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') &
          VARB(1:IVR),', ',UNTS(1:NCH),': ',SCHRX(1)
        INDC = 1
        T_OK = COPYIJK2D( SCHR,SCHRX(1),LNDX,INDC,IFLG )
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHRX(1),INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHRX(1),', m)'
        VARB = 'Cambridge (m): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(3))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(3)
        INDC = 3
        T_OK = COPYIJK2D( SCHR,SCHRX(3),LNDX,INDC,IFLG )
        VARB = 'Cambridge (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHRX(4))
        IF(ME.EQ.0) WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHRX(4)
        INDC = 4
        T_OK = COPYIJK2D( SCHR,SCHRX(4),LNDX,INDC,IFLG )
!
!---  Tabular  ---
!
      ELSEIF( ISCHRX.GE.10 .AND. ISCHRX.LE.13 ) THEN
        IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
          IF(ME.EQ.0) WRITE(IWR,'(A)') &
            'Tabular Water Content versus Capillary Head'
        ELSE
          IF( ISCHRX.GE.12 .AND. ISCHRX.LE.13 ) THEN
            IF(ME.EQ.0) WRITE(IWR,'(A)') &
              'Tabular Aqueous Saturation versus Log Capillary Head'
          ELSE
            IF(ME.EQ.0) WRITE(IWR,'(A)') &
              'Tabular Aqueous Saturation versus Capillary Head'
          ENDIF
        ENDIF
        VARB = 'Number of Table Entries'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
        IF(.NOT.ALLOCATED(ISLTBLX)) ALLOCATE(ISLTBLX(2,NROCK))
        DO I = 1,MAX_IND_FLD
          IF( 'isltbl'.eq.i_nd_2fld_names(i) )THEN
            ISALLOC = 1
          ENDIF
        END DO
        IF( ISALLOC.EQ.0 )THEN
           DIM1 = 2
           CALL ADD_NODE_I2FIELD('isltbl', DIM1, IDX)
           ISLTBL => I_ND_2FLD(IDX)%P
           ISLTBL = 0.d0
           ISALLOC = 1
        ENDIF
        IF(ME.EQ.0) WRITE(IWR,'(2A,I6)') VARB(1:IVR),': ',NLIN
        IF( NLIN.LT.2 ) THEN
          INDX = 4
          CHMSG = 'Saturation Invalid Table'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IJK > 0 )THEN
          LTBLX = LTBL*NUM_NODES
        ELSE
          LTBLX = LTBL
        ENDIF
        IF(.NOT.ALLOCATED(TBLX)) ALLOCATE(TBLX(LTBLX))
        IF(.NOT.ALLOCATED(TBLY)) ALLOCATE(TBLY(LTBLX))
        IF( INDEX(ADUM,'file').NE.0 ) THEN
          VARB = 'Capillary Head'
          UNTS = 'm'
          IUNM = 1
          NTBLX = NTBL
          ILOG = 0
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) ILOG = 1
          T_OK = GETFILENAME( ADUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = 1.0D0
          CALL RDIJKT( T_FILENAME,TBLX,VARX,ISLTBL,NLIN,NTBLX,ILOG, &
            ISBIN,ISHDF5 )
          IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
            VARB = 'Water Content'
          ELSE
            VARB = 'Saturation'
          ENDIF
          UNTS = 'null'
          ILOG = 0
          T_OK = GETFILENAME( ADUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = 1.0D0
          CALL RDIJKT( T_FILENAME,TBLY,VARX,ISLTBL,NLIN,NTBLX,ILOG, &
            ISBIN,ISHDF5 )
          IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
            DO 380 N = 1,NUM_NODES
              DO 380 M = ISLTBL(1,N),ISLTBL(2,N)
                TBLY(M) = TBLY(M)/POR(2,N)
  380       CONTINUE
          ENDIF
        ELSE
          ISLTBLX(1,IROCK) = NTBL + 1
          T_OK = COPYIJK2D_INT( ISLTBL,ISLTBLX(1,IROCK),2,1,IROCK )
          DO 400 NL = 1,NLIN
            NTBL = NTBL + 1
            IF( NTBL.GT.LTBLx ) THEN
              INDX = 5
              CHMSG = 'Number of Table Values > Parameter LTBLX'
              CALL WRMSGS( INDX )
            ENDIF
            ISTART = 1
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            VARB = 'Capillary Head'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF(ME.EQ.0) &
               WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
             UNTS(1:NCH),': ',TBLX(NTBL)
            INDX = 0
            IUNM = 1
            VARX = 1.D+0
            CALL RDUNIT(UNTS,VARX,INDX)
            IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
              TBLX(NTBL) = LOG( EXP(TBLX(NTBL)*VARX) )
            ELSE
              TBLX(NTBL) = VARX*TBLX(NTBL)
            ENDIF
            IF(ME.EQ.0) &
              WRITE(IWR,'(A,1PE11.4,A)') ' (',TBLX(NTBL),', m)'
            IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
              VARB = 'Water Content'
            ELSE
              VARB = 'Saturation'
            ENDIF
            CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLY(NTBL))
            IF(ME.EQ.0) WRITE(IWR,'(2X,2A,1PE11.4)') &
               VARB(1:IVR),': ',TBLY(NTBL)
            IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
              TBLY(NTBL) = TBLY(NTBL)/POR(2,IROCK)
            ENDIF
            IF( NL.EQ.2 ) THEN
              IF( TBLX(NTBL-1).LT.TBLX(NTBL) ) THEN
                ITDX = 1
              ELSEIF( TBLX(NTBL-1).GT.TBLX(NTBL) ) THEN
                ITDX = -1
              ELSE
                INDX = 4
                CHMSG = 'Invalid Saturation Table'
                CALL WRMSGS( INDX )
              ENDIF
              IF( TBLY(NTBL-1).LT.TBLY(NTBL) ) THEN
                ITDY = 1
              ELSEIF( TBLY(NTBL-1).GT.TBLY(NTBL) ) THEN
                ITDY = -1
              ELSE
                INDX = 4
                CHMSG = 'Invalid Saturation Table'
                CALL WRMSGS( INDX )
              ENDIF
            ELSEIF( NL.GT.2 ) THEN
              IF( (ITDX.EQ.1 .AND. TBLX(NTBL).LE.TBLX(NTBL-1)) .OR. &
               (ITDX.EQ.-1 .AND. TBLX(NTBL).GE.TBLX(NTBL-1)) )THEN
                INDX = 4
                CHMSG = 'Invalid Saturation Table'
                CALL WRMSGS( INDX )
              ENDIF
              IF( (ITDY.EQ.1 .AND. TBLY(NTBL).LE.TBLY(NTBL-1)) .OR. &
               (ITDY.EQ.-1 .AND. TBLY(NTBL).GE.TBLY(NTBL-1)) )THEN
                INDX = 4
                CHMSG = 'Invalid Saturation Table'
                CALL WRMSGS( INDX )
              ENDIF
            ENDIF
  400     CONTINUE
          ISLTBLX(2,IROCK) = NTBL
          T_OK = COPYIJK2D_INT( ISLTBL,ISLTBLX(2,IROCK),2,2,IROCK )
          IF( ISCHRX.EQ.11 ) THEN
            CALL SPLINY( ISLTBLX(1,IROCK),ISLTBLX(2,IROCK) )
            CALL SPLINX( ISLTBLX(1,IROCK),ISLTBLX(2,IROCK) )
          ENDIF
        ENDIF
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( NX.LT.NROCK .AND. ME.EQ.0) WRITE(IWR,'(/)')
      GOTO 10
 500  CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDSP1 group ---
!
      RETURN
      END
