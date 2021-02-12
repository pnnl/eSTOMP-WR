!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDHYDR
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
!     Read input file for rock/soil hydraulic information.
!     File read support exists even if ijk indexing is not used.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by VL Freedman, PNNL, 11 March 2013.
!     $Id: rdhydr.F,v 1.9 2006/11/28 00:13:12 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

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
!
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,UNTS,T_FILENAME
      CHARACTER*512 CHDUM
      INTEGER :: DIM1,IPRFX,LO(3),HI(3),LDXX(3)
      DOUBLE PRECISION :: PERMX(9)
      LOGICAL T_OK
      LOGICAL :: ISBIN
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDHYDR'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(145)(1:1),'$').EQ.0 ) CVS_ID(145) = &
      '$Id: rdhydr.F,v 1.9 2006/11/28 00:13:12 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Rock/Soil Hydraulic Properties Card'
      ICD = INDEX( CARD,'  ' )-1
      IF(ME.EQ.0)WRITE (ISC,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      IF(ME.EQ.0)WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
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
!---  Loop over the rock/soil hydraulic information lines  ---
!
      ISBIN = .FALSE.
      NX = 0
      IJK = 0
      DIM1 = 9
      CALL ADD_NODE_D2FIELD('perm', DIM1, IDX)
      PERM => D_ND_2FLD(IDX)%P
      PERM = 0.d0
      CALL ADD_NODE_IFIELD('iprf', IDX)
      IPRF => I_ND_FLD(IDX)%P
      IPRF = 0
   10 CONTINUE
      IF( NX.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  IJK indexing (property read from file)  ---
!
      IF( INDEX(ADUM(1:),'indexing').NE.0 ) THEN
        IJK = 1
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = 1,NROCK
         IF( ADUM.EQ.ROCK(M) ) THEN
            IROCK = M
            ISGRP = 0
            GOTO 200
         ENDIF
  100 CONTINUE
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // ADUM(1:NCH)
      CALL WRMSGS( INDX )
      GOTO 10
  200 CONTINUE
!
!---    Write rock/soil name  ---
!
      IF(ME.EQ.0)WRITE (ISC,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      IF(ME.EQ.0)WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      NX = NX + 1
  220 CONTINUE
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read intrinsic permeabilities: three coordinate directions ---
!
      VARB = 'X-Direction Permeability'
      INDC = 1
      LNDX = 9
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        ISTX = ISTART
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IHC = INDEX( UNTS(1:),'hc' )
        IF( IHC .EQ. 0 ) THEN
          UNTS = 'm^2'
          IUNM = 2
        ELSE
          UNTS = 'm/s'
          IUNM = 1
          IUNS = -1
        ENDIF
        ISTART = ISTX
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,PERM,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,PERMX(1))
        IF( ICOMMA .EQ. ISTART .AND. IFLD .GT. 1 ) THEN
          INDX = 4
          CHMSG = 'Zero X-Dir. Intrinsic Permeability'
          CALL WRMSGS( INDX )
        ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IHC = INDEX( UNTS(1:),'hc' )
        IF( IHC .EQ. 0 ) THEN
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(1)
          IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(1)
          IUNM = 2
        ELSE
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
            UNTS(1:NCH),': ',PERMX(1)
          IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
            UNTS(1:NCH),': ',PERMX(1)
          IUNM = 1
          IUNS = -1
        ENDIF
        INDX = 0
        CALL RDUNIT(UNTS,PERMX(1),INDX)
        IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',PERMX(1),', m^2)'
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',PERMX(1),', m^2)'
        T_OK = COPYIJK2D( PERM,PERMX(1),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Y-Direction Permeability'
      INDC = 2
      LNDX = 9
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        ISTX = ISTART
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IHC = INDEX( UNTS(1:),'hc' )
        IF( IHC .EQ. 0 ) THEN
          UNTS = 'm^2'
          IUNM = 2
        ELSE
          UNTS = 'm/s'
          IUNM = 1
          IUNS = -1
        ENDIF
        ISTART = ISTX
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,PERM,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,PERMX(2))
        IF( ICOMMA .EQ. ISTART .AND. JFLD .GT. 1 ) THEN
          INDX = 4
          CHMSG = 'Zero Y-Dir. Intrinsic Permeability'
          CALL WRMSGS( INDX )
        ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IHC = INDEX( UNTS(1:),'hc' )
        IF( IHC .EQ. 0 ) THEN
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(2)
          IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(2)
          IUNM = 2
        ELSE
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(2)
          IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(2)
          IUNM = 1
          IUNS = -1
        ENDIF
        INDX = 0
        CALL RDUNIT(UNTS,PERMX(2),INDX)
        IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',PERMX(2),', m^2)'
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',PERMX(2),', m^2)'
        T_OK = COPYIJK2D( PERM,PERMX(2),LNDX,INDC,IFLG )
!        T_OK = COPYIJK2D( PERM,PERMX(2),LNDX,INDC,IROCK )
      ENDIF
      VARB = 'Z-Direction Permeability'
      INDC = 3
      LNDX = 9
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        ISTX = ISTART
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IHC = INDEX( UNTS(1:),'hc' )
        IF( IHC .EQ. 0 ) THEN
          UNTS = 'm^2'
          IUNM = 2
        ELSE
          UNTS = 'm/s'
          IUNM = 1
          IUNS = -1
        ENDIF
        ISTART = ISTX
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,PERM,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,PERMX(3))
        IF( ICOMMA .EQ. ISTART .AND. KFLD .GT. 1 ) THEN
          INDX = 4
          CHMSG = 'Zero Z-Dir. Intrinsic Permeability'
          CALL WRMSGS( INDX )
        ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IHC = INDEX( UNTS(1:),'hc' )
        IF( IHC .EQ. 0 ) THEN
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(3)
          IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(3)
          IUNM = 2
        ELSE
          if(me.eq.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(3)
          if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',PERMX(3)
          IUNM = 1
          IUNS = -1
        ENDIF
        INDX = 0
        CALL RDUNIT(UNTS,PERMX(3),INDX)
        IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',PERMX(3),', m^2)'
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',PERMX(3),', m^2)'
        T_OK = COPYIJK2D( PERM,PERMX(3),LNDX,INDC,IFLG )
!        T_OK = COPYIJK2D( PERM,PERMX(3),LNDX,INDC,IROCK )
      ENDIF
      IDPX = 0
      IF( INDEX(ADUM(1:),'dp').NE.0 ) THEN
        DO 290 N = 1,NUM_NODES
          IDP(N) = 1
          IF( INDEX(ADUM(1:),'pseudo').NE.0 ) IDP(N) = -1
  290   CONTINUE
        IDPX = 1
      ENDIF
!
!---  Read fracture total and diffusive porosities for 
!     dual porosity soils
!
        IF( INDEX(ADUM(1:),'fractured').NE.0 .OR. &
         ABS(IDPX).EQ.1 ) THEN
          INDC = 7
          LNDX = 9
          VARB = 'Fracture X-Direction Permeability'
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            ISTX = ISTART
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC .EQ. 0 ) THEN
              UNTS = 'm^2'
              IUNM = 2
            ELSE
              UNTS = 'm/s'
              IUNM = 1
              IUNS = -1
            ENDIF
            ISTART = ISTX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,PERM,VARX, &
             ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERMX(7))
            IF( ICOMMA .EQ. ISTART .AND. IFLD .GT. 1 ) THEN
              INDX = 4
              CHMSG = 'Zero X-Dir. Intrinsic Permeability'
              CALL WRMSGS( INDX )
            ENDIF
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC .EQ. 0 ) THEN
              IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
                UNTS(1:NCH),': ',PERMX(7)
              IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
                UNTS(1:NCH),': ',PERMX(7)
              IUNM = 2
            ELSE
              IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(7)
              IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(7)
              IUNM = 1
              IUNS = -1
            ENDIF
            INDX = 0
            CALL RDUNIT(UNTS,PERMX(7),INDX)
            IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',PERMX(7),', m^2)'
            IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',PERMX(7),', m^2)'
            T_OK = COPYIJK2D( PERM,PERMX(7),LNDX,INDC,IFLG )
          ENDIF
          DO 300 N = 1,NUM_NODES
            PERM(4,N) = PERM(1,N)
            PERM(1,N) = PERM(4,N)*(1.D+0-POR(4,N)) + &
             PERM(7,N)*POR(4,N)
            IF( ABS(IDPX).EQ.1 ) PERM(1,N) = PERM(7,N)
  300     CONTINUE
          VARB = 'Fracture Y-Direction Permeability'
          INDC = 8
          LNDX = 9
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            ISTX = ISTART
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC .EQ. 0 ) THEN
              UNTS = 'm^2'
              IUNM = 2
            ELSE
              UNTS = 'm/s'
              IUNM = 1
              IUNS = -1
            ENDIF
            ISTART = ISTX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,PERM,VARX, &
             ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERMX(8))
            IF( ICOMMA .EQ. ISTART .AND. JFLD .GT. 1 ) THEN
              INDX = 4
              CHMSG = 'Zero Y-Dir. Intrinsic Permeability'
              CALL WRMSGS( INDX )
            ENDIF
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC .EQ. 0 ) THEN
              IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(8)
              IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(8)
              IUNM = 2
            ELSE
              IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(8)
              IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(8)
              IUNM = 1
              IUNS = -1
            ENDIF
            INDX = 0
            CALL RDUNIT(UNTS,PERMX(8),INDX)
            IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',PERMX(8),', m^2)'
            IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',PERMX(8),', m^2)'
            T_OK = COPYIJK2D( PERM,PERMX(8),LNDX,INDC,IFLG )
          ENDIF
          DO 310 N = 1,NUM_NODES
            PERM(5,N) = PERM(2,N)
            PERM(2,N) = PERM(5,N)*(1.D+0-POR(4,N)) + &
             PERM(8,N)*POR(4,N)
            IF( ABS(IDPX).EQ.1 ) PERM(2,N) = PERM(8,N)
  310     CONTINUE
          VARB = 'Fracture Z-Direction Permeability'
          INDC = 9
          LNDX = 9
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            ISTX = ISTART
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC .EQ. 0 ) THEN
              UNTS = 'm^2'
              IUNM = 2
            ELSE
              UNTS = 'm/s'
              IUNM = 1
              IUNS = -1
            ENDIF
            ISTART = ISTX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,PERM,VARX, &
             ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERMX(9))
            IF( ICOMMA .EQ. ISTART .AND. KFLD .GT. 1 ) THEN
              INDX = 4
              CHMSG = 'Zero Z-Dir. Intrinsic Permeability'
              CALL WRMSGS( INDX )
            ENDIF
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC .EQ. 0 ) THEN
              IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(9)
              IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(9)
              IUNM = 2
            ELSE
              IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(9)
              IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
               UNTS(1:NCH),': ',PERMX(9)
              IUNM = 1
              IUNS = -1
            ENDIF
            INDX = 0
            CALL RDUNIT(UNTS,PERMX(9),INDX)
            IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',PERMX(9),', m^2)'
            IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',PERMX(9),', m^2)'
            T_OK = COPYIJK2D( PERM,PERMX(9),LNDX,INDC,IFLG )
          ENDIF
          DO 320 N = 1,NUM_NODES
            PERM(6,N) = PERM(3,N)
            PERM(3,N) = PERM(6,N)*(1.D+0-POR(4,N)) + &
              PERM(9,N)*POR(4,N)
            IF( ABS(IDPX).EQ.1 ) PERM(3,N) = PERM(9,N)
  320     CONTINUE
          ENDIF
!
!---    Check for permeability reduction factors  ---
!
        CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          DO 350 N = 1,NUM_NODES
            IPRF(N) = 1
  350     CONTINUE
          VARB = 'Pore-Body Fractional Length'
          UNTS = 'null'
          INDC = 4
          LNDX = 9
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,PERM,VARX, &
             ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERMX(4))
            IF(ME.EQ.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PERMX(4)
            IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PERMX(4)
            T_OK = COPYIJK2D( PERM,PERMX(9),LNDX,INDC,IFLG )
          ENDIF
          DO 360 N = 1,NUM_NODES
            IF( PERM(4,N).LE.0.D+0.OR.PERM(4,N).GE.1.D+0 ) THEN
              INDX = 9
              CHMSG = 'Out of Range Pore-Body Fractional Length: '
              RLMSG = PERM(4,N)
              CALL WRMSGS( INDX )
            ENDIF
  360     CONTINUE
          VARB = 'Fractional Critical Porosity'
          UNTS = 'null'
          INDC = 5
          LNDX = 9
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,PERM,VARX, &
             ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERMX(5))
            IF(ME.EQ.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PERMX(5)
            IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PERMX(5)
            T_OK = COPYIJK2D( PERM,PERMX(9),LNDX,INDC,IFLG )
          ENDIF
          DO 380 N = 1,NUM_NODES
            IF( PERM(5,N).LE.0.D+0.OR.PERM(5,N).GE.1.D+0 ) THEN
              INDX = 9
              CHMSG = 'Out of Range Fractional Critical Porosity: '
              RLMSG = PERM(5,N)
              CALL WRMSGS( INDX )
            ENDIF
  380     CONTINUE
        ENDIF
!
!---    Check for Kozeny-Carmen   ---
!
        CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(ADUM(1:),'kozeny').NE.0 ) THEN
            if(me.eq.0) WRITE(IWR,'(a)') 'Kozeny & Carmen Intrinsic Permeability'
            DO 410 N = 1,NUM_NODES
              if(iz(n) == irock) then
              IPRF(N) = 2
!              CALL PERM_I( PERMRFX,POR(1,IZ(N)),iz(n))
              CALL PERM_I( PERMRFX,POR(1,N),n)
              PERM(1,N)=PERMX(1)/PERMRFX
              PERM(2,N)=PERMX(2)/PERMRFX
              PERM(3,N)=PERMX(3)/PERMRFX
              endif
  410       CONTINUE
          ENDIF
        ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
        IF( NX.LT.NROCK .and.ME.EQ.0) WRITE(IWR,'(/)')
        GOTO 10
 500  CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDHYDR group ---
!
      RETURN
      END
