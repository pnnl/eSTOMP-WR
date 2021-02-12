!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINAC( ROCKFILENAME )
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
!     Read input file for inactive node information.
!     Label inactive and active nodes.
!     Determine node sequencing for the Jacobian matrix.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     $Id: rdinac.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE FILES
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
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,ROCKFILENAME
      CHARACTER*512 CHDUM
!
      REAL*8, ALLOCATABLE :: BUF1D(:)
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3), G_BUF, THREE
      INTEGER, ALLOCATABLE :: VAL_BUF(:)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:)
      INTEGER, ALLOCATABLE :: BUF3(:,:,:)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IZL
      LOGICAL STATUS,ISBIN,T_OK,ISHDF5
      LOGICAL, EXTERNAL :: CREATE_INTGA,COPYNDFLD,CHKSTAT,RDIJK1D
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      SUBNMX = '/RDINAC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(147)(1:1),'$').EQ.0 ) CVS_ID(147) = &
       '$Id: rdinac.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
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
      VARX = 1.0D0
      ISHDF5 = .FALSE.
      ISBIN = .TRUE.
!
!---  Add matrix index field to grid and initialize to 1  ---
!
      CALL ADD_NODE_IFIELD('inactive',IDX)
      IXP => I_ND_FLD(IDX)%P
      IXP = 1
!
!---  Write card information to ouput file  ---
!
      CARD = 'Inactive Nodes Card'
      ICD = INDEX( CARD,'  ' )-1
      IF (ME.EQ.0) THEN
        WRITE(ISC,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      ENDIF
      NXP = 0
      NDOM = 1
!
!---  Read optional number of inactive node card entries  ---
!
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      VARB = 'Multiple Entries Switch'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM,'multiple').NE.0 ) THEN
        VARB = 'Number of Entries'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NDOM)
      ENDIF
!
!---  Define inactive nodes  ---
!
      DO 900 NDM = 1,NDOM
        ISTART = 1
        IF( NDM.GT.1 ) THEN
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
        ENDIF
        VARB = 'Input Option [Rock/Soil, Zonation File, File, Integer]'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM,'hdffile').NE.0 )THEN
           ISHDF5 = .TRUE.
         ENDIF
        IF( INDEX(ADUM,'formatted').NE.0 )ISBIN = .FALSE.
!
!---  Read inactive node information according to rock/soil type  ---
!
        IF( INDEX(ADUM,'rock').NE.0 .OR. INDEX(ADUM,'soil').NE.0 ) THEN
          VARB = 'Number of Rock/Soil Type Lines'
          CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
          IF (ME.EQ.0) THEN
            WRITE(ISC,'(/,A)') 'Inactive Rock/Soil Types'
          ENDIF
          DO 60 L = 1,NLIN
            ISTART = 1
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            VARB = 'Rock/Soil Name'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Search known rock types for a matching type ---
!
            DO 30 M = 1, NROCK
               IF( ADUM .EQ. ROCK(M)) THEN
                  IROCK = M
                  GOTO 40
               ENDIF
   30       CONTINUE
            INDX = 2
            CHMSG = 'Unrecognized Rock/Soil Type: '//ADUM(1:NCH)
            CALL WRMSGS( INDX )
            GOTO 60
   40       CONTINUE
            IF (ME.EQ.0) THEN
              WRITE( ISC,'(2x,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
            ENDIF
            DO 50 N = 1, NUM_NODES
              IF( IZ(N).EQ.IROCK ) THEN
                IXP(N) = 0
                IF (GRID_MASK(N).GT.0) then
                  NXP = NXP + 1
                endif
              ENDIF
   50       CONTINUE
   60     CONTINUE
!
!---  Read inactive node information from an external zonation file  ---
!
        ELSEIF( INDEX(ADUM,'zonation').NE.0.AND.INDEX(ADUM,'file').NE.0.OR. &
          INDEX(ADUM,'hdffile').NE.0 ) THEN
          VARB = 'Rock/soil zonation external file name'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF(.NOT.ALLOCATED(IZX))ALLOCATE(IZX(NUM_NODES))
          IF( TRIM( ROCKFILENAME ).EQ.TRIM( ADUM ) )THEN
            IZX = IZ
          ELSE
            ALLOCATE( BUF1D(LDXX(1)*LDXX(2)*LDXX(3)),STAT=ISTAT )
            T_OK = CHKSTAT( 'BUF1D',ISTAT,INDX )
            T_OK = RDIJK1D( ADUM,LDXX,LO,HI,BUF1D,VARX,ISBIN,ISHDF5 )
            IZX = INT(BUF1D)
          ENDIF
          DO 70 N = 1, NUM_NODES
            IF( IZX(N).EQ.0 ) THEN
              IXP(N) = 0
              IF(GRID_MASK(N)>0)NXP = NXP + 1
            ENDIF
   70     CONTINUE
          DEALLOCATE(IZX)
!
!---  Read inactive node information from an external file  ---
!
      ELSEIF( INDEX(ADUM,'file').NE.0 ) THEN
        T_OK = CREATE_INTGA( G_BUF,NXDIM,NYDIM,NZDIM )
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        NCH = INDEX(ADUM,'  ')-1
        CALL GA_FILL(G_BUF,1)
        IF (ME.EQ.0) THEN
          IJDIM = NXDIM*NYDIM
          ALLOCATE(IDX_BUF(3,IJDIM))
          ALLOCATE(VAL_BUF(IJDIM))
          VAL_BUF = 0
          OPEN(UNIT=27, FILE=ADUM(1:NCH), STATUS='OLD', FORM='FORMATTED')
          WRITE(ISC,'(/,2A)') 'Inactive Node File: ',ADUM(1:NCH)
          NCOUNT = 0
   80     CONTINUE
          READ(27,*,END=90) I,J,K
          NCOUNT = NCOUNT + 1
          IDX_BUF(1,NCOUNT) = I
          IDX_BUF(2,NCOUNT) = J
          IDX_BUF(3,NCOUNT) = K
          IF( I.LT.1 .OR. I.GT.NXDIM .OR. J.LT.1 .OR. J.GT.NYDIM &
            .OR. K.LT.1 .OR. K.GT.NZDIM ) THEN
            INDX = 4
            CHMSG = 'Inactive Node Index Out of Range'
            CALL WRMSGS(INDX)
          ENDIF
          IF (MOD(NCOUNT,IJDIM).EQ.0) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
          ENDIF
          GOTO 80
   90     CONTINUE
          IF (MOD(NCOUNT,IJDIM).GT.0) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
          ENDIF
          CLOSE(UNIT=27)
          DEALLOCATE(VAL_BUF)
          DEALLOCATE(IDX_BUF)
        ENDIF
        LXP = 1
        T_OK = COPYNDFLD( G_BUF,IXP,LDXX,LO,HI,LXP  )
!        CALL GA_SYNC
      ELSE
!
!---  Read inactive node information from the input file  ---
!
        T_OK = CREATE_INTGA( G_BUF,NXDIM,NYDIM,NZDIM )
        CALL GA_FILL(G_BUF,1)
        VARB = 'Number of Inactive Node Lines'
        ISTART = 1
        CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
        DO 400 NL = 1, NLIN
          ISTART = 1
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          VARB = 'Inactive Node Domain Index'
          CALL RDINT(ISTART,ICOMMA,CHDUM,I1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,I2)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J2)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K2)
          I1 = MAX( 1,I1 )
          I1 = MIN( I1,I2,nxdim )
          I2 = MAX( 1,I1,I2 )
          I2 = MIN( I2,nxdim )
          J1 = MAX( 1,J1 )
          J1 = MIN( J1,J2,nydim )
          J2 = MAX( 1,J1,J2 )
          J2 = MIN( J2,nydim )
          K1 = MAX( 1,K1 )
          K1 = MIN( K1,K2,nzdim )
          K2 = MAX( 1,K1,K2 )
          K2 = MIN( K2,nzdim )
          if (me.eq.0) then
            WRITE(ISC,'(/,A)' ) 'Inactive Node Domain'
            WRITE(ISC,'(2X,2(A,I6))') 'I = ',I1,' to ',I2
            WRITE(ISC,'(2X,2(A,I6))') 'J = ',J1,' to ',J2
            WRITE(ISC,'(2(2X,A,I6))') 'K = ',K1,' to ',K2
          endif
          LO(1) = I1
          LO(2) = J1
          LO(3) = K1
          HI(1) = I2
          HI(2) = J2
          HI(3) = K2
          CALL NGA_FILL_PATCH(G_BUF,LO,HI,0)
  400   CONTINUE
        LO(1) = IAXMIN
        LO(2) = IAYMIN
        LO(3) = IAZMIN
        HI(1) = IAXMAX
        HI(2) = IAYMAX
        HI(3) = IAZMAX
        LXP = 1
        T_OK = COPYNDFLD( G_BUF,IXP,LDXX,LO,HI,LXP  )
      ENDIF
  900 CONTINUE
!
!---  Set values of IXP from GA_BUF, if necessary  ---
!
      CALL GA_IGOP(1,NXP,1,'+')
      LAN = NXDIM*NYDIM*NZDIM-nXP
      IF (ME.EQ.0) THEN
        WRITE(ISC,'(/,A)' ) 'Node Count'
        WRITE(ISC,'(2X,A,I10)') 'Number of Nodes: ',NXDIM*NYDIM*NZDIM
        WRITE(ISC,'(2X,A,I10)') 'Number of Active Nodes: ',NXDIM*NYDIM*NZDIM-NXP
        WRITE(ISC,'(2X,A,I10)') 'Number of Inactive Nodes: ',NXP
      ENDIF
!
!---  Search for additional definitions  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINAC group  ---
!
      RETURN
      END
