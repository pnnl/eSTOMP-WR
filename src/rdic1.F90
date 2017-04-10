!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDIC1
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
!     Read input file for initial conditions information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 2, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE REACT
      USE HYST
      USE GRID
      USE FILES
      USE FDVP
      USE CONST
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
!

!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,FDUM,FMDUM,UNTS
      CHARACTER*24 CHLB(3)
      CHARACTER*512 CHDUM
      INTEGER IDOM(6)
      REAL*8 VAR(5)
      REAL*8, DIMENSION(:), ALLOCATABLE :: C_TMP,VAR_TMP
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ICT_tmp
      INTEGER LDXX(3),LO(3),HI(3),LSTRIDE(3)
      LOGICAL FCHK,T_OK
      LOGICAL STATUS, USE_GA
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE CHLB
      DATA CHLB /'X-Direction Gradient, ','Y-Direction Gradient, ', &
                'Z-Direction Gradient, '/
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
 4009 SUBNMX = '/RDIC1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      IAPE = 0
      IASE = 0
      ALLOCATE(VAR_TMP(NUM_NODES))
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LSTRIDE(1) = IXMAX-IXMIN+1
      LSTRIDE(2) = IYMAX-IYMIN+1
      LSTRIDE(3) = IZMAX-IZMIN+1

!
!---  Write card information to ouput file  ---
!
      CARD = 'Initial Conditions Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 ) WRITE(ISC,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Restart file will be read for initial conditions  ---
!
      IF( IEO.EQ.2 .OR. IEO.EQ.4 ) THEN
        INDX = 2
        CALL RDRST(INDX)
        ISIC = 3
      ENDIF
!
!---  Read saturation initial condition option  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Initial Saturation Option: '
      CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
      CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
      IF( IEO.EQ.2 .OR. IEO.EQ.4 ) GOTO 10
      if (me.eq.0) then
        WRITE(ISC,'(/,A)') VARB(1:IVR)
        WRITE(ISC,'(2X,A)') ADUM
        WRITE(ISC,'(2X,A)') BDUM
      endif
!
!---  Reject cases when both aqueous saturation and moisture
!     content are specified  ---
!
      IF( INDEX(ADUM(1:),'aqueous saturation').NE.0 .AND. &
       INDEX(ADUM(1:),'moisture content').NE.0 ) THEN
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: ' &
           //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
      ENDIF
      IF( INDEX(BDUM(1:),'aqueous saturation').NE.0 .AND. &
       INDEX(BDUM(1:),'moisture content').NE.0 ) THEN
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: ' &
           //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
      ENDIF
      IF( INDEX(ADUM(1:),'aqueous saturation').NE.0 .OR. &
       INDEX(ADUM(1:),'moisture content').NE.0 ) THEN
        IASE = 1
        IF( INDEX(BDUM(1:),'gas pressure').NE.0 ) THEN
          ISIC = 1
          IF( INDEX(BDUM(1:),'moisture content').NE.0 ) ISIC = 11
        ELSEIF( INDEX(BDUM(1:),'aqueous pressure').NE.0 ) THEN
          ISIC = 2
          IAPE = 1
          IF( INDEX(BDUM(1:),'moisture content').NE.0 ) ISIC = 12
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: ' &
           //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'gas pressure').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'aqueous saturation').NE.0 .OR. &
       INDEX(BDUM(1:),'moisture content').NE.0 ) THEN
          ISIC = 1
          IASE = 1
          IF( INDEX(BDUM(1:),'moisture content').NE.0 ) ISIC = 11
        ELSEIF( INDEX(BDUM(1:),'aqueous pressure').NE.0 ) THEN
          ISIC = 3
          IAPE = 1
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: ' &
           //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'aqueous pressure').NE.0 ) THEN
        IAPE = 1
        IF( INDEX(BDUM(1:),'aqueous saturation').NE.0 .OR. &
       INDEX(BDUM(1:),'moisture content').NE.0 ) THEN
          ISIC = 2
          IASE = 1
          IF( INDEX(BDUM(1:),'moisture content').NE.0 ) ISIC = 12
        ELSEIF( INDEX(BDUM(1:),'gas pressure').NE.0 ) THEN
          ISIC = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: ' &
           //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Initial Saturation Option: ' &
         //ADUM(1:NCHA)//','//BDUM(1:NCHB)
        CALL WRMSGS( INDX )
      ENDIF
   10 CONTINUE
!
!---  Read initial conditions  ---
!
      IF(ME.EQ.0)WRITE(ISC,'(/,A)') &
        'Initial Condition Variable(s) and Domain(s)'
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Initial Condition Cards: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      DO 1000 NL = 1, NLIN
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Initial Condition Variable: '
        CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
        IF( IEO.NE.2 .AND. IEO.NE.4 ) THEN
          IF( INDEX( ADUM(1:),'aqueous pres' ).NE.0 ) THEN
            IF( ISIC.EQ.2 .OR. ISIC.EQ.3 .OR. &
             ISIC.EQ.12 ) IAPE = 0
          ELSEIF( INDEX( ADUM(1:),'aqueous sat' ).NE.0 .OR. &
           INDEX(ADUM(1:),'moisture content').NE.0 ) THEN
            IF( ISIC.EQ.1 .OR. ISIC.EQ.2 .OR. &
             ISIC.EQ.11 .OR. ISIC.EQ.12 ) IASE = 0
          ENDIF
        ENDIF
        IF( INDEX( ADUM(1:),'overwrite').EQ.0 .AND. &
         ( IEO.EQ.2 .OR. IEO.EQ.4 ) ) GOTO 1000
        IF( INDEX( ADUM(1:),'aqueous pres' ).NE.0 ) THEN
          VARB = 'Initial Aqueous Pressure'
          IUNM = -1
          IUNKG = 1
          IUNS = -2
        ELSEIF( INDEX( ADUM(1:),'gas pres' ).NE.0 ) THEN
          VARB = 'Initial Gas Pressure'
          IUNM = -1
          IUNKG = 1
          IUNS = -2
        ELSEIF( INDEX( ADUM(1:),'matrix pres' ).NE.0 ) THEN
          VARB = 'Initial Matrix Pressure'
          IUNM = -1
          IUNKG = 1
          IUNS = -2
        ELSEIF( INDEX( ADUM(1:),'imbibition' ).NE.0 .OR. &
         INDEX( ADUM(1:),'wetting' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'primary' ).NE.0 ) THEN
            VARB = 'Initial Primary Imbibition Path'
          ELSE
            VARB = 'Initial Imbibition Path'
          ENDIF
        ELSEIF( INDEX( ADUM(1:),'drainage' ).NE.0 .OR. &
         INDEX( ADUM(1:),'drying' ).NE.0 ) THEN
          VARB = 'Initial Drainage Path'
        ELSEIF( INDEX( ADUM(1:),'temperature' ).NE.0 ) THEN
          VARB = 'Initial Temperature'
          IUNK = 1
        ELSEIF( INDEX( ADUM(1:),'aqueous sat' ).NE.0 ) THEN
          VARB = 'Initial Aqueous Saturation'
        ELSEIF( INDEX( ADUM(1:),'moisture cont' ).NE.0 ) THEN
          VARB = 'Initial Moisture Content'
        ELSEIF( INDEX( ADUM(1:),'relative' ).NE.0 .AND. &
         INDEX( ADUM(1:),'trapped gas' ).NE.0 ) THEN
          VARB = 'Initial Relative Trapped Gas Saturation'
        ELSEIF( INDEX( ADUM(1:),'trapped gas' ).NE.0 ) THEN
          VARB = 'Initial Trapped Gas Saturation'
        ELSEIF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          VARB = 'Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
          VARB = 'Initial Solute Concentration'
          IUNM = -3

        ELSEIF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          VARB = 'Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
          VARB = 'Initial Species Concentration'
!
!---      Set species units  ---
!
          IUNMOL = 1
          IF( INDEX(ADUM(1:),'aqueous').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'molal').NE.0 ) THEN
              IVAR = 3
              IUNKG = -1
            ELSE
              IVAR = 2
              IUNM = -3
            ENDIF
          ELSEIF( INDEX(ADUM(1:),'sediment').NE.0 ) THEN
            IVAR = 5
            IUNM = 0
            IUNKG = -1
          ELSE
            IVAR = 1
            IUNM = -3
          ENDIF
          IF( IEO.EQ.2 .OR. IEO.EQ.4 ) IVAR = IVAR+10

        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Condition Variable: '// &
           ADUM(1:NCHA)
          CALL WRMSGS( INDX )
        ENDIF
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(1))
        CALL RDCHR(ISTART,ICOMMA,NCHU,CHDUM,UNTS)
!
!---  Read initial conditions input from an external file  ---
!
        IF( INDEX( ADUM(1:),'file' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'binary' ).NE.0 ) THEN
            IF(ME.EQ.0)WRITE(ISC,'(2X,3A)') ADUM(1:NCHA),',', &
              UNTS(1:NCHU)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            NCH = INDEX(FDUM,'  ')-1
!
!---        Check for external file  ---
!
            INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
            IF( .NOT.FCHK ) THEN
              INDX = 4
              CHMSG = 'Missing Initial Conditions File: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ELSEIF( FDUM.EQ.'formatted' ) THEN
              INDX = 4
              CHMSG = 'Initial Conditions File Format: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ENDIF
!            OPEN(UNIT=26,FILE=FDUM(1:NCH),STATUS='OLD', &
!             FORM='UNFORMATTED')
            IF(ME.EQ.0)WRITE(ISC,'(/,2A)') &
              'Initial Conditions File: ',FDUM(1:NCH)
          ELSEIF( INDEX( ADUM(1:),'ascii' ).NE.0 ) THEN
            IF(ME.EQ.0)WRITE(ISC,'(2X,3A)') ADUM(1:NCHA),',', &
              UNTS(1:NCHU)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            NCH = INDEX(FDUM,'  ')-1
!
!---        Check for external file  ---
!
            INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
            IF( .NOT.FCHK ) THEN
              INDX = 4
              CHMSG = 'Missing Initial Conditions File: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ELSEIF( FDUM.EQ.'unformatted' ) THEN
              INDX = 4
              CHMSG = 'Initial Conditions File Format: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ENDIF
            OPEN(UNIT=26,FILE=FDUM(1:NCH),STATUS='OLD', &
             FORM='FORMATTED')
             IF(ME.EQ.0)WRITE(ISC,'(/,2A)') &
               'Initial Conditions File: ',FDUM(1:NCH)
          ELSE
            IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4)') ADUM(1:NCHA), &
             ' (Default Value), ',UNTS(1:NCHU),': ',VAR(1)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            NCH = INDEX(FDUM,'  ')-1
!
!---        Check for external file  ---
!
            INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
            IF( .NOT.FCHK ) THEN
              INDX = 4
              CHMSG = 'Missing Initial Conditions File: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ELSEIF( FDUM.EQ.'unformatted' ) THEN
              INDX = 4
              CHMSG = 'Initial Conditions File Format: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ENDIF
            OPEN(UNIT=26,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED')
            IF(ME.EQ.0)WRITE(ISC,'(/,2A)') &
               'Initial Conditions File: ',FDUM(1:NCH)
            INDX = 0
            CALL RDUNIT( UNTS,VAR(1),INDX )
          ENDIF
!
!---  Read initial conditions according to rock/soil zonations  ---
!
        ELSEIF( INDEX( ADUM(1:),'rock' ).NE.0 .OR. &
         INDEX( ADUM(1:),'zonation' ).NE.0 ) THEN
          VARB = 'Rock/Soil Name'
          CALL RDCHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
!
!---  Search known rock types for a matching type ---
!
          DO 20 M = 1, NROCK
            IF( FDUM .EQ. ROCK(M)) THEN
            IROCK = M
            GOTO 30
          ENDIF
   20     CONTINUE
          INDX = 2
          CHMSG = 'Unrecognized Rock/Soil Type: '//FDUM
          CALL WRMSGS( INDX )
          GOTO 1000
   30     CONTINUE
          IF(ME.EQ.0)WRITE(ISC,'(2X,3A,1PE11.4,2A)') &
            ADUM(1:NCHA),UNTS(1:NCHU), ': ',VAR(1),&
            ' Rock/Soil Type: ',FDUM(1:NCHF)
          INDX = 0
          CALL RDUNIT( UNTS,VAR(1),INDX )
!
!---  Read initial condtions input from the input file  ---
!
        ELSE
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4)') ADUM(1:NCHA),', ', &
            UNTS(1:NCHU),': ',VAR(1)
          INDX = 0
          CALL RDUNIT( UNTS,VAR(1),INDX )
          INDX = 2
          VAR(5) = 1.D+0
          NCH = INDEX( UNTS,'  ' ) - 1
          IF( UNTS(1:NCH).EQ.'f' .OR. UNTS(1:NCH).EQ.'r' ) THEN
            VAR(5) = VAR(5)/1.8D+0
          ELSE
            CALL RDUNIT( UNTS,VAR(5),INDX )
          ENDIF
          VARB = 'Initial Condition Variable Gradient: '
          DO 100 I = 2,4
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(I))
            VAR(I) = VAR(I)*VAR(5)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') CHLB(I-1), &
              ', ',UNTS(1:NCH), ': ',VAR(I)
            INDX = 0
            IUNM = -1
            CALL RDUNIT( UNTS,VAR(I),INDX )
            IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',VAR(I),', 1/m)'
  100     CONTINUE
!
!---  Read domain indices  ---
!
          VARB = 'Initial Condition Domain Index: '
          DO 200 I = 1, 6
            CALL RDINT(ISTART,ICOMMA,CHDUM,IDOM(I))
  200     CONTINUE
        ENDIF
!
!---  Read variables  ---
!
        USE_GA = .TRUE.
        IF( INDEX(ADUM(1:),'aqueous pres').NE.0 ) THEN
!
!---  Add aqueous pressure field to grid ---
!
          ADDER = -PATM
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( PL,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( PL,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( PL,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( PL,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( PL,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'gas pres').NE.0 ) THEN
!
!---  Add gas pressure field to grid ---
!
          ADDER = -PATM
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( PG,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( PG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( PG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( PG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( PG,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
!        ELSEIF( INDEX(ADUM(1:),'matrix pres').NE.0 ) THEN
!          ADDER = -PATM
!          INDX = 2
!          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
!            IUNM = -1
!            IUNKG = 1
!            IUNS = -2
!            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
!              CALL RDINBS( PN,ADDER,UNTS,INDX )
!            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
!              CALL RDINAS( PN,ADDER,UNTS,INDX )
!            ELSE
!              CALL RDINFS( PN,VAR,ADDER,UNTS,INDX )
!            ENDIF
!            CLOSE(UNIT=26)
!          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
!           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
!            CALL RDINZS( PN,VAR(1),ADDER,IROCK,INDX )
!          ELSE
!            CALL RDINIS( PN,VAR,ADDER,IDOM,INDX )
!          ENDIF
        ELSEIF( INDEX(ADUM(1:),'aqueous sat').NE.0 ) THEN
!
!---  Add aqueous saturation field to grid ---
!
          ADDER = 0.D+0
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SL,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SL,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SL,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SL,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SL,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
!
!---    Use the variable RHOG to temporarily hold the initial
!       moisture content  ---
!
        ELSEIF( INDEX(ADUM(1:),'moisture cont').NE.0 ) THEN
!
!---  Add moisture content field to grid ---
!
          ADDER = 0.D+0
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( RHOG,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( RHOG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( RHOG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( RHOG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( RHOG,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'imbibition').NE.0 .OR. &
         INDEX(ADUM(1:),'wetting').NE.0 ) THEN
!
!---  Add gas saturation field to grid ---
!---  Add imbibition/wetting field to grid ---
!
          ADDER = 0.D+0
          IF( INDEX(ADUM(1:),'primary').NE.0 .OR. &
           INDEX(ADUM(1:),'main').NE.0 ) THEN
            VAR(1) = 2.D+0
          ELSE
            VAR(1) = 1.D+0
          ENDIF
          VAR(2) = 0.D+0
          VAR(3) = 0.D+0
          VAR(4) = 0.D+0
          INDX = 1
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SG,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SG,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
          DO 202 N = 1,NUM_NODES
            IF( IXP(N).LE.0 ) GOTO 202
            IPH(2,N) = INT( SG(INDX,N) )
  202     CONTINUE
        ELSEIF( INDEX(ADUM(1:),'drainage').NE.0 .OR. &
         INDEX(ADUM(1:),'drying').NE.0 ) THEN
!
!---  Add gas saturation field to grid ---
!---  Add drainage/drying field to grid ---
!
          ADDER = 0.D+0
          VAR(1) = -1.D+0
          VAR(2) = 0.D+0
          VAR(3) = 0.D+0
          VAR(4) = 0.D+0
          INDX = 1
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SG,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SG,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
          DO 204 N = 1,num_nodes
            IF( IXP(N).LE.0 ) GOTO 204
            IPH(2,N) = INT( SG(INDX,N) )
  204     CONTINUE
        ELSEIF( INDEX(ADUM(1:),'temperature').NE.0 ) THEN
!
!---  Add temperature field to grid ---
!
          ADDER = 0.D+0
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IUNK = 1
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( T,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( T,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( T,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( T,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( T,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'relative trapped gas').NE.0 .AND. &
         INDEX(ADUM(1:),'trapped gas').NE.0 ) THEN
!
!---  Add trapped gas saturation field to grid ---
!
          ADDER = 1.D+2
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SGT,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SGT,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SGT,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SGT,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SGT,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'trapped gas').NE.0 ) THEN
!
!---  Add trapped gas field to grid ---
!
          ADDER = 0.D+0
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SG,ADDER,UNTS,INDX,FDUM )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
           INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SG,VAR,ADDER,LO,HI,LDXX,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'solute').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'aqueous equ').NE.0 ) THEN
            IVAR = 3
          ELSEIF( INDEX(ADUM(1:),'aqueous').NE.0 ) THEN
            IVAR = 2
          ELSE
            IVAR = 1
          ENDIF
          IF( INDEX( UNTS(1:),'bd' ).NE.0 ) IVAR = -IVAR
!
!---  Add solute concentration field to grid ---
!
          IF( NSOLU+LSPT.NE.0 ) THEN
            ALLOCATE(C_TMP(NUM_NODES))
            ALLOCATE(ICT_TMP(NUM_NODES))
            C_TMP = 0.D0
            ICT_TMP = 0
          ENDIF
          DO 220 NSL = 1,NSOLU
            IDB = INDEX(SOLUT(NSL)(1:),'  ') - 1
            IF( BDUM(1:NCHB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              ADDER = 0.D+0
              C_TMP(:) = C(NSL,:)
              ICT_TMP(:) = ICT(NSL,:)
              IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
                IUNM = -3
                IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
                  CALL RDINBP( C_TMP,ADDER,ICT_TMP,IVAR,UNTS,FDUM )
                  C(NSL,:) = C_TMP
                  ICT(NSL,:) = ICT_TMP
                ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
                  CALL RDINAP( C_TMP,ADDER,ICT_TMP,IVAR,UNTS )
                  C(NSL,:) = C_TMP
                  ICT(NSL,:) = ICT_TMP
                ELSE
                  CALL RDINFP( C_TMP,VAR,ADDER,ICT_TMP,IVAR,UNTS )
                  C(NSL,:) = C_TMP
                  ICT(NSL,:) = ICT_TMP
                ENDIF
                CLOSE(UNIT=26)
              ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
               INDEX(ADUM(1:),'zonation').NE.0 )  THEN
                CALL RDINZP( C_TMP,VAR(1),ADDER,ICT_TMP, &
                 IVAR,IROCK )
                  C(NSL,:) = C_TMP
                  ICT(NSL,:) = ICT_TMP
              ELSE
                CALL RDINIP( C_TMP,VAR(1),ADDER,ICT_TMP,IVAR, &
                  LO,HI,LDXX,IDOM )
                  C(NSL,:) = C_TMP(:)
                  ICT(NSL,:) = ICT_TMP(:)
              ENDIF
              GOTO 230
            ENDIF
  220     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Solute Name: '//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
  230     CONTINUE
          IF(NSOLU+LSPT.NE.0) THEN
            DEALLOCATE(C_TMP)
            DEALLOCATE(ICT_TMP)
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'specie').NE.0 ) THEN
          ADDER = 0.D+0
          ALLOCATE(C_TMP(NUM_NODES))
          ALLOCATE(ICT_TMP(NUM_NODES))
          C_TMP = 0.D0
          ICT_TMP = 0
!
!---      Conservation- or kinetic-component species  ---
!
          IF( INDEX( BDUM(1:),'total_' ).NE.0 ) THEN
            DO 300 NSLX = NSOLU+1,NSOLU+NEQC+NEQK
              IDB = INDEX(SOLUT(NSLX)(1:),'  ') - 1
              IF( BDUM(1:NCHB).EQ.SOLUT(NSLX) ) THEN
                NSL = NSLX
                GOTO 340
              ENDIF
  300       CONTINUE
          ENDIF
!
!---      Aqueous reactive species  ---
!
          DO 310 NSPX = 1,NSPL
            IDB = INDEX(SPNML(NSPX)(1:),'  ') - 1
            IF( BDUM(1:NCHB).EQ.SPNML(NSPX)(1:IDB) ) THEN
              NSP = NSPX
              GOTO 340
            ENDIF
  310     CONTINUE
!
!---      Solid reactive species  ---
!
          DO 320 NSPX = 1,NSPS
            IDB = INDEX(SPNMS(NSPX)(1:),'  ') - 1
            IF( BDUM(1:NCHB).EQ.SPNMS(NSPX)(1:IDB) ) THEN
              NSP = NSPX + NSPL
!
!---          Verify that solid-species is not a mineral  ---
!
              IF( ISP_MN(NSP).EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Solid-Species Mineral ' //  &
                 '(see Lithology Card): ' // BDUM(1:NCHB)
                CALL WRMSGS( INDX )
              ENDIF
              GOTO 340
            ENDIF
  320     CONTINUE
!
!---      Exchange reactive species  ---
!
          DO 325 NSPX = 1,NSPE
            IDB = INDEX(SPNME(NSPX)(1:),'  ') - 1
            IF( BDUM(1:NCHB).EQ.SPNME(NSPX)(1:IDB) ) THEN
              NSP = NSPX + NSPL + NSPS
              GOTO 340
            ENDIF
  325     CONTINUE
!
!---      pH  ---
!
          IF( BDUM(1:NCHB).EQ.'ph' .AND. ISPLK(1).NE.0 ) THEN
            NSP = MOD(ISPLK(1),100)
            ISPLK(1) = ISPLK(1) + 100
            IVAR = 2
            IF( IEO.EQ.2 .OR. IEO.EQ.4 ) IVAR = IVAR+10
            ADDER = 7.D+0
!
!---        Verify that species linked to pH is a conservation
!           component species  ---
!
            DO 330 NEQ = 1,NEQC
              IF( NSP.EQ.IEQ_C(2,NEQ) ) GOTO 340
  330       CONTINUE
            INDX = 4
            CHMSG = 'pH Species not a Conservation ' // &
             'Component Species: ' // BDUM(1:NCHB)
            CALL WRMSGS( INDX )
          ENDIF
          INDX = 4
          CHMSG = 'Unrecognized Reactive Species: ' // BDUM(1:NCHB)
          CALL WRMSGS( INDX )
  340     CONTINUE
!
!---      Conservation- or kinetic-component species  ---
!
          IF( INDEX( BDUM(1:),'total_' ).NE.0 ) THEN
!           C_TMP(:) = C(NSL,:)
!           ICT_TMP(:) = ICT(NSL,:)
            IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
              IUNM = -3
              IUNMOL = 1
              IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
!                CALL RDINBP( C(1,NSL),ADDER,ICT(1,NSL),IVAR,UNTS )
                CALL RDINBP( c_tmp,ADDER,ict_tmp,IVAR,UNTS,FDUM )
                DO i = 1,NUM_NODES
                  IF( C_TMP(i).NE.0.D+0 )THEN
                    c(nsl,i) = c_tmp(i)
                    ict(nsl,i) = ict_tmp(i)
                  ENDIF
                ENDDO
              ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
!                CALL RDINAP( C(1,NSL),ADDER,ICT(1,NSL),IVAR,UNTS )
                CALL RDINAP( c_tmp,ADDER,ict_tmp,IVAR,UNTS )
                DO i = 1,NUM_NODES
                  IF( C_TMP(i).NE.0.D+0 )THEN
                    c(nsl,i) = c_tmp(i)
                    ict(nsl,i) = ict_tmp(i)
                  ENDIF
                ENDDO
              ELSE
                CALL RDINFP( c_tmp,VAR,ADDER,ict_tmp, &
                 IVAR,UNTS )
                DO i = 1,NUM_NODES
                  IF( C_TMP(i).NE.0.D+0 )THEN
                    c(nsl,i) = c_tmp(i)
                    ict(nsl,i) = ict_tmp(i)
                  ENDIF
                ENDDO
              ENDIF
              CLOSE(UNIT=26)
            ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
             INDEX(ADUM(1:),'zonation').NE.0 )  THEN
             CALL RDINZP( c_tmp,VAR(1),ADDER,ict_tmp, &
               IVAR,IROCK )
              DO i = 1,NUM_NODES
                IF( C_TMP(i).NE.0.D+0 )THEN
                  c(nsl,i) = c_tmp(i)
                  ict(nsl,I) = ict_tmp(i)
                ENDIF
              ENDDO
            ELSE
              CALL RDINIP( C_TMP,VAR,ADDER,ICT_TMP, &
               IVAR,LO,HI,LDXX,IDOM )
              DO I = 1,NUM_NODES
                IF( C_TMP(i).NE.0.D+0 )THEN
                  C(NSL,I) = C_TMP(I)
                  ICT(NSL,I) = ICT_TMP(I)
                ENDIF
              ENDDO
            ENDIF
          ELSE
            IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
!              C_TMP(:) = SP_C(NSP,:)
!              ICT_TMP(:) = IC_SP(NSP,:)
              IUNM = -3
              IUNMOL = 1
              IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
                CALL RDINBP( C_TMP,ADDER,ICT_TMP,IVAR,UNTS,FDUM )
              DO I = 1,NUM_NODES
                IF( C_TMP(i).NE.0.D+0 )THEN
                  SP_C(NSP,I) = C_TMP(I)
                  IC_SP(NSP,I) = ICT_TMP(I)
                ENDIF
              ENDDO
              ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
                CALL RDINAP( C_TMP,ADDER,ICT_TMP,IVAR,UNTS )
              DO I = 1,NUM_NODES
                IF( C_TMP(i).NE.0.D+0 )THEN
                  SP_C(NSP,I) = C_TMP(I)
                  IC_SP(NSP,I) = ICT_TMP(I)
                ENDIF
              ENDDO

              ELSE
                CALL RDINFP( c_tmp,VAR,ADDER,ict_tmp, &
                 IVAR,UNTS )
              DO I = 1,NUM_NODES
                IF( C_TMP(i).NE.0.D+0 )THEN
                  SP_C(NSP,I) = C_TMP(I)
                  IC_SP(NSP,I) = ICT_TMP(I)
                ENDIF
              ENDDO
              ENDIF
              CLOSE(UNIT=26)
            ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR. &
             INDEX(ADUM(1:),'zonation').NE.0 )  THEN
              CALL RDINZP( C_TMP,VAR(1),ADDER,ICT_TMP, &
               IVAR,IROCK )
              DO I = 1,NUM_NODES
                IF( C_TMP(I).NE.0.D+0 )THEN
                  SP_C(NSP,I) = C_TMP(I)
                  IC_SP(NSP,I) = ICT_TMP(I)
                ENDIF
              ENDDO
            ELSE
              CALL RDINIP( C_TMP,VAR,ADDER,ICT_TMP, &
               IVAR,LO,HI,LDXX,IDOM )
              DO I = 1,NUM_NODES
                IF( C_TMP(I).NE.0.D+0 )THEN
                  SP_C(NSP,I) = C_TMP(I)
                  IC_SP(NSP,I) = ICT_TMP(I)
                ENDIF
              ENDDO
            ENDIF
          ENDIF
          DEALLOCATE(C_TMP)
!print *,'initial sp---',sp_c(1:3,1)

          DEALLOCATE(ICT_TMP)    
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Condition Variable: '// &
           ADUM(1:NCHA)
          CALL WRMSGS( INDX )
        ENDIF
 1000 CONTINUE

      IF( IAPE.NE.0 ) THEN
        INDX = 4
        CHMSG = 'Undeclared Initial Aqueous Pressure'
        CALL WRMSGS( INDX )
      ELSEIF( IASE.NE.0 ) THEN
        INDX = 4
        CHMSG = 'Undeclared Initial Aqueous Saturation'
        CALL WRMSGS( INDX )
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDIC1 group  ---
!
      RETURN
      END
