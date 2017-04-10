!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDROCK( FDUM )
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
!     Read input file for rock/soil zonation information.
!     Overlapping rock/soil zone definitions are sequence dependent.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     Last Modified by MD White, PNNL, 5 October 2001.
!     $Id: rdrock.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
   USE SOLTN
   USE PORMED
   USE GRID
   USE GRID_MOD
   USE FILES
   USE GLB_PAR
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
!----------------------Type Declarations-------------------------------!
!
   CHARACTER*64 ADUM,BDUM,FDUM,CDUM
   CHARACTER*512 CHDUM
   CHARACTER*17 FORM2
   CHARACTER*4 FORM1
   LOGICAL FCHK
!
   INTEGER DIMS(3), LO(3), HI(3), LDXX(3), G_BUF, THREE, IO,IOSTAT
   INTEGER, ALLOCATABLE :: IDX_BUF(:,:)
   INTEGER, ALLOCATABLE :: VAL_BUF(:)
   REAL*8, ALLOCATABLE :: BUF1D(:)
   LOGICAL STATUS
   LOGICAL T_OK,ISBIN,ISHDF5
!
!----------------------Data Statements---------------------------------!
!
   SAVE FORM1,FORM2
   DATA FORM2 /'Soil 000000000'/
   DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
   ME = GA_NODEID()
   NPROC = GA_NNODES()
   SUBNMX = '/RDROCK'
   ICSNX = INDEX( SUBNMX,'  ' )-1
   SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
   IF( INDEX(CVS_ID(158)(1:1),'$').EQ.0 ) CVS_ID(158) = &
      '$Id: rdrock.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
   ICSN = ICSN+ICSNX
   ISC = 6
!
!---  total number of nodes  ---
!
   NFLD = NXDIM*NYDIM*NZDIM
!
!---  add rock zone field to grid  ---
!
   CALL ADD_NODE_IFIELD('rock_zone',IDX)
   IZ => I_ND_FLD(IDX)%P
!
!---  Write card information to ouput file  ---
!
   CARD = 'Rock/Soil Zonation Card'
   ICD = INDEX( CARD,'  ' )-1
   IF (ME.EQ.0) THEN
      WRITE (ISC,'(//,3A)') ' ~ ',CARD(1:ICD),': '
   ENDIF
!
!---  Read the number of rock/soil zonation information lines  ---
!
   ISTART = 1
   T_OK = BUFFEREDREAD_GETLINE(CHDUM)

   CALL LCASE( CHDUM )
   NROCK = 0
   CALL ADD_NODE_IFIELD('ISKP', IDX)
   ISKP => I_ND_FLD(IDX)%P
   ISKP = 0
   VARB = 'Input Option [File, Zonation_File, Integer, Indexing]'
   ISTARTX = ISTART
   CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
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
   ISBIN = .FALSE.
!
!---  Read rock/soil zonation information from an external file  ---
!
   FDUM = ''
   IF( INDEX(ADUM(1:),'binary').NE.0 .OR. &
      INDEX(ADUM(1:),'bfile').NE.0 .OR. &
      INDEX(ADUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
   IF( INDEX(ADUM,'file').NE.0 ) THEN
     VARB = 'Rock/soil Zonation external file name'
     CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
     IF( INDEX(ADUM,'file').NE.0 ) THEN
         FDUM = BDUM
         NCHF = NCHB
      ENDIF
      IF( INDEX(BDUM,'file').NE.0 ) THEN
         FDUM = ADUM
         NCHF = NCHA
      ENDIF
   ENDIF
!
!--- Read rock/soil zonation info ---
!
   IF( INDEX(ADUM,'zonation').NE.0.AND.INDEX(ADUM,'file').NE.0.OR. &
     INDEX(ADUM,'hdffile').NE.0 ) THEN
      IF( INDEX(ADUM,'hdffile').NE.0 ) ISHDF5 = .TRUE.
      ALLOCATE( BUF1D(LDXX(1)*LDXX(2)*LDXX(3)),STAT=ISTAT )
      T_OK = CHKSTAT( 'BUF1D',ISTAT,INDX )
      T_OK = RDIJK1D( FDUM,LDXX,LO,HI,BUF1D,VARX,ISBIN,ISHDF5 )
      IZ = INT(BUF1D)
      NROCK = MAXVAL(IZ)
      CALL GA_IGOP(1,NROCK,1,"max")
      LRC = NROCK
      IF(.NOT.ALLOCATED(ROCK)) ALLOCATE( ROCK(NROCK),STAT=ISTAT )
      CALL CHKROCK( ISTART,ICOMMA )
!
!---  Read rock/soil zonation information from an external file  ---
!
   ELSE IF( INDEX(ADUM,'file').NE.0 ) THEN
      T_OK = CREATE_INTGA( G_BUF,NXDIM,NYDIM,NZDIM )
      IF( ME.EQ.0 )T_OK = OPENFILE( FDUM,IUNIT,ISBIN )
      VARB = 'Number of Zonation Lines'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      NROCK = NLIN
      IF(.NOT.ALLOCATED(ROCK)) ALLOCATE(ROCK(NROCK))
      CALL CHKROCK( ISTART,ICOMMA )
      LRC = NROCK
!
!---  Read rock/soil zonation indices  ---
!
      IF (ME.EQ.0) THEN
        IJDIM = NXDIM*NYDIM
        ALLOCATE(IDX_BUF(3,IJDIM))
        ALLOCATE(VAL_BUF(IJDIM))
        NCOUNT = 0
        IO = 0
        DO WHILE( IO /= -1 )
          IF( IO == 0 )READ(IUNIT,*,IOSTAT = IO) I,J,K,IROCK
          NCOUNT = NCOUNT + 1
          IDX_BUF(1,NCOUNT) = I 
          IDX_BUF(2,NCOUNT) = J 
          IDX_BUF(3,NCOUNT) = K
          VAL_BUF(NCOUNT) = IROCK
          IF( I.LT.1 .OR. I.GT.nxdim .OR. J.LT.1 .OR. J.GT.nydim &
            .OR. K.LT.1 .OR. K.GT.nzdim ) THEN
            INDX = 4
            CHMSG = 'Rock/Soil Zonation Index Out of Range'
            CALL WRMSGS(INDX)
          ENDIF
          IF( IROCK.LT.1 .OR. IROCK.GT.NROCK ) THEN
            INDX = 7
            CHMSG = 'Rock/Soil Number Out of Range'
            IMSG = IROCK
            CALL WRMSGS(INDX)
          ENDIF
          IF (MOD(NCOUNT,IJDIM).EQ.0) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
          ENDIF
        END DO
        IF (MOD(NCOUNT,IJDIM).GT.0) THEN
          CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
        ENDIF
        DEALLOCATE(IDX_BUF)
        DEALLOCATE(VAL_BUF)
      ENDIF
      LXP = -1
      T_OK = COPYNDFLD( G_BUF,IZ,LDXX,LO,HI,NXP  )
      if(me.eq.0) CLOSE(UNIT=IUNIT)
      STATUS = GA_DESTROY(G_BUF)
!
!---  Assign rock/soil zonation information by indexing order,
!     useful for stochastic realizations  ---
!
      ELSEIF( INDEX(ADUM,'indexing').NE.0 ) THEN
        NCHR = 5 + ICOUNT(NFLD)
        IROCK = 0
        IJLOC = LDXX(1)*LDXX(2)
        ILOC = LDXX(1)
!
!---    IJK indexing ---  
!---    JKI and KIJ Indexing not supported   ---
!
        IF( INDEX(ADUM,'ijk').NE.0 ) THEN
          NROCK = 1
          IF(.NOT.ALLOCATED(ROCK)) ALLOCATE(ROCK(NROCK))
          ROCK(1) = 'ijk indexing'
          IJDIM = NXDIM*NYDIM
          DO K = LO(3), HI(3)
            KK = K - LO(3)
            DO J = LO(2), HI(2)
              JJ = J - LO(2)
              DO I = LO(1), HI(1)
                II = I-LO(1)
                IZ(KK*IJLOC+JJ*ILOC+II+1) = (K-1)*IJDIM+(J-1)*NXDIM+I
              END DO
            END DO
          END DO
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // ADUM(1:NCHA)
          CALL WRMSGS(INDX)
        ENDIF
      ELSE
!
!---  Read rock/soil zonation information from the input file  ---
!
        T_OK = CREATE_INTGA( G_BUF,NXDIM,NYDIM,NZDIM )
        ISTART = 1
        VARB = 'Number of Zonation Lines'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
        ALLOCATE(ROCK(100))
        DO NL = 1, NLIN
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          ADUM(1:) = ' '
          VARB = 'Rock/Soil Name: '
          CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
          DO M = 1,NROCK
            IF( ROCK(M).EQ.ADUM ) THEN
              IROCK = M
              GOTO 140
             ENDIF
          ENDDO
          NROCK = NROCK+1
          LRC = NROCK
          ROCK(NROCK) = ADUM
          IROCK = NROCK
  140     CONTINUE
!
!---  Read rock/soil domain  ---
!
          VARB = 'Rock/Soil Domain Index: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,I1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,I2)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J2)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K2)
          I1 = MAX( 1,I1 )
          I1 = MIN( NXDIM,I1,I2 )
          I2 = MAX( 1,I1,I2 )
          I2 = MIN( NXDIM,I2 )
          J1 = MAX( 1,J1 )
          J1 = MIN( NYDIM,J1,J2 )
          J2 = MAX( 1,J1,J2 )
          J2 = MIN( NYDIM,J2 )
          K1 = MAX( 1,K1 )
          K1 = MIN( NZDIM,K1,K2 )
          K2 = MAX( 1,K1,K2 )
          K2 = MIN( NZDIM,K2 )
          IF (ME.EQ.0) THEN
            WRITE(ISC,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
            WRITE(ISC,'(2X,A)') 'Rock/Soil Domain:'
            WRITE(ISC,'(4X,A,I6,A,I6)') 'I = ',I1,' to ',I2
            WRITE(ISC,'(4X,A,I6,A,I6)') 'J = ',J1,' to ',J2
            WRITE(ISC,'(4X,A,I6,A,I6)') 'K = ',K1,' to ',K2
          ENDIF
!
!---  Label nodes with rock/soil types  ---
!
          LO(1) = I1
          LO(2) = J1
          LO(3) = K1
          HI(1) = I2
          HI(2) = J2
          HI(3) = K2
          CALL NGA_FILL_PATCH(G_BUF,LO,HI,IROCK)
          IF( INDEX(CHDUM(1:),'skip conv').NE.0 ) ISKP(IROCK) = 1
          IF (ME.EQ.0) THEN
            IF( NL .LT. NLIN ) WRITE(ISC,'(/)')
          ENDIF
        ENDDO
       LO(1) = IAXMIN
       LO(2) = IAYMIN
       LO(3) = IAZMIN
       HI(1) = IAXMAX
       HI(2) = IAYMAX
       HI(3) = IAZMAX
       LXP = -1
       T_OK = COPYNDFLD( G_BUF,IZ,LDXX,LO,HI,LXP  )
       STATUS = GA_DESTROY(G_BUF)
      ENDIF

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDROCK group  ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHKROCK( ISTART,ICOMMA )
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
!     Read input file for rock/soil zonation information.
!     Overlapping rock/soil zone definitions are sequence dependent.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     Last Modified by MD White, PNNL, 5 October 2001.
!     $Id: rdrock.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
   USE GRID
   USE SOLTN
   USE PORMED
   USE BUFFEREDREAD
   USE FILES
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
   CHARACTER*512 CHDUM
!
!----------------------Data Statements---------------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/CHKROCK'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(158)(1:1),'$').EQ.0 ) CVS_ID(158) = &
         '$Id: chkrock.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Check for duplicate rock/soil name
!
      DO NL = 1,NROCK
         T_OK = BUFFEREDREAD_GETLINE(CHDUM)
         ISTART = 1
         CALL LCASE( CHDUM )
         VARB = 'Rock/Soil Name: '
         CALL RDCHR(ISTART,ICOMMA,NCHR,CHDUM,ROCK(NL))
         DO M = 1,NL-1
            IF( ROCK(M).EQ.ROCK(NL) ) THEN
               INDX = 4
               CHMSG = 'Duplicate Rock/Soil Name: ' // ROCK(NL)(1:NCHR)
               CALL WRMSGS( INDX )
            ENDIF
         ENDDO
         IF( INDEX(CHDUM(1:),'skip conv').NE.0 ) THEN
            DO NX=1,NUM_NODES
               IF(IZ(NX).EQ.NL) THEN
                  ISKP(NX) = 1
               ENDIF
            ENDDO
         ENDIF
         IF (ME.EQ.0) THEN
            WRITE(ISC,'(3A,I6)') 'Rock/Soil Name: ',ROCK(NL), &
               'Zonation Number: ',NL
         ENDIF
      ENDDO
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDROCK group  ---
!
      RETURN
      END
