!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDLRP
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
!     Read input file for rock/soil aqueous relative permeability
!     function information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, Battelle, PNL, December 1992.
!     Last Modified by MD White, Battelle, PNL, June 23, 1994.
!     $Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 T_FILENAME,ADUM,RDUM,UNTS
      CHARACTER*512 CHDUM
      INTEGER :: DIM1,DIM2
      INTEGER :: IRPLTX(3),RPLCX(LRPLC),RPLTX(4,LRPL)
      INTEGER, DIMENSION(:), ALLOCATABLE :: FLG_RD
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRPL_TMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IRLTBLX
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RPLC_TMP
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDLRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) =  &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  WRITE card information to ouput file  ---
!
      CARD = 'Rock/Soil Aqueous Relative Permeability Function Card'
      ICD = INDEX( CARD,'  ' )-1
      IF(ME.EQ.0)WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Setup pointer arrays
!
      DIM1 = 3
      CALL ADD_NODE_IFIELD('irpl', IDX)
      IRPL => I_ND_FLD(IDX)%P
      IRPL = 0
      DIM1 = LRPLC
      CALL ADD_NODE_D2FIELD('rplc', DIM1, IDX)
      RPLC => D_ND_2FLD(IDX)%P
      RPLC = 0.d0
      DIM1 = 3
      CALL ADD_NODE_I2FIELD('irplt', DIM1, IDX)
      IRPLT => I_ND_2FLD(IDX)%P
      IRPLT = 0
      DIM1 = 4
      DIM2 = LRPL
      CALL ADD_NODE_D3FIELD('rplt', DIM1, DIM2,IDX)
      RPLT => D_ND_3FLD(IDX)%P
      RPLT = 0.d0
!
!---  Loop over the rock/soil aqueous relative permeability
!     information lines  ---
!
      NN = 0
      IJK = 0
      ISALLOC = 0
   10 CONTINUE
      IF( NN.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  Check for a pair of delimiting slashes in the rock/soil name,
!     indicating a pattern of rock/soil types  ---
!
      KBS = 0
      IBS = INDEX( RDUM(1:),'/' )
      IF( IBS.GT.0 ) THEN
        IBS = IBS + 1
        JBS = INDEX( RDUM(IBS:),'/')
        IF( JBS.GT.0 ) THEN
          JBS = IBS + JBS - 2
          KBS = 1
          ISBS = ISTART
        ENDIF
      ENDIF
      IROCK = 1
   20 CONTINUE
!
!---  IJK indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IJK = 1
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = IROCK,NROCK
        IF( KBS.EQ.1 ) THEN
          IF( INDEX( ROCK(M)(1:),RDUM(IBS:JBS) ).GT.0 ) THEN
            IROCK = M
            GOTO 200
          ENDIF
        ELSE
          IF( RDUM.EQ.ROCK(M) ) THEN
            IROCK = M
            GOTO 200
          ENDIF
        ENDIF
  100 CONTINUE
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
      CALL WRMSGS( INDX )
      GOTO 10
  200 CONTINUE
!
!---  WRITE rock/soil name  ---
!
      IF(ME.EQ.0)WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
!
!---  Read aqueous relative permeability pressure function  ---
!
      NN = NN + 1
  220 CONTINUE
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Nonhysteretic saturation functions  ---
!
      IERR = 0
      IF(IJK > 0) THEN
        IF(.NOT.ALLOCATED(FLG_RD))ALLOCATE(FLG_RD(1))
      ELSE
        IF(.NOT.ALLOCATED(FLG_RD))ALLOCATE(FLG_RD(NROCK))
      ENDIF
      FLG_RD = 0
      NUM_NODESX = NUM_NODES
      IF( IJK > 0 ) NUM_NODESX = 1
      DO N = 1,NUM_NODESX
        IF( IJK <= 0 .and. IZ(N) <= 0) CYCLE
        IF( (IJK <= 0.AND.IZ(N) == IROCK) .OR. IJK > 0 ) THEN
         IF( IJK <= 0 ) THEN
           IF(FLG_RD(IZ(N)) > 0) CYCLE
         ENDIF
         IF( IJK <= 0 ) FLG_RD(IZ(N)) = 1
        
      IF( ISCHR(N).LT.20 .OR. ISCHR(N).GT.30 ) THEN
        VARB = 'Aqueous Relative Permeability Function'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Tabular (relative permeability versus liquid saturation)  ---
!
        IF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
          IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
            IF(ME.EQ.0)WRITE(IWR,'(A)') &
              'Tabular Aqueous Relative Permeability ' &
              // 'Versus Water Content Function'
            IRPLX = 10
          ELSEIF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
            IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
              IF(ME.EQ.0)WRITE(IWR,'(A)') &
               'Tabular Aqueous Relative Permeability ' &
               // 'Versus Log Capillary Head Function'
              IRPLX = 14
            ELSE
              IF(ME.EQ.0)WRITE(IWR,'(A)') &
                'Tabular Aqueous Relative Permeability ' &
                // 'Versus Capillary Head Function'
              IRPLX = 12
            ENDIF
          ELSE
            IF(ME.EQ.0)WRITE(IWR,'(A)') &
              'Tabular Aqueous Relative Permeability ' &
              // 'Versus Saturation Function'
            IRPLX = 10
          ENDIF
          IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
            IRPLX = IRPLX + 1
            IF(ME.EQ.0)WRITE(IWR,'(A)') 'Cubic Spline Interpolation'
          ELSE
            IF(ME.EQ.0)WRITE(IWR,'(A)') 'Linear Interpolation'
          ENDIF
          IF( .NOT.ALLOCATED(IRLTBLX) )ALLOCATE(IRLTBLX(2,NROCK))
          DO I = 1,MAX_IND_FLD 
            IF( 'irltbl'.eq.i_nd_2fld_names(i) )THEN
              ISALLOC = 1
            ENDIF
          END DO
          IF( ISALLOC.EQ.0 )THEN
             DIM1 = 2
             CALL ADD_NODE_I2FIELD('irltbl', DIM1, IDX)
             IRLTBL => I_ND_2FLD(IDX)%P
             IRLTBL = 0.d0
             ISALLOC = 1
          ENDIF
          VARB = 'Number of Tabular Entries'
          CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A,I6)') VARB,': ',NLIN
          IF( NLIN.LT.2 ) THEN
            INDX = 4
            CHMSG = 'Invalid Table'
            CALL WRMSGS( INDX )
          ENDIF
!         LTBL = NLIN
          IF( IJK > 0 )LTBL=LTBL*NUM_NODES
          IF(.NOT.ALLOCATED(TBLX)) ALLOCATE(TBLX(LTBL), STAT=ISTAT)
          T_OK = CHKSTAT( 'TBLX',ISTAT,INDX )
          IF(.NOT.ALLOCATED(TBLY)) ALLOCATE(TBLY(LTBL), STAT=ISTAT)
          T_OK = CHKSTAT( 'TBLY',ISTAT,INDX )
          IF( IJK.GT.0 ) THEN
            IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
              VARB = 'Water Content'
              UNTS = 'null'
            ELSEIF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
              IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
                VARB = 'Log Capillary Head'
              ELSE
                VARB = 'Capillary Head'
              ENDIF
              UNTS = 'm'
              IUNM = 1
            ELSE
              VARB = 'Saturation'
              UNTS = 'null'
            ENDIF
            NTBLX = NTBL
            ILOG = 0
            IF( IRPLX.GE.14 .AND. IRPLX.LE.15 ) ILOG = 1
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = 1.0D0
            CALL RDIJKT( T_FILENAME,TBLX,VARX,IRLTBL,NLIN, &
              NTBLX,ILOG,ISBIN,ISHDF5 )
            IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
              DO 280 NX = 1,NUM_NODES
                DO 280 M = IRLTBL(1,NX),IRLTBL(2,NX)
                  TBLX(M) = TBLX(M)/POR(2,NX)
  280         CONTINUE
            ENDIF
            VARB = 'Aqueous Relative Permeability'
            UNTS = 'null'
            ILOG = 0
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = 1.0D0
            CALL RDIJKT( T_FILENAME,TBLY,VARX,IRLTBL,NLIN, &
              NTBLX,ILOG,ISBIN,ISHDF5 )
          ELSE
            IRLTBLX(1,IROCK) = NTBL + 1
            T_OK = COPYIJK2D_INT( IRLTBL,IRLTBLX(1,IROCK),2,1,IROCK )
            DO 300 NL = 1,NLIN
              NTBL = NTBL + 1
              IF( NTBL.GT.LTBL ) THEN
                INDX = 5
                CHMSG = 'Number of Tables Values > Parameter LTBL'
                CALL WRMSGS( INDX )
              ENDIF
              ISTART = 1
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
                VARB = 'Water Content'
              ELSEIF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
                IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
                  VARB = 'Log Capillary Head'
                ELSE
                  VARB = 'Capillary Head'
                ENDIF
              ELSE
                VARB = 'Saturation'
              ENDIF
!
!---          Correct table values for capillary-head units  ---
!
              IF( IRPLX.GE.12 .AND. IRPLX.LE.15 ) THEN
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') &
                  VARB(1:IVR),', ',UNTS(1:NCH),': ',TBLX(NTBL)
                INDX = 0
                IUNM = 1
                VARX = 1.D+0
                CALL RDUNIT(UNTS,VARX,INDX)
                IF( IRPLX.GE.14 .AND. IRPLX.LE.15 ) THEN
                  TBLX(NTBL) = LOG( EXP(TBLX(NTBL))*VARX )
                ELSE
                  TBLX(NTBL) = TBLX(NTBL)*VARX
                ENDIF
                IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') &
                  ' (',TBLX(NTBL),', m)'
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLX(NTBL)
              ENDIF
              IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
                TBLX(NTBL) = TBLX(NTBL)/POR(2,N)
              ENDIF
              VARB = 'Aqueous Relative Permeability'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLY(NTBL))
              IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLY(NTBL)
              IF( NL.EQ.2 ) THEN
                IF( TBLX(NTBL-1).LT.TBLX(NTBL) ) THEN
                  ITDX = 1
                ELSEIF( TBLX(NTBL-1).GT.TBLX(NTBL) ) THEN
                  ITDX = -1
                ELSE
                  INDX = 4
                  CHMSG = 'Invalid Table'
                  CALL WRMSGS( INDX )
                ENDIF
                IF( TBLY(NTBL-1).LT.TBLY(NTBL) ) THEN
                  ITDY = 1
                ELSEIF( TBLY(NTBL-1).GT.TBLY(NTBL) ) THEN
                  ITDY = -1
                ELSE
                  INDX = 4
                  CHMSG = 'Invalid Table'
                  CALL WRMSGS( INDX )
                ENDIF
              ELSEIF( NL.GT.2 ) THEN
                IF( (ITDX.EQ.1 .AND. TBLX(NTBL).LE.TBLX(NTBL-1)) .OR. &
                 (ITDX.EQ.-1 .AND. TBLX(NTBL).GE.TBLX(NTBL-1)) ) THEN
                  INDX = 4
                  CHMSG = 'Invalid Table'
                  CALL WRMSGS( INDX )
                ENDIF
                IF( (ITDY.EQ.1 .AND. TBLY(NTBL).LE.TBLY(NTBL-1)) .OR. &
                 (ITDY.EQ.-1 .AND. TBLY(NTBL).GE.TBLY(NTBL-1)) ) THEN
                  INDX = 4
                  CHMSG = 'Invalid Table'
                  CALL WRMSGS( INDX )
                ENDIF
              ENDIF
  300       CONTINUE
            IRLTBLX(2,IROCK) = NTBL
            T_OK = COPYIJK2D_INT( IRLTBL,IRLTBLX(2,IROCK),2,2,IROCK )
            IF( IRPLX.EQ.11 .OR. IRPLX.EQ.13 .OR. IRPLX.EQ.15 ) THEN
              CALL SPLINY( IRLTBLX(1,IROCK),IRLTBLX(2,IROCK) )
              CALL SPLINX( IRLTBLX(1,IROCK),IRLTBLX(2,IROCK) )
            ENDIF
          ENDIF
          GOTO 400
        ENDIF
!
        LRPLC = 4
        IF( INDEX(ADUM(1:),'polmann').NE.0 ) THEN
          LRPLC = 12
        ELSEIF( INDEX(ADUM(1:),'pruess').NE.0 ) THEN
          LRPLC = 7
        ENDIF
        LRPLC = 12
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(N).EQ.1 .OR. ISCHR(N).EQ.13 .OR. &
         ISCHR(N).EQ.15 .OR. ISCHR(N).EQ.17 .OR. &
         ISCHR(N).EQ.101 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 2
            VARB = 'Aqueous Relative Permeability'
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR. &
           INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.  &
           INDEX(ADUM(1:),'modified').NE.0 ) THEN
            IF( IOM.NE.1 .AND. IOM.NE.3 .AND. IOM.NE.36 ) IERR = 1
            IRPLX = 22
            VARB = 'Modified-Mualem Porosity Distribution Model'
            CALL MMVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'irreducible').NE.0 ) THEN
            IRPLX = 21
            VARB = 'Mualem-Irreducible Porosity Distribution Model'
            CALL MIVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Corey Relative Permeability Model'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Fatt and Klikoff Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
            IRPLX = 19
            CALL PLY_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( IERR.EQ.1 ) THEN
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( INDEX(ADUM(1:),'polmann').NE.0 ) THEN
            IRPLX = IRPLX + 100
            CALL PA_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'pruess').NE.0 ) THEN
            IRPLX = IRPLX + 200
            CALL GPA_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.  &
           INDEX(ADUM(1:),'anisotropy').NE.0 ) THEN
            IRPLX = IRPLX + 300
            INDX = 2
            CALL MAVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ENDIF
!
!---    Brooks and Corey entrapment or Modified Lenhard
!       saturation functions  ---
!
        ELSEIF( ISCHR(N).EQ.6 .OR. &
         (ISCHR(N).GE.35 .AND. ISCHR(N).LE.38) ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 2
            VARB = 'Aqueous Relative Permeability'
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    van Genuchten entrapment or Modified Lenhard
!       saturation functions  ---
!
        ELSEIF( ISCHR(N).EQ.8 .OR. &
         (ISCHR(N).GE.31 .AND. ISCHR(N).LE.34) ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 2
            VARB = 'Aqueous Relative Permeability'
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
            // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    van Genuchten triple curve function  ---
!
        ELSEIF( ISCHR(N).EQ.301 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability (main drainage)'
            INDX = 2
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
            VARB = 'Aqueous Relative Permeability (main wetting)'
            INDX = 1
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution (main drainage)'
            CALL MVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
            INDX = 1
            JNDX = 5
            VARB = 'Mualem Porosity Distribution (main wetting)'
            CALL MVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution (main drainage)'
            CALL BVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
            INDX = 1
            JNDX = 5
            VARB = 'Burdine Porosity Distribution (main wetting)'
            CALL BVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Brooks and Corey saturation functions  ---
!
        ELSEIF( ISCHR(N).EQ.2 .OR. &
         ISCHR(N).EQ.14 .OR. ISCHR(N).EQ.16 .OR. &
         ISCHR(N).EQ.18 .OR. ISCHR(N).EQ.102 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR. &
           INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'modified').NE.0 ) THEN
            IF( IOM.NE.1 .AND. IOM.NE.3 .AND. IOM.NE.36 ) IERR = 1
            IRPLX = 22
            VARB = 'Modified-Mualem Porosity Distribution Model'
            CALL MMBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'irreducible').NE.0 ) THEN
            IRPLX = 21
            VARB = 'Mualem-Irreducible Porosity Distribution Model'
            CALL MIBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Corey Relative Permeability Model'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Fatt and Klikoff Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
            IRPLX = 19
            CALL PLY_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( IERR.EQ.1 ) THEN
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( INDEX(ADUM(1:),'polmann').NE.0 ) THEN
            IRPLX = IRPLX + 100
            CALL PA_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'pruess').NE.0 ) THEN
            IRPLX = IRPLX + 200
            CALL GPA_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'anisotropy').NE.0 ) THEN
            IRPLX = IRPLX + 300
            INDX = 2
            CALL MABC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ENDIF
!
!---    Brooks and Corey triple curve  ---
!
        ELSEIF( ISCHR(N).EQ.302 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability (main drainage)'
            INDX = 2
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
            VARB = 'Aqueous Relative Permeability (main wetting)'
            INDX = 1
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution (main drainage)'
            CALL MBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
            INDX = 1
            JNDX = 5
            VARB = 'Mualem Porosity Distribution (main wetting)'
            CALL MBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution (main drainage)'
            CALL BBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
            INDX = 1
            JNDX = 5
            VARB = 'Burdine Porosity Distribution (main wetting)'
            CALL BBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Dual Porosity van Genuchten function  ---
!
        ELSEIF( ISCHR(N).EQ.3 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Matrix Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
            VARB = 'Fracture Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'linear fracture').NE.0 ) THEN
            IRPLX = 31
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 .AND. &
           INDEX(ADUM(1:),'linear fracture').NE.0 ) THEN
            IRPLX = 32
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Mualem Porosity Distribution Model'
            CALL MVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Burdine Porosity Distribution Model'
            CALL BVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Dual Porosity Brooks and Corey function  ---
!
        ELSEIF( ISCHR(N).EQ.4 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Matrix Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
            VARB = 'Fracture Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'linear fracture').NE.0 ) THEN
            IRPLX = 31
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 .AND. &
           INDEX(ADUM(1:),'linear fracture').NE.0 ) THEN
            IRPLX = 32
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Mualem Porosity Distribution Model'
            CALL MBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Burdine Porosity Distribution Model'
            CALL BBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Unknown saturation function  ---
!
        ELSE
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR. &
           INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Corey Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Fatt and Klikoff Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
            IRPLX = 19
            CALL PLY_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( INDEX(ADUM(1:),'polmann').NE.0 ) THEN
            IRPLX = IRPLX + 100
            CALL PA_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'pruess').NE.0 ) THEN
            IRPLX = IRPLX + 200
            CALL GPA_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
          ENDIF
        ENDIF
!
!---  Hysteretic saturation functions  ---
!
      ELSE
        VARB = 'Porosity Distribution Model'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:), 'mualem').NE.0 ) THEN
          IRPLX = 1
          IF(ME.EQ.0)WRITE(IWR,'(2X,A)') &
            'Mualem Porosity Distribution Model'
        ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
          IRPLX = 2
          IF(ME.EQ.0)WRITE(IWR,'(2X,A)') &
            'Burdine Porosity Distribution Model'
        ELSE
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Unrecognized Relative Perm. Function: ' &
           // ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Translate relative permeability type  ---
!
  400 CONTINUE
!       DO 420 NX = 1,NUM_NODES
!         IRPL(NX) = IRPLX
!  420  CONTINUE
        T_OK = COPYIJK1D_INT(IRPL,IRPLX,IFLG )
      ENDIF
      ENDDO
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( NN.LT.NROCK .AND.ME.EQ.0) WRITE(IWR,'(/)')
!
!---  Continue reading rock/soil type names for a pattern match  ---
!
!      IF( KBS.EQ.1 .AND. IROCK.LT.NROCK ) THEN
!        IROCK = IROCK + 1
!        ISTART = ISBS
!        GOTO 20
!      ENDIF
      GOTO 10
 500  CONTINUE
!      if( ijk <= 0 ) then
!        do n=1,num_nodes
!           if(ixp(n).gt.0)&
!           print *, rplc(2,n),irpl(n),iz(n),n
!          rplc(:,n) = rplc_tmp(:,iz(n)) 
!          irpl(n) = irpl_tmp(iz(n))
!        enddo
!      endif
!
!---  End of RDLRP group ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE C_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
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
!     Constant aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME,BDUM
      REAL*8 RPLCX(LRPLC)
      INTEGER LO(3),HI(3),LDXX(3)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/C_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 1 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
      VARX = 1.0D0
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,RPLC,VARX, &
          ISBIN,ISHDF5 )
        UNTS = 'null'
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') &
           VARB(1:IVR),': ',RPLCX(INDX)
        T_OK = COPYIJK2D( RPLC,RPLCX(INDX),LNDX,INDX,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF C_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE FC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 4 November 2002.
!     Last Modified by MD White, PNNL, 4 November 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/FC_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 1 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
      VARX = 1.D+0
      IF(ME.EQ.0) &
        WRITE (IWR,'(2X,A)')'Free Corey Relative Permeability Function'
      VARB = 'Endpoint Aqueous Relative Permeability'
      INDC = 1
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(1)
        T_OK = COPYIJK2D( RPLC,RPLCX(1),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Exponent Aqueous Relative Permeability'
      INDC = 2
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(2)
        T_OK = COPYIJK2D( RPLC,RPLCX(2),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Residual Aqueous Saturation'
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(3))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(3)
        T_OK = COPYIJK2D( RPLC,RPLCX(3),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Residual Gas Saturation'
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(4))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(4)
        T_OK = COPYIJK2D( RPLC,RPLCX(4),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF FC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE HK_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Haverkamp aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/HK_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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
      LNDX = LRPLC
      ISBIN = .FALSE.

      IF(ME.EQ.0)WRITE (IWR,'(2X,A)')'Haverkamp Relative Permeability ' // &
       ' Function'
      VARB = 'Haverkamp (A)'
      INDC = 1
      IF( IJK > 1 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
        INDX = 0
        IUNM = 1
        VARX = 1.D+0
        INDC = 4
        CALL RDUNIT(UNTS,VARX,INDX)
        T_OK = COPYIJK2D( RPLC,VARX,LNDX,INDC,IROCK )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
         UNTS(1:NCH),': ',RPLCX(1)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLCX(1),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLCX(1),', m)'
        T_OK = COPYIJK2D( RPLC,RPLCX(1),LNDX,INDC,IFLG )
        INDC = 4
        INDX = 0
        IUNM = 1
        RPLCX(4) = 1.D+0
        CALL RDUNIT(UNTS,RPLCX(4),INDX)
        T_OK = COPYIJK2D( RPLC,RPLCX(4),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Haverkamp (gamma)'
      INDC = 2
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RPLCX(2)
        T_OK = COPYIJK2D( RPLC,RPLCX(2),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Haverkamp: Effective Air Entry Head'
      INDC = 3
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        UNTS = 'null'
        UNTS = 'm'
        IUNM = 1
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(3))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
         UNTS(1:NCH),': ',RPLCX(3)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLCX(3),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLCX(3),', m)'
        T_OK = COPYIJK2D( RPLC,RPLCX(3),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF HK_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Rijtema-Gardner aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RG_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 1 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
      VARB = 'Rijtema-Gardner Modified Exponential Model'
      IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB
      VARB = 'Rijtema-Gardner: a Parameter'
      INDC = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        UNTS = '1/m'
        IUNM = -1
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
         UNTS(1:NCH),': ',RPLCX(2)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,RPLCX(2),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') &
          ' (',RPLCX(2),', 1/m)'
        T_OK = COPYIJK2D( RPLC,RPLCX(2),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Rijtema-Gardner: Effective Air Entry Head'
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
         UNTS(1:NCH),': ',RPLCX(1)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLCX(1),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLCX(1),', m)'
        T_OK = COPYIJK2D( RPLC,RPLCX(2),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF RG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
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
!     Mualem-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE CONST
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MVG_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC

      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLC(INDX,N) = 1.D+0-1.D+0/SCHR(JNDX,N)
          IF( RPLC(INDX,N).LT.EPSL ) THEN
            INDX = 4
            CHMSG = 'Negative van Genuchten ''m'' Parameter'
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(INDX) = 1.D+0-1.D+0/SCHR(JNDX,N)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(INDX,N) = 1.D+0-1.D+0/SCHR(JNDX,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDX),LNDX,INDX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'm Parameter: ',RPLCX(INDX)
        IF( RPLCX(INDX).LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Negative van Genuchten ''m'' Parameter'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
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
!     Mualem-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MBC_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC

      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLC(INDX,N) = SCHR(JNDX,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(INDX) = SCHR(JNDX,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(INDX,N) = SCHR(JNDX,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDX),LNDX,INDX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ', &
         RPLCX(INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MBC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
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
!     Burdine-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE CONST
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/BVG_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC

      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLC(INDX,N) = 1.D+0-2.D+0/SCHR(JNDX,N)
          IF( RPLC(INDX,N).LT.EPSL ) THEN
            INDX = 4
            CHMSG = 'Negative van Genuchten ''m'' Parameter'
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(INDX) = 1.D+0-2.D+0/SCHR(JNDX,N)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(INDX,N) = 1.D+0-2.D+0/SCHR(JNDX,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDX),LNDX,INDX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 2/n'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',&
          RPLCX(INDX)
        IF( RPLCX(INDX).LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Negative van Genuchten ''m'' Parameter'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF BVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX )
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
!     Burdine-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/BBC_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC

      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLC(INDX,N) = SCHR(JNDX,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(INDX) = SCHR(JNDX,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(INDX,N) = SCHR(JNDX,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDX),LNDX,INDX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ', &
          RPLCX(INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF BBC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE STN_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Stone aqueous relative permeability.
!
!     Stone, H.L.  1970.  "Probability Model for Estimating Three-Phase
!     Relative Permeability."  Trans. SPE of AIME, 249:214-218.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 January 2007.
!     Last Modified by MD White, PNNL, 30 January 2007.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/STN_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF

      IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
        'Stone Aqueous Relative Permeability Function'
      VARB = 'Stone (Slr)'
      INDC = 1
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RPLCX(1)
        T_OK = COPYIJK2D( RPLC,RPLCX(1),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Stone (n)'
      INDC = 2
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(2)
        T_OK = COPYIJK2D( RPLC,RPLCX(2),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF STN_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TV_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Touma and Vauclin aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/TV_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC

      IF(ME.EQ.0)WRITE (IWR,'(2X,A)')'Touma and Vauclin Relative ' // &
       'Permeability Function'
      VARB = 'Touma and Vauclin (alpha)'
      INDC = 1
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(1)
        T_OK = COPYIJK2D( RPLC,RPLCX(1),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Touma and Vauclin (beta)'
      INDC = 2
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RPLCX(2)
        T_OK = COPYIJK2D( RPLC,RPLCX(2),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF TV_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PA_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Polmann anisotropy
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
#include "utils.h"
#include "gagrid.h"
!
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/PA_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF

      IF(ME.EQ.0)WRITE (IWR,'(/,2X,A)') 'w/ Polmann Anisotropy '
      VARB = 'Mean of ln(Ks), with Ks in cm/s'
      INDC = 5
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(5))
        IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RPLCX(5)
        T_OK = COPYIJK2D( RPLC,RPLCX(5),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Variance of ln(Ks), with Ks in cm/s'
      INDC = 6
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(6))
        IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RPLCX(6)
        T_OK = COPYIJK2D( RPLC,RPLCX(6),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Slope of the Beta versus ln(Ks) regression' &
       // ' with Ks in cm/s'
      INDC = 7
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(7))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),', (1/cm): ',RPLCX(7)
        T_OK = COPYIJK2D( RPLC,RPLCX(7),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Zeta (Ratio of Standard Deviation/Variance)' &
       // ' with Ks in cm/s'
      INDC = 8
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(8))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),', (1/cm): ',RPLCX(8)
        T_OK = COPYIJK2D( RPLC,RPLCX(8),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Vertical correlation lengths for ln(Ks)' &
       // ' with Ks in cm/s'
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(9))
        IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),', (cm): ',RPLCX(9)
        T_OK = COPYIJK2D( RPLC,RPLCX(9),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Mean slope, Beta, for ln(Ks) vs. Psi' &
       // ' with Ks in cm/s'
      INDC = 10
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(10))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') &
          VARB(1:IVR),', (1/cm): ',RPLCX(10)
        T_OK = COPYIJK2D( RPLC,RPLCX(10),LNDX,INDC,IFLG )
      ENDIF
      RPLCX(11) = 1.D+0
      INDC = 11
      T_OK = COPYIJK2D( RPLC,RPLCX(11),LNDX,INDC,IFLG )
      CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Upper Anisotropy Ratio Limit'
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
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
              ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(11))
          IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
           RPLCX(11)
          T_OK = COPYIJK2D( RPLC,RPLCX(11),LNDX,INDC,IFLG )
        ENDIF
      ENDIF
      RPLCX(12) = 1.D+0
      INDC = 12
      T_OK = COPYIJK2D( RPLC,RPLCX(11),LNDX,INDC,IFLG )
      CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Lower Anisotropy Ratio Limit'
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
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(12))
          IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
           RPLCX(12)
          T_OK = COPYIJK2D( RPLC,RPLCX(12),LNDX,INDC,IFLG )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF PA_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PLY_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Polynomial aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 18 August 2003.
!     Last Modified by MD White, PNNL, 18 August 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      REAL*8 RPLCX(LRPLC)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: CPLY_RLX
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/PLY_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!--- Allocate array
!
      CALL ADD_NODE_IFIELD('nply_rl', IDX)
      NPLY_RL => I_ND_FLD(IDX)%P
      NPLY_RL = 0
!
!--- Read values
!
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
      IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
        'Polynomial Aqueous Relative Permeability Function'
      VARB = 'Number of Polynomial Function Pieces'
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        INDX = 4
        CHMSG = 'IJK Indexing Not Available for Polynomial Functions'
        CALL WRMSGS( INDX )
      ENDIF
      CALL RDINT(ISTART,ICOMMA,CHDUM,NPLY_RLX)
      IF(ME.EQ.0)WRITE(IWR,'(2X,2A,I1)') VARB(1:IVR),': ',NPLY_RLX
      T_OK = COPYIJK1D_INT( NPLY_RL,NPLY_RLX,IFLG )
      LPOLYC = NPLY_RL(IROCK)
!
!--- Allocate arrays
!
      DIM1 = LPOLYC
      DIM2 = LPOLYN
      CALL ADD_NODE_D3FIELD('cply_rl', DIM1, DIM2,IDX)
      CPLY_RL => D_ND_3FLD(IDX)%P
      CPLY_RL = 0.D+0
      ALLOCATE( CPLY_RLX(1:LPOLYC,1:LPOLYN) )
      IF( NPLY_RL(IROCK).GT.LPOLYN ) THEN
        INDX = 5
        CHMSG = 'Number of Aqueous Relative Permeability ' // &
         'Polynomial Function Pieces > Parameter LPOLYN'
        CALL WRMSGS( INDX )
      ENDIF
      VARB = 'Saturated Hydraulic Conductivity'
      RPLCX(2) = 1.D+0
      IDFLT = 1
      CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
      LNDX = LRPLC
      T_OK = COPYIJK2D( RPLC,RPLCX(2),LNDX,INDC,IFLG )
      DO 60 NPX = 1,NPLY_RL(IROCK)
        VARB = 'Polynomial Piece #  : '
        IF(ME.EQ.0)WRITE(VARB(14:14),'(I1)') NPX
        ISTART = 1
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        VARB = 'Number of Polynomial Coefficients'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NCOEF)
        IF( (NCOEF+4).GT.LPOLYC ) THEN
          INDX = 5
          CHMSG = 'Number of Aqueous Relative Permeability ' // &
           'Polynomial Coefficients > Parameter LPOLYC'
          CALL WRMSGS( INDX )
        ENDIF
        VARB = 'Minimum Head for Polynomial Piece'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_RLX(1,NPX))
        LCX = NPLY_RL(IROCK)
        T_OK = COPYIJK3D( CPLY_RL,CPLY_RLX(1,NPX),LPOLYN,1,LCX,NPX, &
          IFLG )
        VARB = 'Maximum Head for Polynomial Piece'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_RLX(2,NPX))
        T_OK = COPYIJK3D( CPLY_RL,CPLY_RLX(2,NPX),LPOLYN,2,LCX,NPX, &
          IFLG )
        CPLY_RLX(3,NPX) = 0.D+0
        CPLY_RLX(4,NPX) = 0.D+0
        T_OK = COPYIJK3D( CPLY_RL,CPLY_RLX(3,NPX),LPOLYN,3,LCX,NPX, &
          IFLG )
        T_OK = COPYIJK3D( CPLY_RL,CPLY_RLX(4,NPX),LPOLYN,4,LCX,NPX, &
          IFLG )
        DO 40 NCX = 5,NCOEF+4
          VARB = 'Coefficient for Polynomial Piece'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_RLX(NCX,NPX))
!
!---      Maximum aqueous relative permeability for polynomial piece ---
!
          CPLY_RLX(3,NPX) = CPLY_RLX(3,NPX) + &
           CPLY_RLX(NCX,NPX)*(LOG10(CPLY_RLX(1,NPX))**(NCX-5))
!
!---      Mininum aqueous relative permeability for polynomial piece ---
!
          CPLY_RLX(4,NPX) = CPLY_RLX(4,NPX) + &
           CPLY_RLX(NCX,NPX)*(LOG10(CPLY_RLX(2,NPX))**(NCX-5))
   40   CONTINUE
        CPLY_RLX(3,NPX) = (1.D+1**CPLY_RLX(3,NPX))/RPLCX(2)
        CPLY_RLX(4,NPX) = (1.D+1**CPLY_RLX(4,NPX))/RPLCX(2)
        T_OK = COPYIJK3D( CPLY_RL,CPLY_RLX(3,NPX),LPOLYN,3, &
          LCX,NPX,IFLG )
        T_OK = COPYIJK3D( CPLY_RL,CPLY_RLX(4,NPX),LPOLYN,4, &
          LCX,NPX,IFLG )
        VARB = 'Head Units for Polynomial Piece'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ',UNTS
        RPLCX(1) = 1.D+0
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLCX(1),INDX)
        INDC = 2
        LNDX = LRPLC
        T_OK = COPYIJK2D( RPLC,RPLCX(2),LNDX,INDC,IFLG )
        IF(ME.EQ.0)WRITE(IWR,'(2X,3A,1PE11.4,$)') &
         'Minimum Head for Polynomial Piece, ',UNTS(1:NCH), &
         ': ',CPLY_RLX(1,NPX)
        VAR = CPLY_RLX(1,NPX)*RPLCX(1)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', m)'
        IF(ME.EQ.0)WRITE(IWR,'(2X,3A,1PE11.4,$)') &
         'Maximum Head for Polynomial Piece, ',UNTS(1:NCH), &
         ': ',CPLY_RLX(2,NPX)
        VAR = CPLY_RLX(2,NPX)*RPLCX(1)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', m)'
        DO  50 NCX = 5,NCOEF+4
          VARB = 'Coefficient #  : '
          IF(ME.EQ.0)WRITE(VARB(14:14),'(I1)') NCX-4
          IF(ME.EQ.0)WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:17), &
           CPLY_RLX(NCX,NPX)
           T_OK = COPYIJK3D( CPLY_RL,CPLY_RLX(NCX,NPX),LPOLYN,4,LCX, &
            NPX,IFLG )
   50   CONTINUE
   60   CONTINUE
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF PLY_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GPA_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Gompertz-Pruess anisotropy
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/GPA_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF

      VARB = 'Gompertz Function a Parameter'
      INDC = 5
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(5))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(5)
        T_OK = COPYIJK2D( RPLC,RPLCX(5),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Gompertz Function b Parameter'
      INDC = 6
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(6))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(6)
        T_OK = COPYIJK2D( RPLC,RPLCX(6),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Gompertz Function c Parameter'
      INDC = 7
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
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(7))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLCX(7)
        T_OK = COPYIJK2D( RPLC,RPLCX(7),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF GPA_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MAVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
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
!     Mualem Anisotropy-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 April 2002.
!     Last Modified by MD White, PNNL, 9 April 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MAVG_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read horizontal pore-scale parameter   ---
!
      INDC = INDX+1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLC(INDC,N) = 0.5D+0
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(INDC) = 0.5D+0
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDC))
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') &
            'Default Value: 1/2 (Square Root)'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'Horizontal Pore-Scale Parameter: ', &
         RPLCX(INDC)
        T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IFLG )
      ENDIF
!
!---  Read vertical pore-scale parameter   ---
!
      INDC = INDX+2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 30 N = 1,NUM_NODES
          RPLC(INDC,N) = 0.5D+0
   30   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(INDC) = 0.5D+0
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDC))
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') &
            'Default Value: 1/2 (Square Root)'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'Vertical Pore-Scale Parameter: ', &
         RPLCX(INDC)
        T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MAVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MABC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX )
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
!     Mualem-Anisotropy Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 April 2002.
!     Last Modified by MD White, PNNL, 9 April 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MABC_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read horizontal pore-scale parameter  ---
!
      INDC = INDX+1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLC(INDC,N) = 0.5D+0
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(INDC) = 0.5D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDC))
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'Horizontal Pore-Scale Parameter: ', RPLCX(INDC)
        T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IFLG )
      ENDIF
!
!---  Read vertical pore-scale parameter  ---
!
      INDC = INDX+2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 30 N = 1,NUM_NODES
          RPLC(INDC,N) = 0.5D+0
   30   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(INDC) = 0.5D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDC))
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'Vertical Pore-Scale Parameter: ', RPLCX(INDC)
        T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MABC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MIVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Mualem Irreducible-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 May 2002.
!     Last Modified by MD White, PNNL, 21 May 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MIVG_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC
!
!---  Read 'm' parameter  ---
!
      INDC = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLC(2,N) = 1.D+0-1.D+0/SCHR(3,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(2) = 1.D+0-1.D+0/SCHR(3,N)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(2,N) = 1.D+0-1.D+0/SCHR(3,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'm Parameter: ',RPLCX(2)
      ENDIF
!
!---  Read irreducible saturation  ---
!
      INDC = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLC(1,N) = SCHR(4,N)
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(1) = SCHR(4,N)
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(1,N) = SCHR(4,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(1),LNDX,INDC,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'slr Parameter: ',RPLCX(1)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MIVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MIBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Mualem Irreducible-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 May 2002.
!     Last Modified by MD White, PNNL, 21 May 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MIBC_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC
!
!---  Read 'm' parameter  ---
!
      INDC = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLC(2,N) = SCHR(3,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(2) = SCHR(3,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(2,N) = SCHR(3,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ', &
         RPLCX(2)
      ENDIF
!
!---  Read irreducible saturation  ---
!
      INDC = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLC(1,N) = SCHR(4,N)
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(1) = SCHR(4,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(1,N) = SCHR(4,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'slr Parameter: ',RPLCX(1)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MIBC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MMVG_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Mualem Irreducible-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by MD White, PNNL, 17 December 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MMVG_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read 'm' parameter  ---
!
      INDC = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLC(2,N) = 1.D+0-1.D+0/SCHR(3,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(2) = 1.D+0-1.D+0/SCHR(3,N)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(2,N) = 1.D+0-1.D+0/SCHR(3,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLCX(2)
      ENDIF
!
!---  Read pore-scale parameter  ---
!
      INDC = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLC(1,N) = 5.D-1
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(1) = 5.D-1
        IDFLT = 1
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') &
            'Default Value: 1/2 (Square Root)'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'Pore-Scale Parameter: ', &
         RPLCX(1)
        T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MMVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MMBC_LRP( ISTART,ICOMMA,N,IROCK,IJK,CHDUM )
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
!     Mualem Irreducible-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by MD White, PNNL, 17 December 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MMBC_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

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

      ISBIN = .FALSE.
      LNDX = LRPLC
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read 'm' parameter  ---
!
      INDC = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLC(2,N) = SCHR(3,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(2) = SCHR(3,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLC(2,N) = SCHR(3,N)
          ENDDO
        ELSE
          T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ', &
         RPLCX(2)
      ENDIF
!
!---  Read pore-scale parameter  ---
!
      INDC = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLC(1,N) = 5.D-1
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,RPLC,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        RPLCX(1) = 5.D-1
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') &
            'Default Value: 1/2 (Square Root)'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'Pore-Scale Parameter: ', RPLCX(1)
        T_OK = COPYIJK2D( RPLC,RPLCX(INDC),LNDX,INDC,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MMBC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDLRPT( ITX )
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
!     Read input file for rock/soil aqueous
!     relative permeability tensor function information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!     $Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*7 PREFIX
      CHARACTER*64 ADUM,RDUM,UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER :: IRPLTX(3)
      INTEGER, DIMENSION(:), ALLOCATABLE :: FLG_RD
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: IRLTBLTX
      INTEGER LO(3),HI(3),LDXX(3),DIM1,DIM2
      REAL*8 RPLCX(LRPLC)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDLRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      ITDX = 0
      ITDY = 0
      ITDZ = 0
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

      ISBIN = .FALSE.
      LNDX = LRPLC

      IF( ITX.EQ.1 ) PREFIX = 'X-Dir. '
      IF( ITX.EQ.2 ) PREFIX = 'Y-Dir. '
      IF( ITX.EQ.3 ) PREFIX = 'Z-Dir. '
!
!---  WRITE card information to ouput file  ---
!
      IF( ITX.EQ.0 ) THEN
        CARD = 'Aqueous Relative Permeability Function Card'
      ELSE
        CARD = PREFIX // 'Aqueous Relative Permeability Function Card'
      ENDIF
      ICD = INDEX( CARD,'  ' )-1
      IF(ME.EQ.0)WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Setup pointer arrays
!
      IF( ITX.EQ.1 )THEN
        DIM1 = 3
        CALL ADD_NODE_I2FIELD('irplt', DIM1, IDX)
        IRPLT => I_ND_2FLD(IDX)%P
        IRPLT = 0
        DIM1 = 4
        DIM2 = LRPL
        CALL ADD_NODE_D3FIELD('rplt', DIM1, DIM2,IDX)
        RPLT => D_ND_3FLD(IDX)%P
        RPLT = 0.d0
        DIM1 = 3
        CALL ADD_NODE_IFIELD('irpl', IDX)
        IRPL => I_ND_FLD(IDX)%P
        IRPL = 0
        DIM1 = LRPLC
        CALL ADD_NODE_D2FIELD('rplc', DIM1, IDX)
        RPLC => D_ND_2FLD(IDX)%P
        RPLC = 0.d0
      ENDIF
!
!---  Loop over the rock/soil aqueous relative permeability
!     information lines  ---
!
      NN = 0
      IJK = 0
      ISALLOC = 0
   10 CONTINUE
      IF( NN.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  Check for a pair of delimiting slashes in the rock/soil name,
!     indicating a pattern of rock/soil types  ---
!
      KBS = 0
      IBS = INDEX( RDUM(1:),'/' )
      IF( IBS.GT.0 ) THEN
        IBS = IBS + 1
        JBS = INDEX( RDUM(IBS:),'/')
        IF( JBS.GT.0 ) THEN
          JBS = IBS + JBS - 2
          KBS = 1
          ISBS = ISTART
        ENDIF
      ENDIF
      IROCK = 1
   20 CONTINUE
!
!---  IJK indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IJK = 1
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = IROCK,NROCK
        IF( KBS.EQ.1 ) THEN
          IF( INDEX( ROCK(M)(1:),RDUM(IBS:JBS) ).GT.0 ) THEN
            IROCK = M
            GOTO 200
          ENDIF
        ELSE
          IF( RDUM.EQ.ROCK(M) ) THEN
            IROCK = M
            GOTO 200
          ENDIF
        ENDIF
  100 CONTINUE
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
      CALL WRMSGS( INDX )
      GOTO 10
  200 CONTINUE
!
!---  WRITE rock/soil name  ---
!
      IF(ME.EQ.0)WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
!
!---  Read aqueous relative permeability pressure function  ---
!
      NN = NN + 1
  220 CONTINUE
!
!---  Nonhysteretic saturation functions  ---
!
      IERR = 0
      IF(IJK > 0) THEN
        IF(.NOT.ALLOCATED(FLG_RD))ALLOCATE(FLG_RD(1))
      ELSE
        IF(.NOT.ALLOCATED(FLG_RD))ALLOCATE(FLG_RD(NROCK))
      ENDIF
      FLG_RD = 0
      NUM_NODESX = NUM_NODES
      IF( IJK > 0 ) NUM_NODESX = 1
      DO N = 1,NUM_NODESX
        IF( IJK <= 0 .and. IZ(N) <= 0) CYCLE
        IF( (IJK <= 0.AND.IZ(N) == IROCK) .OR. IJK > 0 ) THEN
         IF( IJK <= 0 ) THEN
           IF(FLG_RD(IZ(N)) > 0) CYCLE
         ENDIF
         IF( IJK <= 0 ) FLG_RD(IZ(N)) = 1

      IF( ISCHR(N).LT.20 .OR. ISCHR(N).GT.30 ) THEN
      IF( ITX.EQ.0 ) THEN
        VARB = 'Aqueous Relative Permeability Function'
      ELSE
        VARB = PREFIX // 'Aqueous Relative Permeability Function'
      ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Tabular (relative permeability versus liquid saturation)  ---
!
        IF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
          IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
            IF(ME.EQ.0)WRITE(IWR,'(A)') &
             'Tabular Aqueous Relative Permeability ' &
             // 'Versus Water Content Function'
            IRPLX = 10
          ELSEIF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
            IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
              IF(ME.EQ.0)WRITE(IWR,'(A)') &
                'Tabular Aqueous Relative Permeability ' &
               // 'Versus Log Capillary Head Function'
              IRPLX = 14
            ELSE
              IF(ME.EQ.0)WRITE(IWR,'(A)') &
                'Tabular Aqueous Relative Permeability ' &
                // 'Versus Capillary Head Function'
              IRPLX = 12
            ENDIF
          ELSE
            IF(ME.EQ.0)WRITE(IWR,'(A)') &
             'Tabular Aqueous Relative Permeability ' &
             // 'Versus Saturation Function'
            IRPLX = 10
          ENDIF
          IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
            IRPLX = IRPLX+1
            IF(ME.EQ.0)WRITE(IWR,'(A)') 'Cubic Spline Interpolation'
          ELSE
            IF(ME.EQ.0)WRITE(IWR,'(A)') 'Linear Interpolation'
          ENDIF
          IF( .NOT.ALLOCATED(IRLTBLTX) )ALLOCATE(IRLTBLTX(3,2,NROCK))
          DO I = 1,MAX_IND_FLD
            IF( 'irltblt'.eq.i_nd_3fld_names(i) )THEN
              ISALLOC = 1
            ENDIF
          END DO
          IF( ISALLOC.EQ.0 )THEN
             DIM1 = 3
             DIM2 = 2
             CALL ADD_NODE_I3FIELD('irltblt', DIM1, IDX)
             IRLTBLT => I_ND_3FLD(IDX)%P
             IRLTBLT = 0.d0
             ISALLOC = 1
          ENDIF
          VARB = 'Number of Tabular Entries'
          CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A,I6)') VARB,': ',NLIN
          IF( NLIN.LT.2 ) THEN
            INDX = 4
            CHMSG = 'Invalid Table'
            CALL WRMSGS( INDX )
          ENDIF
!
!---      IJK Indexing  ---
!
          IF( INDEX(ADUM,'file').NE.0 ) THEN
            IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
              VARB = 'Water Content'
              UNTS = 'null'
            ELSEIF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
              IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
                VARB = 'Log Capillary Head'
              ELSE
                VARB = 'Capillary Head'
              ENDIF
              IUNM = 1
              UNTS = 'm'
            ELSE
              VARB = 'Saturation'
              UNTS = 'null'
            ENDIF
            NTBLX = NTBL
            ILOG = 0
            IF( IRPLX.GE.14 .AND. IRPLX.LE.15 ) ILOG = 1
            IF( ITX.EQ.0 ) THEN
              T_OK = GETFILENAME( ADUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( ADUM,UNTS,ISTART,ICOMMA )
              CALL RDIJKT( T_FILENAME,TBLX,VARX,IRLTBL,NLIN, &
                NTBLX,ILOG,ISBIN,ISHDF5 )
              DO 280 NX = 1,NUM_NODES
                DO 280 M = IRLTBLT(ITX,1,N),IRLTBLT(ITX,2,N)
                  IRLTBLT(2,1,NX) = IRLTBLT(1,1,NX)
                  IRLTBLT(2,2,NX) = IRLTBLT(1,2,NX)
                  IRLTBLT(3,1,NX) = IRLTBLT(1,1,NX)
                  IRLTBLT(3,2,NX) = IRLTBLT(1,2,NX)
  280         CONTINUE
            ELSE
              T_OK = GETFILENAME( ADUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( ADUM,UNTS,ISTART,ICOMMA )
              CALL RDIJKT( T_FILENAME,TBLX,VARX,IRLTBL,NLIN, &
                NTBLX,ILOG,ISBIN,ISHDF5 )
            ENDIF
            IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
              DO 282 NX = 1,NUM_NODES
                DO 282 M = IRLTBLT(ITX,1,NX),IRLTBLT(ITX,2,NX)
                  TBLX(M) = TBLX(M)/POR(2,NX)
  282         CONTINUE
            ENDIF
            IF( ITX.EQ.0 ) VARB = 'Aqueous Relative Permeability'
            IF( ITX.EQ.1 ) VARB = 'X-Aqueous Relative Permeability'
            IF( ITX.EQ.2 ) VARB = 'Y-Aqueous Relative Permeability'
            IF( ITX.EQ.3 ) VARB = 'Z-Aqueous Relative Permeability'
            UNTS = 'null'
            ILOG = 0
            IF( ITX.EQ.0 ) THEN
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = 1.0D0
              CALL RDIJKT( T_FILENAME,TBLY,VARX,IRLTBL,NLIN, &
                NTBLX,ILOG,ISBIN,ISHDF5 )
              DO 284 NX = 1,NUM_NODES
                DO 284 M = IRLTBLT(ITX,1,NX),IRLTBLT(ITX,2,NX)
                  IRLTBLT(2,1,NX) = IRLTBLT(1,1,NX)
                  IRLTBLT(2,2,NX) = IRLTBLT(1,2,NX)
                  IRLTBLT(3,1,NX) = IRLTBLT(1,1,NX)
                  IRLTBLT(3,2,NX) = IRLTBLT(1,2,NX)
  284         CONTINUE
            ELSE
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = 1.0D0
              CALL RDIJKT( T_FILENAME,TBLY,VARX,IRLTBL,NLIN, &
                NTBLX,ILOG,ISBIN,ISHDF5 )
            ENDIF
!
!---      Rock/Soil Type Indexing  ---
!
          ELSE
            IF( ITX.EQ.0 ) THEN
              IRLTBLTX(1,IROCK,1) = NTBL + 1
              T_OK = COPYIJK3D_INT( IRLTBLT,IRLTBLTX(ITX,1,IROCK),2,1, &
                3,1,IROCK )
              IRLTBLTX(1,IROCK,2) = NTBL + 1
              T_OK = COPYIJK3D_INT( IRLTBLT,IRLTBLTX(ITX,1,IROCK),2,1, &
                3,2,IROCK )
              IRLTBLTX(1,IROCK,3) = NTBL + 1
              T_OK = COPYIJK3D_INT( IRLTBLT,IRLTBLTX(ITX,1,IROCK),2,1, &
                3,3,IROCK )
            ELSE
              IRLTBLTX(1,IROCK,ITX) = NTBL + 1
              T_OK = COPYIJK3D_INT( IRLTBLT,IRLTBLTX(ITX,1,IROCK),2,1, &
                3,ITX,IROCK )
            ENDIF
            DO 300 NL = 1,NLIN
              NTBL = NTBL + 1
              IF( NTBL.GT.LTBL ) THEN
                INDX = 5
                CHMSG = 'Number of Tables Values > Parameter LTBL'
                CALL WRMSGS( INDX )
              ENDIF
              ISTART = 1
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
                VARB = 'Water Content'
              ELSEIF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
                IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
                  VARB = 'Log Capillary Head'
                ELSE
                  VARB = 'Capillary Head'
                ENDIF
              ELSE
                VARB = 'Saturation'
              ENDIF
!
!---          Correct table values for capillary-head units  ---
!
              IF( IRPLX.GE.12 .AND. IRPLX.LE.15 ) THEN
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') &
                  VARB(1:IVR),', ',UNTS(1:NCH),': ',TBLX(NTBL)
                INDX = 0
                IUNM = 1
                VARX = 1.D+0
                CALL RDUNIT(UNTS,VARX,INDX)
                IF( IRPLX.GE.14 .AND. IRPLX.LE.15 ) THEN
                  TBLX(NTBL) = LOG( EXP(TBLX(NTBL))*VARX )
                ELSE
                  TBLX(NTBL) = TBLX(NTBL)*VARX
                ENDIF
                IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') &
                  ' (',TBLX(NTBL),', m)'
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLX(NTBL)
              ENDIF
              IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
                TBLX(NTBL) = TBLX(NTBL)/POR(2,IROCK)
              ENDIF
              VARB = 'Aqueous Relative Permeability'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLY(NTBL))
              IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLY(NTBL)
              IF( NL.EQ.2 ) THEN
                IF( TBLX(NTBL-1).LT.TBLX(NTBL) ) THEN
                  ITDX = 1
                ELSEIF( TBLX(NTBL-1).GT.TBLX(NTBL) ) THEN
                  ITDX = -1
                ELSE
                  INDX = 4
                  CHMSG = 'Invalid Table'
                  CALL WRMSGS( INDX )
                ENDIF
                IF( TBLY(NTBL-1).LT.TBLY(NTBL) ) THEN
                  ITDY = 1
                ELSEIF( TBLY(NTBL-1).GT.TBLY(NTBL) ) THEN
                  ITDY = -1
                ELSE
                  INDX = 4
                  CHMSG = 'Invalid Table'
                  CALL WRMSGS( INDX )
                ENDIF
              ELSEIF( NL.GT.2 ) THEN
                IF( (ITDX.EQ.1 .AND. TBLX(NTBL).LE.TBLX(NTBL-1)) .OR. &
                 (ITDX.EQ.-1 .AND. TBLX(NTBL).GE.TBLX(NTBL-1)) ) THEN
                  INDX = 4
                  CHMSG = 'Invalid Table'
                  CALL WRMSGS( INDX )
                ENDIF
                IF( (ITDY.EQ.1 .AND. TBLY(NTBL).LE.TBLY(NTBL-1)) .OR. &
                 (ITDY.EQ.-1 .AND. TBLY(NTBL).GE.TBLY(NTBL-1)) ) THEN
                  INDX = 4
                  CHMSG = 'Invalid Table'
                  CALL WRMSGS( INDX )
                ENDIF
              ENDIF
  300       CONTINUE
            IF( ITX.EQ.0 ) THEN
              IRLTBLTX(1,2,IROCK) = NTBL
              IRLTBLTX(2,2,IROCK) = NTBL
              IRLTBLTX(3,2,IROCK) = NTBL
              T_OK = COPYIJK3D_INT( IRLTBLT,IRLTBLTX(ITX,1,IROCK),2,2, &
                3,1,IROCK )
              T_OK = COPYIJK3D_INT( IRLTBLT,IRLTBLTX(ITX,1,IROCK),2,2, &
                3,2,IROCK )
              T_OK = COPYIJK3D_INT( IRLTBLT,IRLTBLTX(ITX,1,IROCK),2,2, &
                3,3,IROCK )
              IF( IRPLX.EQ.11 .OR. IRPLX.EQ.13 .OR. IRPLX.EQ.15 ) THEN
                CALL SPLINY( IRLTBLTX(1,1,IROCK),IRLTBLT(2,1,IROCK) )
                CALL SPLINX( IRLTBLTX(1,1,IROCK),IRLTBLT(2,1,IROCK) )
              ENDIF
            ELSE
              IRLTBLT(ITX,2,IROCK) = NTBL
              T_OK = COPYIJK3D_INT( IRLTBLT,IRLTBLTX(ITX,1,IROCK),2,2, &
                3,ITX,IROCK )
              IF( IRPLX.EQ.11 .OR. IRPLX.EQ.13 .OR. IRPLX.EQ.15 ) THEN
                CALL SPLINY( IRLTBLT(ITX,1,IROCK),IRLTBLT(ITX,2,IROCK) )
                CALL SPLINX( IRLTBLT(ITX,1,IROCK),IRLTBLT(ITX,2,IROCK) )
              ENDIF
            ENDIF
          ENDIF
          GOTO 400
        ENDIF
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(N).EQ.1 .OR. ISCHR(N).EQ.13 .OR. &
         ISCHR(N).EQ.15 .OR. ISCHR(N).EQ.17 .OR. &
         ISCHR(N).EQ.101 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 1
            VARB = VARB(1:7) // 'Aqueous Relative Permeability'
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR. &
           INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'modified').NE.0 ) THEN
            IF( IOM.NE.1 .AND. IOM.NE.3 .AND. IOM.NE.36 ) IERR = 1
            IRPLX = 22
            VARB = 'Modified-Mualem Porosity Distribution Model'
            CALL MMVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'irreducible').NE.0 ) THEN
            IRPLX = 21
            VARB = 'Mualem-Irreducible Porosity Distribution Model'
            CALL MIVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Free Corey Relative Permeability Model'
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            CALL FC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Fatt and Klikoff Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( IERR.EQ.1 ) THEN
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    van Genuchten entrapment or Modified Lenhard
!       saturation functions  ---
!
        ELSEIF( ISCHR(N).EQ.8 .OR. &
         (ISCHR(N).GE.31 .AND. ISCHR(N).LE.34) ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 2
            VARB = 'Aqueous Relative Permeability'
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Brooks and Corey saturation function  ---
!
        ELSEIF( ISCHR(N).EQ.2 .OR. ISCHR(N).EQ.6 .OR. &
         ISCHR(N).EQ.14 .OR. ISCHR(N).EQ.16 .OR. &
         ISCHR(N).EQ.18 .OR. ISCHR(N).EQ.102 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR. &
           INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'modified').NE.0 ) THEN
            IF( IOM.NE.1 .AND. IOM.NE.3 .AND. IOM.NE.36 ) IERR = 1
            IRPLX = 22
            VARB = 'Modified-Mualem Porosity Distribution Model'
            CALL MMBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND. &
           INDEX(ADUM(1:),'irreducible').NE.0 ) THEN
            IRPLX = 21
            VARB = 'Mualem-Irreducible Porosity Distribution Model'
            CALL MIBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 1
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Corey Relative Permeability Model'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Fatt and Klikoff Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( IERR.EQ.1 ) THEN
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Brooks and Corey entrapment or Modified Lenhard
!       saturation functions  ---
!
        ELSEIF( ISCHR(N).EQ.6 .OR. &
         (ISCHR(N).GE.35 .AND. ISCHR(N).LE.38) ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 1
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Brooks and Corey w/ hysteresis saturation function  ---
!
        ELSEIF( ISCHR(N).EQ.302 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability (main drainage)'
            INDX = 1
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
            VARB = 'Aqueous Relative Permeability (main wetting)'
            INDX = 2
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR. &
           INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 1
            JNDX = 3
            VARB = 'Mualem Porosity Distribution (main drainage)'
            CALL MBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
            INDX = 2
            JNDX = 5
            VARB = 'Mualem Porosity Distribution (main wetting)'
            CALL MBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution (main drainage)'
            CALL BBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
            INDX = 1
            JNDX = 5
            VARB = 'Burdine Porosity Distribution (main wetting)'
            CALL BBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Dual Porosity van Genuchten function  ---
!
        ELSEIF( ISCHR(N).EQ.3 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Matrix Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
            VARB = 'Fracture Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Mualem Porosity Distribution Model'
            CALL MVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Burdine Porosity Distribution Model'
            CALL BVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Dual Porosity Brooks and Corey function  ---
!
        ELSEIF( ISCHR(N).EQ.4 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Matrix Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
            VARB = 'Fracture Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 1
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
            INDX = 2
            JNDX = 6
            VARB = 'Fracture Mualem Porosity Distribution Model'
            CALL MBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Burdine Porosity Distribution Model'
            CALL BBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Unknown saturation function  ---
!
        ELSE
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR. &
           INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Corey Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
              'Fatt and Klikoff Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: ' &
             // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---  Hysteretic saturation functions  ---
!
      ELSE
        IF( ITX.EQ.0 ) THEN
          VARB = 'Porosity Distribution Model'
        ELSE
          VARB = PREFIX // 'Porosity Distribution Model'
        ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:), 'mualem').NE.0 ) THEN
          IRPLX = 1
          IF(ME.EQ.0)WRITE(IWR,'(2X,A)') &
            'Mualem Porosity Distribution Model'
        ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
          IRPLX = 2
          IF(ME.EQ.0)WRITE(IWR,'(2X,A)') &
            'Burdine Porosity Distribution Model'
        ELSE
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Unrecognized Relative Perm. Function: ' &
           // ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Translate relative permeability type  ---
!
  400 CONTINUE
!      IF( IJK.GT.0 ) THEN
        IF( ITX.EQ.0 ) THEN
          DO 420 NX = 1,NUM_NODES
            IRPLT(1,NX) = IRPLX
            IRPLT(2,NX) = IRPLX
            IRPLT(3,NX) = IRPLX
  420     CONTINUE
        ELSE
          DO 422 NX = 1,NUM_NODES
            IRPLT(ITX,NX) = IRPLX
  422     CONTINUE
        ENDIF
!      ELSE
!        IF( ITX.EQ.0 ) THEN
!          IRPLTX(1) = IRPLX
!          IRPLTX(2) = IRPLX
!          IRPLTX(3) = IRPLX
!        ELSE
!          IRPLTX(ITX) = IRPLX
!        ENDIF
!      ENDIF
      ENDIF
      ENDDO
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( NN.LT.NROCK .AND.ME.EQ.0) WRITE(IWR,'(/)')
!
!---  Continue reading rock/soil type names for a pattern match  ---
!
!      IF( KBS.EQ.1 .AND. IROCK.LT.NROCK ) THEN
!        IROCK = IROCK + 1
!        ISTART = ISBS
!        GOTO 20
!      ENDIF
      GOTO 10
 500  CONTINUE
!
!---  End of RDLRPT group ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE C_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,ITX )
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
!     Constant aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/C_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF

      VARX = 1.0D0
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLTX(ITX,INDX)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF C_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE FC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
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
!     Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 4 November 2002.
!     Last Modified by MD White, PNNL, 4 November 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/FC_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF

      IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
        'Free Corey Relative Permeability Function'
      VARB = 'Endpoint Aqueous Relative Permeability'
      INDX = 1
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RPLTX(ITX,1)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
      VARB = 'Exponent Aqueous Relative Permeability'
      INDX = 2
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLTX(ITX,2)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
      VARB = 'Residual Aqueous Saturation'
      INDX = 3
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,3))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLTX(ITX,3)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
      VARB = 'Residual Gas Saturation'
      INDX = 4
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,4))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RPLTX(ITX,4)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF FC_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE HK_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
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
!     Haverkamp aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
#include "utils.h"
#include "gagrid.h"
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/HK_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF

      IF(ME.EQ.0)WRITE (IWR,'(2X,A)') &
         'Haverkamp Relative Permeability Function'
      VARB = 'Haverkamp (A)'
      INDX = 1
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLTX(ITX,1)
         T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
      VARB = 'Haverkamp (gamma)'
      INDX = 2
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
         RPLTX(ITX,2)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
      VARB = 'Haverkamp: Effective Air Entry Head'
      INDX = 3
      UNTS = 'm'
      IUNM = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,3))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
         UNTS(1:NCH),': ',RPLTX(ITX,3)
        INDX = 0
        CALL RDUNIT(UNTS,RPLTX(ITX,3),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (', &
          RPLTX(ITX,3),', m)'
        INDX = 3
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF HK_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
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
!     Rijtema-Gardner aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RG_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL

      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
      VARB = 'Rijtema-Gardner Modified Exponential Model'
      IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB
      VARB = 'Rijtema-Gardner: a Parameter'
      UNTS = '1/m'
      IUNM = -1
      INDC = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
         UNTS(1:NCH),': ',RPLTX(ITX,1)
        INDX = 0
        CALL RDUNIT(UNTS,RPLTX(ITX,1),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (', &
          RPLTX(ITX,1),', 1/m)'
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDC,4,ITX,IFLG )
      ENDIF
      VARB = 'Rijtema-Gardner: Effective Air Entry Head'
      UNTS = 'm'
      IUNM = 1
      INDC = 2
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
         UNTS(1:NCH),': ',RPLTX(ITX,2)
        INDX = 0
        CALL RDUNIT(UNTS,RPLTX(ITX,2),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (', &
          RPLTX(ITX,2),', m)'
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDC,4,ITX,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF RG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
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
!     Mualem-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE CONST
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MVG_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL

      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLT(ITX,INDX,N) = 1.D+0-1.D+0/SCHR(JNDX,N)
          IF( RPLT(ITX,INDX,N).LT.EPSL ) THEN
            INDX = 4
            CHMSG = 'Negative van Genuchten ''m'' Parameter'
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,INDX) = 1.D+0-1.D+0/SCHR(JNDX,N)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,INDX,N) = 1.D+0-1.D+0/SCHR(JNDX,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ', &
          RPLTX(ITX,INDX)
        IF( RPLTX(ITX,INDX).LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Negative van Genuchten ''m'' Parameter'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MVG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
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
!     Mualem-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MBC_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL

      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLT(ITX,INDX,N) = SCHR(JNDX,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,INDX) = SCHR(JNDX,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,INDX,N) = SCHR(JNDX,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ', &
         RPLTX(ITX,INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MBC_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
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
!     Burdine-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE CONST
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
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/BVG_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL

      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLT(ITX,INDX,N) = 1.D+0-2.D+0/SCHR(JNDX,N)
          IF( RPLT(ITX,INDX,N).LT.EPSL ) THEN
            INDX = 4
            CHMSG = 'Negative van Genuchten ''m'' Parameter'
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,INDX) = 1.D+0-2.D+0/SCHR(JNDX,N)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,INDX,N) = 1.D+0-2.D+0/SCHR(JNDX,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 2/n'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ', &
          RPLTX(ITX,INDX)
        IF( RPLTX(ITX,INDX).LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Negative van Genuchten ''m'' Parameter'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF BVG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,INDX,JNDX,ITX )
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
!     Burdine-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/BBC_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL

      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLT(ITX,INDX,N) = SCHR(JNDX,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,INDX) = SCHR(JNDX,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,INDX,N) = SCHR(JNDX,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ', &
         RPLTX(ITX,INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF BBC_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TV_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
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
!     Touma and Vauclin aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/TV_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) = &
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
      IF(ME.EQ.0)WRITE (IWR,'(2X,A)')'Touma and Vauclin Relative ' // &
       'Permeability Function'
      VARB = 'Touma and Vauclin (alpha)'
      INDX = 1
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
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',&
          RPLTX(ITX,1)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
      VARB = 'Touma and Vauclin (beta)'
      INDX = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        UNTS = 'null'
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        IF(ME.EQ.0)WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',&
          RPLTX(ITX,2)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF TV_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MIVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
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
!     Mualem Irreducible-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 May 2002.
!     Last Modified by MD White, PNNL, 21 May 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MIVG_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) =&
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL

!
!---  Read 'm' parameter  ---
!
      INDX = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLT(ITX,1,N) = 1.D+0-1.D+0/SCHR(3,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,1) = 1.D+0-1.D+0/SCHR(3,N)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,1,N) = 1.D+0-1.D+0/SCHR(3,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ', &
          RPLTX(ITX,1)
      ENDIF
!
!---  Read irreducible saturation  ---
!
      INDX = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLT(ITX,2,N) = SCHR(4,N)
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,2) = SCHR(4,N)
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,2,N) = SCHR(4,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'slr Parameter: ', &
          RPLTX(ITX,2)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MIVG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MIBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
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
!     Mualem Irreducible-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 May 2002.
!     Last Modified by MD White, PNNL, 21 May 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MIBC_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) =&
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL
!
!---  Read 'm' parameter  ---
!
      UNTS = 'null'
      INDX = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLT(ITX,1,N) = SCHR(3,N)
   10   CONTINUE
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,1) = SCHR(3,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,1,N) = SCHR(3,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',&
          RPLTX(ITX,1)
      ENDIF
!
!---  Read irreducible saturation  ---
!
      INDX = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLT(ITX,2,N) = SCHR(4,N)
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,2) = SCHR(4,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,2,N) = SCHR(4,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'slr Parameter: ', &
          RPLTX(ITX,2)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MIBC_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MMVG_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
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
!     Modified Mualem-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by MD White, PNNL, 17 December 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MMVG_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) =&
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read 'm' parameter  ---
!
      INDX = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLT(ITX,1,N) = 1.D+0-1.D+0/SCHR(3,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,1) = 1.D+0-1.D+0/SCHR(3,N)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,1,N) = 1.D+0-1.D+0/SCHR(3,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ', &
          RPLTX(ITX,1)
      ENDIF
!
!---  Read pore-scale parameter  ---
!
      INDX = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,NUM_NODES
          RPLT(ITX,2,N) = 5.D-1
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,2) = 5.D-1
        IDFLT = 1
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        IF( ICOMMA.EQ.ISTX ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(4X,A)') &
            'Default Value: 1/2 (Square Root)'
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'Pore-Scale Parameter: ',&
         RPLTX(ITX,2)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MMVG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MMBC_LRPT( ISTART,ICOMMA,N,IROCK,IJK,CHDUM,ITX )
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
!     Modified Mualem-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by MD White, PNNL, 17 December 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE GRID_MOD
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 UNTS,BDUM
      CHARACTER*512 CHDUM
      CHARACTER*64 T_FILENAME
      INTEGER LO(3),HI(3),LDXX(3)
      REAL*8 RPLTX(4,LRPL)
      LOGICAL ISBIN,ISHDF5,T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/MMBC_LRPT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(152)(1:1),'$').EQ.0 ) CVS_ID(152) =&
      '$Id: rdlrp.F,v 1.18 2008/01/02 18:46:01 d3c002 Exp $' 
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

      ISBIN = .FALSE.
      LNDX = LRPL
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read 'm' parameter  ---
!
      INDX = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 10 N = 1,NUM_NODES
          RPLT(ITX,1,N) = SCHR(3,N)
   10   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,1) = SCHR(3,N)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        IF( IJK > 0 )THEN
          DO N = 1,NUM_NODES
            RPLT(ITX,1,N) = SCHR(3,N)
          ENDDO
        ELSE
          T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IROCK )
        ENDIF
        IF(ME.EQ.0)WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',&
          RPLTX(ITX,1)
      ENDIF
!
!---  Read irreducible saturation  ---
!
      INDX = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IDFLT = 1
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        DO 20 N = 1,num_nodes
          RPLT(ITX,2,N) = 5.D-1
   20   CONTINUE
        UNTS = 'null'
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,4,ITX,&
          RPLT,VARX,ISBIN,ISHDF5 )
      ELSE
        RPLTX(ITX,2) = 5.D-1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        IF(ME.EQ.0)WRITE(IWR,'(4X,A,1PE11.4)') &
          'Pore-Scale Parameter: ',RPLTX(ITX,2)
        T_OK = COPYIJK3D( RPLT,RPLTX(ITX,INDX),LNDX,INDX,4,ITX,IFLG )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  END OF MMBC_LRPT GROUP  ---
!
      RETURN
      END

