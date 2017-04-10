!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDTP1
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
!     Reads the solute/porous media interaction card for the
!     dispersivities, half-lives, and partition coefficients.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
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
#include "utils.h"
!
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,RDUM,UNTS,T_FILENAME
      CHARACTER*512 CHDUM
      INTEGER NCH
      INTEGER :: DIM1, DIM2, LO(3), HI(3), LDXX(3)
      DOUBLE PRECISION :: DISPLX, DISPTX
      DOUBLE PRECISION :: SDCLX(3,LSOLU+LSPT)
      DOUBLE PRECISION :: SMDEFX(LSOLU+LSPT)
      DOUBLE PRECISION :: PCSLX(5,LSOLU)
      INTEGER :: IPCSLX(LSOLU)
      LOGICAL T_OK,ISBIN,ISHDF5
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDTP1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Write card information to output file  ---
!
      CARD = 'Solute/Porous Media Interaction Card'
      ICD = INDEX( CARD,'  ' )-1
      IF(ME.EQ.0) WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      IDISP = 0
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
      CALL ADD_NODE_DFIELD('displ', IDX)
      DISPL => D_ND_FLD(IDX)%P
      DISPL = 0.D0
      CALL ADD_NODE_DFIELD('dispt', IDX)
      DISPT => D_ND_FLD(IDX)%P
      DISPT = 0.d0
      DIM1 = 3
      DIM2 = LSOLU+LSPT
      CALL ADD_NODE_D3FIELD('sdcl', DIM1, DIM2, IDX)
      SDCL => D_ND_3FLD(IDX)%P
      SDCL = 0.D0
      SDCLX = 0.D0
      DIM1 = LSOLU+LSPT
      CALL ADD_NODE_D2FIELD('smdef', DIM1, IDX)
      SMDEF => D_ND_2FLD(IDX)%P
      SMDEF = 1.D0
      SMDEFX = 1.D0
      DIM1 = LSOLU
      CALL ADD_NODE_I2FIELD('ipcsl', DIM1, IDX)
      IPCSL => I_ND_2FLD(IDX)%P
      IPCSL = 0
      DIM1 = 5
      DIM2 = LSOLU
      CALL ADD_NODE_D3FIELD('pcsl', DIM1, DIM2, IDX)
      PCSL => D_ND_3FLD(IDX)%P
      PCSL = 0.D0
      N = 0
      IJK = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 600
      ISTART = 1
      VARB = 'Rock Name: '
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  IJK indexing (property read from file)  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IJK = 1
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
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
      CALL WRMSGS( INDX )
      GOTO 10
  200 CONTINUE
!
!---  Write rock/soil name  ---
!
      IF( ME.EQ.0 ) WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      N = N + 1
  220 CONTINUE
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Longitudinal dispersivity  ---
!
      VARB = 'Longitudinal Dispersivity: '
      IF( INDEX(CHDUM,'file').NE.0 ) THEN
        UNTS = 'm'
        IUNM = 1
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK1D( T_FILENAME,LDXX,LO,HI,DISPL,VARX,ISBIN,ISHDF5 )
        IDISP = 1
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,DISPLX)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF( ME.EQ.0 )WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',DISPLX
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,DISPLX,INDX)
        IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',DISPLX,', m)'
        IF( DISPLX.GE.SMALL ) IDISP = 1
        T_OK = COPYIJK1D( DISPL,DISPLX,IFLG )
      ENDIF
!
!---  Transverse dispersivity  ---
!
      VARB = 'Transverse Dispersivity: '
      IF( INDEX(CHDUM,'file').NE.0 ) THEN
        UNTS = 'm'
        IUNM = 1
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK1D( T_FILENAME,LDXX,LO,HI,DISPT,VARX,ISBIN,ISHDF5 )
        IDISP = 1
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,DISPTX)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),  &
       ': ',DISPTX
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,DISPTX,INDX)
        if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',DISPTX,', m)'
        IF( DISPTX.GE.SMALL ) IDISP = 1
        T_OK = COPYIJK1D( DISPT,DISPTX,IFLG )
      ENDIF
!
!---  Loop over number of solutes or radionuclides  ---
!
      DO 500 NS = 1,NSOLU
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Solute Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Search known solutes for matching name  ---
!
        DO 300 NSL = 1,NSOLU
          IF( ADUM.EQ.SOLUT(NSL)) GOTO 400
  300   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Solute Name: '//ADUM
        CALL WRMSGS( INDX )
  400   CONTINUE
        IF(ME.EQ.0) WRITE(IWR,'(/,2A)') 'Solute Name:',SOLUT(NSL)
!
!---    Solid-aqueous partition coefficient  ---
!
        IDFLT = 1
        VARB = 'Solid-Aqueous Partition Coefficient: '
        ICX = 1
        LCX = 5
        IF( INDEX(CHDUM,'file').NE.0 ) THEN
          UNTS = 'm^3/kg'
          IUNKG = -1
          IUNM = 3
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,NSOLU,NSL,LCX,ICX, &
            PCSL,VARX,ISBIN,ISHDF5 )
          DO 402 IROCK = 1,NUM_NODES
            PCSL(1,NSL,IROCK) = MAX( PCSL(1,NSL,IROCK),1.D-20 )
  402     CONTINUE
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PCSLX(1,NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4)') &
            VARB(1:IVR),', ',UNTS (1:NCH),': ',PCSLX(1,NSL)
          INDX = 0
          IUNKG = -1
          IUNM = 3
          CALL RDUNIT(UNTS,PCSLX(1,NSL),INDX)
          IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
          PCSLX(1,NSL),', m^3/kg)'
          PCSLX(1,NSL) = MAX( PCSLX(1,NSL),1.D-20 )
          T_OK = COPYIJK3D( PCSL,PCSLX(1,NSL),NSOLU,NSL,LCX,ICX,IFLG )
        ENDIF
        IF( IPCL(NSL).EQ.3 .OR. IPCL(NSL).EQ.4 ) THEN
          VARB = 'Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 410 NSS = 1,NSOLU
            IF( BDUM.EQ.SOLUT(NSS)) GOTO 420
  410     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Solute Name: '//ADUM
          CALL WRMSGS( INDX )
  420     CONTINUE
          IF(ME.EQ.0) WRITE(IWR,'(/,2A)') 'Dependent Solute Name:',SOLUT(NSS)
          IPCSLX(NSL) = NSS
          T_OK = COPYIJK2D( IPCSL,IPCSLX(2),NSOLU,NSL,IFLG )
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PCSLX(2,NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          VAR = 1.D+0
          INDX = 0
          IUNKG = -1
          IUNM = 3
          CALL RDUNIT(UNTS,VAR,INDX)
          PCSLX(2,NSL) = PCSLX(2,NSL) + LOG10(VAR)
          T_OK = COPYIJK3D( PCSL,PCSLX(2,NSL),NSOLU,NSL,5,2,IFLG )
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PCSLX(3,NSL))
          T_OK = COPYIJK3D( PCSL,PCSLX(3,NSL),NSOLU,NSL,5,3,IFLG )
        ENDIF
!
!---    van Schaik and Kemper Empirical Aqueous Diffusion Model  ---
!
        IF( IEDL(NSL).EQ.2 ) THEN
          LNDX = LSOLU+LSPT
          VARB = 'Aqueous Molecular Diffusion Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCLX(1,NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
          ': ',SDCLX(1,NSL)
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,SDCLX(1,NSL),INDX)
          IF(ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',  &
            SDCLX(1,NSL),', m^2/s)'
          T_OK = COPYIJK3D( SDCL,SDCLX(1,NSL),LNDX,NSL,3,1,IFLG )
          VARB = 'a Constant'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCLX(2,NSL))
          IF(ME.EQ.0) WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',SDCLX(2,NSL)
          T_OK = COPYIJK3D( SDCL,SDCLX(2,NSL),LNDX,NSL,3,2,IFLG )
          VARB = 'b Constant'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCLX(3,NSL))
          IF(ME.EQ.0) WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',SDCLX(3,NSL)
          T_OK = COPYIJK3D( SDCL,SDCLX(3,NSL),LNDX,NSL,3,3,IFLG )
        ENDIF
!
!---    Macrodispersivity enhancement factor  ---
!
        CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          LNDX = LSOLU+LSPT
          VARB = 'Macrodispersivity Enhancement Factor'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SMDEFX(NSL))
          IF(ME.EQ.0) WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',SMDEFX(NSL)
          T_OK = COPYIJK2D( SMDEF,SMDEFX(NSL),LNDX,NSL,IFLG )
        ENDIF
  500 CONTINUE
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( N.LT.NROCK .and.me.eq.0) WRITE(IWR,'(/)')
      GOTO 10
  600 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDTP1 group  ---
!
      RETURN
      END
