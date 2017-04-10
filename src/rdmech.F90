!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDMECH
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
!     Read input file for rock/soil mechanical information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, December 1992.
!     Last Modified by MD White, PNNL, December 31, 1992.
!     Last Modified by MD White, PNNL, 5 October 2001.
!     $Id: rdmech.F,v 1.12 2006/11/28 00:19:07 d3c002 Exp $
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
!--- PETSc
#include "include/finclude/petsc.h"

!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,CDUM,UNTS,T_FILENAME
      CHARACTER*512 CHDUM
      INTEGER :: DIM1,LO(3),HI(3),LDXX(3)
      DOUBLE PRECISION :: RHOSX, PORX(6), CMPX(3), TORX(6)
      LOGICAL T_OK,ISBIN,ISHDF5
!
!----------------------Common Blocks-----------------------------------!
!





!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDMECH'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(153)(1:1),'$').EQ.0 ) CVS_ID(153) = &
      '$Id: rdmech.F,v 1.12 2006/11/28 00:19:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Check for undefined rock/soil types in active nodes  ---
!
      CARD = 'Rock/Soil Zonation Card'
      DO 1 N = 1,num_nodes
        IF( IXP(N).EQ.0 ) GOTO 1
        IF( IZ(N).EQ.0 ) THEN
          INDX = 7
          IMSG = N
          CHMSG = 'Undefined Rock/Soil Type @ Node'
          CALL WRMSGS( INDX )
        ENDIF
    1 CONTINUE
!
!---  Write card information to ouput file  ---
!
      CARD = 'Rock/Soil Mechanical Properties Card'
      ICD = INDEX( CARD,'  ' )-1
      IF(ME.EQ.0) WRITE (ISC,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      IF(ME.EQ.0) WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
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
!---  Loop over the rock/soil mechanical information lines  ---
!
      ISBIN = .FALSE.
      NX = 0
      IJK = 0
      CHMLX = 0.D+0
      IDPX = 0
      CALL ADD_NODE_DFIELD('rhos', IDX)
      RHOS => D_ND_FLD(IDX)%P
      RHOS = 2650.d0
      DIM1 = 6
      CALL ADD_NODE_D2FIELD('por', DIM1, IDX)
      POR => D_ND_2FLD(IDX)%P
      POR = 0.D0
      DIM1 = 3
      CALL ADD_NODE_D2FIELD('cmp', DIM1, IDX)
      CMP => D_ND_2FLD(IDX)%P
      CMP = 0.d0
      CMP(1:2,:) = 1.d-7
      CMPX = 0.d0
      CMPX(1:2) = 1.d-7
      CALL ADD_NODE_IFIELD('idp', IDX)
      IDP => I_ND_FLD(IDX)%P
      IDP = 0
      CALL ADD_NODE_DFIELD('chml', IDX)
      CHML => D_ND_FLD(IDX)%P
      CHML = 0.d0
      CALL ADD_NODE_IFIELD('itor', IDX)
      ITOR => I_ND_FLD(IDX)%P
      ITOR = 0
      DIM1 = 6
      CALL ADD_NODE_D2FIELD('tor', DIM1, IDX)
      TOR => D_ND_2FLD(IDX)%P
      TOR = 0.d0
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
      DO 100 M = 1, NROCK
        IF( ADUM .EQ. ROCK(M)) THEN
          IROCK = M
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
      IF( ME.EQ.0 )WRITE (ISC,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      IF( ME.EQ.0 )WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      NX = NX + 1
  220 CONTINUE
      IF( IJK > 0 )THEN
        IFLG = -1
      ELSE
        IFLG = IROCK
      ENDIF
!
!---  Read particle density ---
!
      VARB = 'Particle Density'
      UNTS = 'kg/m^3'
      IUNM = -3
      IUNKG = 1
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
!       t_tb= MPI_Wtime(ierr)
!       print *,'begin rdijk time',t_tb
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK1D( T_FILENAME,LDXX,LO,HI,RHOS,VARX,ISBIN,ISHDF5 )
!       t_te= MPI_Wtime(ierr)
!       print *,'print rdijk time',t_te-t_tb
      ELSE
        IDFLT = 1
        RHOSX = 2650.D0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RHOSX)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0) WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
         ': ',RHOSX
        IF(ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
         ': ',RHOSX
        INDX = 0
        CALL RDUNIT(UNTS,RHOSX,INDX)
        IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',RHOSX,', kg/m^3)'
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',RHOSX,', kg/m^3)'
        T_OK = COPYIJK1D( RHOS,RHOSX,IFLG )
      ENDIF
!
!---  Read total and diffusive porosities ---
!
      VARB = 'Total Porosity'
      UNTS = 'null'
      INDC = 1
      LNDX = 6
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM,'file').NE.0 ) THEN
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,POR,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,PORX(1))
        IF(ME.EQ.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PORX(1)
        IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PORX(1)
        T_OK = COPYIJK2D( POR,PORX(1),LNDX,INDC,IFLG )
      ENDIF
      VARB = 'Diffusive Porosity'
      UNTS = 'null'
      INDC = 2
      ISTX = ISTART
      ICMX = ICOMMA
      CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM,'file').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
           INDEX(BDUM(1:),'bfile').NE.0 .OR. &
           INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
        T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
        VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
        T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,POR,VARX, &
          ISBIN,ISHDF5 )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,PORX(2))
        IF(ME.EQ.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PORX(2)
        IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PORX(2)
        T_OK = COPYIJK2D( POR,PORX(2),LNDX,INDC,IFLG )
      ENDIF
!
!---  Read fracture total and diffusive porosities 
!     for dual porosity soils
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR. &
       INDEX(ADUM(1:),'dp').NE.0 .OR. &
       INDEX(ADUM(1:),'dual').NE.0 ) THEN
        VARB = 'Fracture Total Porosity'
        UNTS = 'null'
        INDC = 3
        LNDX = 6
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,POR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PORX(3))
          IF(ME.EQ.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PORX(3)
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PORX(3)
          T_OK = COPYIJK2D( POR,PORX(3),LNDX,INDC,IFLG )
        ENDIF
        VARB = 'Fracture Diffusive Porosity'
        UNTS = 'null'
        INDC = 4
        LNDX = 6
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,POR,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PORX(4))
          IF(ME.EQ.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PORX(4)
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PORX(4)
          T_OK = COPYIJK2D( POR,PORX(4),LNDX,INDC,IFLG )
        ENDIF
      ENDIF
!
!---  Read specific storativity and convert to compressibility ---
!       or read bulk compressibility and reference pressure,
!       or read pore compressibility and reference pressure  ---
!
      ISTARTX = ISTART
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
      IF( INDEX(BDUM(1:),'compressibility').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'pore').NE.0 ) THEN
          ISLC(15) = 1
          IF( IDPX.EQ.0 ) THEN
            VARB = 'Pore Compressibility'
          ELSE
            VARB = 'Pore Matrix Compressibility'
          ENDIF
        ELSE
          IF( IDPX.EQ.0 ) THEN
            VARB = 'Bulk Compressibility'
          ELSE
            VARB = 'Bulk Matrix Compressibility'
          ENDIF
        ENDIF
        UNTS = '1/pa'
        IUNM = 1
        IUNKG = -1
        IUNS = 2
        INDC = 1
        LNDX = 3
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,CMP,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,CMPX(1))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
           ': ',CMPX(1)
          IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
           ': ',CMPX(1)
          INDX = 0
          CALL RDUNIT(UNTS,CMPX(1),INDX)
          IF( ME.EQ.0 )WRITE(ISC,'(A,1PE11.4,A)') ' (',CMPX(1),', 1/Pa)'
          IF( ME.EQ.0 )WRITE(IWR,'(A,1PE11.4,A)') ' (',CMPX(1),', 1/Pa)'
          T_OK = COPYIJK2D( CMP,CMPX(1),LNDX,INDC,IFLG )
        ENDIF
!
!---      Read fracture compressibility ---
!
        IF( IDPX.NE.0 ) THEN
          IF( ISLC(15).EQ.1 ) THEN
            VARB = 'Pore Fracture Compressibility'
          ELSE
            VARB = 'Bulk Fracture Compressibility'
          ENDIF
          UNTS = '1/pa'
          IUNM = 1
          IUNKG = -1
          IUNS = 2
          INDC = 2
          LNDX = 4
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,CMP,VARX, &
              ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CMP(2,IROCK))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ.0 )WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',CMP(2,IROCK)
            INDX = 0
            CALL RDUNIT(UNTS,CMP(2,IROCK),INDX)
            IF( ME.EQ.0 )WRITE(IWR,'(A,1PE11.4,A)') ' (', &
              CMP(2,IROCK),', 1/Pa)'
            T_OK = COPYIJK2D( CMP,CMP(2,IROCK),LNDX,INDC,IFLG )
          ENDIF
        ENDIF
        VARB = 'Bulk Compressibility Reference Pressure'
        UNTS = 'Pa'
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        INDC = 3
        LNDX = 3
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,CMP,VARX, &
            ISBIN,ISHDF5 )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,CMPX(3))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
           ': ',CMPX(3)
          IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
           ': ',CMPX(3)
          INDX = 0
          IUNM = -1
          CALL RDUNIT(UNTS,CMPX(3),INDX)
          IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',CMPX(3),', Pa)'
          IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',CMPX(3),', Pa)'
          T_OK = COPYIJK2D( CMP,CMPX(3),LNDX,INDC,IFLG )
        ENDIF
      ELSE
        ISTART = ISTARTX
        VARB = 'Specific Storage'
        UNTS = '1/m'
        IUNM = -1
        INDC = 1
        LNDX = 3
        ISTX = ISTART
        ICMX = ICOMMA
        CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
             INDEX(BDUM(1:),'bfile').NE.0 .OR. &
             INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          DO 300 IROCK = 1,num_nodes
            PORDX = POR(4,IROCK) + (1.D+0-POR(4,IROCK))*POR(2,IROCK)
            CMP(1,IROCK) = (CMP(1,IROCK)+PORDX*4.591D-10)*RHORL*GRAV
            IF( ISLC(9).EQ.1 ) CMP(1,IROCK) = CMP(1,IROCK)*RHORL*GRAV
  300     CONTINUE
          T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
          VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
          T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,CMP,VARX, &
            ISBIN,ISHDF5 )
          DO 310 IROCK = 1,NUM_NODES
            CMP(1,IROCK) = MAX( CMP(1,IROCK)/(RHORL*GRAV) &
             -POR(2,IROCK)*4.591D-10,ZERO )
            IF( ISLC(9).EQ.1 ) CMP(1,IROCK) = MAX( CMP(1,IROCK)/ &
             (RHORL*GRAV),ZERO )
  310     CONTINUE
        ELSE
          PORDX = PORX(4) + (1.D+0-PORX(4))*PORX(2)
          VAR = (CMPX(1)+PORDX*4.591D-10)*RHORL*GRAV
          IF( ISLC(9).EQ.1 ) VAR = CMPX(1)*RHORL*GRAV
          IDFLT = 1
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',VAR
          IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
           UNTS(1:NCH),': ',VAR
          INDX = 0
          IUNM = -1
          CALL RDUNIT(UNTS,VAR,INDX)
          IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',VAR,', 1/m)'
          IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', 1/m)'
          CMPX(1) = MAX( VAR/(RHORL*GRAV) &
           -PORX(2)*4.591D-10,ZERO )
          IF( ISLC(9).EQ.1 ) CMPX(1) = &
           MAX( VAR/(RHORL*GRAV),0.D+0 )
            IF(ME.EQ.0)WRITE(ISC,'(2X,A,1PE11.4)') &
              'Rock/Soil Compressibility' // ', 1/Pa: ',CMPX(1)
            IF(ME.EQ.0)WRITE(IWR,'(2X,A,1PE11.4)') &
              'Rock/Soil Compressibility' // ', 1/Pa: ',CMPX(1)
            T_OK = COPYIJK2D( CMP,CMPX(1),LNDX,INDC,IFLG )
        ENDIF
      ENDIF
!
!---      Read fracture specific storage  ---
!
        IF( INDEX(ADUM(1:),'dp').NE.0 ) THEN
          IDPX = 1
          INDC = 1
          LNDX = 4
          IF( INDEX(ADUM(1:),'pseudo').NE.0 ) IDPX = -1
          VARB = 'Fracture Specific Storage'
          IDFLT = 1
          UNTS = '1/m'
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            DO 320 IROCK = 1,NUM_NODES
             PORDX = POR(4,IROCK)
             CMP(2,IROCK) = (CMP(2,IROCK)+PORDX*4.591D-10)*RHORL*GRAV
             IF( ISLC(9).EQ.1 ) CMP(2,IROCK) = CMP(2,IROCK)*RHORL*GRAV
  320       CONTINUE
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,CMP,VARX, &
              ISBIN,ISHDF5 )
            DO 330 IROCK = 1,NFLD
              CMP(2,IROCK) = MAX( CMP(2,IROCK)/(RHORL*GRAV) &
                -PORDX*4.591D-10,ZERO )
              IF( ISLC(9).EQ.1 ) CMP(2,IROCK) = MAX( CMP(2,IROCK)/ &
                (RHORL*GRAV),ZERO )
  330       CONTINUE
          ELSE
            PORDX = POR(4,IROCK)
            VAR = (CMP(2,IROCK)+PORDX*4.591D-10)*RHORL*GRAV
            IF( ISLC(9).EQ.1 ) VAR = CMP(2,IROCK)*RHORL*GRAV
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',VAR
            IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',VAR
            INDX = 0
            IUNM = -1
            CALL RDUNIT(UNTS,VAR,INDX)
            IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',VAR,', 1/m)'
            IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', 1/m)'
            CMP(2,IROCK) = MAX( VAR/(RHORL*GRAV) &
              - PORX(2)*4.591D-10,ZERO )
            IF( ISLC(9).EQ.1 ) CMPX(2) = MAX( VAR/(RHORL*GRAV),ZERO )
            IF(ME.EQ.0)WRITE(ISC,'(2X,A,1PE11.4)') &
               'Fracture Compressibility, 1/Pa: ',CMPX(2)
            IF(ME.EQ.0)WRITE(IWR,'(2X,A,1PE11.4)') &
               'Fracture Compressibility, 1/Pa: ',CMPX(2)
            T_OK = COPYIJK2D( CMP,CMP(2,IROCK),LNDX,INDC,IFLG )
          ENDIF
        ENDIF
!
!---      Read compressibility reference pressure ---
!
        ISTARTX = ISTART
        CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
        CDUM = 'null'
        IF( INDX.EQ.1 ) CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,CDUM)
        INDC = 3
        LNDX = 4
        IF( INDEX(CDUM(1:),'reference').NE.0 ) THEN
          VARB = 'Compressibility Reference Pressure'
          UNTS = 'pa'
          IUNM = -1
          IUNKG = 1
          IUNS = -2
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,CMP,VARX, &
              ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CMP(3,IROCK))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',CMP(3,IROCK)
            INDX = 0
            CALL RDUNIT(UNTS,CMP(3,IROCK),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',CMP(3,IROCK),', Pa)'
            T_OK = COPYIJK2D( CMP,CMP(3,IROCK),LNDX,INDC,IFLG )
          ENDIF
        ELSE
          ISTART = ISTARTX
        ENDIF
!
!---    Read fracturing pressure gradient  ---
!
        IF( IDPX.EQ.50 ) THEN
          VARB = 'Fracturing Pressure Gradient'
          UNTS = 'pa/m'
          IUNM = -2
          IUNKG = 1
          IUNS = -2
          INDC = 4
          LNDX = 4
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
               INDEX(BDUM(1:),'bfile').NE.0 .OR. &
               INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,CMP,VARX, &
              ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CMP(4,IROCK))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ.0 )WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',CMP(4,IROCK)
            INDX = 0
            CALL RDUNIT(UNTS,CMP(4,IROCK),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',CMP(4,IROCK),', Pa/m)'
            T_OK = COPYIJK2D( CMP,CMP(4,IROCK),LNDX,INDC,IFLG )
          ENDIF
        ENDIF
!
!---    Read matrix characteristic length ---
!
        IF( IDPX.EQ.-1 ) THEN
          VARB = 'Matrix Characteristic Length'
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
            T_OK = RDIJK1D( T_FILENAME,LDXX,LO,HI,CHML,VARX,ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CHMLX)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF(ME.EQ.0)WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
             UNTS(1:NCH),': ',CHMLX
            IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
             UNTS(1:NCH),': ',CHMLX
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,CHMLX,INDX)
            IF(ME.EQ.0)WRITE(ISC,'(A,1PE11.4,A)') ' (',CHMLX,', m)'
            IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',CHMLX,', m)'
            T_OK = COPYIJK1D( CHML,CHMLX,IFLG )
          ENDIF
        ENDIF
!
!---  Read aqueous, gas, and napl tortuosities if vapor diffusion or
!     the transport algorithms are active ---
!
        IF( ISLC(3) .EQ. 1 ) THEN
          VARB = 'Tortuosity Function'
          IDFLT = 1
          ADUM = 'millington and quirk'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(ADUM(1:),'constant-aqueous').NE.0 .AND. &
            INDEX(ADUM(1:),'millington-gas').NE.0 ) THEN
            DO 340 IROCK = 1,NUM_NODES
              ITOR(IROCK) = 5
  340       CONTINUE
            IF(ME.EQ.0)WRITE(ISC,'(2X,A,1PE11.4)') &
              'Constant Aqueous Tortuosity'
            IF(ME.EQ.0)WRITE(IWR,'(2X,A,1PE11.4)') &
              'Constant Aqueous Tortuosity'
            VARB = 'Aqueous-Phase Tortuosity'
            UNTS = 'null'
            INDC = 1
            LNDX = 6
            ISTX = ISTART
            ICMX = ICOMMA
            CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
            IF( INDEX(BDUM,'file').NE.0 ) THEN
              IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
                 INDEX(BDUM(1:),'bfile').NE.0 .OR. &
                 INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,TOR,VARX, &
                ISBIN,ISHDF5 )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,TORX(1))
              IF(ME.EQ.0)WRITE(ISC,'(4X,2A,1PE11.4)') &
                VARB(1:IVR),': ',TORX(1)
              IF(ME.EQ.0)WRITE(IWR,'(4X,2A,1PE11.4)') &
                VARB(1:IVR),': ',TORX(1)
              T_OK = COPYIJK2D( TOR,TORX(1),LNDX,INDC,IFLG )
            ENDIF
            IF(ME.EQ.0)WRITE(ISC,'(2X,2A)') &
              'Millington and Quirk Gas Tortuosity '
            IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') &
              'Millington and Quirk Gas Tortuosity '
          ELSEIF( INDEX(ADUM(1:),'constant') .NE. 0 ) THEN
            DO 350 IROCK = 1,NUM_NODES
              ITOR(IROCK) = 1
  350       CONTINUE
            IF(ME.EQ.0)WRITE(ISC,'(2X,A,1PE11.4)') &
              'Constant Tortuosity Function'
            IF(ME.EQ.0)WRITE(IWR,'(2X,A,1PE11.4)') &
              'Constant Tortuosity Function'
            IF( IAQU.EQ.1 ) THEN
              VARB = 'Aqueous-Phase Tortuosity'
              UNTS = 'null'
              INDC = 1
              LNDX = 6
              ISTX = ISTART
              ICMX = ICOMMA
              CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
              IF( INDEX(BDUM,'file').NE.0 ) THEN
                IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
                   INDEX(BDUM(1:),'bfile').NE.0 .OR. &
                   INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
                T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
                VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
                T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,TOR,VARX, &
                  ISBIN,ISHDF5 )
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TORX(1))
                IF(ME.EQ.0)WRITE(ISC,'(4X,2A,1PE11.4)') VARB(1:IVR),': ', &
                  TORX(1)
                IF(ME.EQ.0)WRITE(IWR,'(4X,2A,1PE11.4)') VARB(1:IVR),': ', &
                  TORX(1)
                T_OK = COPYIJK2D( TOR,TORX(1),LNDX,INDC,IFLG )
              ENDIF
            ENDIF
            IF( IGAS.EQ.1 ) THEN
              VARB = 'Gas-Phase Tortuosity'
              UNTS = 'null'
              INDC = 2
              LNDX = 6
              ISTX = ISTART
              ICMX = ICOMMA
              CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
              IF( INDEX(BDUM,'file').NE.0 ) THEN
                IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
                   INDEX(BDUM(1:),'bfile').NE.0 .OR. &
                   INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
                T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
                VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
                T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,TOR,VARX, &
                  ISBIN,ISHDF5 )
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TORX(2))
                IF(ME.EQ.0)WRITE(ISC,'(4X,2A,1PE11.4)') VARB(1:IVR),': ', &
                 TORX(2)
                IF(ME.EQ.0)WRITE(IWR,'(4X,2A,1PE11.4)') VARB(1:IVR),': ', &
                 TORX(2)
                T_OK = COPYIJK2D( TOR,TORX(1),LNDX,INDC,IFLG )
              ENDIF
            ENDIF
          ELSEIF( INDEX(ADUM(1:),'free gas').NE.0 ) THEN
            DO 360 IROCK = 1,num_nodes
              ITOR(IROCK) = 3
  360       CONTINUE
            IF(ME.EQ.0)WRITE(ISC,'(2X,2A)') &
              'Millington and Quirk Tortuosity (Free Gas) Function'
            IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') &
              'Millington and Quirk Tortuosity (Free Gas) Function'
          ELSEIF( INDEX(ADUM(1:),'millington').NE.0 .OR. &
                 INDEX(ADUM(1:),'mq').NE.0 ) THEN
            DO 370 IROCK = 1,NUM_NODES
              ITOR(IROCK) = 2
  370       CONTINUE
            IF(ME.EQ.0)WRITE(ISC,'(2X,2A)') &
              'Millington and Quirk Tortuosity Function'
            IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') &
              'Millington and Quirk Tortuosity Function'
          ELSEIF( INDEX(ADUM(1:),'marshall').NE.0 ) THEN
            DO 380 IROCK = 1,NUM_NODES
              ITOR(IROCK) = 4
  380       CONTINUE
            IF(ME.EQ.0)WRITE(ISC,'(2X,2A)') 'Marshall Tortuosity ', &
             'Function'
            IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') 'Marshall Tortuosity ', &
             'Function'
          ELSEIF( INDEX(ADUM(1:),'archie law') .NE. 0 ) THEN
            DO 450 IROCK = 1,NUM_NODES
              ITOR(IROCK) = 6
  450       CONTINUE
            IF(ME.EQ.0)WRITE(ISC,'(2X,A,1PE11.4)') &
              "Archie's Tortuosity Function"
            IF(ME.EQ.0)WRITE(IWR,'(2X,A,1PE11.4)') &
              "Archie's Tortuosity Function"
            IF( IAQU.EQ.1 ) THEN
              VARB = 'Aqueous-Phase Tortuosity'
              UNTS = 'null'
              INDC = 1
              LNDX = 6
              ISTX = ISTART
              ICMX = ICOMMA
              CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
              IF( INDEX(BDUM,'file').NE.0 ) THEN
                IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
                   INDEX(BDUM(1:),'bfile').NE.0 .OR. &
                   INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
                T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
                VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
                T_OK = RDIJK2D( T_FILENAME,LDXX,LO,HI,LNDX,INDC,TOR,VARX, &
                  ISBIN,ISHDF5 )
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TORX(1))
                IF(ME.EQ.0)WRITE(ISC,'(4X,2A,1PE11.4)') VARB(1:IVR),': ', &
                  TORX(1)
                IF(ME.EQ.0)WRITE(IWR,'(4X,2A,1PE11.4)') VARB(1:IVR),': ', &
                  TORX(1)
                T_OK = COPYIJK2D( TOR,TORX(1),LNDX,INDC,IFLG )
              ENDIF
            ENDIF
          ELSE
            INDX = 4
            CHMSG = 'Unrecognized Tortuosity Function: ' // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF

!
!---  Read next rock/soil type ---
!
        IF( NX.LT.NROCK .and.me.eq.0) WRITE(ISC,'(/)')
        IF( NX.LT.NROCK .and.me.eq.0) WRITE(IWR,'(/)')
        GOTO 10
 500  CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDMECH group ---
!
      RETURN
      END
