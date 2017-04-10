!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDTF1
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
!     Reads solute/fluid interaction card for diffusion and partition
!     coefficients, and internodal diffusion term averaging scheme for
!     single phase (aqueous) solute transport equation.
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
      USE FILES
      USE CONST
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,UNTS
      CHARACTER*512 CHDUM
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SOLUTX
      LOGICAL T_OK
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!  309 READ(IRD,'(A)', END=311) CHDUM
!      IF( CHDUM(1:1).EQ.'#' ) GOTO 309
!      CALL LCASE( CHDUM )
!      IF( CHDUM(1:1).EQ.'~' .AND. &
!          INDEX(CHDUM(2:),'solute/fluid').NE.0 ) THEN
!        GOTO 409
!      ELSE
!        GOTO 309
!      ENDIF
!  311 CONTINUE
!      IF(IEQC.NE.0) THEN
!        INDX = 18
!        CHMSG = 'Missing Solute/Fluid Interactions Card'
!        CALL WRMSGS( INDX )
!      ENDIF
!  409 continue
      SUBNMX = '/RDTF1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  if(me.eq.0) WRITE card information to output file  ---
!
      CARD = 'Solute/Fluid Interaction Card'
      ICD = INDEX( CARD,'  ' )-1
      if(me.eq.0) WRITE(ISC,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of different solutes  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Solutes'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      NSOLU = 0
      IF( .NOT.ALLOCATED(IEDL) )THEN
        ALLOCATE(IEDL(1:LSOLU+LSPT))
      ENDIF
      IF( .NOT.ALLOCATED(SMDL) )THEN
        ALLOCATE(SMDL(1:LSOLU+LSPT))
      ENDIF
      IF( .NOT.ALLOCATED(IPCL) )THEN
        ALLOCATE( IPCL(1:LSOLU))
      ENDIF
      IF( .NOT.ALLOCATED(HLF) )THEN
        ALLOCATE(HLF(1:LSOLU+LSPT))
      ENDIF
      IF( .NOT.ALLOCATED(CHDF) )THEN
        ALLOCATE(CHDF(1:lsolu,1:LSOLU))
      ENDIF
      iedl = 1 
      smdl = 0.d0
      ipcl = 0
      hlf = 1.d20
      chdf = 0.d0
      DO 200 NL = 1, NLIN
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        ADUM(1:) = ' '
        VARB = 'Solute Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        DO 100 NSL = 1,NSOLU
          IF( SOLUT(NSL).EQ.ADUM ) GOTO 110
  100   CONTINUE
        NSOLU = NSOLU + 1
        IF( NSOLU.GT.LSOLU ) THEN
          INDX = 5
          CHMSG = 'Number of Solutes > Parameter LSOLU'
          CALL WRMSGS( INDX )
        ENDIF
        SOLUT(NSOLU) = ADUM
        NSL = NSOLU
  110   CONTINUE
        if(me.eq.0) WRITE(ISC,'(/,3A)') VARB(1:IVR),': ',ADUM
!
!---  Aqueous effective diffusion option  ---
!
        VARB = 'Aqueous Effective Diffusion Option: '
        ADUM = 'conventional'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        if(me.eq.0) WRITE( ISC,'(/,A,$)' ) VARB(1:IVR)
        IF( INDEX(ADUM(1:),'empirical').NE.0 )  THEN
          IEDL(NSL) = 2
          if(me.eq.0) WRITE( ISC,'(A)' ) 'Kemper and van Schaik Empirical Diffusion &
    Model'
          if(me.eq.0) WRITE( ISC,'(A)' ) '  Model Parameters Entered on the Solute/P &
    orous Media Interaction Card'
        ELSEIF( INDEX(ADUM(1:),'conventional').NE.0 )  THEN
          IEDL(NSL) = 1
          if(me.eq.0) WRITE( ISC,'(A)' ) 'Conventional Diffusion Model'
          VARB = 'Aqueous Molecular Diffusion Coefficient @ 20 C'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SMDL(NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
          ': ',SMDL(NSL)
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,SMDL(NSL),INDX)
          if(me.eq.0) WRITE(ISC,'(A,1PE11.4,A)') ' (',SMDL(NSL),', m^2/s)'
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 )  THEN
          IEDL(NSL) = 3
          if(me.eq.0) WRITE( ISC,'(A)' ) 'Constant Diffusion Model'
          VARB = 'Aqueous Molecular Diffusion Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SMDL(NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
          ': ',SMDL(NSL)
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,SMDL(NSL),INDX)
          if(me.eq.0) WRITE(ISC,'(A,1PE11.4,A)') ' (',SMDL(NSL),', m^2/s)'
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Aqueous Diffusion Option: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Solid-Aqueous Partition option  ---
!
        VARB = 'Solid-Aqueous Partition Option: '
        ADUM = 'continuous'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        if(me.eq.0) WRITE( ISC,'(/,A,$)' ) VARB(1:IVR)
        IF( INDEX(ADUM(1:),'noncontinuous').NE.0 )  THEN
          IF( INDEX(ADUM(1:),'concentration dependent').NE.0 ) THEN
            IPCL(NSL) = 4
            if(me.eq.0) WRITE( ISC,'(A)' ) 'Noncontinuous Solid Wetting / ' // &
            'Concentration Dependent'
          ELSE
            IPCL(NSL) = 2
            if(me.eq.0) WRITE( ISC,'(A)' ) 'Noncontinuous Solid Wetting'
          ENDIF
        ELSE
          IF( INDEX(ADUM(1:),'concentration dependent').NE.0 ) THEN
            IPCL(NSL) = 3
            if(me.eq.0) WRITE( ISC,'(A)' ) 'Continuous Solid Wetting / Concentration &
    Dependent'
          ELSE
            IPCL(NSL) = 1
            if(me.eq.0) WRITE( ISC,'(A)' ) 'Continuous Solid Wetting'
          ENDIF
        ENDIF

!
!---  Half-life  ---
!
          IDFLT = 1
          VARB = 'Radioactive Half-Life'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,HLF(NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
          ': ',HLF(NSL)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,HLF(NSL),INDX)
          if(me.eq.0) WRITE(ISC,'(A,1PE11.4,A)') ' (',HLF(NSL),', s)'
          HLF(NSL) = MAX( HLF(NSL),SMALL )
!
!---  Cut-off concentration for the Courant number limiter  ---
!
          IF( ISLC(17).EQ.2 ) THEN
            if(.not. allocated(ccl_crn)) allocate(CCL_CRN(1:LSOLU))
            IDFLT = 1
            VARB = 'Aqueous-Phase Cut-0ff Concentration'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CCL_CRN(NSL))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',CCL_CRN(NSL)
            INDX = 0
            IUNM = -3
            CALL RDUNIT(UNTS,CCL_CRN(NSL),INDX)
            if(me.eq.0) WRITE(ISC,'(A,1PE11.4,A)') ' (',CCL_CRN(NSL),', 1/m^3)'
          ENDIF
  200 CONTINUE
!
!---  Electrolyte density option  ---
!
      IF( ISLC(16).EQ.1 ) THEN
        NSL_ELC = 0
        DO 210 MSL = 1,NSOLU
          IF( SOLUT(MSL).EQ.ELC_SOL ) NSL_ELC = MSL
  210   CONTINUE
        IF( NSL_ELC.EQ.0 ) THEN
          NCH = INDEX( ELC_SOL(1:),'  ' )-1
          INDX = 4
          CHMSG = 'Electrolyte Solute Not Listed' // ELC_SOL(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Read number of lines of chain decay information  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Chain Decay Lines'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      IF( NLIN.GT.0 ) THEN
        if(me.eq.0) WRITE(ISC,'(/,A)') 'Chain Decay Fractions:'
      ENDIF
      DO 400 NL = 1, NLIN
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        ADUM(1:) = ' '
        VARB = 'Parent Solute Name'
        NPSL = 0
        CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
        DO 300 NSL = 1,NSOLU
          IF( SOLUT(NSL).EQ.ADUM ) NPSL = NSL
  300   CONTINUE
        BDUM(1:) = ' '
        VARB = 'Daughter Solute Name'
        NDSL = 0
        CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
        DO 310 NSL = 1,NSOLU
          IF( SOLUT(NSL).EQ.BDUM ) NDSL = NSL
  310   CONTINUE
        IF( NPSL.EQ.0 .OR. NDSL.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Invalid Chain Decay: '// &
          ADUM(1:NCHA)//': '//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ELSEIF( NPSL.EQ.NDSL ) THEN
          INDX = 4
          CHMSG = 'Invalid Chain Decay (Parent = Progeny): '// &
          ADUM(1:NCHA)//': '//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
        VARB = 'Chain Decay Fraction'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHDF(NPSL,NDSL))
        if(me.eq.0) WRITE(ISC,'(2X,5A,1PE11.4)') 'From ', &
        ADUM(1:NCHA),' to ',BDUM(1:NCHB),': ',CHDF(NPSL,NDSL)
  400 CONTINUE
      DO 420 NDSL = 1,NSOLU
        CHDFX = 0.D+0
        DO 410 NPSL = NDSL+1,NSOLU
          CHDFX = CHDFX + CHDF(NPSL,NDSL)
  410   CONTINUE
        IF( CHDFX.GE.1.D+0 ) THEN
          INDX = 4
          CHMSG = 'Chain Decay Fraction Summation'
          CALL WRMSGS( INDX )
        ENDIF
  420 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDTF1 group  ---
!
      RETURN
      END
