!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDAQSP
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
!     Read aqueous species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 December 2004.
!     Last Modified by MD White, PNNL, 7 December 2004.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
      USE TRNSPT
      USE GRID_MOD
      USE BUFFEREDREAD
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
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,RDUM,UNTS
      CHARACTER*512 CHDUM
      CHARACTER*32 :: T_STRING
      INTEGER :: IDX, DIM1,DIM2
      LOGICAL :: T_OK
!
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDAQSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
      '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      IF(.NOT.ALLOCATED(IEDL)) THEN
        ALLOCATE(IEDL(1:LSOLU+LSPT))
        IEDL = 1
      ENDIF
      IF(.NOT.ALLOCATED(SMDL)) THEN
        ALLOCATE(SMDL(1:LSOLU+LSPT))
        SMDL = 0.d0
      ENDIF
      IF(.NOT.ALLOCATED(HLF)) THEN
        ALLOCATE(HLF(1:LSOLU+LSPT))
        HLF = 1.D20
      ENDIF
!      if(.not.allocated(sdcl)) allocate( sdcl(1:3,1:lrc,1:lsolu+lspt))
!      if(.not.allocated(smdef)) allocate( smdef(1:lrc,1:lsolu+lspt))
      T_OK = .FALSE.
      T_STRING = 'SDCL'
      I = 0
      DO WHILE (I.LT.DNODE_3FIELD.AND.(.NOT.T_OK))
        I = I + 1
        NLEN = LEN_TRIM(D_ND_3FLD_NAMES(I))
        SLEN = LEN_TRIM(T_STRING)
        IF (T_STRING(1:SLEN).EQ.D_ND_3FLD_NAMES(I)(1:NLEN)) THEN
          IDX = I
          T_OK = .TRUE.
        ENDIF
      ENDDO
      IF( T_OK .EQ. .FALSE. ) THEN
        DIM1 = 3
        DIM2 = LSOLU+LSPT
        CALL ADD_NODE_D3FIELD('SDCL', DIM1, DIM2, IDX)
        SDCL => D_ND_3FLD(IDX)%P
        SDCL = 0.D0
        DIM1 = LSOLU+LSPT
        CALL ADD_NODE_D2FIELD('SMDEF', DIM1, IDX)
        SMDEF => D_ND_2FLD(IDX)%P
        SMDEF = 1.D0
      endif
      CARD = 'Aqueous Species Card'
      ICD = INDEX( CARD,'  ' )-1
      IF(ME.EQ.0)WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of aqueous species  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Aqueous Species'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPL )
      IF( NSPL.GT.LSPL ) THEN
        INDX = 5
        CHMSG = 'Number of Aqueous Species > Parameter LSPL'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Check for molecular diffusion option  ---
!
      VARB = 'Aqueous Species Molecular Diffusion Option'
      IVR = INDEX( VARB,'  ')-1
      CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.0 ) THEN
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'conventional').NE.0 ) THEN
          ISP_IEDL = 1
          IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ', &
           'Conventional'
        ELSEIF( INDEX(ADUM(1:),'empirical').NE.0 ) THEN
          ISP_IEDL = 2
          IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ', &
            'Power Function'
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
          ISP_IEDL = 3
          IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ','Constant'
        ELSEIF( INDEX(ADUM(1:),'power').NE.0 ) THEN
          ISP_IEDL = 4
          IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ', &
           'Power Function'
        ENDIF
      ELSE
        ISP_IEDL = 1
        IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ','Conventional'
      ENDIF
!
!---  Read aqueous molecular diffusion coefficient  ---
!
      VARB = 'Aqueous Species Molecular Diffusion Coefficient'
      IVR = INDEX( VARB,'  ')-1
      CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_MDL)
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',SP_MDL
      INDX = 0
      IUNM = 2
      IUNS = -1
      CALL RDUNIT(UNTS,SP_MDL,INDX)
!
!---  Power Function or van Schaik and Kemper Empirical 
!     Aqueous Diffusion Models  ---
!
      IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
        IF(.NOT.ALLOCATED(SP_SDCL)) THEN
          ALLOCATE(SP_SDCL(3))
          SP_SDCL = 0.D+0
        ENDIF
        SP_SDCL(1) = SP_MDL
        VARB = 'a Constant'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_SDCL(2))
        IF(ME.EQ.0)WRITE(IWR,'(4x,2A,1PE11.4)') VARB(1:IVR),': ',SP_SDCL(2)
        VARB = 'b Constant'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_SDCL(3))
        IF(ME.EQ.0)WRITE(IWR,'(4x,2A,1PE11.4)') VARB(1:IVR),': ',SP_SDCL(3)
      ENDIF
!
!---  Check for activity coefficient option  ---
!
      CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Activity Coefficient Option: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'davies').NE.0 ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Davies Equation'
          IACTV = 1
        ELSEIF( INDEX(ADUM(1:),'pitzer').NE.0 ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Pitzer Equation'
          IACTV = 2
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Constant'
          IACTV = 3
          VARB = 'Constant Activity Coefficient'
          IDFLT = 1
          ACTVC = 1.D+0
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ACTVC)
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
             ': ',ACTVC
        ELSE
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') VARB(1:IVR),'B-Dot Equation'
        ENDIF
      ELSE
        IF(ME.EQ.0)WRITE(IWR,'(2X,2A)') VARB(1:IVR),'B-Dot Equation'
      ENDIF
!
!---  Loop over the aqueous species  ---
!
      IF(.NOT.ALLOCATED(SP_L))ALLOCATE(SP_L(3,LSPL))
      DO 500 NSP = 1,NSPL
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Aqueous Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNML(NSP))
        DO 100 M = 1,NSP-1
          IF( SPNML(M).EQ.SPNML(NSP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Aqueous Species Name: ' // SPNML(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
        IF(ME.EQ.0)WRITE (IWR,'(/,2A)') ' Aqueous Species Name: ',SPNML(NSP)
!
!---    Skip for constant activity coefficent  ---
!
        IF( IACTV.EQ.3 ) GOTO 500
!
!---    Read aqueous species charge  ---
!
        VARB = 'Charge'
        IUNM = 1
        IDFLT = 1
        SP_L(1,NSP) = 0.D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_L(1,NSP))
        IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
           ': ',SP_L(1,NSP)
!
!---    Read aqueous species diameter  ---
!
        VARB = 'Diameter'
        SP_L(2,NSP) = 3.D-10
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_L(2,NSP))
        UNTS = 'm'
        IUNM = 1
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
           ': ',SP_L(2,NSP)
        INDX = 0
        CALL RDUNIT(UNTS,SP_L(2,NSP),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',SP_L(2,NSP),', m)'
!
!---    Read aqueous species molecular weight  ---
!
        VARB = 'Molecular Weight'
        UNTS = 'g/mol'
        IUNMOL = -1
        IUNKG = 1
        IDFLT = 1
        SP_L(3,NSP) = 0.D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_L(3,NSP))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME.EQ.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH), ': ',SP_L(3,NSP)
        INDX = 0
        CALL RDUNIT(UNTS,SP_L(3,NSP),INDX)
        IF(ME.EQ.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',SP_L(3,NSP),',  &
         g/mol)'
!
!---  Read next aqueous species  ---
!
  500 CONTINUE
!
!---  Read Pitzer parameters  ---
!
      IF( IACTV.EQ.2 )THEN
        CALL RDPTZR
      ENDIF

      ALLOCATE(ISPLK(15))
      ISPLK = 0
      ALLOCATE(ISP_MN(LSPR))
      ISP_MN = 0
      ALLOCATE(FACTV(LSPR))
      ALLOCATE(ACTVS(LSPR))

      IF(ME.EQ.0)WRITE(IWR,'(A)') ' '
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDAQSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDCNEQ
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
!     Read conservation equations for reactions.
!     Standard equation form:
!
!     d( Cs(1)*[Sp(1)] + Cs(2)*[Sp(2)] + .... )/dt = 0
!
!     Cs(1) = EQ_C(1,NEQC)
!     Cs(ns) = EQ_C(ns,NEQC)
!     ns = IEQ_C(1)
!     Sp(1) = IEQ_C(2)
!     Sp(ns) = IEQ_C(ns+1)
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 December 2004.
!     Last Modified by MD White, PNNL, 10 December 2004.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE FILES
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDCNEQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX

!
!---  Write card information to ouput file  ---
!
      CARD = 'Conservation Equations Card'
      ICD = INDEX( CARD,'  ' )-1
      IF(ME.EQ.0)WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of conservation equations  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Conservation Equations'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NEQC )
      IF( NEQC.GT.LEQC ) THEN
        INDX = 5
        CHMSG = 'Number of Conservation Equations > Parameter LEQC'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the conservation equations  ---
!
      NSPC = NEQC
      IF(.NOT.ALLOCATED(SPNMC)) ALLOCATE(SPNMC(LEQC))
      IF(.NOT.ALLOCATED(IEQ_C)) ALLOCATE(IEQ_C(LSEC+1,LEQC))
      IF(.NOT.ALLOCATED(EQ_C)) ALLOCATE(EQ_C(LSEC,LEQC))
      DO 500 NEQ = 1,NEQC
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Conservation Component Species Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMC(NEQ))
!
!---    Check for duplicate conservation component species name  ---
!
        DO 100 M = 1,NEQ-1
          IF( SPNMC(NEQ).EQ.SPNMC(M) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Conservation Component Species Name: ' &
              // SPNMC(NEQ)(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
!
!---    Load conservation component name into solute name
!       for output  ---
!
        SOLUT(NSOLU+NEQ) = SPNMC(NEQ)(1:NCH)
!
!---    Extract species name from component species name  ---
!
        IF( INDEX(SPNMC(NEQ)(1:),'total').NE.0 ) THEN
          ICH = INDEX(SPNMC(NEQ)(1:),'total') + 6
          BDUM = SPNMC(NEQ)(ICH:)
        ELSE
          BDUM = SPNMC(NEQ)
        ENDIF
        NCHB = INDEX(BDUM(1:),'  ')-1
!
!---    Aqueous species, assign species index  ---
!
        DO 110 M = 1,NSPL
          IF( SPNML(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = M
            GOTO 150
          ENDIF
  110   CONTINUE
!
!---    Solid species, assign species index  ---
!
        DO 120 M = 1,NSPS
          IF( SPNMS(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = NSPL + M
            GOTO 150
          ENDIF
  120   CONTINUE
!
!---    Exchanged species, assign species index  ---
!
        DO 125 M = 1,NSPE
          IF( SPNME(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = NSPL + NSPS + M
            GOTO 150
          ENDIF
  125   CONTINUE
!
!---    Gas species, assign species index  ---
!
        DO 130 M = 1,NSPG
          IF( SPNMG(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = NSPL + NSPS + NSPE + M
            GOTO 150
          ENDIF
  130   CONTINUE
!
!---    NAPL species, assign species index  ---
!
        DO 140 M = 1,NSPN
          IF( SPNMN(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = NSPL + NSPS + NSPE + NSPG + M
            GOTO 150
          ENDIF
  140   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Component Species Name: ' // &
          'Total-' // BDUM(1:NCHB)
        CALL WRMSGS( INDX )
  150   CONTINUE
        IF(ME.EQ.0)WRITE (IWR,'(/,2A)') &
            'Component Species Name: ','Total-' // BDUM(1:NCHB)
!
!---    Loop over component species  ---
!
        VARB = 'Number of Species in Conservation Equation'
        CALL RDINT( ISTART,ICOMMA,CHDUM,IEQ_C(1,NEQ) )
        IF( IEQ_C(1,NEQ).GT.LSEC ) THEN
          INDX = 5
          CHMSG = 'Number of Species in Conservation Equation > ' // &
            'Parameter LSEC'
          CALL WRMSGS( INDX )
        ENDIF
        ICSPX = 0
!
!---    Loop over the conservation-equation species  ---
!
        DO 300 NSP = 1,IEQ_C(1,NEQ)
          VARB = 'Conservation-Equation Species Name'
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          ICX = NSP + 2 - ICSPX
!
!---      Aqueous species, assign species index  ---
!
          DO 210 M = 1,NSPL
            IF( SPNML(M).EQ.ADUM ) THEN
              IF( M.EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = M
              ENDIF
              GOTO 250
            ENDIF
  210     CONTINUE
!
!---      Solid species, assign species index  ---
!
          DO 220 M = 1,NSPS
            IF( SPNMS(M).EQ.ADUM ) THEN
              IF( (M+NSPL).EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = NSPL + M
              ENDIF
              GOTO 250
            ENDIF
  220     CONTINUE
!
!---      Exchanged species, assign species index  ---
!
          DO 225 M = 1,NSPE
            IF( SPNME(M).EQ.ADUM ) THEN
              IF( (M+NSPL+NSPS).EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = NSPL + NSPS + M
              ENDIF
              GOTO 250
            ENDIF
  225     CONTINUE
!
!---      Gas species, assign species index  ---
!
          DO 230 M = 1,NSPG
            IF( SPNMG(M).EQ.ADUM ) THEN
              IF( (M+NSPL+NSPS+NSPE).EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = NSPL + NSPS + NSPE + M
              ENDIF
              GOTO 250
            ENDIF
  230     CONTINUE
!
!---      NAPL species, assign species index  ---
!
          DO 240 M = 1,NSPN
            IF( SPNMN(M).EQ.ADUM ) THEN
              IF( (M+NSPL+NSPS+NSPE+NSPG).EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = NSPL + NSPS + NSPE + NSPG + M
              ENDIF
              GOTO 250
            ENDIF
  240     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Conservation-Equation Species Name: ' // &
            ADUM(1:NCH)
          CALL WRMSGS( INDX )
  250     CONTINUE
          IF(ME.EQ.0)WRITE (IWR,'(/,2A)') &
              'Conservation-Equation Species Name: ',ADUM(1:NCH)
!
!---      Read conservation-equation species coefficient  ---
!
          VARB = 'Conservation-Equation Species Coefficient: '
          IDFLT = 1
          EQ_C(ICX-1,NEQ) = 0.D+0
!
!---      Allow for returns in input lines  ---
!
          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_C(ICX-1,NEQ))
          IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
              ': ',EQ_C(ICX-1,NEQ)
  300   CONTINUE
        IF( ICSPX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Component Species not a Constituent: ' // &
            BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read next equilibrium equation  ---
!
        IF( NEQ.LT.NEQE.AND.(ME.EQ.0) ) WRITE(IWR,'(/)')
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDCNEQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDEQEQ
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
!     Read equilibrium equations for reactions.
!     Standard equation form:
!
!     (Sp(1)) = (Keq^Ek)*((Sp(2))^Es(2))*((Sp(3))^Es(3))*...
!
!     Es(2) = EQ_E(1,NEQE)
!     Es(ns) = EQ_E(ns-1,NEQE)
!     Ek = EQ_E(ns,NEQE)
!     ns = IEQ_E(1,NEQE)
!     Sp(1) = IEQ_E(2,NEQE)
!     Sp(ns) = IEQ_E(ns+1,NEQE)
!     Keq = IEQ_E(ns+2,NEQE)
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 December 2004.
!     Last Modified by MD White, PNNL, 10 December 2004.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDEQEQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Equilibrium Equations Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of equilibrium equations  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Equilibrium Equations'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NEQE )
      IF( NEQE.GT.LEQE ) THEN
        INDX = 5
        CHMSG = 'Number of Equilibrium Equations > Parameter LEQE'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the equilibrium equations  ---
!
      IF(.NOT.ALLOCATED(IEQ_E)) ALLOCATE(IEQ_E(LSEE+2,LEQE))
      IF(.NOT.ALLOCATED(EQ_E)) ALLOCATE(EQ_E(LSEE,LEQE))
      DO 500 NEQ = 1,NEQE
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Species in Equilibrium Equation'
        ICX = 1
        CALL RDINT( ISTART,ICOMMA,CHDUM,IEQ_E(ICX,NEQ) )
        IF( IEQ_E(ICX,NEQ).GT.LSEE ) THEN
          INDX = 5
          CHMSG = 'Number of Species in Equilibrium Equation > ' // &
            'Parameter LSEE'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Loop over the equilibrium-equation species  ---
!
        DO 200 NSP = 1,IEQ_E(ICX,NEQ)
          VARB = 'Equilibrium-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          ICX = ICX + 1
!
!---      Aqueous species, assign species index  ---
!
          DO 100 M = 1,NSPL
            IF( SPNML(M).EQ.ADUM ) THEN
              IEQ_E(ICX,NEQ) = M
              GOTO 130
            ENDIF
  100     CONTINUE
!
!---      Solid species, assign species index  ---
!
          DO 110 M = 1,NSPS
            IF( SPNMS(M).EQ.ADUM ) THEN
              IEQ_E(ICX,NEQ) = NSPL + M
              GOTO 130
            ENDIF
  110     CONTINUE
!
!---      Exchanged species, assign species index  ---
!
          DO 115 M = 1,NSPE
            IF( SPNME(M).EQ.ADUM ) THEN
              IEQ_E(ICX,NEQ) = NSPL + NSPS + M
              GOTO 130
            ENDIF
  115     CONTINUE
!
!---      Gas species, assign species index  ---
!
          DO 120 M = 1,NSPG
            IF( SPNMG(M).EQ.ADUM ) THEN
              IEQ_E(ICX,NEQ) = NSPL + NSPS + NSPE + M
              GOTO 130
            ENDIF
  120     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Equilibrium-Equation Species Name: ' // &
           ADUM(1:NCH)
          CALL WRMSGS( INDX )
  130     CONTINUE
          IF( ME.EQ.0 )WRITE (IWR,'(/,2A)')  &
           ' Equilibrium-Equation Species Name: ', ADUM
!
!---      Read equilibrium-equation species exponent, skipping
!         the exponent for the equilibrium species  ---
!
          IF( NSP.GT.1 ) THEN
            VARB = 'Equilibrium-Equation Species Exponent: '
            IDFLT = 1
            EQ_E(ICX-2,NEQ) = 1.D+0
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_E(ICX-2,NEQ))
            IF( ME.EQ.0 )WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
                ': ',EQ_E(ICX-2,NEQ)
          ENDIF
  200   CONTINUE
!
!---    Equilibrium-equations equilibrium reaction  ---
!
        VARB = 'Equilibrium-Equation Equilibrium-Reaction Name: '
!
!---    Allow for returns in input lines  ---
!
        CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.0 ) THEN
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
        ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        ICX = ICX+1
!
!---    Assign equilibrium-reaction index  ---
!
        DO 300 M = 1,NRCE
          IF( RCNME(M).EQ.ADUM ) THEN
            IEQ_E(ICX,NEQ) = M
            GOTO 320
          ENDIF
  300   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Equilibrium-Equation ' // &
         'Equilibrium-Reaction Name: ' // ADUM(1:NCH)
        CALL WRMSGS( INDX )
  320   CONTINUE
        IF( ME.EQ.0 )WRITE (IWR,'(/,2A)') ' Equilibrium-Equation ' // &
         'Equilibrium-Reaction Name: ', ADUM
!
!---    Read equilibrium-equation equilibrium-reaction exponent  ---
!
        VARB = 'Equilibrium-Equation Equilibrium-Reaction Exponent: '
        IDFLT = 1
        EQ_E(ICX-2,NEQ) = 1.D+0
!
!---    Allow for returns in input lines  ---
!
        CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.0 ) THEN
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
        ENDIF
        CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_E(ICX-2,NEQ))
        IF( ME.EQ.0 )WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
            ': ',EQ_E(ICX-2,NEQ)
!
!---  Read next equilibrium equation  ---
!
        IF( NEQ.LT.NEQE .AND. ME.EQ.0 ) WRITE(IWR,'(/)')
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDEQEQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDEQRC
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
!     Read equilibrium reactions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 December 2004.
!     Last Modified by MD White, PNNL, 7 December 2004.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,UNTS
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDEQRC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Equilibrium Reactions Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of equilibrium reactions  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Equilibrium Reactions'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NRCE )
      IF( NRCE.GT.LRCE ) THEN
        INDX = 5
        CHMSG = 'Number of Equilibrium Reactions > Parameter LRCE'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the equilibrium reactions  ---
!
      IF( .NOT.ALLOCATED(RCNME) ) ALLOCATE(RCNME(LRCE))
      IF( .NOT.ALLOCATED(RC_E) ) ALLOCATE(RC_E(5,LRCE))
      DO 500 NRC = 1,NRCE
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Equilibrium Reaction Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RCNME(NRC))
        DO 10 M = 1,NRC-1
          IF( RCNME(M).EQ.RCNME(NRC) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Equilibrium Reaction Name: ' // &
              RCNME(NRC)
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        IF( ME.EQ.0 )WRITE (IWR,'(/,2A)') '  Equilibrium Reaction Name: &
          ',RCNME(NRC)
!
!---    Read equilibrium constant function coefficients
!       where, log(K) = b1*ln(T) + b2 + b3*T + b4/T + b5/(T^2)
!       and the equilibrium constant relates the aqueous
!       activity-molality product, gas fugacity, and
!       mineral activity  ---
!
        DO 300 M = 1,5
!
!---      Read equilibrium constant function coefficients  ---
!
          VARB = 'Equilibrium-Reaction Coefficient'
          IDFLT = 1
          RC_E(M,NRC) = 0.D+0
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_E(M,NRC))
          IF( ME.EQ.0 )WRITE(IWR,'(A,1PE11.4)') &
            '    Reaction Coefficient: ',RC_E(M,NRC)
  300   CONTINUE
!
!---    Read next equilibrium reaction  ---
!
        IF( NRC.LT.NRCE.AND.ME.EQ.0 ) WRITE(IWR,'(/)')
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDEQRC group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDEXSP
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
!     Read exchange species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 December 2004.
!     Last Modified by MD White, PNNL, 7 December 2004.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,UNTS,SPNMX
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDGSSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Exchanged Species Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of Exchanged species  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Exchanged Species'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPE )
      IF( NSPE.GT.LSPE ) THEN
        INDX = 5
        CHMSG = 'Number of Exchanged Species > Parameter LSPE'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read number of Exchange sites  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Exchange Sites'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NESITE )
      IF( NESITE.GT.LESITE ) THEN
        INDX = 5
        CHMSG = 'Number of Exchange Sites > Parameter LESITE'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Check for activity coefficient option  ---
!
      CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Activity Coefficient Option: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'gaines-thomas').NE.0 ) THEN
          IF( ME.EQ.0 )WRITE(IWR,'(2X,2A)') VARB(1:IVR), &
             'Gaines-Thomas convention'
          IACTEX = 1
        ELSEIF( INDEX(ADUM(1:),'vanselow').NE.0 ) THEN
          IF( ME.EQ. 0 )WRITE(IWR,'(2X,2A)') VARB(1:IVR), &
            'Vanselow convention'
          IACTEX = 2
        ELSEIF( INDEX(ADUM(1:),'Gapon').NE.0 ) THEN
          IF( ME.EQ. 0) WRITE(IWR,'(2X,2A)') VARB(1:IVR), &
            'Gapon convention'
          IACTEX = 3
        ELSE
          IF( ME.EQ. 0 ) WRITE(IWR,'(2X,2A)') VARB(1:IVR), &
            'Gaines-Thomas convention'
        ENDIF
      ELSE
        IF( ME.EQ.0 ) WRITE(IWR,'(2X,2A)') VARB(1:IVR), &
         'Gaines-Thomas convention'
      ENDIF
!
!---  Loop over the exchanged species  ---
!
      IF( .NOT.ALLOCATED(SPNME) )  ALLOCATE(SPNME(LSPE))
      IF( .NOT.ALLOCATED(ISP_E) )  ALLOCATE(ISP_E(LSPE))
      IF( .NOT.ALLOCATED(IEL_LK) ) ALLOCATE(IEL_LK(LSPE))
      DO 500 NSP = 1,NSPE
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Exchanged Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNME(NSP))
        DO 100 M = 1,NSP-1
          IF( SPNME(M).EQ.SPNME(NSP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Exchanged Species Name: ' // SPNME(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
        DO 110 M = 1,NSPL
          IF( SPNME(NSP).EQ.SPNML(M) )THEN
            INDX = 4
            CHMSG = 'Exchanged Species Name = Aqueous Species Name: ' // &
              SPNME(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  110   CONTINUE
        DO 120 M = 1,NSPS
          IF( SPNME(NSP).EQ.SPNMS(M) )THEN
            INDX = 4
            CHMSG = 'Exchanged Species Name = Solid Species ' // &
             'Name: ' // SPNME(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  120   CONTINUE
        IF( ME.EQ.0 )WRITE (IWR,'(/,2A)') ' Exchanged Species Name: ', &
          SPNME(NSP)
!
!---    Read adsorped cation  ---
!
        VARB = 'Reactive Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMX)
        NSPX = 0
        NSLX = 0
!
!---    Aqueous species, assign species index  ---
!
        DO 125 M = 1,NSPL
          IF( SPNML(M).EQ.SPNMX ) THEN
            NSPX = M
            IEL_LK(NSP) = NSPX
            NCH1 = INDEX(SPNME(NSP)(1:),' ') - 1
            NCH2 = INDEX(SPNMX(1:),' ') - 1
            IF( ME.EQ.0 ) WRITE (IWR,'(2A)') &
              ' Exchanged/Cation Species Link: ' // &
                SPNME(NSP)(1:NCH1)// ' <=> '//SPNMX(1:NCH2)
            GOTO 200
          ENDIF
  125   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Reactive Species Name: ' // &
          SPNMX(1:NCH)
        CALL WRMSGS( INDX )
  200   CONTINUE
!
!---    Read exchange site  ---
!
        VARB = 'Exchange site'
!        IUNM = 1
!        IDFLT = 1
        ISP_E(NSP) = 0
        CALL RDINT(ISTART,ICOMMA,CHDUM,ISP_E(NSP))
        IF( ME.EQ.0 ) WRITE(IWR,'(2X,2A,I4)') VARB(1:IVR), &
            ': ',ISP_E(NSP)
!
!---  Read next exchanged species  ---
!
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDEXSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDGSSP
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
!     Read gas species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 11 August 2005.
!     Last Modified by MD White, PNNL, 11 August 2005.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,UNTS,SPNMLX
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDGSSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Gas Species Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 ) WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of gas species  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Gas Species'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPG )
      IF( NSPG.GT.LSPG ) THEN
        INDX = 5
        CHMSG = 'Number of Gas Species > Parameter LSPG'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read gas molecular diffusion coefficient  ---
!
      VARB = 'Gas Species Molecular Diffusion Coefficient'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_MDG)
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      IF( ME.EQ.0 ) WRITE(IWR,'(4A,1PE11.4)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',SP_MDG
      INDX = 0
      IUNM = 2
      IUNS = -1
      CALL RDUNIT(UNTS,SP_MDG,INDX)
!
!---  Loop over the gas species  ---
!
      IF( .NOT.ALLOCATED(SPNMG) )  ALLOCATE(SPNMG(LSPG))
      DO 500 NSP = 1,NSPG
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Gas Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMG(NSP))
        DO 100 M = 1,NSP-1
          IF( SPNMG(M).EQ.SPNMG(NSP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Gas Species Name: ' // SPNMG(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
        DO 110 M = 1,NSPL
          IF( SPNMG(NSP).EQ.SPNML(M) ) THEN
            INDX = 4
            CHMSG = 'Gas Species Name = Aqueous Species Name: ' // &
             SPNMG(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  110   CONTINUE
        DO 120 M = 1,NSPS
          IF( SPNMG(NSP).EQ.SPNMS(M) ) THEN
            INDX = 4
            CHMSG = 'Gas Species Name = Solid Species Name: ' // &
             SPNMG(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  120   CONTINUE
        IF( ME.EQ.0 )WRITE (IWR,'(/,2A)') ' Gas Species Name: ', &
         SPNMG(NSP)
!
!---  Read next gas species  ---
!
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDGSSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDKNEQ
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
!     Read kinetic equations for reactions.
!     Standard equation form:
!
!     d( Cs(1)*[Sp(1)] + Cs(2)*[Sp(2)] + ... )/dt =
!       Cr(1)*R(1) + Cr(2)*R(2) + ...
!
!     Cs(1) = EQ_K(1,NEQK)
!     Cs(ns) = EQ_K(ns,NEQK)
!     Cr(1) = EQ_K(ns+1,NEQK)
!     Cr(nr) = EQ_K(ns+nr,NEQK)
!     ns = IEQ_K(1,NEQK)
!     Sp(1) = IEQ_K(2,NEQK)
!     Sp(ns) = IEQ_K(ns+1,NEQK)
!     nr = IEQ_K(ns+2,NEQK)
!     R(1) = IEQ_K(ns+3,NEQK)
!     R(nr) = IEQ_K(ns+nr+2,NEQK)
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 December 2004.
!     Last Modified by MD White, PNNL, 10 December 2004.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE FILES
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDKNEQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Kinetic Equations Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 ) WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of kinetic equations  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Kinetic Equations'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NEQK )
      IF( NEQK.GT.LEQK ) THEN
        INDX = 5
        CHMSG = 'Number of Kinetic Equations > Parameter LEQK'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the kinetic equations  ---
!
      IF(.NOT.ALLOCATED(SPNMK))  ALLOCATE(SPNMK(LEQK))
      IF(.NOT.ALLOCATED(IEQ_K)) ALLOCATE(IEQ_K(LSEK+LREK+2,LEQK))
      IF(.NOT.ALLOCATED(EQ_K)) ALLOCATE(EQ_K(LSEK+LREK,LEQK))
      DO 500 NEQ = 1,NEQK
!
!---    Kinetic equation species ---
!
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Kinetic Component Species Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMK(NEQ))
!
!---    Check for duplicate conservation component species name  ---
!
        DO 10 M = 1,NEQ-1
          IF( SPNMK(NEQ).EQ.SPNMK(M) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Kinetic Component Species Name: ' &
              // SPNMK(NEQ)(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
!
!---    Load kinetic component species name into solute name
!       for output  ---
!
        SOLUT(NSOLU+NEQC+NEQ) = SPNMK(NEQ)(1:NCH)
!
!---    Number of species in kinetic equation  ---
!
        VARB = 'Number of Species in Kinetic Equation'
        ICX = 1
        JCX = 0
        CALL RDINT( ISTART,ICOMMA,CHDUM,IEQ_K(ICX,NEQ) )
        IF( IEQ_K(ICX,NEQ).GT.LSEK ) THEN
          INDX = 5
          CHMSG = 'Number of Species in Kinetic Equation > ' // &
            'Parameter LSEK'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Loop over the kinetic equation species  ---
!
        DO 200 NSP = 1,IEQ_K(ICX,NEQ)
          VARB = 'Kinetic-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          ICX = ICX+1
!
!---      Aqueous species, assign species index  ---
!
          DO 100 M = 1,NSPL
            IF( SPNML(M).EQ.ADUM ) THEN
              IEQ_K(ICX,NEQ) = M
              GOTO 120
            ENDIF
  100     CONTINUE
!
!---      Solid species, assign species index  ---
!
          DO 110 M = 1,NSPS
            IF( SPNMS(M).EQ.ADUM ) THEN
              IEQ_K(ICX,NEQ) = NSPL + M
              GOTO 120
            ENDIF
  110     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Kinetic-Equation Species Name: ' // &
            ADUM(1:NCH)
          CALL WRMSGS( INDX )
  120     CONTINUE
          IF( ME.EQ.0 )WRITE (IWR,'(/,2A)') &
            ' Kinetic-Equation Species Name: ', ADUM
!
!---      Read kinetic equation species coefficient  ---
!
          VARB = 'Kinetic-Equation Species Coefficient: '
          IDFLT = 1
          JCX = JCX+1
          EQ_K(JCX,NEQ) = 0.D+0
!
!---      Allow for returns in input lines  ---
!
          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_K(JCX,NEQ))
          IF( ME.EQ.0 )WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
              ': ',EQ_K(JCX,NEQ)
  200   CONTINUE
!
!---    Kinetic-equation kinetic reactions  ---
!
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Kinetic Reactions in Kinetic Equation'
        ICX = ICX+1
        CALL RDINT( ISTART,ICOMMA,CHDUM,IEQ_K(ICX,NEQ) )
        IF( IEQ_K(ICX,NEQ).GT.LREK ) THEN
          INDX = 5
          CHMSG = 'Number of Kinetic-Reactions in Kinetic ' // &
            'Equation > Parameter LREK'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Loop over the kinetic-equation kinetic reactions  ---
!
        DO 400 NRC = 1,IEQ_K(ICX,NEQ)
          VARB = 'Kinetic-Equation Kinetic-Reaction Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          ICX = ICX+1
!
!---      Assign kinetic-reaction index  ---
!
          DO 300 M = 1,NRCK
            IF( RCNMK(M).EQ.ADUM ) THEN
              IEQ_K(ICX,NEQ) = M
              GOTO 320
            ENDIF
  300     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Kinetic-Equation ' // &
            'Kinetic-Reaction Name: ' // ADUM(1:NCH)
          CALL WRMSGS( INDX )
  320     CONTINUE
          IF( ME.EQ.0 )WRITE (IWR,'(/,2A)') ' Kinetic-Equation ' // &
            'Kinetic-Reaction Name: ',ADUM
!
!---      Read kinetic-equation kinetic-reaction coefficient  ---
!
          VARB = 'Kinetic-Equation Kinetic-Reaction Coefficient: '
          IDFLT = 1
          JCX = JCX+1
          EQ_K(JCX,NEQ) = 0.D+0
!
!---      Allow for returns in input lines  ---
!
          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_K(JCX,NEQ))
          IF( ME.EQ.0 ) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
              ': ',EQ_K(JCX,NEQ)
  400   CONTINUE
!
!---  Read next kinetic equation  ---
!
        IF( NEQ.LT.NEQK.AND.ME.EQ.0 ) WRITE(IWR,'(/)')
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDKNEQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDKNRC
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
!     Read kinetic reactions.
!
!     IRC_K(1,NRC)           number of kinetic reac. reactants (NSPR)
!     IRC_K(2,NRC)           number of kinetic reac. products (NSPP)
!                            NSPS = NSPR + NSPP
!     IRC_K(3:NSPR+2,NRC)    global species num. of reactants
!     IRC_K(NSPR+3:NSPS+2,NRC) global species num. of products
!     IRC_K(NSPS+3,NRC)      global species num. of mineral
!     IRCKN(LSPK+11) - flag to read spatially variable kinetic reaction parameters
!       0 - No spatial variation
!       1 - Rxn parameters assigned on a node by node basis
!
!     RC_K(1:NSPR,NRC)       kinetic reac. reactant stochiometric coeff.
!     RC_K(NSPR+1:NSPS,NRC)  kinetic reac. product stochiometric coeff.
!     RC_K(NSPS+1,NRC)       forward kinetic reac. ref. rate, mol/m^2/s
!     RC_K(NSPS+2,NRC)       backward kinetic reac. ref. rate, mol/m^2/s
!     RC_K(NSPS+3,NRC)       kinetic reac. activation energy, J/mol
!     RC_K(NSPS+4,NRC)       kinetic reac. reference temperature, C
!     RC_K(NSPS+5,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+6,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+7,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+8,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+9,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+10,NRC)      equilibrium constant function coefficient
!
!     Multi-rate variables
!
!     IRC_K(NSPS+4,NRC)          number of rate mechanisms ² 5
!     IRC_K(NSPS+5,NRC)          number of species in rate mechanism #1 ² 5
!     IRC_K(NSPS+6:NSPS+10,NRC)  global species num. in rate mechanism #1
!     IRC_K(NSPS+11,NRC)         number of species in rate mechanism #2 ² 5
!     IRC_K(NSPS+12:NSPS+16,NRC) global species num. in rate mechanism #2
!     IRC_K(NSPS+17,NRC)         number of species in rate mechanism #3 ² 5
!     IRC_K(NSPS+18:NSPS+22,NRC) global species num. in rate mechanism #3
!     IRC_K(NSPS+23,NRC)         number of species in rate mechanism #4 ² 5
!     IRC_K(NSPS+24:NSPS+28,NRC) global species num. in rate mechanism #4
!     IRC_K(NSPS+29,NRC)         number of species in rate mechanism #5 ² 5
!     IRC_K(NSPS+30:NSPS+34,NRC) global species num. in rate mechanism #5
!     RC_K(NSPS+11,NRC)            neutral reference reaction rate, mol/m^2/s
!     RC_K(NSPS+12,NRC)            neutral activation energy, J/mol
!     RC_K(NSPS+13,NRC)            neutral reference temperature, C
!     RC_K(NSPS+14,NRC)            mech. #1 reference reaction rate, mol/m^2/s
!     RC_K(NSPS+15,NRC)            mech. #1 activation energy, J/mol
!     RC_K(NSPS+16,NRC)            mech. #1 reference temperature, C
!     RC_K(NSPS+17:NSPS+21,NRC)         mech. #1 species #1-5 stochiometric coeff.
!     RC_K(NSPS+22,NRC)           mech. #1 species #5 stochiometric coeff.
!     RC_K(NSPS+23,NRC)           mech. #2 reference reaction rate, mol/m^2/s
!     RC_K(13,NRC)           mech. #2 activation energy, J/mol
!     RC_K(14,NRC)           mech. #2 reference temperature, C
!     RC_K(15:19,NRC)        mech. #2 species #1-5 stochiometric coeff.
!     RC_K(20,NRC)           mech. #3 reference reaction rate, mol/m^2/s
!     RC_K(21,NRC)           mech. #3 activation energy, J/mol
!     RC_K(22,NRC)           mech. #3 reference temperature, C
!     RC_K(23:27,NRC)        mech. #3 species #1-5 stochiometric coeff.
!     RC_K(28,NRC)           mech. #4 reference reaction rate, mol/m^2/s
!     RC_K(29,NRC)           mech. #4 activation energy, J/mol
!     RC_K(30,NRC)           mech. #4 reference temperature, C
!     RC_K(31:35,NRC)        mech. #4 species #1-5 stochiometric coeff.
!     RC_K(36,NRC)           mech. #5 reference reaction rate, mol/m^2/s
!     RC_K(37,NRC)           mech. #5 activation energy, J/mol
!     RC_K(38,NRC)           mech. #5 reference temperature, C
!     RC_K(39:43,NRC)        mech. #5 species #1-5 stochiometric coeff.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!     Last Modified by MD White, PNNL, 8 December 2004.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE FILES
      USE GRID_MOD
      USE BUFFEREDREAD
      USE GRID_MOD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
#include "utils.h"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,UNTS,T_FILENAME
      CHARACTER*512 CHDUM
      INTEGER :: DIM1,LO(3),HI(3),LDXX(3)
      LOGICAL T_OK,ISBIN,ISHDF5
      LOGICAL,EXTERNAL :: RDIJK1D,RDIJK3D
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDKNRC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Kinetic Reactions Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ. 0) WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
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
!---  Read number of kinetic reactions  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Kinetic Reactions'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NRCK )
      IF( NRCK.GT.LRCK ) THEN
        INDX = 5
        CHMSG = 'Number of Kinetic Reactions > Parameter LRCK'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the kinetic reactions  ---
!
      NCKN = LDXX(1)*LDXX(2)*LDXX(3)
      IF( FILEREAD ) LCKN = MAX(LCKN,NCKN)
      IF(.NOT.ALLOCATED(RCNMK)) ALLOCATE(RCNMK(LRCK))
      IF(.NOT.ALLOCATED(IRCKT)) ALLOCATE(IRCKT(LRCK))
      IF(.NOT.ALLOCATED(IRC_K)) ALLOCATE(IRC_K(LSPK+3,LRCK))
      IF(.NOT.ALLOCATED(RC_K)) ALLOCATE(RC_K(LSPK+11,LCKN,LRCK))
      IF(.NOT.ALLOCATED(IRCKN)) ALLOCATE(IRCKN(LSPK+11))
      IRCKN = 0
      IRCKT = 0
      IRCK = 0
      IRCKN = 0
      RC_K = 0.D+0
      DO 500 NRC = 1,NRCK
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Kinetic Reaction Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RCNMK(NRC))
        DO 10 M = 1,NRC-1
          IF( RCNMK(M).EQ.RCNMK(NRC) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Kinetic-Reaction Name: ' // RCNMK(NRC)
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        IF( ME.EQ. 0) WRITE (IWR,'(/,2A)') ' Kinetic-Reaction Name: ', &
          RCNMK(NRC)
!
!---    Kinetic reaction type
!
        VARB = 'Kinetic Reaction Type: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'dissolu').NE.0 .OR. &
          INDEX(ADUM(1:),'precip').NE.0 .OR. &
          INDEX(ADUM(1:),'tst').NE.0 ) THEN
          IF ( INDEX(ADUM(1:),'tst').NE.0.AND.ME.EQ.0 ) WRITE(IWR,*) &
            'TST'
          IF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
            IRCKT(NRC) = 5
            IF( INDEX(ADUM(1:),'schaef').NE.0 ) THEN
              IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
                IRCKT(NRC) = 8
              ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
                IRCKT(NRC) = 9
              ENDIF
            ELSE IF( INDEX(ADUM(1:),'glass').NE.0 ) THEN
              IF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
                IRCKT(NRC) = 14
                IF( ME.EQ. 0) WRITE(IWR,*) 'Glass'
              ENDIF
!Fang
            ELSE IF( INDEX(ADUM(1:),'time dependency').NE.0 ) THEN
              IF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
                IRCKT(NRC) = 120
                IF( ME.EQ. 0) WRITE(IWR,*) 'IEX pH and time dependency'
              ENDIF
            ELSE
              IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
                IRCKT(NRC) = 6
              ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
                IRCKT(NRC) = 7
              ENDIF
            ENDIF
          ELSE IF( INDEX(ADUM(1:),'constant').NE.0 .or.  &
           INDEX(ADUM(1:),'constant rate').NE.0 ) THEN
            cement = 1
            IRCKT(NRC) = 16
          ELSE
            IRCKT(NRC) = 10
            IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
              IRCKT(NRC) = 11
            ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
              IRCKT(NRC) = 12
            ENDIF
          ENDIF
!
!---    Read coefficient matrix to translate rxn rate from pore- to macro-scale
!
          IF ( INDEX(ADUM(1:),'w/coef').NE.0 ) THEN
             IF( ME.EQ. 0) WRITE(IWR,*) 'w/coeff'
             ISLC(58) = 1
             IJK = 1
             UNTS = 'null'
             IF( .NOT.ALLOCATED(CFMX) )ALLOCATE( CFMX(num_nodes) )
             CFMX = 1.D+0
             T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
             T_OK = RDIJK1D( T_FILENAME,LDXX,LO,HI,CFMX,1.0D+0,ISBIN,ISHDF5 )
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'emulsion').NE.0 .OR. &
          INDEX(ADUM(1:),'oil').NE.0 ) .AND. &
          INDEX(ADUM(1:),'sorption').NE.0 ) THEN
          IRCKT(NRC) = 15
        ELSEIF( INDEX(ADUM(1:),'multi').NE.0 .AND. &
          INDEX(ADUM(1:),'rate').NE.0 ) THEN
          IRCKT(NRC) = 20
        ELSEIF( INDEX(ADUM(1:),'forward').NE.0 .OR. &
          INDEX(ADUM(1:),'backward').NE.0 ) THEN
          IRCKT(NRC) = 1
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 .AND. &
          INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKT(NRC) = 2
        ELSEIF( INDEX(ADUM(1:),'sorption').NE.0 .AND. &
          INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKT(NRC) = 3
        ELSEIF( INDEX(ADUM(1:),'sorption').NE.0 .AND. &
          INDEX(ADUM(1:),'langmuir').NE.0 ) THEN
          IRCKT(NRC) = 13
        ELSEIF( INDEX(ADUM(1:),'biomass').NE.0 .AND. &
          INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKT(NRC) = 4
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 .AND. &
          INDEX(ADUM(1:),'dual').NE.0 ) THEN
          IRCKT(NRC) = 35
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 .AND. &
          INDEX(ADUM(1:),'single').NE.0 ) THEN
          IRCKT(NRC) = 36
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 ) THEN
          IRCKT(NRC) = 22
        ELSEIF( INDEX(ADUM(1:),'biomass').NE.0 ) THEN
          IRCKT(NRC) = 24
        ELSEIF( INDEX(ADUM(1:),'lognormal').NE.0 .AND. &
          INDEX(ADUM(1:),'liu').NE.0 ) THEN
          IRCKT(NRC) = 41
        ELSEIF( INDEX(ADUM(1:),'dualdomain').NE.0 .AND. &
          INDEX(ADUM(1:),'liu').NE.0 ) THEN
          IRCKT(NRC) = 42
        ENDIF
!
!---    Mineral  ---
!
        IF( (IRCKT(NRC).GE.10 .AND. IRCKT(NRC).LE.12) .OR. &
          (IRCKT(NRC).GE.5 .AND. IRCKT(NRC).LE.9) .OR. &
          (IRCKT(NRC).EQ.14 ) .OR. &
          (IRCKT(NRC).EQ.16 ) .OR. &
          IRCKT(NRC).EQ.20 .OR. IRCKT(NRC).EQ.120) THEN
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          VARB = 'Mineral Name'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
!
!---      Solid species, assign species index  ---
!
          DO 20 M = 1,NSPS
            IF( SPNMS(M).EQ.BDUM ) THEN
              IRC_KX = NSPL + M
              GOTO 30
            ENDIF
   20     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Kinetic Mineral Name: ' // &
            BDUM(1:NCH)
          CALL WRMSGS( INDX )
   30     CONTINUE
        ENDIF
!
!---    Multi-rate mineral  ---
!
        IF( IRCKT(NRC).EQ.20 ) GOTO 400
!
!---    Number of reactants in kinetic reaction  ---
!
        VARB = 'Number of Reactants in Kinetic Reaction'
        CALL RDINT( ISTART,ICOMMA,CHDUM,IRC_K(1,NRC) )
        NSPRX = IRC_K(1,NRC)
        IF( NSPRX.GT.LSPK ) THEN
          INDX = 4
          CHMSG = 'Number of Reactants in Kinetic Reaction > ' // &
            'Parameter LSPK'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.2 .AND. NSPRX.NE.3 ) THEN
          INDX = 4
          CHMSG = 'Valocchi-Monod Kinetics:  Number of Reactants ­ 3'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.3 .AND. NSPRX.NE.1 ) THEN
          INDX = 4
          CHMSG = 'Valocchi-Sorption Kinetics:  Number of Reactants ­ 1'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.13 .AND. NSPRX.NE.1 ) THEN
          INDX = 4
          CHMSG = 'Langmuir-Sorption Kinetics:  Number of Reactants ­ 1'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.4 .AND. NSPRX.NE.3 ) THEN
          INDX = 4
          CHMSG = 'Valocchi-Biomass Kinetics:  Number of Reactants ­ 3'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.22 .AND. NSPRX.LE.1 ) THEN
          INDX = 4
          CHMSG = 'Monod Kinetics:  Number of Reactants <= 1'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.24 .AND. NSPRX.LE.1 ) THEN
          INDX = 4
          CHMSG = 'Biomass Kinetics:  Number of Reactants <= 1'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.35 .AND. NSPRX.NE.2 ) THEN
          INDX = 4
          CHMSG = 'Dual-Monod Kinetics:  Number of Reactants ­ 2'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.36 .AND. NSPRX.NE.2 ) THEN
          INDX = 4
          CHMSG = 'Single-Monod Kinetics:  Number of Reactants ­ 2'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.15 .AND. NSPRX.NE.1 ) THEN
          INDX = 4
          CHMSG = 'Oil-Sorption Kinetics:  Number of Reactants ­ 1'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Loop over the kinetic reaction reactants  ---
!
        ICX = 2
        JCX = 0
        DO 130 NSP = 1,NSPRX
          VARB = 'Kinetic-Reaction Reactant Name: '
          ICX = ICX+1
          JCX = JCX+1
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---      Aqueous species, assign species index  ---
!
          DO 100 M = 1,NSPL
            IF( SPNML(M).EQ.ADUM ) THEN
              IRC_K(ICX,NRC) = M
              GOTO 120
            ENDIF
  100     CONTINUE
!
!---      Solid species, assign species index  ---
!
          DO 110 M = 1,NSPS
            IF( SPNMS(M).EQ.ADUM ) THEN
              IRC_K(ICX,NRC) = NSPL + M
              GOTO 120
            ENDIF
  110     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Kinetic Species Name: ' // &
            ADUM(1:NCH)
          CALL WRMSGS( INDX )
  120     CONTINUE
          IF( ME.EQ. 0) WRITE (IWR,'(/,2A)') &
            ' Kinetic Reaction Reactant Name: ', ADUM
!
!---      Set default values
!
          IJK = 1
          LNDX = LSPK+11
!
!---      Skip kinetic reaction reactant stoichiometric coefficient
!         for Monod and Biomass kinetics  ---
!
          IF( IRCKT(NRC).NE.22 .AND. IRCKT(NRC).NE.24 ) THEN
!
!---        Read kinetic reaction reactant stoichiometric coefficient  ---
!
            VARB = 'Kinetic-Reaction Reactant Stoichiometric ' // &
              'Coefficient: '
            IDFLT = 1
!
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              UNTS = 'null'
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
                  ': ',RC_K(JCX,1,NRC)
            ENDIF
          ELSE
            JCX = JCX-1
          ENDIF
  130   CONTINUE
!
!---    Number of products in kinetic reaction  ---
!
        ICX = 2+NSPRX
!
!---    Skip kinetic reaction products
!       for Monod and Biomass kinetics  ---
!
        IF( IRCKT(NRC).NE.22 .AND. IRCKT(NRC).NE.24 ) THEN
          VARB = 'Number of Products in Kinetic Reaction'
          CALL RDINT( ISTART,ICOMMA,CHDUM,IRC_K(2,NRC) )
          NSPPX = IRC_K(2,NRC)
          IF( NSPPX+NSPRX.GT.LSPK ) THEN
            INDX = 4
            CHMSG = 'Number of Reactants + Products in ' // &
              'Kinetic Reaction > Parameter LSPK'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.2 .AND. NSPPX.NE.0 ) THEN
            INDX = 4
            CHMSG = 'Valocchi-Monod Kinetics:  Number of Products ­ 0'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.3 .AND. NSPPX.NE.1 ) THEN
            INDX = 4
            CHMSG = 'Valocchi-Sorption Kinetics:  ' // &
              'Number of Products ­ 1'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.13 .AND. NSPPX.NE.1 ) THEN
            INDX = 4
            CHMSG = 'Langmuir-Sorption Kinetics:  ' // &
              'Number of Products ­ 1'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.4 .AND. NSPPX.NE.0 ) THEN
            INDX = 4
            CHMSG = 'Valocchi-Biomass Kinetics:  Number of Products ­ 0'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.35 .AND. NSPPX.NE.0 ) THEN
            INDX = 4
            CHMSG = 'Dual-Monod Kinetics:  Number of Products ­ 0'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.36 .AND. NSPPX.NE.0 ) THEN
            INDX = 4
            CHMSG = 'Single-Monod Kinetics:  Number of Products ­ 0'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.15 .AND. NSPPX.NE.1 ) THEN
            INDX = 4
            CHMSG = 'Oil-Sorption Kinetics:  Number of Products ­ 1'
            CALL WRMSGS( INDX )
          ENDIF
!
!---      Loop over the kinetic reaction products  ---
!
          DO 230 NSP = 1,NSPPX
            VARB = 'Kinetic-Reaction Product Name: '
            ICX = ICX+1
!
!---        Allow for returns in input lines  ---
!
            CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---        Aqueous species, assign species index  ---
!
            DO 200 M = 1,NSPL
              IF( SPNML(M).EQ.ADUM ) THEN
                IRC_K(ICX,NRC) = M
                GOTO 220
              ENDIF
  200       CONTINUE
!
!---        Solid species, assign species index  ---
!
            DO 210 M = 1,NSPS
              IF( SPNMS(M).EQ.ADUM ) THEN
                IRC_K(ICX,NRC) = NSPL + M
                GOTO 220
              ENDIF
  210       CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Kinetic Species Name: ' // &
              ADUM(1:NCH)
            CALL WRMSGS( INDX )
  220       CONTINUE
            IF( ME.EQ. 0) WRITE (IWR,'(/,2A)') &
              ' Kinetic Reaction Reactant Name: ', ADUM
!
!---        Read kinetic reaction reactant
!           stoichiometric coefficient  ---
!
            VARB = 'Kinetic-Reaction Reactant Stoichiometric ' // &
              'Coefficient: '
            IDFLT = 1
            JCX = JCX+1
!           RC_K(JCX,NRC) = 0.D+0
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              UNTS = 'null'
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC)) 
            IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
                  ': ',RC_K(JCX,1,NRC)
            ENDIF
  230     CONTINUE
        ELSE
          IRC_K(2,NRC) = 0
        ENDIF
!
!---    Read kinetic reaction parameters  ---
!
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---    Dissolution-precipitation type reactions  ---
!
        IF( (IRCKT(NRC).GE.10 .AND. IRCKT(NRC).LE.12) .OR. &
         (IRCKT(NRC).EQ.14 ) .OR. &
         (IRCKT(NRC).EQ.16 ) .OR. &
         (IRCKT(NRC).GE.5 .AND. IRCKT(NRC).LE.9) .OR. &
         (IRCKT(NRC).EQ.120 ) ) THEN
!
!---      Identify the mineral species  ---
!
          IRC_K(3+NSPRX+NSPPX,NRC) = IRC_KX
          ISP_MN(IRC_KX) = 1
!
!---      Read forward dissolution-precipitation
!         reference reaction rate  ---
!
          VARB = 'Kinetic Reaction Reference Rate'
          UNTS = 'mol/m^2 s'
          IUNMOL = 1
          IUNM = -2
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---      Convert from kmol/m^2 s to mol/m^2 s  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/m^2 s to mol/m^2 s  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
              RC_K(JCX,1,NRC),', mol/m^2 s)'
          ENDIF
!
!---      Read activation energy  ---
!
          VARB = 'Kinetic Reaction Activation Energy'
          UNTS = 'j/mol'
          IUNMOL = -1
          IUNM = 2
          IUNKG = 1
          IUNS = -2
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
            DO 234 J = 1,LRC
!
!---        Convert from J/kmol to J/mol ---
!
              RC_K(JCX,J,NRC) = 1.D-3*RC_K(JCX,J,NRC)
  234       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
              ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from J/kmol to J/mol  ---
!
            RC_K(JCX,1,NRC) = 1.D-3*RC_K(JCX,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
              RC_K(JCX,1,NRC),', J/mol)'
          ENDIF
!
!---      Read forward dissolution-precipitation
!         reference reaction temperature  ---
!
          VARB = 'Kinetic Reaction Reference Temperature'
          UNTS = 'c'
          IUNK = 1
          IDFLT = 1
          JCX = JCX + 1
!xl          RC_K(JCX,1:LRCK,NRC) = 1.D+20
          RC_K(JCX,1:LCKN,NRC) = 1.D+20
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
            ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0)WRITE(IWR,'(A,1PE11.4,A)') ' (', &
              RC_K(JCX,1,NRC), &
            ', C)'
          ENDIF
!
!---      Read dissolution-precipitation
!         pH exponent  ---
!
          IF(( IRCKT(NRC).GE.5 .AND. IRCKT(NRC).LE.9 ) .OR. &
            (IRCKT(NRC).EQ.14 ) ) THEN
            VARB = 'Kinetic Reaction pH Exponent'
            IDFLT = 1
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX+6
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE
              RC_K(JCX+6,1,NRC) = 0.D+0
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX+6,1,NRC))
            IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
                ': ',RC_K(JCX+6,1,NRC)
            ENDIF
          ENDIF
!---      Read iex ph and time dependency parameters
!         pH dependency  ---
!
          IF( IRCKT(NRC).EQ.120) THEN
            VARB = 'Kinetic Reaction pH Dependency Parameter'
            IDFLT = 1
            INDX = JCX + 6 
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
!----         five spots claimed for temperaturate dependency
              INDX = JCX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE
              RC_K(INDX,1,NRC) = 0.D+0
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(INDX,1,NRC))
            IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
                ': ',RC_K(INDX,1,NRC)
            ENDIF
!           Simulation time dependency
            VARB = 'Kinetic Reaction Simulation Time &
                          Dependency Parameter'
            IDFLT = 1
            INDX = JCX + 7
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(INDX)=1
            ELSE
              RC_K(INDX,1,NRC) = 0.D+0
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(INDX,1,NRC))
              IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
                ': ',RC_K(INDX,1,NRC)
            ENDIF
          ENDIF
!
!---      Read equilibrium constant function coefficients
!         where, log(K) = b1*ln(T) + b2 + b3*T + b4/T + b5/(T^2)
!         and the equilibrium constant relates the aqueous
!         activity-molality product, gas fugacity, and
!         mineral activity  ---
!
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          DO 300 M = 1,5
!
!---        Read equilibrium constant function coefficients  ---
!
            VARB = 'Equilibrium Reaction Constant Coefficient'
            IDFLT = 1
            JCX = JCX + 1
!           RC_K(JCX,NRC) = 0.D+0
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
                ': ',RC_K(JCX,1,NRC)
            ENDIF
  300     CONTINUE
!
!---    Forward-backward type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.1 ) THEN
!
!---      Read forward reaction rate  ---
!
          VARB = 'Forward Kinetic Reaction Rate'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
          VARX = 1.D+0
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
          ENDIF
!          CALL RDUNIT(UNTS,VARX,INDX)
!!
!!---      Convert from exponent to rate, 1/s  ---
!!
!          RC_K(JCX,NRC) = VARX*EXP(TOLN*RC_K(JCX,NRC))
          IF( ME.EQ. 0)WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', 1/s)'
!
!---      Read backward reaction rate  ---
!
          VARB = 'Backward Kinetic Reaction Rate'
          UNTS = 'mol/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
          VARX = 1.D+0
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
          ENDIF
!          CALL RDUNIT(UNTS,VARX,INDX)
!!
!!---      Convert from exponent to rate, 1/s  ---
!!
!          RC_K(JCX,NRC) = VARX*EXP(TOLN*RC_K(JCX,NRC))
          IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', 1/s)'
!
!---    Valocchi-Monod type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.2 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' // &
            'Half-Sat. Const. for Donor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---      Convert from kmol/kg to mol/kg  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', mol/kg)'
          ENDIF
!
!---      Read half-saturation constant for acceptor  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' // &
            'Half-Sat. Const. for Acceptor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---      Convert from kmol/kg to mol/kg  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', mol/kg)'
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' // &
           'Max. Spec. Rate of Substrate Util.'
          UNTS = '1/s'
          IUNMOL = 0
          IUNKG = 0 
          IUNS = -1
          IUNM = 0
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
              ', 1/s)'
          ENDIF
!
!---    Valocchi-Sorption type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.3 ) THEN
!
!---      Read mass transfer coefficient  ---
!
          VARB = 'Valocchi-Sorption Kin. Reac.: ' // &
            'Mass Transfer Coeff.'
          UNTS = '1/s'
          IUNMOL = 0
          IUNKG = 0 
          IUNS = -1
          IUNM = 0
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', 1/s)'
          ENDIF
!
!---      Read distribution coefficient  ---
!
          VARB = 'Valocchi-Sorption Kin. Reac.: ' // &
            'Distribution Coeff.'
          UNTS = 'm^3/kg'
          IUNM = 3
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', m^3/kg)'
          ENDIF
!
!---    Langmuir-Sorption type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.13 ) THEN
!
!---      Read forward mass transfer coefficient  ---
!
          VARB = 'Langmuir-Sorption Kin. Reac.: ' // &
            'Forward Mass Transfer Coeff.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
              ', ',UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)')' &
              (',RC_K(JCX,1,NRC),', 1/s)'
          ENDIF
!
!---      Read backward mass transfer coefficient  ---
!
          VARB = 'Langmuir-Sorption Kin. Reac.: ' // &
            'Backward Mass Transfer Coeff.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
              ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)')' &
              (',RC_K(JCX,1,NRC),', 1/s)'
          ENDIF
!
!---      Read maximum sorbed concentration ---
!
          VARB = 'Langmuir-Sorption Kin. Reac.: ' // &
            'Maximum Sorbed Concentration'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---      Convert from kmol/kg to mol/kg  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
              ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            IF( ME.EQ.0) WRITE(IWR,'(A,1PE11.4,A)') &
            ' (',RC_K(JCX,1,NRC), ', mol/kg)'
          ENDIF
!
!---    Valocchi-Biomass type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.4 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' // &
            'Half-Sat. Const. for Donor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---      Convert from kmol/kg to mol/kg  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') '  &
              (',RC_K(JCX,1,NRC), ', mol/kg)'
          ENDIF
!
!---      Read half-saturation constant for acceptor  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' // &
            'Half-Sat. Const. for Acceptor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---        Convert from kmol/kg to mol/kg  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
              ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
            INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---       Convert from kmol/kg to mol/kg  ---
!
             RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
             IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
               RC_K(JCX,1,NRC),', mol/m^3)'
          ENDIF
!
!---       Read maximum specific rate of substrate utilization  ---
!
           VARB = 'Valocchi-Monod Kin. Reac.: ' // &
             'Max. Spec. Rate of Substrate Util.'
           UNTS = '1/s'
           IUNMOL = 0
           IUNKG = 0 
           IUNS = -1
           IUNM = 0
           IDFLT = 1
           JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
               ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
            INDX = 0
             CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
             IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
               RC_K(JCX,1,NRC), ', 1/s)'
          ENDIF
!
!---       Read microbial yield coefficient  ---
!
           VARB = 'Valocchi-Monod Kin. Reac.: ' // &
             'Microbial Yield Coeff.'
           IDFLT = 1
           JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
              ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---       Read first-order micobial decay coefficient  ---
!
           VARB = 'Valocchi-Monod Kin. Reac.: ' // &
             'First-Order Microbial Decay Coeff.'
           UNTS = '1/s'
           IUNS = -1
           IDFLT = 1
           JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
              RC_K(JCX,1,NRC),', 1/s)'
          ENDIF
!
!---    Micro-emulsion type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.15 ) THEN
!
!---       Read equivalent collector diameter  ---
!
           VARB = 'Micro-Emulsion Kin. Reac.: ' // &
             'Equivalent Collector Diameter'
           UNTS = 'm'
           IUNM = 1
           IDFLT = 1
           JCX = JCX + 1
          RC_K(JCX,1:LRC,NRC) = 1.D-4
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
           CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
             UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
           CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
           IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
             ', m)'
          ENDIF
!
!---       Read empty bed collision efficiency  ---
!
           VARB = 'Micro-Emulsion Kin. Reac.: ' // &
             'Empty Bed Collision Efficiency'
           IDFLT = 1
           JCX = JCX + 1
           RC_K(JCX,1:LRC,NRC) = 2.5D-5
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
           CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
             ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---       Read maximum oil retention by sediment  ---
!
           VARB = 'Micro-Emulsion Kin. Reac.: ' // &
             'Maximum Oil Retention by Sediment'
           IDFLT = 1
           JCX = JCX + 1
          RC_K(JCX,1:LRC,NRC) = 3.7D-3
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
              ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---       Read equivalent oil-droplet diameter  ---
!
           VARB = 'Micro-Emulsion Kin. Reac.: ' // &
             'Equivalent Oil-Droplet Diameter'
           UNTS = 'm'
           IUNM = 1
           IDFLT = 1
           JCX = JCX + 1
          RC_K(JCX,1,NRC) = 1.25D-6
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
             ', m)'
          ENDIF
!
!---       Read oil-droplet density  ---
!
           VARB = 'Micro-Emulsion Kin. Reac.: ' // &
             'Oil-Droplet Density'
           UNTS = 'kg/m^3'
           IUNKG = 1
           IUNM = -3
           IDFLT = 1
           JCX = JCX + 1
          RC_K(JCX,1:LRC,NRC) = 9.5D+2
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
             ', kg/m^3)'
          ENDIF
!
!---    Monod type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.22 ) THEN
!
!---      Loop over reactants, less one  ---
!
          DO 320 NSP = 1,NSPRX-1
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read half-saturation constant  ---
!
            VARB = 'Monod Kinetic Reaction: ' // &
              'Half-Saturation Constant'
            UNTS = 'mol/liter'
            IUNMOL = 1
            IUNM = -3
            IDFLT = 1
            JCX = JCX + 1
!           RC_K(JCX,NRC) = 0.D+0
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---        Convert from kmol/m^3 to mol/m^3  ---
!
              VARX = VARX*1.D+3
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
               ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---        Convert from kmol/m^3 to mol/m^3  ---
!
              RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
              IF( ME.EQ. 0)WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
              ', mol/m^3)'
            ENDIF
  320     CONTINUE
!
!---      Allow for returns in input lines  ---
!
          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Monod Kinetic Reaction: ' // &
            'Maximum Specific Rate of Reactant Utilization'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
            RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
             UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0)WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
           ', 1/s)'
          ENDIF
!
!---    Biomass type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.24 ) THEN
!
!---      Loop over reactants, less one  ---
!
          DO 330 NSP = 1,NSPRX-1
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read half-saturation constant  ---
!
            VARB = 'Biomass Kinetic Reaction: ' // &
              'Half-Saturation Constant'
            UNTS = 'mol/liter'
            IUNMOL = 1
            IUNM = -3
            IDFLT = 1
            JCX = JCX + 1
!           RC_K(JCX,NRC) = 0.D+0
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---        Convert from kmol/m^3 to mol/m^3  ---
!
              VARX = VARX*1.D+3
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
                IRCKN(JCX)=1
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
                ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---        Convert from kmol/m^3 to mol/m^3  ---
!
              RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
              IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
              ', mol/m^3)'
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            ENDIF
!
!---        Read maximum specific rate of substrate utilization  ---
!
            VARB = 'Biomass Kinetic Reaction: ' // &
              'Maximum Specific Rate of Reactant Utilization'
            UNTS = '1/s'
            IUNS = -1
            IDFLT = 1
            JCX = JCX + 1
!           RC_K(JCX,NRC) = 0.D+0
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
              ', ', UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
              IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
              ', 1/s)'
            ENDIF
  330     CONTINUE
!
!---       Read microbial yield coefficient  ---
!
           VARB = 'Biomass Kinetic Reaction: ' // &
             'Microbial Yield Coefficient'
           IDFLT = 1
           JCX = JCX + 1
!          RC_K(JCX,NRC) = 0.D+0
!vlf
           IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
             INDX = JCX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
             IRCKN(JCX)=1
           ELSE
             CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
               ': ',RC_K(JCX,1,NRC)
           ENDIF
!
!---      Read first-order micobial decay coefficient  ---
!
          VARB = 'Biomass Kinetic Reaction: ' // &
            'Microbial Decay Coefficient'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', 1/s)'
          ENDIF
!
!---    Dual-Monod type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.35 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          VARB = 'Dual-Monod Kin. Reac.: ' // &
            'Half-Sat. Const. for Donor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---      Convert from kmol/kg to mol/kg  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
          ENDIF
!
!---      Convert from kmol/kg to mol/kg  ---
!
          RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
          IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', mol/kg)'
!
!---      Read half-saturation constant for acceptor  ---
!
          VARB = 'Dual-Monod Kin. Reac.: ' // &
            'Half-Sat. Const. for Acceptor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---        Convert from kmol/kg to mol/kg  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', mol/kg)'
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Dual-Monod Kin. Reac.: ' // &
            'Max. Spec. Rate of Substrate Util.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), & 
              ', 1/s)'
          ENDIF
!
!---    Single-Monod type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.36 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          VARB = 'Single-Monod Kin. Reac.: ' // &
            'Half-Sat. Const. for Donor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!
!---      Convert from kmol/kg to mol/kg  ---
!
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', mol/kg)'
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Single-Monod Kin. Reac.: ' // &
            'Max. Spec. Rate of Substrate Util.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
              ', 1/s)'
          ENDIF
!
!---    Liu's lognormal multi rate type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.41 ) THEN
!
!---      Read rate from lognormal distribution
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' // &
           'Rate Constant'
          UNTS = '1/s'
          IUNMOL = 0
          IUNKG = 0
          IUNS = -1
          IUNM = 0
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ.0 ) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
            ', ',UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ.0 ) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
              RC_K(JCX,1,NRC),', 1/s)'
          ENDIF
!
!---      Read kinetic site density  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' // &
            'Kinetic Site Density'
          UNTS = 'mol/g'
          IUNMOL = 1
          IUNKG = -1
          IUNS = 0
          IUNM = 0
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
!            
!---        Convert from kmol/m^3 to mol/m^3 ---
!          
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/m^3 to mol/m^3  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', mol/m^3)'
          ENDIF
!
!---      Read pore ratio  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' // &
            'Pore Ratio.'
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
              ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---      Read logK1 for the first sorped species on the site  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' // &
            'Log K1.'
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
             ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---      Read logK2 for the second sorped species on the site ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' // &
            'Log K2.'
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
            ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---    Liu's dual domain rate type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.42 ) THEN
!
!---      Mass transfer rate
!
          VARB = 'Liu Dual-Domain Kin. Reac.: ' // &
            'Rate Constant'
          UNTS = '1/s'
          IUNMOL = 0
          IUNKG = 0
          IUNS = -1
          IUNM = 0
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC), &
            ', 1/s)'
          ENDIF
!
!---      Read pore ratio (immobile pore/mobile pore) ---
!
          VARB = 'Liu Dual-Domain Kin. Reac.: ' // &
            'Pore Ratio.'
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          IF( ME.EQ. 0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
              ': ',RC_K(JCX,1,NRC)
          ENDIF
        ENDIF
!
!---    Multi-rate mineral  ---
!
  400   CONTINUE
        IF( IRCKT(NRC).EQ.20 ) THEN
!
!---      Identify the mineral species  ---
!
          IRC_K(1,NRC) = IRC_KX
          ISP_MN(IRC_KX) = 1
!
!---      Number of mechanisms in kinetic reaction  ---
!
          VARB = 'Number of Mechanisms in Kinetic Reaction'
          CALL RDINT( ISTART,ICOMMA,CHDUM,IRC_K(2,NRC) )
          IF( IRC_K(2,NRC).GT.5 ) THEN
            INDX = 4
            CHMSG = 'Multirate Kinetics:  Number of Mechanisms > 5'
            CALL WRMSGS( INDX )
          ENDIF
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
!
!---      Neutral reference reaction rate  ---
!
          VARB = 'Neutral Reaction Reference Rate'
          UNTS = 'mol/m^2 s'
          IUNMOL = 1
          IUNM = -2
          IUNS = -1
          IDFLT = 1
!         RC_K(1,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = 1
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            VARX = VARX*1.D+3
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(1,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(1,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(1,1,NRC),INDX)
!
!---      Convert from kmol/m^2 s to mol/m^2 s  ---
!
            RC_K(1,1,NRC) = 1.D+3*RC_K(1,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(1,1,NRC), &
            ', mol/m^2 s)'
          ENDIF
!
!---      Neutral activation energy  ---
!
          VARB = 'Neutral Reaction Activation Energy'
          UNTS = 'j/mol'
          IUNMOL = -1
          IUNM = 2
          IUNKG = 1
          IUNS = -2
          IDFLT = 1
!         RC_K(2,NRC) = 0.D+0
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = 2
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
            DO 266 J = 1,LRC
!            
!---        Convert from J/kmol to J/mol ---
!          
              RC_K(JCX,J,NRC) = 1.D-3*RC_K(2,J,NRC)
  266       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(2,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(2,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(2,1,NRC),INDX)
!
!---      Convert from J/kmol to J/mol  ---
!
            RC_K(2,1,NRC) = 1.D-3*RC_K(2,1,NRC)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(2,1,NRC), &
            ', J/mol)'
          ENDIF
!
!---      Neutral reference reaction temperature  ---
!
          VARB = 'Neutral Reaction Reference Temperature'
          UNTS = 'c'
          IUNK = 1
          IDFLT = 1
          RC_K(3,1:LRC,NRC) = 1.D+20
!vlf
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = 3
            T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
            VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
            T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
              RC_K,VARX,ISBIN,ISHDF5 )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(3,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
              UNTS(1:NCH),': ',RC_K(3,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(3,1,NRC),INDX)
            IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(3,1,NRC), &
            ', C)'
          ENDIF
!
!---      Loop over mechanisms  ---
!
          DO 490 NKRM = 1,IRC_K(2,NRC)
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
!
!---        Mechanism reference reaction rate  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Rate'
            UNTS = 'mol/m^2 s'
            IUNMOL = 1
            IUNM = -2
            IUNS = -1
            IDFLT = 1
            IX = 4+((NKRM-1)*8)
!           RC_K(IX,NRC) = 0.D+0
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = IX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              VARX = VARX*1.D+3
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE

              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(IX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
              ', ', UNTS(1:NCH),': ',RC_K(IX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(IX,1,NRC),INDX)
!
!---        Convert from kmol/m^2 s to mol/m^2 s  ---
!
              RC_K(IX,1,NRC) = 1.D+3*RC_K(IX,1,NRC)
              IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(IX,1,NRC), &
             ', mol/m^2 s)'
             ENDIF
!
!---        Mechanism activation energy  ---
!
            VARB = 'Mechanism Kinetic Reaction Activation Energy'
            UNTS = 'j/mol'
            IUNMOL = -1
            IUNM = 2
            IUNKG = 1
            IUNS = -2
            IDFLT = 1
            IX = 5+((NKRM-1)*8)
!            RC_K(IX,NRC) = 0.D+0
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = IX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
              DO 272 J = 1,LRC
!              
!---          Convert from J/kmol to J/mol ---
!            
                RC_K(JCX,J,NRC) = 1.D-3*RC_K(IX,J,NRC)
  272         CONTINUE
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(IX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
              ', ',  UNTS(1:NCH),': ',RC_K(IX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(IX,1,NRC),INDX)
!
!---        Convert from J/kmol to J/mol  ---
!
              RC_K(IX,1,NRC) = 1.D-3*RC_K(IX,1,NRC)
              IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
                RC_K(IX,1,NRC),', J/mol)'
            ENDIF
!
!---        Mechanism reference reaction temperature  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Temperature'
            UNTS = 'c'
            IUNK = 1
            IDFLT = 1
            IX = 6+((NKRM-1)*8)
            RC_K(IX,1:LRC,NRC) = 1.D+20
!vlf
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = IX
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                RC_K,VARX,ISBIN,ISHDF5 )
              IRCKN(JCX)=1
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(IX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( ME.EQ. 0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR), &
             ', ', UNTS(1:NCH),': ',RC_K(IX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(IX,1,NRC),INDX)
              IF( ME.EQ. 0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
                RC_K(IX,1,NRC), ', C)'
            ENDIF
!
!---        Number of species in mechanism  ---
!
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
            VARB = 'Number of Species in Mechanism'
            IX = 3+((NKRM-1)*6)
            CALL RDINT( ISTART,ICOMMA,CHDUM,IRC_K(IX,NRC) )
            IF( IRC_K(IX,NRC).GT.5 ) THEN
              INDX = 4
              CHMSG = 'Multirate Kinetics:  Number of Species > 5'
              CALL WRMSGS( INDX )
            ENDIF
!
!---        Loop over species  ---
!
            DO 450 NSP = 1,IRC_K(IX,NRC)
              IX = 3+((NKRM-1)*6)+NSP
              VARB = 'Mechanism Species Name: '
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---          Aqueous species, assign species index  ---
!
              DO 420 M = 1,NSPL
                IF( SPNML(M).EQ.ADUM ) THEN
                  IRC_K(IX,NRC) = M
                  GOTO 440
                ENDIF
  420         CONTINUE
!
!---          Solid species, assign species index  ---
!
              DO 430 M = 1,NSPS
                IF( SPNMS(M).EQ.ADUM ) THEN
                  IRC_K(IX,NRC) = NSPL + M
                  GOTO 440
                ENDIF
  430         CONTINUE
!
!---          Exchanged species, assign species index  ---
!
              DO 435 M = 1,NSPE
                IF( SPNME(M).EQ.ADUM ) THEN
                  IRC_K(IX,NRC) = NSPL + NSPS + M
                  GOTO 440
                ENDIF
  435         CONTINUE
              INDX = 4
              CHMSG = 'Unrecognized Mechanism Species Name: ' // &
               ADUM(1:NCH)
              CALL WRMSGS( INDX )
  440         CONTINUE
              IF( ME.EQ. 0) WRITE (IWR,'(/,2A)') &
                ' Mechanism Species Name: ', ADUM
!
!---          Read kinetic reaction reactant stoichiometric coefficient  ---
!
              VARB = 'Mechanism Species Stoichiometric Coefficient: '
              IDFLT = 1
              IX = 6+((NKRM-1)*8)+NSP
              RC_K(IX,1:LRC,NRC) = 1.D+0
!vlf
              IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
                INDX = IX
                T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
                VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
                T_OK = RDIJK3D( T_FILENAME,LDXX,LO,HI,LNDX,INDX,LRCK,NRC,&
                  RC_K,VARX,ISBIN,ISHDF5 )
                  IRCKN(JCX)=1
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(IX,1,NRC))
                IF( ME.EQ. 0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
                    ': ',RC_K(IX,1,NRC)
              ENDIF
  450       CONTINUE
  490     CONTINUE
        ENDIF
!
!---    Read next kinetic reaction  ---
!
        IF( NRC.LT.NRCK.AND.ME.EQ.0 ) WRITE(IWR,'(/)')
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDKNRC group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDLITH
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
!     Read solid species lithology of rock/soil types.
!
!     RS_S(1,LSPS,LFDR) - initial mineral (solid species) area
!     RS_S(2,LSPS,LFDR) - initial mineral (solid species) vol. fraction
!     RS_S(3,LSPS,LFDR) - current mineral (solid species) vol. fraction
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!     Last Modified by MD White, PNNL, 7 January 2005.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RS_SX(3)
      CHARACTER*64 ADUM,UNTS,ROCKX(LRC),BDUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK,ISBIN,ISHDF5
      INTEGER :: LO(3),HI(3),LDXX(3)
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDLITH'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
        '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Lithology Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ. 0 ) WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Check for IJK, JKI, or KIJ Indexing  ---
!
      IF(.NOT.ALLOCATED(IZX))ALLOCATE(IZX(NUM_NODES))
      IF( INDEX(ROCK(1),'indexing').NE.0 ) THEN
        iflg = -1
        NROCKX = NROCK
        ROCKX(1) = ROCK(1)
!        DO 10 N = 1,NROCK2
!          ROCKX(N) = ROCK2(N)
!   10   CONTINUE
!        DO 20 N = 1,NFLD
!          IZX(N) = IZ2(N)
!   20   CONTINUE
      ELSE
        NROCKX = NROCK
        DO 30 N = 1,NROCK
          ROCKX(N) = ROCK(N)
   30   CONTINUE
        DO 40 N = 1,NUM_NODES
          IZX(N) = IZ(N)
   40   CONTINUE
      ENDIF
!
!---  Loop over the rock/soil lithology information lines  ---
!
      N = 0
   50 CONTINUE
      IF( N.GE.NROCKX ) GOTO 500
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = 1,NROCKX
        IF( ADUM.EQ.ROCKX(M)) THEN
          IROCK = M
          GOTO 200
        ENDIF
  100 CONTINUE
!
!---  Search known scaling groups for a matching type ---
!
!      IF( ISLC(19).EQ.1 ) THEN
!        DO 110 M = 1,NSCALE
!           IF( ADUM.EQ.SCALNM(M) ) THEN
!              ISGRP = M
!              IROCK = 1
!              GOTO 200
!           ENDIF
!  110   CONTINUE
!        INDX = 2
!        CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
!     &    // ADUM(1:NCH)
!        CALL WRMSGS( INDX )
!        GOTO 50
!      ENDIF
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // ADUM(1:NCH)
      CALL WRMSGS( INDX )
      GOTO 50
  200 CONTINUE
!
!---  Loop over rock/soils within scaling group  ---
!
!      IF( ISLC(19).EQ.1 .AND. ISGRP.NE.0 ) THEN
!        DO 202 M = IROCK,NROCKX
!          IF( ISCALE(M).EQ.ISGRP ) THEN
!            IROCK = M
!            GOTO 204
!          ENDIF
!  202   CONTINUE
!      ENDIF
  204 CONTINUE
!
!---  Write rock/soil name  ---
!
      IF( ME.EQ.0 ) WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCKX(IROCK)
      N = N + 1
  220 CONTINUE
      iflg = irock
!
!---  Write rock/soil name  ---
!
      VARB = 'Number of Solid-Species Minerals'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPSX )
      IF( NSPSX.GT.NSPS ) THEN
        INDX = 5
        CHMSG = 'Number of Solid Species > Solid Species Count'
        CALL WRMSGS( INDX )
      ENDIF
!
!---    Read inert (non reactive) volume fraction ---
!
      CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
          LO(1) = IAXMIN
          LO(2) = IAYMIN
          LO(3) = IAZMIN
          HI(1) = IAXMAX
          HI(2) = IAYMAX
          HI(3) = IAZMAX
          LDXX(1) = IAXMAX - IAXMIN + 1
          LDXX(2) = IAYMAX - IAYMIN + 1
          LDXX(3) = IAZMAX - IAZMIN + 1
          VARB = 'Inert Volume Fraction'
          UNTS = 'null'
          INDC = 3
          ISTX = ISTART
          ICMX = ICOMMA
          CALL RDCHR(ISTX,ICMX,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM,'file').NE.0 ) THEN
            IF( INDEX(BDUM(1:),'binary').NE.0 .OR. &
              INDEX(BDUM(1:),'bfile').NE.0 .OR. &
              INDEX(BDUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
              T_OK = GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA,ISHDF5 )
              VARX = GETUNITS( CHDUM,UNTS,ISTART,ICOMMA )
              T_OK = RDIJK1D( T_FILENAME,LDXX,LO,HI,VFRAC_I,VARX,ISBIN,ISHDF5 )
          ELSE
             CALL RDDPR(ISTART,ICOMMA,CHDUM,VFX)
             IF(ME.EQ.0)WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',VFX
             IF(ME.EQ.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',VFX
             T_OK = COPYIJK1D( VFRAC_I,VFX,IFLG )
          ENDIF
      ENDIF


!
!---  Loop over the solid species  ---
!
      DO 300 M = 1,NSPSX
        ISTART = 1
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
         CALL LCASE( CHDUM )
        VARB = 'Lithology Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        DO 250 NSP = 1,NSPS
          IF( SPNMS(NSP).EQ.ADUM ) GOTO 260
  250   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Solid-Species Mineral Name: ' // &
          ADUM(1:NCH)
        CALL WRMSGS( INDX )
  260   CONTINUE
        IF( ME.EQ.0 ) WRITE (IWR,'(/,2A)') &
          ' Solid-Species Mineral Name: ', ADUM(1:NCH)
!
!---    Read initial mineral (solid species) area  ---
!
        VARB = 'Initial Solid-Species Mineral Specific Area'
        UNTS = 'm^2/kg'
        IUNM = 2
        IUNKG = -1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RS_SX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF( ME.EQ.0 ) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH), ': ',RS_SX(1)
        INDX = 0
        CALL RDUNIT(UNTS,RS_SX(1),INDX)
        IF( ME.EQ.0 ) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
          RS_SX(1),', m^2/kg)'
!
!---    Read initial mineral volume fraction ---
!
        VARB = 'Initial Solid-Species Mineral Volume Fraction'
        UNTS = 'null'
        INDX = NSP
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RS_SX(2))
        IF( ME.EQ.0 ) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RS_SX(2)
!
!---    Load current mineral volume fraction ---
!
        VARB = 'Current Solid-Species Mineral Volume Fraction'
        UNTS = 'null'
        ISTART = ISTX
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RS_SX(3))
        IF( ME.EQ.0 ) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ', &
          RS_SX(3)
!
!---    Restart overwrite option ---
!
        ISP_OWX = 0
        CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(ADUM(1:),'overwrite').NE.0 ) ISP_OWX = 1
        ENDIF
!
!---    Map rock/soil types to nodes  ---
!
        DO 270 NX = 1,NUM_NODES
          IF( IROCK.EQ.IZX(NX) ) THEN
            RS_S(1,NSP,NX) = RS_SX(1)
            RS_S(2,NSP,NX) = RS_SX(2)
            RS_S(3,NSP,NX) = RS_SX(3)
            ISP_OW(NSP,NX) = ISP_OWX
          ENDIF
  270   CONTINUE
  300 CONTINUE
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( N.LT.NROCKX.AND.ME.EQ.0 ) WRITE(IWR,'(/)')
      GOTO 50
 500  CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDLITH group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDPTZR

!----------------------Authors-----------------------------------------!
!
!     Written by A. Felmy, from GMIN
!     Last Modified by VL Freedman, PNNL, 13 March 2007.
!
!
! This routine reads in the necessary data for the chemical equilibrium 
! model. Two input files are required.  Modified 03/13/07 by VL Freedman 
! to change inputread based on id. no. to species name.The comp.dat 
! database contains the species charge atomic no. etc
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
      USE PTZRCOEF
      USE PTZR
      USE FDVP
      USE FDVP
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64  INTMP

      REAL*8 TMIN(LSPL),TMAX(LSPL)
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDPTZR'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
        '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      LUN2 = 8
      OPEN(UNIT=LUN2,FILE='comp.dat',STATUS='OLD')
!
!---- Allocate arrays needed for calculating Pitzer activity
!
      IF( .NOT.ALLOCATED(IDD) )ALLOCATE( IDD(LSPL) )
      IF( .NOT.ALLOCATED(JPA) )ALLOCATE( JPA(LANI) )
      IF( .NOT.ALLOCATED(JPC) )ALLOCATE( JPC(LCAT) )
      IF( .NOT.ALLOCATED(JPN) )ALLOCATE( JPN(LNEU) )
      IF( .NOT.ALLOCATED(B0) )ALLOCATE( B0(LCAT,LANI) )
      IF( .NOT.ALLOCATED(B1) )ALLOCATE( B1(LCAT,LANI) )
      IF( .NOT.ALLOCATED(B2) )ALLOCATE( B2(LCAT,LANI) )
      IF( .NOT.ALLOCATED(CMXX) )ALLOCATE( CMXX(LCAT,LANI) )
      IF( .NOT.ALLOCATED(TCC) )ALLOCATE( TCC(LNCF) )
      IF( .NOT.ALLOCATED(TAA) )ALLOCATE( TAA(LNAF) )
      IF( .NOT.ALLOCATED(PSIC) )ALLOCATE( PSIC(LNCF,LANI) )
      IF( .NOT.ALLOCATED(PSIA) )ALLOCATE( PSIA(LNAF,LCAT) )
      IF( .NOT.ALLOCATED(ALAMB) )ALLOCATE( ALAMB(LNEU,LANI) )
      IF( .NOT.ALLOCATED(CLAMB) )ALLOCATE( CLAMB(LNEU,LCAT) )
      IF( .NOT.ALLOCATED(ELAMB) )ALLOCATE( ELAMB(LNEU,LNEU) )
      IF( .NOT.ALLOCATED(HOLAMB) )ALLOCATE( HOLAMB(LNNF,LANI) )
      IF( .NOT.ALLOCATED(BPPR) )ALLOCATE( BPPR(LCAT,LANI) )
      IF( .NOT.ALLOCATED(BPHI) )ALLOCATE( BPHI(LCAT,LANI) )
      IF( .NOT.ALLOCATED(BPR) )ALLOCATE( BPR(LCAT,LANI) )
      IF( .NOT.ALLOCATED(BMMX) )ALLOCATE( BMMX(LCAT,LANI) )
      IF( .NOT.ALLOCATED(ATB0) )ALLOCATE( ATB0(LCAT,LANI,8) )
      IF( .NOT.ALLOCATED(ATB1) )ALLOCATE( ATB1(LCAT,LANI,8) )
      IF( .NOT.ALLOCATED(ATB2) )ALLOCATE( ATB2(LCAT,LANI,8) )
      IF( .NOT.ALLOCATED(ATCMX) )ALLOCATE( ATCMX(LCAT,LANI,8) )
      IF( .NOT.ALLOCATED(ATNLAM) )ALLOCATE( ATNLAM(LNEU,LNEU,6) )
      IF( .NOT.ALLOCATED(ATCLAM) )ALLOCATE( ATCLAM(LNEU,LCAT,6) )
      IF( .NOT.ALLOCATED(ATALAM) )ALLOCATE( ATALAM(LNEU,LANI,6) )
      IF( .NOT.ALLOCATED(ATHLAM) )ALLOCATE( ATHLAM(LNNF,LANI,6) )
      IF( .NOT.ALLOCATED(ATTC) )ALLOCATE( ATTC(LNCF,6) )
      IF( .NOT.ALLOCATED(ATPC) )ALLOCATE( ATPC(LNCF,LANI,6) )
      IF( .NOT.ALLOCATED(ATTA) )ALLOCATE( ATTA(LNAF,6) )
      IF( .NOT.ALLOCATED(ATPA) )ALLOCATE( ATPA(LNAF,LCAT,6) )
      IF( .NOT.ALLOCATED(CTCPH) )ALLOCATE( CTCPH(LNCF) )
      IF( .NOT.ALLOCATED(CTC) )ALLOCATE( CTC(LNCF) )
      IF( .NOT.ALLOCATED(CTCPR) )ALLOCATE( CTCPR(LNCF) )
      IF( .NOT.ALLOCATED(CTCPPR) )ALLOCATE( CTCPPR(LNCF) )
      IF( .NOT.ALLOCATED(CTAPH) )ALLOCATE( CTAPH(LNAF) )
      IF( .NOT.ALLOCATED(CTA) )ALLOCATE( CTA(LNAF) )
      IF( .NOT.ALLOCATED(CTAPR) )ALLOCATE( CTAPR(LNAF) )
      IF( .NOT.ALLOCATED(CTAPPR) )ALLOCATE( CTAPPR(LNAF) )
      IF( .NOT.ALLOCATED(ETH) )ALLOCATE( ETH(LMCG*LMCG) )
      IF( .NOT.ALLOCATED(ETHP) )ALLOCATE( ETHP(LMCG*LMCG) )
      IF( .NOT.ALLOCATED(ETHP2) )ALLOCATE( ETHP2(LMCG*LMCG) )
      DO  I = 1,NSPL
        IDD(I) = 0
        TMIN(I) = 0.0
        TMAX(I) = 0.0
      ENDDO
!       
!--- Assume temperature at first node in domain
!
      IKH2O = 0
      TX = T(2,1) + 273.15
!
!--- First aphi
!
      APHI=.336901532-6.3210043D-04*TX+9.14252359/TX-1.35143986D-02*   &
            DLOG(TX)+2.26089488D-03/(TX-263.0)+1.92118597D-06*TX*TX  &
            +4.52586464D+01/(680.0-TX)

      IFOUND = 0
      NOTFOUND = 0
      I = 1

110   READ(LUN2,5009) IDTMP,INTMP,IZTMP

      CALL LCASE(INTMP)
!xyl
!
!--- Match in comp.dat is made based on name, not id no.
!--- Remove leading spaces from intmp in comp.dat.For minerals, switch to lower case.
!
!---  Eliminate trailing blank spaces  ---
!
       K = 64
 116   CONTINUE
       IF( ICHAR(SPNML(I)(K:K)).EQ.32 ) THEN
         K = K-1
         GOTO 116
       ENDIF
!
!--- Check to see if at end of comp.dat file.
!--- Check to see species which are not found in the comp.dat.
!--- ID number of notfound species are set to 299999<300000
      IF( IDTMP.LT.0 ) THEN
        IF( ME.EQ.0 ) WRITE(*,5020) SPNML(I)
        IDD(I) = 299999
        NOTFOUND = NOTFOUND+1
        GOTO 156
      END IF
!
!---  Eliminate leading blank spaces  ---
!
       J = 1
 115  CONTINUE
      IF( ICHAR(INTMP(J:J)).EQ.32 )THEN
        J = J+1
        GO TO 115
      END IF
!xyl
      IF( INTMP(J:20).EQ.SPNML(I)(1:20) ) THEN
         IFOUND=IFOUND+1
         IDD(I)=IDTMP
         IF(IDD(I).EQ.1080) IKH2O = I
      END IF
!  
!--- Now count cations anions and neutrals
!--- and set up pointers
!  
  156   CONTINUE
      IF( INTMP(J:20).EQ.SPNML(I)(1:20) ) THEN
        IF( I.GE.1 ) THEN
       
          IF(SP_L(1,I).GT.0.AND.IDD(I).NE.101000.AND.IDD(I).LT.300000) &
             THEN
            NCC=NCC+1
            JPC(NCC)=I
            GO TO 155
          END IF
          IF( SP_L(1,I).LT.0.AND.IDD(I).LT.300000 ) THEN
            NA=NA+1
            JPA(NA)=I
            GO TO 155
          END IF
          IF(SP_L(1,I).EQ.0.0D0.AND.I.LE.NSPL.and.IDD(I).LT.300000) THEN
            NNN = NNN+1
            JPN(NNN)=I
          END IF
        END IF
  
  155   CONTINUE
  
        IF( IFOUND+NOTFOUND.EQ.NSPL ) GO TO 120
        REWIND(LUN2)
        I=I+1
        GO TO 110
      END IF

      GO TO 110

120   CONTINUE

      IF( IKH2O.EQ.0 ) THEN
        CHMSG = 'H2O Must Be Included As Species in Pitzer Formulation '
        INDX = 1
        IMSG = INDX
        CALL WRMSGS( INDX )
      ENDIF

      CALL PTZRP

      CLOSE(LUN2)

5001  FORMAT(4X,'ID',6X,'NAME',17X,'Z',3X,'tmin',4X,'tmax')
5003  FORMAT(10X,'AQUEOUS SPECIES'/)
5004  FORMAT(/,10X,'SOLID PHASES'/)
5009  FORMAT(I6,A20,1X,I2)
5011  FORMAT(1X,I6,5X,A20,F3.0,F7.3,1X,F7.3,1x,F7.3)
5020  FORMAT(1X,'Species not found in data (comp.dat) file:  ',A20)
5031  FORMAT(/12X,'t = ',f7.2,1X,'aphi = ',F7.4/)
6040  FORMAT(/' ** TOO MANY SPECIES **')
6045  FORMAT(/' ** TOO MANY CATIONS **')
6050  FORMAT(/' ** TOO MANY ANIONS  **')
6055  FORMAT(/' ** TOO MANY NEUTRALS *')

!
!---  End of RDPTZR  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END 

!----------------------Subroutine--------------------------------------!
      SUBROUTINE PTZRP
!
!----------------------Authors-----------------------------------------!
!
!     Written by A. Felmy, from GMIN
!     Last Modified by VL Freedman, PNNL, 13 March 2007.
!
!
! This subroutine reads in the necessary pitzer parameters for
! a non-ideal solution model. The parameters are stored in three
! separate files.  In this version the parameters are parameterized
! as a function of temperature.
!
!     file lun1 contains the single electrolyte parameters
!     file lun2 contains the theta and psi parameters
!     file lun3 contains the lambdas and higher order lambdas
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZRCOEF
      USE PTZR
      USE FILES
      USE REACT
!
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!

!
!----------------------Common Blocks-----------------------------------!
!

!
!----------------------Type Declarations-------------------------------!
!

      INTEGER idtmp(3,2),idtmp2(3)
      CHARACTER*64 binfile,ternfile,lamfile
!
!----------------------Executable Lines--------------------------------!
!
!     ISUB_LOG = ISUB_LOG+1
!     SUB_LOG(ISUB_LOG) = '/PTZRP'
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/PTZRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Open Files
!
      !TERNFILE(1:) = ''
      BINFILE = 'binaryt.dat'
      TERNFILE = 'ternaryt.dat'
      LAMFILE = 'lambdat.dat'
      LUN1=9
      LUN2=10
      LUN3=11
      OPEN(UNIT=LUN1,FILE=binfile,STATUS='OLD')
      OPEN(UNIT=LUN2,FILE=ternfile,STATUS='OLD')
      OPEN(UNIT=LUN3,FILE=lamfile,STATUS='OLD')
!
!---  Initialize variables
!
      AT1 = 0.0
      AT2 = 0.0
      AT3 = 0.0
      AT4 = 0.0
      AT5 = 0.0
      AT6 = 0.0
      AT7 = 0.0
      AT8 = 0.0
!
!--- Temperature is in T(2,N)
!
      TK = 2.7315D+02 + 2.5D+01
      TEMPC = TK-2.7315D+02
!     TEMPC = 25.
!
!--- Now read in the single electrolyte parameters
!

  100 READ(LUN1,5000) ID1,ID2,INDX,INDXT,AT1,AT2,AT3, &
                      AT4,AT5,AT6,AT7,AT8,TTMIN,TTMAX
5000  FORMAT(I6,1X,I6,I2,I2,8F15.7,F7.3,1X,F7.3)

      IF( ID1.EQ.0 ) GO TO 200
      IF( INDXT.EQ.1.AND.TEMPC.GT.TTMAX) GO TO 100
      IF( INDXT.EQ.2.AND.TEMPC.LT.TTMIN) GO TO 100
      TBB = AT1+AT2*TK+AT3/TK+AT4*DLOG(TK)+AT5/(TK-263.0)+AT6*TK*TK &
         +AT7/(680.0-TK)+AT8/(TK-227.0)
      DO I=1,NCC
        IF( IDD(JPC(I)).EQ.ID1 )THEN
          DO J=1,NA
            IF(IDD(JPA(J)).EQ.ID2) THEN
              IF( INDX.EQ.1 ) THEN
                B0(I,J)=TBB
                ATB0(I,J,1) = AT1
                ATB0(I,J,2) = AT2
                ATB0(I,J,3) = AT3
                ATB0(I,J,4) = AT4
                ATB0(I,J,5) = AT5
                ATB0(I,J,6) = AT6
                ATB0(I,J,7) = AT7
                ATB0(I,J,8) = AT8
                IF( TEMPC.GT.TTMAX )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5018)TEMPC,TTMAX, &
                    SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5019)TEMPC,TTMIN, &
                    SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
5017            FORMAT(/'*** WARNING ****')
5018            FORMAT('Input temperature',1x,f7.3,1x,'exceeds max  &
       database temp (',F7.3,' ) for the single electrolyte parameters &
       (b0) of', 1X,A10,'and',1X,A10)
5019            FORMAT('Input temperature',1x,f7.3,1x,'is less than &
       min databasetemp (',F7.3,' ) for the single electrolyte  &
       parameters (b0) of',  1X,A10,'and',1X,A10)
              END IF

              IF( INDX.EQ.2 )THEN
                B1(I,J)=TBB
                ATB1(I,J,1) = AT1
                ATB1(I,J,2) = AT2
                ATB1(I,J,3) = AT3
                ATB1(I,J,4) = AT4
                ATB1(I,J,5) = AT5
                ATB1(I,J,6) = AT6
                ATB1(I,J,7) = AT7
                ATB1(I,J,8) = AT8
                IF( TEMPC.GT.TTMAX )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5021)TEMPC,TTMAX, &
                    SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5022)TEMPC,TTMIN, &
                    SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
5021            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max &
       database temp (',f7.3,' ) for the single electrolyte &
       parameters (b1) of', 1X,A10,'and',1X,A10)
5022            FORMAT('Input temperature',1X,F7.3,1X,'is less than min &
        database temp (',F7.3,' ) for the single electrolyte parameters &
        (b1) of', 1X,A10,'and',1X,A10)
              END IF
              
              IF( INDX.EQ.3 )THEN
                B2(I,J)=TBB
                ATB2(I,J,1) = AT1
                ATB2(I,J,2) = AT2
                ATB2(I,J,3) = AT3
                ATB2(I,J,4) = AT4
                ATB2(I,J,5) = AT5
                ATB2(I,J,6) = AT6
                ATB2(I,J,7) = AT7
                ATB2(I,J,8) = AT8
                IF( TEMPC.GT.TTMAX )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5023)TEMPC,TTMAX, &
                    SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5024)TEMPC,TTMIN, &
                    SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
5023            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max &
       database temp (',F7.3,' ) for the single electrolyte parameters &
        (b2) of',  1X,a10,'and',1X,A10)
5024            FORMAT('Input temperature',1X,F7.3,1x,'is less than min &
        database temp (',F7.3,' ) for the single electrolyte parameters &
        (b2) of', 1X,A10,'and',1X,A10)
              END IF
        
              IF( INDX.EQ.4 )THEN
                CMXX(I,J)=TBB
                ATCMX(I,J,1) = AT1
                ATCMX(I,J,2) = AT2
                ATCMX(I,J,3) = AT3
                ATCMX(I,J,4) = AT4
                ATCMX(I,J,5) = AT5
                ATCMX(I,J,6) = AT6
                ATCMX(I,J,7) = AT7
                ATCMX(I,J,8) = AT8
                IF( TEMPC.GT.TTMAX )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5025)TEMPC,TTMAX, &
                    SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5026)TEMPC,TTMIN, &
                    SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
5025            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max  &
       database temp (',f7.3,' ) for the single electrolyte parameters &
        (cmxx) of',  1X,A10,'and',1X,A10) 
5026            FORMAT('Input temperature',1X,F7.3,1X,'is less than min &
       database temp (',F7.3,' ) for the single electrolyte parameters &
       (cmxx) of', 1X,A10,'and',1X,A10)
              END IF
              GO TO 100
            END IF
          ENDDO
          GOTO 100
        END IF
      ENDDO
      GOTO 100

200   CONTINUE
210   CONTINUE

      DO i=1,3
        IDTMP2(I)=0
        DO J=1,2
          IDTMP(I,J)=0
        END DO
      END DO

      READ(LUN3,5011) ID1,ID2,ID3A,A1T,A2T,A3T,A4T,A5T,A6T,TTMIN,TTMAX
5011  FORMAT(I6,1X,I6,1X,I6,1X,6F15.7,F7.3,1X,F7.3)

      IF( ID1.NE.0 )THEN
        TLAMBDA = A1T+A2T*TK+A3T/TK+A4T*dlog(TK)+A5T/ &
                  (TK-263.0)+A6T*TK*TK
        IDTMP2(1)=ID1
        IDTMP2(2)=ID2
        IDTMP2(3)=ID3A
        IF(ID3A.NE.0)THEN
          NJ=3
        ELSE
          NJ=2
        END IF
        DO J=1,NJ
          DO I=1,NNN
            IF( IDD(JPN(I)).EQ.IDTMP2(J) )THEN
              IDTMP(J,1)=1
              IDTMP(J,2)=i
              J1 = JPN(I)
            END IF
          END DO
          DO I=1,NCC
            IF( IDD(JPC(I)).EQ.IDTMP2(J) )THEN
              IDTMP(J,1)=2
              IDTMP(J,2)=i
              J2 = JPC(I)
            END IF
          END DO
          DO I=1,NA
            IF( IDD(JPA(I)).EQ.IDTMP2(J) )THEN
              IDTMP(J,1)=3
              IDTMP(J,2)=I
              J3 = JPA(I)
            END IF
          END DO
        END DO
        
        IF(( IDTMP(1,1).EQ.0).OR.(IDTMP(2,1).EQ.0) )THEN
          GO TO 210
        ELSE IF( IDTMP(3,1).EQ.0.and.NJ.EQ.3 )THEN
          GO TO 210
        END IF
        
  
        IF( TEMPC.GT.TTMAX.OR.TEMPC.LT.TTMIN )THEN
          IF( ME.EQ.0 ) WRITE(IWR,5017)
          IF( ME.EQ.0 ) WRITE(IWR,*) &
            'Temperature out of range in lambdat.dat neutral &
      ion parameters for:'
          IF( NJ.EQ.2 )THEN
            IF( ME.EQ.0 ) WRITE(IWR,5029)SPNML(J1),SPNML(J2)
          ENDIF
          IF( NJ.EQ.3 )THEN
            IF( ME.EQ.0 ) WRITE(IWR,5029)SPNML(J1),SPNML(J2),SPNML(J3)
          ENDIF
5029      FORMAT(3(1x,a10))
        END IF
  
        IF( NJ.EQ.2 )THEN
          IF(IDTMP(1,1).EQ.1.AND.IDTMP(2,1).EQ.1)THEN
            ELAMB(IDTMP(1,2),IDTMP(2,2)) = TLAMBDA
            ATNLAM(IDTMP(1,2),IDTMP(2,2),1) = A1T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),2) = A2T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),3) = A3T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),4) = A4T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),5) = A5T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),6) = A6T
          ELSEIF( IDTMP(1,1).EQ.1.AND.IDTMP(2,1).EQ.2 )THEN
            CLAMB(IDTMP(1,2),IDTMP(2,2)) = TLAMBDA
            ATCLAM(IDTMP(1,2),IDTMP(2,2),1) = A1T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),2) = A2T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),3) = A3T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),4) = A4T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),5) = A5T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),6) = A6T
          ELSEIF( IDTMP(1,1).EQ.1.AND.idtmp(2,1).EQ.3 )THEN
            ALAMB(IDTMP(1,2),IDTMP(2,2)) = TLAMBDA
            ATALAM(IDTMP(1,2),IDTMP(2,2),1) = A1T
            ATALAM(IDTMP(1,2),IDTMP(2,2),2) = A2T
            ATALAM(IDTMP(1,2),IDTMP(2,2),3) = A3T
            ATALAM(IDTMP(1,2),IDTMP(2,2),4) = A4T
            ATALAM(IDTMP(1,2),IDTMP(2,2),5) = A5T
            ATALAM(IDTMP(1,2),IDTMP(2,2),6) = A6T
          END IF
        ELSE
          NT=(IDTMP(1,2)-1)*NCC+IDTMP(2,2)
          HOLAMB(NT,IDTMP(3,2)) = TLAMBDA
          ATHLAM(NT,IDTMP(3,2),1) = A1T
          ATHLAM(NT,IDTMP(3,2),2) = A2T
          ATHLAM(NT,IDTMP(3,2),3) = A3T
          ATHLAM(NT,IDTMP(3,2),4) = A4T
          ATHLAM(NT,IDTMP(3,2),5) = A5T
          ATHLAM(NT,IDTMP(3,2),6) = A6T
        END IF
        GO TO 210
      END IF
!
!--- Now read in theta's and psi's
!--- First cation-cation terms
!
      IF( NCC.LT.2 ) GOTO 360
      NT=0
      DO I=1,NCC
        DO J=I+1,NCC
          NT=NT+1
 310      CONTINUE
          READ(LUN2,5010) ID1,ID2,ID3A,INDXT,A1T,A2T,A3T,A4T,A5T,A6T, &
                         TTMIN,TTMAX
5010      FORMAT(I6,1x,I6,1X,I6,1X,I2,6f15.7,F7.3,1X,F7.3)
          IF( ID1.EQ.0 ) GOTO 330
          IF( IDD(JPC(I)).EQ.ID1.OR.IDD(JPC(J)).EQ.ID1 )THEN
            IF( IDD(JPC(I)).EQ.ID2.OR.IDD(JPC(J)).EQ.ID2 )THEN
              IF( ID3A.EQ.0 )THEN
                IF( INDXT.EQ.1.AND.TEMPC.GT.TTMAX ) GOTO 310
                IF( INDXT.EQ.2.AND.TEMPC.LT.TTMIN ) GOTO 310
                TCC(NT)=A1T+A2T*TK+A3T/TK+A4T*DLOG(TK)+A5T/(TK-263.0) &
                        +A6T*TK*TK
                ATTC(NT,1) = A1T
                ATTC(NT,2) = A2T
                ATTC(NT,3) = A3T
                ATTC(NT,4) = A4T
                ATTC(NT,5) = A5T
                ATTC(NT,6) = A6T
                IF( TEMPC.GT.TTMAX )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5027)TEMPC,TTMAX, &
                    SPNML(JPC(I)),SPNML(JPC(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5028)TEMPC,TTMIN, &
                    SPNML(JPC(I)),SPNML(JPC(J))
                ENDIF
5027            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max  &
       database temp (',F7.3,' ) for the ternary electrolyte parameters &
       (theta-c) of', 1X,A10,'and',1X,A10)
5028            FORMAT('Input temperature',1X,F7.3,1X,'is less than min &
       database temp (',F7.3,' ) for the ternary electrolyte parameters &
       (theta-c) of',  1X,A10,'and',1X,A10)
              ELSE
                DO N=1,NA
                  IF( IDD(JPA(N)).EQ.ID3A )THEN
                    IF(INDXT.EQ.1.AND.TEMPC.GT.TTMAX ) GO TO 310
                    IF(INDXT.EQ.2.AND.TEMPC.LT.TTMIN ) GO TO 310
                    PSIC(NT,N)=A1T+A2T*TK+A3T/TK+A4T*DLOG(TK)+ &
                      A5T/(TK-263.0)+A6T*TK*TK
                    ATPC(NT,N,1) = A1T
                    ATPC(NT,N,2) = A2T
                    ATPC(NT,N,3) = A3T
                    ATPC(NT,N,4) = A4T
                    ATPC(NT,N,5) = A5T
                    ATPC(NT,N,6) = A6T
                    IF( TEMPC.GT.TTMAX )THEN
                      IF( ME.EQ.0 ) WRITE(IWR,5017)
                      IF( ME.EQ.0 ) WRITE(IWR,5031)TEMPC,TTMAX, &
                        SPNML(JPC(I)),SPNML(JPC(J)),SPNML(JPA(N))
                    ENDIF
                    IF( TEMPC.LT.TTMIN )THEN
                      IF( ME.EQ.0 ) WRITE(IWR,5017)
                      IF( ME.EQ.0 ) WRITE(IWR,5032)TEMPC,TTMIN, &
                        SPNML(JPC(I)),SPNML(JPC(J)),SPNML(JPA(N))
                    ENDIF
5031            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max &
       database temp (',F7.3,' ) for the ternary electrolyte parameters &
       (psi-c) of', 1X,A10,'and',1X,A10,1X, 'and',1X,A10)
5032                FORMAT('Input temperature',1X,F7.3,1X,'is less than &
       min database temp (',F7.3,' ) for the ternary electrolyte  &
       parameters (psi-c) of',  1X,A10,'and',1X,A10,1X,'and',1X,A10)
                    GO TO 310
                  ENDIF
                ENDDO
              ENDIF
              GO TO 310
            ENDIF
          ENDIF
          GO TO 310
330       REWIND(LUN2)
        ENDDO
      ENDDO

360   CONTINUE
!
!--- Now anion-anion terms
!
      IF( NA.LT. 2 )GO TO 460
      REWIND(lun2)
      NT = 0
      DO I=1,NA
        DO J=I+1,NA
          NT = NT+1
410       read(LUN2,5010) ID1,ID2,ID3A,INDXT,A1T,A2T,A3T,A4T,A5T,A6T, &
                           TTMIN,TTMAX
          IF( ID1.EQ.0 )GO TO 430
          IF(IDD(JPA(I)).EQ.ID1.OR.IDD(JPA(J)).EQ.ID1 )THEN
            IF( IDD(JPA(I)).EQ.ID2.OR.IDD(JPA(J)).EQ.ID2 )THEN
              IF( ID3A.EQ. 0 )THEN
                IF( INDXT.EQ.1.AND.TEMPC.GT.TTMAX ) GO TO 410
                IF( INDXT.EQ.2.AND.TEMPC.LT.TTMIN ) GO TO 410
                TAA(NT)= A1T+A2T*TK+A3T/TK+A4T*DLOG(TK)+A5T/(TK-263.0) &
                        +A6T*TK*TK
                ATTA(NT,1) = A1T
                ATTA(NT,2) = A2T
                ATTA(NT,3) = A3T
                ATTA(NT,4) = A4T
                ATTA(NT,5) = A5T
                ATTA(NT,6) = A6T
                IF( TEMPC.GT.TTMAX )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5033)TEMPC,TTMAX, &
                    SPNML(JPA(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  IF( ME.EQ.0 ) WRITE(IWR,5017)
                  IF( ME.EQ.0 ) WRITE(IWR,5034)TEMPC,TTMIN, &
                    SPNML(JPA(I)),SPNML(JPA(J))
                ENDIF
5033            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max &
       database temp (',F7.3,' ) for the ternary electrolyte parameters &
       (theta-a) of', 1X,A10,'and',1X,A10)
5034            FORMAT('Input temperature',1X,F7.3,1X,'is less than min &
       database temp (',F7.3,' ) for the ternary electrolyte parameters &
       (theta-a) of',  1X,A10,'and',1X,A10)
              ELSE
                DO N=1,NCC
                  IF( IDD(JPC(N)).EQ.ID3A )THEN
                    IF( INDXT.EQ.1.AND.TEMPC.GT.TTMAX )GO TO 410
                    IF( INDXT.EQ.2.AND.TEMPC.LT.TTMIN )GO TO 410
                    PSIA(NT,N) = A1T+A2T*TK+A3T/TK+A4T*DLOG(TK)+A5T/ &
                                 (TK-263.0)+A6T*TK*TK
                    ATPA(NT,N,1) = A1T
                    ATPA(NT,N,2) = A2T
                    ATPA(NT,N,3) = A3T
                    ATPA(NT,N,4) = A4T
                    ATPA(NT,N,5) = A5T
                    ATPA(NT,N,6) = A6T
                    IF( TEMPC.GT.TTMAX )THEN
                      IF( ME.EQ.0 ) WRITE(IWR,5017)
                      IF( ME.EQ.0 ) WRITE(IWR,5035)TEMPC,TTMAX, &
                        SPNML(JPA(I)),SPNML(JPA(J)),SPNML(JPC(N))
                    ENDIF
                    IF( TEMPC.LT.TTMIN )THEN
                      IF( ME.EQ.0 ) WRITE(IWR,5017)
                      IF( ME.EQ.0 ) WRITE(IWR,5036)TEMPC,TTMIN, &
                        SPNML(JPA(I)),SPNML(JPA(J)),SPNML(JPC(N))
                    ENDIF
5035                FORMAT('Input temperature',1X,F7.3,1X,'exceeds max &
      database temp (',F7.3,' ) for the ternary electrolyte parameters &
      (psi-a) of',  1X,A10,'and',1X,A10,1X,'and',1X,A10)
5036                FORMAT('Input temperature',1X,F7.3,1x,'is less than &
       min database temp (',F7.3,' ) for the ternary electrolyte &
       parameters (psi-a) of',  1X,A10,'and',1X,A10,1X,'and',1X,A10)
                    GO TO 410
                  END IF
                END DO
              END IF
              GO TO 410
            END IF
          END IF
          GO TO 410
 430      REWIND(lun2)
        END DO
      END DO

460   CONTINUE
!
!-- Now output the pitzer parameters
!
      IF( ME.EQ.0 ) WRITE(IWR,5020)
5020  FORMAT(/'       non-ideal electrolyte parameters')
      IF( ME.EQ.0 ) WRITE(IWR,5030)
5030  FORMAT(/'       single electrolyte parameters'/)

      DO I=1,NCC
        DO J=1,NA
          IF( ME.EQ.0 )WRITE(IWR,5050) SPNML(JPC(I)),SPNML(JPA(J)), &
            B0(I,J),B1(I,J), B2(I,J),CMXX(I,J)
          TMP=SP_L(1,JPC(I))*SP_L(1,JPA(J))
          TMP=DABS(TMP)
          CMXX(I,J)=CMXX(I,J)/(2.0D0*DSQRT(TMP))
        END DO
      END DO
5050  FORMAT(2A20,4(1X,F10.5))
!
!--- Now write out the theta's and psi's
!
      IPRTI=8
      IF( ME.EQ.0 ) WRITE(IWR,5070)
5070  FORMAT(/'       ternary electrolyte parameters')
!
!--- First cation-cation-anion
!
      IF(NCC.LT.2) GO TO 620
      M = 1
      IPRT1 = 1
615   IPRT2 = M*IPRTI
      IF( NA.LE.IPRT2 )IPRT2 = NA
      IF( ME.EQ.0 ) WRITE(IWR,5080) (SPNML(JPA(K)),K=IPRT1,IPRT2)
5080  FORMAT(/64X,20A20)
      
      NT = 0
      DO i=1,ncc
        DO j=i+1,ncc
         nt=nt+1
         IF( ME.EQ.0 ) WRITE(IWR,5060) SPNML(JPC(I)),SPNML(JPC(J)), &
           TCC(NT),(PSIC(NT,K),K=IPRT1,IPRT2)
        END DO
      END DO
5060  FORMAT(2A20,1X,F10.5,10X,20(F10.5,10X))

      M = M + 1
      IPRT1 = IPRT2 + 1
      IF( IPRT2.LT.NA )GO TO 615

620   CONTINUE
!
!--- Now anion-anion-cation
!
      IF( NA.LT.2 )GO TO 720

      M = 1
      IPRT1=1
715   IPRT2 = M*IPRTI

      IF( NCC.LE.IPRT2 )IPRT2 = NCC
      IF( ME.EQ.0 ) WRITE(IWR,5081) (SPNML(JPC(K)),K=IPRT1,IPRT2)
5081  FORMAT(/64X,20A20)
      NT = 0
      DO I=1,NA
        DO J=I+1,NA
          NT = NT+1
          IF( ME.EQ.0 ) WRITE(IWR,5060) SPNML(jpa(i)), &
            SPNML(jpa(j)),TAA(NT),(PSIA(NT,K),K=IPRT1,IPRT2)
        END DO
      END DO
      M = M+1
      IPRT1 = IPRT2+1
      IF( IPRT2.lt.NCC ) GO TO 715

720   CONTINUE

      IF( NNN.GT.0 )THEN
        IF( ME.EQ.0 ) WRITE(IWR,5090)
5090    FORMAT(/'       neutral ion parameters')
        IF( ME.EQ.0 ) WRITE(IWR,5095) (SPNML(JPN(K)),K=1,NNN)
5095    FORMAT(24X,10A20)
        DO I=1,NCC
          IF( ME.EQ.0 )WRITE(IWR,5100) SPNML(JPC(I)),(CLAMB(J,I),J=1,NNN)
        END DO
        DO I=1,NA
          IF( ME.EQ.0 ) WRITE(IWR,5100) SPNML(JPA(I)),(ALAMB(J,I),J=1,NNN)
        END DO
  
        DO I=1,NNN
          IF( ME.EQ.0 ) WRITE(IWR,5100) SPNML(JPN(I)),(ELAMB(J,I),J=1,NNN)
        END DO
5100    FORMAT(A20,1X,10(F10.5,10X))
!
!--- Now higher order lambdas
!
        IF( ME.EQ.0 )WRITE(IWR,5110)
5110    FORMAT(/'        higher order lambdas')
        M = 1
        IPRT1 = 1
  815   IPRT2 = M*IPRTI
  
        IF( NA.LE.IPRT2) IPRT2 = NA
        IF( ME.EQ.0 ) WRITE(IWR,5096) (SPNML(JPA(K)),K=IPRT1,IPRT2)
5096    FORMAT(/44X,10A20)
        NT = 0
        DO I=1,NNN
          DO J=1,NCC
            NT=NT+1
            IF( ME.EQ.0 ) WRITE(IWR,5065) SPNML(JPN(I)),SPNML(JPC(J)), &
                          (HOLAMB(NT,K),K=IPRT1,IPRT2)
5065  FORMAT(2A20,1X,F10.5,20(10X,F10.5))
          END DO
        END DO
        M = M+1
        IPRT1 = IPRT2+1
        IF(IPRT2.LT.NA) go to 815
      END IF


      CLOSE(LUN1)
      CLOSE(LUN2)
      CLOSE(LUN3)

!
!---  End of PTZRP group ---
!
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSDSP
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
!     Read solid species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 December 2004.
!     Last Modified by MD White, PNNL, 7 December 2004.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,UNTS
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDSDSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Solid Species Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of solid species  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Solid Species'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPS )
      IF( NSPS.GT.LSPS ) THEN
        INDX = 5
        CHMSG = 'Number of Solid Species > Parameter LSPS'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the solid species  ---
!
!      IF(.NOT.ALLOCATED(SPNMS)) ALLOCATE(SPNMS(LSPS))
      IF(.NOT.ALLOCATED(SP_S)) ALLOCATE(SP_S(2,LSPS))
      DO 500 NSP = 1,NSPS
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Solid Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMS(NSP))
        DO 100 M = 1,NSP-1
          IF( SPNMS(M).EQ.SPNMS(NSP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Solid Species Name: ' // SPNMS(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
        DO 110 M = 1,NSPL
          IF( SPNMS(NSP).EQ.SPNML(M) ) THEN
            INDX = 4
            CHMSG = 'Solid Species Name = Aqueous Species Name: ' // &
              SPNMS(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  110   CONTINUE
        IF( ME.EQ.0 ) WRITE (IWR,'(/,2A)') ' Solid Species Name: ', &
          SPNMS(NSP)
!
!---    Read solid species mass density  ---
!
        VARB = 'Solid Species Mass Density'
        UNTS = 'kg/m^3'
        IUNM = -3
        IUNKG = 1
        IDFLT = 1
        SP_S(1,NSP) = 2.65D+3
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_S(1,NSP))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF( ME.EQ.0 ) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',&
          UNTS(1:NCH), ': ',SP_S(1,NSP)
        INDX = 0
        CALL RDUNIT(UNTS,SP_S(1,NSP),INDX)
        IF( ME.EQ.0 ) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
          SP_S(1,NSP),', kg/m^3)'
!
!---    Read solid species molecular weight  ---
!
        VARB = 'Solid Species Molecular Weight'
        UNTS = 'g/mol'
        IUNMOL = -1
        IUNKG = 1
        IDFLT = 1
        SP_S(2,NSP) = 0.D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_S(2,NSP))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF( ME.EQ.0 ) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH), ': ',SP_S(2,NSP)
        INDX = 0
        CALL RDUNIT(UNTS,SP_S(2,NSP),INDX)
        IF( ME.EQ.0 ) WRITE(IWR,'(A,1PE11.4,A)') ' (',SP_S(2,NSP), &
          ', g/mol)'
!
!---  Read next solid species  ---
!
      IF( NSP.LT.NSPS.AND.ME.EQ.0 ) WRITE(IWR,'(/)')
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDSDSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSPLK
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
!     Link reactive species with pH and multifluid flow components.
!
!     ISPLK(1) <=> Aqueous pH
!     ISPLK(2) <=> Aqueous H2O
!     ISPLK(3) <=> Gas H2O
!     ISPLK(4) <=> Aqueous Air
!     ISPLK(5) <=> Gas Air
!     ISPLK(6) <=> Aqueous CO2
!     ISPLK(7) <=> Gas CO2
!     ISPLK(8) <=> Aqueous CH4
!     ISPLK(9) <=> Gas CH4
!     ISPLK(10) <=> Aqueous NaCl
!     ISPLK(11) <=> Solid NaCl
!     ISPLK(12) <=> Aqueous Oil
!     ISPLK(13) <=> NAPL Oil
!     ISPLK(14) <=> Gas Oil
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 December 2004.
!     Last Modified by MD White, PNNL, 26 January 2005.
!     $Id: eckechem.F,v 1.4 2011/09/09 17:15:36 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE FILES
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,UNTS,SPNMX
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDSPLK'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Reactive Species Link Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 ) WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of reactive species links  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reactive Species Links'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPLK )
!
!---  Loop over the number of reactive species links  ---
!
      DO 500 NSP = 1,NSPLK
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Reactive Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMX)
        NSPX = 0
        NSLX = 0
!
!---    Aqueous species, assign species index  ---
!
        DO 100 M = 1,NSPL
          IF( SPNML(M).EQ.SPNMX ) THEN
            NSPX = M
            GOTO 200
          ENDIF
  100   CONTINUE
!
!---    Solid species, assign species index  ---
!
        DO 110 M = 1,NSPS
          IF( SPNMS(M).EQ.SPNMX ) THEN
            NSPX = NSPL + M
            GOTO 200
          ENDIF
  110   CONTINUE
!
!---    Gas species, assign species index  ---
!
        DO 120 M = 1,NSPG
          IF( SPNMG(M).EQ.SPNMX ) THEN
            NSPX = NSPS + NSPL + M
            GOTO 200
          ENDIF
  120   CONTINUE
!
!---    Conservation component species,
!       assign negative solute index  ---
!
        DO 130 M = 1,NEQC
          IF( SPNMC(M).EQ.SPNMX ) THEN
            NSLX = NSOLU + M
            GOTO 200
          ENDIF
  130   CONTINUE
!
!---    Kinetic component species,
!       assign negative solute index  ---
!
        DO 140 M = 1,NEQK
          IF( SPNMK(M).EQ.SPNMX ) THEN
            NSLX = NSOLU + NEQC + M
            GOTO 200
          ENDIF
  140   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Reactive Species Name: ' // &
          SPNMX(1:NCH)
        CALL WRMSGS( INDX )
  200   CONTINUE
!
!---    Read reactive species link  ---
!
        VARB = 'Reactive Species Link: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
          IF( NSPX.EQ.0 .AND. NSLX.GT.0 ) THEN
            INDX = 4
            CHMSG = 'pH Linked to Component Species: ' // SPNMX(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          ISPLK(1) = NSPX
          IF( ME.EQ.0 )WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'pH <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'h2o').NE.0 .OR. &
          INDEX(ADUM(1:),'water').NE.0) .AND. &
          INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          ISPLK(2) = NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Aqueous H2O <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'h2o').NE.0 .OR. &
          INDEX(ADUM(1:),'water').NE.0) .AND. &
          INDEX(ADUM(1:),'gas').NE.0 ) THEN
          ISPLK(3) = NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Gas H2O <=>',SPNMX
        ELSEIF( INDEX(ADUM(1:),'air').NE.0 .AND. &
          INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          IF( NSPX.GT.0 .AND. NSLX.EQ.0 ) THEN
            INDX = 4
            CHMSG = 'Aqueous Air Linked to Non-Component Species: ' // &
              SPNMX(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          ISPLK(4) = NSLX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Aqueous Air <=>',SPNMX
        ELSEIF( INDEX(ADUM(1:),'air').NE.0 .AND. &
          INDEX(ADUM(1:),'gas').NE.0 ) THEN
          ISPLK(5) = NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Gas Air <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'co2').NE.0 .OR. &
          (INDEX(ADUM(1:),'carbon').NE.0 .AND. &
           INDEX(ADUM(1:),'dioxide').NE.0)) .AND. &
          INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          IF( NSPX.GT.0 .AND. NSLX.EQ.0 ) THEN
            INDX = 4
            CHMSG = 'Aqueous CO2 Linked to Non-Component Species: ' // &
              SPNMX(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          ISPLK(6) = NSLX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Aqueous CO2 <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'co2').NE.0 .OR. &
          (INDEX(ADUM(1:),'carbon').NE.0 .AND. &
          INDEX(ADUM(1:),'dioxide').NE.0)) .AND. &
          INDEX(ADUM(1:),'gas').NE.0 ) THEN
          ISPLK(7) = NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Gas CO2 <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'ch4').NE.0 .OR. &
         INDEX(ADUM(1:),'methane').NE.0) .AND. &
         INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          ISPLK(8) = NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Aqueous CH4 <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'ch4').NE.0 .OR. &
          INDEX(ADUM(1:),'methane').NE.0) .AND. &
          INDEX(ADUM(1:),'gas').NE.0 ) THEN
          ISPLK(9) = NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Gas CH4 <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'nacl').NE.0 .OR. &
          INDEX(ADUM(1:),'salt').NE.0) .AND. &
          INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          ISPLK(10) = NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            'Aqueous NaCl <=>',SPNMX
        ELSEIF( INDEX(ADUM(1:),'concentration').NE.0 ) THEN 
          ISPLK(14+NSP) = -NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            SPNMX, ' Concentration fixed '
        ELSEIF( INDEX(ADUM(1:),'activity').NE.0 ) THEN
          ISPLK(14+NSP) = -1000-NSPX
          IF( ME.EQ.0 ) WRITE (IWR,'(2A)') ' Reactive Species Link: ' // &
            SPNMX, ' Activity fixed '
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Reactive Species Link: ' // &
            ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read next reactive species link  ---
!
  500 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!
!---  End of RDSPLK group  ---
!
      RETURN
      END
