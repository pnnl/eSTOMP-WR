!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BCK_STP
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
!     Back step.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 6 June 2006.
!     Last Modified by MD White, PNNL, 6 June 2006.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE HYST
      USE GRID
!     USE FDVS
      USE FDVP
!     USE FDVGC
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/BCK_STP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
      ICSN = ICSN+ICSNX
      NSTEP = NSTEP-1
!
!---  Water Operational Mode  ---
!
      IF( IOM.EQ.1 ) THEN
        DO 10 N = 1,NUM_NODES
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SGT(2,N) = SGT(1,N)
          ASLMIN(2,N) = ASLMIN(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
          IPH(2,N) = IPH(1,N)
  10    CONTINUE
!
!---  Water-Air Operational Mode  ---
!
      ELSEIF( IOM.EQ.2 ) THEN
        DO 20 N = 1,NUM_NODES
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SGT(2,N) = SGT(1,N)
          ASLMIN(2,N) = ASLMIN(1,N)
          SG(2,N) = SG(1,N)
          XMLA(2,N) = XMLA(2,N)
          SN(2,N) = SN(1,N)
          XGO(2,N) = XGO(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
  20    CONTINUE
!
!---  H2O-NaCl-CO2 Operational Mode  ---
!
      ELSEIF( IOM.EQ.32 ) THEN
        DO 320 N = 1,NUM_NODES
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SGT(2,N) = SGT(1,N)
          ASLMIN(2,N) = ASLMIN(1,N)
          SG(2,N) = SG(1,N)
          XLA(2,N) = XLA(1,N)
!         YLS(2,N) = YLS(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
 320    CONTINUE
!
!---  WNE Operational Mode  ---
!
      ELSEIF( IOM.EQ.30 ) THEN
        DO 302 N = 1,NUM_NODES
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SG(2,N) = SG(1,N)
          PVW(2,N) = PVW(1,N)
          PVA(2,N) = PVA(1,N)
          XGA(2,N) = XGA(1,N)
          XGW(2,N) = XGW(1,N)
          XMGA(2,N) = XMGA(1,N)
          XMGW(2,N) = XMGW(1,N)
          XLA(2,N) = XLA(1,N)
          XLW(2,N) = XLW(1,N)
          XMLA(2,N) = XMLA(1,N)
          XMLW(2,N) = XMLW(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
!         DO 300 IGC = 1,NGC
!           PVC(IGC,2,N) = PVC(IGC,1,N)
!           XGC(IGC,2,N) = XGC(IGC,1,N)
!           XMGC(IGC,2,N) = XMGC(IGC,1,N)
!           XLC(IGC,2,N) = XLC(IGC,1,N)
!           XMLC(IGC,2,N) = XMLC(IGC,1,N)
!           DFLC(IGC,2,N) = DFLC(IGC,1,N)
!           DFGC(IGC,2,N) = DFGC(IGC,1,N)
!           HGC(IGC,2,N) = HGC(IGC,1,N)
! 300      CONTINUE
 302    CONTINUE
      ENDIF
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of BCK_STP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BDOT( ACTVX,SP_CX,DSP_CX,SLX,PORDX,RHOLX,TX,XLWX )
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
!     B-Dot or extended Debye-Huckel model for activity coefficient.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 December 2004.
!     Last Modified by MD White, PNNL, 21 December 2004.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE CONST
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
      REAL*8 ACTVX(LSPL+1,LSPL),SP_CX(LSPL),DSP_CX(LSPL)
      REAL*8 CPIX(LSPL+1)
      REAL*8 ACOF(5),BCOF(5),BDCOF(5)
      REAL*8 SACOF(4),SBCOF(4),SCCOF(4)
!
!----------------------Data Statements---------------------------------!
!
      SAVE ACOF,BCOF,BDCOF,SACOF,SBCOF,SCCOF
      SAVE ASMX,BSMX,CSMX,TSMI
      SAVE ACPX,BCPX,BDCPX
      DATA ACOF / 0.49463D+0,0.000398438D+0,9.9092D-6,-4.36244D-8, &
       1.09811D-10 /
      DATA BCOF / 0.325325D+0,0.000122253D+0,6.68944D-7,-2.57037D-9, &
       4.89847D-12 /
      DATA BDCOF / 0.0373904D+0,0.000191209D+0,-2.05222D-6, &
       1.31092D-8,-3.26031D-11 /
      DATA SACOF / 0.131678D+0,-0.000836829D+0,3.07179D-06, &
       1.46701D-09 /
      DATA SBCOF / -0.0186731D+0,0.00039022D+0,-2.62611D-06, &
       4.40918D-09 /
      DATA SCCOF / 0.00288841D+0,-6.70405D-05,5.65666D-07, &
       -1.34012D-09 /
      DATA TSMI / -1.D+3 /
!
!----------------------Executable Lines--------------------------------!
!
!     ISUB_LOG = ISUB_LOG+1
!     SUB_LOG(ISUB_LOG) = '/BDOT'
      SUBNMX = '/BDOT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Coefficients as a function of temperature, where
!     calculations are skipped for sequential calls to
!     subroutine with the same temperature  ---
!
      IF( ABS(TX-TSMI).GT.EPSL ) THEN
        ACPX = ACOF(5)
        BCPX = BCOF(5)
        BDCPX = BDCOF(5)
        DO 10 I = 4,1,-1
          ACPX = ACPX*TX + ACOF(I)
          BCPX = BCPX*TX + BCOF(I)
          BDCPX = BDCPX*TX + BDCOF(I)
   10   CONTINUE
        ASMX = SACOF(4)
        BSMX = SBCOF(4)
        CSMX = SCCOF(4)
        DO 20 I = 3,1,-1
          ASMX = ASMX*TX + SACOF(I)
          BSMX = BSMX*TX + SBCOF(I)
          CSMX = CSMX*TX + SCCOF(I)
   20   CONTINUE
      ENDIF
      TSMI = TX
!
!---  Ionic strength of the aqueous solution  ---
!
      DO 40 M = 1,NSPL+1
        CPIX(M) = 0.D+0
        SUM_MZ = 0.D+0
        SUM_M = 0.D+0
        DO 30 NSP = 1,NSPL
!
!---      Skip neutral species  ---
!
          IF( ABS(SP_L(1,NSP)).LT.EPSL ) GOTO 30
          IF( NSP.EQ.(M-1) ) THEN
            CLX = SP_CX(NSP) + DSP_CX(NSP)
          ELSE
            CLX = SP_CX(NSP)
          ENDIF
!
!---      Molarity in mol solute/m^3 aqueous
!         or mol solute/l aqueous  ---
!
          CMX = CLX/(SLX*PORDX)
!
!---      Molality in mol solute/kg water  ---
!
          CMX = CMX/(RHOLX*XLWX)
          SUM_M = SUM_M + CMX
          SUM_MZ = SUM_MZ + CMX*SP_L(1,NSP)
          CPIX(M) = CPIX(M) + CMX*(SP_L(1,NSP)**2)
   30   CONTINUE
!
!---    Correct for electrical neutrality  ---
!
        IF( ABS(SUM_MZ).GT.EPSL ) &
          CPIX(M) = CPIX(M) + (SUM_MZ**2)/SUM_M
        CPIX(M) = CPIX(M)*5.D-1
   40 CONTINUE
!
!---  Activity coefficients for aqueous species  ---
!
      DO 60 M = 1,NSPL+1
        DO 50 NSP = 1,NSPL
          IF( ABS(SP_L(1,NSP)).GT.EPSL ) THEN
!
!---        Activity coefficients for charged species  ---
!
            ACTVX(M,NSP) = -ACPX*(SP_L(1,NSP)**2)*SQRT(CPIX(M)) &
            /(1.D+0 + SP_L(2,NSP)*1.D+10*BCPX*SQRT(CPIX(M))) &
            + BDCPX*CPIX(M)
            ACTVX(M,NSP) = EXP(TOLN*ACTVX(M,NSP))
!
!---        Activity coefficients for electrically neutral species  ---
!
          ELSE
            ACTVX(M,NSP) = EXP(TOLN*(ASMX*CPIX(M) + BSMX*(CPIX(M)**2) + &
              CSMX*(CPIX(M)**3)))
          ENDIF
   50   CONTINUE
   60 CONTINUE
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of BDOT group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CECHEM( ACTVX,AJM,BJM,CX,SP_CX,N,NEQ,INDX )
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
!     Conservation Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!     Last Modified by MD White, PNNL, 7 January 2005.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FDVP
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
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 SP_CX(LSPR)
      REAL*8 CX(LEQC+LEQK)
      REAL*8 ACTVX(LSPL+1,LSPL)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/CECHEM'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Volumetric concentration to aqueous concentration,
!     mol/m^3 -> mol/m^3 aqu  ---
!
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
!
!---  Loop over conservation equation species  ---
!
      NEQX = NEQ - NEQE
      BJM(NEQ) = CX(NEQX)*VTOLX
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.100 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),100)) .AND. &
          (NSTEP-NRST).EQ.0 ) THEN
          AJM(NEQ,NEQ) = 1.D+0
          BJM(NEQ) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!---  Fixed species concentration or activity  ---
!
      DO 100 NSPKX = 1,NSPLK
        IF( ISPLK(14+NSPKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSPKX))
          IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
          IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
            AJM(NEQ,NEQ) = 1.D+0
            BJM(NEQ) = 0.D+0
            GOTO 1000
          ENDIF
        ENDIF
  100 CONTINUE
      DO 110 M = 1,IEQ_C(1,NEQX)
        NSP = IEQ_C(M+1,NEQX)
        BJM(NEQ) = BJM(NEQ) - EQ_C(M,NEQX)*SP_CX(NSP)*VTOLX
!
!---    Skip for initial pH  ---
!
        IF( ISPLK(1).GT.100 ) THEN
          IF( NSP.EQ.MOD(ISPLK(1),100) .AND. &
            (NSTEP-NRST).EQ.0 ) GOTO 110
        ENDIF
!
!---    Skip fixed species concentration ---
!
        DO 102 NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))
            IF( NSP.EQ.NSPX) GOTO 110
          ENDIF
  102   CONTINUE
        AJM(NEQ,IEQ_S(NSP)) = - EQ_C(M,NEQX)*VTOLX
        DO 104 NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))
            IF( NSPX .GT. 1000 ) NSPX = NSPX - 1000
            IF( NSP.EQ.NSPX ) AJM(NEQ,IEQ_S(NSP)) = &
              -1.D+0/ACTVX(1,NSP)*EQ_C(M,NEQX)*VTOLX
          ENDIF
  104   CONTINUE
  110 CONTINUE
!
!---  Return residual vector and Jacobian matrix  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NEQ) = -BJM(NEQ)
 1000 CONTINUE
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of CECHEM group  ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CECHEM_R( ACTVX,AJM,BJM,CX,SP_CX,N,NEQ,INDX )
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
!     Conservation Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!     Last Modified by MD White, PNNL, 7 January 2005.
!     $Id: eckechem.F,v 1.14 2006/09/19 14:41:18 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FDVP
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
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 SP_CX(LSPR)
      REAL*8 CX(LEQC+LEQK)
      REAL*8 ACTVX(LSPL+1,LSPL)
!
!----------------------Executable Lines--------------------------------!
!
!     ISUB_LOG = ISUB_LOG+1
!     SUB_LOG(ISUB_LOG) = '/CECHEM_R'
      SUBNMX = '/CECHEM_R'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Volumetric concentration to aqueous concentration,
!     mol/m^3 -> mol/m^3 aqu  ---
!
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
!
!---  Loop over conservation equation species  ---
!
      NEQX = NEQ - NEQE
      NROW = NEQX
      BJM(NROW) = CX(NEQX)*VTOLX
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.100 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),100)) .AND. &
          (NSTEP-NRST).EQ.0 ) THEN
          AJM(NROW,NROW) = 1.D+0
          BJM(NROW) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!--- fixed species concentration or activity
!
      DO NSPKX = 1,NSPLK
       IF( ISPLK(14+NSPKX).LT.0 ) THEN
        NSPX = ABS(ISPLK(14+NSPKX))
        IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
        IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
          AJM(NROW,NROW) = 1.D+0
          BJM(NROW) = 0.D+0
          GOTO 1000
        ENDIF
       ENDIF
      ENDDO
!      
      DO 110 M = 1,IEQ_C(1,NEQX)
        NSP = IEQ_C(M+1,NEQX)
        NEQXX = IEQ_S(NSP)
        BJM(NROW) = BJM(NROW) - EQ_C(M,NEQX)*SP_CX(NSP)*VTOLX
!        DO NSPKX = 1,NSPLK
!          IF(ISPLK(14+NSPKX).LT.-1000) THEN
!            NSPXX = ABS(ISPLK(14+NSPKX))-1000
!            IF(NSP.EQ.NSPXX) 
!     &      BJM(NEQX) = BJM(NEQ)+EQ_C(M,NEQX)*SP_CX(NSP)*VTOLX
!     &              - EQ_C(M,NEQX)*SP_CX(NSP)/ACTVX(1,NSP)*VTOLX
!          ENDIF
!        ENDDO
        IF( NEQXX.GT.NEQE ) THEN
!
!---    Skip for initial pH  ---
!
         IF( ISPLK(1).GT.100 ) THEN
          IF( NSP.EQ.MOD(ISPLK(1),100) .AND. &
            (NSTEP-NRST).EQ.0 ) GOTO 110
         ENDIF
!
!---   Skip fixed species concentration
!
         DO NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
           NSPX = ABS(ISPLK(14+NSPKX))
           IF( NSP.EQ.NSPX) GOTO 110
          ENDIF
         ENDDO
         NCOL = NEQXX-NEQE
         AJM(NROW,NCOL) = AJM(NROW,NCOL)- EQ_C(M,NEQX)*VTOLX
!fixed activity
         DO NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
           NSPX = ABS(ISPLK(14+NSPKX))
           IF( NSPX .GT. 1000 ) NSPX = NSPX - 1000
           IF( NSP.EQ.NSPX ) AJM(NROW,NCOL) = AJM(NROW,NCOL)- &
             1.D0/ACTVX(1,NSP)*EQ_C(M,NEQX)*VTOLX
          ENDIF
         ENDDO
        ELSE
!
!---   Equilibrium mass action
!
          NSE = IEQ_E(1,NEQXX)
!
!---  Loop over equilibrium species  ---
!
          DO MSX = 2,NSE
            NCM_SP = IEQ_E(MSX+1,NEQXX)
            NCOL = IEQ_S(NCM_SP)-NEQE
            AJM(NROW,NCOL) = AJM(NROW,NCOL)-EQ_C(M,NEQX)*VTOLX* &
              EQ_E(MSX-1,NEQXX)*SP_CX(NSP)/SP_CX(NCM_SP)
          ENDDO
        ENDIF	         

  110 CONTINUE
!
!---  Return residual vector and Jacobian matrix  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NROW) = -BJM(NROW)
 1000 CONTINUE
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of CECHEM_R group  ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DAVIES( ACTVX,SP_CX,DSP_CX,SLX,PORDX,RHOLX,TX,XLWX )
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
!     B-Dot or extended Debye-Huckel model for activity coefficient.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 December 2004.
!     Last Modified by MD White, PNNL, 21 December 2004.
!     $Id: eckechem.F,v 1.14 2006/09/19 14:41:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE CONST
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
      REAL*8 ACTVX(LSPL+1,LSPL),SP_CX(LSPL),DSP_CX(LSPL)
      REAL*8 CPIX(LSPL+1)
      REAL*8 ACOF(5),BCOF(5),BDCOF(5)
      REAL*8 SACOF(4),SBCOF(4),SCCOF(4)
!
!----------------------Data Statements---------------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/DAVIES'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Ionic strength of the aqueous solution  ---
!
      DO 40 M = 1,NSPL+1
        CPIX(M) = 0.D+0
        SUM_MZ = 0.D+0
        SUM_M = 0.D+0
        DO 30 NSP = 1,NSPL
!
!---      Skip neutral species  ---
!
          IF( ABS(SP_L(1,NSP)).LT.EPSL ) GOTO 30
          IF( NSP.EQ.(M-1) ) THEN
            CLX = SP_CX(NSP) + DSP_CX(NSP)
          ELSE
            CLX = SP_CX(NSP)
          ENDIF
!
!---      Molarity in mol solute/m^3 aqueous
!         or mol solute/l aqueous  ---
!
          CMX = CLX/(SLX*PORDX)
!
!---      Molality in mol solute/kg water  ---
!
          CMX = CMX/(RHOLX*XLWX)
          SUM_M = SUM_M + CMX
          SUM_MZ = SUM_MZ + CMX*SP_L(1,NSP)
          CPIX(M) = CPIX(M) + CMX*(SP_L(1,NSP)**2)
   30   CONTINUE
!
!---    Correct for electrical neutrality  ---
!
        CPIX(M) = CPIX(M)+DABS(SUM_MZ)
        CPIX(M) = CPIX(M)*5.D-1
   40 CONTINUE
!
!---  Activity coefficients for aqueous species  ---
!
      DO 60 M = 1,NSPL+1
        DO 50 NSP = 1,NSPL
!
!---      Activity coefficients for charged species  ---
!
          IF( ABS(SP_L(1,NSP)).GT.EPSL ) THEN
            ACTVX(M,NSP) = 5.D-1*(SP_L(1,NSP)**2)*(SQRT(CPIX(M)) &
              /(1.D+0 + SQRT(CPIX(M)))- 2.4D-1*CPIX(M))
            IF(ACTVX(M,NSP) > 1.D+1) ACTVX(M,NSP) = 0.D+0
            ACTVX(M,NSP) = 1.D+1**(-ACTVX(M,NSP))
!
!---      Activity coefficients for electrically neutral species  ---
!
          ELSE
            ACTVX(M,NSP) = 1.D+0
          ENDIF
   50   CONTINUE
   60 CONTINUE
!vlf  This loop doesn't appear in Yilin's code
      DO 70 IC = 1,6      
        ACTV16(IC) = 5.D-1*(REAL(IC)**2)*(SQRT(CPIX(1)) &
          /(1.D+0 + SQRT(CPIX(1)))- 2.4D-1*CPIX(1))
        IF(ACTV16(IC) > 1.D+1) ACTV16(IC) = 0.D+0
        ACTV16(IC) = 1.D+1**(-ACTV16(IC))
   70 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of DAVIES group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DGELG(R,A,MXDOF,M,EPS,IER)
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Full pivoting from BIOGEOCHEM
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, December 16, 2005.
!     Last Modified by Mark White, PNNL, December 16, 2005.
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
      DIMENSION A(MXDOF,MXDOF),R(MXDOF),IPIV(MXDOF)
!
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
!     ISUB_LOG = ISUB_LOG+1
!     SUB_LOG(ISUB_LOG) = '/DGELG'
      SUBNMX = '/DGELG'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
      IF(M) 23,23,1
!
!     SEARCH FOR THE GREATEST ELEMENT IN MATRIX A
!
    1 IER=0
      PIV=0.0D0
      DO 3 LI=1,M
      DO 3 LJ=1,M
      TBX=DABS(A(LI,LJ))
      IF(TBX-PIV) 3,3,2
    2 PIV=TBX
      I=LI
      J=LJ
    3 CONTINUE
      TOL=EPS*PIV
!     A(I,J) IS PIVOT ELEMENT. PIV CONTAINS THE ABSOLUTE VALUE OF A(I,J)
!     START ELIMINATION LOOP
      LST=1
      DO 17 K=1,M
!
!     TEST ON SINGULARITY
!
      IF(PIV) 23,23,4
    4 IF(IER) 7,5,7
    5 IF(PIV-TOL) 6,6,7
    6 IER=K-1
    7 PIVI=1.0D0/A(I,J)
      I=I-K
      J=J-K
!     I+K IS ROW-INDEX, J+K COLUMN-INDEX OF PIVOT ELEMENT
!
!     PIVOT ROW REDUCTION AND ROW INTERCHANGE IN RIGHT HAND SIDE R
      KK=K+I
      TBX=PIVI*R(KK)
      R(KK)=R(K)
      R(K)=TBX
!
!     IS ELIMINATION TERMINATED?
      IF(K-M) 9,18,18
!     COLUMN INTERCHANGE IN MATRIX A
    9 CONTINUE
      IF(J) 12,12,10
   10 KK=J+K
      DO 11 L=K,M
      TBX=A(L,K)
      A(L,K)=A(L,KK)
   11 A(L,KK)=TBX
!     ROW INTERCHANGE AND PIVOT ROW REDUCTION IN MATRIX A
   12 KK=K+I
      DO 13 L=K,M
      TBX=PIVI*A(KK,L)
      IF(L.EQ.K) TBX=A(KK,L)
      A(KK,L)=A(K,L)
   13 A(K,L)=TBX
!     SAVE COLUMN INTERCHANGE INFORMATION
      IPIV(K)=J
!     ELEMENT REDUCTION AND NEXT PIVOT SEARCH
      PIV=0.0D0
      LST=K+1
      DO 16 LI=LST,M
      PIVI=-A(LI,K)
      DO 15 LJ=LST,M
      A(LI,LJ)=A(LI,LJ)+PIVI*A(K,LJ)
      TBX=DABS(A(LI,LJ))
      IF(TBX-PIV) 15,15,14
   14 PIV=TBX
      I=LI
      J=LJ
   15 CONTINUE
      R(LI)=R(LI)+PIVI*R(K)
   16 CONTINUE
   17 CONTINUE
!     END OF ELIMINATION
!     BACK SUBSTITUTION AND BACK INTERCHANGE
   18 IF(M-1) 23,22,19
   19 LST=M+1
      DO 21 I=2,M
      II=LST-I
      L=IPIV(II)
      III=II+1
      TBX=R(II)
      DO 20 K=III,M
      TBX=TBX-A(II,K)*R(K)
   20 CONTINUE
      K=II+L
      R(II)=R(K)
      R(K)=TBX
   21 CONTINUE
   22 CONTINUE
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
!     ERROR RETURN
   23 IER=-1
!
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ECKECHEM
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
!     Equilibrium, Conservation, and Kinetic Equation CHEMistry.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 December 2004.
!     Last Modified by MD White, PNNL, 21 December 2004.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE SOURC
      USE REACT
      USE PORMED
      USE FILES
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      USE GRID_MOD
      use outpu
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
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
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 SP_CXX(LSPR)
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SP_CX
      REAL*8 CX(LEQC+LEQK),COX(LEQC+LEQK)
      REAL*8 DCLX(LSPL),CLX(LSPL),ACTVX(LSPL+1,LSPL)
      REAL*8 VFX(LSPS+1)
      INTEGER IJM(LSPR)
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      USE_GA = .TRUE.
      SUBNMX = '/ECKECHEM'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!print *,'eckechem-',sp_co(1:4,1)
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( SP_CX( LSPR,NUM_NODES ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_CX'
        CALL WRMSGS( INDX )
      ENDIF
      SP_CX = 0.D+0
!
!---  Total number of equations  ---
!
      NEQR = NEQE + NEQK + NEQC
!
!---  Total number species  ---
!
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Loop over all nodes, skipping inactive nodes  ---
!
      ICHEM = 0
      NODE = 0
      DO 300 N_X=1,NUM_LOC_NODES
        N = ID_L2G(N_X)
        IF( IXP(N).EQ.0 ) GOTO 300
!
!---    Assign local values of old-time-step species concentration  ---
!
        DO 110 NSP = 1,NSPR
          IF ( ISLC(57).EQ.0) THEN
            SP_CX(NSP,N) = SP_C(NSP,N)
          ELSE
            SP_CX(NSP,N) = MAX(SP_C(NSP,N),CMIN)
          ENDIF
  110   CONTINUE

!        DO 110 NSP = 1,NSPR
!          IF ( ISLC(59).EQ.0) THEN
!            SP_CX(NSP,N) = SP_C(NSP,N)
!          ELSE
!            IF( ISP_MN(NSP).EQ.1 ) THEN
!              SP_CX(NSP,N) = MAX(SP_C(NSP,N),0.D+0)
!            ELSE
!              SP_CX(NSP,N) = MAX(SP_C(NSP,N),CMIN)
!            ENDIF
!          ENDIF
!  110   CONTINUE

!
!---    Assign local values of component species concentration  ---
!
        DO 120 NSP = 1,NEQC
          NSL = NSP + NSOLU
          CX(NSP) = C(NSL,N)
          COX(NSP) = C(NSL,N)
  120   CONTINUE
!
!---    Assign local values of kinetic species concentration  ---
!
        DO 130 NSP = 1,NEQK
          NSPX = NSP + NEQC
          NSL = NSPX + NSOLU
          CX(NSPX) = C(NSL,N)
          COX(NSPX) = C(NSL,N)
  130   CONTINUE
!
!---    Guess specie concentrations, assuming activity
!       coefficients of 1.0  ---
!
        IF( ISLC(42).EQ.1 .AND. (NSTEP-NRST).EQ.0 ) THEN
!
!---      Check if neighboring cell has identical
!         conservation component species concentrations  ---
!
          IF( N.GT.1 .AND. NUM_NODES.GT.1 ) THEN
!vlf  Loop 140 commented out in Yilin's code
            DO 140 NSL = NSOLU+1,NSOLU+NEQC
              IF( ABS(CO(NSL,N-1)-C(NSL,N))/EPSL.GT.EPSL ) GOTO 170
  140       CONTINUE
!
!---        Skip guess calculation, use neighbor-cell species
!           concentrations  ---
!
!            DO 150 NSP = 1,NSPR
!              SP_CX(NSP,N) = SP_CX(NSP,N-1)
!  150       CONTINUE
!            GOTO 180
          ENDIF
  170     CONTINUE
          CALL GUSPCN( ME,CX,SP_CX(1:NSPR,N),N )
        ENDIF
  180   CONTINUE
        IF (ISLC(57).EQ.0) THEN
!
!---    Conventional Newton method, Global solve  ---
!
          CALL ECKECN( CX,COX,SP_CX(1:NSPR,N),N )
        ELSE
!
!---    Reduced Equations  ---
!
           CALL ECKECN_R( CX,COX,SP_CX(1:NSPR,N),N )
        ENDIF
        IF( ECKE_ER )THEN
          ICHEM = 1
          NX = LOC2NAT(N)
          NODE = NX
          EXIT
!          GOTO 300
        ENDIF
!
!---   Fixed species activity
!
        NSPH = MOD(ISPLK(1),100)
        DO 195 NSP = 1,NSPR
          SP_CXX(NSP) = SP_CX(NSP,N)
!fixed pH
!          IF(NSP.EQ.NSPH.and.nstep==nrst) then
!            VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!if(n==1) print *,'found-',-dlog10(sp_cx(nsp,n)),actvS(nsp),vtomx,iactv,sp_cx(nsp,n),nsp
!            IF(NSP.EQ.NSPX) SP_CX(NSP,N) = SP_CX(NSP,N)/ACTVS(NSP)/VTOMX
!          endif
          DO 190 NSPKX = 1,NSPLK
           IF( ISPLK(14+NSPKX).LT.-1000 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))-1000
            VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
            IF(NSP.EQ.NSPX) SP_CX(NSP,N) = SP_CX(NSP,N)/ACTVX(1,NSP)/VTOMX
           ENDIF
  190     CONTINUE
  195   CONTINUE
!
!---    Conservation-component species, mobile conservation-component
!       species, and old time step mobile conservation-component
!       species  ---
!
        DO 210 NEQ = 1,NEQC
          NSL = NSOLU + NEQ
          C(NSL,N) = 0.D+0
          CMBX = 0.D+0
!
!---      Loop over conservation-component species  ---
!
          DO 200 M = 1,IEQ_C(1,NEQ)
            NSP = IEQ_C(M+1,NEQ)
            IF( ABS(SP_CX(NSP,N)).LT.CMIN ) THEN
              SP_CX(NSP,N) = 0.D+0
            ENDIF
            C(NSL,N) = C(NSL,N) + EQ_C(M,NEQ)*SP_CX(NSP,N)
!
!---        Mobile species ---
!
            IF( NSP.LE.NSPL .OR. NSP.GT.(NSPL+NSPS+NSPE) ) &
              CMBX = CMBX + EQ_C(M,NEQ)*SP_CX(NSP,N)
!
!---        Immobile species ---
!
            IF( NSP.GT.NSPL .AND. NSP.LE.(NSPL+NSPS+NSPE) ) THEN
              IF( ABS(SP_CO(NSP,N)).LT.CMIN ) THEN
                SP_COX = 0.D+0
              ELSE
                SP_COX = SP_CO(NSP,N)
              ENDIF
              SPI_CX = EQ_C(M,NEQ)*SP_CX(NSP,N)
              SPI_COX = EQ_C(M,NEQ)*SP_COX
!
!---          Load CO2 sources for linked aqueous CO2,
!             skip for initial conditions  ---
!
              IF( ISPLK(6).EQ.NSL .AND. (NSTEP-NRST).GT.0 ) THEN
                SRCAX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*WTMA
                IF( ABS(SRCAX).LT.1.D-16 ) SRCAX = 0.D+0
                SRCA(1,N) = SRCA(1,N) + SRCAX
              ENDIF
!
!---          Load air sources for linked aqueous air,
!             skip for initial conditions  ---
!
              IF( ISPLK(4).EQ.NSL .AND. (NSTEP-NRST).GT.0 ) THEN
                SRCAX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*WTMA
                IF( ABS(SRCAX).LT.1.D-16 ) SRCAX = 0.D+0
                SRCA(1,N) = SRCA(1,N) + SRCAX
              ENDIF
            ENDIF
  200     CONTINUE
  210   CONTINUE
!
!---    Kinetic-component species, mobile kinetic-component species
!       and old time step mobile kinetic-component species  ---
!
        DO 230 NEQ = 1,NEQK
          NSL = NSOLU + NEQC + NEQ
          C(NSL,N) = 0.D+0
          CMBX = 0.D+0
!
!---      Loop over kinetic-component species  ---
!
          DO 220 M = 1,IEQ_K(1,NEQ)
            NSP = IEQ_K(M+1,NEQ)
            C(NSL,N) = C(NSL,N) + EQ_K(M,NEQ)*SP_CX(NSP,N)
!
!---        Mobile species ---
!
            IF( NSP.LE.NSPL .OR. NSP.GT.(NSPL+NSPS+NSPE) ) &
              CMBX = CMBX + EQ_K(M,NEQ)*SP_CX(NSP,N)
!
!---        Immobile species ---
!
            IF( NSP.GT.NSPL .AND. NSP.LE.(NSPL+NSPS+NSPE) ) THEN
              IF( ABS(SP_CO(NSP,N)).LT.CMIN ) THEN
                SP_COX = 0.D+0
              ELSE
                SP_COX = SP_CO(NSP,N)
              ENDIF
              SPI_CX = EQ_K(M,NEQ)*SP_CX(NSP,N)
              SPI_COX = EQ_K(M,NEQ)*SP_COX
!
!---          Load CO2 sources for linked aqueous CO2,
!             skip for initial conditions  ---
!
              IF( ISPLK(6).EQ.NSL .AND. (NSTEP-NRST).GT.0 ) THEN
                SRCAX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*WTMA*DTI
                IF( ABS(SRCAX).LT.1.D-16 ) SRCAX = 0.D+0
                SRCA(1,N) = SRCA(1,N) + SRCAX
              ENDIF
!
!---          Load air sources for linked aqueous air,
!             skip for initial conditions  ---
!
              IF( ISPLK(4).EQ.NSL .AND. (NSTEP-NRST).GT.0 ) THEN
                SRCAX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*WTMA*DTI
                IF( ABS(SRCAX).LT.1.D-16 ) SRCAX = 0.D+0
                SRCA(1,N) = SRCA(1,N) + SRCAX
              ENDIF
            ENDIF
  220     CONTINUE
  230   CONTINUE
!
!---    Assign global species concentrations  ---
!
        DO 240 NSP = 1,NSPR
          IF( ABS(SP_CX(NSP,N)).LT.CMIN ) THEN
            SP_C(NSP,N) = 0.D+0
          ELSE
            SP_C(NSP,N) = SP_CX(NSP,N)
          ENDIF
!
!---   Fixed species activity
!
          DO 235 NSPKX = 1,NSPLK
           IF( ISPLK(14+NSPKX).LT.-1000 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))-1000
            SP_C(NSP,N) = SP_CXX(NSP)
           ENDIF
  235     CONTINUE

!          NSPH = MOD(ISPLK(1),100)
!if(n==11.and.nsp == 3) then
!print *,'c_ph--',c_ph(n),sp_c(nsp,n),actvs(nsp)
!stop
!endif
  240   CONTINUE
!
!---    Loop over solid species to determine current mineral
!       volume fractions and porosity ---
!
!        POR_M(1,N) = 1.D+0
!        POR_M(2,N) = 1.D+0 - POR(2,N) + POR(1,N)
!        if(islc(43) == 2) then
          POR_M(1,N) = 1.D+0 - vfrac_i(n)
          POR_M(2,N) = 1.D+0 - POR(2,N) + POR(1,N) - vfrac_i(n)
!        endif
        DO 250 NSP = NSPL+1,NSPL+NSPS
          NSP_M = NSP-NSPL
!
!---      Minerals ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            IF( SP_CX(NSP,N)+SP_CMN(NSP_M,N).LT.CMIN ) THEN
              RS_S(3,NSP_M,N) = 0.D+0
            ELSE
              RS_S(3,NSP_M,N) = 1.D-3*(SP_CX(NSP,N)+SP_CMN(NSP_M,N))* &
                SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
!
!---      Non minerals ---
!
          ELSE
            IF( SP_CX(NSP,N).GT.CMIN ) THEN
              RS_S(3,NSP_M,N) = 1.D-3*SP_CX(NSP,N)*SP_S(2,NSP_M)/ &
                SP_S(1,NSP_M)
            ELSE
              RS_S(3,NSP_M,N) = 0.D+0
            ENDIF
          ENDIF
          RS_S(3,NSP_M,N) = MIN(RS_S(3,NSP_M,N),1.D+0)
          POR_M(1,N) = POR_M(1,N) - RS_S(3,NSP_M,N)
          POR_M(2,N) = POR_M(2,N) - RS_S(3,NSP_M,N)
          POR_M(1,N) = MAX(POR_M(1,N),1.D-12)
          POR_M(2,N) = MAX(POR_M(2,N),1.D-12)
  250   CONTINUE
  300   CONTINUE

!
!---    Convergence failure, double the number of
!       sub-time steps  ---
!
        CALL GA_IGOP(1,ICHEM,1,'max')
        CALL GA_IGOP(2,NODE,1,'max')
        IF(ICHEM.EQ.1)ECKE_ER = .TRUE.
        IF( ECKE_ER ) THEN
          N_RST = 2*N_RST
!           n_rst = n_rst + 1
          REALX = REAL(N_RST)
          DT = DT_RST/REALX
          DTI = 1.D+0/(DT+EPSL)
          TM = TM_RST - DT_RST
!
!          DTX = DT
!          TM = TM - (1.D+0-DTCF)*DT
!          DT = DTCF*DT
!          DTO = DT
!          DTI = 1.D+0/DT
!          VAR = DT
!          VARX = DTX
!          IF( UNTM.NE.'null' ) THEN
!            INDX = 1
!            IUNS = 1
!            CALL RDUNIT(UNTM,VAR,INDX)
!            INDX = 1
!            IUNS = 1
!            CALL RDUNIT(UNTM,VARX,INDX)
!            NCH = INDEX( UNTM,'  ')-1
!          ENDIF
          IF(ME.EQ.0) THEN
            WRITE(ISC,'(A,I6,A)') &
             '          ---  ECKEChem Convergence Failure, Node = ' &
             ,NODE,'  ---'
            WRITE(ISC,'(A,I6,A)') &
              '          ---  ECKEChem Sub-Time Stepping Factor = ' &
              ,N_RST,'  ---'
            WRITE(IWR,'(A,I6,A)') &
              '          ---  ECKEChem Convergence Failure, Node = ' &
              ,NODE,'  ---'
            WRITE(IWR,'(A,I6,A)') &
              '          ---  ECKEChem Sub-Time Stepping Factor = ' &
            ,N_RST,'  ---'
!          if(me.eq.0)WRITE(ISC,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')       &
!          'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ', &
!          VAR,UNTM(1:NCH)
!          if(me.eq.0)WRITE(IWR,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')        &
!          'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ', &
!          VAR,UNTM(1:NCH)
          ENDIF
        ENDIF
!
!vlf   IF( ISLC(52).EQ.1 .AND. (.NOT.ECKE_ER) .AND. NSTEP.GE.NRST ) &
!      IF( .NOT.ECKE_ER .AND. NSTEP.GE.NRST ) &
!        NSPLK = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      IF(ECKE_ER.AND.(NSTEP-NRST).EQ.0) THEN
        IF(ME.EQ.0)PRINT *, &
          'Initial ECKEChem Convergence Failure, stop...'
        STOP
      ENDIF
      DEALLOCATE( SP_CX )
!
!---  End of ECKECHEM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ECKECN( CX,COX,SP_CX,N )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Conventional Newton scheme for solving the nonlinear
!     equilibrium, conservation, and kinetic chemistry equations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, January 26, 2006.
!     Last Modified by Mark White, PNNL, January 26, 2006.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FDVP
      USE CONST
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
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
!     REAL*8 AJMC(LSPR,LSPR),BJMC(LSPR)
      REAL*8 SP_CX(LSPR)
      REAL*8 CX(LEQC+LEQK),COX(LEQC+LEQK)
      REAL*8 DCLX(LSPL),CLX(LSPL),ACTVX(LSPL+1,LSPL)
      REAL*8 VFX(LSPS+1)
      INTEGER IJM(LSPR)
      CHARACTER*64 GETSPNM
      EXTERNAL GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/ECKECN'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
      ECKE_ER = .FALSE.
!
!---  Total number of equations  ---
!
      NEQR = NSPR
      RLXF = 1.0D+0
!
!--- Fixed species activity ---
!
      NSPH = MOD(ISPLK(1),100)
      IF( (NSTEP-NRST).EQ.0 ) THEN
!fixed pH
        IF(NSPH > 0) FACTV(NSPH) = SP_CX(NSPH)
!if(n==11) print *,'fact-',factv(nsph)
        DO 10 NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))
            IF( NSPX.GT.1000 ) THEN
              NSPX = NSPX - 1000
              FACTV(NSPX) = SP_CX(NSPX)
            ENDIF
          ENDIF
   10   CONTINUE
      ENDIF
!
!---  For initial conditions, skip kinetic equations  ---
!
      IF( (NSTEP-NRST).EQ.0 ) NEQR = NEQE+NEQC
  100 CONTINUE
!!
!!---  Guess specie concentrations, assuming activity
!!     coefficients of 1.0  ---
!!
!      IF( IGUSPCN.EQ.1 ) THEN
!!
!!---    Assign local values of old-time-step species concentration  ---
!!
!        DO 110 NSP = 1,NSPR
!          IF( ABS(SP_C(NSP,N)).LT.CMIN ) THEN
!            SP_CX(NSP) = 0.D+0
!          ELSE
!            SP_CX(NSP) = SP_C(NSP,N)
!          ENDIF
!  110   CONTINUE
!!
!!---    Assign local values of component species concentration  ---
!!
!        DO 120 NSP = 1,NEQC
!          NSL = NSP + NSOLU
!          CX(NSP) = C(NSL,N)
!          COX(NSP) = C(NSL,N)
!  120   CONTINUE
!!
!!---    Assign local values of kinetic species concentration  ---
!!
!        DO 140 NSP = 1,NEQK
!          NSPX = NSP + NEQC
!          NSL = NSPX + NSOLU
!          CX(NSPX) = C(NSL,N)
!          COX(NSPX) = C(NSL,N)
!  140   CONTINUE
!        CALL GUSPCN( CX,SP_CX,N )
!      ENDIF
!
!---  Top of Newton-Raphson loop  ---
!
      NC = 0
  220 CONTINUE
      NC = NC + 1
!
!---  Set index to compute both the Jacobian matrix
!     and residual vector  ---
!
      INDX = 0
      CALL ECKEJCB( ACTVX,AJM,BJM,CX,COX,SP_CX,N,INDX,IER )

!!
!!---  Matrix-problem vector copy  ---
!!
!      DO 320 L = 1,NEQR
!        DO 310 M = 1,NEQR
!          print *, 'ajm:',AJM(L,M),l,m,n
!  310   CONTINUE
!        print *, BJM(L),l,n
!  320 CONTINUE
!
!---  Solve linear system using LU Decomposition  ---
!
!      CALL LUDCMP( AJM,NEQR,LSPR,IJM,DJM )
!      CALL LUBKSB( AJM,NEQR,LSPR,IJM,BJM )
!      CALL MPROVE( AJMC,AJM,NEQR,LSPR,IJM,BJMC,BJM )
      CALL DGELG(BJM,AJM,LSPR,NEQR,EPSL,IER)
      IF( IER.EQ.-1 ) GOTO 350
!
!---  Maximum residual  ---
!
      RSDMXX = 1.D-20
      DO 330 NSP = 1,NSPR
        IF( SP_CX(NSP)/EPSL.GT.EPSL .OR. &
          BJM(IEQ_S(NSP))/EPSL.GT.EPSL ) THEN
          SP_CMX = MAX( ABS(SP_CX(NSP)),ABS(SP_C(NSP,N)),1.D-24 )
          IF( ABS(SP_CMX).GT.CMIN ) THEN
            RSDX = ABS(BJM(IEQ_S(NSP)))/SP_CMX
          ELSE
            RSDX = 1.D+0
          ENDIF
!vlf
          IF( INDEX(GETSPNM(NSP),'fix').NE.0 ) THEN
            RSDX = 0.D+0
          ENDIF
!vlf
          IF( RSDX.GT.RSDMXX ) THEN
            IF( ABS(SP_CX(NSP)).GT.1.D-16 ) THEN
              RSDMXX = RSDX
              NSPMX = NSP
            ENDIF
          ENDIF
        ENDIF
  330 CONTINUE
!
!---  Update the species concentrations, mol/m^3  ---
!
      DO 332 NSP = 1,NSPR
!
!---    Relaxation/Check for NaNs  ---
!
        CCX = RLXF*BJM(IEQ_S(NSP))
        IF( CCX.NE.CCX )THEN
          IER = -1
          GOTO 350
        ENDIF
        IF( CCX.LT.0.D+0 ) THEN
!
!---      Mineral species  ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            NSP_MN = NSP - NSPL
            SP_CCX = SP_CMN(NSP_MN,N)+SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
!
!---      Immobile species  ---
!
          ELSEIF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
            SP_CCX = SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
!
!---      Mobile species  ---
!
          ELSE
            SP_CCX = 9.5D-1*SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
          ENDIF
        ENDIF
!
!---    Update mineral species concentration  ---
!
        IF( ISP_MN(NSP).EQ.1 ) THEN
          SP_CX(NSP) = SP_CX(NSP)+CCX
          IF( ABS(SP_CX(NSP)).LT.CMIN ) SP_CX(NSP) = 0.D-20
!
!---    Update non-mineral species concentration  ---
!
        ELSE
!
!---    Fix species concentration
!
          IF( INDEX(GETSPNM(NSP),'fix').NE.0 ) THEN
!           RSDMX = 0.D+0
            CCX = 0.D+0
            IF ( NSTEP-NRST.GT.0 ) THEN
!
!---          Convert reactive species from node volumetric, kmol/m^3
!             to node volumetric, mol/m^3  ---
!
              IF( MOD(IC_SP(NSP,N),10).EQ.1 ) THEN
                SP_CX(NSP) = 1.D+3*SP_CI(NSP,N)
!
!---          Convert reactive species from aqueous volumetric, kmol/m^3
!             to node volumetric, mol/m^3  ---
!
              ELSEIF( MOD(IC_SP(NSP,N),10).EQ.2 ) THEN
                SP_CX(NSP) = 1.D+3*SP_CI(NSP,N)*SL(2,N)*PORD(2,N)
!
!---          Convert reactive species from aqueous molal, kmol/kg water
!             to node volumetric, mol/m^3  ---
!
              ELSEIF( MOD(IC_SP(NSP,N),10).EQ.3 ) THEN
                SP_CX(NSP) = 1.D+3*SP_CI(NSP,N)*SL(2,N)*PORD(2,N)* &
                  RHOL(2,N)*XLW(2,N)
!
!---          Convert reactive species from gas volumetric, kmol/m^3
!             to node volumetric, mol/m^3  ---
!
              ELSEIF( MOD(IC_SP(NSP,N),10).EQ.4 ) THEN
                SP_CX(NSP) = 1.D+3*SP_CI(NSP,N)*SG(2,N)*PORD(2,N)
              ENDIF
            ENDIF
          ENDIF
          SP_CX(NSP) = MAX( SP_CX(NSP)+CCX,0.D+0 )
        ENDIF
  332 CONTINUE
!
!--- Fixed species activity  ---
!
      DO 340 NSPKX = 1,NSPLK
        IF( ISPLK(14+NSPKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSPKX))
          IF( NSPX.GT.1000 ) THEN
            NSPX = NSPX - 1000
            IF( IACTV.EQ.3 ) THEN
             ACTVX(1,NSP) = ACTVC
            ELSEIF( IACTV.EQ.1 ) THEN
             CALL DAVIES( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N), &
              RHOL(2,N),T(2,N),XLW(2,N) )
            ELSE
             CALL BDOT( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N), &
             RHOL(2,N),T(2,N),XLW(2,N) )
            ENDIF
            SP_CX(NSPX) = FACTV(NSPX)/ACTVX(1,NSPX)
          ENDIF
        ENDIF
  340 CONTINUE
!fixed pH
        IF( NSPH > 0 .and. nstep == nrst .and. (ieo == 1 .or. IC_SP(NSPH,N) > 10)) THEN !only noraml solution or overwritten
!        IF( NSPH > 0 .and. nstep == nrst) then

            NSPX = NSPH
            VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
            SP_CX(NSPX) = FACTV(NSPX)/ACTVS(NSPX)/VTOMX
!if(n==11) print *,'activ=',nspx,actvs(nspx),sp_cx(nspx)
!        nsph = mod(isplk(1),100)
!        if(nspx == nsph) then
!             VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!             c_ph(n) = -dlog10(sp_cx(nsph)*actvs(nsph)*vtomx)
!if(n==11) print *,'n--',nsp,c_ph(n),sp_cx(nsph),vtomx
!        endif
        ENDIF
      do nsp=1,nspr
!          if(getspnm(nsp) == 'h+') then
          if(nsp == mod(isplk(1),100)) then
             VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
             c_ph(n) = -dlog10(sp_cx(nsp)*actvs(nsp)*vtomx)
!if(n==1) print *,'n--',nsp,c_ph(n),sp_cx(nsp),vtomx
!stop
             exit
          endif
      enddo
!       if (nstep.eq.0)stop
!
!---  Unconverged species concentration exit or repeat
!     Newton-Raphson loop  ---
!
      IF( RSDMXX.GT.1.D-6 ) THEN
        IF( NC.EQ.32 ) THEN
          RLXF = 6.D-1
          GOTO 220
        ELSEIF( NC.EQ.100 ) THEN
          ECKE_ER = .TRUE.
        ELSE
          GOTO 220
        ENDIF
      ENDIF
  350 CONTINUE
      IF(IER.EQ.-1) ECKE_ER = .TRUE.
!
!---  End of ECKECN group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ECKECN_R( CX,COX,SP_CX,N )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Conventional Newton scheme for solving the nonlinear
!     equilibrium, conservation, and kinetic chemistry equations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, January 26, 2006.
!     Last Modified by Mark White, PNNL, January 26, 2006.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE FDVP
      USE REACT
      USE CONST
      USE TRNSPT

      USE GRID
!
!----------------------Fortran 90 Modules------------------------------!
!
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
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR),AJMC(LSPR,LSPR),BJMC(LSPR)
      REAL*8 SP_CX(LSPR),EQKX(LEQE)
      REAL*8 CX(LEQC+LEQK),COX(LEQC+LEQK)
      REAL*8 DCLX(LSPL),CLX(LSPL),ACTVX(LSPL+1,LSPL)
      REAL*8 VFX(LSPS+1)
      INTEGER IJM(LSPR)
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      SUBNMX = '/ECKECN_R'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
      ECKE_ER = .FALSE.
!
!---  Total number of equations  ---
!
      NEQR = NEQC+NEQK
      RLXF = 1.D+0
!
!---  Volumetric concentration to molality  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!
!---  Equilibrium constant as a function of temperature  ---
!
      DO NEQ = 1,NEQE
        NS = IEQ_E(1,NEQ)
        IRCE = IEQ_E((NS+2),NEQ)
        CALL EQCN( EQKX(NEQ),T(2,N),IRCE,N )
      ENDDO
!vlf this is reduced system...does this need to remain uncommented?
!
!---  For initial conditions, skip kinetic equations  ---
!YFang -- Keep kinetic variables
!      IF( (NSTEP-NRST).EQ.0 ) NEQR = NEQE+NEQC
  100 CONTINUE
!
!---  Guess specie concentrations, assuming activity
!     coefficients of 1.0  ---
!
      IF( NSTEP.EQ.NRST ) THEN
!
!---    Assign local values of old-time-step species concentration  ---
!
        DO 110 NSP = 1,NSPR
          IF( ABS(SP_C(NSP,N)).LT.CMIN ) THEN
!            SP_CX(NSP) = 0.D+0
            SP_CX(NSP) = MAX(SP_CX(NSP),CMIN)
          ELSE
            SP_CX(NSP) = SP_C(NSP,N)
          ENDIF
  110   CONTINUE
!
!---    Assign local values of component species concentration  ---
!
        DO 120 NSP = 1,NEQC
          NSL = NSP + NSOLU
          CX(NSP) = C(NSL,N)
          COX(NSP) = C(NSL,N)
  120   CONTINUE
!
!---    Assign local values of kinetic species concentration  ---
!
        DO 140 NSP = 1,NEQK
          NSPX = NSP + NEQC
          NSL = NSPX + NSOLU
          CX(NSPX) = C(NSL,N)
          COX(NSPX) = C(NSL,N)
  140   CONTINUE
        NSPH = MOD(ISPLK(1),100)
        DO NEQ = 1,NEQC
          NSP = IEQ_C(2,NEQ)
          IF( NSP.EQ.NSPH ) CYCLE
          SP_CX(NSP) = MAX( CX(NEQ)*5.D-2,CMIN )
        ENDDO
!
!---  If uninitialized set pH to 1.D-7 molality, mol/kg water  ---
!
        IF( NSPH.GE.1 .AND. NSPH.LE.LSPR ) THEN
          IF( SP_CX(NSPH)/EPSL.LT.EPSL ) SP_CX(NSPH) = 1.D-7/VTOMX
        ENDIF
!
!---  Recalculate equilibrium species concentrations 
!
        DO NEQ = 1,NEQE
         NEQ_SP = IEQ_E(2,NEQ)
         NS = IEQ_E(1,NEQ)
!
!---    Equilibrium constant and exponent  ---
!
!xyl
        IF( ISLC(60).EQ.0 ) THEN
          C_NEG = EQKX(NEQ)**EQ_E(NS,NEQ)
        ELSE
          C_NEG = EQ_E(NS,NEQ)*LOG(EQKX(NEQ))
        ENDIF
!xyl
!
!---    Loop over species in equilibrium equation
!       skipping the equilibrium species  ---
!
         DO K = 1,NS
          NCM_SP = IEQ_E(K+1,NEQ)
!
!---      Skip equilibrium species  ---
!
          IF( NCM_SP.EQ.NEQ_SP ) CYCLE
!
!---      Convert conservation species concentration to
!         molality, mol/kg H2O  ---
!
          Nx = loc2nat(n)
          CMX = SP_CX(NCM_SP)*VTOMX
!xyl
          IF( ISLC(60).EQ.0 ) THEN
            C_NEG = C_NEG*(CMX**EQ_E(K-1,NEQ))
          ELSE
            C_NEG = C_NEG+EQ_E(K-1,NEQ)*LOG(CMX)
          ENDIF
!xyl
         ENDDO
!xyl
         IF( ISLC(60).EQ.1 ) THEN
           C_NEG = EXP(C_NEG)
         ENDIF
!
!---    Convert equilibrium species concentration to
!       node volumetric, mol/m^3  ---
!
         SP_CX(NEQ_SP) = C_NEG/VTOMX
         SPCXX = MIN( 1.0D+30,SP_CX(NEQ_SP) )
         DO NCX = 1,NEQC
           LIN: DO IX=1,IEQ_C(1,NCX)
             NSP=IEQ_C(IX+1,NCX)
             IF(NEQ_SP.EQ.NSP) THEN
               SPCXM=ABS(CX(NCX)/EQ_C(IX,NCX))
               SPCXX=MIN(SPCXX,SPCXM)
               EXIT LIN                               			 			                
             ENDIF
           ENDDO LIN
         ENDDO
         SP_CX(NEQ_SP) = MAX(SPCXX,CMIN)
        ENDDO
      ENDIF
!
!---  Top of Newton-Raphson loop  ---
!
      NC = 0
  220 CONTINUE
      NC = NC + 1
      INDX = 0
!
!---  Set index to compute both the Jacobian matrix
!     and residual vector  ---
!
      CALL ECKEJCB( ACTVX,AJM,BJM,CX,COX,SP_CX,N,INDX,IER )
!
!---  Matrix-problem vector copy  ---
!
!      DO 320 L = 1,NEQR
!        DO 310 M = 1,NEQR
!          AJMC(L,M) = AJM(L,M)
!  310   CONTINUE
!        print *, BJM(L),l
!  320 CONTINUE
!
!---  Solve linear system using LU Decomposition  ---
!
!      CALL LUDCMP( AJM,NEQR,LSPR,IJM,DJM )
!      CALL LUBKSB( AJM,NEQR,LSPR,IJM,BJM )
!      CALL MPROVE( AJMC,AJM,NEQR,LSPR,IJM,BJMC,BJM )
      CALL DGELG(BJM,AJM,LSPR,NEQR,EPSL,IER)

      IF( IER.EQ.-1 ) goto 350
!
!---  Maximum residual  ---
!
      RSDMXX = 1.D-20
      DO 330 NSP = 1,NSPR
        NEQX = IEQ_S(NSP)
        IF( NEQX.GT.NEQE) THEN
         NROWX = NEQX-NEQE
         IF( SP_CX(NSP)/EPSL.GT.EPSL .OR. &
          BJM(NROWX)/EPSL.GT.EPSL ) THEN
          SP_CMX = MAX( ABS(SP_CX(NSP)),ABS(SP_C(NSP,N)),1.D-24 )
          IF( ABS(SP_CMX).GT.CMIN ) THEN
            RSDX = ABS(BJM(NROWX))/SP_CMX
          ELSE
            RSDX = 1.D+0
          ENDIF
          IF( RSDX.GT.RSDMXX ) THEN
            IF( ABS(SP_CX(NSP)).GT.1.D-16 ) THEN
              RSDMXX = RSDX
              NSPMX = NSP
!print *,'nsp--',nsp
            ENDIF
          ENDIF
         ENDIF
        ENDIF
  330 CONTINUE
!
!---  Update the species concentrations, mol/m^3  ---
!
      DO 332 NSP = 1,NSPR
!
!---    Relaxation  ---
!
       NEQX = IEQ_S(NSP)
       IF( NEQX.GT.NEQE ) THEN
        NROWX = NEQX-NEQE
        CCX = RLXF*BJM(NROWX)
        IF( CCX.LT.0.D+0 ) THEN
!
!---      Mineral species  ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            NSP_MN = NSP - NSPL
            SP_CCX = SP_CMN(NSP_MN,N)+SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
!            CCX = sign(min(1.d-4*sp_cmn(nsp_mn,n),abs(ccx)),ccx)
!
!---      Immobile species  ---
!
          ELSEIF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
            SP_CCX = SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
!
!---      Mobile species  ---
!
          ELSE
            SP_CCX = 9.5D-1*SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
          ENDIF
!        else
!          IF( ISP_MN(NSP).EQ.1 ) THEN
!            CCX = sign(min(1.d-3,abs(ccx)),ccx)
!print *,'here---'
!          endif
        ENDIF
!
!---    Update mineral species concentration  ---
!
        IF( ISP_MN(NSP).EQ.1 ) THEN
!          SP_CX(NSP) = SP_CX(NSP)+min(CCX,1.d-5)
          SP_CX(NSP) = SP_CX(NSP)+CCX
          IF( ABS(SP_CX(NSP)).LT.CMIN ) SP_CX(NSP) = 0.d-20
!
!---    Update non-mineral species concentration  ---
!
        ELSE
!          SP_CX(NSP) = MAX( SP_CX(NSP)+min(CCX,1.d-3),0.D+0 )
          SP_CX(NSP) = MAX( SP_CX(NSP)+CCX,0.D+0 )
        ENDIF
       ENDIF
  332 CONTINUE
!
!---  Update equilium species concentration
!
!
!---  Calculate species activity coefficient
!
!      CLX(1:NSPL) = SP_CX(1:NSPL)
!      DCLX = 1.D-6
!      IF( IACTV.EQ.3 ) THEN
!        ACTVX(1,NSP) = ACTVC
!      ELSEIF( IACTV.EQ.1 ) THEN
!        CALL DAVIES( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
!     &    RHOL(2,N),T(2,N),XLW(2,N) )
!      ELSE
!        CALL BDOT( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
!     &    RHOL(2,N),T(2,N),XLW(2,N) )
!      ENDIF
!
!---  Recalculate equilibrium species concentrations 
!
      DO NEQ = 1,NEQE
        NEQ_SP = IEQ_E(2,NEQ)
        NS = IEQ_E(1,NEQ)
!
!---    Equilibrium constant and exponent  ---
!
!xyl
        IF( ISLC(60).EQ.0 ) THEN
          C_NEG = EQKX(NEQ)**EQ_E(NS,NEQ)
        ELSE
          C_NEG = EQ_E(NS,NEQ)*LOG(EQKX(NEQ))
        ENDIF
!xyl
!
!---    Loop over species in equilibrium equation
!       skipping the equilibrium species  ---
!
        DO K = 1,NS
          NCM_SP = IEQ_E(K+1,NEQ)
!
!---      Skip equilibrium species  ---
!
          IF( NCM_SP.EQ.NEQ_SP ) CYCLE
!
!---      Convert conservation species concentration to
!         molality, mol/kg H2O  ---
!
          IF(NCM_SP.LE.NSPL) THEN
            ACX = ACTVX(1,NCM_SP)
          ELSE
            ACX = 1.D+0
          ENDIF
          CMX = SP_CX(NCM_SP)*VTOMX*ACX
!xyl
          IF( ISLC(60).EQ.0 ) THEN
            C_NEG = C_NEG*(CMX**EQ_E(K-1,NEQ))
          ELSE
            C_NEG = C_NEG+EQ_E(K-1,NEQ)*LOG(CMX)
          ENDIF
!xyl
        ENDDO
!xyl
         IF( ISLC(60).EQ.1 ) THEN
           C_NEG = EXP(C_NEG)
         ENDIF
!
!---    Convert equilibrium species concentration to
!       node volumetric, mol/m^3  ---
!
        IF(NEQ_SP.LE.NSPL) THEN
          ACX = ACTVX(1,NEQ_SP)
        ELSE
          ACX = 1.D+0
        ENDIF
        SP_CX(NEQ_SP) = C_NEG/VTOMX/ACX
      ENDDO
!
!---  Unconverged species concentration exit or repeat
!     Newton-Raphson loop  ---
!
      IF( RSDMXX.GT.1.D-6 ) THEN
        IF( NC.EQ.32 ) THEN
          RLXF = 6.D-1
          GOTO 220
        ELSEIF( NC.EQ.200 ) THEN
          ECKE_ER = .TRUE.
        ELSE
          GOTO 220
        ENDIF
      ENDIF
!
!---  Check mass balance for components
!
      DO NEQX=1,NEQC
        NSP =  ISP_S(NEQX+NEQE)
        IF( MOD(ISPLK(1),100).NE.0 .AND. NSTEP == NRST) CYCLE
        DIFF = CX(NEQX)
        DO M=1,IEQ_C(1,NEQX)
          NSP=IEQ_C(M+1,NEQX)
          DIFF=DIFF-EQ_C(M,NEQX)*SP_CX(NSP)          
        ENDDO
        IF( CX(NEQX).EQ.0.D+0 .AND. DIFF.LE.1.D-8 ) CYCLE
        IF( DIFF .LE. 0.04*ABS(CX(NEQX)) ) CYCLE
        ECKE_ER = .TRUE.
      ENDDO	     
  350 continue
      if(ier.eq.-1) ecke_er = .true.
!
!---  End of ECKECN_R group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ECKEJCB( ACTVX,AJM,BJM,CX,COX,SP_CX,N,INDX,IER )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Numerical Recipes in Fortran 77, The Art of Scientific Computing
!     W.H. Press, B.P. Flannery, Saul A. Teukolsky, and W.T. Vetterling
!
!     INDX =  1 - This subroutine replaces the original "funcv"
!     INDX = -1 - This subroutine replaces the original "fdjac"
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, January 26, 2006.
!     Last Modified by Mark White, PNNL, January 26, 2006.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FDVP
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
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 CX(LEQC+LEQK),COX(LEQC+LEQK)
      REAL*8 DCLX(LSPL),CLX(LSPL),ACTVX(LSPL+1,LSPL)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/ECKEJCB'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Number of equations equals number of species  ---
!
      NEQR = NSPR
!
!---  For initial conditions, skip kinetic equations  ---
!
!vlf Yilin commented this out - keep kinetic vars - is it needed?
!      IF( (NSTEP-NRST).EQ.0 ) NEQR = NEQE+NEQC
!
!---  Specie increments  ---
!
      DO 230 NSP = 1,NSPR
!        DSP_CX(NSP) = 1.D-2*SP_CX(NSP)
        DSP_CX(NSP) = 1.D-6*SP_CX(NSP)
!sp_cx(nsp) = max(sp_cx(nsp),1.d-30)
        IF( ABS(SP_CX(NSP)).LT.CMIN ) THEN
          IF( ISP_MN(NSP).EQ.1 ) THEN
            DSP_CX(NSP) = 1.D-6
!            DSP_CX(NSP) = 1.D-3
          ELSE
            SP_CX(NSP) = CMIN
!             SP_CX(NSP) = 1.d-30
            DSP_CX(NSP) = 1.D-16
!            DSP_CX(NSP) = 1.D-12
          ENDIF
        ENDIF
  230 CONTINUE
!
!---  Load parameters for activity coefficient
!     calculations for aqueous species  ---
!
      DO 240 NSP = 1,NSPL
         CLX(NSP) = SP_CX(NSP)
         DCLX(NSP) = DSP_CX(NSP)
  240 CONTINUE
!
!---  Activity coefficients for aqueous species  ---
!
      IF( IACTV.EQ.3 ) THEN
        DO 260 NSP = 1,NSPL
          DO 250 M = 1,NSPL+1
            ACTVX(M,NSP) = ACTVC
  250     CONTINUE
  260   CONTINUE
      ELSEIF( IACTV.EQ.1 ) THEN
        CALL DAVIES( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N), &
          RHOL(2,N),T(2,N),XLW(2,N) )
      ELSEIF( IACTV.EQ.2 ) THEN
        CALL PITZER( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N), &
          RHOL(2,N),T(2,N),XLW(2,N) )
      ELSE
        CALL BDOT( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N), &
          RHOL(2,N),T(2,N),XLW(2,N) )
      ENDIF
      ACTVS(1:LSPL) = ACTVX(1,1:LSPL)
!
!---  Initialize residual and residual partial
!     derivatives  ---
!
      DO 280 NEQ = 1,NEQR
        BJM(NEQ) = 0.D+0
        DO 270 M = 1,NEQR
          AJM(NEQ,M) = 0.D+0
  270   CONTINUE
  280 CONTINUE
!
!---  Loop over equations  ---
!
      IF( ISLC(57).EQ.0 ) THEN
      DO 300 NEQ = 1,NEQR
!
!---    Equilibrium equation  ---
!
        IF( NEQ.LE.NEQE ) THEN
          CALL EECHEM( ACTVX,AJM,BJM,SP_CX,DSP_CX,N,NEQ,INDX )
!
!---    Conservation equation  ---
!
        ELSEIF( NEQ.LE.NEQE+NEQC ) THEN
          CALL CECHEM( ACTVX,AJM,BJM,CX,SP_CX,N,NEQ,INDX )
!
!---    Kinetic equation  ---
!
        ELSEIF( NEQ.LE.NEQE+NEQC+NEQK ) THEN
          CALL KECHEM( ACTVX,AJM,BJM,COX,SP_CX,DSP_CX,N,NEQ,INDX,IER )
!
!---    Unknown equation  ---
!
        ELSE
          CHMSG = 'Unrecognized Equation Index: '
          IMSG = NEQ
          INDX = 12
          CALL WRMSGS( INDX )
        ENDIF
  300 CONTINUE
      ELSE
        DO 400 NEQ = 1,NEQR
!
!---      Equilibrium equation  ---
!         
          IF( NEQ.LE.NEQE ) THEN
            GOTO 400
!         
!---      Conservation equation  ---
!         
          ELSEIF( NEQ.LE.NEQE+NEQC ) THEN
            CALL CECHEM_R( ACTVX,AJM,BJM,CX,SP_CX,N,NEQ,INDX )
!         
!---      Kinetic equation  ---
!         
          ELSEIF( NEQ.LE.NEQE+NEQC+NEQK ) THEN
            CALL KECHEM_R( ACTVX,AJM,BJM,COX,SP_CX,DSP_CX,N,NEQ,INDX )
!         
!---      Unknown equation  ---
!         
          ELSE
            CHMSG = 'Unrecognized Equation Index: '
            IMSG = NEQ
            INDX = 12
            CALL WRMSGS( INDX )
          ENDIF
  400   CONTINUE
      ENDIF
!
!---  End of ECKEJCB group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE EECHEM_OLD( ACTVX,AJM,BJM,SP_CX,DSP_CX,N,NEQ,INDX )
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
!     Equilibrium Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!     Last Modified by MD White, PNNL, 7 January 2005.
!     $Id: eckechem.F,v 1.14 2006/09/19 14:41:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FDVP
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 ACTVX(LSPL+1,LSPL)
      REAL*8 BJM(LSPR),AJM(LSPR,LSPR),SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 ACTEX(LESITE)
      LOGICAL FCHK
!
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/EECHEM'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
      '$Id: eckechem.F,v 1.14 2006/09/19 14:41:18 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.100 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),100)) .AND.  &
       (NSTEP-NRST).EQ.0 ) THEN
          AJM(NEQ,NEQ) = 1.D+0
          BJM(NEQ) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!--- fixed species concentration or activity
!
      DO NSPKX = 1,NSPLK
       IF( ISPLK(14+NSPKX).LT.0 ) THEN
        NSPX = ABS(ISPLK(14+NSPKX))
        IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
        IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
          AJM(NEQ,NEQ) = 1.D+0
          BJM(NEQ) = 0.D+0
          GOTO 1000
        ENDIF
       ENDIF
      ENDDO
!
!---  Equilibrium constant as a function of temperature  ---
!
      NS = IEQ_E(1,NEQ)
      IRCE = IEQ_E((NS+2),NEQ)
      CALL EQCN( EQKX,T(2,N),IRCE,N )
!
!---  Volumetric concentration to molality  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
      VTOGX = 1.D+0/(MAX( 1.D-20,SG(2,N)*PORD(2,N)) )
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
      RHOWX = RHOL(2,N)*XLW(2,N)
!
!--- Prepare for exchanged species activity calculation
!
      IF( NSPE.NE.0 ) THEN
        DO 20 ISITE = 1,NESITE
          ACTEX(ISITE) = 0.0D+0
   20   CONTINUE
!
!--- Gaines-Thomas convention
!
        IF( IACTEX.EQ.1 ) THEN
          DO 30 NSP = 1,NSPE
            NCM_SP = NSPL + NSPS + NSP
            SP_CX(NCM_SP) = MAX(CMIN,SP_CX(NCM_SP))
            ISITE = ISP_E(NSP)
            ICAT = IEL_LK(NSP)
            ACTEX(ISITE) = ACTEX(ISITE)+SP_CX(NCM_SP)*SP_L(1,ICAT)
   30     CONTINUE
        ENDIF
      ENDIF
!
!---  Total number of species  ---
!
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Base residual  ---
!
!      C_NEG = EQKX**EQ_E(NS,NEQ)
      c_neg=eq_e(ns,neq)*log(eqkx)
!
!---  Loop over equilibrium species  ---
!
      DO 100 M = 1,NS
        NCM_SP = IEQ_E(M+1,NEQ)
!
!---    Aqueous species,
!       concentration in molality, mol/kg H2O  ---
!
        IF( NCM_SP.LE.NSPL ) THEN
          CMX = SP_CX(NCM_SP)*VTOMX
          ACX = max(ACTVX(1,NCM_SP),1.0d-30)
!
!---    Non-aqueous species,
!       concentration in kmolal, mol/kg H2O  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS) THEN
          CMX = SP_CX(NCM_SP)*VTOMX
          ACX = 1.D+0
!
!---    Exchanged species,
!       concentration in kmolal, mol/kg H2O  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE) THEN
          ISITE = ISP_E(NCM_SP-NSPL-NSPS)
          CMX = SP_CX(NCM_SP)
          ICAT = IEL_LK(NCM_SP-NSPL-NSPS)
          ACX = CMX*SP_L(1,ICAT)/ACTEX(ISITE)
          CMX = 1.D+0

!---    Gas species,
!       convert from mol/node volume to mol/volume of gas,
!       use RHOWX for Henry's law  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE+NSPG) THEN
          CMX = SP_CX(NCM_SP)*VTOGX/RHOWX
          ACX = 1.D+0
        ENDIF
!
!---    Equilibrium species  ---
!
        IF( M.EQ.1 ) THEN
          C_POS = ACX*CMX
!---    Conservation species  ---
!
        ELSE
!          C_NEG = C_NEG*((ACX*CMX)**EQ_E(M-1,NEQ))
          c_neg=c_neg+eq_e(m-1,neq)*log(acx*cmx)
        ENDIF
  100 CONTINUE
      c_neg = exp(c_neg)
      BJM(NEQ) = C_POS - C_NEG
!
!---  Return residual vector  ---
!
      IF( INDX.EQ.1 ) GOTO 1000
      DO 400 M=1,NS
        NCM_SP = IEQ_E(M+1,NEQ)
        IF( ISPLK(1).GT.100 ) THEN
!          IF( NCM_SP.EQ.MOD(ISPLK(1),100) .AND.
!     &      (NSTEP-NRST).EQ.0 ) CYCLE
        ENDIF
        DO NSPKX = 1,NSPLK
          NSPXX = ISPLK(14+NSPKX)
          IF(NSPXX.LT.0) THEN
            NSPXX = ABS(NSPXX)
            IF(NSPXX.GT.1000) NSPXX=NSPXX-1000
            IF(NSPXX.EQ.NCM_SP) CYCLE
          ENDIF
        ENDDO

        IF( NCM_SP.LE.NSPL) THEN
          CMX = SP_CX(NCM_SP)
          ACX = max(ACTVX(1,NCM_SP),1.d-30)
        ELSEIF( NCM_SP.LE.NSPL+NSPS+NSPE )THEN
          CMX = SP_CX(NCM_SP)
          CHARGX = SP_L(1,ICAT)
          ACX = CMX*CHARGX/ACTEX(ISITE)
          CMX = 1.D+0
        ELSE
          CMX = SP_CX(NCM_SP)
          ACX = 1.D+0
        ENDIF
        IF(M.EQ.1) THEN
          AJM(NEQ,IEQ_S(NCM_SP)) = ACX*VTOMX
        ELSE
          AJM(NEQ,IEQ_S(NCM_SP)) = -EQ_E(M-1,NEQ)*C_NEG/CMX
        ENDIF
  400 CONTINUE
!
!---  Return Jacobian matrix and residual vector  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NEQ) = -BJM(NEQ)
 1000 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of EECHEM group  ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE EECHEM( ACTVX,AJM,BJM,SP_CX,DSP_CX,N,NEQ,INDX )
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
!     Equilibrium Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!     Last Modified by MD White, PNNL, 7 January 2005.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FDVP
      USE CONST
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
      REAL*8 ACTVX(LSPL+1,LSPL)
      REAL*8 BJM(LSPR),AJM(LSPR,LSPR),SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 ACTEX(LESITE)
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/EECHEM'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.100 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),100)) .AND. &
          (NSTEP-NRST).EQ.0 ) THEN
          AJM(NEQ,NEQ) = 1.D+0
          BJM(NEQ) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!--- Fixed species concentration or activity  ---
!
      DO 10 NSLKX = 1,NSPLK
        IF( ISPLK(14+NSLKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSLKX))
          IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
          IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
            AJM(NEQ,NEQ) = 1.D+0
            BJM(NEQ) = 0.D+0
            GOTO 1000
          ENDIF
        ENDIF
   10 CONTINUE
!
!---  Total number of species  ---
!
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Equilibrium constant as a function of temperature  ---
!
      NS = IEQ_E(1,NEQ)
      IRCE = IEQ_E((NS+2),NEQ)
      CALL EQCN( EQKX,T(2,N),IRCE,N )
!
!---  Volumetric concentration to molality  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
      VTOGX = 1.D+0/(MAX( 1.D-20,SG(2,N)*PORD(2,N)) )
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
      RHOWX = RHOL(2,N)*XLW(2,N)
!
!--- Prepare for exchanged species activity calculation
!
      IF( NSPE.NE.0 ) THEN
        DO 20 ISITE = 1,NESITE
          ACTEX(ISITE) = 0.0D+0
   20   CONTINUE
!
!--- Gaines-Thomas convention
!
        IF( IACTEX.EQ.1 ) THEN
          DO 30 NSP = 1,NSPE
            NCM_SP = NSPL + NSPS + NSP
            SP_CX(NCM_SP) = MAX(CMIN,SP_CX(NCM_SP))
            ISITE = ISP_E(NSP)
            ICAT = IEL_LK(NSP)
            ACTEX(ISITE) = ACTEX(ISITE)+SP_CX(NCM_SP)*SP_L(1,ICAT)
   30     CONTINUE
        ENDIF
      ENDIF
!
!---  Base residual  ---
!
      IF( ISLC(60).EQ.0 ) THEN
        C_NEG = EQKX**EQ_E(NS,NEQ)
      ELSE
        C_NEG = EQ_E(NS,NEQ)*LOG(EQKX)
      ENDIF
!
!---  Loop over equilibrium species  ---
!
      DO 100 M = 1,NS
        NCM_SP = IEQ_E(M+1,NEQ)
!
!---    Aqueous species,
!       concentration in molality, mol/kg H2O  ---
!
        IF( NCM_SP.LE.NSPL ) THEN
          CMX = SP_CX(NCM_SP)*VTOMX
          ACX = ACTVX(1,NCM_SP)
!
!---    Non-aqueous species,
!       concentration in kmolal, mol/kg H2O  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS) THEN
          CMX = SP_CX(NCM_SP)*VTOMX
          ACX = 1.D+0
!
!---    Exchanged species,
!       concentration in kmolal, mol/kg H2O  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE) THEN
          ISITE = ISP_E(NCM_SP-NSPL-NSPS)
          CMX = SP_CX(NCM_SP)
          ICAT = IEL_LK(NCM_SP-NSPL-NSPS)
          ACX = CMX*SP_L(1,ICAT)/ACTEX(ISITE)
          CMX = 1.D+0
!
!---    Gas species,
!       convert from mol/node volume to mol/volume of gas,
!       use RHOWX for Henry's law  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE+NSPG) THEN
          CMX = SP_CX(NCM_SP)*VTOGX/RHOWX
          ACX = 1.D+0
        ENDIF
!
!---    Equilibrium species  ---
!
        IF( M.EQ.1 ) THEN
          C_POS = ACX*CMX
!
!---    Conservation species  ---
!
        ELSE
!xyl 
          islc(60) = 0
          IF( ISLC(60).EQ.0 ) THEN
           C_NEG = C_NEG*((ACX*CMX)**EQ_E(M-1,NEQ))
          ELSE
           C_NEG = C_NEG+EQ_E(M-1,NEQ)*LOG(ACX*CMX)
          ENDIF
!xyl
        ENDIF
  100 CONTINUE
      IF( ISLC(60).EQ.1 ) THEN
        C_NEG = EXP(C_NEG)
      ENDIF
      BJM(NEQ) = C_POS - C_NEG
!
!---  Return residual vector  ---
!
      IF( INDX.EQ.1 ) GOTO 1000
!
!---  Incremented residuals, computed numerically  ---
!
      DO 400 NSP = 1,NSPR
!
!---    Check whether specie is a equilibrium equation specie,
!         which affects the residual via the equilibrium equation
!       or an aqueous specie,
!         which affects the residual via the equilibrium equation,
!         via the equilibrium equation reactions
!         via the equilibrium constant,
!         via the activity coefficient,
!         via the ionic strength  ---
!
        FCHK = .FALSE.
        IF( NSP.LE.NSPL ) FCHK = .TRUE.
!
!---    Loop over equilibrium equation species  ---
!
        IF( .NOT.FCHK ) THEN
          DO 310 M = 1,NS
            NSP_E = IEQ_E(M+1,NEQ)
            IF( NSP.EQ.NSP_E ) THEN
              FCHK = .TRUE.
              GOTO 320
            ENDIF
  310     CONTINUE
  320     CONTINUE
        ENDIF
!
!---    Skip for initial pH  ---
!
        IF( ISPLK(1).GT.100 ) THEN
          IF( NSP.EQ.MOD(ISPLK(1),100) .AND. &
            (NSTEP-NRST).EQ.0 ) FCHK = .FALSE.
        ENDIF
!
!---    Specie is either an equilibrium equation specie
!       or an aqueous specie  ---
!
        IF( FCHK ) THEN
!
!---      Base residual  ---
!
          IF( ISLC(60).EQ.0 ) THEN
            C_NEG = EQKX**EQ_E(NS,NEQ)
          ELSE
            C_NEG = EQ_E(NS,NEQ)*LOG(EQKX)
          ENDIF
!
!---      Loop over equilibrium species  ---
!
          DO 330 M = 1,NS
            NCM_SP = IEQ_E(M+1,NEQ)
!
!---        Aqueous species,
!           concentration in molality, mol/kg H2O  ---
!
            IF( NCM_SP.LE.NSPL ) THEN
!
!---          Incremented species  ---
!
              IF( NCM_SP.EQ.NSP )  THEN
                CMX = (SP_CX(NCM_SP)+DSP_CX(NCM_SP))*VTOMX
                ACX = ACTVX(NSP+1,NCM_SP)
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NCM_SP)*VTOMX
                ACX = ACTVX(1,NCM_SP)
              ENDIF
!
!---        Non-aqueous species,
!           concentration in kmolal, mol/kg H2O  ---
!
            ELSEIF(NCM_SP.LE.NSPL+NSPS) THEN
!
!---          Incremented species  ---
!
              IF( NCM_SP.EQ.NSP )  THEN
                CMX = (SP_CX(NCM_SP)+DSP_CX(NCM_SP))*VTOMX
                ACX = 1.D+0
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NCM_SP)*VTOMX
                ACX = 1.D+0
              ENDIF
!
!---        Exchanged species  ---
!
            ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE) THEN
!
!---          Incremented species  ---
!
              ISITE = ISP_E(NCM_SP-NSPL-NSPS)
              ICAT = IEL_LK(NCM_SP-NSPL-NSPS)
              CHARGX = SP_L(1,ICAT)
              IF( NCM_SP.EQ.NSP )  THEN
                CMX = SP_CX(NCM_SP)+DSP_CX(NCM_SP)
                ACX = CMX*CHARGX/(ACTEX(ISITE)+DSP_CX(NCM_SP)*CHARGX)
                CMX = 1.D+0
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NCM_SP)
                ACX = CMX*CHARGX/ACTEX(ISITE)
                CMX = 1.D+0
              ENDIF
!
!---        Gas species
!
            ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE+NSPG) THEN
!
!---          Incremented species  ---
!
              IF( NCM_SP.EQ.NSP )  THEN
                CMX = (SP_CX(NCM_SP)+DSP_CX(NCM_SP))*VTOGX/RHOWX
                ACX = 1.D+0
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NCM_SP)*VTOGX/RHOWX
                ACX = 1.D+0
              ENDIF
            ENDIF
!
!---        Equilibrium species  ---
!
            IF( M.EQ.1 ) THEN
              C_POS = ACX*CMX
!
!---        Conservation species  ---
!
            ELSE
!xyl 
              IF( ISLC(60).EQ.0 ) THEN
                C_NEG = C_NEG*((ACX*CMX)**EQ_E(M-1,NEQ))
              ELSE
                C_NEG = C_NEG+EQ_E(M-1,NEQ)*LOG(ACX*CMX)
              ENDIF
!xyl
            ENDIF
  330     CONTINUE
          IF( ISLC(60).EQ.1 ) THEN
            C_NEG = EXP(C_NEG)
          ENDIF
          AVX = C_NEG + BJM(NEQ)
          BVX = ABS(C_NEG) + ABS(BJM(NEQ))
          CVX = C_POS
          IF( BVX/EPSL.GT.EPSL ) THEN
            IF( ABS(AVX)/BVX.GT.EPSL ) THEN
              CVX = C_POS - AVX
            ENDIF
          ENDIF
!          AJM(NEQ,IEQ_S(NSP)) = (C_POS - C_NEG - BJM(NEQ))/DSP_CX(NSP)
          AJM(NEQ,IEQ_S(NSP)) = CVX/DSP_CX(NSP)
        ENDIF
  400 CONTINUE
!
!---  Return Jacobian matrix and residual vector  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NEQ) = -BJM(NEQ)
 1000 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of EECHEM group  ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ELECTS(ZI,ZJ,IT,CPIX,APHI)
!
!----------------------Description-------------------------------------!
!
! This subroutine calculates higher order electrostatic functions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by A Felmy; from GMIN
!     Last Modified by VL Freedman, PNNL, 19 March 2007
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE CONST
      USE PTZRCOEF
      USE PTZR
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
      REAL*8 A1(21),A2(21),A3(7),A4(9),A5(9),A6(9),P(10)
      REAL*8 J01,J02,J03,J11,J12,J13,J0,J1,J2,J21,J22,J23

      DATA a1/-.000000000010991D0,-.000000000002563D0 &
       ,0.000000000001943D0,0.000000000046333D0,-.000000000050847D0 &
       ,-.000000000821969D0,0.000000001229405D0,0.000000013522610D0 &
       ,-.000000025267769D0,-.000000202099617D0,0.000000396566462D0 &
       ,0.000002937706971D0,-.000004537895710D0,-.000045036975204D0 &
       ,0.000036583601823D0,0.000636874599598D0,0.000388260636404D0 &
       ,-.007299499690937D0,-.029779077456514D0,-.060076477753119D0 &
       ,1.925154014814667D0/

      DATA a2/0.000000000237816D0,-.000000002849257D0 & 
      ,-.000000006944757D0,0.000000004558555D0,0.000000080779570D0 & 
      ,0.000000216991779D0,-.000000250453880D0,-.000003548684306D0 & 
      ,-.000004583768938D0,0.000034682122751D0,0.000087294451594D0 & 
      ,-.000242107641309D0,-.000887171310131D0,0.001130378079086D0 & 
      ,0.006519840398744D0,-.001668087945272D0,-.036552745910311D0 & 
      ,-.028796057604906D0,0.150044637187895D0,0.462762985338493D0 & 
      ,0.628023320520852D0/

      DATA a3/.000029308779366,.000029869648486,.000009838580455 & 
       ,.000000827954226,-.000000098522914,.000000013943401 & 
       ,-.000000005963131/

      DATA a4/.018407992691,.023626104695,.005004621881 & 
       ,-.000300844194,-.000001533185,.000009318246,-.000004258797 & 
       ,.000001509090,-.000000766492/

      DATA a5/3.9986000731,3.7950588585,-.3325673918 & 
       ,-.0899335274,.1338914658,.1948882815,.2368262404 & 
       ,.1379406836,.0478072558/

      DATA a6/37.837805702,24.470110234,-3.719597578 & 
       ,0.991895847,-.327141729,.121485594,-.051539972 & 
       ,.017779940,-.011800766/
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/ELECTS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
      DSQI=DSQRT(CPIX)
      X1 = 6.0d0*APHI*DSQI
      P(1)=1.0D0
!
!--- Calculate integrals(see Harvie 1981)
!
      DO I = 1,3
        IF( I.EQ.1 ) XX = X1*ZI*ZI
        IF( I.EQ.2 ) XX = X1*ZJ*ZJ
        IF( I.EQ.3 ) XX = X1*ZI*ZJ
        BK = 0.0D0
        DK = 0.0D0
        BK1 = 0.0D0
        BK2 = 0.0D0
        DK1 = 0.0D0
        DK2 = 0.0D0
        IF( XX.LE.1.0D0 )THEN
          TT = 4.0d0*(XX**0.2D0)
          DZ = TT/(5.0d0*XX)
          ZZ = TT-2.0D0
          IF( XX.LE.0.05D0 )THEN
            Z2 = (XX/0.0245D0)-1.040816D0
            P(2) = Z2
            DO K = 3,7
              P(K)=2.0d0*Z2*P(K-1)-P(K-2)
            END DO
            J2 = 0.0D0
            DO K = 1,7
              J2 = J2+A3(K)*P(K)
            END DO
          ELSE
            Z2 = (XX/0.475D0)-1.105263D0
            P(2)=Z2
            DO K = 3,9
              P(K) = 2.0d0*Z2*P(K-1)-P(K-2)
            END DO
            J2 = 0.0D0
            DO K = 1,9
              J2 = J2+A4(K)*P(K)
            END DO
          END IF
          DO K = 1,21
            BK2 = BK1
            BK1 = BK
            BK = ZZ*BK1-BK2+A1(K)
            DK2 = DK1
            DK1 = DK
            DK = BK1+ZZ*DK1-DK2
          END DO
        ELSE
          TT = 4.444444444D0*(XX**(-.10D0))
          DZ = -TT/(10.0D0*XX)
          ZZ = TT-2.44444444D0
          IF( XX.LE.50.0D0 )THEN
            Z2 = (XX/24.5D0)-1.040816
            P(2) = Z2
            DO K = 3,9
              P(K) = 2.0D0*Z2*P(K-1)-P(K-2)
            END DO
            J2 = 0.0D0
            DO K = 1,9
              J2 = J2+A5(K)*P(K)
            END DO
        ELSE
            Z2 = (XX/425.0D0)-1.117647
            P(2) = Z2
            DO K = 3,9
              P(K) = 2.0D0*Z2*P(K-1)-P(k-2)
            END DO
            J2 = 0.0D0
            DO K = 1,9
              J2 = J2+A6(K)*P(K)
            END DO
          END IF
          DO K = 1,21
            BK2 = BK1
            BK1 = BK
            BK = ZZ*BK1-BK2+A2(K)
            DK2 = DK1
            DK1 = DK
            DK = BK1+ZZ*DK1-DK2
          END DO
        END IF
!
!--- Now calculate electrostatic functions
!
        J0 =0.25D0*xx+0.5D0*(BK-BK2)-1.0D0
        J1 = XX*(0.25D0+0.5D0*DZ*(DK-DK2))
        J2 = J2/XX
        J03 = J0
        J13 = J1
        J23 = J2
        IF( I.EQ.1 )THEN
          J01 = J0
          J11 = J1
          J21 = J2
        END IF
        IF( I.eq.2 )THEN
          J02 = J0
          J12 = J1
          J22 = J2
        END IF
      END DO
!
!--- Now calculate eth and ethp
!
      TMP = (ZI*ZJ)/(4.0D0*CPIX)
      ETH(IT) = TMP*(J03-0.5D0*(J01+J02))
      ETHP(IT) = (TMP/(2.0D0*CPIX))*(J13-0.5D0*(J11+J12)) &
               -ETH(IT)/CPIX

      ETHP2(IT) = -(1.0D0/(2.0D0*CPIX))*(ETH(IT)/CPIX+5.0D0*ETHP(IT)) &
       +(TMP/(4.0D0*CPIX*CPIX))*(J23-0.5D0*(J21+J22))
!
!---  End of ELECTS ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE EQCN( EQKX,TX,INDX,N )
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
!     Equilibrium reaction constant as a function of temperature.
!
!     INDX > 0 : equilibrium reaction index  RC_E
!     INDX < 0 : kinetic reaction index RC_K
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 December 2004.
!     Last Modified by MD White, PNNL, 21 December 2004.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE CONST
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
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/EQCN'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Absolute temperature  ---
!
      TKX = TX+TABS
!
!---  Equilibrium reaction  ---
!
      IF( INDX.GT.0 ) THEN
        EQKX = RC_E(1,INDX)*LOG(TKX) + RC_E(2,INDX) + &
         RC_E(3,INDX)*TKX + RC_E(4,INDX)/TKX + &
         RC_E(5,INDX)/(TKX**2)
        EQKX = EXP(TOLN*EQKX)
!
!---  Kinetic reaction  ---
!
      ELSEIF( INDX.LT.0 ) THEN
        JNDX = -INDX
        NSPRX = IRC_K(1,JNDX)
        NSPPX = IRC_K(2,JNDX)
        NSPKX = NSPRX+NSPPX
        N4 = MAX(1,N*IRCKN(NSPKX+4))
        N5 = MAX(1,N*IRCKN(NSPKX+5))
        N6 = MAX(1,N*IRCKN(NSPKX+6))
        N7 = MAX(1,N*IRCKN(NSPKX+7))
        N8 = MAX(1,N*IRCKN(NSPKX+8))
        EQKX = RC_K(NSPKX+4,N4,JNDX)*LOG(TKX) + RC_K(NSPKX+5,N5,JNDX) + &
          RC_K(NSPKX+6,N6,JNDX)*TKX + RC_K(NSPKX+7,N7,JNDX)/TKX + &
          RC_K(NSPKX+8,N8,JNDX)/(TKX**2)
        EQKX = EXP(TOLN*EQKX)
      ELSE
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of EQCN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE EQEQ( EQKX,SP_CX,VTOMX )
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
!     Calculate equilibrium species concentrations according to
!     the conservation species concentrations, assuming
!     an activity of 1.0
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 August 2005.
!     Last Modified by MD White, PNNL, 24 August 2005.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
      REAL*8 SP_CX(LSPR)
      REAL*8 EQKX(LEQE)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/EQEQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Set equilibrium species concentrations according to
!     the conservation species concentrations, assuming
!     an activity of 1.0  ---
!
      DO 40 NEQ = 1,NEQE
        NEQ_SP = IEQ_E(2,NEQ)
        NS = IEQ_E(1,NEQ)
!
!---    Equilibrium constant and exponent  ---
!
!xyl
        IF( ISLC(60).EQ.0 ) THEN
          C_NEG = EQKX(NEQ)**EQ_E(NS,NEQ)
        ELSE
          C_NEG = EQ_E(NS,NEQ)*LOG(EQKX(NEQ))
        ENDIF
!xyl
!
!---    Loop over species in equilibrium equation
!       skipping the equilibrium species  ---
!
        DO 30 K = 1,NS
          NCM_SP = IEQ_E(K+1,NEQ)
!
!---      Skip equilibrium species  ---
!
          IF( NCM_SP.EQ.NEQ_SP ) GOTO 30
!
!---      Convert conservation species concentration to
!         molality, mol/kg H2O  ---
!
          SP_CX(NCM_SP) = MAX(CMIN,SP_CX(NCM_SP))
          CMX = SP_CX(NCM_SP)*VTOMX
!xyl
        IF( ISLC(60).EQ.0 ) THEN
          C_NEG = C_NEG*(CMX**EQ_E(K-1,NEQ))
        ELSE
          C_NEG = C_NEG+EQ_E(K-1,NEQ)*LOG(CMX)
        ENDIF
   30   CONTINUE
!xyl
        IF( ISLC(60).EQ.1 ) THEN
          C_NEG = EXP(C_NEG)
        ENDIF
!
!---    Convert equilibrium species concentration to
!       node volumetric, mol/m^3  ---
!
        SP_CX(NEQ_SP) = C_NEG/VTOMX
   40 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of EQEQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE FLHSP
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
!     Convert reactive species initial condition concentrations into
!     mol/m^3 node volume
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 18 August 2005.
!     Last Modified by MD White, PNNL, 18 August 2005.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE FDVP
      USE CONST
      USE BCV
      USE PORMED
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
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 GETSPNM
      EXTERNAL GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/FLHSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
      ME = GA_NODEID()
      USE_GA = .TRUE.
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
      ALLOCATE(IMMB(LSOLU+LSPT))
      IMMB = 0
!
!---  Define completely immobile conservation component species  ---
!
      DO 2 NEQ = 1,NEQC
        IMMB(NEQ) = 1
!
!---    Loop over conservation-component species  ---
!
        DO 4 M = 1,IEQ_C(1,NEQ)
          NSP = IEQ_C(M+1,NEQ)
!
!---      Mobile species ---
!
          IF( NSP.LE.NSPL .OR. NSP.GT.(NSPL+NSPS+NSPE) ) IMMB(NEQ) = 0
    4   CONTINUE
    2 CONTINUE
!
!---  Define completely immobile kinetic component species  ---
!
      DO 8 NEQ = 1,NEQK
        IMMB(NEQ+NEQC) = 1
!
!---    Loop over kinetic-component species  ---
!
        DO 6 M = 1,IEQ_K(1,NEQ)
          NSP = IEQ_K(M+1,NEQ)
!
!---      Mobile species ---
!
          IF( NSP.LE.NSPL .OR. NSP.GT.(NSPL+NSPS+NSPE) ) &
            IMMB(NEQ+NEQC) = 0
    6   CONTINUE
    8 CONTINUE
!
!---  Loop over all nodes, skipping inactive nodes  ---
!
      DO 400 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 400
        IZN = N
!
!---    Loop over reactive species  ---
!
        DO 100 NSP = 1,NSPR
          IF( INDEX(GETSPNM(NSP),'fix').NE.0 ) THEN
            SP_CI(NSP,N) = SP_C(NSP,N)
          ENDIF
!
!---      Restart simulation  ---
!
          IF( IEO.EQ.2 .OR. IEO.EQ.4 ) THEN
!fixed pH
            if(mod(isplk(1),100) == nsp) cycle
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( IC_SP(NSP,N).EQ.11 ) THEN
              SP_C(NSP,N) = 1.D+3*SP_C(NSP,N)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(NSP,N).EQ.12 ) THEN
              SP_C(NSP,N) = 1.D+3*SP_C(NSP,N)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(NSP,N).EQ.13 ) THEN
              SP_C(NSP,N) = 1.D+3*SP_C(NSP,N)*SL(2,N)*PORD(2,N)* &
                RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(NSP,N).EQ.14 ) THEN
              SP_C(NSP,N) = 1.D+3*SP_C(NSP,N)*SG(2,N)*PORD(2,N)
!
!---        Convert solid-species mineral volumetric fraction
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
              DO 10 NRC = 1,NRCK
                IF( IRCKT(NRC).EQ.20 ) THEN
                  NSPX = IRC_K(1,NRC)
                ELSE
                  NSPRX = IRC_K(1,NRC)
                  NSPPX = IRC_K(2,NRC)
                  NSPX = IRC_K(3+NSPRX+NSPPX,NRC)
                ENDIF
                IF( NSP.EQ.NSPX ) THEN
                  NSPX = NSP-NSPL
!
!---              Restart simulation w/ lithology overwrite  ---
!
                  IF( SP_S(2,NSPX)/EPSL.GT.EPSL .AND. &
                    ISP_OW(NSPX,N).EQ.1 ) THEN
                    SP_CMN(NSPX,N) = 1.D+3*RS_S(2,NSPX,N)* &
                      SP_S(1,NSPX)/SP_S(2,NSPX)
                  ENDIF
                  GOTO 20
                ENDIF
  10          CONTINUE
  20          CONTINUE
            ENDIF
!
!---      Normal simulation  ---
!
          ELSE
!fixed pH
            if(mod(isplk(1),100) == nsp) cycle
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( IC_SP(NSP,N).EQ.1 ) THEN
              SP_C(NSP,N) = 1.D+3*SP_C(NSP,N)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(NSP,N).EQ.2 ) THEN
              SP_C(NSP,N) = 1.D+3*SP_C(NSP,N)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(NSP,N).EQ.3 ) THEN
              SP_C(NSP,N) = 1.D+3*SP_C(NSP,N)*SL(2,N)*PORD(2,N)* &
                RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(NSP,N).EQ.4 ) THEN
              SP_C(NSP,N) = 1.D+3*SP_C(NSP,N)*SG(2,N)*PORD(2,N)
!
!---        Convert solid-species mineral volumetric fraction
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
              DO 30 NRC = 1,NRCK
                IF( IRCKT(NRC).EQ.20 ) THEN
                  NSPX = IRC_K(1,NRC)
                ELSE
                  NSPRX = IRC_K(1,NRC)
                  NSPPX = IRC_K(2,NRC)
                  NSPX = IRC_K(3+NSPRX+NSPPX,NRC)
                ENDIF
                IF( NSP.EQ.NSPX ) THEN
                  NSPX = NSP-NSPL
                  IF( SP_S(2,NSPX)/EPSL.GT.EPSL ) THEN
                    SP_CMN(NSPX,N) = 1.D+3*RS_S(2,NSPX,N)* &
                      SP_S(1,NSPX)/SP_S(2,NSPX)
!                      SP_S(1,NSPX)/SP_S(2,NSPX)+1.d-10
                  ENDIF
                  GOTO 40
                ENDIF
  30          CONTINUE
  40          CONTINUE
            ENDIF
          ENDIF
 100    CONTINUE
!
!---    Component species concentration  ---
!
        DO 200 NEQ = 1,NEQC
          NSL = NEQ + NSOLU
!
!---      Restart simulation  ---
!
          IF( IEO.EQ.2 .OR. IEO.EQ.4 ) THEN
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( ICT(NSL,N).EQ.11 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.12 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.13 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SL(2,N)*PORD(2,N)* &
                RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.14 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SG(2,N)*PORD(2,N)
            ELSE
              IEDL(NSL) = ISP_IEDL
              IF( IEQW.GT.0 ) THEN
                SMDL(NSL) = SP_MDL
                IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
                  SDCL(1,NSL,IZN) = SP_SDCL(1)
                  SDCL(2,NSL,IZN) = SP_SDCL(2)
                  SDCL(3,NSL,IZN) = SP_SDCL(3)
                ENDIF
              ENDIF
              IF( IEQA.GT.0 ) SMDG(NSL) = SP_MDG
              IF( IEQO.GT.0 ) SMDN(NSL) = SP_MDN
              C(NSL,N) = 0.D+0
              DO 110 NSP = 1,IEQ_C(1,NEQ)
                C(NSL,N) = C(NSL,N)+EQ_C(NSP,NEQ)* &
                  SP_C(IEQ_C(NSP+1,NEQ),N)
 110          CONTINUE
            ENDIF
            CO(NSL,N) = C(NSL,N)
!
!---      Normal simulation  ---
!
          ELSE
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( ICT(NSL,N).EQ.1 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.2 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.3 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SL(2,N)*PORD(2,N)* &
                RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.4 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SG(2,N)*PORD(2,N)
            ELSE
              IEDL(NSL) = ISP_IEDL
              IF( IEQW.GT.0 ) THEN
                SMDL(NSL) = SP_MDL
                IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
                  SDCL(1,NSL,IZN) = SP_SDCL(1)
                  SDCL(2,NSL,IZN) = SP_SDCL(2)
                  SDCL(3,NSL,IZN) = SP_SDCL(3)
                ENDIF
              ENDIF
              IF( IEQA.GT.0 ) SMDG(NSL) = SP_MDG
              IF( IEQO.GT.0 ) SMDN(NSL) = SP_MDN
              C(NSL,N) = 0.D+0
              DO 120 NSP = 1,IEQ_C(1,NEQ)
                C(NSL,N) = C(NSL,N)+EQ_C(NSP,NEQ)* &
                  SP_C(IEQ_C(NSP+1,NEQ),N)
 120          CONTINUE
            ENDIF
            CO(NSL,N) = C(NSL,N)
          ENDIF
 200    CONTINUE
!
!---    Kinetic species concentration  ---
!
        DO 300 NEQ = 1,NEQK
          NSL = NEQ + NEQC + NSOLU
!
!---      Restart simulation  ---
!
          IF( IEO.EQ.2 .OR. IEO.EQ.4 ) THEN
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( ICT(NSL,N).EQ.11 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.12 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.13 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SL(2,N)*PORD(2,N)* &
                RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.14 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SG(2,N)*PORD(2,N)
            ELSE
              IEDL(NSL) = ISP_IEDL
              IF( IEQW.GT.0 ) THEN
                SMDL(NSL) = SP_MDL
                IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
                  SDCL(1,NSL,IZN) = SP_SDCL(1)
                  SDCL(2,NSL,IZN) = SP_SDCL(2)
                  SDCL(3,NSL,IZN) = SP_SDCL(3)
                ENDIF
              ENDIF
              IF( IEQA.GT.0 ) SMDG(NSL) = SP_MDG
              IF( IEQO.GT.0 ) SMDN(NSL) = SP_MDN
              C(NSL,N) = 0.D+0
              DO 220 NSP = 1,IEQ_K(1,NEQ)
                C(NSL,N) = C(NSL,N)+EQ_K(NSP,NEQ)* &
                  SP_C(IEQ_K(NSP+1,NEQ),N)
 220          CONTINUE
              CO(NSL,N) = C(NSL,N)
            ENDIF
!
!---      Normal simulation  ---
!
          ELSE
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( ICT(NSL,N).EQ.1 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.2 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.3 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SL(2,N)*PORD(2,N)* &
                RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(NSL,N).EQ.4 ) THEN
              C(NSL,N) = 1.D+3*C(NSL,N)*SG(2,N)*PORD(2,N)
            ELSE
              IEDL(NSL) = ISP_IEDL
              IF( IEQW.GT.0 ) THEN
                SMDL(NSL) = SP_MDL
                IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
                  SDCL(1,NSL,IZN) = SP_SDCL(1)
                  SDCL(2,NSL,IZN) = SP_SDCL(2)
                  SDCL(3,NSL,IZN) = SP_SDCL(3)
                ENDIF
              ENDIF
              IF( IEQA.GT.0 ) SMDG(NSL) = SP_MDG
              IF( IEQO.GT.0 ) SMDN(NSL) = SP_MDN
              C(NSL,N) = 0.D+0
              DO 230 NSP = 1,IEQ_K(1,NEQ)
                C(NSL,N) = C(NSL,N)+EQ_K(NSP,NEQ)* &
                  SP_C(IEQ_K(NSP+1,NEQ),N)
 230          CONTINUE
              CO(NSL,N) = C(NSL,N)
            ENDIF
          ENDIF
 300    CONTINUE
 400  CONTINUE
!
!---  Assign old species concentrations  ---
!
      DO 510 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 510
        IZN = N
!
!---    Loop over reactive species  ---
!
        DO 500 NSP = 1,NSPR
          SP_CO(NSP,N) = SP_C(NSP,N)
  500   CONTINUE
  510 CONTINUE
!
!---  Assign boundary solute concentrations for initial condition
!     type boundary conditions  ---
!
      DO 530 NSP = 1,NSPR
        DO 520 NB = 1,NUM_BCNX
          N = IBCN(NB)
          SP_CBO(NB,NSP) = SP_C(NSP,N)
  520   CONTINUE
  530 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of FLHSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      FUNCTION GETSPNM( INDX )
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
!     Get species name from global species index.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 16 December 2004.
!     Last Modified by MD White, PNNL, 16 December 2004.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
      CHARACTER*64 GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/GETSPNM'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Aqueous species  ---
!
      IF( INDX.LE.NSPL ) THEN
        GETSPNM = SPNML(INDX)
!
!---  Solid species  ---
!
      ELSEIF( INDX.LE.(NSPL+NSPS) ) THEN
        GETSPNM = SPNMS(INDX-NSPL)
!
!---  Exchange species  ---
!
      ELSEIF( INDX.LE.(NSPL+NSPS+NSPE) ) THEN
        GETSPNM = SPNME(INDX-NSPL-NSPS)
!
!---  Gas species  ---
!
      ELSEIF( INDX.LE.(NSPL+NSPS+NSPE+NSPG) ) THEN
        GETSPNM = SPNMG(INDX-NSPL-NSPS-NSPE)
!
!---  NAPL species  ---
!
      ELSEIF( INDX.LE.(NSPL+NSPS+NSPE+NSPG+NSPN) ) THEN
        GETSPNM = SPNMN(INDX-NSPL-NSPS-NSPE-NSPG)
      ELSE
        CHMSG = 'Unrecognized Global Species Index: '
        IMSG = INDX
        INDX = 12
        CALL WRMSGS( INDX )
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of GETSPNM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GUSPCN( ME,CX,SP_CX,N )
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
!     Guess specie concentrations using component rule of thumb,
!     equilibrium equations, and pH.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 26 January 2005.
!     Last Modified by MD White, PNNL, 26 January 2005.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FDVP
      USE CONST
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
      REAL*8 AJM(LEQC,LEQC),BJM(LEQC)
      REAL*8 CX(LEQC+LEQK),CIX(LEQC+LEQK)
      REAL*8 SP_CX(LSPR),SP_CIX(LSPR)
      REAL*8 ERRX(2),DERRX(2,LEQC),EQKX(LEQE)
!     INTEGER ICIX(LSPR)
      INTEGER INEG(LEQC),IPOS(LEQC)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/GUSPCN'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Total number of equations and species  ---
!
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Volumetric concentration to molality  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!
!---  Set initial concentrations  ---
!
      DO 10 NSP = 1,NSPR
        SP_CIX(NSP) = SP_CX(NSP)
   10 CONTINUE
!
!---  Set conservation species concentrations to 1/50 of
!     the total component concentration  ---
!
      NSPH = MOD(ISPLK(1),100)
      DO 20 NEQ = 1,NEQC
        NSP = IEQ_C(2,NEQ)
        IF( NSP.EQ.NSPH ) GOTO 20
        SP_CIX(NSP) = MAX( CX(NEQ)*5.D-2,0.D+0 )
!        SP_CIX(NSP) = MAX( CX(NEQ)*5.D-2,CMIN )
   20 CONTINUE
!
!---  If uninitialized set pH to 1.D-7 molality, mol/kg water  ---
!
      IF( NSPH.GE.1 .AND. NSPH.LE.LSPR ) THEN
        IF( SP_CX(NSPH)/EPSL.LT.EPSL ) SP_CIX(NSPH) = 1.D-7/VTOMX
      ENDIF
!
!---  Equilibrium constant as a function of temperature  ---
!
      DO 40 NEQ = 1,NEQE
        NS = IEQ_E(1,NEQ)
        IRCE = IEQ_E((NS+2),NEQ)
        CALL EQCN( EQKX(NEQ),T(2,N),IRCE,N )
   40 CONTINUE
!
!---  Recalculate equilibrium species concentrations according to
!     the conservation species concentrations, assuming
!     an activity of 1.0  ---
!
      CALL EQEQ( EQKX,SP_CIX,VTOMX )
!
!---  Total error in conservation species concentration  ---
!
      DO 50 NEQ = 1,NEQC
        DERRX(1,NEQ) = 0.D+0
        CIX(NEQ) = 0.D+0
        DO 45 M = 1,IEQ_C(1,NEQ)
          NSP = IEQ_C(M+1,NEQ)
          CIX(NEQ) = CIX(NEQ)+EQ_C(M,NEQ)*SP_CIX(NSP)
   45   CONTINUE
        DERRX(1,NEQ) = ABS(CIX(NEQ)-CX(NEQ))/(CX(NEQ)+SMALL)
   50 CONTINUE
!
!---  Adjust conservation species concentrations by 1.667 and search
!     for improvements in total error in component species
!     concentrations  ---
!
      NC = 0
   60 CONTINUE
      NC = NC + 1
!
!---  Loop over conservation species  ---
!
      DO 200 NEQX = 1,NEQC
        NSPX = IEQ_C(2,NEQX)
!
!---    Initialize conservation species direction flags  ---
!
        IPOS(NEQX) = 0
        INEG(NEQX) = 0
!
!---  Skip for initial pH  ---
!
        IF( NSPX.EQ.NSPH .AND. ISPLK(1).GT.100 ) GOTO 200
!
!---    Decrease conservation species concentration by 1.667  ---
!
!        CALL EQEQ( EQKX,SP_CIX,VTOMX )
        SP_CIX(NSPX) = SP_CIX(NSPX)/1.667D+0
!
!---    Recalculate equilibrium species concentrations according to
!       the conservation species concentrations, assuming
!       an activity of 1.0  ---
!
        CALL EQEQ( EQKX,SP_CIX,VTOMX )
!
!---    Total error in conservation species concentration  ---
!
        DO 110 NEQ = 1,NEQC
          DERRX(2,NEQ) = 0.D+0
          CIX(NEQ) = 0.D+0
          DO 100 M = 1,IEQ_C(1,NEQ)
            NSP = IEQ_C(M+1,NEQ)
            CIX(NEQ) = CIX(NEQ)+EQ_C(M,NEQ)*SP_CIX(NSP)
  100     CONTINUE
          DERRX(2,NEQ) = ABS(CIX(NEQ)-CX(NEQ))/(CX(NEQ)+SMALL)
  110   CONTINUE
!
!---    Check for improvement in total error in component species
!       concentration  ---
!
!        CERRX = (ERRX(1)-ERRX(2))/(ERRX(1)+SMALL)
        CERRX = (DERRX(1,NEQX)-DERRX(2,NEQX))/(DERRX(1,NEQX)+SMALL)
!
!---    Improvement found  ---
!
        IF( CERRX.GT.1.D-2 ) THEN
!          ERRX(1) = ERRX(2)
          DO 112 NEQ = 1,NEQC
            DERRX(1,NEQ) = DERRX(2,NEQ)
  112     CONTINUE
          INEG(NEQX) = 1
!
!---    No improvement found, increase conservation species
!       concentration ---
!
        ELSE
!          CALL EQEQ( EQKX,SP_CIX,VTOMX )
          SP_CIX(NSPX) = SP_CIX(NSPX)*(1.667D+0**2)
!
!---      Recalculate equilibrium species concentrations according to
!         the conservation species concentrations, assuming
!         an activity of 1.0  ---
!
          CALL EQEQ( EQKX,SP_CIX,VTOMX )
!
!---      Total error in component species concentration  ---
!
          DO 130 NEQ = 1,NEQC
            DERRX(2,NEQ) = 0.D+0
            CIX(NEQ) = 0.D+0
            DO 120 M = 1,IEQ_C(1,NEQ)
              NSP = IEQ_C(M+1,NEQ)
              CIX(NEQ) = CIX(NEQ)+EQ_C(M,NEQ)*SP_CIX(NSP)
  120       CONTINUE
            DERRX(2,NEQ) = ABS(CIX(NEQ)-CX(NEQ))/(CX(NEQ)+SMALL)
  130     CONTINUE
!
!---      Check for improvement in total error in component species
!         concentration  ---
!
          CERRX = (DERRX(1,NEQX)-DERRX(2,NEQX))/(DERRX(1,NEQX)+SMALL)
!
!---      Improvement found  ---
!
          IF( CERRX.GT.1.D-2 ) THEN
!            ERRX(1) = ERRX(2)
            DO 132 NEQ = 1,NEQC
              DERRX(1,NEQ) = DERRX(2,NEQ)
  132       CONTINUE
            IPOS(NEQX) = 1
!
!---      No improvement found, reset conservation species
!         concentration ---
!
          ELSE
            SP_CIX(NSPX) = SP_CIX(NSPX)/1.667D+0
          ENDIF
        ENDIF
  200 CONTINUE
!
!---  Loop over conservation species, checking for no
!     further improvements  ---
!
      DO 300 NEQ = 1,NEQC
        IF( IPOS(NEQ).EQ.1 .OR. INEG(NEQ).EQ.1 ) GOTO 60
  300 CONTINUE
!
!---  Set species concentration guesses  ---
!
      DO 400 NSP = 1,NSPR
        SP_CX(NSP) = MAX( SP_CIX(NSP),0.D+0 )
  400 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of GUSPCN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE HOMIX(CPIX,APHI)
!
!----------------------Description-------------------------------------!
!
! This subroutine calculates higher order mixing terms for
! unsymmetrical electrolyte mixings.
!
!----------------------Authors-----------------------------------------!
!
!     Written by A. Felmy, from GMIN
!     Last Modified by VL Freedman, PNNL, 19 March 2007
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE CONST
      USE PTZRCOEF
      USE PTZR
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
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/HOMIX'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
      MAXETH = LMCG**2
      DO I = 1,MAXETH
        ETH(I)=0.0D0
        ETHP(I)=0.0D0
        ETHP2(I)=0.0D0
      END DO
      IF( NCC.GE. 2 )THEN
        NT = 1
        DO I = 1,NCC
          DO J = I+1,NCC
            IT = 1
            ZI = SP_L(1,JPC(I))
            ZJ = SP_L(1,JPC(J))
            IF( ZI.NE.ZJ ) THEN
              IT = INT(ZI*ZJ)
              IF( ETH(IT).EQ.0.0D0 ) CALL ELECTS(ZI,ZJ,IT,CPIX,APHI)
            END IF
            CTCPH(NT) = TCC(NT)+ETH(IT)+CPIX*ETHP(IT)
            CTC(NT) = TCC(NT)+ETH(IT)
            CTCPR(NT) = ETHP(IT)
            CTCPPR(NT) = ETHP2(IT)
            NT = NT+1
          END DO
        END DO
      END IF
      IF( NA.GE.2 )THEN
        NT = 1
        DO I = 1,NA
          DO J = I+1,NA
            IT = 1
            ZI = SP_L(1,JPA(I))
            ZJ = SP_L(1,JPA(J))
            IF( ZI.NE.ZJ )THEN
              IT = INT(ZI*ZJ)
              IF( ETH(IT).EQ.0.0D0) CALL ELECTS(ZI,ZJ,IT,CPIX,APHI)
            END IF
            CTAPH(NT) = TAA(NT)+ETH(IT)+CPIX*ETHP(IT)
            CTA(NT) = TAA(NT)+ETH(IT)
            CTAPR(NT) = ETHP(IT)
            CTAPPR(NT) = ETHP2(IT)
            NT = NT+1
          END DO
        END DO
      END IF
!
!---  End of HOMIX ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE IMOBCF( NEQ )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Add immobile species concentrations into component species
!     concentrations.
!
!     C(NSL,N) component species concentration (kmol/m^3 node)
!     SP_C(NSP,N) species concentration (kmol/m^3 node)
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!     Last Modified by Mark White, PNNL, August 15, 2005.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE CONST
      USE FDVP
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/IMOBCF'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Loop over active nodes  ---
!
      DO 200 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 200
        NSL = NSOLU + NEQ
!
!---    Linked aqueous air   ---
!
        IF( ISPLK(4).EQ.NSL ) THEN
          COX = 1.D+3*XLA(1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/WTMA
          CX = 1.D+3*XLA(2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/WTMA
          C(NSL,N) = COX + (CX-COX)*DT/DT_RST
        ENDIF
!
!---    Linked aqueous CO2   ---
!
        IF( ISPLK(6).EQ.NSL ) THEN
          COX = 1.D+3*XLA(1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/WTMA
          CX = 1.D+3*XLA(2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/WTMA
          C(NSL,N) = COX + (CX-COX)*DT/DT_RST
        ENDIF
!
!---    Loop over conservation-component species  ---
!
        DO 100 M = 1,IEQ_C(1,NEQ)
          NSP = IEQ_C(M+1,NEQ)
!
!---      Solid and exchange species ---
!
          IF( NSP.GT.NSPL .AND. NSP.LE.(NSPL+NSPS+NSPE) ) THEN
            IF( ABS(SP_C(NSP,N)).LT.CMIN ) THEN
              SP_CX = 0.d0
            ELSE
              SP_CX = SP_C(NSP,N)
            ENDIF
            C(NSL,N) = C(NSL,N) + EQ_C(M,NEQ)*SP_CX
          ENDIF
  100   CONTINUE
!
!---    Update old time step conservation-component species ---
!
        CO(NSL,N) = C(NSL,N)
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of IMOBCF group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE IMOBKF( NEQ )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Mobile kinetic component fractions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!     Last Modified by Mark White, PNNL, August 15, 2005.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE CONST
      USE FDVP
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/IMOBKF'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Convert to global component indices  ---
!
      NEQX = NEQ + NEQC
!
!---  Loop over active nodes  ---
!
      DO 200 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 200
        NSL = NSOLU + NEQX
!
!---    Linked aqueous air   ---
!
        IF( ISPLK(4).EQ.NSL ) THEN
          COX = 1.D+3*XLA(1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/WTMA
          CX = 1.D+3*XLA(2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/WTMA
          C(NSL,N) = COX + (CX-COX)*DT/DT_RST
        ENDIF
!
!---    Linked aqueous CO2   ---
!
        IF( ISPLK(6).EQ.NSL ) THEN
          COX = 1.D+3*XLA(1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/WTMA
          CX = 1.D+3*XLA(2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/WTMA
          C(NSL,N) = COX + (CX-COX)*DT/DT_RST
        ENDIF
!
!---    Loop over kinetic-component species  ---
!
        DO 100 M = 1,IEQ_K(1,NEQ)
          NSP = IEQ_K(M+1,NEQ)
!
!---      Solid and exchange species ---
!
          IF( NSP.GT.NSPL .AND. NSP.LE.(NSPL+NSPS+NSPE) ) THEN
            IF( ABS(SP_C(NSP,N)).LT.CMIN ) THEN
              SP_CX = 0.D+0
            ELSE
              SP_CX = SP_C(NSP,N)
            ENDIF
            C(NSL,N) = C(NSL,N) + EQ_K(M,NEQ)*SP_CX
          ENDIF
  100   CONTINUE
!
!---    Update old time step kinetic-component species ---
!
        CO(NSL,N) = C(NSL,N)
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of IMOBKF group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE KECHEM( ACTVX,AJM,BJM,COX,SP_CX,DSP_CX,N,NEQ,INDX,IER )
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
!     Kinetic Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!     Last Modified by MD White, PNNL, 7 January 2005.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PORMED
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
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
      REAL*8 ACTVX(LSPL+1,LSPL)
      REAL*8 BJM(LSPR),AJM(LSPR,LSPR)
      REAL*8 SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 COX(LEQC+LEQK)
      REAL*8 EQKX(LREK),RRBX(LREK),RRCX(LREK)
      LOGICAL FCHK
      CHARACTER*64 GETSPNM
      EXTERNAL GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/KECHEM'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.100 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),100)) .AND. &
          (NSTEP-NRST).EQ.0 ) THEN
          AJM(NEQ,NEQ) = 1.D+0
          BJM(NEQ) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!---  Fixed species concentration or activity  ---
!
      DO 10 NSLKX = 1,NSPLK
        IF( ISPLK(14+NSLKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSLKX))
          IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
          IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
            AJM(NEQ,NEQ) = 1.D+0
            BJM(NEQ) = 0.D+0
            GOTO 1000
          ENDIF
        ENDIF
   10 CONTINUE
!
!---  Volumetric concentration to molality, mol/m^3 -> mol/kg aqu  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!
!---  Volumetric concentration to aqueous concentration,
!     mol/m^3 -> mol/m^3 aqu  ---
!
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
!
!---  Volumetric concentration to sorbed concentration,
!     mol/m^3 -> mol/kg sol  ---
!
      VTOSX = 1.D+0/((1.D+0-PORT(2,N))*RHOS(N))
!
!---  Total number of species  ---
!
      NEQX = NEQ - NEQE - NEQC
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Number of species and reactions in kinetic equation  ---
!
      NS = IEQ_K(1,NEQX)
      NR = IEQ_K(NS+2,NEQX)
!vlf In Yilin's code, not Mark's from equivalent date
      IF( (NSTEP-NRST).EQ.0 ) THEN
        NR = 0
        NSPRX = 0
        NSPPX = 0
        NSPKX = 0
      ENDIF
!
!---  Find aqueous silica
!
      DO M = NSPG+1,NSPG+NSPL
        IF (GETSPNM(M) == 'sio2(aq)') NSPSI = M
      END DO
!
!---  Loop over kinetic reactions in kinetic equation to
!     determine rate constants  ---
!
      DO 100 M = 1,NR
!
!---    Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
!
!---    Dissolution-precipitation kinetic reaction  ---
!
        IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR. &
          (IRCKT(IRCX).EQ.14) .OR. &
          (IRCKT(IRCX).EQ.16) .OR. &
          (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) .OR. & 
           IRCKT(IRCX).EQ.120) THEN
!
!---      Equilibrium constants as a function of temperature  ---
!
          IRCX = -IRCX
          CALL EQCN( EQKX(M),T(2,N),IRCX,N )
          IRCX = -IRCX
!
!---      Reaction rate constants as a function of temperature
!         mol/m^2 s  ---
!
          TKX = T(2,N)+TABS
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          TKRX = RC_K(NSPKX+3,N3,IRCX)+TABS
          RRCX(M) = RC_K(NSPKX+1,N1,IRCX)*EXP( -RC_K(NSPKX+2,N2,IRCX)* &
            ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )
        ENDIF
  100 CONTINUE
!
!---  Base residual  ---
!
      BJM(NEQ) = 0.D+0
      RSBX = 0.D+0
!
!---  Loop over kinetic reactions  ---
!
      DO 290 M = 1,NR
!
!---    Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
!
!---    TST kinetic reaction  ---
!
        IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR. &
          (IRCKT(IRCX).EQ.14) .OR. &
          (IRCKT(IRCX).EQ.16) .OR. &
          (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) .OR. &
           IRCKT(IRCX).EQ.120 ) THEN 
!
!---      Ion activity product mol/kg water, loop over species in
!         kinetic reaction  ---
!
          QX = 1.D+0
!
!---      Glass equilibrium dependent on aqueous silica only
!
          IF (IRCKT(IRCX).EQ.14) THEN
            CMX = SP_CX(NSPSI)*VTOMX
            ACX = ACTVX(1,NSPSI)
            QX = QX*(CMX*ACX)
          ELSE
!
!---      Loop over species in kinetic reaction  ---
!
          DO 120 L = 1,NSPKX
            NSPX = IRC_K(L+2,IRCX)
!
!---        Aqueous species,
!           concentration in molality, mol/kg H2O  ---
!
            IF( NSPX.LE.NSPL ) THEN
              CMX = SP_CX(NSPX)*VTOMX
              ACX = ACTVX(1,NSPX)
!
!---          Reactants  ---
!
              N1 = MAX(1,N*IRCKN(L))
              IF( L.LE.NSPRX ) THEN
                QX = QX*((CMX*ACX)**RC_K(L,N1,IRCX))
!
!---          Products  ---
!
              ELSE
                QX = QX/((CMX*ACX)**RC_K(L,N1,IRCX))
              ENDIF
!
!---      CFMX is scaling factor to translate between pore-scale
!---      and macro-scale simulations.  Default = 1
!
              IF (ISLC(58).EQ.1) THEN
                QX = CFMX(N)*QX
              ENDIF
!
!---        Solid species, skip  ---
!
            ELSEIF( NSPX.LE.NSPL+NSPS ) THEN
              GOTO 120
            ENDIF
  120     CONTINUE
          ENDIF
!
!---      Initial reactive surface area, initial mineral volume fraction,
!         current mineral volume fraction, minimum current mineral volume
!         fraction allows re-precipitation of dissolved primary minerals
!         NSP_M - mineral species number  ---
!
          NSPX = IRC_K(3+NSPKX,IRCX)
          NSP_M =  NSPX - NSPL
!
!---      Primary mineral  ---
!
          IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
            AOX = RS_S(1,NSP_M,N)*VOL(N)*RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
            VFMOX = RS_S(2,NSP_M,N)
            IF( ISP_MN(NSPX).EQ.1 ) THEN
              VFMX = 1.D-3*(SP_CMN(NSP_M,N)+SP_CX(NSPX)) &
                *SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ELSE
              VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
            IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND. &
                (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) .AND. &
                 IRCKT(IRCX).NE.120 ) &
            VFMX = MAX( VFMX,1.D-5 )
!
!---      Secondary mineral, initial reactive surface area
!         for seconary minerals is set to 0.25 m^2/dm^3  ---
!
          ELSE
            IF( RS_S(1,NSP_M,N).GT.EPSL ) THEN
              VFMOX = 1.D-5
              AOX = RS_S(1,NSP_M,N)*VOL(N)*VFMOX*SP_S(1,NSP_M)
            ELSE
              AOX = 0.25D+3*VOL(N)
              VFMOX = 1.D-2
            ENDIF
            VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND. &
                (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) .AND. &
                 IRCKT(IRCX).NE.120 ) &
            VFMX = MAX( VFMX,1.D-5 )
          ENDIF
!
!---      Reactive surface area  ---
!
          AX = AOX*(((POR_M(2,N)*VFMX)/ &
            (POR(2,N)*VFMOX))**(2.D+0/3.D+0))
          IF( ISLC(56).EQ.1 ) AX = AX * SL(2,N)
          IF( ISLC(56).EQ.2 ) AX = AOX
          IF( ISLC(58).EQ.1 ) AX = 1.0D+0
!
!---      Reaction rate, mol/s  ---
!
          RRBX(M) = -AX*RRCX(M)*(1.D+0-(QX/EQKX(M)))
          

!          if(islc(43) >= 2) then
          IF( IRCKT(IRCX).EQ.16 ) THEN
            AX = AOX
            RRBX(M) = -RRCX(M)*(1.D+0-(QX/EQKX(M)))*vol(n)
          endif
          IF( RRBX(M).NE.RRBX(M) ) THEN
            IER = -1
            RETURN
          ENDIF
!
!---      pH dependence  ---
!
          IF( ((IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) &
          .OR. (IRCKT(IRCX).EQ.14)) &
            .AND. ISPLK(1).NE.0 ) THEN
            NSP_PHX = MOD(ISPLK(1),100)
            PHX = -LOG10(1.D-3*SP_CX(NSP_PHX)*VTOLX)
            IF( IRCKT(IRCX).GE.8 .AND. IRCKT(IRCX).LE.9 ) THEN
              RRBX(M) = RRBX(M)*MAX( 0.D+0, &
                (7.9201D-1 - 1.3479D-1*PHX + 5.2D-3*(PHX**2)))
            ELSE
              N9 = MAX(1,N*IRCKN(NSPKX+9))
              RRBX(M) = RRBX(M)*(1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))
            ENDIF
          ENDIF
!
!---      iex pH dependence  ---
!
          IF( IRCKT(IRCX).EQ.120 .AND. ISPLK(1).NE.0 ) THEN
            NSP_PHX = MOD(ISPLK(1),100)
            PHX = -LOG10(1.D-3*SP_CX(NSP_PHX)*VTOLX)
!---        the first 8 spots already claimed for other parameters
            N9 = MAX(1,N*IRCKN(NSPKX+9))
            TRRBX = (1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))
!
!---      iex simulation time dependence  ---
!
            N10 = MAX(1,N*IRCKN(NSPKX+10))
            TRRBX = TRRBX*TM**(-RC_K(NSPKX+10,N10,IRCX))
            RRBX(M) = RRBX(M)*TRRBX
          ENDIF
!
!---      Reaction rate, mol/m^3 aqu s  ---
!
          RRBX(M) = RRBX(M)*VTOLX/VOL(N)
          SP_AREA(NSP_M,N) = AX
          NSP_MIN = IEQ_K(2,NEQX)-NSPL
          IF (M.EQ.1) THEN
            SP_RATE(NSP_MIN,N) = RRBX(M)/VTOLX*VOL(N) * EQ_K(M+1,NEQX)
          ELSE
            SP_RATE(NSP_MIN,N) = SP_RATE(NSP_MIN,N)  &
                               + RRBX(M)/VTOLX*VOL(N) * EQ_K(M+1,NEQX)
          ENDIF
!
!---      Direction limited  ---
!
          IF( IRCKT(IRCX).EQ.6 .OR. IRCKT(IRCX).EQ.8 &
            .OR. IRCKT(IRCX).EQ.11 ) THEN
            RRBX(M) = MAX( RRBX(M),0.D+0 )
          ELSEIF( IRCKT(IRCX).EQ.7 .OR. IRCKT(IRCX).EQ.9 &
            .OR. IRCKT(IRCX).EQ.12 .OR. IRCKT(IRCX).EQ.14 &
            .OR. IRCKT(IRCX).EQ.120 ) THEN
            RRBX(M) = MIN( RRBX(M),0.D+0 )
          ENDIF
!
!---    Forward-backward kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.1 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          FRRX = RC_K(NSPKX+1,N1,IRCX)
          BRRX = RC_K(NSPKX+2,N2,IRCX)
!
!---      Loop over reactants  ---
!
          DO 140 L = 1,NSPRX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
            N1 = MAX(1,N*IRCKN(L))
            FRRX = FRRX*(CMX**RC_K(L,N1,IRCX))
  140     CONTINUE
!
!---      Loop over products  ---
!
          DO 160 L = 1,NSPPX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2+NSPRX,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
            N1 = MAX(1,N*IRCKN(L+NSPRX))
            BRRX = BRRX*(CMX**RC_K(L+NSPRX,N1,IRCX))
  160     CONTINUE
          RRBX(M) = FRRX - BRRX
!
!---    Valocchi-Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.2 ) THEN
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Partial rate of donor degredation  ---
!
          N6 = MAX(1,N*IRCKN(6))
          RRBX(M) = RC_K(6,N6,IRCX)*CMX
!
!---      Concentration of donor in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/kg water  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---    Valocchi-Sorption kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.3 ) THEN
!
!---      Concentration of sorbed species in mol/kg soil  ---
!
          NSPX = IRC_K(4,IRCX)
!         CMX = SP_CX(NSPX)*VTOSX*1.D-3
          CMX = SP_CX(NSPX)*VTOSX
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = CMX/RC_K(4,N4,IRCX)
!
!---      Concentration of aqueous species in mol/m^3 water  ---
!
          NSPX = IRC_K(3,IRCX)
!         CMX = SP_CX(NSPX)*VTOMX
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of sorption in mol/m^3 aqu s  ---
!
!         RRBX(M) = RC_K(3,IRCX)*(CMX-RRBX(M))*1.D+3
          N3 = MAX(1,N*IRCKN(3))
          RRBX(M) = RC_K(3,N3,IRCX)*(CMX-RRBX(M))
!
!---    Langmuir-Sorption kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.13 ) THEN
!
!---      Concentration of sorbed species in mol/kg soil  ---
!
          NSPX = IRC_K(4,IRCX)
          CSX = SP_CX(NSPX)*VTOSX
!
!---      Concentration of aqueous species in mol/m^3 water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of sorption in mol/m^3 aqu s  ---
!
          N3 = MAX(1,N*IRCKN(3))
          N4 = MAX(1,N*IRCKN(4))
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = RC_K(3,N3,IRCX)*CMX*(RC_K(5,N5,IRCX)-CSX)- &
            RC_K(4,N4,IRCX)*CSX
!
!---    Valocchi-Biomass kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.4 ) THEN
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Partial rate of donor degredation  ---
!
          N6 = MAX(1,N*IRCKN(6))
          RRBX(M) = RC_K(6,N6,IRCX)*CMX
!
!---      Concentration of donor in mol/m^3 aqu  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/m^3 aqu  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of biomass production in mol/m^3 aqu s  ---
!
          N7 = MAX(1,N*IRCKN(7))
          N8 = MAX(1,N*IRCKN(8))
          RRBX(M) = RC_K(7,N7,IRCX)*RRBX(M) - RC_K(8,N8,IRCX)*CMX
!
!---    Emulsion- or oil-sorption kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.15 ) THEN
!
!---      Neighborhood-particle dimensionless number  ---
!
          GAMMAX = (MAX( 1.D+0-PORD(2,N),0.D+0 ))**(THIRD)
          ASX = 2.D+0*(1.D+0-(GAMMAX**5))/(2.D+0 - 3.D+0*GAMMAX &
            + 3.D+0*(GAMMAX**5) - 2.D+0*(GAMMAX**6))
!
!---      Hamaker constant (J)  ---
!
          CHX = 1.D-20
!
!---      Bolztmann constant (kg m^2/s^2 K)  ---
!
          CBX = 1.38D-23
!
!---      Average aqueous velocity (m/s)  ---
!
          ULAVX = 0.D+0
          NC = 0
          NPX = NSX(N)
          IF( ABS(UL(1,NPX)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + UL(1,NPX)
          ENDIF
          NQX = NSX(N)+1
          IF( ABS(UL(1,NQX)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + UL(1,NQX)
          ENDIF
          NPY = NSY(N)
          IF( ABS(VL(1,NPY)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + VL(1,NPY)
          ENDIF
          NQY = NSY(N)+IFLD
          IF( ABS(VL(1,NQY)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + VL(1,NQY)
          ENDIF
          NPZ = NSZ(N)
          IF( ABS(WL(1,NPZ)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + WL(1,NPZ)
          ENDIF
          NQZ = NSZ(N)+IJFLD
          IF( ABS(WL(1,NQZ)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + WL(1,NQZ)
          ENDIF
          ULAVX = ULAVX/REAL(NC)
!
!---      London - van der Waals dimensionless number  ---
!
          N6 = MAX(1,N*IRCKN(6))
          DNLOX = 4.D+0*CHX/ &
            (9.D+0*GPI*VISL(2,N)*(RC_K(6,N6,IRCX)**2)*ULAVX)
!
!---      Inception dimensionless number  ---
!
          N3 = MAX(1,N*IRCKN(3))
          DNRX = RC_K(6,N6,IRCX)/RC_K(3,N3,IRCX)
!
!---      Sedimentation dimensionless number  ---
!
          N7 = MAX(1,N*IRCKN(7))
          DNGX = GRAV*(RHOL(2,N)-RC_K(7,N7,IRCX))*(RC_K(6,N6,IRCX)**2) &
            /(1.8D+1*VISL(2,N)*ULAVX)
!
!---      Diffusion dimensionless number  ---
!
          DNPEX = 3.D+0*GPI*VISL(2,N)*ULAVX*RC_K(3,N3,IRCX) &
            *RC_K(6,N6,IRCX)/(CBX*(T(2,N)+TABS))
!
!---      Single collector efficiency  ---
!
          ETAX = ASX*((DNLOX**(1.D+0/8.D+0))*(DNRX**(1.5D+1/8.D+0)) + &
            3.38D-3*(DNGX**1.2D+0)*(DNRX**(-4.D-1)) + &
            4.D+0*(DNPEX**(-2.D+0/3.D+0)))
!
!---      Concentration of immobile oil (kg oil/kg soil)  ---
!
          NSPX = IRC_K(3,IRCX)
          WTMX = SP_S(2,(NSPX-NSPL))
          CIMX = 1.D-3*SP_CX(NSPX)*VTOSX*WTMX
!
!---      Concentration of mobile oil (kg oil/m^3 aqu)  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = 1.D-3*SP_CX(NSPX)*VTOLX*WTMX
!
!---      Rate of immobile-oil production in kg oil/kg soil s  ---
!
          N4 = MAX(1,N*IRCKN(4))
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = 3.D+0*ULAVX*ETAX*MAX(1.D+0-PORD(2,N),0.D+0)* &
            RC_K(4,N4,IRCX)*MAX(RC_K(5,N5,IRCX)-CIMX,0.D+0)*CMX/ &
            (2.D+0*RC_K(3,N3,IRCX)*RC_K(5,N5,IRCX))
!
!---      Rate of immobile-oil production in mol/m^3 aqu s  ---
!
          RRBX(M) = 1.D+3*RRBX(M)*VTOLX/(VTOSX*WTMX)
!
!---    Multirate  ---
!
        ELSEIF( IRCKT(IRCX).EQ.20 ) THEN
!
!---      Neutral reaction rate, mol/m^2 s  ---
!
          TKX = T(2,N)+TABS
          N1 = MAX(1,N*IRCKN(1))
          N2 = MAX(1,N*IRCKN(2))
          N3 = MAX(1,N*IRCKN(3))
          TKRX = RC_K(3,N3,IRCX)+TABS
          RRCX(M) = RC_K(1,N1,IRCX)*EXP( -RC_K(2,N2,IRCX)* &
            ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )
!
!---      Loop over mechanisms  ---
!
          DO 190 NKRMX = 1,IRC_K(2,IRCX)
            IX = 3+((NKRMX-1)*6)
!
!---        Ion activity product mol/kg water, loop over species  ---
!
            QX = 1.D+0
            DO 180 L = 1,IRC_K(IX,IRCX)
              IX = 3+((NKRMX-1)*6)+L
              NX = MAX(1,N*IRCKN(IX))
              NSPX = IRC_K(IX,IRCX)
!
!---          Aqueous species,
!             concentration in molality, mol/kg H2O  ---
!
              IF( NSPX.LE.NSPL ) THEN
                CMX = SP_CX(NSPX)*VTOMX
                ACX = ACTVX(1,NSPX)
                IX = 6+((NKRMX-1)*8)+L
                QX = QX*((CMX*ACX)**RC_K(IX,NX,IRCX))
              ENDIF
  180       CONTINUE
            IX = 4+((NKRMX-1)*8)
            NX = MAX(1,N*IRCKN(IX))
            N1 = MAX(1,N*IRCKN(IX+1))
            N2 = MAX(1,N*IRCKN(IX+2))
            TKRX = RC_K(IX+2,N2,IRCX)+TABS
            RRCX(M) = RRCX(M) + RC_K(IX,NX,IRCX) &
              *EXP( -RC_K(IX+1,N1,IRCX)* &
            ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )*QX
  190     CONTINUE
!
!---      Initial reactive surface area, initial mineral volume fraction,
!         current mineral volume fraction, minimum current mineral volume
!         fraction allows re-precipitation of dissolved primary minerals
!         NSP_M - mineral species number  ---
!
          NSPX = IRC_K(1,IRCX)
          NSP_M =  NSPX - NSPL
!
!---      Primary mineral  ---
!
          IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
            AOX = RS_S(1,NSP_M,N)*VOL(N)*RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
            VFMOX = RS_S(2,NSP_M,N)
            IF( ISP_MN(NSPX).EQ.1 ) THEN
              VFMX = 1.D-3*(SP_CMN(NSP_M,N)+SP_CX(NSPX)) &
                *SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ELSE
              VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
            IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND. &
                (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) .AND. &
                 IRCKT(IRCX).NE.120 ) &
            VFMX = MAX( VFMX,1.D-5 )
!
!---      Secondary mineral, initial reactive surface area
!         for seconary minerals is set to 0.25 m^2/dm^3  ---
!
          ELSE
            IF( RS_S(1,NSP_M,N).GT.EPSL ) THEN
              VFMOX = 1.D-5
              AOX = RS_S(1,NSP_M,N)*VOL(N)*VFMOX*SP_S(1,NSP_M)
            ELSE
              AOX = 0.25D+3*VOL(N)
              VFMOX = 1.D-2
            ENDIF
            VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND. &
                (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) .AND. &
                 IRCKT(IRCX).NE.120 ) &
            VFMX = MAX( VFMX,1.D-5 )
          ENDIF
!
!---      Reactive surface area  ---
!
          AX = AOX*(((POR_M(2,N)*VFMX)/ &
           (POR(2,N)*VFMOX))**(2.D+0/3.D+0))
          IF( ISLC(56).EQ.1 ) AX = AX * SL(2,N)
          IF( ISLC(56).EQ.2 ) AX = AOX
!
!---      Reaction rate, mol/s  ---
!
          RRBX(M) = -AX*RRCX(M)
!          if(islc(43) >= 2) then
          IF( IRCKT(IRCX).EQ.16 ) THEN
            AX = AOX
            rrbx(m) = -rrcx(m)*vol(n)
          endif
!
!---      Reaction rate, mol/m^3 aqu s  ---
!
          RRBX(M) = RRBX(M)*VTOLX/VOL(N)

          SP_AREA(NSP_M,N) = AX
          NSP_MIN = IEQ_K(2,NEQX)-NSPL
          IF (M.EQ.1) THEN
            SP_RATE(NSP_MIN,N) = RRBX(M)/VTOLX*VOL(N) * EQ_K(M+1,NEQX)
          ELSE
            SP_RATE(NSP_MIN,N) = SP_RATE(NSP_MIN,N)  &
                               + RRBX(M)/VTOLX*VOL(N) * EQ_K(M+1,NEQX)
          ENDIF
!
!---    Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.22 ) THEN
          JCX = 0
          RRBX(M) = 1.D+0
!
!---      Loop over the number of reactants, less one  ---
!
          DO 200 NSP = 1,NSPRX-1
!
!---        Concentration of reactant in mol/m^3 aqu  ---
!
            NSPX = IRC_K(NSP+3,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
!
!---        Partial rate of donor degredation  ---
!
            JCX = JCX+1
            N1 = MAX(1,N*IRCKN(JCX))
            RRBX(M) = RRBX(M)*(CMX/(RC_K(JCX,N1,IRCX)+CMX))
  200     CONTINUE
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of reactant degredation in mol/m^3 aqu s  ---
!
          JCX = JCX+1
          N1 = MAX(1,N*IRCKN(JCX))
          RRBX(M) = -RRBX(M)*RC_K(JCX,N1,IRCX)*CMX
!
!---    Biomass kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.24 ) THEN
          JCX = 0
          RRBX(M) = 0.D+0
!
!---      Loop over the number of reactants, less one  ---
!
          DO 210 NSP = 1,NSPRX-1
!
!---        Concentration of reactant in mol/m^3 water  ---
!
            NSPX = IRC_K(NSP+3,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
!
!---        Partial rate of reactant degredation  ---
!
            JCX = JCX+1
            N1 = MAX(1,N*IRCKN(JCX))
            RRBXX = (CMX/(RC_K(JCX,N1,IRCX)+CMX))
!
!---        Concentration of biomass in mol/m^3 aqu  ---
!
            NSPX = IRC_K(3,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
!
!---        Partial rate of biomass degredation  ---
!
            JCX = JCX+1
            N1 = MAX(1,N*IRCKN(JCX))
            RRBXX = RRBXX*RC_K(JCX,N1,IRCX)*CMX
            RRBX(M) = RRBX(M) + RRBXX
  210     CONTINUE
!
!---      Microbial specific yield coefficient  ---
!
          JCX = JCX+1
          N1 = MAX(1,N*IRCKN(JCX))
          RRBX(M) = RRBX(M)*RC_K(JCX,N1,IRCX)
!
!---      Concentration of reactant in mol/kg water  ---
!
          NSPX = IRC_K(NSP+3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of microbial degredation  ---
!
          JCX = JCX+1
          N1 = MAX(1,N*IRCKN(JCX))
          RRBX(M) = RRBX(M) - RC_K(JCX,N1,IRCX)*CMX
!
!---    Dual Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.35 ) THEN
!
!---      Partial rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = RC_K(5,N5,IRCX)
!
!---      Concentration of donor in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N3 = MAX(1,N*IRCKN(3))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(3,N3,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/kg water  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---    Single Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.36 ) THEN
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = RC_K(4,N4,IRCX)
!
!---      Concentration of donor in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N3 = MAX(1,N*IRCKN(3))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(3,N3,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/kg water  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          RRBX(M) = RRBX(M)*CMX
!
!---    Liu's multi-rate kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.41 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          N4 = MAX(1,N*IRCKN(NSPKX+4))
          N5 = MAX(1,N*IRCKN(NSPKX+5))
          RMX = RC_K(NSPKX+1,N1,IRCX)
          SDENX = RC_K(NSPKX+2,N2,IRCX)*VTOLX/VTOSX
          PFRCX = RC_K(NSPKX+3,N3,IRCX)
          XLGK1 = RC_K(NSPKX+4,N4,IRCX)
          XLGK2 = RC_K(NSPKX+5,N5,IRCX)
          FRRX = 1.D+0
          BRRX = 1.D+0
!
!---      Loop over reactants  ---
!
          DO 220 L = 1,NSPRX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2,IRCX)
            CMX = SP_CX(NSPX)*VTOMX
            IF( NSPX.LE.NSPL ) THEN
              ACX = ACTVX(1,NSPX)
            ELSE
              ACX = 1.D+0
            ENDIF
            N1 = MAX(1,N*IRCKN(L))
            FRRX = FRRX*((CMX*ACX)**RC_K(L,N1,IRCX))
  220     CONTINUE
!
!---      Loop over products  ---
!
          DO 230 L = 1,NSPPX-1
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2+NSPRX,IRCX)
            CMX = SP_CX(NSPX)*VTOMX
            IF( NSPX.LE.NSPL ) THEN
              ACX = ACTVX(1,NSPX)
            ELSE
              ACX = 1.D+0
            ENDIF
            N1 = MAX(1,N*IRCKN(L+NSPRX))
            BRRX = BRRX*((CMX*ACX)**RC_K(L+NSPRX,N1,IRCX))
  230     CONTINUE
          L = NSPPX
          NSPX = IRC_K(L+2+NSPRX,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX*(1.D+1**XLGK1)
          BRRX = BRRX*(1.D+1**XLGK2)
          RRBX(M) = RMX*(SDENX*PFRCX*FRRX/(1.D+0+FRRX+BRRX)-CMX)
!
!---    Liu's dual domain kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.42 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          RMX = RC_K(NSPKX+1,N1,IRCX)
!
!---      Loop over reactants  ---
!
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          PFRCX = RC_K(NSPKX+3,N3,IRCX)
          FRRX = 0.D+0
!
!---      Global species index  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX + (CMX**VTOLX)
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX - (CMX**VTOLX)
          RRBX(M) = RMX*FRRX
        ENDIF
!
!---    Residual contribution  ---
!
!        BJM(NEQ) = BJM(NEQ) - (RRBX(M)*EQ_K(NS+M,NEQX))
        RSBX = RSBX + RRBX(M)*EQ_K(NS+M,NEQX)
  290 CONTINUE
!
!---  Loop over kinetic species in kinetic equation  ---
!
      CX = 0.D+0
      CTOX = 0.D+0
      DO 310 M = 1,NS
        NSPX = IEQ_K(M+1,NEQX)
        CX = CX + SP_CX(NSPX)*EQ_K(M,NEQX)
        IF( ISP_MN(NSPX).EQ.1 ) THEN
          NSP_M =  NSPX - NSPL
          CTOX = CTOX + (SP_CO(NSPX,N)+SP_CMN(NSP_M,N))*EQ_K(M,NEQX)
        ELSE
          CTOX = CTOX + SP_CO(NSPX,N)*EQ_K(M,NEQX)
        ENDIF
!vlf for fixed conc?
        DO 300 NSPKX = 1,NSPLK
          IF(ISPLK(14+NSPKX).LT.-1000) THEN
            NSPXX=ABS(ISPLK(14+NSPKX))-1000
            IF(NSPX.EQ.NSPXX) THEN
             IF( NSPX.LE.NSPL ) THEN
               ACX = ACTVX(1,NSPX)
             ELSE
               ACX = 1.D0
             ENDIF
             CTOX = CTOX-SP_CO(NSPX,N)*EQ_K(M,NEQX) &
                  + SP_CO(NSPX,N)/ACX*EQ_K(M,NEQX)
            ENDIF
          ENDIF
  300 CONTINUE
!vlf
  310 CONTINUE
      CMX = CX*VTOLX
      CMOX = COX(NEQ-NEQE)*VTOLX
      CMTOX = CTOX*VTOLX
!
!---  Check for complete consumption  ---
!
      IF( RSBX.LT.0.D+0 ) THEN
        RSBX = MAX( RSBX,(-CMTOX*DTI) )
!if(cmtox < 1.d-10) then
!print *,'rxbx-',rsbx,cmtox,n
!stop
!endif
      ENDIF
      BJM(NEQ) = (CMX-CMOX)*DTI - RSBX
!
!---  Return residual vector  ---
!
      IF( INDX.EQ.1 ) GOTO 1000
!
!---  Incremented residuals  ---
!
      DO 900 NSP = 1,NSPR
!
!---  Check whether specie is a kinetic equation specie,
!     which affects the residual via the kinetic equation
!     or a kinetic reaction specie,
!     which affects the residual via the kinetic equation,
!     via the kinetic equation reactions  ---
!
        FCHK = .FALSE.
!
!---    Fixed species concentration  ---
!
        DO 400 NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
            NSPXX = ABS(ISPLK(14+NSPKX))
            IF( NSP.EQ.NSPXX ) THEN
              FCHK = .FALSE.
              GOTO 450
            ENDIF
          ENDIF
  400   CONTINUE
!
!---    Loop over species in kinetic equation  ---
!
        DO 410 M = 1,NS
          NSPX = IEQ_K(M+1,NEQX)
!
!---      Specie is a kinetic reaction specie  ---
!
          IF( NSP.EQ.NSPX ) THEN
            FCHK = .TRUE.
            GOTO 450
          ENDIF
  410   CONTINUE
!
!---    Loop over kinetic reaction reactants  ---
!
        DO 420 L = 1,NSPRX
!
!---      Global species index  ---
!
          NSPX = IRC_K(L+2,IRCX)
!
!---      Specie is a kinetic reaction specie  ---
!
          IF( NSP.EQ.NSPX ) THEN
            FCHK = .TRUE.
            GOTO 450
          ENDIF
  420   CONTINUE
!
!---    Skip for initial pH  ---
!
        IF( ISPLK(1).GT.100 ) THEN
          IF( NSP.EQ.MOD(ISPLK(1),100) .AND. &
            (NSTEP-NRST).EQ.0 ) FCHK = .FALSE.
        ENDIF
!
!---    Loop over kinetic reaction products  ---
!
        DO 430 L = 1,NSPPX
!
!---      Global species index  ---
!
          NSPX = IRC_K(L+2+NSPRX,IRCX)
!
!---      Specie is a kinetic reaction specie  ---
!
          IF( NSP.EQ.NSPX ) THEN
            FCHK = .TRUE.
            GOTO 450
          ENDIF
  430   CONTINUE
  450   CONTINUE
!
!---    Specie is a kinetic equation specie,
!       or a kinetic equation reaction specie  ---
!
        IF( FCHK ) THEN
          RSX = 0.D+0
!
!---      Loop over kinetic reactions  ---
!
          DO 600 M = 1,NR
!
!---        Reaction index, number of reactants, number of products  ---
!
            IRCX = IEQ_K(NS+2+M,NEQX)
            NSPRX = IRC_K(1,IRCX)
            NSPPX = IRC_K(2,IRCX)
            NSPKX = NSPRX+NSPPX
!
!---        Dissolution-precipitation kinetic reaction  ---
!
            IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR. &
              (IRCKT(IRCX).EQ.14) .OR. &
              (IRCKT(IRCX).EQ.16) .OR. &
              (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) .OR. &
               IRCKT(IRCX).EQ.120 ) THEN
!
!---          Ion activity product mol/kg water, loop over species in
!             kinetic reaction  ---
!
              QX = 1.D+0
!
!---          Glass equilibrium dependent on aqueous silica only
!
              IF (IRCKT(IRCX).EQ.14) THEN
!
!---            Incremented species  ---
!
                IF( NSPSI.EQ.NSP ) THEN
                  SP_CXX = SP_CX(NSPSI)+DSP_CX(NSPSI)
!
!---            Unincremented species  ---
!
                ELSE
                  SP_CXX = SP_CX(NSPSI)
                ENDIF
                  CMX = SP_CXX*VTOMX
                  ACX = ACTVX(1,NSPSI)
                  QX = QX*(CMX*ACX)
              ELSE
!
!---          Loop over species in kinetic reaction  ---
!
              DO 510 L = 1,NSPKX
                NSPX = IRC_K(L+2,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  SP_CXX = SP_CX(NSPX)+DSP_CX(NSPX)
!
!---            Unincremented species  ---
!
                ELSE
                  SP_CXX = SP_CX(NSPX)
                ENDIF
!
!---            Aqueous species,
!               concentration in molality, mol/kg H2O  ---
!
                IF( NSPX.LE.NSPL ) THEN
                  CMX = SP_CXX*VTOMX
                  ACX = ACTVX(1,NSPX)
!
!---              Reactants  ---
!
                  N1 = MAX(1,N*IRCKN(L))
                  IF( L.LE.NSPRX ) THEN
                    QX = QX*((CMX*ACX)**RC_K(L,N1,IRCX))
!
!---              Products  ---
!
                  ELSE
                    QX = QX/((CMX*ACX)**RC_K(L,N1,IRCX))
                  ENDIF
!
!---              CFMX is scaling factor to translate between pore-scale
!---              and macro-scale simulations.  Default = 1
!
                  IF (ISLC(58).EQ.1) THEN
                    QX = CFMX(N)*QX
                  ENDIF
!
!---            Solid species, skip  ---
!
                ELSEIF( NSPX.LE.NSPL+NSPS ) THEN
                  GOTO 510
                ENDIF
  510         CONTINUE
              ENDIF
!
!---          Reactive surface area
!             NSP_M - mineral species number  ---
!
              NSPX = IRC_K(3+NSPKX,IRCX)
              NSP_M = NSPX - NSPL
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                SP_CXX = SP_CX(NSPX)+DSP_CX(NSPX)
!
!---          Unincremented species  ---
!
              ELSE
                SP_CXX = SP_CX(NSPX)
              ENDIF
!
!---          Primary mineral  ---
!
              IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
                AOX = RS_S(1,NSP_M,N)*VOL(N)* &
                  RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
                VFMOX = RS_S(2,NSP_M,N)
                IF( ISP_MN(NSPX).EQ.1 ) THEN
                  VFMX = 1.D-3*(SP_CMN(NSP_M,N)+SP_CXX) &
                    *SP_S(2,NSP_M)/SP_S(1,NSP_M)
                ELSE
                  VFMX = 1.D-3*SP_CXX*SP_S(2,NSP_M)/SP_S(1,NSP_M)
                ENDIF
                IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND. &
                    (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) .AND. &
                 IRCKT(IRCX).NE.120 ) &
                VFMX = MAX( VFMX,1.D-5 )
!
!---          Secondary mineral, initial reactive surface area
!             for seconary minerals is set to 0.25 m^2/dm^3  ---
!
              ELSE
                IF( RS_S(1,NSP_M,N).GT.EPSL ) THEN
                  VFMOX = 1.D-5
                  AOX = RS_S(1,NSP_M,N)*VOL(N)*VFMOX*SP_S(1,NSP_M)
                ELSE
                  AOX = 0.25D+3*VOL(N)
                  VFMOX = 1.D-2
                ENDIF
                VFMX = 1.D-3*SP_CXX*SP_S(2,NSP_M)/SP_S(1,NSP_M)
                IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND. &
                    (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) .AND. &
                 IRCKT(IRCX).NE.120 ) &
                VFMX = MAX( VFMX,1.D-5 )
              ENDIF
!
!---          Reactive surface area  ---
!
              AX = AOX*(((POR_M(2,N)*VFMX)/ &
                (POR(2,N)*VFMOX))**(2.D+0/3.D+0))
              IF( ISLC(56).EQ.1 ) AX = AX * SL(2,N)
              IF( ISLC(56).EQ.2 ) AX = AOX
              IF( ISLC(58).EQ.1 ) AX = 1.0D+0
!
!---          Reaction rate, mol/s  ---
!
              RRX = -AX*RRCX(M)*(1.D+0-(QX/EQKX(M)))
!              if(islc(43) >= 2) then
              IF( IRCKT(IRCX).EQ.16 ) THEN
                AX = AOX
                RRX = -RRCX(M)*(1.D+0-(QX/EQKX(M)))*vol(n)

              endif
!
!---          pH dependence  ---
!
              IF( ((IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) &
               .OR. (IRCKT(IRCX).EQ.14)) &
               .AND. ISPLK(1).NE.0 ) THEN
                NSP_PHX = MOD(ISPLK(1),100)
!
!---            Incremented species  ---
!
                IF( NSP_PHX.EQ.NSP ) THEN
                  SP_CXX = SP_CX(NSP_PHX)+DSP_CX(NSP_PHX)
!
!---            Unincremented species  ---
!
                ELSE
                  SP_CXX = SP_CX(NSP_PHX)
                ENDIF
                PHX = -LOG10(1.D-3*SP_CXX*VTOLX)
                IF( IRCKT(IRCX).GE.8 .AND. IRCKT(IRCX).LE.9 ) THEN
                  RRX = RRX*MAX( 0.D+0, &
                    (7.9201D-1 - 1.3479D-1*PHX + 5.2D-3*(PHX**2)))
                ELSE
                  N9 = MAX(1,N*IRCKN(NSPKX+9))
                  RRX = RRX*(1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))
                ENDIF
              ENDIF
!
!---          iex pH and simulation time dependence  ---
!
              IF( IRCKT(IRCX).EQ. 120 .AND. ISPLK(1).NE.0 ) THEN
                NSP_PHX = MOD(ISPLK(1),100)
!
!---            Incremented species  ---
!
                IF( NSP_PHX.EQ.NSP ) THEN
                  SP_CXX = SP_CX(NSP_PHX)+DSP_CX(NSP_PHX)
!
!---            Unincremented species  ---
!
                ELSE
                  SP_CXX = SP_CX(NSP_PHX)
                ENDIF
                PHX = -LOG10(1.D-3*SP_CXX*VTOLX)
                N9 = MAX(1,N*IRCKN(NSPKX+9))
                TRRX = (1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))

                N10 = MAX(1,N*IRCKN(NSPKX+10))
                TRRX = TRRX * TM**(-RC_K(NSPKX+10,N10,IRCX))
                RRX = RRX*TRRX
  
              ENDIF
!
!---          Reaction rate, mol/m^3 aqu s  ---
!
              RRX = RRX*VTOLX/VOL(N)
!
!---          Direction limited  ---
!
              IF( IRCKT(IRCX).EQ.6 .OR. IRCKT(IRCX).EQ.8 &
                .OR. IRCKT(IRCX).EQ.11 ) THEN
                RRX = MAX( RRX,0.D+0 )
              ELSEIF( IRCKT(IRCX).EQ.7 .OR. IRCKT(IRCX).EQ.9 &
                .OR. IRCKT(IRCX).EQ.12 .OR. IRCKT(IRCX).EQ.14 &
                .OR. IRCKT(IRCX).EQ.120 ) THEN
                RRX = MIN( RRX,0.D+0 )
              ENDIF
!
!---        Forward-backward kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.1 ) THEN
              N1 = MAX(1,N*IRCKN(NSPKX+1))
              N2 = MAX(1,N*IRCKN(NSPKX+2))
              FRRX = RC_K(NSPKX+1,N1,IRCX)
              BRRX = RC_K(NSPKX+2,N2,IRCX)
!
!---          Loop over reactants  ---
!
              DO 520 L = 1,NSPRX
!
!---            Global species index  ---
!
                NSPX = IRC_K(L+2,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOLX
                ENDIF
                N1 = MAX(1,N*IRCKN(L))
                FRRX = FRRX*(CMX**RC_K(L,N1,IRCX))
  520         CONTINUE
!
!---          Loop over products  ---
!
              DO 530 L = 1,NSPPX
!
!---            Global species index  ---
!
                NSPX = IRC_K(L+2+NSPRX,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOLX
                ENDIF
                N1 = MAX(1,N*IRCKN(L+NSPRX))
                BRRX = BRRX*(CMX**RC_K(L+NSPRX,N1,IRCX))
  530         CONTINUE
              RRX = FRRX - BRRX
!
!---        Valocchi-Monod kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.2 ) THEN
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(5,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N6 = MAX(1,N*IRCKN(6))
              RRX = RC_K(6,N6,IRCX)*CMX
!
!---          Concentration of donor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N4 = MAX(1,N*IRCKN(4))
              RRX = RRX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---          Concentration of acceptor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Rate of donor degredation, mol/m^3 aqu s  ---
!
              N5 = MAX(1,N*IRCKN(5))
              RRX = RRX*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---        Valocchi-Sorption kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.3 ) THEN
!
!---          Concentration of sorbed species in mol/gm soil  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
!               CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOSX*1.D-3
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOSX
!
!---          Unincremented species  ---
!
              ELSE
!               CMX = SP_CX(NSPX)*VTOSX*1.D-3
                CMX = SP_CX(NSPX)*VTOSX
              ENDIF
              N4 = MAX(1,N*IRCKN(4))
              RRX = CMX/RC_K(4,N4,IRCX)
!
!---          Concentration of aqueous species in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
!               CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
!               CMX = SP_CX(NSPX)*VTOMX
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Rate of sorption in mol/m^3 aqu s  ---
!
!             RRX = RC_K(3,IRCX)*(CMX-RRX)*1.D+3
              N3 = MAX(1,N*IRCKN(3))
              RRX = RC_K(3,N3,IRCX)*(CMX-RRX)
!
!---        Langmuir-Sorption kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.13 ) THEN
!
!---          Concentration of sorbed species in mol/gm soil  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CSX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOSX
!
!---          Unincremented species  ---
!
              ELSE
                CSX = SP_CX(NSPX)*VTOSX
              ENDIF
!
!---          Concentration of aqueous species in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Rate of sorption in mol/m^3 aqu s  ---
!
              N3 = MAX(1,N*IRCKN(3))
              N4 = MAX(1,N*IRCKN(4))
              N5 = MAX(1,N*IRCKN(5))
              RRX = RC_K(3,N3,IRCX)*CMX*(RC_K(5,N5,IRCX)-CSX) &
                -RC_K(4,N4,IRCX)*CSX
!
!---        Valocchi-Biomass kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.4 ) THEN
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(5,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N6 = MAX(1,N*IRCKN(6))
              RRX = RC_K(6,N6,IRCX)*CMX
!
!---          Concentration of donor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N4 = MAX(1,N*IRCKN(4))
              RRX = RRX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---          Concentration of acceptor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Rate of donor degredation, mol/m^3 aqu s  ---
!
              N5 = MAX(1,N*IRCKN(5))
              RRX = RRX*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(5,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Rate of biomass production in mol/m^3 aqu s  ---
!
              N7 = MAX(1,N*IRCKN(7))
              N8 = MAX(1,N*IRCKN(8))
              RRX = RC_K(7,N7,IRCX)*RRX - RC_K(8,N8,IRCX)*CMX
!
!---        Emulsion- or oil-sorption kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.15 ) THEN
!
!---          Concentration of immobile oil (kg oil/kg soil)  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CIMX = 1.D-3*(SP_CX(NSPX)+DSP_CX(NSPX))*VTOSX*WTMX
!
!---          Unincremented species  ---
!
              ELSE
                CIMX = 1.D-3*SP_CX(NSPX)*VTOSX*WTMX
              ENDIF
!
!---          Concentration of mobile oil (kg oil/m^3 aqu)  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = 1.D-3*(SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX*WTMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = 1.D-3*SP_CX(NSPX)*VTOLX*WTMX
              ENDIF
!
!---          Rate of immobile-oil production in kg oil/kg soil s  ---
!
              N3 = MAX(1,N*IRCKN(3))
              N4 = MAX(1,N*IRCKN(4))
              N5 = MAX(1,N*IRCKN(5))
              RRX = 3.D+0*ULAVX*ETAX*MAX(1.D+0-PORD(2,N),0.D+0)* &
               RC_K(4,N4,IRCX)*MAX(RC_K(5,N5,IRCX)-CIMX,0.D+0)*CMX/ &
               (2.D+0*RC_K(3,N3,IRCX)*RC_K(5,N5,IRCX))
!
!---          Rate of immobile-oil production in mol/m^3 aqu s  ---
!
              RRX = 1.D+3*RRX*VTOLX/(VTOSX*WTMX)
!
!---        Monod kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.22 ) THEN
              JCX = 0
              RRX = 1.D+0
!
!---          Loop over the number of reactants, less one  ---
!
              DO 540 L = 1,NSPRX-1
!
!---            Concentration of reactant in mol/m^3 water  ---
!
                NSPX = IRC_K(L+3,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOLX
                ENDIF
!
!---            Partial rate of reactant degredation  ---
!
                JCX = JCX+1
!vlf Yilin's code has this calculated using FRRX
!               RRX = RRX*(CMX/(RC_K(JCX,IRCX)+CMX))
                N1 = MAX(1,N*IRCKN(JCX))
                RRX = RRX*(CMX**RC_K(JCX,N1,IRCX))*VTOLX/VTOSX*1.D-3
!vlf
  540         CONTINUE
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Partial rate of biomass degredation  ---
!
              JCX = JCX+1
              N1 = MAX(1,N*IRCKN(JCX))
              RRX = -RRX*RC_K(JCX,N1,IRCX)*CMX
!
!---        Biomass kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.24 ) THEN
              JCX = 0
              RRX = 0.D+0
!
!---          Loop over the number of reactants, less one  ---
!
              DO 560 L = 1,NSPRX-1
!
!---            Concentration of reactant in mol/m^3 water  ---
!
                NSPX = IRC_K(L+3,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOLX
                ENDIF
!
!---            Partial rate of reactant degredation  ---
!
                JCX = JCX+1
                N1 = MAX(1,N*IRCKN(JCX))
                RRBXX = (CMX/(RC_K(JCX,N1,IRCX)+CMX))
!
!---            Concentration of biomass in mol/m^3 aqu  ---
!
                NSPX = IRC_K(3,IRCX)
                CMX = SP_CX(NSPX)*VTOLX
!
!---            Partial rate of biomass degredation  ---
!
                JCX = JCX+1
                N1 = MAX(1,N*IRCKN(JCX))
                RRBXX = RRBXX*RC_K(JCX,N1,IRCX)*CMX
                RRX = RRX + RRBXX
  560         CONTINUE
!
!---          Microbial specific yield coefficient  ---
!
              JCX = JCX+1
              N1 = MAX(1,N*IRCKN(JCX))
              RRX = RRX*RC_K(JCX,N1,IRCX)
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Partial rate of microbial degredation  ---
!
              JCX = JCX+1
              N1 = MAX(1,N*IRCKN(JCX))
              RRX = RRX - RC_K(JCX,N1,IRCX)*CMX
!
!---        Dual-Monod kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.35 ) THEN
!
!---          Partial rate of donor degredation  ---
!
              N5 = MAX(1,N*IRCKN(5))
              RRX = RC_K(5,N5,IRCX)
!
!---          Concentration of donor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N3 = MAX(1,N*IRCKN(3))
              RRX = RRX*(CMX/(RC_K(3,N3,IRCX)+CMX))
!
!---          Concentration of acceptor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Rate of donor degredation, mol/m^3 aqu s  ---
!
              N4 = MAX(1,N*IRCKN(4))
              RRX = RRX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---        Single-Monod kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.36 ) THEN
!
!---          Partial rate of donor degredation  ---
!
              N4 = MAX(1,N*IRCKN(4))
              RRX = RC_K(4,N4,IRCX)
!
!---          Concentration of donor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N3 = MAX(1,N*IRCKN(3))
              RRX = RRX*(CMX/(RC_K(3,N3,IRCX)+CMX))
!
!---          Concentration of acceptor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Rate of donor degredation, mol/m^3 aqu s  ---
!
              RRX = RRX*CMX
!
!---        Liu's multi-rate kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.41 ) THEN
              N1 = MAX(1,N*IRCKN(NSPKX+1))
              N2 = MAX(1,N*IRCKN(NSPKX+2))
              N3 = MAX(1,N*IRCKN(NSPKX+3))
              N4 = MAX(1,N*IRCKN(NSPKX+4))
              N5 = MAX(1,N*IRCKN(NSPKX+5))
              RMX = RC_K(NSPKX+1,N1,IRCX)
              SDENX = RC_K(NSPKX+2,N2,IRCX)*VTOLX/VTOSX
              PFRCX = RC_K(NSPKX+3,N3,IRCX)
              XLGK1 = RC_K(NSPKX+4,N4,IRCX)
              XLGK2 = RC_K(NSPKX+5,N5,IRCX)
              FRRX = 1.D+0
              BRRX = 1.D+0
!
!---          Loop over reactants  ---
!
              DO 570 L = 1,NSPRX
!
!---            Global species index  ---
!
                NSPX = IRC_K(L+2,IRCX)
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
                  IF(NSPX.LE.NSPL) THEN
                    ACX = ACTVX(NSP+1,NSPX)
                  ELSE
                    ACX = 1.D+0
                  ENDIF
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOMX
                  IF( NSPX.LE.NSPL ) THEN
                    ACX = ACTVX(1,NSPX)
                  ELSE
                    ACX = 1.D+0
                  ENDIF
                ENDIF
                N1 = MAX(1,N*IRCKN(L))
                FRRX = FRRX*((CMX*ACX)**RC_K(L,N1,IRCX))
  570         CONTINUE
!
!---          Loop over products  ---
!
              DO 580 L = 1,NSPPX-1
!
!---            Global species index  ---
!
                NSPX = IRC_K(L+2+NSPRX,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX) + DSP_CX(NSPX))*VTOMX
                  IF( NSPX.LE.NSPL ) THEN
                    ACX = ACTVX(NSP+1,NSPX)
                  ELSE
                    ACX = 1.D+0
                  ENDIF
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOMX
                  IF( NSPX.LE.NSPL ) THEN
                    ACX = ACTVX(1,NSPX)
                  ELSE
                    ACX = 1.D+0
                  ENDIF
               ENDIF
                N1 = MAX(1,N*IRCKN(L+NSPRX))
                BRRX = BRRX*((CMX*ACX)**RC_K(L+NSPRX,N1,IRCX))
  580         CONTINUE
              L = NSPPX
              NSPX = IRC_K(L+2+NSPRX,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX) + DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
              FRRX = FRRX*(1.D+1**XLGK1)
              BRRX = BRRX*(1.D+1**XLGK2)
              RRX = RMX*(SDENX*PFRCX*FRRX/(1.D+0+FRRX+BRRX)-CMX)
!
!---        Liu's dual domain kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.42 ) THEN
              N1 = MAX(1,N*IRCKN(NSPKX+1))
              N3 = MAX(1,N*IRCKN(NSPKX+3))
              RMX = RC_K(NSPKX+1,N1,IRCX)
              PFRCX = RC_K(NSPKX+3,N3,IRCX)
              FRRX = 0.D+0
!
!---        Global species index  ---
!
              NSPX = IRC_K(3,IRCX)
              IF( NSPX.EQ.NSP ) THEN
                SP_CXX = SP_CX(NSPX) + DSP_CX(NSPX)
              ELSE
                SP_CXX = SP_CX(NSPX)
              ENDIF               
              CMX = SP_CXX*VTOLX
              FRRX = FRRX + (CMX**VTOLX)
              NSPX = IRC_K(4,IRCX)
              IF( NSPX.EQ.NSP ) THEN
                SP_CXX = SP_CX(NSPX) + DSP_CX(NSPX)
              ELSE
                SP_CXX = SP_CX(NSPX)
              ENDIF               
              CMX = SP_CXX*VTOLX
              FRRX = FRRX - (CMX**VTOLX)
              RRX = RMX*FRRX
            ENDIF
!
!---        Liu's dual domain kinetic reaction  ---
!
            IF( IRCKT(IRCX).EQ.42 ) THEN
              EQ_KX = EQ_K(NS+M,NEQX)
              IF( IMMB(NEQC+NEQX).EQ.1 ) THEN
                N2 = MAX(1,N*IRCKN(NSPKX+2))
                EQ_KX = EQ_K(NS+M,NEQX)*RC_K(NSPKX+2,N2,IRCX)
              ENDIF
!
!---        Residual contribution  ---
!
            ELSE
              RSX = RSX + RRX*EQ_K(NS+M,NEQX)
            ENDIF
!            RSX = RSX - ((RRX-RRBX(M))*EQ_K(NS+M,NEQX))
  600     CONTINUE
!
!---      Loop over kinetic species in kinetic equation  ---
!
          DO 700 M = 1,NS
            NSPX = IEQ_K(M+1,NEQX)
!
!---        Incremented species  ---
!
            IF( NSPX.EQ.NSP ) THEN
              AJM(NEQ,IEQ_S(NSP)) = EQ_K(M,NEQX)*VTOLX*DTI
!
!---          Fixed species activity  ---
!
              DO 610 NSPKX = 1,NSPLK
                IF( ISPLK(14+NSPKX).LT.-1000 ) THEN
                  NSPXX = ABS(ISPLK(14+NSPKX))-1000
                  IF( NSP.EQ.NSPXX ) THEN
                    IF( NSP.LE.NSPL ) THEN
                      ACX = ACTVX(1,NSP)
                    ELSE
                      ACX = 1.D+0
                    ENDIF
                    AJM(NEQ,IEQ_S(NSP))=EQ_K(M,NEQX)/ACX*VTOLX*DTI
                  ENDIF
                ENDIF
  610         CONTINUE
              GOTO 710
            ENDIF
  700     CONTINUE
  710     CONTINUE
!
!---      Check for complete consumption  ---
!
          IF( RSX.LT.0.D+0 ) THEN
            RSX = MAX( RSX,(-CMTOX*DTI) )
          ENDIF
!          AJM(NEQ,IEQ_S(NSP)) = AJM(NEQ,IEQ_S(NSP)) + RSX/DSP_CX(NSP)
          AJM(NEQ,IEQ_S(NSP)) = AJM(NEQ,IEQ_S(NSP)) - &
            (RSX-RSBX)/DSP_CX(NSP)
        ENDIF
  900 CONTINUE
!
!---  Return residual vector and Jacobian matrix  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NEQ) = -BJM(NEQ)

 1000 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of KECHEM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE KECHEM_R( ACTVX,AJM,BJM,COX,SP_CX,DSP_CX,N,NEQ,INDX )
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
!     Kinetic Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!     Last Modified by MD White, PNNL, 7 January 2005.
!     $Id: eckechem.F,v 1.14 2006/09/19 14:41:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PORMED
      USE GRID
      USE FDVP
      USE CONST
      USE FLUXP
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
      REAL*8 ACTVX(LSPL+1,LSPL)
      REAL*8 BJM(LSPR),AJM(LSPR,LSPR)
      REAL*8 SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 COX(LEQC+LEQK)
      REAL*8 EQKX(LREK),RRBX(LREK),RRCX(LREK),RSBX(LREK)
      LOGICAL FCHK
      SAVE NR
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/KECHEM_R'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
      NROW = NEQ-NEQE
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.100 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),100)) .AND. &
          (NSTEP-NRST).EQ.0 ) THEN
          AJM(NROW,NROW) = 1.D+0
          BJM(NROW) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!--- fixed species concentration or activity
!
      DO NSLKX = 1,NSPLK
        IF( ISPLK(14+NSLKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSLKX))
          IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
          IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
            AJM(NROW,NROW) = 1.D+0
            BJM(NROW) = 0.D+0
            GOTO 1000
          ENDIF
        ENDIF
      ENDDO
!
!---  Volumetric concentration to molality, mol/m^3 -> mol/kg aqu  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!
!---  Volumetric concentration to aqueous concentration,
!     mol/m^3 -> mol/m^3 aqu  ---
!
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
!
!---  Volumetric concentration to sorbed concentration,
!     mol/m^3 -> mol/kg sol  ---
!
      VTOSX = 1.D+0/((1.D+0-PORT(2,N))*RHOS(N))
!
!---  Total number of species  ---
!
      NEQX = NEQ - NEQE - NEQC
      NSPR = NSPG + NSPL + NSPN + NSPS
!
!---  Number of species and reactions in kinetic equation  ---
!
      NS = IEQ_K(1,NEQX)
      NR = IEQ_K(NS+2,NEQX)
      IF( (NSTEP-NRST).EQ.0 ) THEN
        NR = 0
        NSPRX = 0
        NSPPX = 0
        NSPKX = 0
      ENDIF
!
!---  Loop over kinetic reactions in kinetic equation to
!     determine rate constants  ---
!
      DO 100 M = 1,NR
!
!---    Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
!
!---    Dissolution-precipitation kinetic reaction  ---
!
        IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR. &
          (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) .OR. &
           IRCKT(IRCX).EQ.120) THEN
!
!---      Equilibrium constants as a function of temperature  ---
!
          IRCX = -IRCX
          CALL EQCN( EQKX(M),T(2,N),IRCX,N )
          IRCX = -IRCX
!
!---      Reaction rate constants as a function of temperature
!         mol/m^2 s  ---
!
          TKX = T(2,N)+TABS
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          TKRX = RC_K(NSPKX+3,N3,IRCX)+TABS
          RRCX(M) = RC_K(NSPKX+1,N1,IRCX)*EXP( -RC_K(NSPKX+2,N2,IRCX)* &
            ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )
        ENDIF
  100 CONTINUE
!
!---  Base residual  ---
!
      BJM(NROW) = 0.D+0
      RSBXX = 0.D0
!
!---  Loop over kinetic reactions  ---
!
      DO 200 M = 1,NR
        RSBX(M) = 0.D+0
!
!---    Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
        CALL RATER( NEQX,M,NS,N,IRCX,NSPKX,NSPRX,NSPPX,SP_CX,ACTVX, &
            VTOMX,VTOLX,VTOSX,EQKX(M),RRCX(M),RSBX(M) )
        RSBXX = RSBXX + RSBX(M)
  200 CONTINUE
!
!---  Loop over kinetic species in kinetic equation  ---
!
      CX = 0.D+0
      CTOX = 0.D+0
      DO 300 M = 1,NS
        NSPX = IEQ_K(M+1,NEQX)
        CX = CX + SP_CX(NSPX)*EQ_K(M,NEQX)
        IF( ISP_MN(NSPX).EQ.1 ) THEN
          NSP_M =  NSPX - NSPL
          CTOX = CTOX + (SP_CO(NSPX,N)+SP_CMN(NSP_M,N))*EQ_K(M,NEQX)
        ELSE
          CTOX = CTOX + SP_CO(NSPX,N)*EQ_K(M,NEQX)
        ENDIF
!
        DO NSPKX = 1,NSPLK
          IF(ISPLK(14+NSPKX).LT.-1000) THEN
            NSPXX=ABS(ISPLK(14+NSPKX))-1000
            IF(NSPX.EQ.NSPXX) THEN
             IF( NSPX.LE.NSPL ) THEN
               ACX = ACTVX(1,NSPX)
             ELSE
               ACX = 1.D0
             ENDIF             
             CTOX = CTOX-SP_CO(NSPX,N)*EQ_K(M,NEQX) &
                  + SP_CO(NSPX,N)/ACX*EQ_K(M,NEQX)
            ENDIF
          ENDIF
        ENDDO
  300 CONTINUE
      CMX = CX*VTOLX
      CMOX = COX(NEQ-NEQE)*VTOLX
      CMTOX = CTOX*VTOLX
!
!---  Check for complete consumption  ---
!
!      IF( RSBXX.LT.0.D+0 ) THEN
!        RSBXX = MAX( RSBXX,(-CMTOX*DTI) )
!      ENDIF
      BJM(NROW) = (CMX-CMOX)*DTI - RSBXX
!
!---  Return residual vector  ---
!
      IF( INDX.EQ.1 ) GOTO 1000
!
!---      Loop over kinetic reactions  ---
!
      DO 600 M = 1,NR
!
!---        Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
        DO L = 1,NSPKX
          RSX = 0.D0
          NSPX = IRC_K(L+2,IRCX)
          CMMX = SP_CX(NSPX)
          IF( NSPX.LE.NSPL ) ACX = ACTVX(1,NSPX)
!          DSP_CX(NSPX) = MAX(1.D-10,DSP_CX(NSPX))
          SP_CX(NSPX) = CMMX+DSP_CX(NSPX)
          IF( NSPX.LE.NSPL ) ACTVX(1,NSPX) = ACTVX(NSPX+1,NSPX)
          CALL RATER( NEQX,M,NS,N,IRCX,NSPKX,NSPRX,NSPPX,SP_CX,ACTVX, &
             VTOMX,VTOLX,VTOSX,EQKX(M),RRCX(M),RSX )
          SP_CX(NSPX) = CMMX
          IF( NSPX.LE.NSPL ) ACTVX(1,NSPX) = ACX
          NEQXX=IEQ_S(NSPX)
          IF( NEQXX.GT.NEQE) THEN
            NCOL = NEQXX-NEQE
            AJM(NROW,NCOL) = AJM(NROW,NCOL) - (RSX-RSBX(M))/DSP_CX(NSPX)
          ELSE
!
!---   Equilibrium mass action
!
            NSE = IEQ_E(1,NEQXX)
!
!---  Loop over equilibrium species  ---
!
            DO MSX = 2,NSE
              NCM_SP = IEQ_E(MSX+1,NEQXX)
              NCOL = IEQ_S(NCM_SP)-NEQE
              AJM(NROW,NCOL) = AJM(NROW,NCOL)-(RSX-RSBX(M))/ &
                DSP_CX(NSPX)*EQ_E(MSX-1,NEQXX)*SP_CX(NSPX)/SP_CX(NCM_SP)
            ENDDO
          ENDIF
        ENDDO
  600 CONTINUE
!
!---      Loop over kinetic species in kinetic equation  ---
!
      DO 700 M = 1,NS
        NSPX = IEQ_K(M+1,NEQX)
!
!---        Incremented species  ---
!
        NEQXX = IEQ_S(NSPX)
        IF( NEQXX.GT.NEQE ) THEN
         NCOL = IEQ_S(NSPX)-NEQE
         AJM(NROW,NCOL) = AJM(NROW,NCOL)+EQ_K(M,NEQX)*VTOLX*DTI
!
!---  fixed species activity
!
         DO NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.-1000 ) THEN
            NSPXX = ABS(ISPLK(14+NSPKX))-1000
            IF( NSPX.EQ.NSPXX ) THEN
              IF( NSPX.LE.NSPL ) THEN
                ACX = ACTVX(1,NSPX)
              ELSE
                ACX = 1.D0
              ENDIF
              AJM(NROW,NCOL)=AJM(NROW,NCOL)+EQ_K(M,NEQX)/ACX*VTOLX*DTI
            ENDIF
          ENDIF
         ENDDO
        ELSE
!
!---   Equilibrium mass action
!
          NSE = IEQ_E(1,NEQXX)
!
!---  Loop over equilibrium species  ---
!
          DO MSX = 2,NSE
            NCM_SP = IEQ_E(MSX+1,NEQXX)
            NCOL = IEQ_S(NCM_SP)-NEQE
            AJM(NROW,NCOL) = AJM(NROW,NCOL)+EQ_K(M,NEQX)*VTOLX*DTI* &
              EQ_E(MSX-1,NEQXX)*SP_CX(NSPX)/SP_CX(NCM_SP)
          ENDDO
        ENDIF
  700 CONTINUE
!
!---  Return residual vector and Jacobian matrix  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NROW) = -BJM(NROW)
 1000 CONTINUE
!
!---  End of KECHEM_R group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END


!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MOBCF( NEQ )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Mobile conservation component fractions.
!
!     YSPLX aqueous fraction of component species NEQ at node N
!     YSPGX gas fraction of component species NEQ at node N
!     YSPNX NAPL fraction of component species NEQ at node N
!     C(NSL,N) component species concentration (kmol/m^3 node)
!     SP_C(NSP,N) species concentration (kmol/m^3 node)
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!     Last Modified by Mark White, PNNL, August 15, 2005.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE CONST
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/MOBCF'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Loop over active nodes  ---
!
      DO 200 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 200
        YSPLX = 0.D+0
        YSPGX = 0.D+0
        YSPNX = 0.D+0
        YSPLZ = 0.D+0
        YSPGZ = 0.D+0
        YSPNZ = 0.D+0
        NSL = NSOLU + NEQ
        IF( ICT(NSL,N).NE.0 .AND. (NSTEP-NRST).EQ.0 ) GOTO 200
        C(NSL,N) = 0.D+0
!
!---    Loop over conservation-component species  ---
!
        DO 100 M = 1,IEQ_C(1,NEQ)
          NSP = IEQ_C(M+1,NEQ)
          IF( ABS(SP_C(NSP,N)).LT.CMIN ) THEN
            SP_CX = 0.D+0
          ELSE
            SP_CX = SP_C(NSP,N)
          ENDIF
!
!---      Aqueous species ---
!
          IF( NSP.LE.NSPL .AND. IEQW.GT.0 ) THEN
            YSPLX = YSPLX + EQ_C(M,NEQ)*SP_CX
            YSPLZ = YSPLZ + EQ_C(M,NEQ)
            C(NSL,N) = C(NSL,N) + EQ_C(M,NEQ)*SP_CX
!
!---      Gas species ---
!
          ELSEIF( NSP.GT.(NSPL+NSPS+NSPE) .AND. &
            NSP.LE.(NSPL+NSPS+NSPE+NSPG) .AND. IEQA.GT.0 ) THEN
            YSPGX = YSPGX + EQ_C(M,NEQ)*SP_CX
            YSPGZ = YSPGZ + EQ_C(M,NEQ)
            C(NSL,N) = C(NSL,N) + EQ_C(M,NEQ)*SP_CX
!
!---      NAPL species ---
!
          ELSEIF( NSP.GT.(NSPL+NSPS+NSPE+NSPG) .AND. &
            NSP.LE.(NSPL+NSPS+NSPE+NSPG+NSPN) .AND. IEQO.GT.0 ) THEN
            YSPNX = YSPNX + EQ_C(M,NEQ)*SP_CX
            YSPNZ = YSPNZ + EQ_C(M,NEQ)
            C(NSL,N) = C(NSL,N) + EQ_C(M,NEQ)*SP_CX
          ENDIF
  100   CONTINUE
!
!---    Update old time step conservation-component species ---
!
        CO(NSL,N) = C(NSL,N)
        YSPZ = YSPLZ+YSPGZ+YSPNZ
!
!---    Aqueous species ---
!
        IF( IEQW.GT.0 ) THEN
!
!---      Zero mobile species  ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPLX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(NSL,N))/EPSL.LT.EPSL ) THEN
            YSPLX = YSPLZ/YSPZ
!            YSPLX = 0.D+0
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPLX = YSPLX/C(NSL,N)
          ENDIF
!
!---      pH link ---
!
          IF( ISPLK(1).EQ.NSL ) THEN
            YL(NSL,N) = 1.D+0
!
!---      Air link ---
!
          ELSEIF( ISPLK(4).EQ.NSL ) THEN
            YL(NSL,N) = 1.D+0
!
!---      CO2 link ---
!
          ELSEIF( ISPLK(6).EQ.NSL ) THEN
            YL(NSL,N) = 1.D+0
          ELSE
            YL(NSL,N) = YSPLX
          ENDIF
        ENDIF
!
!---    Gas species ---
!
        IF( IEQA.GT.0 ) THEN
!
!---      Zero mobile species  ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPGX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(NSL,N))/EPSL.LT.EPSL ) THEN
!            YSPGX = 0.D+0
            YSPGX = YSPGZ/(YSPZ)
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPGX = YSPGX/C(NSL,N)
          ENDIF
          YG(NSL,N) = YSPGX
        ENDIF
!
!---    NAPL species ---
!
        IF( IEQO.GT.0 ) THEN
!
!---      Zero mobile species  ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPNX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(NSL,N))/EPSL.LT.EPSL ) THEN
!            YSPNX = 0.D+0
            YSPNX = YSPNZ/(YSPZ)
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPNX = YSPNX/C(NSL,N)
          ENDIF
          YN(NSL,N) = YSPNX
        ENDIF
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of MOBCF group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MOBKF( NEQ )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Mobile kinetic component fractions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!     Last Modified by Mark White, PNNL, August 15, 2005.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE CONST
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/MOBKF'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Convert to global component indices  ---
!
      NEQX = NEQ + NEQC
!
!---  Loop over active nodes  ---
!
      DO 200 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 200
        YSPLX = 0.D+0
        YSPGX = 0.D+0
        YSPNX = 0.D+0
        YSPLZ = 0.D+0
        YSPGZ = 0.D+0
        YSPNZ = 0.D+0
        NSL = NSOLU + NEQX
        IF( ICT(NSL,N).NE.0 .AND. (NSTEP-NRST).EQ.0 ) GOTO 200
        C(NSL,N) = 0.D+0
!
!---    Loop over kinetic-component species  ---
!
        DO 100 M = 1,IEQ_K(1,NEQ)
          NSP = IEQ_K(M+1,NEQ)
          IF( ABS(SP_C(NSP,N)).LT.CMIN ) THEN
            SP_CX = 0.D+0
          ELSE
            SP_CX = SP_C(NSP,N)
          ENDIF
!
!---      Aqueous species ---
!
          IF( NSP.LE.NSPL ) THEN
            YSPLX = YSPLX + EQ_K(M,NEQ)*SP_CX
            YSPLZ = YSPLZ + EQ_K(M,NEQ)
            C(NSL,N) = C(NSL,N) + EQ_K(M,NEQ)*SP_CX
!
!---      Gas species ---
!
          ELSEIF( NSP.GT.(NSPL+NSPS+NSPE) .AND. &
            NSP.LE.(NSPL+NSPS+NSPE+NSPG) ) THEN
            YSPGX = YSPGX + EQ_K(M,NEQ)*SP_CX
            YSPGZ = YSPGZ + EQ_K(M,NEQ)
            C(NSL,N) = C(NSL,N) + EQ_K(M,NEQ)*SP_CX
!
!---      NAPL species ---
!
          ELSEIF( NSP.GT.(NSPL+NSPS+NSPE+NSPG) .AND. &
            NSP.LE.(NSPL+NSPS+NSPE+NSPG+NSPN) ) THEN
            YSPNX = YSPNX + EQ_K(M,NEQ)*SP_CX
            YSPNZ = YSPNZ + EQ_K(M,NEQ)
            C(NSL,N) = C(NSL,N) + EQ_K(M,NEQ)*SP_CX
          ENDIF
  100   CONTINUE
!
!---    Update old time step kinetic-component species ---
!
        CO(NSL,N) = C(NSL,N)
        YSPZ = YSPLZ+YSPGZ+YSPNZ
!
!---    Aqueous species ---
!
        IF( IEQW.GT.0 ) THEN
!
!---      Zero mobile species ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPLX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(NSL,N))/EPSL.LT.EPSL ) THEN
!            YSPLX = 0.D+0
            YSPLX = YSPLZ/YSPZ
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPLX = YSPLX/C(NSL,N)
          ENDIF
          YL(NSL,N) = YSPLX
        ENDIF
!
!---    Gas species ---
!
        IF( IEQA.GT.0 ) THEN
!
!---      Zero mobile species ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPGX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(NSL,N))/EPSL.LT.EPSL ) THEN
!            YSPGX = 0.D+0
            YSPGX = YSPGZ/YSPZ
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPGX = YSPGX/C(NSL,N)
          ENDIF
          YG(NSL,N) = YSPGX
        ENDIF
!
!---    NAPL species ---
!
        IF( IEQO.GT.0 ) THEN
!
!---      Zero mobile species ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPNX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(NSL,N))/EPSL.LT.EPSL ) THEN
!            YSPNX = 0.D+0
            YSPNX = YSPNZ/YSPZ
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPNX = YSPNX/C(NSL,N)
          ENDIF
          YN(NSL,N) = YSPNX
        ENDIF
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of MOBKF group  ---
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE NMNSP
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Normalize mineral species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 2 May 2006.
!     Last Modified by Mark White, PNNL, 2 May 2006.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE GRID
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/NMNSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Loop over active nodes  ---
!
      DO 100 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Loop over solid species  ---
!
        DO 10 NSPX = 1,NSPS
          NSP = NSPL + NSPX
!
!---      Mineral species  ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            SP_CMN(NSPX,N) = SP_C(NSP,N)
            SP_C(NSP,N) = 0.D+0
          ENDIF
   10   CONTINUE
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of NMNSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PITZER( ACTVX,SP_CX,DSP_CX,SLX,PORDX,RHOLX,TX,XLWX )
!
!----------------------Description-------------------------------------!
!
! This subroutine computes activity and osmotic coefficients for
! electrolyte solutions: Pitzer's equations for mixed electrolyte solutions
!----------------------Authors-----------------------------------------!
!
!     Written by A. Felmy, from GMIN
!     Last Modified by VL Freedman, PNNL, 13 March 2007.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE CONST
      USE PTZRCOEF
      USE PTZR
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

      REAL*8 ACTVX(LSPL+1,LSPL),SP_CX(LSPL),DSP_CX(LSPL)
      REAL*8 CPIX(LSPL+1),CAPZ(LSPL+1),LNG(LSPL+1,LSPL),TFUNC
      REAL*8 FF(LSPL+1),G4M(LSPL+1)
      EXTERNAL TFUNC
      REAL*8 LNA, G(4),GP(4),GPP(4),ALPHA(4),ATTMP(8)
      SAVE TSMI

      DATA TSMI / -1.D+3 /
      DATA ALPHA / 2.0D+00,1.40D+00,1.20D+01,5.0D+01 /
      DATA BB  /1.2D+00 /

!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/PITZER'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!--- Initialize Variables
!
      DO I = 1,4
        G(I) = 0.D+0
        GP(I) = 0.D+0
        GPP(I) = 0.D+0
      ENDDO
      
      DO I = 1,8
        ATTMP(I) = 0.D+0
      ENDDO

      DO M = 1,NSPL+1
        CAPZ(M) = 0.D+0
        CPIX(M) = 0.D+0
          FF(M) = 0.D+0
          G4M(M) = 0.D+0
        DO NSP = 1,NSPL
          LNG(M,NSP) = 0.D+0
        ENDDO
      ENDDO
      
      IKH2O = 0

!
!---  Recalculate Pitzer parameters for non-isothermal solution.
!
      TK = TX + 273.15d0

      APHI = 0.336901532d0-6.3210043D-04*TK+9.14252359d0/ &
             TK-1.35143986D-02*DLOG(TK)+2.26089488D-03/(TK-263.d0)+ &
             1.92118597D-06*TK*TK+4.52586464D+01/(680.d0-TK)

      IF( ABS(TX-TSMI).GT.EPSL ) THEN
!  
!---  Re-calculate binary parameters
!
        DO I = 1,NCC
          DO J = 1,NA
            DO k = 1,8
              ATTMP(K) = ATB0(I,J,K)
            END DO
            B0(I,J) = TFUNC(ATTMP,TK)
          END DO
        END DO
        
        DO I = 1,NCC
          DO J = 1,NA
            DO K = 1,8
              ATTMP(K) = ATB1(I,J,K)
            END DO
            B1(I,J) = TFUNC(ATTMP,TK)
          END DO
        END DO
 
        DO I = 1,NCC
          DO J = 1,NA
            DO K = 1,8
             ATTMP(K) = ATB2(I,J,K)
            END DO
            B2(I,J) = TFUNC(ATTMP,TK)
          END DO
        END DO
  
        DO I = 1,NCC
          DO J = 1,NA
            DO K = 1,8
              ATTMP(K) = ATCMX(I,J,K)
            END DO
            CMXX(I,J) = TFUNC(ATTMP,TK)
            CMXX(I,J)=CMXX(I,J)/(2.*SQRT(ABS(SP_L(1,JPC(I))* &
                      SP_L(1,JPA(J)))))
          END DO
        END DO
!  
!---  Recalculate theta and psi.
!  
        II = 0
        IF( NCC.GE.2 )THEN
          DO I = 1,NCC
            DO J = I+1,NCC
              II = II+1
              DO K = 1,6
                ATTMP(K) = ATTC(II,K)
              END DO
              TCC(II)=TFUNC(ATTMP,TK)
              DO KK = 1,NA
                DO K = 1,6
                  ATTMP(K) = ATPC(II,KK,K)
                END DO
                PSIC(II,KK)=TFUNC(ATTMP,TK)
              END DO
            END DO
          END DO
        END IF
 
        II = 0
        IF( NA.GE.2 )THEN
          DO I = 1,NA
            DO J = I+1,NA
              II = II+1
              DO K = 1,6
                ATTMP(K) = ATTA(II,K)
              END DO
              TAA(II)=TFUNC(ATTMP,TK)
              DO KK = 1,NCC
                DO K = 1,6
                  ATTMP(K) = ATPA(II,KK,K)
                END DO
                PSIA(II,KK)=TFUNC(ATTMP,TK)
              END DO
            END DO
          END DO
        END IF
!  
!--- Recalculate ternary parameters.
!  
        DO I = 1,NNN
          DO J = 1,NNN
            DO K = 1,6
              ATTMP(K) = ATNLAM(I,J,K)
            END DO
            ELAMB(I,J) = TFUNC(ATTMP,TK)
          END DO
          
          DO J = 1,NCC
            DO K = 1,6
              ATTMP(K) = ATCLAM(I,J,K)
            END DO
            CLAMB(I,J) = TFUNC(ATTMP,TK)
          END DO
          
          DO J = 1,NA
            DO K = 1,6
              ATTMP(K) = ATALAM(I,J,K)
            END DO
            ALAMB(I,J) = TFUNC(ATTMP,TK)
          END DO
        END DO
        
        II = 0
        DO I = 1,NNN
          DO J = 1,NCC
            II = II+1
            DO KK = 1,NA
              DO K = 1,6
               ATTMP(K) = ATHLAM(II,KK,K)
              END DO
              HOLAMB(II,KK) = TFUNC(ATTMP,TK)
            END DO
          END DO
        END DO
      END IF
      TSMI = TX
!
!--- End temperature-dependent calculation of Pitzer parameters
!
!
!---  Ionic strength of the aqueous solution  ---
!
      DO 40 M = 1,NSPL+1
        CPIX(M) = 0.D+0
        SUM_M = 0.D+0
        DO 30 NSP = 1,NSPL
          IF(IDD(NSP).GE.300000) GO TO 30
          IF(SPNML(NSP).EQ.'h2o') THEN
            IKH2O = NSP
            GO TO 30
          ENDIF
          IF( NSP.EQ.(M-1) ) THEN
            CLX = SP_CX(NSP) + DSP_CX(NSP)
          ELSE
            CLX = SP_CX(NSP)
          ENDIF
!       
!---    Molarity in mol solute/m^3 aqueous
!       or mol solute/l aqueous  ---
!       
          CMX = CLX/(SLX*PORDX)
!       
!---    Molality in mol solute/kg water  ---
!       
          CMX = CMX/(RHOLX*XLWX)
          SUM_M = SUM_M + CMX
          CPIX(M) = CPIX(M) + CMX*(SP_L(1,NSP)**2)
          CAPZ(M) = CAPZ(M) + CMX*DABS(SP_L(1,NSP))
 30     CONTINUE
        CPIX(M) = CPIX(M)*5.D-1
 40   CONTINUE

!
!--- Calculate Pitzer Activities
!
      DO 60 M = 1,NSPL+1

        PHI1 = 0.D+0
        PHI2 = 0.D+0
        PHI3 = 0.D+0
        PHI4 = 0.D+0
        PHI5 = 0.D+0
        PHI6 = 0.D+0
        PHI7 = 0.D+0
        PHI8 = 0.D+0
        
        F1   = 0.D+0
        F2   = 0.D+0
        F3   = 0.D+0
        F4   = 0.D+0
        F5   = 0.D+0
        TMA  = 0.D+0
        TMC  = 0.D+0
        TMN  = 0.D+0
        FPR  = 0.D+0
!
!--- Calculate g functions
!
        DO I = 1,4
          X1 = ALPHA(I)*DSQRT(CPIX(M))
          X2 = X1*X1
          DEX = DEXP(-X1)
          G(I) = 2.0D0*(1.0D0-(1.0D0+X1)*DEX)/X2
          GP(I) = -2.0D0*(1.0D0-(1.0D0+X1+X2/2.0D0)*DEX)/X2
          GPP(I) = -(2.0d0*GP(I)+(X1/2.0d0)*DEX)/(CPIX(M)*CPIX(M))
        END DO
!
!--- Calculate b functions
!
        DO I = 1,NCC
          DO J = 1,NA
            K1 = 1
            K2 = 3
            IF( SP_L(1,JPC(I)).GE.2.0D0.AND. &
              DABS(SP_L(1,(JPA(J)))).GE.2.0D0 )THEN
              K1 = 2
              K2 = 4
            END IF
            IF(SP_L(1,JPC(I)).EQ.2.0D0.AND. &
              DABS(SP_L(1,JPA(J))).EQ.2.0D0 )THEN
              K1 = 2
              K2 = 3
            END IF
            X1 = -ALPHA(K1)*DSQRT(CPIX(M))
            X2 = -ALPHA(K2)*DSQRT(CPIX(M))
            BMMX(I,J) = B0(I,J)+B1(I,J)*G(K1)+B2(I,J)*G(K2)
            BPHI(I,J) = B0(I,J)+B1(I,J)*DEXP(X1)+B2(I,J)*DEXP(X2)
            BPR(I,J) = B1(I,J)*GP(K1)/CPIX(M)+B2(I,J)*GP(K2)/CPIX(M)
            BPPR(I,J) = B1(I,J)*GPP(K1)+B2(I,J)*GPP(K2)
          END DO
        END DO
!
!--- Calculate higher order mixing functions
!
        CALL HOMIX(CPIX(M),APHI)
!
!--- Start calculations for activity and osmotic coefficients
!
        TMP = 1.0d0+BB*DSQRT(CPIX(M))
        F1 = -APHI*DSQRT(CPIX(M))/TMP
        F2 = -APHI*(2.0d0/BB)*DLOG(TMP)
        PHI1 = F1*CPIX(M)
        FPR = -APHI*(TMP+0.5d0)/(DSQRT(CPIX(M))*TMP*TMP)
!
!--- Start first major loop
!--- The terms are labeled in roughly the order they appear
!--- in the sums given by Felmy and Weare

        F3 = 0.0D0
        PHI2 = 0.0D0
        HBPP = 0.0D0

        DO I = 1,NCC
          DO J = 1,NA
            IF( JPC(I).EQ.(M-1) ) THEN
              CLXC = SP_CX(JPC(I)) + DSP_CX(JPC(I))
            ELSE
              CLXC = SP_CX(JPC(I))
            ENDIF
            IF( JPA(J).EQ.(M-1) ) THEN
              CLXA = SP_CX(JPA(J)) + DSP_CX(JPA(J))
            ELSE
              CLXA = SP_CX(JPA(J))
            ENDIF
            CMXC = CLXC/(SLX*PORDX)
            CMXC = CMXC/(RHOLX*XLWX)
            CMXA = CLXA/(SLX*PORDX)
            CMXA = CMXA/(RHOLX*XLWX)
            TMP = CMXC*CMXA
            F3 = F3+TMP*BPR(I,J)
            G4M(M) = G4M(M)+TMP*CMXX(I,J)
            PHI2 = PHI2+TMP*BPHI(I,J)
            HBPP = HBPP+TMP*BPPR(I,J)
            TMP1 = 2.0d0*BMMX(I,J)+CAPZ(M)*CMXX(I,J)
            LNG(M,JPC(I)) = LNG(M,JPC(I))+CMXA*TMP1
            LNG(M,JPA(J)) = LNG(M,JPA(J))+CMXC*TMP1
          END DO
        END DO

        
        PHI2 = PHI2+CAPZ(M)*G4M(M)

!
!--- Ternary electrolyte terms
!
        F4 = 0.0D0
        PHI3 = 0.0D0
        HTC = 0.0D0
        
        IF( NCC.GE.2 )THEN
          NT = 1
          DO I = 1,NCC-1
            DO J = I+1,NCC
              IF( JPC(I).EQ.(M-1) ) THEN
                CLXC = SP_CX(JPC(I)) + DSP_CX(JPC(I))
              ELSE
                CLXC = SP_CX(JPC(I))
              ENDIF
              IF( JPC(J).EQ.(M-1) ) THEN
                CLXC2 = SP_CX(JPC(J)) + DSP_CX(JPC(J))
              ELSE
                CLXC2 = SP_CX(JPC(J))
              ENDIF
              CMXC = CLXC/(SLX*PORDX)
              CMXC = CMXC/(RHOLX*XLWX)
              CMXC2 = CLXC2/(SLX*PORDX)
              CMXC2 = CMXC2/(RHOLX*XLWX)
              TMP = CMXC*CMXC2
              F4 = F4+TMP*CTCPR(NT)
              PHI3 = PHI3+TMP*CTCPH(NT)
              HTC = HTC+TMP*CTCPPR(NT)
!      
!--- Now g2m
!      
              DO K = 1,NCC
                N1 = 0
                IF( JPC(I).EQ.JPC(K) ) N1=J
                IF( JPC(J).EQ.JPC(K) ) N1=I
                IF( N1.NE.0 )THEN
                  IF( JPC(N1).EQ.(M-1) ) THEN
                    CLXC = SP_CX(JPC(N1)) + DSP_CX(JPC(N1))
                  ELSE
                    CLXC = SP_CX(JPC(N1))
                  ENDIF
                  CMXC = CLXC/(SLX*PORDX)
                  CMXC = CMXC/(RHOLX*XLWX)
                  TMP1 = CMXC
                  LNG(M,JPC(K)) = LNG(M,JPC(K))+2.0d0*TMP1*CTC(NT)
                  DO N = 1,NA
                    IF( JPA(N).EQ.(M-1) ) THEN
                      CLXA = SP_CX(JPA(N)) + DSP_CX(JPA(N))
                    ELSE
                      CLXA = SP_CX(JPA(N))
                    ENDIF
                    CMXA = CLXA/(SLX*PORDX)
                    CMXA = CMXA/(RHOLX*XLWX)
                    LNG(M,JPC(K)) = LNG(M,JPC(K))+TMP1*CMXA*PSIC(NT,N)
                  END DO
                END IF
              END DO
             
              DO K = 1,NA
                TMP1 = TMP*PSIC(NT,K)
                LNG(M,JPA(K)) = LNG(M,JPA(K))+TMP1
                IF( JPA(K).EQ.(M-1) ) THEN
                  CLXA = SP_CX(JPA(K)) + DSP_CX(JPA(K))
                ELSE
                  CLXA = SP_CX(JPA(K))
                ENDIF
                CMXA = CLXA/(SLX*PORDX)
                CMXA = CMXA/(RHOLX*XLWX)
                PHI3 = PHI3+TMP1*CMXA
              END DO
              NT=NT+1
            END DO
          END DO
        END IF

        F5 = 0.0D0
        PHI4 = 0.0D0
        HTA = 0.0D0

        IF( NA.GE.2 )THEN
          NT = 1
          DO I = 1,NA-1
            DO J = I+1,NA
              IF( JPA(I).EQ.(M-1) ) THEN
                CLXA = SP_CX(JPA(I)) + DSP_CX(JPA(I))
              ELSE
                CLXA = SP_CX(JPA(I))
              ENDIF
              IF( JPA(J).EQ.(M-1) ) THEN
                CLXA2 = SP_CX(JPA(J)) + DSP_CX(JPA(J))
              ELSE
                CLXA2 = SP_CX(JPA(J))
              ENDIF
              CMXA = CLXA/(SLX*PORDX)
              CMXA = CMXA/(RHOLX*XLWX)
              CMXA2 = CLXA2/(SLX*PORDX)
              CMXA2 = CMXA2/(RHOLX*XLWX)
              TMP = CMXA*CMXA2
              F5 = F5+TMP*CTAPR(NT)
              PHI4 = PHI4+TMP*CTAPH(NT)
              HTA = HTA+TMP*CTAPPR(NT)
!      
!--- Now g2x
!      
              DO K = 1,NA
                N1 = 0
                IF( JPA(I).EQ.JPA(K) )N1=J
                IF( JPA(J).EQ.JPA(K) )N1=I
                IF( N1.NE.0 )THEN
                  IF( JPA(N1).EQ.(M-1) ) THEN
                    CLXA = SP_CX(JPA(N1)) + DSP_CX(JPA(N1))
                  ELSE
                    CLXA = SP_CX(JPA(N1))
                  ENDIF
                  CMXA = CLXA/(SLX*PORDX)
                  CMXA = CMXA/(RHOLX*XLWX)
                  TMP1 = CMXA
                  LNG(M,JPA(K)) = LNG(M,JPA(K))+2.0d0*TMP1*CTA(NT)
                  DO N = 1,NCC
                    IF( JPC(N).EQ.(M-1) ) THEN
                      CLXC = SP_CX(JPC(N)) + DSP_CX(JPC(N))
                    ELSE
                      CLXC = SP_CX(JPC(N))
                    ENDIF
                    CMXC = CLXC/(SLX*PORDX)
                    CMXC = CMXC/(RHOLX*XLWX)
                    LNG(M,JPA(K)) = LNG(M,JPA(K))+TMP1*CMXC*PSIA(NT,N)
                  END DO
                END IF
              END DO
          
              DO K = 1,NCC
                TMP1 = TMP*PSIA(NT,K)
                LNG(M,JPC(K)) = LNG(M,JPC(K))+TMP1
                IF( JPC(K).EQ.(M-1) ) THEN
                  CLXC = SP_CX(JPC(K)) + DSP_CX(JPC(K))
                ELSE
                  CLXC = SP_CX(JPC(K))
                ENDIF
                CMXC = CLXC/(SLX*PORDX)
                CMXC = CMXC/(RHOLX*XLWX)
                PHI4 = PHI4+TMP1*CMXC
              END DO
              NT = NT+1
            END DO
          END DO
        END IF


        PHI5 = 0.0D0
        PHI6 = 0.0D0
        PHI7 = 0.0D0
        PHI8 = 0.0D0
        
        IF( NNN.GT.0 )THEN
          NT = 1
          DO I = 1,NNN
            IF( JPN(I).EQ.(M-1) ) THEN
              CLXN = SP_CX(JPN(I)) + DSP_CX(JPN(I))
            ELSE
              CLXN = SP_CX(JPN(I))
            ENDIF
            CMXN = CLXN/(SLX*PORDX)
            CMXN = CMXN/(RHOLX*XLWX)
            TMN = CMXN
            DO J = 1,NCC
              IF( JPC(J).EQ.(M-1) ) THEN
                CLXC = SP_CX(JPC(J)) + DSP_CX(JPC(J))
              ELSE
                CLXC = SP_CX(JPC(J))
              ENDIF
              CMXC = CLXC/(SLX*PORDX)
              CMXC = CMXC/(RHOLX*XLWX)
              TMC = CMXC
              PHI5 = PHI5+TMN*TMC*CLAMB(I,J)
              LNG(M,JPC(J)) = LNG(M,JPC(J))+2.0d0*TMN*CLAMB(I,J)
              LNG(M,JPN(I)) = LNG(M,JPN(I))+2.0d0*TMC*CLAMB(I,J)
              DO K = 1,NA
                IF( JPA(K).EQ.(M-1) ) THEN
                  CLXA = SP_CX(JPA(K)) + DSP_CX(JPA(K))
                ELSE
                  CLXA = SP_CX(JPA(K))
                ENDIF
                CMXA = CLXA/(SLX*PORDX)
                CMXA = CMXA/(RHOLX*XLWX)
                TMA = CMXA
                PHI7 = PHI7+TMN*TMC*TMA*HOLAMB(NT,K)
                LNG(M,JPN(I)) = LNG(M,JPN(I))+TMC*TMA*HOLAMB(NT,K)
                LNG(M,JPC(J)) = LNG(M,JPC(J))+TMN*TMA*HOLAMB(NT,K)
                LNG(M,JPA(K)) = LNG(M,JPA(K))+TMN*TMC*HOLAMB(NT,K)
              END DO
              NT = NT+1
            END DO
            DO K = 1,NA
              LNG(M,JPA(K)) = LNG(M,JPA(K))+2.0d0*TMN*ALAMB(I,K)
              IF( JPA(K).EQ.(M-1) ) THEN
                CLXA = SP_CX(JPA(K)) + DSP_CX(JPA(K))
              ELSE
                CLXA = SP_CX(JPA(K))
              ENDIF
              CMXA = CLXA/(SLX*PORDX)
              CMXA = CMXA/(RHOLX*XLWX)
              LNG(M,JPN(I)) = LNG(M,JPN(I))+2.0D0*CMXA*ALAMB(I,K)
              PHI6 = PHI6+TMN*CMXA*ALAMB(I,K)
            END DO
            DO K = 1,NNN
              IF( JPN(K).EQ.(M-1) ) THEN
                CLXN = SP_CX(JPN(K)) + DSP_CX(JPN(K))
              ELSE
                CLXN = SP_CX(JPN(K))
              ENDIF
              CMXN = CLXN/(SLX*PORDX)
              CMXN = CMXN/(RHOLX*XLWX)
              PHI8 = PHI8+CMXN*TMN*ELAMB(I,K)
              LNG(M,JPN(I)) = LNG(M,JPN(I))+2.0d0*CMXN*ELAMB(I,K)
            END DO
          END DO
        END IF
!
!--- Sum up the f function and carry along the proper charges
!
        FF(M) = F1+F2+F3+F4+F5
!
!--- Calculate osmotic coefficient
!
        IF(SUM_M.NE.0)PHI = (2.0d0/SUM_M)* &
                            (PHI1+PHI2+PHI3+PHI4+PHI5+PHI6+PHI7+PHI8)
        PHI = PHI+1.0d0
        
        DO 50 NSP = 1,NSPL
          IF( NSP.EQ.IKH2O ) THEN
            IF( IKH2O.EQ.(M-1) ) THEN
              CLX = SP_CX(IKH2O) + DSP_CX(IKH2O)
            ELSE
              CLX = SP_CX(IKH2O)
            ENDIF
            CMX = CLX/(SLX*PORDX)
            CMX = CMX/(RHOLX*XLWX)
            LNA = -.0180153D+0*SUM_M*PHI
            H2OACT = DEXP(LNA)
            LNG(M,IKH2O) = LNA - DLOG(CMX)
            ACTVX(M,IKH2O) = DEXP(LNG(M,IKH2O))
          ELSE
            LNG(M,NSP) = LNG(M,NSP)+SP_L(1,NSP)*SP_L(1,NSP)*FF(M)+ &
                       DABS(SP_L(1,NSP))*G4M(M)
            ACTVX(M,NSP) = DEXP(LNG(M,NSP))
          ENDIF
  50    CONTINUE
  60  CONTINUE

!
!---  End of PITZER ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END 
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RATER( NEQX,M,NS,N,IRCX,NSPKX,NSPRX,NSPPX,SP_CX,ACTVX, &
            VTOMX,VTOLX,VTOSX,EQKX,RRCX,RSBX )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     REACTION RATE.
!
!     YSPLX aqueous fraction of component species NEQ at node N
!     YSPGX gas fraction of component species NEQ at node N
!     YSPNX NAPL fraction of component species NEQ at node N
!     C(NSL,N) component species concentration (kmol/m^3 node)
!     SP_C(NSP,N) species concentration (kmol/m^3 node)
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!     Last Modified by Mark White, PNNL, August 15, 2005.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE PORMED
      USE FDVP
      USE CONST
!
!----------------------Fortran 90 Modules------------------------------!
!
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
      REAL*8 ACTVX(LSPL+1,LSPL), SP_CX(LSPR)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RATER'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---    TST kinetic reaction  ---
!
        IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR. &
          (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) .OR. &
           IRCKT(IRCX).EQ.120 ) THEN
!
!---      Ion activity product mol/kg water, loop over species in
!         kinetic reaction  ---
!
          QX = 1.D+0
          DO 120 L = 1,NSPKX
            NSPX = IRC_K(L+2,IRCX)
!
!---        Aqueous species,
!           concentration in molality, mol/kg H2O  ---
!
            IF( NSPX.LE.NSPL ) THEN
              CMX = SP_CX(NSPX)*VTOMX
              ACX = ACTVX(1,NSPX)
!
!---          Reactants  ---
!
              N1 = MAX(1,N*IRCKN(L))
              IF( L.LE.NSPRX ) THEN
                QX = QX*((CMX*ACX)**RC_K(L,N1,IRCX))
!
!---          Products  ---
!
              ELSE
                QX = QX/((CMX*ACX)**RC_K(L,N1,IRCX))
              ENDIF
!
!---        Solid species, skip  ---
!
            ELSEIF( NSPX.LE.NSPL+NSPS ) THEN
              GOTO 120
            ENDIF
  120     CONTINUE
!
!---      Initial reactive surface area, initial mineral volume fraction,
!         current mineral volume fraction, minimum current mineral volume
!         fraction allows re-precipitation of dissolved primary minerals
!         NSP_M - mineral species number  ---
!
          NSPX = IRC_K(3+NSPKX,IRCX)
          NSP_M =  NSPX - NSPL
!
!---      Primary mineral  ---
!
          IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
            AOX = RS_S(1,NSP_M,N)*VOL(N)*RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
            VFMOX = RS_S(2,NSP_M,N)
            IF( ISP_MN(NSPX).EQ.1 ) THEN
              VFMX = 1.D-3*(SP_CMN(NSP_M,N)+SP_CX(NSPX)) &
                *SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ELSE
              VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
            VFMX = MAX( VFMX,1.D-2 )
!
!---      Secondary mineral, initial reactive surface area
!         for seconary minerals is set to 0.25 m^2/dm^3  ---
!
          ELSE
            AOX = 0.25D+3*VOL(N)
            VFMOX = 1.D-2
            VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            VFMX = MAX( VFMX,1.D-2 )
          ENDIF
!
!---      Reactive surface area  ---
!
          AX = AOX*(((POR_M(2,N)*VFMX)/ &
            (POR(2,N)*VFMOX))**(2.D+0/3.D+0))
          IF( ISLC(56).EQ.2 ) AX = AOX
!
!---      Reaction rate, mol/s  ---
!
          RRBX = -AX*RRCX*(1.D+0-(QX/EQKX))
!          if(islc(43) >= 2) then
          IF( IRCKT(IRCX).EQ.16 ) THEN
            AX = AOX
            RRBX = -RRCX*(1.D+0-(QX/EQKX))*vol(n)
          endif
!
!---      pH dependence  ---
!
          IF( IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9 &
            .AND. ISPLK(1).NE.0 ) THEN
            NSP_PHX = MOD(ISPLK(1),100)
            PHX = -LOG10(1.D-3*SP_CX(NSP_PHX)*VTOLX)
            IF( IRCKT(IRCX).GE.8 .AND. IRCKT(IRCX).LE.9 ) THEN
              RRBX = RRBX*MAX( 0.D+0, &
                (7.9201D-1 - 1.3479D-1*PHX + 5.2D-3*(PHX**2)))
            ELSE
              N9 = MAX(1,N*IRCKN(NSPKX+9))
              RRBX = RRBX*(1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))
            ENDIF
          ENDIF
!
!---      iex pH and simulation time dependence  ---
!
          IF( IRCKT(IRCX).EQ.120 .AND. ISPLK(1).NE.0 ) THEN
            NSP_PHX = MOD(ISPLK(1),100)
            PHX = -LOG10(1.D-3*SP_CX(NSP_PHX)*VTOLX)
            N9 = MAX(1,N*IRCKN(NSPKX+9))
            RRBX = RRBX*(1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))

            N10 = MAX(1,N*IRCKN(NSPKX+10))
            RRBX = RRBX*TM**(-RC_K(NSPKX+10,N10,IRCX))
          ENDIF
!
!---      Reaction rate, mol/m^3 aqu s  ---
!
          RRBX = RRBX*VTOLX/VOL(N)
!
!---      Direction limited  ---
!
          IF( IRCKT(IRCX).EQ.6 .OR. IRCKT(IRCX).EQ.8 &
            .OR. IRCKT(IRCX).EQ.11 ) THEN
            RRBX = MAX( RRBX,0.D+0 )
          ELSEIF( IRCKT(IRCX).EQ.7 .OR. IRCKT(IRCX).EQ.9 &
            .OR. IRCKT(IRCX).EQ.12 .OR. IRCKT(IRCX).EQ.120) THEN
            RRBX = MIN( RRBX,0.D+0 )
          ENDIF
!
!---    Forward-backward kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.1 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          FRRX = RC_K(NSPKX+1,N1,IRCX)
          BRRX = RC_K(NSPKX+2,N2,IRCX)
!
!---      Loop over reactants  ---
!
          DO 140 L = 1,NSPRX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
            N1 = MAX(1,N*IRCKN(L))
            FRRX = FRRX*(CMX**RC_K(L,N1,IRCX))
  140     CONTINUE
!
!---      Loop over products  ---
!
          DO 160 L = 1,NSPPX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2+NSPRX,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
            N1 = MAX(1,N*IRCKN(L+NSPRX))
            BRRX = BRRX*(CMX**RC_K(L+NSPRX,N1,IRCX))
  160     CONTINUE
          RRBX = FRRX - BRRX
!
!---    Valocchi-Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.2 ) THEN
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Partial rate of donor degredation  ---
!
          N6 = MAX(1,N*IRCKN(6))
          RRBX = RC_K(6,N6,IRCX)*CMX
!
!---      Concentration of donor in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX = RRBX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/kg water  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX = RRBX*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---    Valocchi-Sorption kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.3 ) THEN
!
!---      Concentration of sorbed species in mol/gm soil  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOSX*1.D-3
          N4 = MAX(1,N*IRCKN(4))
          RRBX = CMX/RC_K(4,N4,IRCX)
!
!---      Concentration of aqueous species in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of sorption in mol/m^3 aqu s  ---
!
          N3 = MAX(1,N*IRCKN(3))
          RRBX = RC_K(3,N3,IRCX)*(CMX-RRBX)*1.D+3
!
!---    Valocchi-Biomass kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.4 ) THEN
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Partial rate of donor degredation  ---
!
          N6 = MAX(1,N*IRCKN(6))
          RRBX = RC_K(6,N6,IRCX)*CMX
!
!---      Concentration of donor in mol/m^3 aqu  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX = RRBX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/m^3 aqu  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX = RRBX*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of biomass production in mol/m^3 aqu s  ---
!
          N7 = MAX(1,N*IRCKN(7))
          N8 = MAX(1,N*IRCKN(8))
          RRBX = RC_K(7,N7,IRCX)*RRBX - RC_K(8,N8,IRCX)*CMX
!
!---    Liu's multi-rate kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.41 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          RMX = RC_K(NSPKX+1,N1,IRCX)
!
!---      Loop over reactants  ---
!
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          N4 = MAX(1,N*IRCKN(NSPKX+4))
          N5 = MAX(1,N*IRCKN(NSPKX+5))
          SDENX = RC_K(NSPKX+2,N2,IRCX)*VTOLX/VTOSX
          PFRCX = RC_K(NSPKX+3,N3,IRCX)
          XLGK1 = RC_K(NSPKX+4,N4,IRCX)
          XLGK2 = RC_K(NSPKX+5,N5,IRCX)
          FRRX = 1.D0
          BRRX = 1.D0
          DO L = 1,NSPRX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2,IRCX)
            CMX = SP_CX(NSPX)*VTOMX
            IF(NSPX.LE.NSPL) THEN
              ACX = ACTVX(1,NSPX)
            ELSE
              ACX =1.D0
            ENDIF
            N1 = MAX(1,N*IRCKN(L))
            FRRX = FRRX*((CMX*ACX)**RC_K(L,N1,IRCX))
          ENDDO
!
!---      Loop over products  ---
!
          DO L = 1,NSPPX-1
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2+NSPRX,IRCX)
            CMX = SP_CX(NSPX)*VTOMX
            IF(NSPX.LE.NSPL) THEN
              ACX = ACTVX(1,NSPX)
            ELSE
              ACX = 1.D0
            ENDIF
            N1 = MAX(1,N*IRCKN(L+NSPRX))
            BRRX = BRRX*((CMX*ACX)**RC_K(L+NSPRX,N1,IRCX))
          ENDDO
          L=NSPPX
          NSPX = IRC_K(L+2+NSPRX,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX*10**XLGK1
          BRRX = BRRX*10**XLGK2
          RRBX = RMX*(SDENX*PFRCX*FRRX/(1.D0+FRRX+BRRX)-CMX)
!
!---    Liu's dual domain kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.42 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          RMX = RC_K(NSPKX+1,N1,IRCX)
!
!---      Loop over reactants  ---
!
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          PFRCX = RC_K(NSPKX+3,N3,IRCX)
          FRRX = 0.D0
!
!---        Global species index  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX+CMX**VTOLX
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX-CMX**VTOLX
          RRBX = RMX*FRRX
!
!---    Multirate  ---
!
        ELSEIF( IRCKT(IRCX).EQ.20 ) THEN
!
!---      Neutral reaction rate, mol/m^2 s  ---
!
          TKX = T(2,N)+TABS
          N1 = MAX(1,N*IRCKN(1))
          N2 = MAX(1,N*IRCKN(2))
          N3 = MAX(1,N*IRCKN(3))
          TKRX = RC_K(3,N3,IRCX)+TABS
          RRCX = RC_K(1,N1,IRCX)*EXP( -RC_K(2,N2,IRCX)* &
            ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )
!
!---      Loop over mechanisms  ---
!
          DO 190 NKRMX = 1,IRC_K(2,IRCX)
            IX = 3+((NKRMX-1)*6)
!
!---        Ion activity product mol/kg water, loop over species  ---
!
            QX = 1.D+0
            DO 180 L = 1,IRC_K(IX,IRCX)
              IX = 3+((NKRMX-1)*6)+L
              NSPX = IRC_K(IX,IRCX)
!
!---          Aqueous species,
!             concentration in molality, mol/kg H2O  ---
!
              IF( NSPX.LE.NSPL ) THEN
                CMX = SP_CX(NSPX)*VTOMX
                ACX = ACTVX(1,NSPX)
                IX = 6+((NKRMX-1)*8)+L
                NX = MAX(1,N*IRCKN(IX))
                QX = QX*((CMX*ACX)**RC_K(IX,NX,IRCX))
              ENDIF
  180       CONTINUE
            IX = 4+((NKRMX-1)*8)
            NX = MAX(1,N*IRCKN(IX))
            N1 = MAX(1,N*IRCKN(IX+1))
            N2 = MAX(1,N*IRCKN(IX+2))
            TKRX = RC_K(IX+2,N2,IRCX)+TABS
            RRCX = RRCX + RC_K(IX,NX,IRCX)*EXP( -RC_K(IX+1,N1,IRCX)* &
            ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )*QX
  190     CONTINUE
!
!---      Initial reactive surface area, initial mineral volume fraction,
!         current mineral volume fraction, minimum current mineral volume
!         fraction allows re-precipitation of dissolved primary minerals
!         NSP_M - mineral species number  ---
!
          NSPX = IRC_K(1,IRCX)
          NSP_M =  NSPX - NSPL
!
!---      Primary mineral  ---
!
          IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
            AOX = RS_S(1,NSP_M,N)*VOL(N)*RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
            VFMOX = RS_S(2,NSP_M,N)
            IF( ISP_MN(NSPX).EQ.1 ) THEN
              VFMX = 1.D-3*(SP_CMN(NSP_M,N)+SP_CX(NSPX)) &
                *SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ELSE
              VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
            VFMX = MAX( VFMX,1.D-2 )
!
!---      Secondary mineral, initial reactive surface area
!         for seconary minerals is set to 0.25 m^2/dm^3  ---
!
          ELSE
            AOX = 0.25D+3*VOL(N)
            VFMOX = 1.D-2
            VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            VFMX = MAX( VFMX,1.D-2 )
          ENDIF
!
!---      Reactive surface area  ---
!
          AX = AOX*(((POR_M(2,N)*VFMX)/ &
            (POR(2,N)*VFMOX))**(2.D+0/3.D+0))
!
!---      Reaction rate, mol/s  ---
!
          RRBX = -AX*RRCX
!          if(islc(43) >= 2) then
          IF( IRCKT(IRCX).EQ.16 ) THEN
            AX = AOX
            RRBX = -RRCX*vol(n)
          endif
!
!---      Reaction rate, mol/m^3 aqu s  ---
!
          RRBX = RRBX*VTOLX/VOL(N)
        ENDIF
!
!Liu's dual domain model
        IF( IRCKT(IRCX).EQ.42 ) THEN
          EQ_KX = EQ_K(NS+M,NEQX)
          IF( IMMB(NEQC+NEQX) == 1 ) THEN
! RC_K(2,IRCX) is pore fraction of immobile domain
            N2 = MAX(1,N*IRCKN(NSPKX+2))
            EQ_KX = EQ_K(NS+M,NEQX)*RC_K(NSPKX+2,N2,IRCX)
          ENDIF
          RSBX = RSBX + RRBX*EQ_KX
        ELSE
          RSBX = RSBX + RRBX*EQ_K(NS+M,NEQX)
        ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RATER group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RESET_SP
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Reset reactive-species concentrations with old time-step
!     component-species concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 08 January 2013.
!     Last Modified by Mark White, PNNL, 08 January 2013.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RESET_SP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = &
       '$Id: eckechem.F,v 1.12 2013/01/25 19:54:22 d3c002 Exp $'
!
!---  Loop over active nodes  ---
!
      DO 100 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 100
        DO 20 NSP = 1,NSPR
          SP_C(NSP,N) = SP_CO(NSP,N)
   20   CONTINUE
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RESET_SP group  ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RMNSP
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Reconstitute mineral species concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 2 May 2006.
!     Last Modified by Mark White, PNNL, 2 May 2006.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE GRID
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RMNSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = & 
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Loop over active nodes  ---
!
      DO 100 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Loop over solid species  ---
!
        DO 10 NSPX = 1,NSPS
          NSP = NSPL + NSPX
!
!---      Mineral species  ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            SP_C(NSP,N) = SP_C(NSP,N) + SP_CMN(NSPX,N)
          ENDIF
   10   CONTINUE
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RMNSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SEQEQ
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
!     Sequence reaction equations to assure non-zero diagonals.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 15 December 2004.
!     Last Modified by MD White, PNNL, 15 December 2004.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
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
      INTEGER ISPX(LSPR)
      CHARACTER*64 GETSPNM
      EXTERNAL GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      USE_GA = .TRUE.
      SUBNMX = '/SEQEQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = & 
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Define a pseudo card  ---
!
      ALLOCATE(IEQ_S(LSPR))
      ALLOCATE(ISP_S(LEQE+LEQC+LEQK))
      IEQ_S = 0
      ISP_S = 0
      CARD = 'Equilibrium-Kinetic-Conservation Equation Cards'
      ICD = INDEX( CARD,'  ' )-1
!
!---  Check that the number of reaction equations (i.e., equilibrium,
!     kinetic, and conservation equations equals the number of reactive
!     species  ---
!
      NEQR = NEQE + NEQK + NEQC
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
      IF( NEQR.NE.NSPR ) THEN
        INDX = 4
        CHMSG = 'Number of Equations ­ Number of Species'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Initialize species and equation counters  ---
!
      DO 10 NSP = 1,NSPR
        ISPX(NSP) = 0
   10 CONTINUE
!
!---  Count number of times species appear in equations  ---
!
      DO 30 NEQ = 1,NEQE
        DO 20 NSP = 2,IEQ_E(1,NEQ)+1
          I = IEQ_E(NSP,NEQ)
          ISPX(I) = ISPX(I) + 1
   20   CONTINUE
   30 CONTINUE
      DO 50 NEQ = 1,NEQC
        DO 40 NSP = 2,IEQ_C(1,NEQ)+1
          I = IEQ_C(NSP,NEQ)
          ISPX(I) = ISPX(I) + 1
   40   CONTINUE
   50 CONTINUE
      DO 70 NEQ = 1,NEQK
        DO 60 NSP = 2,IEQ_K(1,NEQ)+1
          I = IEQ_K(NSP,NEQ)
          ISPX(I) = ISPX(I) + 1
   60   CONTINUE
   70 CONTINUE
!
!---  Check for species that never appear in a
!     reactive equation  ---
!
      IMNX = NSPR
      DO 100 NSP = 1,NSPR
        IMNX = MIN( IMNX,ISPX(NSP) )
        IF( ISPX(NSP).EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Species Not Found in Reactive Equations: ' // & 
            GETSPNM(NSP)
          CALL WRMSGS( INDX )
        ENDIF
  100 CONTINUE
!
!---  Associate conservation equations with their component species  ---
!
      DO 110 NEQ = 1,NEQC
        NSP = IEQ_C(2,NEQ)
        NC = NEQ + NEQE
        IEQ_S(NSP) = NC
        ISP_S(NC) = NSP
  110 CONTINUE
      GOTO 300
!
!---  Correlate species and equations for species that appear
!     in a single reactive equation  ---
!
  200 CONTINUE
!
!---  Loop over all species  ---
!
      DO 290 NSP = 1,NSPR
        IF( ISPX(NSP).EQ.IMNX ) THEN
          NSPMN = NSPR
!
!---      Loop over equilibrium equations  ---
!
          DO 230 NEQ = 1,NEQE
            NC = NEQ
            DO 220 N = 2,IEQ_E(1,NEQ)+1
!
!---          If the equilibrium equation contains the specie,
!             is unassigned, and contains the fewest number of species,
!             then assign equilibrium equation to species, using
!             global equation indexing
!             (i.e., equilibrium-conservation-kinetic)  ---
!
              IF( NSP.EQ.IEQ_E(N,NEQ) .AND. ISP_S(NC).EQ.0 ) THEN
                IF( IEQ_E(1,NEQ).LE.NSPMN ) THEN
                  IEQ_S(NSP) = NC
                  NSPMN = IEQ_E(1,NEQ)
                  GOTO 230
                ENDIF
              ENDIF
  220       CONTINUE
  230     CONTINUE
!          DO 250 NEQ = 1,NEQC
!            NC = NC + 1
!            DO 240 N = 2,IEQ_C(1,NEQ)+1
!!
!!---          If the conservation equation contains the specie,
!!             is unassigned, and contains the fewest number of species,
!!             then assign conservation equation to species, using
!!             global equation indexing
!!             (i.e., equilibrium-conservation-kinetic)  ---
!!
!              IF( NSP.EQ.IEQ_C(N,NEQ) .AND. ISP_S(NC).EQ.0 ) THEN
!                IF( IEQ_C(1,NEQ).LT.NSPMN ) THEN
!                  IEQ_S(NSP) = NC
!                  NSPMN = IEQ_C(1,NEQ)
!                  GOTO 250
!                ENDIF
!              ENDIF
!  240       CONTINUE
!  250     CONTINUE
!
!---      Loop over kinetic equations  ---
!
          DO 270 NEQ = 1,NEQK
            NC = NEQ + NEQC + NEQE
            DO 260 N = 2,IEQ_K(1,NEQ)+1
!
!---          If the kinetic equation contains the specie,
!             is unassigned, and contains the fewest number of species,
!             then assign kinetic equation to species, using
!             global equation indexing
!             (i.e., equilibrium-conservation-kinetic)  ---
!
              IF( NSP.EQ.IEQ_K(N,NEQ) .AND. ISP_S(NC).EQ.0 ) THEN
                IF( IEQ_K(1,NEQ).LT.NSPMN ) THEN
                  IEQ_S(NSP) = NC
                  NSPMN = IEQ_K(1,NEQ)
                  GOTO 270
                ENDIF
              ENDIF
  260       CONTINUE
  270     CONTINUE
!
!---      Assign specie to equation with the fewest number
!         of species, regardless of equation type  ---
!
          IF( IEQ_S(NSP).GT.0 ) ISP_S(IEQ_S(NSP)) = NSP
        ENDIF
  290 CONTINUE
  300 CONTINUE
!
!---  Reinitialize species counter  ---
!
      DO 310 NSP = 1,NSPR
        ISPX(NSP) = 0
  310 CONTINUE
!
!---  Loop over equilibrium equations  ---
!
      DO 330 NEQ = 1,NEQE
        NC = NEQ
!
!---    Skip assigned equations  ---
!
        IF( ISP_S(NC).EQ.0 ) THEN
          DO 320 NSP = 2,IEQ_E(1,NEQ)+1
            I = IEQ_E(NSP,NEQ)
!
!---        Skip assigned species  ---
!
            IF( IEQ_S(I).EQ.0 ) THEN
              ISPX(I) = ISPX(I) + 1
            ENDIF
  320     CONTINUE
        ENDIF
  330 CONTINUE
!      DO 350 NEQ = 1,NEQC
!        NC = NC + 1
!!
!!---    Skip assigned equations  ---
!!
!        IF( ISP_S(NC).EQ.0 ) THEN
!          DO 340 NSP = 2,IEQ_C(1,NEQ)+1
!            I = IEQ_C(NSP,NEQ)
!!
!!---        Skip assigned species  ---
!!
!            IF( IEQ_S(I).EQ.0 ) THEN
!              ISPX(I) = ISPX(I) + 1
!            ENDIF
!  340     CONTINUE
!        ENDIF
!  350 CONTINUE
!
!---  Loop over kinetic equations  ---
!
      DO 370 NEQ = 1,NEQK
        NC = NEQ + NEQC + NEQE
!
!---    Skip assigned equations  ---
!
        IF( ISP_S(NC).EQ.0 ) THEN
          DO 360 NSP = 2,IEQ_K(1,NEQ)+1
            I = IEQ_K(NSP,NEQ)
!
!---        Skip assigned species  ---
!
            IF( IEQ_S(I).EQ.0 ) THEN
              ISPX(I) = ISPX(I) + 1
            ENDIF
  360     CONTINUE
        ENDIF
  370 CONTINUE
!
!---  Minimum species count in unassigned equations  ---
!
      IMNX = NSPR
      DO 380 NSP = 1,NSPR
        IF( ISPX(NSP).GT.0 ) IMNX = MIN( IMNX,ISPX(NSP) )
  380 CONTINUE
!
!---  Equation assignment not complete, continue search  ---
!
      IF( IMNX.LT.NSPR ) GOTO 200
!
!---  Check for unassigned species  ---
!
      DO 400 NSP = 1,NSPR
        IF( IEQ_S(NSP).EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Unassigned Species: ' // GETSPNM(NSP)
          CALL WRMSGS( INDX )
        ENDIF
  400 CONTINUE
!
!---  Print species and equations  ---
!
      IF( ME.EQ.0 ) WRITE (IWR,'(//,3A,/)') ' ~ ',CARD(1:ICD),': '
      DO 410 NSP = 1,NSPR
        IF( IEQ_S(NSP).GT.0 .AND. IEQ_S(NSP).LE.NEQE ) THEN
          IF( ME.EQ.0 ) WRITE (IWR,'(2A,I3,i3)') GETSPNM(NSP), & 
            ' => Equilibrium Equation # ',IEQ_S(NSP),ieq_s(nsp)
        ELSEIF( IEQ_S(NSP).LE.(NEQE+NEQC) ) THEN
          IF( ME.EQ.0 ) WRITE (IWR,'(2A,2I3)') GETSPNM(NSP), & 
            ' => Conservation Equation # ',IEQ_S(NSP),ieq_s(nsp)-neqe
        ELSEIF( IEQ_S(NSP).LE.(NEQE+NEQC+NEQK) ) THEN
          IF( ME.EQ.0 ) WRITE (IWR,'(2A,2I3)') GETSPNM(NSP), & 
            ' => Kinetic Equation # ',IEQ_S(NSP),ieq_s(nsp)-neqe-neqc
        ELSE
          INDX = 4
          CHMSG = 'Species Not Assigned to a ' & 
            // 'Conservation Equation: ' // GETSPNM(NSP)
          CALL WRMSGS( INDX )
        ENDIF
  410 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SEQEQ group  ---
!
      RETURN
      END
!----------------------Function--------------------------------------!
!
      REAL*8 FUNCTION TFUNC(A,TX)
!
!----------------------Description-------------------------------------!
!
! This subroutine calculates higher order electrostatic functions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by S. Yabusaki
!     Last Modified by VL Freedman, PNNL, 19 March 2007
!
      USE SOLTN
!
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
      REAL *8 A(8)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/TFUNC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = & 
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
      TFUNC = A(1)+A(2)*TX+A(3)/TX+A(4)*LOG(TX)+A(5)/(TX-263)+A(6) &
              *TX**2+A(7)/(680-TX)+A(8)/(TX-227)
!
!---  End of TFUNC ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END 

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE UPDTCHEM
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Load old reactive-species concentrations and
!     component-species concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, December 16, 2005.
!     Last Modified by Mark White, PNNL, December 16, 2005.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/UPDTCHEM'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = & 
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Loop over active nodes  ---
!
      DO 100 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 100
        DO 10 NEQ = 1,NEQC+NEQK
          NSL = NEQ + NSOLU
          CO(NSL,N) = C(NSL,N)
   10   CONTINUE
        DO 20 NSP = 1,NSPR
          SP_CO(NSP,N) = SP_C(NSP,N)
   20   CONTINUE
  100 CONTINUE
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of UPDTCHEM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ZLKSRC
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
!     Zero linked sources
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 6 June 2006.
!     Last Modified by MD White, PNNL, 6 June 2006.
!     $Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE GRID
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/ZLKSRC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IF( INDEX(CVS_ID(285)(1:1),'$').EQ.0 ) CVS_ID(285) = & 
       '$Id: eckechem.F,v 1.3 2011/01/24 23:56:18 d3c002 Exp $'
!
!---  Loop over all nodes, skipping inactive nodes  ---
!
      DO 100 N = 1,NUM_NODES
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Zero linked CO2 source  ---
!
        SRCA(1,N) = 0.D+0
 100  CONTINUE
!     ISUB_LOG = ISUB_LOG-1
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of ZLKSRC group  ---
!
      RETURN
      END
