

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SORIT1( NSL )
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
!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Compute solute transport source integrals.
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
      USE SOURC
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE FDVP
      USE BCVP
      USE CONST
      USE COUP_WELL
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
      REAL*8 SRX(8+LSOLU),C_FX(6),AREAXX(6)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/SORIT1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
        '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Loop over sources  ---
!
      DO 600 NS = 1,NSR
        IF( TM.LE.SRC(1,1,NS) ) GOTO 600
        SRX(1) = TM
        IF( ISRM(NS).EQ.1 ) THEN
          DO 70 N = 2,8+NSOLU
            SRX(N) = SRC(N,1,NS)
   70     CONTINUE
        ELSE
          DO 100 M = 2,ISRM(NS)
            IF( TM.LE.SRC(1,M,NS) ) THEN
              DTSR = MIN( SRC(1,M,NS)-TM,DT )
              TFSR = (TM-0.5D+0*DTSR-SRC(1,M-1,NS))/ &
              (SRC(1,M,NS)-SRC(1,M-1,NS))
              DO 80 N = 2,8+NSOLU
                SRX(N) = SRC(N,M-1,NS)+TFSR*(SRC(N,M,NS)-SRC(N,M-1,NS))
   80         CONTINUE
             GOTO 110
            ENDIF
  100     CONTINUE
          GOTO 600
        ENDIF
  110   CONTINUE

!
!---  Loop over source domain  ---
!
!          DO 500 I = ISRDM(1,NS),ISRDM(2,NS)
!          DO 500 J = ISRDM(3,NS),ISRDM(4,NS)
!          DO 500 K = ISRDM(5,NS),ISRDM(6,NS)
!            N = ND(I,J,K)
          NODE_S = UBOUND(ISRDM,DIM=1)
          DO 500 NODE_SX = 1,NODE_S
            N = ISRDM(NODE_SX,NS)
            IF(N.EQ.0) CYCLE
            IF( IXP(N).LE.0 ) GOTO 500
!
!---  Aqueous Volumetric Sink  ---
!
            IF( ISRT(NS).EQ.2 .AND. SRX(4).LT.0.D+0 ) THEN
              SRCIC(NSL,N) = SRCIC(NSL,N) - C(NSL,N)*SRX(4)* &
              YL(NSL,N)*DT/(SL(2,N)*PORD(2,N))
!
!---  Aqueous Volumetric Sink  ---
!
            ELSEIF( ISRT(NS).EQ.3 .AND. SRX(4).LT.0.D+0 ) THEN
              SRCIC(NSL,N) = SRCIC(NSL,N) - C(NSL,N)*SRX(4)*VOL(N)* &
              YL(NSL,N)*DT/(SL(2,N)*PORD(2,N))
!
!---  Aqueous Mass Sink  ---
!
            ELSEIF( ISRT(NS).EQ.4 .AND. SRX(4).LT.0.D+0 ) THEN
              SRCIC(NSL,N) = SRCIC(NSL,N) - C(NSL,N)*SRX(4)* &
              YL(NSL,N)*DT/(SL(2,N)*PORD(2,N)*RHOL(2,N))
!
!---  Aqueous Mass Density Sink  ---
!
            ELSEIF( ISRT(NS).EQ.5 .AND. SRX(4).LT.0.D+0 ) THEN
              SRCIC(NSL,N) = SRCIC(NSL,N) - C(NSL,N)*SRX(4)*VOL(N)* &
              YL(NSL,N)*DT/(SL(2,N)*PORD(2,N)*RHOL(2,N))
!
!---  Solute source  ---
!
            ELSEIF( ISRT(NS).EQ.-NSL .AND. NSL.LE.NSOLU ) THEN
              SRCIC(NSL,N) = SRCIC(NSL,N) + SRX(4)*DT
!
!---  Solute Density Source  ---
!
            ELSEIF( ISRT(NS).EQ.-(NSL+NSOLU) .AND. NSL.LE.NSOLU ) THEN
              SRCIC(NSL,N) = SRCIC(NSL,N) + SRX(4)*DT*VOL(N)
!
!---  Solute Inventory Source  ---
!
            ELSEIF( ISRT(NS).EQ.-(NSL+3*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!
!---  Solute inventory lost through 1 decay
!     Solute inventory transported from the node  ---
!
              CIRDX = 6.931D-1*SRX(3)*DT/HLF(NSL)
              DO IFCX = 1,6
                NX = ID_L2G(N)
                ICNX = ND2CNX(IFCX,NX)
                IF(ICNX > 0) THEN
                  AREAXX(IFCX) = AREAC(ICNX)
                  C_FX(IFCX) = C_FLUX(NSL,ICNX)
                ELSEIF(ICNX < 0) THEN
                  ICNX = ABS(ICNX)
                  IF(ICNX <= NUM_BCNX) THEN
                    AREAXX(IFCX) = AREAB(ICNX)
                    C_FX(IFCX)= C_FLUX_B(NSL,ICNX)
                  ENDIF
                ELSE
                  C_FX(IFCX) = 0.D0
                ENDIF
              ENDDO
              CITOX = -( C_FX(5)*AREAXX(5) + &
                C_FX(3)*AREAXX(3) + &
                C_FX(1)*AREAXX(1) - &
                C_FX(2)*AREAXX(2) - &
                C_FX(4)*AREAXX(4) - &
                C_FX(6)*AREAXX(6) )*DT
              CIRDX = MAX( CIRDX,0.D+0 )
              CITOX = MAX( CITOX,0.D+0 )
!
!---          If inventory remains, compute the inventory source
!             and inventory time derivative, reduce the inventory by the
!             amount of solute exiting the node and decaying, and
!             store the inventory in a field variable for writing  ---
!
              IF( SRC(3,1,NS).GT.EPSL ) THEN
!
!---            For the initial time step include the inventory needed
!               to charge the node with dissolved and sorbed solute  ---
!
                IF( SRX(5).LT.0.D+0 ) THEN
                  IF( SRX(3).LT.(SRX(4)*SL(2,N)*PORD(2,N)*VOL(N)/ &
                  YL(NSL,N)) ) THEN
                    SRCIC(NSL,N) = SRCIC(NSL,N) + SRX(3)
                    SRC(3,1,NS) = 0.D+0
                  ELSE
                    SRC(5,1,NS) = 1.D+0
                    CIFSX = MAX( SRX(4)*SL(2,N)*PORD(2,N)/ &
                    YL(NSL,N)-CO(NSL,N),0.D+0 )*VOL(N)
                    SRCIC(NSL,N) = SRCIC(NSL,N)+CIFSX+CITOX
                    SRC(3,1,NS) = SRC(3,1,NS)-CIFSX-CIRDX-CITOX
                  ENDIF
                ELSE
                  SRCIC(NSL,N) = SRCIC(NSL,N) + CITOX
                  SRC(3,1,NS) = SRC(3,1,NS)-CIRDX-CITOX
!
!---              For exhausted inventory reduce the dissolved and
!                 sorbed solute  ---
!
                  IF( SRC(3,1,NS).LT.0.D+0 ) THEN
                    C(NSL,N) = MAX((C(NSL,N)+SRC(3,1,NS)/VOL(N)),0.D+0)
                    SRC(3,1,NS) = 0.D+0
                  ENDIF
                ENDIF
                YN(NSL,N) = SRC(3,1,NS)
              ENDIF
!
!---        Advection-dominated solute release model  ---
!
            ELSEIF( ISRT(NS).EQ.-(NSL+4*NSOLU) .AND. NSL.LE.NSOLU ) THEN
              SRCIC(NSL,N) = SRCIC(NSL,N) + CNL(NSL,N)*DT
!
!---        Diffusion-dominated solute release model  ---
!
            ELSEIF( (ISRT(NS).EQ.-(NSL+5*NSOLU) .OR. &
            ISRT(NS).EQ.-(NSL+8*NSOLU)) .AND. NSL.LE.NSOLU ) THEN
              SRCIC(NSL,N) = SRCIC(NSL,N) + CNL(NSL,N)*DT
!
!---        Solubility-controlled solute release model or
!           solubility-controlled salt-cake release model  ---
!
            ELSEIF( (ISRT(NS).EQ.-(NSL+6*NSOLU) .OR. &
            ISRT(NS).EQ.-(NSL+7*NSOLU)) .AND. NSL.LE.NSOLU ) THEN
!
!---          SRX(2): nodal solute inventory  ---
!
              IF ( CNL(NSL,N).GT.ZERO ) THEN
!
!---            Integrated solute mass has exceeded solute inventory
!               and is being truncated to solute inventory  ---
!
                SRCIC(NSL,N) = SRCIC(NSL,N) + CNL(NSL,N)*DT
              ELSEIF ( SRCIC(NSL,N).LT.SRX(2) ) THEN
!
!---            Integrated solute mass still below solute inventory  ---
!
                DCDT =  (C(NSL,N)-CO(NSL,N))*VOL(N)*DTI
                CLX = ZERO
                C_FX = 0.D+0
                DO IFCX = 1,6
                  ICNX = ND2CNX(IFCX,N)
                  IF(ICNX > 0) THEN
                    AREAXX(IFCX) = AREAC(ICNX)
                    C_FX(IFCX) = C_FLUX(NSL,ICNX)
                  ELSEIF(ICNX < 0) THEN
                    ICNX = ABS(ICNX)
                    IF(ICNX <= NUM_BCNX) THEN
                      AREAXX(IFCX) = AREAB(ICNX)
                      C_FX(IFCX)= C_FLUX_B(NSL,ICNX)
                    ENDIF
                  ELSE
                    C_FX(IFCX) = 0.D0
                    AREAXX(IFCX) = 0.D0
                  ENDIF
                ENDDO
                CLX = CLX + C_FX(1)*AREAXX(1)
                CLX = CLX - C_FX(2)*AREAXX(2)
                CLX = CLX + C_FX(3)*AREAXX(3)
                CLX = CLX - C_FX(4)*AREAXX(4)
                CLX = CLX + C_FX(5)*AREAXX(5)
                CLX = CLX - C_FX(6)*AREAXX(6)
                SRCIC(NSL,N) = SRCIC(NSL,N) + (DCDT-CLX)*DT
              ENDIF
            ENDIF
  500     CONTINUE
  600 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SORIT1 group  ---
!
      RETURN
      END
