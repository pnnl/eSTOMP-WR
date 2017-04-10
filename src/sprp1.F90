
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SPRP1( NSL )
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
!     Calculates the aqueous-phase solute
!     mole fractions from user-specified partition coefficients.
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
      USE FDVP
      USE CONST
      use grid_mod
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
      SUBNMX = '/SPRP1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
       '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Loop over all nodes  ---
!
      DO 100 N = 1,num_nodes
        IF( IXP(N).LE.0 ) cycle
        IZN = N
        XVL = SL(2,N)*PORD(2,N)
        IF( IPCL(NSL).EQ.4 ) THEN
          NS = IPCSL(NSL,IZN)
          XVS = SL(2,N)*RHOS(IZN)*PCSL(1,NS,IZN)*(1.D+0-PORT(2,N))
          CLX = C(NS,N)/(XVS+XVL)
          IF( CLX.LT.SMALL ) THEN
            PCSLX = PCSL(1,NSL,IZN)
          ELSE
            PCSLX = 1.D+1**(PCSL(2,NSL,IZN)+PCSL(3,NSL,IZN)*LOG10(CLX))
          ENDIF
          XVS = RHOS(IZN)*PCSLX*(1.D+0-PORT(2,N))*SL(2,N)
        ELSEIF( IPCL(NSL).EQ.3 ) THEN
          NS = IPCSL(NSL,IZN)
          XVS = SL(2,N)*RHOS(IZN)*PCSL(1,NS,IZN)*(1.D+0-PORT(2,N))
          CLX = C(NS,N)/(XVS+XVL)
          IF( CLX.LT.SMALL ) THEN
            PCSLX = PCSL(1,NSL,IZN)
          ELSE
            PCSLX = 1.D+1**(PCSL(2,NSL,IZN)+PCSL(3,NSL,IZN)*LOG10(CLX))
          ENDIF
          XVS = RHOS(IZN)*PCSLX*(1.D+0-PORT(2,N))
        ELSEIF( IPCL(NSL).EQ.2 ) THEN
          XVS = RHOS(IZN)*PCSL(1,NSL,IZN)*(1.D+0-PORT(2,N))*SL(2,N)
        ELSE
          XVS = RHOS(IZN)*PCSL(1,NSL,IZN)*(1.D+0-PORT(2,N))
        ENDIF
!
!---  Phase-volumetric concentration ratios  ---
!
        YVL = 1.D+0/(XVS + XVL)
!
!---  Phase mole fractions  ---
!
        YL(NSL,N) = XVL*YVL
!
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SPRP1 group  ---
!
      RETURN
      END
