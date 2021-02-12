

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CISC1
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
!     Compute initial solute concentrations.
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
      USE BCV
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

      LOGICAL :: use_ga

!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      IF( IEQC.EQ.0 .AND. ISLC(40).EQ.0 ) RETURN
      SUBNMX = '/CISC1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      DO 110 N = 1,num_nodes
        IF( IXP(N).LE.0 ) GOTO 110
        IZN = N
        XVL = SL(2,N)*PORD(2,N)
        DO 140 NSL = 1,NSOLU
          IF( IPCL(NSL).EQ.4 ) THEN
            NS = IPCSL(NSL,IZN)
            XVS = SL(2,N)*RHOS(IZN)*PCSL(1,NS,IZN)*(1.D+0-PORT(2,N))
            CLX = C(NS,n)/(XVS+XVL)
            IF( CLX.LT.SMALL ) THEN
              PCSLX = PCSL(1,NSL,IZN)
            ELSE
             PCSLX = 1.D+1**(PCSL(2,NSL,IZN)+PCSL(3,NSL,IZN)*LOG10(CLX))
            ENDIF
            XVS = SL(2,N)*RHOS(IZN)*PCSLX*(1.D+0-PORT(2,N))
          ELSEIF( IPCL(NSL).EQ.3 ) THEN
            NS = IPCSL(NSL,IZN)
            XVS = SL(2,N)*RHOS(IZN)*PCSL(1,NS,IZN)*(1.D+0-PORT(2,N))
            CLX = C(NS,n)/(XVS+XVL)
            IF( CLX.LT.SMALL ) THEN
              PCSLX = PCSL(1,NSL,IZN)
            ELSE
             PCSLX = 1.D+1**(PCSL(2,NSL,IZN)+PCSL(3,NSL,IZN)*LOG10(CLX))
            ENDIF
            XVS = RHOS(IZN)*PCSLX*(1.D+0-PORT(2,N))
          ELSEIF( IPCL(NSL).EQ.2 ) THEN
            XVS = SL(2,N)*RHOS(IZN)*PCSL(1,NSL,IZN)*(1.D+0-PORT(2,N))
          ELSE
            XVS = RHOS(IZN)*PCSL(1,NSL,IZN)*(1.D+0-PORT(2,N))
          ENDIF
          IF( (XVL+XVS)/EPSL.LT.EPSL ) THEN
            YL(NSL,n) = 0.D+0
          ELSE
            YL(NSL,n) = XVL/(XVL+XVS)
          ENDIF
!
!---  Phase-volumetric concentration ratios  ---
!
          IF( ICT(NSL,n).EQ.2 ) THEN
            C(NSL,n) = C(NSL,n)*(XVS + XVL)
          ELSEIF( ICT(NSL,n).EQ.3 ) THEN
            C(NSL,n) = C(NSL,n)*XVL/YL(NSL,n)
          ELSEIF( ICT(NSL,n).EQ.-1 ) THEN
            C(NSL,n) = C(NSL,n)*RHOS(IZN)*(1.D+0-PORT(2,N))
          ELSEIF( ICT(NSL,n).EQ.-2 ) THEN
            C(NSL,n) = C(NSL,n)*(XVS+XVL)*RHOS(IZN)*(1.D+0-PORT(2,N))
          ENDIF
  140   CONTINUE
  110 CONTINUE
!
!---  Assign boundary solute concentrations for initial condition
!     type boundary conditions  ---
!
      do nsl=1,nsolu
        DO 130 NB = 1,num_bcnx
          N = IBCN(NB)
          CBO(nb,nsl) = C(NSL,n)
  130   CONTINUE
      enddo
!
!---  Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration  ---
!
      IF( ISLC(16).EQ.1 ) THEN
        DO 200 N = 1,num_nodes
          CLX = C(NSL_ELC,n)*YL(NSL_ELC,n)/(SL(2,N)*PORD(2,N)+EPSL)
          XLW(2,N) = RHOL(2,N)
          CALL ELC_DEN( RHOL(2,N),CLX,ELC_DCF )
          XLW(2,N) = XLW(2,N)/RHOL(2,N)
          CALL ELC_VIS( VISL(2,N),CLX,ELC_VCF )
  200   CONTINUE
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of CISC1 group  ---
!
      RETURN
      END
