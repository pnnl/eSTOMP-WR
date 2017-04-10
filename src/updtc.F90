!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE UPDTC(NSL)
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
!     Updates concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by ML Rockhold, Battelle, PNL, May 1993.
!     Last Modified by ML Rockhold, Battelle, PNL, April 14, 1994.
!     $Id: updtc.F,v 1.8 2006/01/09 19:27:01 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE JACOB
      USE GRID
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
      SUBNMX = '/UPDTC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(247)(1:1),'$').EQ.0 ) CVS_ID(247) = &
       '$Id: updtc.F,v 1.8 2006/01/09 19:27:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Put the B array into the A array, skipping masked nodes  ---
!
      DO 900 N = 1,num_nodes
        IF( IXP(N) .LE. 0 ) GOTO 900
        NMD = IXP(N)
        C(nsl,N) = BLU(1,n)
!        IF( ABS(C(Nsl,N)).LT.EPSL ) C(nsl,N) = 1.D-30
!mlr        IF( ABS(C(Nsl,N)).LT.EPSL ) C(nsl,N) = 0.D+0
        IF( ABS(C(Nsl,N)).LT.1.D-30 ) C(nsl,N) = 0.D+0
  900 CONTINUE
!
!---  End of UPDTC group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE UPDTCO(NSL)
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
!     Updates concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by ML Rockhold, Battelle, PNL, May 1993.
!     Last Modified by ML Rockhold, Battelle, PNL, April 14, 1994.
!     $Id: updtc.F,v 1.8 2006/01/09 19:27:01 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE GRID
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
      SUBNMX = '/UPDTCO'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(247)(1:1),'$').EQ.0 ) CVS_ID(247) = &
       '$Id: updtc.F,v 1.8 2006/01/09 19:27:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      DO 900 N = 1,num_nodes
        IF( IXP(N) .LE. 0 ) GOTO 900
        CO(nsl,N) = C(nsl,N)
  900 CONTINUE
!
!---  End of UPDTCO group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
