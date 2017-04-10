!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LDO1
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
!     Load the current time step values into the old time step
!     variables.
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
      USE REACT
      USE HYST
      USE GRID
      USE FDVSO
      USE FDVP
!      USE FDVD
      USE BCVP
      USE BCV
      use grid_mod
      use pormed
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
      SUBNMX = '/LDO1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Second-order time differencing  ---
!
      IF( ISLC(11).NE.0 ) THEN
        M = 1
        IF( NSTEP.EQ.0 ) M = 2
        DO 10 N = 1,num_nodes
          IF( IXP(N).EQ.0 ) GOTO 10
          PORD_O(N) = PORD(M,N)
          SL_O(N) = SL(M,N)
          XLW_O(N) = XLW(M,N)
          RHOL_O(N) = RHOL(M,N)
  10   CONTINUE
      ENDIF
!
!---  Assign old time step values  ---
!
      DO 100 N = 1,num_nodes
        PG(1,N) = PG(2,N)
        T(1,N) = T(2,N)
        PL(1,N) = PL(2,N)
        PORD(1,N) = PORD(2,N)
        PORT(1,N) = PORT(2,N)
        SL(1,N) = SL(2,N)
        SG(1,N) = SG(2,N)
        PVW(1,N) = PVW(2,N)
        XLW(1,N) = XLW(2,N)
        RHOL(1,N) = RHOL(2,N)
        VISL(1,N) = VISL(2,N)
        TORL(1,N) = TORL(2,N)
        RKL(1,1,N) = RKL(1,2,N)
        RKL(2,1,N) = RKL(2,2,N)
        RKL(3,1,N) = RKL(3,2,N)
        TRPGL(1,N) = TRPGL(2,N)
        ASLMIN(1,N) = MIN( ASL(N),ASLMIN(2,N) )
        ASLMIN(2,N) = ASLMIN(1,N)
        NPHAZ(1,N) = NPHAZ(2,N)
        IPH(1,N) = IPH(2,N)
        DO 90 NSL = 1,NSOLU
          CO(NSL,N) = C(NSL,N)
   90   CONTINUE
       
        DO 92 NEQ = 1,NEQC+NEQK
          NSL = NEQ + NSOLU 
          CO(NSL,N) = C(NSL,N)
   92   CONTINUE
        DO 94 NSP = 1,NSPR
          SP_CO(NSP,N) = SP_C(NSP,N)
   94   CONTINUE

  100 CONTINUE
!      DO 105 NS = 1,NSR
!        QLW(1,NS) = QLW(1,NS) + QLW(3,NS)
!  105 CONTINUE
      DO 107 NB = 1,num_bcnx
        PLB(1,NB) = PLB(2,NB)
        PGB(1,NB) = PGB(2,NB)
        SLB(1,NB) = SLB(2,NB)
  107 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of LDO1 group
!
      RETURN
      END
