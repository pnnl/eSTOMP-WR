

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE INCRM1
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
!     Compute primary variable increments
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE JACOB
      USE HYST
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
      SUBNMX = '/INCRM1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
        '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!--- Determine wetting/drying direction on the first
!    iteration  ---
!
      IF( NITER.EQ.1 ) THEN
        DO 10 N = 1,num_nodes
          IF( IXP(N).LE.0 ) GOTO 10
!
!---      Draining  ---
!
          IF( (PG(2,N)-PL(2,N)).GT.(PG(1,N)-PL(1,N)) ) THEN
              IPH(2,N) = -1
!
!---        Reset reversal point  ---
!
            IF( IPH(1,N).EQ.1 ) THEN
              ASLMIN(2,N) = SL(1,N)
              ASTMIN(2,N) = BTGL(1,N)*(PG(1,N)-PL(1,N))/RHORL/GRAV
            ENDIF
!
!---      Wetting  ---
!
          ELSE
            IPH(2,N) = 1
!
!---        Reset reversal point  ---
!
            IF( IPH(1,N).EQ.-1 ) THEN
              ASLMIN(2,N) = SL(1,N)
              ASTMIN(2,N) = BTGL(1,N)*(PG(1,N)-PL(1,N))/RHORL/GRAV
            ENDIF
          ENDIF
   10   CONTINUE
      ENDIF
!
!--- Compute increments  ---
!
      DO 200 N = 1,num_nodes
        IF( IXP(N).LE.0 ) GOTO 200
        IZN = N
        IF( NITER.EQ.1 ) THEN
          IF( IPH(2,N).EQ.2 ) THEN
            IF( (PG(2,N)-PL(2,N))/RHORL/GRAV.LE.HCMWE(IZN) ) THEN
              IF( (PG(2,N)-PL(2,N)).GT.(PG(1,N)-PL(1,N)) ) THEN
                IPH(2,N) = -1
              ELSE
                IPH(2,N) = 1
              ENDIF
            ENDIF
          ELSE
            IF( (PG(2,N)-PL(2,N)).GT.(PG(1,N)-PL(1,N)) ) THEN
              IPH(2,N) = -1
            ELSE
              IPH(2,N) = 1
            ENDIF
          ENDIF
        ENDIF
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.2 ) THEN
!          IF( PG(2,N).GT.PL(2,N) ) THEN
!            CALL QSAT1( PL(2,N),PG(2,N),DPGLX,N )
!            DNR(IEQW,N) = -DPGLX
!          ELSE
            DNR(IEQW,N) = -MAX( 1.D-1,1.D-6*ABS(PG(2,N)-PL(2,N)) )
!          ENDIF
        ELSE
          DNR(IEQW,N) = -MAX( 1.D-1,1.D-6*ABS(PG(2,N)-PL(2,N)) )
        ENDIF
!
!--- Increment the primary variables  ---
!
        DO 100 M = 3,ISVC+2
          PL(M,N) = PL(2,N)
          IF( M.EQ.IEQW+2 ) THEN
            PL(M,N) = PL(M,N) + DNR(IEQW,N)
          ENDIF
  100   CONTINUE
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of INCRM1 group
!
      RETURN
      END
