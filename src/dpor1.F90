
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DPOR1
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
!     Dual porosity model.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, February, 1997.
!     Last Modified by MD White, Battelle, PNL, February 9, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOURC
      USE SOLTN
      USE REACT
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 PLX(2),SLX(2),RKLX(3,2),PORDX(2),PORTX(2), &
      RHOLX(2),VISLX(2)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/DPOR1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      ASLX = 0.D+0
      ASLMINX = 0.D+0
      ASGTX = 0.D+0
      SGRMX = 0.D+0
      INDX = 1
      DO 1000 N = 1,num_nodes
        IF( IXP(N).LE.0 ) GOTO 1000
        IZN = N
        IF( IDP(IZN).EQ.0 ) GOTO 1000
        IDP(IZN) = 2*IDP(IZN)
        PLX(1) = PN(1,N)
        PLX(2) = PN(1,N) + DNR(IEQW,N)

        POR(5,IZN) = POR_M(1,N)
        POR(6,IZN) = POR_M(2,N)

!
!---  Compute matrix properties  ----
!
        DO 100 M = 1,2
          CALL KSP1( N,IZN,M,PG(2,N),PLX(M),SLX(M),RKLX(1,M), &
          ASLX,ASLMINX,ASGTX,SGRMX,INDX,IPH(2,N),PG(1,N), &
          PL(1,N),SL(1,N) )
          PX = MAX( PG(2,N),PLX(M) )+PATM
          CALL PORSTY(IZN,PX,PCMP(N),PORDX(M),PORTX(M))
          PX = MAX( PLX(M)+PATM,PG(2,N)+PATM,PVW(2,N) )
          CALL WATLQD( T(2,N),PX,RHOLX(M) )
          CALL WATLQV( T(2,N),PX,PSW(2,N),VISLX(M) )
          PERMX = (PERM(4,IZN)*PERM(5,IZN)*PERM(6,IZN))**(1./3.)
  100   CONTINUE
        STOR = (PORDX(2)*SLX(2)*RHOLX(2)-PORDX(1)*SLX(1)*RHOLX(1))/ &
        DNR(IEQW,N)
        RKLMX = (RKLX(1,1)*RKLX(2,1)*RKLX(3,1))**(1./3.)
        IF( IDP(IZN).EQ.2 ) THEN
          ALPHA = RHOLX(1)*RKLMX*PERMX/STOR/VISLX(1)
          THETA = ALPHA*DT/(4.D+0*CHML(IZN)**2)
        ELSE
          ALPHA = 4.D+0/(CHML(IZN)**2)
          PHI = STOR*DTI*VISLX(1)/(ALPHA*RHOLX(1)*RKLMX*PERMX+SMALL)
        ENDIF
        DO 300 M = 2,ISVC+2
          IF( IDP(IZN).EQ.2 ) THEN
            YX = 0.D+0
            DO 200 I = 1,499,2
              REALX = REAL(I)
              BETA = GPI*REALX
              YX = YX + (8.D+0/(BETA**2))*EXP(-(BETA**2)*THETA)
  200       CONTINUE
            YX = MIN( YX,1.D+0 )
            PN(M,N) = (PN(1,N)-PL(M,N))*YX + PL(M,N)
          ELSE
            PN(M,N) = PN(1,N)
            IF( ABS(PHI).GT.EPSL ) PN(M,N) = (2.D+0*PL(M,N) - &
            PN(1,N)*(1.D+0-2.D+0*PHI))/(1.D+0 + 2.D+0*PHI)
!            IF( PN(1,N).GE.PL(M,N) ) PN(M,N) = MAX( PL(M,N),PN(M,N) )
!            IF( PN(1,N).LE.PL(M,N) ) PN(M,N) = MIN( PL(M,N),PN(M,N) )
          ENDIF
          SRCW(M,N) = SRCW(M,N) + (PN(1,N)-PN(M,N))*DTI*STOR* &
         (1.D+0-PORD(M,N))*VOL(N)
  300   CONTINUE
        IDP(IZN) = IDP(IZN)/2
 1000 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of DPOR1 group  ---
!
      RETURN
      END
