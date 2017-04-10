

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SMC1
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
!     Control saturation, relative permeability, porosity, and
!     tortuosity calculations
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
      USE REACT
      USE PORMED
      USE JACOB
      USE HYST
      USE GRID
      USE FDVP
!      USE FDVD
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
      SUBNMX = '/SMC1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      INDX = 1
      DO 200 N = 1,num_nodes
        IF( IXP(N).LE.0 ) GOTO 200
        IZN = N
        if(islc(40).eq.1) then
         POR(5,IZN) = POR_M(1,N)
         POR(6,IZN) = POR_M(2,N)
        endif

        DO 100 M = 2,ISVC+2
          SGRMX = SCHR(15,IZN)/(1.D+0+TRPGL(2,N)/SCHR(9,IZN))
          ASLMINX = ASLMIN(1,N)
          CALL KSP1( N,IZN,M,PG(M,N),PL(M,N),SL(M,N),RKL(1,M,N), &
          ASLX,ASLMINX,ASGTX,SGRMX,INDX,IPH(2,N), &
          PG(1,N),PL(1,N),SL(1,N) )
          IF( M.EQ.2 ) THEN
            ASL(N) = ASLX
            ASGT(N) = ASGTX
            ASLMIN(2,N) = ASLMINX
          ENDIF
          SGT(M,N) = ASGTX*(1.D+0-SCHR(4,IZN))
          SG(M,N) = MAX( 1.D+0-SL(M,N),ZERO )
          PX = MAX( PG(M,N),PL(M,N) )+PATM
          CALL PORSTY(IZN,PX,PCMP(N),PORD(M,N),PORT(M,N))
          IF( ISLC(3).EQ.1 ) CALL TORTU( IZN,SL(M,N),SG(M,N),ZERO, &
          PORD(M,N),TORL(M,N),TORGX,TORNX )
!
!---      Kozeny-Carman ---
!
          permrf(m,n) = 1.d0
          IF( IPRF(IZN).EQ.2 ) CALL PERM_I( PERMRF(M,N),PORD(M,N),IZN)
  100   CONTINUE
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SMC1 group  ---
!
      RETURN
      END
