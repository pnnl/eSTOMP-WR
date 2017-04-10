

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SORC1
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
!     Compute source terms.
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
      USE PORMED
      USE JACOB
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
!----------------------Type Declarations-------------------------------!
!



      REAL*8 SRX(8+LSOLU)
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/SORC1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
        '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Zero source terms  ---
!
      IF( NSR.GT.0 ) THEN
        DO 50 N = 1,num_nodes
          DO 40 M = 2,ISVC+2
              SRCW(M,N) = 0.D+0
   40     CONTINUE
   50   CONTINUE
      ENDIF
!
!---  Loop over sources  ---
!
      DO 700 NS = 1,NSR
!
!---  Check source time  ---
!
        IF( TM.LE.SRC(1,1,NS) ) GOTO 700
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
!
!---  Stair step the slug and pulse well sources  ---
!
              IF( ISRT(NS).GE.24 .AND. ISRT(NS).LE.27 ) THEN
                DO 82 N = 2,8+NSOLU
                  SRX(N) = SRC(N,M-1,NS)
   82           CONTINUE
              ENDIF
              GOTO 110
            ENDIF
  100     CONTINUE
          GOTO 700
        ENDIF
  110   CONTINUE



!
!---  Loop source domain  ---
!
          node_s = ubound(isrdm,DIM=1)
          DO 500 I = 1,node_s
            N = isrdm(i,ns)
!print *,'nodes_s',me,node_s,n,ns
            if(n.eq.0) cycle
            IZN = N
            IF( IXP(N).EQ.0 ) GOTO 500
            DO 400 M = 2,ISVC+2
!
!---          Aqueous volumetric source  ---
!
              IF( ISRT(NS).EQ.2 ) THEN
                SRCW(M,N) = SRCW(M,N) + SRX(4)*RHOL(M,N)
!
!---          Aqueous volumetric density source  ---
!
              ELSEIF( ISRT(NS).EQ.3 ) THEN
                SRCW(M,N) = SRCW(M,N) + SRX(4)*RHOL(M,N)*VOL(N)
!
!---          Aqueous mass source  ---
!
              ELSEIF( ISRT(NS).EQ.4 ) THEN
                SRCW(M,N) = SRCW(M,N) + SRX(4)
!
!---          Aqueous mass density source  ---
!
              ELSEIF( ISRT(NS).EQ.5 ) THEN
                SRCW(M,N) = SRCW(M,N) + SRX(4)*VOL(N)
!
!---          Z-direction vertical injection well  ---
!
              ELSEIF( ISRT(NS).EQ.31 ) THEN
!
!---            Geometric factors  ---
!
                RDW = SRX(3)
                icnx = nd2cnx(5,n)
                RDE = SQRT( areac(icnx)/GPI/SRX(4) )
!                ACWX = 2.D+0*GPI*RDW*DZGF(N)
                DRD2 = (RDE**2-RDW**2)
!
!---            Well pressure  ---
!
                IF( M.EQ.2 ) THEN
                  IF( K.EQ.ISRDM(5,NS) ) THEN
                    PLWX = SRX(2)
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                  ELSE
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                    n = n-1
                    ld1 = iaxmax-iaxmin+1
                    ld2 = iaymax-iaymin+1
                    NX = n-ld1*ld2
                    GB = (ZP(N)-ZP(NX))*GRAVZ
                    PLWX = PLWX - RHOLX*GB
                  ENDIF
                ENDIF
                IF( (PERM(1,IZN)/EPSL).GT.EPSL ) THEN
                  IF( (PERM(2,IZN)/EPSL).GT.EPSL ) THEN
                    PERMX = SQRT( PERM(1,IZN)*PERM(2,IZN) )
                  ELSE
                    PERMX = PERM(1,IZN)
                  ENDIF
                ELSE
                  PERMX = PERM(2,IZN)
                ENDIF
                HCLX = 2.D+0*GPI*PERMX*DRD2*DZGF(N)/ &
                (VISL(M,N)*((RDE**2)*LOG(RDE/RDW)-5.D-1*DRD2))
!
!---            Injection  ---
!
                IF( PLWX-PL(M,N).GT.EPSL ) THEN
                  QLX = SRX(4)*(PLWX-PL(M,N))*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOLX
!
!---              Withdrawl  ---
!
                ELSE
                  RKLX = SQRT(RKL(1,M,N)*RKL(2,M,N))
                  QLX = SRX(4)*(PLWX-PL(M,N))*RKLX*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOL(M,N)
                ENDIF
!
!---          X-direction horizontal injection well  ---
!
              ELSEIF( ISRT(NS).EQ.32 ) THEN
!
!---            Geometric factors  ---
!
                RDW = SRX(3)
                icnx = nd2cnx(1,n)
                RDE = SQRT( areac(icnx)/GPI/SRX(4) )
!                ACWX = 2.D+0*GPI*RDW*DXGF(N)
                DRD2 = (RDE**2-RDW**2)
!
!---            Well pressure  ---
!
                IF( M.EQ.2 ) THEN
                  IF( I.EQ.ISRDM(1,NS) ) THEN
                    PLWX = SRX(2)
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                  ELSE
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                    nx = n - 1
!                    NX = ND(I-1,J,K)
                    GB = (XP(N)-XP(NX))*GRAVX
                    PLWX = PLWX - RHOLX*GB
                  ENDIF
                ENDIF
                IF( (PERM(2,IZN)/EPSL).GT.EPSL ) THEN
                  IF( (PERM(3,IZN)/EPSL).GT.EPSL ) THEN
                    PERMX = SQRT( PERM(2,IZN)*PERM(3,IZN) )
                  ELSE
                    PERMX = PERM(2,IZN)
                  ENDIF
                ELSE
                  PERMX = PERM(3,IZN)
                ENDIF
                HCLX = 2.D+0*GPI*PERMX*DRD2*DXGF(N)/ &
                (VISL(M,N)*((RDE**2)*LOG(RDE/RDW)-5.D-1*DRD2))
!
!---            Injection  ---
!
                IF( PLWX-PL(M,N).GT.EPSL ) THEN
                  QLX = SRX(4)*(PLWX-PL(M,N))*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOLX
!
!---              Withdrawl  ---
!
                ELSE
                  RKLX = SQRT(RKL(2,M,N)*RKL(3,M,N))
                  QLX = SRX(4)*(PLWX-PL(M,N))*RKLX*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOL(M,N)
                ENDIF
!
!---          Y-direction horizontal injection well  ---
!
              ELSEIF( ISRT(NS).EQ.33 ) THEN
!
!---            Geometric factors  ---
!
                RDW = SRX(3)
                icnx = nd2cnx(3,n)
                RDE = SQRT( areac(icnx)/GPI/SRX(4) )
!                ACWX = 2.D+0*GPI*RDW*DYGF(N)
                DRD2 = (RDE**2-RDW**2)
!
!---            Well pressure  ---
!
                IF( M.EQ.2 ) THEN
                  IF( J.EQ.ISRDM(3,NS) ) THEN
                    PLWX = SRX(2)
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                  ELSE
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
!                    NX = ND(I,J-1,K)
                    nx = n - (iaxmax-iaxmin+1)
                    GB = (YP(N)-YP(NX))*RP(I)*GRAVY
                    PLWX = PLWX - RHOLX*GB
                  ENDIF
                ENDIF
                IF( (PERM(1,IZN)/EPSL).GT.EPSL ) THEN
                  IF( (PERM(3,IZN)/EPSL).GT.EPSL ) THEN
                    PERMX = SQRT( PERM(1,IZN)*PERM(3,IZN) )
                  ELSE
                    PERMX = PERM(1,IZN)
                  ENDIF
                ELSE
                  PERMX = PERM(3,IZN)
                ENDIF
                HCLX = 2.D+0*GPI*PERMX*DRD2*DYGF(N)/ &
                (VISL(M,N)*((RDE**2)*LOG(RDE/RDW)-5.D-1*DRD2))
!
!---            Injection  ---
!
                IF( PLWX-PL(M,N).GT.EPSL ) THEN
                  QLX = SRX(4)*(PLWX-PL(M,N))*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOLX
!
!---            Withdrawl  ---
!
                ELSE
                  RKLX = SQRT(RKL(1,M,N)*RKL(3,M,N))
                  QLX = SRX(4)*(PLWX-PL(M,N))*RKLX*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOL(M,N)
                ENDIF
              ENDIF
  400       CONTINUE
  500     CONTINUE
  700 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SORC1 group  ---
!
      RETURN
      END
