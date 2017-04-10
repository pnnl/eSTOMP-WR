!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CRNTNB
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
!     Compute the local maximum Courant numbers.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, October, 1995.
!     Last Modified by MD White, Battelle, PNL, October 10, 1995.
!     $Id: crntnb.F,v 1.12 2008/01/02 18:45:59 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE HYST
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      use grid_mod
      use bcv
      use bcvp
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
      SUBNMX = '/CRNTNB'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(27)(1:1),'$').EQ.0 ) CVS_ID(27) = &
       '$Id: crntnb.F,v 1.12 2008/01/02 18:45:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Aqueous Phase Courant Number  ---
!
      IF( IEQW.GT.0 ) THEN
        do n = 1,num_nodes
          crntl(n) = 0.d0
        enddo
        DO 100 icnx = 1,num_cnx
          id_up = conn_up(icnx)
          id_dn = conn_dn(icnx)
          if(ixp(id_up) <= 0.and.ixp(id_dn) <=0 ) cycle
          crntlx_up = crntl(id_up)
          crntlx_dn = crntl(id_dn)
          dist_upx = dist_up(icnx)
          dist_dnx = dist_dn(icnx)
          distx = dist_upx + dist_dnx
          qfluxx = abs(q_flux(1,icnx))
          crntl(id_up) = max(qfluxx*dt/distx,crntlx_up)
          crntl(id_dn) = max(qfluxx*dt/distx,crntlx_dn)
  100   CONTINUE
        do icnx = 1,num_bcnx
          n = ibcn(icnx)
          crntlx = crntl(n)
!          crntl(n) = max(abs(q_flux_b(1,icnx))*dt/distb(icnx)/2.d0,crntlx)
          crntl(n) = max(abs(q_flux_b(1,icnx))*dt/distb(icnx),crntlx)
        enddo
        do n=1,num_nodes
          IF( SL(2,N)*PORD(2,N).GT.EPSL ) THEN
            CRNTL(N) = CRNTL(N)/(SL(2,N)*PORD(2,N))
          ELSE
            CRNTL(N) = 0.D+0
          ENDIF
        enddo
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of CRNTNB group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CRN_LIM( NSL )
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
!     Compute a time step that globally satisfies the Courant
!     number limit.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, October, 2000.
!     Last Modified by MD White, Battelle, PNNL, October 17, 2000.
!     $Id: crntnb.F,v 1.12 2008/01/02 18:45:59 d3c002 Exp $




!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
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
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
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
      SUBNMX = '/CRN_LIM'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(27)(1:1),'$').EQ.0 ) CVS_ID(27) = &
       '$Id: crntnb.F,v 1.12 2008/01/02 18:45:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Compute the maximum Courant number ---
!
      CRNLX = 0.D+0
      CRNNX = 0.D+0
      CRNGX = 0.D+0
      IF( ISLC(17).EQ.1 ) THEN
        IF( IEQW.GT.0 ) THEN
          DO 100 N = 1,num_nodes
            IF( IXP(N).EQ.0 ) GOTO 100
            CRNLX = MAX( CRNTL(N),CRNLX )
  100     CONTINUE
        ENDIF
      ELSEIF( ISLC(17).EQ.2 ) THEN
        IF( IEQW.GT.0 ) THEN
          CCL_CRNX = CCL_CRN(NSL)
          DO 210 N = 1,NUM_NODES
            IF( IXP(N).EQ.0 ) GOTO 210
            IF( 1.D+0-SL(2,N).LT.EPSL ) GOTO 210
            CLX = C(NSL,N)*YL(NSL,N)/(SL(2,N)*PORD(2,N)+EPSL)
            IF( CLX.GT.CCL_CRNX ) CRNLX = MAX( CRNTL(N),CRNLX )
  210     CONTINUE
        ENDIF
      ENDIF
      CRNMX = MAX( CRNLX,CRNGX,CRNNX )
      call ga_dgop(1,crnmx,1,'max')
      DT_CRN = DT
      DTI_CRN = DTI
      TM_CRN = TM
      TM = TM-DT
      IF( CRNMX.GT.CRNTMXT ) THEN
        N_CRN(NSL) = INT(CRNMX/CRNTMXT) + 1
        REALX = REAL(N_CRN(NSL))
        DT = DT/REALX
        DTI = 1.D+0/(DT+EPSL)
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of CRN_LIM group  ---
!
      RETURN
      END
      




