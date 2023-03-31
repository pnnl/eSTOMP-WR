!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE SORIC1
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
!     Compute coupled-equation source integrals.
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 12 May 2009
!     Last Modified by M.D. White, PNNL, 12 May 2009
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE TRNSPT
  USE SOURC
  USE SOLTN
  USE GRID
  USE coup_well
  USE FILES
  USE CONST
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
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 SRX(8+LSOLU)
  integer:: i,n2cw
!
!----------------------Executable Lines--------------------------------!
!
!  ISUB_LOG = ISUB_LOG+1
!  SUB_LOG(ISUB_LOG) = '/SORIC33'
  IF( INDEX(CVS_ID(213)(1:1),'$').EQ.0 ) CVS_ID(213) = &
   '$Id: stomp33.F,v 1.34 2009/05/15 14:59:10 d3c002 Exp $'
!
!---  Integrate coupled-well rates  ---
!
      me=ga_nodeid()
      IF( N_CW > 0 ) THEN
      g_qm_cw = 0.d0
        DO 10 NCW = 1,N_L_CW
          ngcwx = id_cw(7,ncw)
          QM_CW(2,NCW) = QM_CW(2,NCW) + QM_CW(1,NCW)*DT
          QM_CW(4,NCW) = QM_CW(4,NCW) + QM_CW(3,NCW)*DT
 !         QM_CW(6,NCW) = QM_CW(6,NCW) + QM_CW(5,NCW)*DT
          g_qm_cw(1:6,ngcwx) = qm_cw(1:6,ncw)
   10   CONTINUE
      ncw6=n_cw*6
      call ga_dgop(1,g_qm_cw(1,1),ncw6,'+')
      if (write_well_flux(1) == 1) then
        q_total_cw(:) = g_qm_cw(2,:)
        if (me==0) then
          n2cw = n_cw*2
          write(IWAF,'(1PE22.15,1X,<n2cw>(1PE22.15,1X))')&
                TM, (q_flux_cw(i),q_total_cw(i),i=1,n_cw)  
        endif
      endif
      ENDIF
!
!---  Loop over sources  ---
!
!  DO 900 NS = 1,NSR
!    IF( TM.LE.SRC(1,1,NS) ) GOTO 900
!    SRX(1) = TM
!    IF( ISRM(NS).EQ.1 ) THEN
!      DO 70 N = 1,8+NSOLU
!        SRX(N) = SRC(N,1,NS)
!     70     CONTINUE
!    ELSE
!      DO 100 M = 2,ISRM(NS)
!        IF( TM.LE.SRC(1,M,NS) ) THEN
!         DTSR = MIN( SRC(1,M,NS)-TM,DT )
!         TFSR = (TM-0.5D+0*DTSR-SRC(1,M-1,NS))/ &
!           (SRC(1,M,NS)-SRC(1,M-1,NS))
!         DO 80 N = 1,8+NSOLU
!           SRX(N) = SRC(N,M-1,NS) + TFSR*(SRC(N,M,NS)-SRC(N,M-1,NS))
!     80        CONTINUE
!         GOTO 110
!        ENDIF
!    100     CONTINUE
!      GOTO 900
!    ENDIF
!    110   CONTINUE
!
!---    Integrate total CO2 and water mass injected  ---
!
!    IF( MOD(ISRT(NS),100).GE.25 .AND.  &
!      MOD(ISRT(NS),100).LE.27  ) THEN
!      DO 600 NC = 1,ISRDM(1,NS)
!        N = IWSI(NC,NS)
!        IF( IXP(N).LE.0 ) GOTO 600
 !       SRCP(1,NS) = SRCP(1,NS) + SWSI(1,NC,NS)*SWSI(2,NC,NS)*DT
!        SRCP(2,NS) = SRCP(2,NS) + SWSI(1,NC,NS)*SWSI(3,NC,NS)*DT
!    600     CONTINUE
!    ENDIF
!    900 CONTINUE
!  ISUB_LOG = ISUB_LOG-1
!
!---  End of SORIC33 group  ---
!
  RETURN
  END
