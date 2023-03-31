!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TMSTEP
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
!     Compute the time step based on the previous time step,
!     the acceleration factor, the maximum time step, the time until
!     the next print, and the time until the next start of a
!     boundary condition.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, February, 1993.
!     Last Modified by MD White, Battelle, PNL, April 14, 1994.
!     Last Modified by MD White, PNNL, 13 August 2003.




!     $Id: tmstep.F,v 1.9 2005/07/19 17:53:45 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE UCODE
      USE SOURC
      USE SOLTN
      USE OUTPU
      USE CONST
      USE BCV
      use grid_mod
      use trnspt
      use coup_well
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
      LOGICAL :: use_ga
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/TMSTEP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(243)(1:1),'$').EQ.0 ) CVS_ID(243) = &
      '$Id: tmstep.F,v 1.9 2005/07/19 17:53:45 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Loop over execution periods to determine the current execution
!     period  ---
!
      NP = 0
      TMPSX = -1.D+20
      TMTOL = 1.D-9
      TMZ = TM
      IF( MEPD.EQ.1 ) TMZ = MOD(TM,TMPE(NEPD))
      DO 10 N = 1,NEPD
        IF( (TMZ-TMPS(N))/(ABS(TMZ)+EPSL).GT.-EPSL .AND. &
        (TMPE(N)-TMZ)/(ABS(TMZ)+EPSL).GT.-EPSL ) THEN
          IF( TMPS(N).GT.TMPSX ) THEN
            TMPSX = TMPS(N)
            NP = N
          ENDIF
        ENDIF
   10 CONTINUE
      IF( NP.NE.0 ) IEPD = NP
      DTAF = TMPA(IEPD)
      DTCF = TMPC(IEPD)
      DTMX = TMPX(IEPD)
      NRIMX = NRIM(IEPD)
      RSDMX = RSDM(IEPD)
      if(tcrntmxt(iepd) > 0.d0) then
        crntmxt = tcrntmxt(iepd)
      endif
!
!---  Assign the initial time step  ---
!
      IF( ABS((TMZ-TMPS(IEPD))/(ABS(TMZ)+EPSL)).LE.EPSL ) THEN
        DT = TMPD(IEPD)/DTAF
        DTO = DT
      ENDIF
      DTNX = MAX( DTO,DT*DTAF )
      DTQU = TMMX - TM
!
!---  Loop over print times
!     compute time step to next print time  ---
!
      DTPR = BIG
      DO 110 N = 1,NPRTM
        IF( PRTM(N).GT.TM ) DTPR = MIN( DTPR,PRTM(N)-TM )
  110 CONTINUE
      TMPR = TM + DTPR
!
!---  Loop over boundary conditions
!     compute time step to next boundary condition transition  ---
!
      DTBC = BIG
!      DO 130 N = 1,num_bcnx
      do 130 mb = 1,nb_t
        TMZ = TM
!        MB = IBCIN(N)
        IF( IBCC_T(mb).EQ.1 ) TMZ = MOD(TM,BC_T(IBCM_T(mb),mb))
        DO 120 M = 1,IBCM_T(MB)
          DTBCX = DTBC
          IF( BC_T(M,mb)-TMZ.GT.TMTOL )  &
          DTBC = MIN( DTBC,BC_T(M,mb)-TMZ )
          IF( (TM+DTBC).EQ.TM ) DTBC = DTBCX
  120   CONTINUE
  130 CONTINUE
!      call ga_dgop(1,dtbc,1,'min')
!
!---  Loop over sources,
!     compute time step to next source transition  ---
!
      DTSR = BIG
      DO 150 N = 1,NSR
        DO 140 M = 1,ISRM(N)
          IF( SRC(1,M,N).GT.TM ) DTSR = MIN( DTSR,SRC(1,M,N)-TM )
  140   CONTINUE
  150 CONTINUE
!
!---  Loop over observed data,
!     compute time step to next observed data time  ---
!
      DTOB = BIG
      IF( ISLC(20).EQ.1 ) THEN
        DO 160 NT = 1,NOBDT
        DO 160 NS = 1,NOBDS(NT)
          IF( R_OBDS(2,NS,NT).GT.TM ) &
           DTOB = MIN( DTOB,R_OBDS(2,NS,NT)-TM )
  160   CONTINUE
        TMOB = TM + DTOB
      ENDIF
!
!---  Loop over execution periods,
!     compute time step to next execution period start  ---
!
      DTEP = BIG
      TMZ = TM
      IF( MEPD.EQ.1 ) TMZ = MOD(TM,TMPS(NEPD))
      DO 170 N = 1,NEPD
        IF( TMPS(N).GT.TMZ ) DTEP = MIN( DTEP,TMPS(N)-TMZ )
  170 CONTINUE
!
!---  Loop over well times
!     compute time step to next well transition  ---
!
!      DTWL = BIG
!      IF( LWELL.EQ.1 ) THEN
!        DO 190 NWL = 1,NWLS
!          TMZ = TM
!          IF( IWCC(NWL).EQ.1 ) TMZ = MOD(TM,WLVR(1,IWM(NWL),NWL))
!          DO 180 M = 1,IWM(NWL)
!            DTWLX = DTWL
!            IF( WLVR(1,M,NWL).GT.TMZ )
!     &        DTWL = MIN( DTWL,WLVR(1,M,NWL)-TMZ )
!            IF( (TM+DTWL).EQ.TM ) DTWL = DTWLX
!  180     CONTINUE
!  190   CONTINUE
!      ENDIF
!*************Coupled well - Bryan******************************
!---  Loop over coupled-well times
!     compute time step to next coupled-well transition  ---
!
       DT_CW = BIG
       DO 240 NCW = 1,N_CW
         TMZ = TM
         IF( ICC_CW(NCW).EQ.1 ) TMZ = MOD( TM,VAR_CW(1,IM_CW(NCW),NCW) )
         DO 230 M = 2,IM_CW(NCW)
           DT_CWX = DT_CW
           IF( VAR_CW(1,M,NCW)-TMZ.GT.TMTOL ) &
            DT_CW = MIN( DT_CW,VAR_CW(1,M,NCW)-TMZ )
           IF( (TM+DT_CW).EQ.TM ) DT_CW = DT_CWX
  230   CONTINUE
  240 CONTINUE
!******************************************************************
!
!---  Compute the next step based on the minimum of
!     the incremented time step, the time step to quit,
!     the time step to print, the time step to a boundary condition
!     transition, the time step to a source transitions,
!     the time step to an execution period start,
!     the time step to a well time,





!     or the maximum time step  ---
!

      DT = MIN( DTNX,DTQU,DTPR,DTBC,DTSR,DTOB,DTEP,DTMX,DT_CW )
!      DT = MIN( DTNX,DTQU,DTPR,DTBC,DTSR,DTOB,DTEP,DTMX )

      IF( LWELL.EQ.1 ) DT = MIN( DT,DTWL )
      DTI = 1.D+0/(DT+SMALL)
      IF( ABS(DT-DTNX)/(ABS(DT)+EPSL).LE.EPSL .OR. &
      ABS(DT-DTMX)/(ABS(DT)+EPSL).LE.EPSL ) DTO = DT
      TM = TM + DT
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of TMSTEP group
!
      RETURN
      END
