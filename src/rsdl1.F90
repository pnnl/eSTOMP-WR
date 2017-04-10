

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RSDL1
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
!     Compute the maximum relative residuals
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.




!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE JACOB
      USE HYST
      USE GRID
      USE FILES
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
     real*8 :: rsdw(isvc)
     integer :: nsdw(isvc)
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      IF( ICNV.EQ.1 .OR. ICNV.EQ.4 ) RETURN
      SUBNMX = '/RSDL1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      DO 100 M = 1,ISVC
        RSD(M) = 0.D+0
        NSD(M) = 0
  100 CONTINUE
     
!
!---  Loop over all nodes
!
      DO 200 N = 1,num_nodes
        IF( IXP(N).LE.0 ) GOTO 200
        IZN = N
        IF( ISKP(IZN).EQ.1 ) GOTO 200
!
!---  Water saturated system prior to iteration  ---
!
        IF( NPHAZ(2,N).EQ.1 ) THEN
          ACP = PORD(2,N)*RHOL(2,N)*SL(2,N)*DTI*VOL(N)
          RSDX = MIN( ABS(blu(ieqw,n))/(ABS(PL(2,N))+PATM), &
          ABS(residual(IEQW,N)/ACP) )

          IF( RSDX.GT.RSD(IEQW) ) THEN
            RSD(IEQW) = RSDX
            NSD(IEQW) = N
          ENDIF
!
!---  Water-gas system prior to iteration  ---
!
        ELSEIF( NPHAZ(2,N).EQ.2 ) THEN
          ACP = PORD(2,N)*RHOL(2,N)*SL(2,N)*DTI*VOL(N)
          RSDX = MIN( ABS(blu(ieqw,n))/(ABS(PL(2,N))+PATM), &
          ABS(residual(IEQW,N)/ACP) )
          IF( RSDX.GT.RSD(IEQW) ) THEN
            RSD(IEQW) = RSDX
            NSD(IEQW) = N
          ENDIF
        ENDIF
  200 CONTINUE
     call ga_sync
     rsdw = rsd
     nsdw = nsd
     call ga_dgop(1,rsd,isvc,'max')
     do m=1,isvc
       nsd(m) = 0
       if(rsd(m).eq.rsdw(m)) then
         nsd(m) = loc2nat(nsdw(m))
       endif
     enddo
     call ga_igop(1,nsd,isvc,'max')
!
!---  Assign a convergence index  ---
!
      DO 300 M = 1,ISVC
        IF( RSD(M).GT.RSDMX ) ICNV = 2
  300 CONTINUE
      IF( ICNV.EQ.2 .AND. NITER.GE.NRIMX ) ICNV = 1
!
!---  Unconverged solution Newton-Raphson iteration limit exceeded  ---
!
      IF( ICNV.EQ.1 ) THEN
        if(me.eq.0)WRITE(ISC,'(10X,A)') '---  Convergence Failure  ---'
        if(me.eq.0)WRITE(IWR,'(10X,A)') '---  Convergence Failure  ---'
        if(me.eq.0)WRITE(ISC,'(4X,A,1PE11.4,A,I6)') 'Water Equation Maximum Residual = ', &
         RSD(IEQW),' Node = ',NSD(IEQW)
        if(me.eq.0)WRITE(IWR,'(4X,A,1PE11.4,A,I6)') 'Water Equation Maximum Residual = ', &
         RSD(IEQW),' Node = ',NSD(IEQW)
!
!---  Reduce time step  ---
!
!vlf        IF( NTSR.LT.4 ) THEN
        IF( NTSR.LT.10 ) THEN
          NTSR = NTSR + 1
          DTX = DT
          TM = TM - (1.D+0-DTCF)*DT
          DT = DTCF*DT
          DTO = DT
          DTI = 1.D+0/DT
          VAR = DT
          VARX = DTX
          IF( UNTM.NE.'null' ) THEN
            INDX = 1
            IUNS = 1
            CALL RDUNIT(UNTM,VAR,INDX)
            INDX = 1
            IUNS = 1
            CALL RDUNIT(UNTM,VARX,INDX)
            NCH = INDEX( UNTM,'  ')-1
          ENDIF
          if(me.eq.0)WRITE(ISC,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')       &
          'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ', &
          VAR,UNTM(1:NCH)
          if(me.eq.0)WRITE(IWR,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')        &
          'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ', &
          VAR,UNTM(1:NCH)
          DO 400 N = 1,num_nodes
            PL(2,N) = PL(1,N)
!            IXP(N) = ABS(IXP(N))
            NPHAZ(2,N) = NPHAZ(1,N)
            IPH(2,N) = IPH(1,N)
  400     CONTINUE
          call ga_sync
!
!---  Number of time step reductions failure: stop simulation  ---
!
        ELSE
          if(me.eq.0)WRITE(ISC,'(10X,A)') '---  Time Step Reduction Limit Exceeded ---'
          if(me.eq.0)WRITE(IWR,'(10X,A)') '---  Time Step Reduction Limit Exceeded ---'
          ICNV = 4
        ENDIF
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RSDL1 group
!
      RETURN
      END
