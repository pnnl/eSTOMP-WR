

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE UPDT1
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
!
!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Update the primary variables.
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

      LOGICAL :: use_ga

!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/UPDT1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      IF( ICNV.EQ.1 ) GOTO 300
      rlxf = 1.d0
!
!---  Compute maximum residuals and update primary variables
!
      DO 200 N = 1,num_nodes
        plwx = 0.d0
        IF( IXP(N).LE.0 ) GOTO 200
        IZN = N
        NMD = IXP(N)
        PAE = 0.D+0
        IF( MOD(ISCHR(IZN),10).EQ.2 ) PAE = SCHR(1,IZN)*RHORL*GRAV
!
!---  Limit aqueous pressure updates to changes in aqueous
!     saturation of 0.125 for unsaturated conditions   ---
!
        SLX = SL(2,N)-SIGN(0.125D+0,(SL(2,N)-0.5D+0))
        IF( (ISCHR(IZN).NE.301) .AND. (ISCHR(IZN).NE.302) .AND. &
        (ISCHR(IZN).NE.3) .AND. (ISCHR(IZN).NE.4) .AND. &
        (PG(2,N)-PL(2,N)-PAE.GT.0.D+0) ) THEN
          CPGLO = PG(1,N)-PL(1,N)
          CALL CAP1( IZN,SLX,SGT(2,N),CPGL,SL(1,N),CPGLO,IPH(2,N) )
          DPL = ABS( CPGL-PG(2,N)+PL(2,N) )
          DPL = MIN( DPL,ABS(blu(ieqw,n)) )
          DPL = SIGN( DPL,blu(ieqw,n) )*RLXF
!        if(N==52003)  write(*,*) '000 DPL',DPL
        ELSE
          DPL = blu(ieqw,n)*RLXF
!        if(N==52003)  write(*,*) '111 DPL',DPL
        ENDIF
        pllx = 0.d0
!
!---  Relax aqueous pressure updates when transitioning from
!     unsaturated to saturated states   ---
!
        IF( PG(2,N)-PL(2,N)-PAE.GT.ZERO .AND. &
          PG(2,N)-PL(2,N)-PAE-DPL.LT.ZERO ) DPL = 6.D-1*DPL
!       if(N==52003) write(*,*) '222 DPL',DPL
        PL(2,N) = PL(2,N) + DPL
!
!---  Check for excessive aqueous pressure  ---
!
        nlwx = 0  ! Yilin/BH
        IF(pl(2,n).gt.pmx-patm) THEN
          pllx = pl(2,n)
          plwx = pllx
        !  nlwx = n
          nlwx = loc2nat(n)
          exit
        ENDIF
  200 CONTINUE    
!      nlwx =0
      call ga_dgop(1,pllx,1,'max')
      n = 0        
!      if(pllx.eq.plwx) n = loc2nat(nlwx)
       if(pllx.eq.plwx) n = nlwx
      call ga_igop(1,n,1,'max')

        IF( PLlx.GT.PMX-PATM ) THEN
          ICNV = 1
          if(me.eq.0)WRITE(ISC,'(10X,A)')'---  Primary Variable(s) Error  ---'
          if(me.eq.0)WRITE(IWR,'(10X,A)')'---  Primary Variable(s) Error  ---'
          if(me.eq.0)WRITE(ISC,'(4X,A,1PE12.5,A,I6)') &
!          'Water Pressure = ',PL(2,N)+PATM,' Node = ',N
           'Water Pressure = ',pllx+PATM,' Node = ',N
          if(me.eq.0)WRITE(IWR,'(4X,A,1PE12.5,A,I6)') &
           'Water Pressure = ',pllx+PATM,' Node = ',N
!          'Water Pressure = ',PL(2,N)+PATM,' Node = ',N
!          GOTO 300
        ENDIF
!  200 CONTINUE
!
!---  Reduce time step  ---
!
  300 CONTINUE
      IF( ICNV.EQ.1 ) THEN
!        IF( NTSR.LT.4 ) THEN
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
          if(me.eq.0)&
          WRITE(ISC,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')       &
          'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ', &
          VAR,UNTM(1:NCH)
          if(me.eq.0)&
          WRITE(IWR,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')       &
          'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ', &
          VAR,UNTM(1:NCH)
          DO 400 N = 1,num_nodes
            PL(2,N) = PL(1,N)
!            IXP(N) = ABS(IXP(N))
            NPHAZ(2,N) = NPHAZ(1,N)
            IPH(2,N) = IPH(1,N)
  400     CONTINUE
!
!---  Number of time step reductions failure: stop simulation  ---
!
        ELSE
          DO 410 N = 1,num_nodes
            PL(2,N) = PL(1,N)
!            IXP(N) = ABS(IXP(N))
            NPHAZ(2,N) = NPHAZ(1,N)
            IPH(2,N) = IPH(1,N)
  410     CONTINUE
          if(me.eq.0)WRITE(ISC,'(10X,A)') '---  Time Step Reduction Limit Exceeded ---'
          if(me.eq.0)WRITE(IWR,'(10X,A)') '---  Time Step Reduction Limit Exceeded ---'
          ICNV = 4
        ENDIF
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of UPDT1 group
!
      RETURN
      END
