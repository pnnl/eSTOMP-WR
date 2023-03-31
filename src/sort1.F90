

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SORT1( NSL,petsc_a )
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
!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Compute solute transport source terms.
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
      USE GRID_MOD
      USE FLUXP
      USE FDVP
      USE BCVP
      USE CONST
      use petscapp
      USE COUP_WELL
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





      REAL*8 SRX(8+LSOLU),VX(10),C_FX(6),AREAXX(6)
      INTEGER IVX(10)
      EXTERNAL ADRM
      LOGICAL :: use_ga
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!--- Petsc includes
!
#include "petscwrapper.h"
!
!----------------------Common Blocks-----------------------------------!
!

      PetscInt :: ic(2),ir(2),nr,nc 
      PetscScalar :: values_(4)
      PetscErrorCode :: ierr
      Mat :: jac
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/SORT1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
       '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Loop over sources  ---
!
      DO 600 NS = 1,NSR
!
!---  Check source time  ---
!
        IF( TM.LE.SRC(1,1,NS) ) GOTO 600
        SRX(1) = TM
        IF( ISRM(NS).EQ.1 ) THEN
          DO 70 N = 2,8+NSOLU
            SRX(N) = SRC(N,1,NS)
   70     CONTINUE
          DTMAX = TM-SRC(1,1,NS)
        ELSE
          DO 100 M = 2,ISRM(NS)
            IF( TM.LE.SRC(1,M,NS) ) THEN
              DTSR = MIN( SRC(1,M,NS)-TM,DT )
              TFSR = (TM-0.5D+0*DTSR-SRC(1,M-1,NS))/ &
              (SRC(1,M,NS)-SRC(1,M-1,NS))
              DO 80 N = 2,8+NSOLU
                SRX(N) = SRC(N,M-1,NS)+TFSR*(SRC(N,M,NS)-SRC(N,M-1,NS))
   80         CONTINUE
              DTMAX = TM-SRC(1,M-1,NS)
              GOTO 110
            ENDIF
  100     CONTINUE
          GOTO 600
        ENDIF
  110   CONTINUE
!
!---    Loop over source domain  ---
!
!        DO 500 I = ISRDM(1,NS),ISRDM(2,NS)
!        DO 500 J = ISRDM(3,NS),ISRDM(4,NS)
!        DO 500 K = ISRDM(5,NS),ISRDM(6,NS)
        node_s = ubound(isrdm, DIM=1)
        do 500 node_sx = 1,node_s   
!          N = ND(I,J,K)
!          print *, 'node_sx = ',node_sx,node_s,isrdm(node_sx,ns)
          n = isrdm(node_sx,ns)
          if(n.eq.0) cycle
          MP = IXP(N)
          IADDVAL = 1
          SORTX = 0.D+0
!
!---      Aqueous Volumetric Sink  ---
!
          IF( ISRT(NS).EQ.2 .AND. SRX(4).LT.0.D+0 ) THEN
            SORTX = -SRX(4)*YL(NSL,N)/(PORD(2,N)*SL(2,N))
!
!---      Aqueous Volumetric Density Sink  ---
!
          ELSEIF( ISRT(NS).EQ.3 .AND. SRX(4).LT.0.D+0 ) THEN
            SORTX = -SRX(4)*YL(NSL,N)*VOL(N)/(SL(2,N)*PORD(2,N))
!
!---      Aqueous Mass Sink  ---
!
          ELSEIF( ISRT(NS).EQ.4 .AND. SRX(4).LT.0.D+0 ) THEN
            SORTX = -SRX(4)*YL(NSL,N)/(SL(2,N)*PORD(2,N)*RHOL(2,N))
!
!---      Aqueous Mass Density Sink  ---
!
          ELSEIF( ISRT(NS).EQ.5 .AND. SRX(4).LT.0.D+0 ) THEN
            SORTX = -SRX(4)*YL(NSL,N)*VOL(N)/ &
            (SL(2,N)*PORD(2,N)*RHOL(2,N))
!
!---      Solute Source  ---
!
          ELSEIF( ISRT(NS).EQ.-NSL .AND. NSL.LE.NSOLU ) THEN
            residual(1,n) = residual(1,n) + SRX(4)
!
!---      Solute Density Source  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+NSOLU) .AND. NSL.LE.NSOLU ) THEN
            residual(1,n) = residual(1,n) + SRX(4)*VOL(N)
!
!---      Solute Inventory Source  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+3*NSOLU) .AND. NSL.LE.NSOLU ) THEN
            IF( SRX(3).GT.EPSL ) THEN
              IF( SRX(3).LT.(SRX(4)*SL(2,N)*PORD(2,N)*VOL(N)/ &
              YL(NSL,N)) .AND. SRX(5).LT.0.D+0 ) THEN
                residual(1,n) = residual(1,n) + SRX(3)*DTI
              ELSE
                sortx = 1.D+9*DTI*VOL(N)
!                ir(1) = loc_map(n)-1
!                ic(1) = ir(1)
!                nr = 1
!                nc = 1
!                call MatSetValuesLocal( A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
                residual(1,n) = SRX(4)*SL(2,N)*PORD(2,N)*1.D+9*DTI*VOL(N) &
                /YL(NSL,N)
              ENDIF
            ENDIF
            IADDVAL = 1
!
!---      Advection-dominated solute release model  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+4*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!--- section to be worked on
!            NPZ = NSZ(N)
!            NQZ = NPZ + IJFLD
!            WLX = ABS(5.D-1*(WL(1,NQZ)+WL(1,NPZ)))
!            WLX = MAX( -WL(1,NPZ),ZERO )
!            ALPHAX = WLX/(PORD(2,N)*SL(2,N)*SRX(4))
!            VX(1) = ALPHAX
!            VX(2) = SRX(3)
!            IVX(1) = INT(SRX(5))
!            CALL QROMB( ADRM,VX,ZERO,DTMAX,QSRIX,IERR,IVX )
!            IF( IERR.NE.0 ) THEN
!              INDX = 3
!              CHMSG = 'Failed Romberg Integration: ' // &
!              'Advection-Dominated Release Model'
!              CALL WRMSGS( INDX )
!            ENDIF
!            QSRIX = MIN( SRX(3),QSRIX )
!            QSRRX = MAX( (QSRIX-SRCIC(NSL,N))*DTI,0.D+0 )
!
!---        Hold solute release rate in variable CNL  ---
!
            CNL(NSL,N) = QSRRX
            residual(1,n) = residual(1,n) + QSRRX
!
!---      Diffusion-dominated solute release model  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+5*NSOLU) .AND. NSL.LE.NSOLU ) THEN
            QSRIX = 2.D+0*SRX(3)*SQRT(SRX(5)*DTMAX/GPI)/SRX(4)
            QSRIX = MIN( SRX(3),QSRIX )
            QSRRX = MAX( (QSRIX-SRCIC(NSL,N))*DTI,0.D+0 )
!
!---        Hold solute release rate in variable CNL  ---
!
            CNL(NSL,N) = QSRRX
            residual(1,n) = residual(1,n) + QSRRX
!
!---      Solubility-controlled solute release models or
!         solubility-controlled salt cake release models  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+6*NSOLU) .OR. &
                ISRT(NS).EQ.-(NSL+7*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!
!---        SRX(2): nodal solute inventory  ---
!
            IF ( SRCIC(NSL,N).LT.SRX(2) ) THEN
!
!---          Forecast whether the cummulative mass dissolved will 
!             exceed the nodal inventory within the current time step.  
!             This is an approximation since the fluxes were computed  
!             at the previous time step.  However, the calculation 
!             does provide an estimate for minimizing the error 
!             due to overshooting the mass dissolved.  ---
!
              DCDT =  (C(NSL,N)-CO(NSL,N))*VOL(N)*DTI
              CLX = ZERO
              DO IFCX = 1,6
                ICNX = ND2CNX(IFCX,N)
                IF(ICNX > 0) THEN
                  AREAXX(IFCX) = AREAC(ND2CNX(IFCX,N))
                  C_FX(IFCX) = C_FLUX(NSL,ND2CNX(IFCX,N))
                ELSEIF(ICNX < 0) THEN
                  ICNX = ABS(ND2CNX(IFCX,N))
                  IF(ICNX <= NUM_BCNX) THEN
                    AREAXX(IFCX) = AREAB(ND2CNX(IFCX,N))
                    C_FX(IFCX)= C_FLUX_B(NSL,ND2CNX(IFCX,N))
                  ENDIF
                ELSE
                  C_FX(IFCX) = 0.D0
                  AREAXX(IFCX) = 0.0D0
                ENDIF
              ENDDO
              CLX = CLX + C_FX(1)*AREAXX(1)
              CLX = CLX - C_FX(2)*AREAXX(2)
              CLX = CLX + C_FX(3)*AREAXX(3)
              CLX = CLX - C_FX(4)*AREAXX(4)
              CLX = CLX + C_FX(5)*AREAXX(5)
              CLX = CLX - C_FX(6)*AREAXX(6)
              CNL(NSL,N) = ZERO
              IF ( SRCIC(NSL,N)+(DCDT-CLX)*DT.GT.SRX(2) ) THEN
!
!---            If the forecast predicts overshoot, set the source mass
!               as the difference between the current cummulative mass
!               and the nodal inventory.  ---
!
                QSRRX = (SRX(2)-SRCIC(NSL,N))*DTI
                CNL(NSL,N) = QSRRX
                residual(1,n) = residual(1,n) + QSRRX
              ELSE
!
!---            Otherwise, set diagonal matrix entry to 1 and all 
!               off-diagonal to 0, to force the concentration to be the 
!               solubility limit.  ---
!
!                 ir(1) = loc_map(n)-1
                 ir(1) = gloc_map(n)-1
                 ic(1) = ir(1)
             !   ir = loc_map(n)-1
             !   ic = ir
                nr = 1
                nc = 1
                values_(1) = 1.d0
                call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_, &
                  ADD_VALUES,ierr )
!
!---            Elements connected to this node
!
                do ifcx = 1,6
                  icnx = nd2cnx(ifcx,n)
                  if(icnx > 0) then
                   id_up = conn_up(icnx)
                   id_dn = conn_dn(icnx)
                   if(id_up.eq.n) id_nx = id_dn
                   if(id_dn.eq.n) id_nx = id_up
                   ir(1) = gloc_map(n)-1
                   ic(1) = gloc_map(id_nx)-1
             !      ir(1) = loc_map(n)-1
             !      ic(1) = loc_map(id_nx)-1
             !      ic(1) = ir(1)
             !      ir = loc_map(id_nx)-1
             !      ic = ir
                   nr = 1
                   nc = 1
                   values_(1) = 0.d0
                   call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_, &
                     ADD_VALUES,ierr )
                  endif
                enddo
!
!---            Solubility-controlled solute release model  ---
!
                IF( ISRT(NS).EQ.-(NSL+6*NSOLU) &
                .AND. NSL.LE.NSOLU ) THEN
!
!---              SRX(2): nodal solute inventory
!                 SRX(3): aqueous solubility  ---
!
                  residual(1,n) = SRX(3)*SL(2,N)*PORD(2,N)/YL(NSL,N)
!
!---              Solubility-controlled salt cake release model  ---
!
                ELSEIF ( ISRT(NS).EQ.-(NSL+7*NSOLU)  &
                .AND. NSL.LE.NSOLU ) THEN
!
!---              SRX(2): nodal solute inventory
!                 SRX(3): nodal salt cake inventory
!                 SRX(4): salt cake solubility  ---
!
                  residual(1,n) = SRX(4)*SRX(2)/SRX(3)*SL(2,N)*PORD(2,N)/ &
                        YL(NSL,N)
                ENDIF
              ENDIF
            ENDIF
            IADDVAL = 0
!
!---      Diffusion-dominated solute release model 
!         (w/variable diffusion)  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+8*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!
!---        SRX(3): nodal solute inventory
!           SRX(4): vertical depth of residual waste
!           SRX(5): diffusion coeficient
!           SRX(6): constrictivity  ---
!
            TORLX = TORL(2,N)
            DIFFIX = SRX(5)*PORD(2,N)*SRX(6)/(TORLX*TORLX)
            IZN = N
            ALPHAX = PORD(2,N) + RHOS(IZN)*PCSL(1,NSL,IZN)* &
                               (1.d0-PORT(2,N))
            DIFFAX = DIFFIX/ALPHAX
            QSRIX = 2.D+0*SRX(3)*SQRT(DIFFAX*DTMAX/GPI)/SRX(4)
            QSRIX = MIN( SRX(3),QSRIX )
            QSRRX = MAX( (QSRIX-SRCIC(NSL,N))*DTI,0.D+0 )
!
!---        Hold solute release rate in variable CNL  ---
!
            CNL(NSL,N) = QSRRX
            residual(1,n) = residual(1,n) + QSRRX
!
!---      In-Well Vapor-Stripping Solute Source  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+2*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!
!---        Loop over inlet well screen surface domain and compute
!           volumetric aqueous and solute fluxes, using outflow
!           logic for computing surface fluxes  ---
!
            QLVS = 0.D+0
            QCVS = 0.D+0
!            DO 200 IVS = ISRDM(8,NS),ISRDM(9,NS)
!            DO 200 JVS = ISRDM(10,NS),ISRDM(11,NS)
!            DO 200 KVS = ISRDM(12,NS),ISRDM(13,NS)
            node_vs = ubound(isrdm_,DIM=1)
            do 200 node_vsx = 1,node_vs-1
!              NVS = ND(IVS,JVS,KVS)
             nvs = isrdm_(node_vsx,ns)
             if(node_vs.eq.0) cycle 
             id_x = isrdm_(node_vs,ns)              
             loop_ifx: do ifcx = 1,6
               icnxx = nd2cnx(ifcx,n)
               u_x = unvxc(icnxx)
               v_x = unvyc(icnxx)
               w_x = unvzc(icnxx)
               if(u_x.eq.-1.d0.and.id_x.eq.-1) then
                 icnx = icnxx
                 exit loop_ifx
               elseif(u_x.eq.1.d0.and.id_x.eq.1) then
                 icnx = icnxx
                 exit loop_ifx
               elseif(v_x.eq.-1.d0.and.id_x.eq.-2) then
                 icnx = icnxx
                 exit loop_ifx
               elseif(v_x.eq.1.d0.and.id_x.eq.2) then
                 icnx = icnxx
                 exit loop_ifx
               elseif(w_x.eq.-1.d0.and.id_x.eq.-3) then
                 icnx = icnxx
                 exit loop_ifx
               elseif(w_x.eq.1.d0.and.id_x.eq.3) then
                 icnx = icnxx
                 exit loop_ifx
               endif
             enddo loop_ifx
             x_id = real(sign(1,id_x))
                
             QLX = MAX( x_id*q_flux(1,icnx)*areac(icnx),0.D+0 )
             QLVS = QLVS + QLX
             FCL = YL(NSL,NVS)/(SL(2,NVS)*PORD(2,NVS)+SMALL)
             QCVS = QCVS + QLX*FCL*C(NVS,NSL)
  200       CONTINUE
            CALL WATSP( SRX(6),PSWX )
            CALL WATLQD( SRX(6),SRX(2),RHOLX )
            PVAX = MAX( SRX(2)-PSWX,0.D+0 )
            INDX = 0
            CALL WATGSD( SRX(6),PSWX,RHOW,INDX )
            CALL AIRGSD( SRX(6),PVAX,RHOA )
            RHOGX = RHOW+RHOA
            WTMG = PVAX*WTMA/SRX(2) + PSWX*WTMW/SRX(2)
            IF( SRX(3).GT.1.D-6 ) THEN
              XLOX = QCVS/(QLVS*(SRX(7)*SRX(4)*RHOGX*SRX(3)/ &
              (SRX(2)*WTMG)+RHOLX/WTMW)+SMALL)
            ELSE
              XLOX = QCVS/(QLVS*SRX(7)*RHOLX/WTMW+SMALL)
            ENDIF
            residual(1,n) = residual(1,n)+XLOX*RHOLX*QLVS*VOL(N)/(SRX(5)*WTMW)
          ENDIF
!
!---      Load Jacobian  ---
!
          values_ = 0.d0
          IF( IADDVAL.EQ.1 ) THEN
             ir(1) = gloc_map(n)-1
!             ir(1) = loc_map(n)-1
             ic(1) = ir(1)
             nr = 1
             nc = 1
             buffer = sortx
             values_(1) = sortx
!            print *,'me nodes',me,node_s,n,ns,sortx
             call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,buffer,ADD_VALUES,ierr )
          ENDIF
  500   CONTINUE
  600 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SORT1 group  ---
!
      RETURN
      END
