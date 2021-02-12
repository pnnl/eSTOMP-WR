

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CBND1( NB,NBCT,NSL,ICYCLE,AP,DLZ,FLB,FCL,FCLP,BCXX,ISJCB )
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
!     Modify the Jacobian matrix for the solute transport equation
!     to incorporate boundary conditions.
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
      USE SOLTN
      USE REACT
      USE PORMED
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      USE BCVP
      USE BCV
      use grid_mod
      use petscapp
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
!--- Petsc includes
!
#include "petscwrapper.h"
!
!----------------------Parameter Statements----------------------------!
!

      PetscInt :: ic(2),ir(2),nr,nc 
      PetscScalar :: values_(4)
      PetscErrorCode :: ierr
!
      REAL*8 BCX(LSPBC+1)
      double precision :: s_fx_up(3),s_area_x(3)
      LOGICAL :: ISJCB
      LOGICAL :: use_ga
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
!      SUBNMX = '/CBND1'
!      ICSNX = INDEX( SUBNMX,'  ' )-1
!      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
!      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
!       '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
!      ICSN = ICSN+ICSNX
        TMZ = TM
        IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
        MB = IBCIN(NB)
        IF( IBCC(NB).EQ.1 ) TMZ = MOD( TM,BC(1,IBCM(NB),MB) )
        IF( TMZ.LE.BC(1,1,MB) ) THEN
          ICYCLE = 1
          RETURN
        ENDIF
        IF( IBCM(NB).GT.1 .AND. TMZ.GT.BC(1,IBCM(NB),MB) ) THEN
          ICYCLE = 1
          RETURN
        ENDIF
        BCXX = 0.d+0
        IF( IBCM(NB).EQ.1 ) THEN
!
!---      Solute transport  ---
!
          IF( NSL.LE.NSOLU ) THEN
            BCX(1) = BC(NSL+LBCU,1,MB)
            IF( IBCT(NBCT,NB).EQ.12 ) BCX(1) = CBO(NB,NSL)
!
!---      Reactive species transport  ---
!
          ELSE
            BCX(1) = 0.D+0
            DO 10 NSPX = 1,IBCSP(1,NB)
              MX = NSOLU+LBCU+NSPX
              BCX(NSPX+1) = BC(MX,1,MB)
              IF( IBCT(NBCT,NB).EQ.12 ) BCX(NSPX+1) = SP_CBO(NB,NSP)
   10       CONTINUE
          ENDIF
        ELSE
          DO 100 M = 2,IBCM(NB)
            IF( TMZ.LE.BC(1,M,MB) ) THEN
              TDBC = (BC(1,M,MB)-BC(1,M-1,MB))
              DTBC = MIN( BC(1,M,MB)-TMZ,DT )
              TFBC = (TMZ-5.D-1*DTBC-BC(1,M-1,MB))/TDBC
!
!---          Solute transport  ---
!
              IF( NSL.LE.NSOLU ) THEN
                BCX(1) = BC(NSL+LBCU,M-1,MB) + &
                TFBC*(BC(NSL+LBCU,M,MB)-BC(NSL+LBCU,M-1,MB))
                IF( IBCT(NBCT,NB).EQ.12 ) BCX(1) = CBO(NB,NSL)
!
!---          Reactive species transport  ---
!
              ELSE
                BCX(1) = 0.D+0
                DO 20 NSPX = 1,IBCSP(1,NB)
                  NSP = IBCSP(NSPX+1,NB)
                  MX = NSOLU+LBCU+NSPX
                  BCX(NSPX+1) = BC(MX,M-1,MB) + &
                  TFBC*(BC(MX,M,MB)-BC(MX,M-1,MB))
                  IF( IBCT(NBCT,NB).EQ.12 ) BCX(NSPX+1) = SP_CBO(NB,NSP)
   20           CONTINUE
              ENDIF
              GOTO 110
            ENDIF
  100     CONTINUE
          ICYCLE = 1
          RETURN
        ENDIF
  110   CONTINUE
        N = IBCN(NB)
        IZN = N
!
!---  Phase fraction factors at node adjacent to boundary  ---
!
        FCLP = YL(nsl,N)/(SL(2,N)*PORD(2,N)+SMALL)
!
!---    Diffusion coefficients at node adjacent to boundary  ---
!
        TCOR = (T(2,N)+TABS)/TSPRF
        SMDLP = SMDL(NSL)*TCOR*(VISRL/VISL(2,N))
!
!---    Kemper and van Schaik empirical model  ---
!
        IF( IEDL(NSL).EQ.2 ) THEN
          DLP = SDCL(1,NSL,IZN)*SDCL(2,NSL,IZN)* &
          EXP(SL(2,N)*PORD(2,N)*SDCL(3,NSL,IZN))
!
!---    Temperature dependent molecular diffusion coefficient  ---
!
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DLP = TORL(2,N)*SL(2,N)*PORD(2,N)*SMDL(NSL)
!
!---    Power function molecular diffusion coefficient  ---
!
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DLP = SDCL(1,NSL,IZN)*SDCL(2,NSL,IZN)* &
           (SL(2,N)*PORD(2,N))**SDCL(3,NSL,IZN)
!
!---    Constant molecular diffusion coefficient  ---
!
        ELSE
          DLP = TORL(2,N)*SL(2,N)*PORD(2,N)*SMDLP
        ENDIF
!
!---    Solute transport only, skip calculations for reactive
!       species transport  ---
!
        XVLB = SLB(2,NB)*PORDB(2,NB)
        IF( NSL.LE.NSOLU ) THEN
!
!---      Phase fraction factors at boundary  ---
!
          IF( IPCL(NSL).EQ.4 ) THEN
            NS = IPCSL(NSL,IZN)
            XVSB = SLB(2,NB)*RHOS(IZN)*PCSL(1,NS,IZN)* &
            (1.D+0-PORTB(2,NB))
            CLX = CB(NB,NS)/(XVSB+XVLB)
            IF( CLX.LT.SMALL ) THEN
              PCSLX = PCSL(1,NSL,IZN)
            ELSE
              PCSLX = 1.D+1**(PCSL(2,NSL,IZN)+PCSL(3,NSL,IZN)* &
              LOG10(CLX))
            ENDIF
            XVSB = RHOS(IZN)*PCSLX*(1.D+0-PORTB(2,NB))*SLB(2,NB)
          ELSEIF( IPCL(NSL).EQ.3 ) THEN
            NS = IPCSL(NSL,IZN)
            XVSB = SLB(2,NB)*RHOS(IZN)*PCSL(1,NS,IZN)* &
            (1.D+0-PORTB(2,NB))
            CLX = CB(NB,NS)/(XVSB+XVLB)
            IF( CLX.LT.SMALL ) THEN
              PCSLX = PCSL(1,NSL,IZN)
            ELSE
              PCSLX = 1.D+1**(PCSL(2,NSL,IZN)+PCSL(3,NSL,IZN)* &
              LOG10(CLX))
           ENDIF
            XVSB = RHOS(IZN)*PCSLX*(1.D+0-PORTB(2,NB))
          ELSEIF( IPCL(NSL).EQ.2 ) THEN
            XVSB = RHOS(IZN)*PCSL(1,NSL,IZN)*(1.D+0-PORTB(2,NB))* &
            SLB(2,NB)
          ELSE
            XVSB = RHOS(IZN)*PCSL(1,NSL,IZN)*(1.D+0-PORTB(2,NB))
          ENDIF
!
!---      Phase-volumetric concentration ratios  ---
!
          FCL = 0.D+0
          IF( (XVSB+XVLB)/EPSL.GT.EPSL ) FCL = 1.D+0/(XVSB+XVLB)
!
!---      Phase mole fractions  ---
!
          YLB(NB,NSL) = XVLB*FCL
!
!---      Convert boundary phase concentrations to
!         volumetric concentrations  ---
!
          IF( IBCT(NBCT,NB).EQ.8 .OR. IBCT(NBCT,NB).EQ.14 &
          .OR. IBCT(NBCT,NB).EQ.23 ) THEN
            BCX(1) = BCX(1)/FCL
          ENDIF
!
!---      Load boundary concentration  ---
!
          CB(NB,NSL) = BCX(1)

        ELSE
!
!---      Convert species concentrations to total-component
!         concentrations  ---
!
          IF( NSL.LE.NSOLU+NEQC ) THEN
            NEQ = NSL-NSOLU
            DO 130 NSP = 1,IEQ_C(1,NEQ)
              DO 120 NSPX = 1,IBCSP(1,NB)
                IF( IBCSP(NSPX+1,NB).EQ.IEQ_C(NSP+1,NEQ) ) THEN
                  BCX(1) = BCX(1) + EQ_C(NSP,NEQ)*BCX(NSPX+1)
                ENDIF
  120         CONTINUE
  130       CONTINUE
!
!---      Convert species concentrations to total-kinetic
!         concentrations  ---
!
          ELSEIF( NSL.LE.NSOLU+NEQC+NEQK ) THEN
            NEQ = NSL-NSOLU-NEQC
            DO 150 NSP = 1,IEQ_K(1,NEQ)
              DO 140 NSPX = 1,IBCSP(1,NB)
                IF( IBCSP(NSPX+1,NB).EQ.IEQ_K(NSP+1,NEQ) ) THEN
                  BCX(1) = BCX(1) + EQ_K(NSP,NEQ)*BCX(NSPX+1)
                ENDIF
  140         CONTINUE
  150       CONTINUE
          ENDIF
!
!---      Phase-volumetric concentration ratios  ---
!
          FCL = 0.D+0
          IF( XVLB/EPSL.GT.EPSL ) FCL = 1.D+0/XVLB
!
!---      Phase mole fractions  ---
!
          YLB(NB,NSL) = 1.D+0
!
!---      Convert boundary phase concentrations to
!         volumetric concentrations  ---
!
          IF( IBCT(NBCT,NB).EQ.8 .OR. IBCT(NBCT,NB).EQ.14 &
          .OR. IBCT(NBCT,NB).EQ.23 ) THEN
            BCX(1) = BCX(1)/FCL
          ENDIF
!
!---      Load boundary concentration  ---
!
          CB(NB,NSL) = BCX(1)

        ENDIF
!
!---  Hydraulic dispersion
!
        xnorm = 0.d0
        u_x = abs(uvxb(nb))
        u_y = abs(uvyb(nb))
        u_z = abs(uvzb(nb))
        if(u_x >0.d0) then
          xnorm = uvxb(nb)
        elseif(u_y > 0.d0) then
          xnorm = uvyb(nb)
        elseif(u_z > 0.d0 )then
          xnorm = uvzb(nb)
        endif
        if(isjcb) c_flux_b(nsl,nb) = 0.D+0
        IF( IDISP.EQ.1 ) THEN
            areabx = areab(nb)
            s_fx_up = 0.d0
            s_area_x = 0.d0
     
            do ix=1,3
              s_fx_up(ix) = s_fx(ix,n)/(s_area(ix,n)+1.d-60)
            enddo 

            if(u_x > 0.d0) then
              ubx = q_flux_b(1,nb)
              vbx = s_fx_up(2)
              wbx = s_fx_up(3)
            elseif(u_y > 0.d0) then
              ubx = s_fx_up(1)
              vbx = q_flux_b(1,nb)
              wbx = s_fx_up(3)
            elseif(u_z > 0.d0) then
              ubx = s_fx_up(1)
              vbx = s_fx_up(2)
              wbx = q_flux_b(1,nb)
            endif
            ulbsq = ubx*ubx
            vlbsq = vbx*vbx
            wlbsq = wbx*wbx
            zvb = sqrt(ulbsq+vlbsq+wlbsq)
            displx = displ(izn)
            disptx = dispt(izn)
            aux = abs(u_x)
            avx = abs(u_y)
            awx = abs(u_z)
!BH
            disptvx = disptv(izn)  !BH
            IF (aux.EQ.1 .AND. avx.EQ.0 .AND.awx.EQ.0) THEN
                    dplb = (displx * ulbsq + disptx * vlbsq + disptvx *wlbsq)/(zvb+small)
            ELSEIF (aux.EQ.0 .AND. avx.EQ.1 .AND.awx.EQ.0) THEN
                    dplb = (displx * vlbsq + disptx * ulbsq + disptvx *wlbsq)/(zvb+small)
            ELSEIF (aux.EQ.0 .AND. avx.EQ.0 .AND.awx.EQ.1) THEN
                    dplb = (displx * wlbsq + disptvx * ulbsq + disptvx *vlbsq)/(zvb+small)
            ENDIF
!                dplb = (displx*(ulbsq*aux+vlbsq*avx+wlbsq*awx) + &
!                        disptx*(ulbsq+vlbsq+wlbsq-(ulbsq*aux+vlbsq*avx+wlbsq*awx)))/(zvb+small)
!BH
            DPLB = DPLB*SMDEF(NSL,IZN)
        ELSE
            DPLB = 0.D+0
        ENDIF
        qfluxx = q_flux_b(1,nb)
        areabx = areab(nb)    
        FLB = areabx*qfluxx
        CRLB = ABS( qfluxx )*DT/(2.d0*distb(nb)*XVLB+SMALL)
        if(islc(1).ge.1) then
            u_x = abs(uvxb(nb))
            u_y = abs(uvyb(nb))
            u_z = abs(uvzb(nb))
             ldx = iaxmax - iaxmin + 1
             ldy = iaymax - iaymin + 1
             ldz = iazmax - iazmin + 1
              i_ = n - 1
              ix = mod(i_,ldx)
              i_ = (i_-ix)/ldx
              jx = mod(i_,ldy)
              kx = (i_-jx)/ldy
              ix = ix + 1
              jx = jx + 1
              kx = kx + 1
!              xnorm = 0.d0
              i_procx = 0
              n_up1 = 0
              if( u_x.gt.0.d0 ) then
                  if(uvxb(nb) < 0.d0) then
                   n_up1 = n+1
!                   xnorm = -1.d0
                  else
                   n_up1 = n-1
!                   xnorm = 1.d0
                  endif
                  if(n_up1>0.and.n_up1<num_nodes) then
                    dist_u23 = abs(xp(n_up1) - xp(n))
                    i_procx = 1
                  endif
              elseif( u_y.gt.0.d0 ) then
                  if(uvyb(nb) < 0.d0) then
                   n_up1 = n+ldx
!                   xnorm = -1.d0
                  else
                   n_up1 = n-ldx
!                   xnorm = 1.d0
                  endif
                  if(n_up1>0.and.n_up1<num_nodes) then
                    dist_u23 = abs(yp(n_up1) - yp(n))
                    i_procx = 1
                  endif
              elseif( u_z.gt.0.d0 ) then
                  if(uvzb(nb) < 0.d0) then
                   n_up1 = n+ldx*ldy
!                   xnorm = -1.d0
                  else
                   n_up1 = n-ldx*ldy
!                   xnorm = 1.d0
                  endif
                  if(n_up1>0.and.n_up1<num_nodes) then
                    dist_u23 = abs(zp(n_up1) - zp(n))
                    i_procx = 1
                  endif
              endif
        endif
        BCXX = BCX(1)
!
!---  Dirichlet ---
!
        AP = 0.D+0
       WCZ = 0.D+0
        IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8 &
          .OR. IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SMDLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,NSL,IZN)*SDCL(2,NSL,IZN)* &
              EXP(XVLB*SDCL(3,NSL,IZN))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*XVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,NSL,IZN)*SDCL(2,NSL,IZN)* &
                XVLB**SDCL(3,NSL,IZN)
            ELSE
              DLB = TORLB(2,NB)*XVLB*SMDLB
            ENDIF
            INDX = 16
            distbx = distb(nb)
            if(xnorm <0.d0 ) then
              DLZ = DIFMN(DLB,DLP,distbx,distbx,qfluxx,INDX)
            else
              DLZ = DIFMN(DLP,DLB,distbx,distbx,qfluxx,INDX)
            endif
            DLZ = areabx*(DLZ+DPLB)/distbx
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 )  THEN
              FLB = areabx*qfluxx
              IF( xnorm*flb.lt.0.d0 ) THEN
                WCZ = BCX(1)*FCL*FLB
!
!---  Use Patankar on the boundaries ---
!
!              ELSEIF( xnorm*FLB.ge.0.d0 .and. i_procx.eq.1 ) then
!                FCLT = YL(nsl,n_up1)/(SL(2,n_up1)*PORD(2,n_up1)+SMALL)
!                R = ((C(nsl,n)*FCLP-C(nsl,n_up1)*FCLT) &
!                 /(BCX(1)*FCL-C(nsl,n)*FCLP+SMALL)) &
!                 *(distbx/dist_u23)
!                THETA = FLIMIT( R,CRLB,ISLC(1) )
!                if(xnorm.gt.0.d0) then
!                  WCZ = BCX(1)*FLB*THETA*FCL &
!                  + C(NSL,n)*FLB*(1.D+0-THETA)*FCLP
!                else
!                  WCZ = BCX(1)*FLB*(1.d0-THETA)*FCL &
!                  + C(NSL,n)*FLB*THETA*FCLP
!                endif
              ELSE
                WCZ = C(NSL,n)*FLB*FCLP
              ENDIF
              IF(ISJCB)c_flux_b(nsl,nb) = c_flux_b(nsl,nb)+wcz/areabx
              AB = DLZ*FCL
              AP = DLZ*FCLP
              residual(1,n) = residual(1,n) -xnorm*WCZ
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              icnx = 0
              flt = 0.d0
              loop_ifx : do ifx = 1,6
                icnx = nd2cnx(ifx,n)
                if(icnx > 0) then
                  v_x = abs(unvxc(icnx))
                  v_y = abs(unvyc(icnx))
                  v_z = abs(unvzc(icnx))
                  if(u_x*v_x.gt.0.d0 .or. u_y*v_y.gt.0.d0 .or. u_z*v_z.gt.0.d0 ) then
                    flt = q_flux(1,icnx)*areac(icnx)
                    exit loop_ifx
                  endif
                endif
              enddo loop_ifx
              IF( xnorm*flt.lt.0.d0 ) THEN
                XVLX = SL(2,n_up1)*PORD(2,n_up1)
                FCLT = YL(nsl,n_up1)/(XVLX+SMALL)
                CRLT = ABS( q_flux(1,icnx) )*DT &
                 /( dist_up(icnx)+dist_dn(icnx) ) &
                 /(XVLX+SMALL)
                R = ((C(nsl,N)*FCLP-BCX(1)*FCL) &
                /(C(nsl,n_up1)*FCLT-C(nsl,N)*FCLP+SMALL)) &
                *(dist_u23/distbx)
                THETA = FLIMIT( R,CRLT,ISLC(1) )
                if(xnorm<0.d0) then
                  dzf = dist_dn(icnx)/dist_u23
                else
                  dzf = dist_up(icnx)/dist_u23
                endif
                WCZ = C(nsl,N)*FLT*(1.D+0-THETA*DZF)*FCLP &
                + C(nsl,n_up1)*FLT*THETA*DZF*FCLT
                WCZF = CO(nsl,N)*FLT*FCLP
                residual(1,n) = residual(1,n) + xnorm*(WCZ - WCZF)
                residual(1,n_up1) = residual(1,n_up1) - xnorm*( WCZ-WCZF)
              ENDIF
            ELSE
              ALB = MAX(-1.d0*sign(1,ibcd(nb))*FLB,ZERO) &
              + DLZ*MAX((ONE-(TENTH*ABS(FLB)/(DLZ+SMALL)))**5,ZERO)
              AP = (ALB+sign(1,ibcd(nb))*FLB)*FCLP
              AB = ALB*FCL
            ENDIF
            residual(1,n) = residual(1,n) + AB*BCX(1)
!
!---  Outflow ---
!
        ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.                    &
          ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23) &
          .AND. (xnorm*FLB.gt.0.D+0)) ) THEN
          if(ibcd(nb) < 0) then
             flb = min(flb,zero)
          else
             flb = max(flb,zero)
          endif
!vlf          flb = sign( max(xnorm*flb,0.d0),flb )
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 )  THEN
              distbx = distb(nb)
              IF( xnorm*FLB.ge.0.d0 .and. i_procx.eq.1 ) then
                FCLT = YL(nsl,n_up1)/(SL(2,n_up1)*PORD(2,n_up1)+SMALL)
                R = ((C(nsl,n)*FCLP-C(nsl,n_up1)*FCLT) &
                 /(BCX(1)*FCL-C(nsl,n)*FCLP+SMALL)) *(distbx/dist_u23)
                THETA = FLIMIT( R,CRLB,ISLC(1) )
                  WCZ = BCX(1)*FLB*THETA*FCL &
                  + C(NSL,n)*FLB*(1.D+0-THETA)*FCLP
              ELSE
                WCZ = C(NSL,n)*FLB*FCLP
              ENDIF
              AB = 0.d0 
              AP = 0.d0
              residual(1,n) = residual(1,n) -xnorm*WCZ

              IF(ISJCB)c_flux_b(nsl,nb) = c_flux_b(nsl,nb)+wcz/areabx
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              icnx = 0
              flt = 0.d0
              loop_ifxo: do ifx = 1,6
                icnx = nd2cnx(ifx,n)
                if(icnx > 0) then
                  v_x = abs(unvxc(icnx))
                  v_y = abs(unvyc(icnx))
                  v_z = abs(unvzc(icnx))
                  if(u_x*v_x.gt.0.d0 .or. u_y*v_y.gt.0.d0 .or. u_z*v_z.gt.0.d0 ) then
                    flt = q_flux(1,icnx)*areac(icnx)
                    exit loop_ifxo
                  endif
                endif
              enddo loop_ifxo
              IF( xnorm*flt.lt.0.d0 ) THEN
                XVLX = SL(2,n_up1)*PORD(2,n_up1)
                FCLT = YL(nsl,n_up1)/(XVLX+SMALL)
                CRLT = ABS( q_flux(1,icnx) )*DT &
                 /( dist_up(icnx)+dist_dn(icnx) ) &
                 /(XVLX+SMALL)
                R = ((C(nsl,N)*FCLP-BCX(1)*FCL) &
                /(C(nsl,n_up1)*FCLT-C(nsl,N)*FCLP+SMALL)) &
                *(dist_u23/distbx)
                THETA = FLIMIT( R,CRLT,ISLC(1) )
                if(xnorm<0.d0) then
                  dzf = dist_dn(icnx)/dist_u23
                else
                  dzf = dist_up(icnx)/dist_u23
                endif
                WCZ = C(nsl,N)*FLT*(1.D+0-THETA*DZF)*FCLP &
                + C(nsl,n_up1)*FLT*THETA*DZF*FCLT
                WCZF = CO(nsl,N)*FLT*FCLP
                residual(1,n) = residual(1,n) + xnorm*(WCZ - WCZF)
                residual(1,n_up1) = residual(1,n_up1) -xnorm*(WCZ - WCZF)
              ENDIF
            ELSE
              ALB = MAX(-1.d0*sign(1,ibcd(nb))*FLB,ZERO)
              AP = (ALB+sign(1,ibcd(nb))*FLB)*FCLP
              AB = ALB*FCL
            ENDIF

            residual(1,n) = residual(1,n) + AB*BCX(1)
!
!--- Inflow ---
!
        ELSEIF( IBCT(NBCT,NB).EQ.13 .OR. IBCT(NBCT,NB).EQ.14    &
           .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23) &
          .AND. (xnorm*FLB.LE.0.D+0)) ) THEN
          if(ibcd(nb) > 0) then
             flb = min(flb,zero)
          else
             flb = max(flb,zero)
          endif
!          flb =  min(xnorm*flb,0.d0)
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 )  THEN
              distbx = distb(nb)
              IF( xnorm*flb.lt.0.d0 ) WCZ = BCX(1)*FCL*FLB
              AB = 0.d0
              AP = 0.d0
              residual(1,n) = residual(1,n) -xnorm*WCZ
              IF(ISJCB)c_flux_b(nsl,nb) = c_flux_b(nsl,nb)+wcz/areabx
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              icnx = 0
              flt = 0.d0
              loop_ifxi: do ifx = 1,6
                icnx = nd2cnx(ifx,n)
                if(icnx > 0) then
                  v_x = abs(unvxc(icnx))
                  v_y = abs(unvyc(icnx))
                  v_z = abs(unvzc(icnx))
                  if(u_x*v_x.gt.0.d0 .or. u_y*v_y.gt.0.d0 .or. u_z*v_z.gt.0.d0 ) then
                    flt = q_flux(1,icnx)*areac(icnx)
                    exit loop_ifxi
                  endif
                endif
              enddo loop_ifxi
              IF( xnorm*flt.lt.0.d0 ) THEN
                XVLX = SL(2,n_up1)*PORD(2,n_up1)
                FCLT = YL(nsl,n_up1)/(XVLX+SMALL)
                CRLT = ABS( q_flux(1,icnx) )*DT &
                 /( dist_up(icnx)+dist_dn(icnx) ) &
                 /(XVLX+SMALL)
                R = ((C(nsl,N)*FCLP-BCX(1)*FCL) &
                /(C(nsl,n_up1)*FCLT-C(nsl,N)*FCLP+SMALL)) &
                *(dist_u23/distbx)
                THETA = FLIMIT( R,CRLT,ISLC(1) )
                if(xnorm<0.d0) then
                  dzf = dist_dn(icnx)/dist_u23
                else
                  dzf = dist_up(icnx)/dist_u23
                endif
                WCZ = C(nsl,N)*FLT*(1.D+0-THETA*DZF)*FCLP &
                + C(nsl,n_up1)*FLT*THETA*DZF*FCLT
                WCZF = CO(nsl,N)*FLT*FCLP
                residual(1,n) = residual(1,n) +xnorm*(WCZ-WCZF)
                residual(1,n_up1) = residual(1,n_up1) - xnorm*(WCZ - WCZF)
              ENDIF
            ELSE
              ALB = MAX(-1.d0*sign(1,ibcd(nb))*FLB,ZERO)
              AP = (ALB+sign(1,ibcd(nb))*FLB)*FCLP
              AB = ALB*FCL
            ENDIF
            residual(1,n) = residual(1,n) + AB*BCX(1)
        ENDIF
!      ICSN = ICSN-ICSNX
!      SUBNM = SUBNM(1:ICSN)
!
!---  End of CBND1 group  ---
!

      RETURN
      END
