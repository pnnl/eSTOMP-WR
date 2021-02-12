!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DRCVLB( N,NB )
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
!     Compute the aqueous-phase Darcy flux from pressure gradients
!     and gravitational body forces on a boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1993.
!     Last Modified by MD White, PNNL, December 13, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      USE BCVP
      USE BCV
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!

      LOGICAL :: use_ga
      REAL*8:: vdr(3),max_idr
      integer:: id_max
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/DRCVLB'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(52)(1:1),'$').EQ.0 ) CVS_ID(52) = &
      '$Id: drcvlb.F,v 1.15 2007/01/17 19:38:01 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      DO 100 M = 1,ISVF
!        MP = MNEG(M)
        mp = mpos(m)
!       write(*,'(a,3I6,3F16.8)') 'me,N,nb,vx,vy,vz: ',me,N,NB,uvxb(nb),uvyb(nb),uvzb(nb)
!       write(*,'(a,2I6,3F16.8)') 'me,N,xc,yc,zc: ',me,N,d_xc(n),d_yc(n),d_zc(n)
! to be replaced with surface normal direction
       if (ics /= 3 .AND. ics /= 8) then ! BH
        if(abs(uvxb(nb)).gt.1.d-20) then
          xnorm = uvxb(nb)
        elseif(abs(uvyb(nb)).gt.1.d-20) then
          xnorm = uvyb(nb)
        elseif(abs(uvzb(nb)).gt.1.d-20) then
          xnorm = uvzb(nb)
        endif
!BH
       else
        vdr(1) = uvxb(nb)
        vdr(2) = uvyb(nb)
        vdr(3) = uvzb(nb)
        max_idr = 0.0
        id_max = 0
        do i_dir = 1, 3
          if (abs(vdr(i_dir))>=max_idr) then
            max_idr = abs(vdr(i_dir))
            id_max = i_dir
          endif
        enddo
        xnorm = vdr(id_max)
       endif
!BH
!        IF( IBCD(NB).LT.0 )THEN
!          RKLU = RKLB(1,MP,NB)
!          RKLX = RKL(1,MP,N)
!        ELSE
!          RKLU = RKL(1,MP,N)
!          RKLX = RKLB(1,MP,NB)
!        ENDIF
        HX = xnorm*(PL(MP,N)-PLB(MP,NB) - &
               (uvxb(nb)*gravx+uvyb(nb)*gravy+uvzb(nb)*gravz) &
                *rholb(mp,nb)*distb(nb))
        if(xnorm > 0.d0) then
         rkl_dn = dabs(rkl(1,mp,n)*uvxb(nb)+rkl(2,mp,n)*uvyb(nb)+rkl(3,mp,n)*uvzb(nb))
         rkl_up = dabs(rklb(1,mp,nb)*uvxb(nb)+rklb(2,mp,nb)*uvyb(nb)+rklb(3,mp,nb)*uvzb(nb))
         visl_dn = visl(mp,n)
         visl_up = vislb(mp,nb)
        else
         rkl_dn = dabs(rklb(1,mp,nb)*uvxb(nb)+rklb(2,mp,nb)*uvyb(nb)+rklb(3,mp,nb)*uvzb(nb))
         rkl_up = dabs(rkl(1,mp,n)*uvxb(nb)+rkl(2,mp,n)*uvyb(nb)+rkl(3,mp,n)*uvzb(nb))
         visl_dn = vislb(mp,nb)
         visl_up = visl(mp,n)
        endif
        IF( M.EQ.1 ) HD = HX
        INDX = 8
!       RKM = DIFMN(RKL(1,MP,N),RKLB(1,MP,NB),distb(nb),distb(nb),HD,INDX)
        RKM = DIFMN(RKL_dn,RKL_up,distb(nb),distb(nb),HD,INDX)
        INDX = 5
        VM = DIFMN(VISL_dn,VISL_up,distb(nb),distb(nb),HD,INDX)
!if(mp.eq.3) then
!print *,'rkm---',rkm,vm,vislb(mp,nb),visl(mp,n),distb(nb),hd
!endif
        PERM_P = abs(PERMRF(MP,N)*(PERM(1,N)*uvxb(nb)+ &
                 perm(2,n)*uvyb(nb) + perm(3,n)*uvzb(nb)))
        q_flux_b(M,NB) = PERM_P*RKM*HX/(VM*distb(nb))
        q_flux_bw = q_flux_b(m,nb)

!        if(nb.eq.1903) print *,'goto drc-',nb,perm_p,rkm,hx,vm,'pres',plb(mp,nb),pl(mp,n)
!
!---    Outflow, diode or potential-evaporation
!       boundary condition  ---
!
        IF( ABS(IBCT(IEQW,NB)).EQ.7 .OR. &
        ABS(IBCT(IEQW,NB)).EQ.23 ) THEN
          IF( IBCD(NB).LT.0 )THEN
            q_flux_b(M,NB) = MIN( 0.D+0,q_flux_b(M,NB) )
          ELSE
            q_flux_b(M,NB) = MAX( 0.D+0,q_flux_b(M,NB) )
          ENDIF
!
!---    Seepage or x-y-z seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17  &
       .OR. ABS(IBCT(IEQW,NB)).EQ.45 ) THEN
           IF( PG(MP,N)-PL(MP,N).GT.EPSL ) THEN
             IF( PLB(MP,NB)-PGB(MP,NB).GT.EPSL ) THEN
               if(xnorm.gt.0.d0) then
                 q_flux_b(M,NB) = min( 0.D+0,q_flux_b(M,NB) )
               else
                 q_flux_b(M,NB) = max( 0.D+0,q_flux_b(M,NB) )
               endif
             ELSE
               q_flux_b(M,NB) = 0.D+0
             ENDIF
           ELSE
             IF( PLB(MP,NB)-PGB(MP,NB).LE.EPSL ) THEN
               if(xnorm.gt.0.d0) then
                 q_flux_b(M,NB) = max( 0.D+0,q_flux_b(M,NB) )
               else
                 q_flux_b(M,NB) = min( 0.D+0,q_flux_b(M,NB) )
               endif
             ENDIF
           ENDIF
!
!---    Shuttleworth-Wallace boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
          IF( PL(MP,N)-PG(2,N).GE.EPSL ) THEN
            q_flux_b(M,NB) = MIN( 0.D+0,q_flux_b(M,NB) )
          ELSE
            q_flux_b(M,NB) = 0.D+0
          ENDIF
        ENDIF
!print *,'q_flux_b-++++++',nb,q_flux_b(m,nb),'n=',n,pl(mp,n),plb(mp,nb),vm, &
!  perm_p,hx,distb(nb),rkm,mp,rholb(mp,nb),uvxb(nb),uvzb(nb)
!if(nstep.eq.30.and.abs(ibct(ieqw,nb)).eq.17) print *,'plb,pgb',me,nb,m,q_flux_b(m,nb),loc2nat(n),niter
!if(nstep.ge.26.and.abs(ibct(ieqw,nb)).eq.11) &
! print *,'plb,pgb',me,nb,m,pl(mp,n),pg(2,n),plb(mp,nb),pgb(mp,nb),loc2nat(n),niter,q_flux_b(m,nb),q_flux_bw
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of DRCVLB group  ---
!
      RETURN
      END
