!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DRCVL
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
!     and gravitational body forces.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, January, 1992.
!     Last Modified by MD White, PNNL, December 13, 1995.
!     Last Modified by MD White, PNNL, 16 January 2002.
!     Last Modified by MD White, PNNL, 29 May 2002.
!     $Id: drcvl.F,v 1.8 2006/03/30 21:12:18 d3c002 Exp $
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
!      USE FDVS
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 KLM_
      REAL*8:: idr(3),max_idr
      integer:: id_max
!
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!



!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      SUBNMX = '/DRCVL'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(51)(1:1),'$').EQ.0 ) CVS_ID(51) = &
      '$Id: drcvl.F,v 1.8 2006/03/30 21:12:18 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
  q_flux(:,:) = 0.d0
!---  Loop over connections
!  write(*,*) 'me,num_cnx',me,num_cnx
  do iconn=1,num_cnx
    id_up = conn_up(iconn)
    id_dn = conn_dn(iconn)
    if (ixp(id_dn) == 0 .or. ixp(id_up) == 0) cycle
!    write(*,*) 'me,iconn,isvf: ',me,iconn,isvf
    do m=1,isvf
      mn = mpos(m)
      mp = mneg(m)
!      write(*,'(a,4I8X)') 'me,icoon,down,up:',me,iconn,id_dn,id_up
!      write(*,*) 'norms: ',unvxc(iconn),unvyc(iconn),unvzc(iconn)
      hdl_ = pl(mn,id_dn) - pl(mp,id_up) - &
       (gravx*unvxc(iconn)+gravy*unvyc(iconn)+gravz*unvzc(iconn))* &
             (rhol(mp,id_up)*dist_dn(iconn)+ &
              rhol(mn,id_dn)*dist_up(iconn))
      permup_ = (perm(1,id_up)*unvxc(iconn)+ &
                 perm(2,id_up)*unvyc(iconn)+ &
                 perm(3,id_up)*unvzc(iconn))* &
                permrf(mp,id_up)
      permdn_ = (perm(1,id_dn)*unvxc(iconn)+ &
                 perm(2,id_dn)*unvyc(iconn)+ &
                 perm(3,id_dn)*unvzc(iconn))* &
                permrf(mn,id_dn)
      rklup_ = rkl(1,mp,id_up)*unvxc(iconn)+ &
                 rkl(2,mp,id_up)*unvyc(iconn)+ &
                 rkl(3,mp,id_up)*unvzc(iconn)
      rkldn_ = rkl(1,mn,id_dn)*unvxc(iconn)+ &
                 rkl(2,mn,id_dn)*unvyc(iconn)+ &
                 rkl(3,mn,id_dn)*unvzc(iconn)

      if(m.eq.1) hdl = hdl_
!
!---          Interfacial average intrinsic and relative permeability
!             separately  ---
!

              IF( IDMN(19).EQ.8 ) THEN
                INDX = 11
                KLM_ = DIFMN(PERMdn_,PERMup_,dist_dn(iconn),dist_up(iconn),HDL,INDX)
                INDX = 8
                RKLM_ = DIFMN(rkldn_,rklup_,dist_dn(iconn), &
                dist_up(iconn),HDL,INDX)
!
!---          Interfacial average intrinsic and relative permeability
!             jointly  ---
!
              ELSE
                INDX = 19
                RKLupX = PERMup_*RKLup_
                RKLdnX = PERMdn_*RKLdn_
                RKLM_ = DIFMN(RKLdnX,RKLupX,dist_dn(iconn),dist_up(iconn),HDL,INDX)
                KLM_ = 1.D+0
              ENDIF
              IF( PERMup_/EPSL.LT.EPSL ) KLM_ = 0.D+0
              IF( PERMdn_/EPSL.LT.EPSL ) KLM_ = 0.D+0
              INDX = 5
              VLM = DIFMN(VISL(mn,id_dn),VISL(mp,id_up),dist_dn(iconn), &
                dist_up(iconn),HDL,INDX)
      IF(PERMUP_/EPSL < EPSL .OR. PERMDN_/EPSL < EPSL) KLM_ = 0.d0
      q_flux(M,iconn) = HDL_*KLM_*RKLM_/((dist_up(iconn)+dist_dn(iconn))*VLM)
!--- prepare for surface flux output
!BH q_flux_nd needs to be fixed for bfg mesh, do it later 09/14/2020

      idr(1) = unvxc(iconn)
      idr(2) = unvyc(iconn)
      idr(3) = unvzc(iconn)
     if (ics/=3 .AND. ics/=8 ) then
      if(idr(1) /= 0) then
        q_flux_nd(1,id_up) = q_flux(1,iconn)
      elseif(idr(2) /= 0) then
        q_flux_nd(2,id_up) = q_flux(1,iconn)
      elseif(idr(3)/= 0) then
        q_flux_nd(3,id_up) = q_flux(1,iconn)
      endif
!BH
     else
      max_idr = 0.0
      id_max = 0
      do i_dir = 1, 3
         if (abs(idr(i_dir))>=max_idr) then
            max_idr = abs(idr(i_dir))
            id_max = i_dir
         endif
      enddo
      q_flux_nd(id_max,id_up) = q_flux(1,iconn)
     endif 
!BH
    enddo
  enddo
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of DRCVL group  ---
!
      RETURN
      END
