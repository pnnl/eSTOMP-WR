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
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/DRCVL'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(51)(1:1),'$').EQ.0 ) CVS_ID(51) = &
      '$Id: drcvl.F,v 1.8 2006/03/30 21:12:18 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
  q_flux(:,:) = 0.d0
!---  Loop over connections
  do iconn=1,num_cnx
    id_up = conn_up(iconn)
    id_dn = conn_dn(iconn)
    if (ixp(id_dn) == 0 .or. ixp(id_up) == 0) cycle
    do m=1,isvf
      mn = mpos(m)
      mp = mneg(m)
      hdl_ = pl(mn,id_dn) - pl(mp,id_up) - 0.5_dp* &
       (gravx*unvxc(iconn)+gravy*unvyc(iconn)+gravz*unvzc(iconn))* &
             (rhol(mp,id_up)*dist_up(iconn)+ &
              rhol(mn,id_dn)*dist_dn(iconn))
      permup_ = (perm(1,id_up)*unvxc(iconn)+ &
                 perm(2,id_up)*unvyc(iconn)+ &
                 perm(3,id_up)*unvzc(iconn))* &
                permrf(mp,id_up)
      permdn_ = (perm(1,id_dn)*unvxc(iconn)+ &
                 perm(2,id_dn)*unvyc(iconn)+ &
                 perm(3,id_dn)*unvzc(iconn))* &
                permrf(mn,id_dn)
      if(m.eq.1) hdl = hdl_
!
!---          Interfacial average intrinsic and relative permeability
!             separately  ---
!
              IF( IDMN(19).EQ.8 ) THEN
                INDX = 11
                KLM_ = DIFMN(PERMup_,PERMdn_,dist_up(iconn),dist_dn(iconn),HDL,INDX)
                INDX = 8
                RKLM_ = DIFMN(rkl(1,mp,id_up),rkl(1,mn,id_dn),dist_up(iconn), &
                dist_dn(iconn),HDL,INDX)
!
!---          Interfacial average intrinsic and relative permeability
!             jointly  ---
!
              ELSE
                INDX = 19
                RKLupX = PERMup_*RKL(1,MN,id_up)
                RKLdnX = PERMdn_*RKL(1,MP,id_dn)
                RKLM_ = DIFMN(RKLupX,RKLdnX,dist_up(iconn),dist_dn(iconn),HDL,INDX)
                KLM_ = 1.D+0
              ENDIF
              IF( PERMup_/EPSL.LT.EPSL ) KLM_ = 0.D+0
              IF( PERMdn_/EPSL.LT.EPSL ) KLM_ = 0.D+0
              INDX = 5
              VLM = DIFMN(VISL(MN,id_up),VISL(MP,id_dn),dist_up(iconn), &
                dist_dn(iconn),HDL,INDX)
      IF(PERMUP_/EPSL < EPSL .OR. PERMDN_/EPSL < EPSL) KLM_ = 0.d0
      q_flux(M,iconn) = HDL_*KLM_*RKLM_/((dist_up(iconn)+dist_dn(iconn))*VLM)
if(niter.eq.2) then 
!print *,'niter==========',niter
!print *,' drcvl-----',iconn,m,q_flux(m,iconn),hdl_,klm_,rklm_,vlm,visl(mn,id_up),visl(mn,id_dn)
endif

    enddo
  enddo
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of DRCVL group  ---
!
      RETURN
      END
