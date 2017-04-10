!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CFLUX1( NSL,ICONN,ID_UP,ID_DN,ICYCLE, &
         FCL_UP,FCL_DN,FLB,DLZ,UCX,ISJCB )
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
!
!     Compute solute transport flux aqueous-phase, excluding boundaries,
!     using either a Patankar scheme or a TVD scheme with  third-order
!     Leonard limiting for the  advective transport component.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on September 5, 1996.
!     $Id: sjcbl.F90,v 1.3 2009/04/28 20:39:17 d3m045 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      use bcvp
      use grid_mod
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
      double precision, dimension(3) :: s_fx_up
      double precision, dimension(3) :: s_fx_dn
      double precision, dimension(3) :: s_area_x 
      logical :: isjcb
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
!      SUBNMX = '/CFLUX'
!      ICSNX = INDEX( SUBNMX,'  ' )-1
!      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
!      IF( INDEX(CVS_ID(196)(1:1),'$').EQ.0 ) CVS_ID(196) = &
!       '$Id: cflux.F90,v 1.3 2009/04/28 20:39:17 d3k870 Exp $' 
!      ICSN = ICSN+ICSNX\
    izn_up = (id_up)
    izn_dn = (id_dn)
    dist_upx = dist_up(iconn)
    dist_dnx = dist_dn(iconn)
    qfluxx = q_flux(1,iconn)
    areax = areac(iconn)
!
!---    Hydrodynamic dispersion coefficients at cell faces  ---
!
    volx_up = vol(id_up)
    volx_dn = vol(id_dn)
    s_fx_up = 0.d0
    s_area_x = 0.d0
    s_fx_dn = 0.d0
    u_x = abs(unvxc(iconn))
    u_y = abs(unvyc(iconn))
    u_z = abs(unvzc(iconn))
    vfx = 0.d0
    vfy = 0.d0
    vfz = 0.d0
!
!--- unstructured grid use vector projection to get area in each direction
!--- average of velocity in each direction, weighted by area    
     
    do ix=1,3
     s_fx_up(ix) = s_fx(ix,id_up)/(s_area(ix,id_up)+1.d-60)
    enddo 
!  
    do ix=1,3
     s_fx_dn(ix) = s_fx(ix,id_dn)/(s_area(ix,id_dn)+1.d-60)
    enddo   
    if(u_x > 0.d0) then
      ubx = q_flux(1,iconn)
      vbx = (s_fx_dn(2)*dist_upx+s_fx_up(2)*dist_dnx)/(dist_upx+dist_dnx)
      wbx = (s_fx_dn(3)*dist_upx+s_fx_up(3)*dist_dnx)/(dist_upx+dist_dnx)
    elseif(u_y > 0.d0) then
      ubx = (s_fx_dn(1)*dist_upx+s_fx_up(1)*dist_dnx)/(dist_upx+dist_dnx)
      vbx = q_flux(1,iconn)
      wbx = (s_fx_dn(3)*dist_upx+s_fx_up(3)*dist_dnx)/(dist_upx+dist_dnx)
    elseif(u_z > 0.d0) then
      ubx = (s_fx_dn(1)*dist_upx+s_fx_up(1)*dist_dnx)/(dist_upx+dist_dnx)
      vbx = (s_fx_dn(2)*dist_upx+s_fx_up(2)*dist_dnx)/(dist_upx+dist_dnx)
      wbx = q_flux(1,iconn)
    endif
    ulbsq = ubx*ubx
    vlbsq = vbx*vbx
    wlbsq = wbx*wbx
    zvb = sqrt(ulbsq+vlbsq+wlbsq)
    indx = 17
    displ_dn = displ((id_dn))*smdef(nsl,id_dn)
    displ_up = displ((id_up))*smdef(nsl,id_up)
    dispt_dn = dispt((id_dn))*smdef(nsl,id_dn)
    dispt_up = dispt((id_up))*smdef(nsl,id_up)
    dplb = difmn(displ_dn,displ_up,dist_dnx,dist_upx,q_flux(1,iconn),indx)
    dptb = difmn(dispt_dn,dispt_up,dist_dnx,dist_upx,q_flux(1,iconn),indx)
    aux = abs(u_x)
    avx = abs(u_y)
    awx = abs(u_z)
    dpb = (dplb*(ulbsq*aux+vlbsq*avx+wlbsq*awx) + &
       dptb*(ulbsq+vlbsq+wlbsq-(ulbsq*aux+vlbsq*avx+wlbsq*awx)))/(zvb+small)
!
!---        Molecular diffusion coefficients at the nodes  ---
!
    tcor_up = (t(2,id_up)+tabs)/tsprf
    sdfl_up = smdl(nsl)*tcor_up*(visrl/visl(2,id_up))
    vmc_up = sl(2,id_up)*pord(2,id_up)
    fcl_up = yl(nsl,id_up)/(vmc_up+small)
    if( iedl(nsl).eq.2 ) then
      dl_up = sdcl(1,nsl,izn_up)*sdcl(2,nsl,izn_up)* &
              exp(vmc_up*sdcl(3,nsl,izn_up))
    elseif( iedl(nsl).eq.3 ) then
      dl_up = torl(2,id_up)*vmc_up*smdl(nsl)
    elseif( iedl(nsl).eq.4) then
      dl_up = sdcl(1,nsl,izn_up)*sdcl(2,nsl,izn_up)* &
        vmc_up**sdcl(3,nsl,izn_up)
    else
      dl_up = torl(2,id_up)*vmc_up*sdfl_up
    endif
    if( ixp(id_dn).le.0 ) then
      icycle = 1
      return
    endif
    tcor_dn = (t(2,id_dn)+tabs)/tsprf
    sdfl_dn = smdl(nsl)*tcor_dn*(visrl/visl(2,id_dn))
    vmc_dn = sl(2,id_dn)*pord(2,id_dn)
    fcl_dn = yl(nsl,id_dn)/(vmc_dn+small)
    if( iedl(nsl).eq.2 ) then
      dl_dn = sdcl(1,nsl,izn_dn)*sdcl(2,nsl,izn_dn)* &
              exp(vmcb*sdcl(3,nsl,izn_dn))
    elseif( iedl(nsl).eq.3 ) then
      dl_dn = torl(2,id_dn)*vmc_dn*smdl(nsl)
    elseif( iedl(nsl).eq.4) then
      dl_dn = sdcl(1,nsl,izn_dn)*sdcl(2,nsl,izn_dn)* &
        vmc_dn**sdcl(3,nsl,izn_dn)
    else
      dl_dn = torl(2,id_dn)*vmc_dn*sdfl_dn
    endif
    indx = 16
    dlz = difmn(dl_dn,dl_up,dist_dnx,dist_upx,qfluxx,indx)
    if(isjcb)then
      dlz = areax*(dlz+dpb)/(dist_dnx+dist_upx)
      flb = areax*q_flux(1,iconn)
    else
      dlz = (dlz+dpb)/(dist_dnx+dist_upx)
      flb = q_flux(1,iconn)
    endif
    if( mod(islc(23),10).eq.1 ) flb = 0.d+0
    vmcbx = difmn(vmc_dn,vmc_up,dist_dnx,dist_upx,qfluxx,indx)
    crlb = abs(qfluxx)*dt/((dist_dnx+dist_upx)*vmcbx+small)
!
!--- TVD solute transport  ---
!
    if( islc(1).ge.1 ) then
      ldx = iaxmax - iaxmin + 1
      ldy = iaymax - iaymin + 1
      ldz = iazmax - iazmin + 1
      i_ = id_dn - 1
      ix = mod(i_,ldx)
      i_ = (i_-ix)/ldx
      jx = mod(i_,ldy)
      kx = (i_-jx)/ldy
      ix = ix + 1
      jx = jx + 1
      kx = kx + 1
      if(u_x .gt.0.d0) then
          n_dn1 = id_dn - 1
          n_up1 = id_up
          n_up2 = id_dn + 2
          dist_d12 = dist_upx+dist_dnx
          if(n_dn1>0)dist_un12 = xp(id_dn) - xp(n_dn1)
          if(ix+2<=ldx)dist_up23 = xp(n_up2) - xp(n_up1)
      elseif(u_y.gt.0.d0) then
          n_dn1 = id_dn - ldx
          n_up1 = id_up
          n_up2 = id_dn + 2*ldx
          dist_d12 = dist_upx+dist_dnx
          if(n_dn1>0)dist_un12 = yp(id_dn) - yp(n_dn1)
          if(jx+2<=ldy)dist_up23 = yp(n_up2) - yp(n_up1)
      elseif(u_z.gt.0.d0) then
          ldxy = ldx*ldy
          n_dn1 = id_dn - ldxy
          n_up1 = id_up
          n_up2 = id_dn + 2*ldxy
          dist_d12 = dist_upx+dist_dnx
          if(n_dn1>0)dist_un12 = zp(id_dn) - zp(n_dn1)
          if(kx+2<=ldz)dist_up23 = zp(n_up2) - zp(n_up1)
      endif
      ucx = 0.d0
      if( flb.ge.0.d0 ) then
        i_procx = 0
        if( u_x.gt.0.d0 .and. ((ix.ne.1.and.ixmin.eq.1).or.ixmin.gt.1) ) then
          i_procx = 1
        elseif( u_y.gt.0.d0 .and. ((jx.ne.1.and.iymin.eq.1).or.iymin.gt.1) ) then
          i_procx = 1
        elseif( u_z.gt.0.d0 .and. ((kx.ne.1.and.izmin.eq.1).or.izmin.gt.1) ) then
          i_procx = 1
        endif
        if( i_procx .eq. 1 ) then
          if( ixp(n_dn1).gt.0 ) then
            fclw = yl(nsl,n_dn1)/(sl(2,n_dn1)*pord(2,n_dn1)+small)
            rx = (co(nsl,id_dn)*fcl_dn-co(nsl,n_dn1)*fclw) &
                   /(1.d15*(co(nsl,id_up)*fcl_up-co(nsl,id_dn)*fcl_dn)+small*1.d15)*1.d15 &
                   *(dist_d12/dist_un12)
            theta = flimit( rx,crlb,islc(1) )
            dxf = dist_dnx/dist_d12
            ucx =  co(nsl,id_up)*flb*theta*dxf*fcl_up &
                  + co(nsl,id_dn)*flb*(1.d+0-theta*dxf)*fcl_dn
          else
            ucx =  co(nsl,id_dn)*flb*fcl_dn
          endif
        else
          ucx = co(nsl,id_dn)*flb*fcl_dn
        endif
      elseif( flb.lt.0.d0 ) then
        i_procx = 0
        if( u_x.gt.0.d0 .and. ((ix.lt.ldx-1.and.ixmax.eq.nxdim).or.ixmax.lt.nxdim) ) then
          i_procx = 1
        elseif( u_y.gt.0.d0 .and. ((jx.lt.ldy-1.and.iymax.eq.nydim).or.iymax.lt.nydim) ) then
          i_procx = 1
        elseif( u_z.gt.0.d0 .and. ((kx.lt.ldz-1.and.izmax.eq.nzdim).or.izmax.lt.nzdim) ) then
          i_procx = 1
        endif
        if( i_procx .eq. 1 ) then
          if( ixp(n_up2).gt.0 ) then
            fclee = yl(nsl,n_up2)/(sl(2,n_up2)*pord(2,n_up2)+small)
            r = (co(nsl,id_up)*fcl_up-co(nsl,n_up2)*fclee) &
                 /(small*1.d15+1.d15*(co(nsl,id_dn)*fcl_dn-co(nsl,id_up)*fcl_up))*1.d15 &
                 *(dist_d12/dist_up23)
            theta = flimit( r,crlb,islc(1) )
            dxf = dist_upx/dist_d12
            ucx = co(nsl,id_up)*flb*(1.d+0-theta*dxf)*fcl_up &
                  + co(nsl,id_dn)*flb*theta*dxf*fcl_dn
          else
            ucx = co(nsl,id_up)*flb*fcl_up
          endif
        else
          ucx = co(nsl,id_up)*flb*fcl_up
        endif
      endif
      if(isjcb)c_flux(nsl,iconn) = ucx/areax
    endif
!
!---  End of CFLUX group  ---
!
!      ICSN = ICSN-ICSNX
!      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

