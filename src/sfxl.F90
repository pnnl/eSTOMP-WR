!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SFXL( NSL )
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
      LOGICAL :: use_ga
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
      SUBNMX = '/SFXL'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(196)(1:1),'$').EQ.0 ) CVS_ID(196) = &
       '$Id: sfxl.F90,v 1.3 2009/04/28 20:39:17 d3m045 Exp $' 
      ICSN = ICSN+ICSNX

  isjcb = .false.
  do iconn = 1,num_cnx
!
!--- Initialize flux conc array
!
    ICYCLE = 0
    ID_UP = CONN_UP(ICONN)
    ID_DN = CONN_DN(ICONN)
    IF (IXP(ID_DN) == 0 .OR. IXP(ID_UP) == 0) CYCLE
    CALL CFLUX1 (NSL,ICONN,ID_UP,ID_DN,ICYCLE, &
      FCL_UP,FCL_DN,FLB,DLZ,UCX,ISJCB)
    IF( ICYCLE.EQ.1 )CYCLE
!
!--- TVD solute transport  ---
!
    IF( ISLC(1).GE.1 ) THEN
      C_FLUX(NSL,ICONN) = C_FLUX(NSL,ICONN) +  &
        DLZ*(C(NSL,ID_DN)*FCL_DN - C(NSL,ID_UP)*FCL_UP)
    ELSE
     C_FLUX (NSL,ICONN) = 0.D0
!
!--- Patankar solute transport  ---
!
      ALUX = MAX(-FLB,ZERO) !UP AS EAST
      ALDX = MAX(FLB,ZERO) !DN AS NODE
      ALDF = DLZ*MAX((ONE-(TENTH*ABS(FLB)/(DLZ+SMALL)))**5,ZERO)
      AL = ALUX+ALDF
      ALP = ALDX+ALDF
      C_FLUX(NSL,ICONN) = C_FLUX(NSL,ICONN) + &
        C(NSL,ID_DN)*FCL_DN*ALP - C(NSL,ID_UP)*AL*FCL_UP
    ENDIF
!--- prepare for surface flux output
    idr_x = unvxc(iconn)
    idr_y = unvyc(iconn)
    idr_z = unvzc(iconn)
    if(idr_x /= 0) then
      c_flux_nd(1,nsl,id_up) = c_flux(nsl,iconn)
    elseif(idr_y /= 0) then
      c_flux_nd(2,nsl,id_up) = c_flux(nsl,iconn)
    elseif(idr_z /= 0) then
      c_flux_nd(3,nsl,id_up) = c_flux(nsl,iconn)
    endif
  ENDDO
!
!---  End of SFXL group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

