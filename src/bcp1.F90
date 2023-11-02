

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BCP1
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
!     Compute saturation, relative permeability and thermodynamic
!     properties for boundary surfaces.
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
      USE HYST
      USE GRID
      USE FDVP
      USE CONST
      USE BCVP
      USE BCV
      use grid_mod
      use grid
      USE PLT_ATM
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

     

      interface
        subroutine get_bcnx_ifield(t_string, t_field,  &
                t_ok)
        use grid_mod
        implicit none
        character (len=*) :: t_string ! in
        integer, pointer :: t_field(:) ! out
        logical :: t_ok ! out
        end subroutine get_bcnx_ifield
      end interface


!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 BCX(LBCV)
      real*8 pgbx(isvc+2),tbx(isvc+2)
      real*8, dimension(:,:), allocatable :: beta
      integer, dimension(:), allocatable :: ncntx
      integer :: nrfx
      LOGICAL :: use_ga,t_ok
      integer, pointer ::i_bid(:), i_bidi(:), i_bidj(:),i_bidk(:)
      character(len=64):: format_110
      integer :: pft
      integer :: ix_x,iy_x,iz_x
      REAL*8 :: c_coef 
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/BCP1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Assign values for initial condition type boundary conditions  ---
      ijfld = (iaxmax-iaxmin+1)*(iaymax-iaymin+1)
      ifld = iaxmax-iaxmin+1
!
      IF( NSTEP-NRST.LE.1 ) THEN
      DO 50 NB = 1,num_bcnx
!        MB = IBCIN(NB)
        IF( IBCT(IEQW,NB).EQ.12 ) THEN
          N = IBCN(NB)
          dirx = distb(nb)*(uvxb(nb)+uvyb(nb)+uvzb(nb))
          DB = sign(dirx,-1.d0*dirx/abs(dirx))
          GB = abs(gravx*uvxb(nb)+gravy*uvyb(nb)+gravz*uvzb(nb))*DB

          IF( IBCT(IEQW,NB).EQ.12 ) PLB(1,NB) = PL(2,N) + RHOL(2,N)*GB
        ENDIF
   50 CONTINUE
      ENDIF
!
!---  Loop over boundary conditions  ---
!
! BH get global index for boundary cells
        call get_bcnx_ifield('bcnx_id',i_bid,t_ok)
!        ib = i_bid(nb)
      nrfx = size(base_node,1)
      if(nrfx >0) then
       allocate(ncntx(nrfx))
       ncntx = 0
      endif
      nhdcx = 0
!      write(*,*) 'me,num_bcnx',me,num_bcnx
      if(lplant > 0) veg_bc(:) = 0.d0
      DO 400 NB = 1,num_bcnx
        ib = i_bid(nb)
        TMZ = TM
        IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
        MB = IBCIN(NB)
        IF( IBCC(NB).EQ.1 ) TMZ = MOD( TM,BC(1,IBCM(NB),MB) )
        IF( TMZ.LE.BC(1,1,MB) ) GOTO 400
!
!---  Assign local boundary condition variables  ---
!
        IF( IBCM(NB).EQ.1 ) THEN
          DO 80 N = 1,LBCV
            BCX(N) = BC(N,1,MB)
            irefbx = irefb(nb)

            if(n.eq.2.and.irefbx.gt.0) then
              nhdcx = irefbx
              bcxpxx = bcx(n)
            endif
   80     CONTINUE
        ELSE
          irefbx = irefb(nb)
          DO 100 M = 2,IBCM(NB)
            IF( TMZ.LE.BC(1,M,MB) ) THEN
             TDBC = (BC(1,M,MB)-BC(1,M-1,MB))
             DTBC = MIN( BC(1,M,Mb)-TMZ,DT )
             TFBC = (TMZ-BC(1,M-1,Mb))/TDBC
             DO 90 N = 1,LBCV
               BCX(N) = BC(N,M-1,MB) + TFBC*(BC(N,M,MB)-BC(N,M-1,MB))
               if(n.eq.2.and.irefbx.gt.0 ) then
                 nhdcx = irefbx
                 bcxpxx = bcx(n)
               endif
   90        CONTINUE
             IF( IBCT(IEQW,NB).EQ.2 ) THEN
               BCX(2) = BCX(2)-5.D-1*DTBC*(BC(2,M,MB)-BC(2,M-1,MB))/TDBC
             ENDIF
!             if(n.eq.2.and.basen(irefbx).eq.1 .and. nhdcx.ne.irefbx) then
!             if(n.eq.2.and.basen(irefbx).eq.1 ) then

             GOTO 110
            ENDIF
  100     CONTINUE
          GOTO 400
        ENDIF
  110   CONTINUE
!
!---    Initial condition boundary condition  ---
!
        IF( IBCT(IEQW,NB).EQ.12 ) BCX(2) = PLB(1,NB)
        N = IBCN(NB)
!        IZN = IZ(N)
        IBD = ABS(IBCD(NB))

!        POR(5,IZN) = POR_M(1,N)
!        POR(6,IZN) = POR_M(2,N)

!!
!!---  Check for constant boundary conditions (Dirichlet,
!!     hydraulic gradient, seepage face)  ---
!!
!        IF( IBCC(NB).EQ.1 ) THEN
!          IF( IBCT(IEQW,NB).EQ.2 ) GOTO 400
!          IF( IBCT(IEQW,NB).EQ.11 ) GOTO 400
!          IF( IBCT(IEQW,NB).EQ.17 ) GOTO 400
!        ENDIF
!
!---  Boundary Direction  ---
!
        dirx = distb(nb)*(uvxb(nb)+uvyb(nb)+uvzb(nb))
        DB = sign(dirx,-1.d0*dirx/abs(dirx))
        GB = abs(gravx*uvxb(nb)+gravy*uvyb(nb)+gravz*uvzb(nb))*DB
!
!---  Loop over secondary variable indices  ---
!
        DO 300 M = 2,ISVC+2
          PLX = PL(M,N)
          PGX = PG(2,N)
          TX = T(2,N)
!
!---  Aqueous Dirichlet  ---
!
          IF( IBCT(IEQW,NB).EQ.1 .OR. IBCT(IEQW,NB).EQ.12 ) THEN
            PLX = BCX(2)
!
!---  Aqueous Neumann  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.2 ) THEN
            PLX = PLX + BCX(2)*DB*VISL(M,N)/PERM(IBD,N) &
            + RHOL(M,N)*GB
!
!---  Aqueous Zero Flux  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.3 ) THEN
            PLX = PLX + RHOL(M,N)*GB
!
!---  Aqueous Saturated  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.4 ) THEN
            PLX = PGX
!
!---  Aqueous Unit Gradient  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.5 ) THEN
            PLX = PLX
!
!---  Aqueous Free Gradient  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.6 ) THEN
            N = IBCN(NB)
            IF( IBCD(NB).EQ.-3 ) THEN
              ICNX = ABS(ND2CNX(5,N))
              IF ( ICNX.EQ.0 )THEN
                 ICNX = ND2CNX(6,N)
                 XDIST = 2.D0*DIST_DN(ICNX)
              ELSE
                 XDIST = 2.D0*DIST_UP(ICNX)
              ENDIF
              IF( IXP(N+IJFLD).NE.0 ) THEN
                PCT = PL(1,N+IJFLD)
                PCP = PL(1,N)
!               PLX = PCP + 0.5D+0*(PCP-PCT)*DZGF(N)/DZGP(NQZ)
                PLX = PCP + 0.5D+0*(PCP-PCT)*2.0D0*DB/XDIST
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.-2 ) THEN
              ICNX = ABS(ND2CNX(3,N))
              IF ( ICNX.EQ.0 )THEN
                 ICNX = ND2CNX(4,N)
                 XDIST = 2.D0*DIST_DN(ICNX)
              ELSE
                 XDIST = 2.D0*DIST_UP(ICNX)
              ENDIF
              IF( IXP(N+IFLD).NE.0 ) THEN
                PCN = PL(1,N+IFLD)
                PCP = PL(1,N)
!               PLX = PCP + 0.5D+0*(PCP-PCN)*DYGF(N)/DYGP(NQY)
                PLX = PCP + 0.5D+0*(PCP-PCN)*2.0D0*DB/XDIST
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.-1 ) THEN
              ICNX = ABS(ND2CNX(1,N))
              IF ( ICNX.EQ.0 )THEN
                 ICNX = ND2CNX(2,N)
                 XDIST = 2.D0*DIST_DN(ICNX)
              ELSE
                 XDIST = 2.D0*DIST_UP(ICNX)
              ENDIF
              IF( IXP(N+1).NE.0 ) THEN
                PCE = PL(1,N+1)
                PCP = PL(1,N)
!               PLX = PCP + 0.5D+0*(PCP-PCE)*DXGF(N)/DXGP(NQX)
                PLX = PCP + 0.5D+0*(PCP-PCE)*2.0D0*DB/XDIST
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.1 ) THEN
              ICNX = ABS(ND2CNX(2,N))
              IF ( ICNX.EQ.0 )THEN
                 ICNX = ND2CNX(1,N)
                 XDIST = 2.D0*DIST_UP(ICNX)
              ELSE
                 XDIST = 2.D0*DIST_DN(ICNX)
              ENDIF
              IF( IXP(N-1).NE.0 ) THEN
                PCW = PL(1,N-1)
                PCP = PL(1,N)
!               PLX = PCP + 0.5D+0*(PCP-PCW)*DXGF(N)/DXGP(NQX)
                PLX = PCP + 0.5D+0*(PCP-PCW)*2.0D0*DB/XDIST
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.2 ) THEN
              ICNX = ABS(ND2CNX(4,N))
              IF ( ICNX.EQ.0 )THEN
                 ICNX = ND2CNX(3,N)
                 XDIST = 2.D0*DIST_UP(ICNX)
              ELSE
                 XDIST = 2.D0*DIST_DN(ICNX)
              ENDIF
              IF( IXP(N-IFLD).NE.0 ) THEN
                PCS = PL(1,N-IFLD)
                PCP = PL(1,N)
!               PLX = PCP + 0.5D+0*(PCP-PCS)*DYGF(N)/DYGP(NQY)
                PLX = PCP + 0.5D+0*(PCP-PCS)*2.0D0*DB/XDIST
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.3 ) THEN
              ICNX = ABS(ND2CNX(6,N))
              IF ( ICNX.EQ.0 )THEN
                 ICNX = ND2CNX(5,N)
                 XDIST = 2.D0*DIST_UP(ICNX)
              ELSE
                 XDIST = 2.D0*DIST_DN(ICNX)
              ENDIF
              ICNX = ABS(ND2CNX(6,NB))
              IF( IXP(N-IJFLD).NE.0 ) THEN
                PCB = PL(1,N-IJFLD)
                PCP = PL(1,N)
!               PLX = PCP + 0.5D+0*(PCP-PCB)*DZGF(N)/DZGP(NQZ)
                PLX = PCP + 0.5D+0*(PCP-PCB)*2.0D0*DB/XDIST
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ENDIF
!
!---  Aqueous Outflow  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.7 ) THEN
            PLX = BCX(2)
!
!---  Aqueous Free Boundary  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
            IF( IBCD(NB).EQ.-3 .AND. IXP(N+IJFLD).NE.0 ) THEN
              K = KD(N)
              NA = N+IJFLD
              ICNX = ABS(ND2CNX(5,N))
              XDIST = 2.D+0*DIST_UP(ICNX)
              DGP = XP(NA) - XP(N)
!              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DZGP(NPZ)/ &
!              DZGP(NQZ)
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DGP/ &
               XDIST 
            ELSEIF( IBCD(NB).EQ.-2 .AND. IXP(N+IFLD).NE.0 ) THEN
              J = JD(N)
              NA = N+IFLD
              ICNX = ABS(ND2CNX(3,N))
              XDIST = 2.D+0*DIST_UP(ICNX)
              DGP = XP(NA) - XP(N)
!              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DYGP(NPY)/ &
!                DYGP(NQY)
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DGP/ &
               XDIST 
            ELSEIF( IBCD(NB).EQ.-1 .AND. IXP(N+1).NE.0 ) THEN
              I = ID(N)
              NA = N+1
              ICNX = ABS(ND2CNX(1,N))
              XDIST = 2.D+0*DIST_UP(ICNX)
              DGP = XP(NA) - XP(N)
!             PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DXGP(NPX)/ &
!             DGP(NQX)
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DGP/ &
               XDIST 
            ELSEIF( IBCD(NB).EQ.1 .AND. IXP(N-1).NE.0 ) THEN
              I = ID(N)
              NA = N-1
              ICNX = ABS(ND2CNX(2,N))
              XDIST = 2.D+0*DIST_DN(ICNX)
              DGP = XP(N) - XP(NA)
!              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DXGP(NQX)/ &
!              DXGP(NPX)
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DGP/ &
               XDIST 
            ELSEIF( IBCD(NB).EQ.2 .AND. IXP(N-IFLD).NE.0 ) THEN
              J = JD(N)
              NA = N-IFLD
              ICNX = ABS(ND2CNX(4,N))
              XDIST = 2.D+0*DIST_DN(ICNX)
              DGP = XP(N) - XP(NA)
!              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DYGP(NQY)/ &
!              DYGP(NPY)
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DGP/ &
               XDIST 
            ELSEIF( IBCD(NB).EQ.3 .AND. IXP(N-IJFLD).NE.0 ) THEN
              K = KD(N)
              NA = N-IJFLD
              ICNX = ABS(ND2CNX(6,N))
              XDIST = 2.D+0*DIST_DN(ICNX)
              DGP = XP(N) - XP(NA)
!              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DZGP(NQZ)/ &
!              DZGP(NPZ)
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DGP/ &
               XDIST 
            ENDIF
            PLX = PGX
!
!---  Aqueous Hydraulic Gradient  ---
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.11 ) THEN
!!            IF( IBCT(IEQW,NB).EQ.-11 ) THEN
!              PLX = BCX(2)
            if(ncntx(irefbx) == 0) then
              ncntx(irefbx) = 1
!              nwx = base_node(irefbx) 
              zbs = basez(irefbx)
              plx = bcxpxx
              plxb = plx
              tbx = t(m,n)
!--- Create table for pressure interpolation
              hgz_table_p(1:nzdim,irefbx) = 0.d0
!              call locate(hgz_table_z,nzdim,zbs,ix)
!              hgz_table_z(ix) = zbs
              do i=1,nzdim
!                if(hgz_table_z(i) < zbs) then
!                  hgz_table_p(i,irefbx) = 0.d0
                if(hgz_table_z(i) == zbs) then
                  hgz_table_p(i,irefbx) = bcxpxx
                  plxb_ = bcxpxx
                  tx = t(m,n)
                  tbx = tx
                  ibs_ = i
                  exit
                endif
              enddo
              plx = plxb              
              do i=ibs_,1,-1
                if(hgz_table_z(i) == zbs) cycle
                  tx = t(m,n)
                  px = plx + patm
                  call watlqd( tx,px,rholx )
                  plx = plx + (hgz_table_z(i+1)-hgz_table_z(i))*rholx*gravz
                  hgz_table_p(i,irefbx) = plx
              enddo
              plx = plxb_
              do i=ibs_,nzdim
                if(hgz_table_z(i) == zbs) cycle
                  tx = t(m,n)
!                  px = max( plx,pgx ) + patm
                  px = plx + patm
                  call watlqd( tx,px,rholx )
                  plx = plx - (hgz_table_z(i)-hgz_table_z(i-1))*rholx*gravz
                  hgz_table_p(i,irefbx) = plx
              enddo
           endif
           i = 0
           call locate(hgz_table_z,nzdim,zpb(nb),i)
           zlx = hgz_table_z(i)
           plowx = hgz_table_p(i,irefbx)
           if(i.eq.nzdim) then
             zux = zlx
             puppx = plowx
           else
             zux = hgz_table_z(i+1)
             puppx = hgz_table_p(i+1,irefbx)
           endif
           if(zlx.eq.zux) then
             plx = plowx
           elseif(zlx.eq.zpb(nb)) then
             plx = plowx
           elseif(zux.eq.zpb(nb)) then
             plx = puppx
           else
             px = max( plowx,pgx ) + patm
             call watlqd( tbx,px,rholx )
             plx = plowx - (zpb(nb)-zlx)*rholx*gravz
           endif
!
!---  Aqueous Seepage Face  ---
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17 ) THEN
!            IF( IBCT(IEQW,NB).EQ.-17 ) THEN
            if(ncntx(irefbx) == 0) then
              ncntx(irefbx) = 1
              zbs = basez(irefbx) 
              plx = bcxpxx
              plxb = plx
              tbx = t(m,n)
!--- Create table for pressure interpolation
              hgz_table_p(:,irefbx) = 0.d0
!              call locate(hgz_table_z,nzdim,zbs,ix)
!              hgz_table_z(ix) = zbs
              do i=1,nzdim
!                if(hgz_table_z(i) < zbs) then
!                  hgz_table_p(i,irefbx) = 0.d0
                if(hgz_table_z(i) == zbs) then
                  hgz_table_p(i,irefbx) = bcxpxx
                  plxb_ = bcxpxx
                  tx = t(m,n)
                  tbx = tx
                  ibs_ = i
                  exit
                endif
              enddo
              plx = plxb_
              do i=ibs_,1,-1
                if(hgz_table_z(i) == zbs) cycle
                  tx = t(m,n)
                  px = plx + patm
                  call watlqd( tx,px,rholx )
                  plx = plx + (hgz_table_z(i+1)-hgz_table_z(i))*rholx*gravz
                  hgz_table_p(i,irefbx) = plx
              enddo
              plx = plxb_
              do i=ibs_,nzdim
                if(hgz_table_z(i) == zbs) cycle
                  tx = t(m,n)
!                  px = max( plx,pgx ) + patm
                  px = plx + patm
                  call watlqd( tx,px,rholx )
                  plx = plx - (hgz_table_z(i)-hgz_table_z(i-1))*rholx*gravz
                  hgz_table_p(i,irefbx) = plx
              enddo
!              plx = plxb
            endif
!            ELSEIF( IBCT(IEQW,NB).EQ.17 ) THEN
           i = 0
           call locate(hgz_table_z,nzdim,zpb(nb),i)
           zlx = hgz_table_z(i)
           plowx = hgz_table_p(i,irefbx)
           if(i.eq.nzdim) then
             zux = zlx
             puppx = plowx
           else
             zux = hgz_table_z(i+1)
             puppx = hgz_table_p(i+1,irefbx)
           endif
           if(zlx.eq.zux) then
!             plx = max(plowx,pgx)
             plx = plowx
           elseif(zlx.eq.zpb(nb)) then
!             plx = max(plowx,pgx)
             plx = plowx
           elseif(zux.eq.zpb(nb)) then
!             plx = max( puppx, pgx)
             plx = puppx
           else
             px = max( plowx,pgx ) + patm
             call watlqd( tbx,px,rholx )
!             plx = max(plowx - (zpb(nb)-zlx)*rholx*gravz, pgx)
             plx = plowx - (zpb(nb)-zlx)*rholx*gravz
           endif
           plxw = plx
           pgxw = pgx
           plx = max(pgxw,plxw)
!
!---  Aqueous Potential Evaporation  ---
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.24 ) THEN
!            HDGL = MAX( (PG(M,N)-PL(M,N))/RHORL/GRAV,1.D-14 )
!            PEFX = 1.D+0
!            IF( HDGL.GT.BCX(3) ) THEN
!              PEFX = (BCX(3)/HDGL)**4
!            ENDIF
!            IF( IBCD(NB).LT.0 ) THEN
!              PLX = PLX - ABS(BCX(2))*PEFX*DB*VISL(M,N)/PERM(IBD,IZN)
!     &          + RHOL(M,N)*GB
!            ELSE
!              PLX = PLX + ABS(BCX(2))*PEFX*DB*VISL(M,N)/PERM(IBD,IZN)
!     &          + RHOL(M,N)*GB
!            ENDIF
          if(lplant == 1) then
!             print *, "lplant =", lplant, "BCX(2) = ", BCX(2)
              call parse_id_local(n,ix_x,iy_x,iz_x)
              lndx = ix_x + (iy_x-1)*ldx
              veg_bc(lndx) = bcx(2)
          else
            IF( IBCD(NB).LT.0 ) THEN
              PLX = PLX - ABS(BCX(2))*DB*VISL(M,N)/ &
              (PERM(IBD,N)*RKL(IBD,M,N)) + RHOL(M,N)*GB
            ELSE
              PLX = PLX + ABS(BCX(2))*DB*VISL(M,N)/ &
              (PERM(IBD,N)*RKL(IBD,M,N)) + RHOL(M,N)*GB
            ENDIF
            HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
            IF( HDGL.GT.BCX(3) ) THEN
              PLX = PGX - BCX(3)*RHORL*GRAV
              IBCT(IEQW,NB) = -24
            ELSE
              IBCT(IEQW,NB) = 24
            ENDIF
          endif 
!
!---      X-Y-Z Aqueous Hydraulic Gradient
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.44 ) THEN
            if(ncntx(irefbx) == 0) then
              ncntx(irefbx) = 1
!              nwx = base_node(irefbx) 
              xbs = basex(irefbx)
              ybs = basey(irefbx)
              zbs = basez(irefbx)
              plx = bcxpxx
              plxb = plx
              tbx = t(m,n)
!--- Create table for pressure interpolation
              hgz_table_p(1:nzdim,irefbx) = 0.d0
!              call locate(hgz_table_z,nzdim,zbs,ix)
               do i=1,nzdim
!                if(hgz_table_z(i) < zbs) then
!                  hgz_table_p(i,irefbx) = 0.d0
                if(hgz_table_z(i) == zbs) then
                  hgz_table_p(i,irefbx) = bcxpxx
                  plxb_ = bcxpxx
                  tx = t(m,n)
                  tbx = tx
                  ibs_ = i
                  exit
                endif
              enddo
              plx = plxb_
              do i=ibs_,1,-1
                if(hgz_table_z(i) == zbs) cycle
                  tx = t(m,n)
                  px = plx + patm
                  call watlqd( tx,px,rholx )
                  plx = plx + (hgz_table_z(i+1)-hgz_table_z(i))*rholx*gravz
                  hgz_table_p(i,irefbx) = plx
              enddo
              plx = plxb_
              do i=ibs_,nzdim
                if(hgz_table_z(i) == zbs) cycle
                  tx = t(m,n)
!                  px = max( plx,pgx ) + patm
                  px = plx + patm
                  call watlqd( tx,px,rholx )
                  plx = plx - (hgz_table_z(i)-hgz_table_z(i-1))*rholx*gravz
                  hgz_table_p(i,irefbx) = plx
              enddo
           endif
           i = 0
           call locate(hgz_table_z,nzdim,zpb(nb),i)
           zlx = hgz_table_z(i)
           plowx = hgz_table_p(i,irefbx)
           if(i.eq.nzdim) then
             zux = zlx
             puppx = plowx
           else
             zux = hgz_table_z(i+1)
             puppx = hgz_table_p(i+1,irefbx)
           endif
           if(zlx.eq.zux) then
             plx = plowx
           elseif(zlx.eq.zpb(nb)) then
             plx = plowx
           elseif(zux.eq.zpb(nb)) then
             plx = puppx
           else
             px = max( plowx,pgx ) + patm
             call watlqd( tbx,px,rholx )
             plx = plowx - (zpb(nb)-zlx)*rholx*gravz
           endif
           dxx = xpb(nb) - xbs
           dyx = ypb(nb) - ybs
           dzx = zpb(nb) - zbs
           plx = plx + dxx*bcx(lbcv-2) + dyx*bcx(lbcv-1)  &
                      + dzx*bcx(lbcv)
!
!---      X-Y-Z Aqueous Seepage Face
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.45 ) THEN
       !     ix = i_bidi(nb)
       !     jx = i_bidj(nb)
       !     kx = i_bidk(nb)
!            ib = i_bid(nb)
!           write(*,'(a,4I6)') 'me,nx,ny,nz',me,nxdim,nydim,nzdim
!           write(*,'(a,4I6)') 'me,lx,ly,lz',me,ldx,ldy,ldz
!           write(*,*) 'me,size i_bid',me,size(i_bid)
!           write(*,'(a,3I6,4F16.8)') 'me,nb,gi,xc,yc,zc',me,nb,ib,d_xc(ib),d_yc(ib),d_zc(ib),zpb(nb)

! Now use cell centroidi, instead of face centroid, to specify static
! pressure. -BH
           if(ncntx(irefbx) == 0) then
              ncntx(irefbx) = 1
              xbs = basex(irefbx)
              ybs = basey(irefbx)
              zbs = basez(irefbx)
              plx = bcxpxx
              plxb = plx
              tbx = t(m,n)
!--- Create table for pressure interpolation
!              write(*,'(a,3I6,6F16.8)') 'me,nb,ib,xref,yref,zref,xbs, &
!                           ybs,zbs',me,nb,ib,d_xc(ib),d_yc(ib),d_zc(ib),xbs,ybs,zbs
              hgz_table_p(:,irefbx) = 0.d0
!              call locate(hgz_table_z,nzdim,zbs,ix)
!              hgz_table_z(ix) = zbs
             if(ics == 3 .OR. ics == 8) then
              plxb_ = bcxpxx
              tx = t(m,n)
              tbx = tx
              h_min_dz = 10000.00
              iz_min = 0
              do i = 1, nzdim
                ib_tmp = ib+(i-1)*ldx*ldy
                hgz_table_z(i) = d_zc(ib_tmp)
                dz_tmp = abs(hgz_table_z(i)-zbs)
                 !   write(*,*) 'me, dz_tmp: ',me,dz_tmp
                    if (dz_tmp <= h_min_dz) then
                       h_min_dz = dz_tmp
                       iz_min = i
                    endif
              enddo
!              write(*,'(a,2(I3X),F16.8)') 'me,z_min,d_basez: ',me,iz_min,h_min_dz
              plx = plxb_
              dz_tmp = hgz_table_z(iz_min)-zbs
              px = plx + patm
              call watlqd( tx,px,rholx )
              plx = plx - rholx*gravz*dz_tmp
              hgz_table_p(iz_min,irefbx) = plx
                
              do i = iz_min,1,-1
                 if (i == iz_min) cycle
                   dz_tmp = hgz_table_z(i+1) - hgz_table_z(i)
                   px = plx + patm
                   call watlqd( tx,px,rholx )
                   plx = plx + rholx*gravz*dz_tmp
                   hgz_table_p(i,irefbx) = plx
              enddo
              plx = hgz_table_p(iz_min,irefbx)
              do i =iz_min,nzdim
                 if (i == iz_min) cycle
                   dz_tmp = hgz_table_z(i) - hgz_table_z(i-1)
                   px = plx + patm
                   call watlqd( tx,px,rholx )
                   plx = plx - rholx*gravz*dz_tmp
                   hgz_table_p(i,irefbx) = plx
              enddo
!     write(format_110, '(a,i1,a)') '(a,I3,X,',nzdim,'(F8.4,2X))'
!     write(*,format_110) 'bfg_hgz_table_z:',me,hgz_table_z(:)
!     write(format_110, '(a,i1,a)') '(a,I3,X,',nzdim,'(F16.8,2X))'
!     write(*,format_110) 'bfg_hgz_table_p:',me,hgz_table_p(:,irefbx)

       !         write(*,'(a,I3,X,10(F8.4,2X))') 'bfg_hgz_table_z: ',me,hgz_table_z(:)
       !         write(*,'(a,I3,X,10(F16.8,2X))') 'bfg_hgz_table_p: ',me,hgz_table_p(:,irefbx) 
             else
              do i=1,nzdim
!                 write(*,'(a,2I4,2F16.8)') 'me,i,hgz_table_z(i),zbs',me,i,hgz_table_z(i),zbs
!                if(hgz_table_z(i) < zbs) then
!                  hgz_table_p(i,irefbx) = 0.d0
                if(hgz_table_z(i) == zbs) then
                  hgz_table_p(i,irefbx) = bcxpxx
                  plxb_ = bcxpxx
                  tx = t(m,n)
                  tbx = tx
                  ibs_ = i
                  exit
                endif
              enddo
              plx = plxb_
              do i=ibs_,1,-1
                if(hgz_table_z(i) == zbs) cycle
                  tx = t(m,n)
                  px = plx + patm
                  call watlqd( tx,px,rholx )
                  plx = plx + (hgz_table_z(i+1)-hgz_table_z(i))*rholx*gravz
                  hgz_table_p(i,irefbx) = plx
              enddo
              plx = plxb_
              do i=ibs_,nzdim
                if(hgz_table_z(i) == zbs) cycle
                  tx = t(m,n)
                  px = plx + patm
                  call watlqd( tx,px,rholx )
                  plx = plx - (hgz_table_z(i)-hgz_table_z(i-1))*rholx*gravz
                  hgz_table_p(i,irefbx) = plx
              enddo
!              write(*,'(a,I3,X,10(F8.4,2X))') 'hgz_table_z: ',me,hgz_table_z(:)
!                write(*,'(a,I3,X,10(F16.8,2X))')'hgz_table_z: ',me,hgz_table_p(:,irefbx)
             endif
            endif
         if (ics .ne. 3 .AND. ics .ne. 8) then
           i = 0
           call locate(hgz_table_z,nzdim,zpb(nb),i)
           zlx = hgz_table_z(i)
           plowx = hgz_table_p(i,irefbx)
           if(i.eq.nzdim) then
             zux = zlx
             puppx = plowx
           else
             zux = hgz_table_z(i+1)
             puppx = hgz_table_p(i+1,irefbx)
           endif
           if(zlx.eq.zux) then
!             plx = max(plowx,pgx)
             plx = plowx
           elseif(zlx.eq.zpb(nb)) then
!             plx = max(plowx,pgx)
             plx = plowx
           elseif(zux.eq.zpb(nb)) then
!             plx = max( puppx, pgx)
             plx = puppx
           else
             px = max( plowx,pgx ) + patm
             call watlqd( tbx,px,rholx )
!             plx = max(plowx - (zpb(nb)-zlx)*rholx*gravz, pgx)
             plx = plowx - (zpb(nb)-zlx)*rholx*gravz
           endif
         else
           h_min_dz = 10000.00
           iz_min = 0
           do i = 1, nzdim
                dz_tmp = abs(hgz_table_z(i)-zpb(nb))
                    if (dz_tmp <= h_min_dz) then
                       h_min_dz = dz_tmp
                       iz_min = i
                    endif
           enddo
!            if (iz_min == 0) write(*,'(a,I6,a,I2)') 'Cannot locate the &
!                                 cell at NB=',NB,', at Node: ',me
!           write(*,*) 'me,z_min,d_z',me,z_min,min_dz 
           zlx = hgz_table_z(iz_min)
           plowx = hgz_table_p(iz_min,irefbx)
           px = max( plowx,pgx ) + patm
           call watlqd( tbx,px,rholx )
           plx = plowx - (zpb(nb)-zlx)*rholx*gravz  
         endif
           dxx = xpb(nb) - xbs
           dyx = ypb(nb) - ybs
           dzx = zpb(nb) - zbs
           plx = plx + dxx*bcx(lbcv-2) + dyx*bcx(lbcv-1)  &
                      + dzx*bcx(lbcv)
!           write(*,'(a,2I6,3F16.8)') 'me,nb,bcx',me,nb,bcx(lbcv-2),bcx(lbcv-1),bcx(lbcv)
           plxw = plx
           pgxw = pgx
           plx = max(pgxw,plxw)
!          write(*,'(a,I3X,3(I6X))') 'me,lx,ly,lz: ',me,ldx,ldy,ldz
!          write(*,'(a,4(I6X),4(F16.8X))')'me,nb,ib,m,x,y,z,plx:',me,nb,ib,m,d_xc(ib),d_yc(ib),d_zc(ib),plx
       ENDIF
!
!---  Secondary and primary boundary variables  ---
!
          SGRMX = 0.D+0
          MX = 2
          INDX = 1
          CALL KSP1( N,N,MX,PGX,PLX,SLB(M,NB),RKLB(1,M,NB), &
          ASLX,ASLMINX,ASGTX,SGRMX,INDX,IPH(2,N),PGB(1,NB), &
          PLB(1,NB),SLB(1,NB) )
          SGB(M,NB) = MAX( 1.D+0-SLB(M,NB),ZERO )
          PX = MAX( PGX,PLX )+PATM
          CALL PORSTY(N,PX,PCMP(N),PORDB(M,NB),PORTB(M,NB))
          IF( ISLC(3).EQ.1 ) CALL TORTU( N,SLB(M,NB),SGB(M,NB),ZERO, &
          PORDB(M,NB),TORLB(M,NB),TORGX,TORNX )
!
!---  Convert pressure to absolute prior to computing physical
!     properties  ---
!
          PLX = PLX + PATM
          PGX = PGX + PATM
!
!---  Compute thermodynamic properties  ---
!
          CALL WATSP( T(2,N),PVWB(M,NB) )
          PX = MAX( PLX,PGX,PVWB(M,NB) )
          CALL WATLQD( T(2,N),PX,RHOLB(M,NB) )
          CALL WATLQV( T(2,N),PX,PVWB(2,NB),VISLB(M,NB) )
!
!---  Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration  ---
!
          XLWB(M,NB) = 1.D+0
          IF( ISLC(16).EQ.1 ) THEN
            XVLB = SLB(2,NB)*PORDB(2,NB)
            XVSB = SLB(2,NB)*RHOS(N)*PCSL(1,NSL_ELC,N)* &
            (1.D+0-PORTB(2,NB))
            CLX = CB(NB,NSL_ELC)/(XVSB+XVLB)
            XLWB(M,NB) = RHOLB(M,NB)
            CALL ELC_DEN( RHOLB(M,NB),CLX,ELC_DCF )
            XLWB(M,NB) = XLWB(M,NB)/RHOLB(M,NB)
            CALL ELC_VIS( VISLB(M,NB),CLX,ELC_VCF )
          ENDIF
          PLB(M,NB) = PLX - PATM
          PGB(M,NB) = PGX - PATM
          TB(M,NB) = TX
  200     CONTINUE
  300   CONTINUE
  400 CONTINUE
      if(lplant == 1) then
        if(.not. allocated(beta)) allocate(beta(isvc+2,ldx*ldy))
        beta = 0.d0
!        call ga_dgop(1,veg_bc,ldx*ldy,'+')
        do m = 2,isvc+2
          do lndx = 1,ldx*ldy
            if(veg_varx(2,lndx) > 0.d0) then
              zsurf = veg_varx(1,lndx)
              ix = int(veg_varx(2,lndx))
              iy = int(veg_varx(3,lndx))
              pft = int(veg_varx(4,lndx))
              beta(m,lndx) = 0.d0
              do izz = 1, ldz
                ndx = ix + (iy-1)*ldx + (izz-1) * ldx * ldy
                if(id_g2l(ndx) > 0 .and. ixp(ndx)>0 ) then
                  if ((rsd_p(1,pft) == 0.d0) .or. &
                     ((rsd_p(1,pft) > 0.d0).and.&
                      ((zsurf-(zp(ndx)-dzgf(ndx)/2.d0))<=rsd_p(1,pft)))) then 
                      z_up = zsurf-(zp(ndx) + dzgf(ndx)/2.d0)
                      z_bt = zsurf-(zp(ndx) - dzgf(ndx)/2.d0)
                    if (IPLF_P(pft)>=3) then
                      smp = (pg(m,ndx)-pl(m,ndx))/rhol(m,ndx)/grav
                      call soil_stress (pft, smp, z_up, z_bt, wiltf(m,ndx))
                      beta(m,lndx) = beta(m,lndx)+wiltf(m,ndx)*root_fr(ndx)
                    elseif (IPLF_P(pft) == 1 .or. IPLF_P(pft) == 2) then
                      call roots(root_fr(ndx),z_up,z_bt,pft,izz,ndx)
                      beta(m,lndx) = beta(m,lndx)+root_fr(ndx)
                    endif
                  endif 
                endif
              enddo
            endif
          enddo
        enddo
!        call ga_dgop(1,beta,lsv*ldx*ldy,'+')    
!        print*,'me,loc,ldxyz:',me,num_loc_nodes,ldx*ldy*ldz,id_l2g(1),id_l2g(num_loc_nodes)
        do m = 2,isvc+2
          do lndx = 1,ldx*ldy
            if(veg_varx(2,lndx) > 0.d0) then
              bcx(2) = veg_bc(lndx)
              zsurf = veg_varx(1,lndx)
              ix = int(veg_varx(2,lndx))
              iy = int(veg_varx(3,lndx))
              pft = int(veg_varx(4,lndx))
              do izz = 1, ldz
                ndx = ix + (iy-1)*ldx + (izz-1) * ldx * ldy
                if(id_g2l(ndx) > 0 .and. ixp(ndx)>0) then
                  if ((rsd_p(1,pft) == 0.d0) .or. &
                     ((rsd_p(1,pft) > 0.d0).and.&
                       ((zsurf-(zp(ndx)-dzgf(ndx)/2.d0))<=rsd_p(1,pft)))) then
                    if (IPLF_P(pft)>=3) then
                      veg_sink(m,ndx) =-bcx(2) * wiltf(m,ndx)*root_fr(ndx) &
                        *dxgf(ndx) * dygf(ndx)
                    elseif (IPLF_P(pft) == 1 .or. IPLF_P(pft) == 2) then
                      if (beta(m,lndx) > 1) then
                        root_fr(ndx) = root_fr(ndx)/beta(m,lndx)
                      endif
                      veg_sink(m,ndx) =-bcx(2) * root_fr(ndx) &
                                   * dxgf(ndx) * dygf(ndx)
                    endif
!                    veg_sink(m,ndx) = veg_sink(m,ndx)*rhol(m,ndx)
                    call crop_coeff(pft,TM,c_coef)
                    evap_trans(m,ndx) =c_coef*bcx(2) * beta(m,lndx)
                    veg_sink(m,ndx) = veg_sink(m,ndx)*rhol(m,ndx)*c_coef
                  endif
                endif
              enddo
            endif
          enddo
        enddo
!        call ga_dgop(1,et,lsv*ldx*ldy,'+')
      endif
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of BCP1 group
!
      RETURN
      ENDSUBROUTINE BCP1
