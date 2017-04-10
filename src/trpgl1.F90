
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TRPGL1
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
!     Compute the total trapping number for gas entrapment in the
!     aqueous phase.
!
!     Pennell, K.D., G.A. Pope, L.M. Abriola.  1996.
!     "Influence of Viscous and Buoyancy Forces on the Mobilization
!     of Residual Tetrachloroethylene during Surfactant Flushing."
!     Environ. Sci. Technol.  30(4):1328-1335.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1997.
!     Last Modified by MD White on December 18, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FLUXP
      USE FDVP
      USE FDVD
      USE CONST
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/TRPGL1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      DO 100 N = 1,NFLD
        IF( IXP(N).LE.0 ) GOTO 100
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
        NQX = NSX(N)+1
        NQY = NSY(N)+IFLD
        NQZ = NSZ(N)+IJFLD
        IZN = N
        DFMLX = SQRT( ABS(UL(1,NPX))*ABS(UL(1,NQX)) + &
        ABS(VL(1,NPY))*ABS(VL(1,NQY)) + &
        ABS(WL(1,NPZ))*ABS(WL(1,NQZ)))
        ULX = 5.D-1*(UL(1,NPX)+UL(1,NQX))
        VLX = 5.D-1*(VL(1,NPY)+VL(1,NQY))
        WLX = 5.D-1*(WL(1,NPZ)+WL(1,NQZ))
        ULGX = 5.D-1*(UL(1,NPX)*GRVX(NPX)+UL(1,NQX)*GRVX(NQX))
        VLGX = 5.D-1*(VL(1,NPY)*GRVY(NPY)+VL(1,NQY)*GRVY(NQY))
        WLGX = 5.D-1*(WL(1,NPZ)*GRVZ(NPZ)+WL(1,NQZ)*GRVZ(NQZ))
        DFALX = ((ULGX + VLGX + WLGX)/GRAV)/  &
       ( SQRT( ULX**2 + VLX**2 + WLX**2 ) + SMALL )
        SKL = SQRT((PERM(1,IZN)*(5.D-1*(GRVX(NPX)+GRVX(NQX))))**2 +  &
        (PERM(2,IZN)*5.D-1*(GRVY(NPY)+GRVY(NQY)))**2 + &
        (PERM(3,IZN)*5.D-1*(GRVZ(NPZ)+GRVZ(NQZ)))**2)/GRAV
        RKLMX = (RKL(1,2,N)*RKL(2,2,N)*RKL(3,2,N))**(1./3.)
        BNDX = (RHOL(2,N)-RHORG)*GRAV*SKL*RKLMX
        CAPX = DFMLX*VISL(2,N)
        TRPGL(2,N) = SQRT(CAPX**2 + 2.D+0*CAPX*BNDX*DFALX + BNDX**2)
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of TRPGL1 group  ---
!
      RETURN
      END
