!----------------------Function----------------------------------------!
!
      FUNCTION FNHGBL( NBS,NBE,M )
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
!     Aqueous hydraulic gradient boundary condition function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, June 1994.
!     Last Modified by MD White, Battelle, PNL, September 10, 1997.
!     Last Modified by MD White, Battelle, PNNL, July 12, 2000.
!     $Id: fnhgbl.F,v 1.9 2006/03/30 21:12:19 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE GRID
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



!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/FNHGBL'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(57)(1:1),'$').EQ.0 ) CVS_ID(57) = &
      '$Id: fnhgbl.F,v 1.9 2006/03/30 21:12:19 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Set starting-point pressures and temperatures  ---
!
!      PLX = PLB(M,NBS)
!      PGX = PGB(M,NBS)
      plx = basepx
!      PNX = PLB(M,NBS)
      pnx = 0.d0
!      IF( IEQO.GT.0 ) PNX = PNB(M,NBS)
      TX = TB(M,NBS)
      PX = MAX( PLX,PGX,PNX ) + PATM
      CALL WATLQD( TX,PX,RHOLX )
!      NS = IBCN(NBS)
!      NE = IBCN(NBE)
!      IS = ID(NS)
!      IE = ID(NE)
!      JS = JD(NS)
!      JE = JD(NE)
!      KS = KD(NS)
!      KE = KD(NE)
!      II = SIGN(1,IE-IS)
!      JI = SIGN(1,JE-JS)
!      KI = SIGN(1,KE-KS)
!
!---  Convert boundary pressure to node-centroid pressure  ---
!
      plx = plx + distb(nbe)*rholx*(gravx*uvxb+gravy*uvyb+gravz*uvzb)
!      IF( IBCD(NBS).EQ.-3 ) THEN
!        NPZ = NSZ(NS)
!        PLX = PLX - 5.D-1*DZGF(NS)*RHOLX*GRVZ(NPZ)
!      ELSEIF( IBCD(NBS).EQ.-2 ) THEN
!        NPY = NSY(NS)
!        PLX = PLX - 5.D-1*DYGF(NS)*RHOLX*GRVY(NPY)*RP(ID(NS))
!      ELSEIF( IBCD(NBS).EQ.-1 ) THEN
!        NPX = NSX(NS)
!        PLX = PLX - 5.D-1*DXGF(NS)*RHOLX*GRVX(NPX)
!      ELSEIF( IBCD(NBS).EQ.1 ) THEN
!        NQX = NSX(NS)+1
!        PLX = PLX + 5.D-1*DXGF(NS)*RHOLX*GRVX(NQX)
!      ELSEIF( IBCD(NBS).EQ.2 ) THEN
!        NQY = NSY(NS)+IFLD
!        PLX = PLX + 5.D-1*DYGF(NS)*RHOLX*GRVY(NQY)*RP(ID(NS))
!      ELSEIF( IBCD(NBS).EQ.3 ) THEN
!        NQZ = NSZ(NS)+IJFLD
!        PLX = PLX + 5.D-1*DZGF(NS)*RHOLX*GRVZ(NQZ)
!      ENDIF
!
!---  Loop over nodes in the x direction
!
      I = IS
      J = JS
      K = KS
      NX = ND(I,J,K)
      DO 100 I = IS+II,IE,II
        N = ND(I,J,K)
        NPX = NSX(N)
        GB = (XP(N)-XP(NX))*GRVX(NPX)
        PX = MAX( PLX,PGX,PNX ) + PATM
        CALL WATLQD( TX,PX,RHOLX )
        PLX = PLX - RHOLX*GB
        NX = N
  100 CONTINUE
!
!---  Loop over nodes in the y direction
!
      I = IE
      J = JS
      K = KS
      NX = ND(I,J,K)
      DO 110 J = JS+JI,JE,JI
        N = ND(I,J,K)
        NPY = NSY(N)
        GB = (YP(N)*RP(I)-YP(NX)*RP(I))*GRVY(NPY)
        PX = MAX( PLX,PGX,PNX ) + PATM
        CALL WATLQD( TX,PX,RHOLX )
        PLX = PLX - RHOLX*GB
        NX = N
  110 CONTINUE
!
!---  Loop over nodes in the z direction
!
      I = IE
      J = JE
      K = KS
      NX = ND(I,J,K)
      DO 120 K = KS+KI,KE,KI
        N = ND(I,J,K)
        NPZ = NSZ(N)
        GB = (ZP(N)-ZP(NX))*GRVZ(NPZ)
        PX = MAX( PLX,PGX,PNX ) + PATM
        CALL WATLQD( TX,PX,RHOLX )
        PLX = PLX - RHOLX*GB
        NX = N
  120 CONTINUE
!
!---  Convert node-centroid pressure to boundary pressure  ---
!
      NE = IBCN(NBE)
      IF( IBCD(NBE).EQ.-3 ) THEN
        NPZ = NSZ(NE)
        PLX = PLX + 5.D-1*DZGF(NE)*RHOLX*GRVZ(NPZ)
      ELSEIF( IBCD(NBE).EQ.-2 ) THEN
        NPY = NSY(NE)
        PLX = PLX + 5.D-1*DYGF(NE)*RHOLX*GRVY(NPY)*RP(ID(NE))
      ELSEIF( IBCD(NBE).EQ.-1 ) THEN
        NPX = NSX(NE)
        PLX = PLX + 5.D-1*DXGF(NE)*RHOLX*GRVX(NPX)
      ELSEIF( IBCD(NBE).EQ.1 ) THEN
        NQX = NSX(NE)+1
        PLX = PLX - 5.D-1*DXGF(NE)*RHOLX*GRVX(NQX)
      ELSEIF( IBCD(NBE).EQ.2 ) THEN
        NQY = NSY(NE)+IFLD
        PLX = PLX - 5.D-1*DYGF(NE)*RHOLX*GRVY(NQY)*RP(ID(NE))
      ELSEIF( IBCD(NBE).EQ.3 ) THEN
        NQZ = NSZ(NE)+IJFLD
        PLX = PLX - 5.D-1*DZGF(NE)*RHOLX*GRVZ(NQZ)
      ENDIF
      FNHGBL = PLX
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of FNHGBL group
!
      RETURN
      END
