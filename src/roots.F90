!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ROOTS( UPTAKE,ZSX,ZEX,IP,NZ,N)
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
!     Stressed root-water uptake.
!
!----------------------Authors-----------------------------------------!
!
!     Written by EJ Freeman and MD White, PNNL, 20 June 2002.
!     Last Modified by MD White, PNNL, 20 June 2002.
!     Last Modified by MD White, PNNL, 19 August 2002.
!     Last Modified by MD White, PNNL, 20 August 2002.
!     Last Modified by MD White, PNNL, 1 December 2003.

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PLT_ATM
      USE GRID
      USE FDVP
      USE CONST
      USE PORMED
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      REAL *8 UPTAKE
      INTEGER:: IC
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!
      EXTERNAL VRUGT
!
!----------------------Executable Lines--------------------------------!
!
!      ISUB_LOG = ISUB_LOG+1
!      SUB_LOG(ISUB_LOG) = '/ROOTS'
!      IF( INDEX(CVS_ID(171)(1:1),'$').EQ.0 ) CVS_ID(171) = &
!      '$Id: roots.F,v 1.1.1.1 2011/01/17 20:36:35 d3c002 Exp $'
!
!---  Set plant variables  ---
      me = ga_nodeid()
!
      ZM = RSD_P(1,IP)
!      ZSX = PARMS_P(4,IP)
      PZ = RSD_P(3,IP)
!
!---  Vrugt root-water uptake model  ---
!
      CALL QROMB( VRUGT,ZSX,ZEX,RWUX,IERR,IP)
      IF( IERR.EQ.1 ) THEN
        INDX = 12
        IMSG = N  
        CHMSG = 'Unconverged Romberg Integration: ' // &
         'Source Root-Water Uptake Integration: Node: '
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Vrugt water stress function  ---
!
      IF( IPLF_P(IP).EQ.1 ) THEN
        HDGL = abs(PG(1,N)-PL(1,N))/(RHORL*GRAV)
        H1 = PLF_P(1,IP)
        H2 = PLF_P(2,IP)
        H3 = PLF_P(3,IP)
        H4 = PLF_P(4,IP)
        IF( HDGL.GE.H4 .OR. HDGL.LE.H1 )THEN
          WSFX = 0.D+0
        ELSEIF( HDGL.LE.H3 .AND. HDGL.GE.H2 )THEN
          WSFX = 1.D+0
        ELSEIF( HDGL.GT.H1 .AND. HDGL.LT.H2 )THEN
          WSFX = (1.D+0/(H2-H1))*(HDGL-H1)
        ELSE
          WSFX = (-1.D+0/(H4-H3))*(HDGL-H3) + 1.D+0
        ENDIF
!
!---  Jarvis water stress function  ---
!
      ELSEIF( IPLF_P(IP).EQ.2 ) THEN
        WC1 = PLF_P(1,IP)
        WC2 = PLF_P(2,IP)
        WC3 = PLF_P(3,IP)
        WC4 = PLF_P(4,IP)
        WCX = SL(1,N)*POR(1,N)
        WSFX = (WCX-WC1)/(WC4-WC1+SMALL)
        WSFX = max(WSFX,0.0)
        WSF1X = (WC2-WC1)/(WC4-WC1+SMALL)
        WSF2X = (WC3-WC1)/(WC4-WC1+SMALL)
        IF( WSFX.GT.WSF2X .AND. (WSFX-EPSL).LE.1.D+0 ) THEN
          WSFX = (1.D+0-WSFX)/(1.D+0-WSF2X+SMALL)
        ELSEIF( WSFX.GE.WSF1X .AND. WSFX.LE.WSF2X ) THEN
          WSFX = 1.D+0
        ELSEIF( (WSFX+EPSL).GE.0.D+0 .AND. WSFX.LT.WSF1X ) THEN
          WSFX = WSFX/(WSF1X+SMALL)
        ELSE
          INDX = 12
          IMSG = N 
          CHMSG = 'Out of Range Stress Index: Node: '
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      UPTAKE = MAX( WSFX*RWUX,0.D+0 )
     1000 CONTINUE
!
!---  Reset subroutine string sequence  ---
!
!      ISUB_LOG = ISUB_LOG-1
!
!---  End of ROOTS group  ---
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION VRUGT( ZX,IP )
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
!     Vrugt one-dimensional root water uptake model function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 19 August 2002.
!     Last Modified by Mark White, PNNL, 19 August 2002.
!     $Id: roots.F,v 1.1.1.1 2011/01/17 20:36:35 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PLT_ATM
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
!      ISUB_LOG = ISUB_LOG+1
!      SUB_LOG(ISUB_LOG) = '/VRUGT'
!      IF( INDEX(CVS_ID(171)(1:1),'$').EQ.0 ) CVS_ID(171) = &
!       '$Id: roots.F,v 1.1.1.1 2011/01/17 20:36:35 d3c002 Exp $'
!
!---  Set maximum rooting depth, null root depth, and root depth
!     fitting parameter  ---
!
      ZM = RSD_P(1,IP)
      ZSX = RSD_P(2,IP)
      PZ = RSD_P(3,IP)
      VRUGT = (1.D+0 - ZX/(ZM+1.D-20))*EXP(-(PZ/ZM)*ABS(ZSX-ZX))
!
!---  End of VRUGT group
!
!      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------subroutine--------------------------------------!
      subroutine zeng(pft,z_top,z_bot,root_frx)
      use plt_atm
!
      implicit none
!
      integer :: pft
      double precision :: z_top
      double precision :: z_bot
      double precision :: root_frx
      root_frx =  0.5d0*( exp(-rsd_p(2,pft) * z_top)  &
                  + exp(-rsd_p(3,pft) * z_top)  &
                  - exp(-rsd_p(2,pft) * z_bot)  &
                  - exp(-rsd_p(3,pft) * z_bot) )
      end subroutine zeng

!----------------------subroutine--------------------------------------!
      subroutine crop_coeff (ip,realtime,c_coef)
      use plt_atm
      implicit none
      integer :: ip,ncoef,i
      double precision ::realtime,time,c_coef,d1,d2,c1,c2,year
      year = 365.25*24*60*60
      time = MOD(realtime,year)
      if (ncrop_p(ip) <=0) then
        c_coef = 1.0
      else
        ncoef = ncrop_p(ip)
        if (time<crop_p(1,1,ip) .or. time>crop_p(1,ncoef,ip)) then
          d2 = crop_p(1,1,ip)+year
          d1 = crop_p(1,ncoef,ip)
          c2 = crop_p(2,1,ip)
          c1 = crop_p(2,ncoef,ip)
          if (time<crop_p(1,1,ip)) then
            time = time+year
          endif
          c_coef =  ((c2-c1)/(d2-d1))*(time-d1)+c1
        else
          do i = 1,ncoef-1
            d1 = crop_p(1,i,ip)
            d2 = crop_p(1,i+1,ip)
            c1 = crop_p(2,i,ip)
            c2 = crop_p(2,i+1,ip)
            if ((time>=d1) .and. (time<=d2)) then
              c_coef =  ((c2-c1)/(d2-d1))*(time-d1)+c1
            endif
          enddo
        endif
      endif
      endsubroutine crop_coeff

!----------------------subroutine--------------------------------------!
      subroutine soil_stress (ip, smp, z_top, z_bot, wilt)
      use plt_atm
      implicit none
      integer :: ip
      double precision :: smp,z_top,z_bot,rootfr,wilt
!subroutine to calculate soil stress index in transpiration
      smp = min(plf_p(1,ip), abs(smp))
      wilt = min((smp-plf_p(1,ip))/(plf_p(2,ip) - plf_p(1,ip)),1.d0)
      end subroutine soil_stress



!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TRAPZD( FUNC,A,B,S,N,INDX )
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
!     This routine computes the N'th stage of refinement of an
!     extended trapezoid rule.
!
!     Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling.
!     1986.  Numerical Recipes, The Art of Scientific Computing.
!     Cambridge University Press, Cambridge.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: roots.F,v 1.1.1.1 2011/01/17 20:36:35 d3c002 Exp $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      EXTERNAL FUNC
      SAVE IT
!
!----------------------Executable Lines--------------------------------!
!
      IF( N.EQ.1 ) THEN
        S = 5.D-1*(B-A)*(FUNC(A,INDX)+FUNC(B,INDX))
!
!---  IT is the number of points to be added on the next call  ---
!
        IT = 1
      ELSE
        REALX = REAL(IT)
        TNM = REALX
!
!---  Spacing of the points to be added.  ---
!
        DEL = (B-A)/TNM
        X = A + 5.D-1*DEL
        SUM = 0.D+0
        DO 100 J = 1,IT
          SUM = SUM + FUNC(X,INDX)
          X = X + DEL
        100   CONTINUE
!
!---  Replace S by its refined value  ---
!
        S = 5.D-1*(S+(B-A)*SUM/TNM)
        IT = 2*IT
      ENDIF
!
!---  End of TRAPZD group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE QROMB( FUNC,A,B,SSX,IERR,INDX )
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
!     Returns as S the integral of the function FUNC from A to B.
!     Integration is performed by Romberg's method of order 2K, where
!     e.g., K=2 is Simpson's rule.
!
!     Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling.
!     1986.  Numerical Recipes, The Art of Scientific Computing.
!     Cambridge University Press, Cambridge.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, November 19, 1999.
!     Last Modified by Mark White, Battelle, November 19, 1999.
!     $Id: roots.F,v 1.1.1.1 2011/01/17 20:36:35 d3c002 Exp $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( EPS=1.D-6, EPSL=1.D-14 )
      PARAMETER( JMAX=20, JMAXP=JMAX+1, K=5, KM=K-1 )
!
!----------------------Type Declarations-------------------------------!
!
      EXTERNAL FUNC
      REAL*8 S(JMAXP),H(JMAXP)
!
!----------------------Executable Lines--------------------------------!
!
      IERR = 0
      ZERO = 0.D+0
!
!---  S and H store the successive trapezodial approximations and their
!     relative step-sizes.  ---
!
      H(1) = 1.D+0
      DO 10 J = 1,JMAX
        CALL TRAPZD( FUNC,A,B,S(J),J,INDX )
        IF( J.GE.K ) THEN
          CALL POLINT( H(J-KM),S(J-KM),K,ZERO,SSX,DSS,IERR )
          IF( (ABS(DSS)-EPS*ABS(SSX))/EPSL.LT.EPSL ) RETURN
        ENDIF
        S(J+1) = S(J)
        H(J+1) = 2.5D-1*H(J)
      10 CONTINUE
      IERR = 1
!
!---  End of QROMB group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE POLINT( XA,YA,N,X,Y,DY,IERR )
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
!     Given arrays XA and YA, each of length N, and given a value X,
!     this routine returns a value Y, and an error estimate DY.
!     If P(X) is the polynomial of degree N-1 such that
!     P(XA(I)) = YA(I),
!     I = 1,N, then the returned value Y = P(X).
!
!     Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling.
!     1986.  Numerical Recipes, The Art of Scientific Computing.
!     Cambridge University Press, Cambridge.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: roots.F,v 1.1.1.1 2011/01/17 20:36:35 d3c002 Exp $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( NMAX=10, EPSL=1.D-14 )
      REAL*8 XA(N),YA(N),C(NMAX),D(NMAX)
!
!----------------------Executable Lines--------------------------------!
!
      IERR = 0
      NS = 1
      DIF = ABS(X-XA(1))
!
!---  Find the index NS of the closest table entry  ---
!
      DO 11 I = 1,N
        DIFT = ABS(X-XA(I))
        IF( DIFT.LT.DIF ) THEN
          NS = I
          DIF = DIFT
        ENDIF
!
!---  Initialize the tableau fo C's and D's  ---
!
       C(I) = YA(I)
       D(I) = YA(I)
     11 CONTINUE
!
!---  Initial approximation to Y  ---
!
      Y = YA(NS)
      NS = NS-1
!
!---  Loop over the columns in the tableau and
!     update the C's and D's  ---
!
      DO 13 M = 1,N-1
        DO 12 I = 1,N-M
          HO = XA(I)-X
          HP = XA(I+M)-X
          W = C(I+1)-D(I)
          DEN = HO-HP
!
!---  Exit subroutine if two input XA's are
!     identical, within roundoff  ---
!
          IF( ABS( DEN )/EPSL.LT.EPSL ) THEN
            IERR = 1
            RETURN
          ENDIF
          DEN = W/DEN
!
!---  Update C's and D's  ---
!
          D(I) = HP*DEN
          C(I) = HO*DEN
        12   CONTINUE
!
!---  After each column in the tableau is completed, decide
!     which direction C or D to add the accumulating value
!     of Y, (i.e., which path to take through the tableau -
!     forking up or down).  Do this in such a way as
!     to take the most "straight line" route through the
!     tableau to its apex, updating NS accordingly to keep track.
!     This route keeps the partial approximations centered
!     (insofar as possible) on the target X.  The
!     last DY added is thus the error indication.  ---
!
        IF( 2*NS.LT.N-M ) THEN
          DY = C(NS+1)
        ELSE
          DY = D(NS)
          NS = NS-1
        ENDIF
        Y = Y+DY
        13 CONTINUE
!
!---  End of POLINT group
!
      RETURN
      END
