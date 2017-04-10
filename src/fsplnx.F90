!----------------------Function----------------------------------------!
!
      FUNCTION FSPLNX( FY,ITS,ITE )
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
!     Given arrays TBLY and TBLX containing a tabulated function
!     (i.e., x(n) = f(y(n))), with y(1) < y(2) < ... < y(n), and given
!     the array TBLDDX, which is output from subroutine SPLINE, and
!     given a value of FY this function returns a cubic spline
!     interpolated value.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, September, 1994.
!     Last Modified by MD White, Battelle, PNL, September 9, 1994.




!     $Id: fsplnx.F,v 1.7 2005/02/03 21:24:33 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TABL
      USE SOLTN
      USE FILES
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
      SUBNMX = '/FSPLNX'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(64)(1:1),'$').EQ.0 ) CVS_ID(64) = &
     '$Id: fsplnx.F,v 1.7 2005/02/03 21:24:33 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Ascending table order  ---
!
      IF( TBLY(ITE).GT.TBLY(ITS) ) THEN
!
!---  Find the right place in the table by means of bisection  ---
!
        KLO = ITS
        KHI = ITE
   10   CONTINUE
        IF( KHI-KLO.GT.1 ) THEN
          K = (KHI+KLO)/2
          IF( TBLY(K).GT.FY ) THEN
            KHI = K
          ELSE
            KLO = K
          ENDIF
          GOTO 10
        ENDIF
        H = TBLY(KHI)-TBLY(KLO)
        F = MIN( ABS(H/TBLY(KHI)),ABS(H/TBLY(KLO)) )
        IF( F.LT.EPSL ) THEN
          WRITE(IWR,'(2A)') 'ERROR: Invalid Table',SUBNM(1:ICSN)
          WRITE(ISC,'(2A)') 'ERROR: Invalid Table',SUBNM(1:ICSN)



          STOP

        ENDIF
!
!---  Evaluate cubic spline  ---
!
        A = (TBLY(KHI)-FY)/H
        B = (FY-TBLY(KLO))/H
        FSPLNX = A*TBLX(KLO)+B*TBLX(KHI)+ &
        ((A**3-A)*TBLDDX(KLO)+(B**3-B)*TBLDDX(KHI))*(H**2)/6.D+0
!
!---  Descending table order  ---
!
      ELSEIF( TBLY(ITE).LT.TBLY(ITS) ) THEN
!
!---  Find the right place in the table by means of bisection  ---
!
        KLO = ITS
        KHI = ITE
   20   CONTINUE
        IF( KHI-KLO.GT.1 ) THEN
          K = (KHI+KLO)/2
          IF( TBLY(K).LT.FY ) THEN
            KHI = K
          ELSE
            KLO = K
          ENDIF
          GOTO 20
        ENDIF
        H = TBLY(KLO)-TBLY(KHI)
        F = MIN( ABS(H/TBLY(KLO)),ABS(H/TBLY(KHI)) )
        IF( F.LT.EPSL ) THEN
          WRITE(IWR,'(2A)') 'ERROR: Invalid Table',SUBNM(1:ICSN)
          WRITE(ISC,'(2A)') 'ERROR: Invalid Table',SUBNM(1:ICSN)



          STOP

        ENDIF
!
!---  Evaluate cubic spline  ---
!
        A = (TBLY(KLO)-FY)/H
        B = (FY-TBLY(KHI))/H
        FSPLNX = A*TBLX(KHI)+B*TBLX(KLO)+ &
        ((A**3-A)*TBLDDX(KHI)+(B**3-B)*TBLDDX(KLO))*(H**2)/6.D+0
      ELSE
        WRITE(IWR,'(2A)') 'ERROR: Invalid Table',SUBNM(1:ICSN)
        WRITE(ISC,'(2A)') 'ERROR: Invalid Table',SUBNM(1:ICSN)
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of FSPLNX group
!
      RETURN
      END

