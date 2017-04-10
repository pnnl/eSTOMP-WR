!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PERM_I( PERMRFX,PORDX,IZN )
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
!     Kozeny-Carmen  Porosity-Permeability Relationship
!
!     Original:
!     Carman PC. 1937. Fluid flow through granular beds. Transactions of
!     the Institution of Chemical Engineers, 15:150-156.
!     Reprinted:
!     Carman PC. 1997. Fluid flow through granular beds. Chemical 
!     Engineering Research & Design, 75:S32-S48.
!
!
!----------------------Authors-----------------------------------------!
!
!     Written by DH Bacon, PNNL, 11 Nov 2010.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      use pormed
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/PERM_I'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(331)(1:1),'$').EQ.0 ) CVS_ID(331) = &
      '$Id: perm_i.F,v 1.24 2008/02/13 01:02:39 d3c002 Exp $'
      ICSN = ICSN+ICSNX
!
!---  Permeability reduction factor  ---
!
!      PERMRFX = PORDX*PORDX*PORDX/((1.D+0 - PORDX)*(1.D+0 - PORDX))
!      if(islc(43) == 2) then
        PERMRFX = PORDX*PORDX*PORDX/((1.D+0 - PORDX)*(1.D+0 - PORDX))
        pordxx = pordx
        pordx = por(1,izn)
        PERMRFX0 = PORDX*PORDX*PORDX/((1.D+0 - PORDX)*(1.D+0 - PORDX))
        PERMRFX = PERMRFX/PERMRFX0
        pordx = pordxx
!      endif
!
!---  Reset subroutine string sequence  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of PERM_I group  ---
!
      RETURN
      END
