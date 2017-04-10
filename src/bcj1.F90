

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BCJ1
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
!     Modify the Jacobian matrix for boundary conditions
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE GRID
      USE CONST
      USE BCV
      use grid_mod
      use bcvp
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
      SUBNMX = '/BCJ1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Loop over boundary conditions  ---
!
      DO 100 nb = 1,num_bcnx
        mb = ibcin(nb)
        TMZ = TM
        IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
        IF( IBCC(NB).EQ.1 ) TMZ = MOD( TM,BC(1,IBCM(NB),MB) )
        IF( TMZ.LE.BC(1,1,MB) ) GOTO 100
        IF( IBCM(NB).GT.1 .AND. TMZ.GT.BC(1,IBCM(NB),MB) ) GOTO 100
        N = IBCN(NB)
        IF( IXP(N).LE.0 ) GOTO 100
!
!---  Aqueous  ---
!
        IF( IBCT(IEQW,NB).NE.3 ) THEN
          CALL JCBWLB( N,NB )
        ENDIF
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of BCJ1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBWLB( N,NB )
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
!     Modify the Jacobian matrix for boundary conditions.
!     (aqueous boundary, water equation, bottom surface)
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on August 1, 1996.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVP
      USE BCVP
      use bcv
      use grid_mod
      use petscapp
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!
!
!
!--- Petsc includes
!
#include "petscwrapper.h"

#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Parameter Statements----------------------------!
!

!      PetscInt :: ic(2),ir(2),nr,nc 
!      PetscScalar :: values_(4)
!      PetscErrorCode :: ierr

      integer :: ic(2),ir(2),nr,nc 
      real*8 :: values_(4)
      integer :: ierr


!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RS(LUK+1)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/JCBWLB'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(113)(1:1),'$').EQ.0 ) CVS_ID(113) = &
      '$Id: jcbwlb.F,v 1.7 2006/03/30 21:12:21 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      DO 100 M = 1,ISVC+1
!        MP = MNEGB(M)
        MP = mposb(M)
        MB = M + 1
        FLWB = XLWB(MB,NB)*RHOLB(MB,NB)
        FLWP = XLW(MB,N)*RHOL(MB,N)
        IF( IBCD(NB) < 0 ) THEN
         FLWDN = FLWB
         FLWUP = FLWP
        ELSE
         FLWDN = FLWP
         FLWUP = FLWB
        ENDIF
        INDX = 2
        FLW = DIFMN( FLWDN,FLWUP,distb(nb),distb(nb),q_flux_b(1,nb),INDX )
        signx = sign(1,ibcd(nb))
        RS(M) = signx*areab(nb)*q_flux_b(mp,nb)*FLW
  100 CONTINUE
!
!---  Load Jacobian Matrix  ---
!
      nr = isvc
      nc = isvc
      do m=1,isvc
       icol = (loc_map(n)-1)*luk+m-1
!       icol = ((n)-1)*luk+m-1
       ic(m) = icol
       ir(m) = icol
       values_(m) = (rs(m+1)-rs(1))/dnr(m,n)
      enddo
!print *,'ir--ic',ir,ic,'nb',values_(1),ibct(1,nb)
      call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
!
!--- Set RHS
!
      residual(ieqw,n) = residual(ieqw,n) + rs(1)
!print *,'residual-----------bc',me,n,residual(ieqw,n),nb,ibct(1,nb),rs(1)
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of JCBWLB group  ---
!
      RETURN
      END
