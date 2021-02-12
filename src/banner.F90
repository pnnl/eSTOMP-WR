!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BANNER
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
!     Display a banner on the screen and output file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     Last Modified by MD White, PNNL, 30 May 2002.




!     $Id: banner.F,v 1.10 2008/04/11 14:52:34 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE FILES
      USE FDVP
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



!
!----------------------Type Declarations-------------------------------!
!
      LOGICAL :: use_ga

!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/BANNER'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(14)(1:1),'$').EQ.0 ) CVS_ID(14) = &
     '$Id: banner.F,v 1.10 2008/04/11 14:52:34 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Output File  ---
!
      if(me.eq.0) WRITE(IWR,'(A,//)')' Welcome to ...'
      if(me.eq.0) WRITE(IWR,'(A)')   '                           STOMP'
      if(me.eq.0) WRITE(IWR,'(A,//)')'        Subsurface Transport Over Multiple ' &
      // 'Phases'
      if(me.eq.0) WRITE(IWR,'(A)')   ' This file was produced by STOMP, a ' &
      // 'numerical simulator'
      if(me.eq.0) WRITE(IWR,'(A)')   ' developed by the Pacific Northwest ' &
      // 'Laboratory, with'
      if(me.eq.0) WRITE(IWR,'(A)')   ' support from the VOC-Arid Integrated ' &
      // 'Demonstration Project,'
      if(me.eq.0) WRITE(IWR,'(A)')   ' Office of Technology Development, U.S. ' &
      // 'Department of Energy.'
      if(me.eq.0) WRITE(IWR,'(A)')   ' Results from this version of STOMP should ' &
      // 'not be used for'
      if(me.eq.0) WRITE(IWR,'(A,/)') ' license related applications.'
      if(me.eq.0) WRITE(IWR,'(A)')   ' For support:  Tel: 509.372.6070'
      if(me.eq.0) WRITE(IWR,'(A,/)') '               E-mail:  mark.white@pnl.gov'
      if(me.eq.0) WRITE(IWR,'(A,/)') '                      ---  OUTPUT  ---'






!
!---  Screen Echo ---
!
      if(me.eq.0) write(isc,'(A,//)')' Welcome to ...'
      if(me.eq.0) write(isc,'(A)')   '                           STOMP'
      if(me.eq.0) write(isc,'(A,//)')'        Subsurface Transport Over Multiple ' &
      // 'Phases'
      if(me.eq.0) write(isc,'(A)')   ' This file was produced by STOMP, a' &
      // ' numerical simulator'
      if(me.eq.0) write(isc,'(A)')   ' developed by the Pacific Northwest ' &
      // 'Laboratory, with'
      if(me.eq.0) write(isc,'(A)')   ' support from the VOC-Arid Integrated ' &
      // 'Demonstration Project,'
      if(me.eq.0) write(isc,'(A)')   ' Office of Technology Development, U.S. ' &
      // 'Department of Energy.'
      if(me.eq.0) write(isc,'(A)')   ' Results from this version of STOMP should ' &
      // 'not be used for'
      if(me.eq.0) write(isc,'(A,/)') ' license related applications.'
      if(me.eq.0) write(isc,'(A)')   ' For support:  Tel: 509.372.6070'
      if(me.eq.0) write(isc,'(A,/)') '               E-mail:  mark.white@pnl.gov'






      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of BANNER group ---
!
      RETURN
      END
