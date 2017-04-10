!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSIMU
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
!     Read input file for simulation title information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     $Id: rdsimu.F,v 1.1.1.1 2011/01/17 20:36:35 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE FILES
      USE BUFFEREDREAD
#ifdef USE_E4D
      USE E4D_LINK
#endif
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64  ADUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDSIMU'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      if(.not.allocated(cvs_id)) allocate(cvs_id(400))
      IF( INDEX(CVS_ID(160)(1:1),'$').EQ.0 ) CVS_ID(160) = &
        '$Id: rdsimu.F90,v 1.1.1.1 2012/02/11 12:36:35 d3k870 Exp $' 
!
!---  Write number of cores to  ouput file  ---
!
      NPROC = GA_NNODES()
      IF(ME.EQ.0)WRITE(IWR,'(/,A,I8)') &
        'Number of Cores used in STOMP simulation: ',NPROC
#ifdef USE_E4D
      IF((ME.EQ.0).AND.(E4DFLAG)) THEN
         WRITE(IWR,'(/,A,I8)') &
        'Number of Cores used in E4D simulation: ',E4DPROCS
      ENDIF
#endif
!
!---  Write card information to ouput file  ---
!
      CARD = 'Simulation Title Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read version number  ---
!
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(/,A)') 'Version Number: ' // &
       'See Configuration Version Record Below'
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      VARB = 'Simulation Title'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,TITLE)
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(3A)') VARB(1:IVR),': ',TITLE(1:NCH)
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      VARB = 'User Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,USER)
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(3A)') VARB(1:IVR),': ',USER(1:NCH)
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      VARB = 'Company Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,CMPNY)
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(3A)') VARB(1:IVR),': ',CMPNY(1:NCH)
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      VARB = 'Input Creation Date'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,INPDAT)
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(3A)') VARB(1:IVR),': ',INPDAT(1:NCH)
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      VARB = 'Input Creation Time'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,INPTIM)
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(3A)') VARB(1:IVR),': ',INPTIM(1:NCH)
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      VARB = 'Number of Simulation Note Lines: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      LNOTES = MAX( 1,NLIN )
      ALLOCATE(NOTES(LNOTES))
      DO 96 L = 1,LNOTES
       NOTES(L) = ' '
   96 CONTINUE

!
!---  Read simulation notes  ---
!
      VARB = 'Simulation Notes: '
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE (IWR,'(/,A,/)') VARB
      DO 120 NL = 1,NLIN
        T_OK = BUFFEREDREAD_GETLINE(NOTES(NL))
        ICD = INDEX( CARD,'  ' )-1
        IF( ME.EQ.0 )WRITE(IWR,'(A)') NOTES(NL)
  120 CONTINUE
!
!---  End of RDSIMU group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
      RETURN
      END
