!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRMSGS( INDX )
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
!     Write warnings and error messages to the screen and output file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, September, 1994.
!     Last Modified by MD White, Battelle, PNL, September 4, 1998.
!     $Id: wrmsg.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      EXTERNAL ICOUNT
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*9 FORM1
      CHARACTER*9 FORM2
      CHARACTER*17 FORM3
      CHARACTER*19 FORM4
      CHARACTER*9 FORM5
      CHARACTER*9 FORM6
      CHARACTER*19 FORM14
      CHARACTER*19 FORM17
      CHARACTER*19 FORM21
      CHARACTER*9 FORM25
      integer me
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2,FORM3,FORM4,FORM5,FORM6
      DATA FORM1 / '(/,3A,I4)' /
      DATA FORM2 / '(/,2A,I6)' /
      DATA FORM3 / '(/,2A,I6,1PE11.4)' /
      DATA FORM4 / '(/,3A,I6,A,1PE11.4)' /
      DATA FORM5 / '(/,3A,I6)' /
      DATA FORM6 / '(/,2A,I6)' /
      DATA FORM14 / '(/,A,I6,2A,1PE11.4)' /
      DATA FORM17 / '(/,A,I6,2A,1PE11.4)' /
      DATA FORM21 / '(/,A,I6,2A,1PE11.4)' /
      DATA FORM25 / '(/,3A,I6)' /
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      SUBNMX = '/WRMSGS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(276)(1:1),'$').EQ.0 ) CVS_ID(276) = &
       '$Id: wrmsg.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      NCH = INDEX( CHMSG(1:),'  ' )-1
      IF( INDX.EQ.0 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,A)') CHMSG(:NCH)
          WRITE(IWR,'(/,A)') CHMSG(:NCH)
        endif
      ELSEIF( INDX.EQ.1 ) THEN
        if (me.eq.0) then
          ! Not sure why the message would be substringed here.  The message
          ! does not contain critical information - kls
          WRITE(ISC,'(2A)') 'NOTE: ',CHMSG(:NCH)
          WRITE(IWR,'(2A)') 'NOTE: ',CHMSG(:NCH)
        endif
      ELSEIF( INDX.EQ.2 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'INPUT WARNING: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'INPUT WARNING: ',CHMSG(:NCH)
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
      ELSEIF( INDX.EQ.3 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'EXECUTION ERROR: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'EXECUTION ERROR: ',CHMSG(:NCH)
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.4 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.5 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.6 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
      ELSEIF( INDX.EQ.7 ) THEN
        if (me.eq.0) then
          WRITE(FORM1(8:8),'(I1)') ICOUNT( IMSG )
          WRITE(ISC,FORM1) 'INPUT ERROR: ',CHMSG(:NCH),': ',IMSG
          WRITE(IWR,FORM1) 'INPUT ERROR: ',CHMSG(:NCH),': ',IMSG
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.8 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.9 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,3A,1PE11.4)') 'INPUT ERROR: ',CHMSG(:NCH),': ', &
            RLMSG
          WRITE(IWR,'(/,3A,1PE11.4)') 'INPUT ERROR: ',CHMSG(:NCH),': ', &
            RLMSG
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.10 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ', &
            CHMSG(:NCH),': ',RLMSG
          WRITE(IWR,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ', &
            CHMSG(:NCH),': ',RLMSG
        endif
      ELSEIF( INDX.EQ.11 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ', &
            CHMSG(:NCH),': ',RLMSG
          WRITE(IWR,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ', &
            CHMSG(:NCH),': ',RLMSG
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.12 ) THEN
        if (me.eq.0) then
          WRITE(FORM2(8:8),'(I1)') ICOUNT( IMSG )
          WRITE(ISC,FORM2) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
          WRITE(IWR,FORM2) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.13 ) THEN
        if (me.eq.0) then
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM14) 'EXECUTION WARNING: NODE = ', &
            N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'EXECUTION WARNING: NODE = ', &
            N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
      ELSEIF( INDX.EQ.14 ) THEN
        if (me.eq.0) then
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM14) 'EXECUTION ERROR: NODE = ', &
            N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'EXECUTION ERROR: NODE = ', &
            N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.15 ) THEN
        if (me.eq.0) then
          WRITE(FORM3(8:8),'(I1)') ICOUNT( IMSG )
          WRITE(ISC,FORM3) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG,RLMSG
          WRITE(IWR,FORM3) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG,RLMSG
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.16 ) THEN
        if (me.eq.0) then
          WRITE(FORM4(8:8),'(I1)') ICOUNT( IMSG )
          WRITE(ISC,FORM4) 'INPUT ERROR: ',CHMSG(:NCH), &
            ': ',IMSG,': ',RLMSG
          WRITE(IWR,FORM4) 'INPUT ERROR: ',CHMSG(:NCH), &
            ': ',IMSG,': ',RLMSG
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.17 ) THEN
        if (me.eq.0) then
          WRITE(FORM17(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM17) 'STATE CONDIITON ERROR: NODE = ', &
            N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM17) 'STATE CONDITION ERROR: NODE = ', &
            N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.18 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.19 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A,1PE11.4)')'EXECUTION ERROR: ',CHMSG(:NCH),RLMSG
          WRITE(IWR,'(/,2A,1PE11.4)')'EXECUTION ERROR: ',CHMSG(:NCH),RLMSG
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.20 ) THEN
        if (me.eq.0) then
          WRITE(FORM6(8:8),'(I1)') ICOUNT( IMSG )
          WRITE(ISC,FORM6) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
          WRITE(IWR,FORM6) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
      ELSEIF( INDX.EQ.21 ) THEN
        if (me.eq.0) then
          WRITE(FORM21(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM21) 'STATE CONDIITON ERROR: NODE = ', &
            N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM21) 'STATE CONDITION ERROR: NODE = ', &
            N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
      ELSEIF( INDX.EQ.22 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.21 ) THEN
        if (me.eq.0) then
          WRITE(ISC,'(/,2A)') 'OUTPUT ERROR: ',CHMSG(:NCH)
          WRITE(IWR,'(/,2A)') 'OUTPUT ERROR: ',CHMSG(:NCH)
          WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
          WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        endif
!        CALL WRCVS
        call ga_error("Fatal STOMP error",0)
      ELSEIF( INDX.EQ.24 ) THEN
        if (me.eq.0) then
          WRITE(FORM1(8:8),'(I1)') ICOUNT( IMSG )
          WRITE(ISC,FORM1) 'INPUT WARNING: ',CHMSG(:NCH),': ',IMSG
          WRITE(IWR,FORM1) 'INPUT WARNING: ',CHMSG(:NCH),': ',IMSG
          WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
          WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        endif
      ELSEIF( INDX.EQ.25 ) THEN
        if (me.eq.0) then
          WRITE(FORM25(8:8),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM25) 'EXECUTION NOTE: ',CHMSG(:NCH), &
            ': NODE =',N_DB
          WRITE(IWR,FORM25) 'EXECUTION NOTE: ',CHMSG(:NCH), &
            ': NODE =',N_DB
        endif
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRMSGS group
!
      RETURN
      END
