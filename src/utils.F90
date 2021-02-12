!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION OPENFILE( FDUM,IUNIT,ISBIN ) RESULT (T_OK)
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
!     Computes interfacial averages
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNNL, 21 June 2001.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE CONST
      USE SOLTN
      USE FILES
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
!
!----------------------Type Declarations-------------------------------!
!
       CHARACTER*64 ADUM,BDUM,CDUM
       CHARACTER(*),INTENT(IN) :: FDUM
       INTEGER IUNIT
       LOGICAL FCHK,ISBIN
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/OPENFIL'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: utils.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
      NCHF = LEN_TRIM(FDUM)
      INQUIRE( FILE=FDUM(1:NCHF), EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = TRIM(CARD)//' file does not exist: ' &
           // FDUM(1:NCHF)
        CALL WRMSGS( INDX )
      ENDIF
      IF( ISBIN.EQ..TRUE. ) THEN
        CDUM = 'unformatted'
        INDX = 1
        CHMSG = TRIM(CARD)//' file is unformatted: ' &
           // FDUM(1:NCHF)
        CALL WRMSGS( INDX )
      ELSE
        CDUM = 'formatted'
        INDX = 1
        CHMSG = TRIM(CARD)//' file is formatted: ' &
           // FDUM(1:NCHF)
        CALL WRMSGS( INDX )
      END IF
      IUNIT = 36
      OPEN( UNIT=IUNIT,FILE=FDUM(1:NCHF),STATUS='OLD',FORM=CDUM )
      T_OK = .TRUE.
      WRITE(ISC,'(/,2A)') TRIM(CARD), ' File: ',FDUM(1:NCHF)
!
!---  End of file open 
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION CHKSTAT( ADUM,ISTAT,INDX ) RESULT (T_OK)
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
!     Checks for successful array allocation, and prints error
!     if the array is not successfully allocated
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNNL, 21 June 2001.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
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
!
!----------------------Type Declarations-------------------------------!
!
       CHARACTER*64 ADUM
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/CHKSTAT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: utils.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX

      T_OK = .FALSE.
      IF( ISTAT.NE.0 )THEN
        CHMSG = 'Allocation Error for '//TRIM(ADUM)//' in ' //TRIM(CARD)
        CALL WRMSGS( INDX )
      ELSE
        T_OK = .TRUE.
      ENDIF
!
!---  End of chkstat
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION FILEEXISTS( CHDUM,ISTART,ICOMMA ) RESULT (T_OK)
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
!     Checks for successful array allocation, and prints error
!     if the array is not successfully allocated
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNNL, 21 June 2001.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
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
!
!----------------------Type Declarations-------------------------------!
!
       CHARACTER*512 CHDUM
       CHARACTER*64 GDUM,FDUM
       LOGICAL FLG_CHK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/FILEEXISTS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: utils.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
!
!---        Check that the file exists  ---
!
       CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
       NCH  = INDEX(FDUM,':')+1
       NCHF = INDEX(FDUM,'  ')-1
!
!---   Check that the file exists  ---
!
       INQUIRE( FILE=FDUM(NCH:NCHF), EXIST=FLG_CHK )
       IF( .NOT.FLG_CHK ) THEN
         INDX = 4
         CHMSG ='File does not exist for '//TRIM(VARB)// ': '// FDUM(NCH:NCHF)
         CALL WRMSGS( INDX )
       ENDIF
      IF( NCHF > 0 )T_OK = .TRUE.
!
!---  End of FILEEXISTS
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION GETFILENAME( CHDUM,T_FILENAME,ISTART,ICOMMA, &
        ISHDF5 ) RESULT (T_OK)
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
!     Checks for successful array allocation, and prints error
!     if the array is not successfully allocated
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNNL, 21 June 2001.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
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
!
!----------------------Type Declarations-------------------------------!
!
       CHARACTER*512 CHDUM
       CHARACTER*64 T_FILENAME,ADUM
       LOGICAL ISHDF5
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/GETFILENAME'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: utils.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
!
!---  Read filename and determine if hdf5 is used
!
      IDFLT = 0
      NCH = INDEX( VARB(1:),'  ' )
      VARB = VARB(1:NCH)//'External File,'
      CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
      ISHDF5 = .FALSE.
      IF( INDEX(ADUM(1:),'hdffile').NE.0 )  then
        ISHDF5 = .true.
      ENDIF
      ICOLON = INDEX(ADUM(1:),':') + 1
      NCHF = NCHA-ICOLON+1
      T_FILENAME = ''
      T_FILENAME(1:NCHF) = ADUM(ICOLON:NCHA)
      VARB = VARB(1:NCH)//T_FILENAME(1:NCHF)
      IF( NCHF > 0 )T_OK = .TRUE.
!
!---  End of GETFILENAME
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      REAL*8 FUNCTION GETUNITS( CHDUM,UNTS,ISTART,ICOMMA ) RESULT (VARX)
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
!     Checks for successful array allocation, and prints error
!     if the array is not successfully allocated
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNNL, 21 June 2001.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE FILES
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
!
!----------------------Type Declarations-------------------------------!
!
       CHARACTER*512 CHDUM
       CHARACTER*64 ADUM,UNTS,UNTSX
       LOGICAL ISHDF5
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/GETUNITX'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: utils.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
!
!---    Check for units  ---
!
       VARX = 1.D+0
       NCHU = INDEX( UNTS(1:),'  ' ) - 1
       UNTSX = ''
       IF( UNTS(1:NCHU).NE.'null' ) THEN
         IDFLT = 1
         UNTSX = UNTS
         CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTSX)
         INDX = 0
         CALL RDUNIT(UNTSX,VARX,INDX)
!BH
         IF(ME.EQ.0)WRITE(ISC,'(2X,3A)') VARB(1:IVR),', ',UNTSX(1:NCH)
         IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),', ',UNTSX(1:NCH)
       ELSE
         IF(ME.EQ.0)WRITE(ISC,'(2X,3A)') VARB(1:IVR),', ',UNTS
         IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),', ',UNTS
!BH
       ENDIF
       

!       IVR = INDEX( VARB(1:),'  ' ) - 1
!       IF(ME.EQ.0)WRITE(ISC,'(2X,3A)') VARB(1:IVR),', ',UNTSX(1:NCH)
!       IF(ME.EQ.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),', ',UNTSX(1:NCH)
!
!---  End of GETUNITX
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
