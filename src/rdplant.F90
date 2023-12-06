    
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE RDPLANT
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
!     Read input file for plant property information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Gene Freeman, PNNL, 9 April 2002.
!     Last Modified by MD White, PNNL, 9 April 2002.
!     $Id: rdplant.F,v 1.1.1.1 2011/01/17 20:36:35 d3c002 Exp $
!-------------------List of input parameters and units ----------------!
!
!     Maximum extent in Z (Zm), m
!     Maximum extent in X (Xm), m
!     Maximum extent in Y (Ym), m
!     Fit parameter in Z (z*), m
!     Fit parameter in X (x*), m
!     Fit parameter in Y (y*), m
!     Leaf area index (LAI), unitless
!     Plant canopy height (PCH), m
!     Water stress head 1 (h1), m
!     Water stress head 2 (h2), m
!     Water stress head 3 (h3), m
!     Water stress head 4 (h4), m
!     Water uptake reduced by 50% (h50), m
!     Crop coefficient - stage1, unitless
!     Crop start time jan.1=1, dec.31=365, day
!     Crop coefficient - stage2, unitless
!     Crop start time jan.1=1, dec.31=365, day
!     Crop coefficient - stage3, unitless
!     Crop start time jan.1=1, dec.31=365, day
!     Crop coefficient - stage4, unitless
!     Crop start time jan.1=1, dec.31=365, day
!     Evapotranspiration (ET), m/day
!
  
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE FILES
      USE CONST
      USE BCV
      USE GRID_MOD
      USE GRID
      USE BUFFEREDREAD
      USE BCVP
      USE PLT_ATM
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
 
#include "mafdecls.fh"
#include "global.fh" 
#include "utils.h"  
  
!
!----------------------Type Declarations-------------------------------!
!
  CHARACTER*64 UNTS,ADUM
  CHARACTER*512 CHDUM
  INTEGER :: NCROPCX
  character(len=8) :: fmt,XIP ! format descriptor
<<<<<<< HEAD
  fmt = '(I1)' 
!
=======
  REAL*8::RSF ! root scaling factor
  fmt = '(I1)' 

>>>>>>> v523
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Executable Lines--------------------------------!
!
  me = ga_nodeid()
  use_ga = .true.
  SUBNMX = '/RDPLANT'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
 '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $'
  ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
  CARD = 'Plant Properties Card'
  ICD = INDEX( CARD,'  ' )-1
  if(me==0) WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of plant varietals  ---
!
  ISTART = 1
  T_OK = BUFFEREDREAD_GETLINE(CHDUM)
  CALL LCASE( CHDUM )
  ISTART = 1
  VARB = 'Number of Plants'
  CALL RDINT( ISTART,ICOMMA,CHDUM,NPLANT )
 
  allocate(PLANT(NPLANT))
  allocate(PLF_P(5,NPLANT))
<<<<<<< HEAD
  allocate(RSD_P(3,NPLANT))
=======
  allocate(RSD_P(4,NPLANT))
>>>>>>> v523
  allocate(IPLF_P(NPLANT))
  PLF_P = 0.0
  RSD_P = 0.0
  IPLF_P = 0
 
!
!---  Loop over the plants information lines  ---
!
  DO 500 IP = 1,NPLANT
!    write(*,*) 'IP',IP
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    ISTART = 1
    VARB = 'Plant Name: '
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,PLANT(IP))
    IF (IP .GT. 1) THEN
      DO 100 M = 1,IP-1
        IF( PLANT(M).EQ.PLANT(IP) ) THEN
          INDX = 4
          CHMSG = 'Duplicate Plant Name: ' // PLANT(IP)
          CALL WRMSGS( INDX )
        ENDIF
      100   CONTINUE
    ENDIF
   IF(ME==0)  WRITE (IWR,'(/,A,I2,2A)') '#',IP,' Plant Name: ',PLANT(IP)
!
!---  Check for root stress or stomatal resistance options  ---
!
   CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
   IF( INDX.EQ.1 ) THEN
     VARB = 'Plant Options'
     CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
     IF( INDEX(ADUM(1:),'vrugt').NE.0 ) THEN
       IPLF_P(IP) = 1
       IF(ME==0) WRITE(IWR,'(A)') '  Vrugt Root Stress Model'
     ELSEIF( INDEX(ADUM(1:),'jarvis').NE.0 ) THEN
       IPLF_P(IP) = 2
       IF(ME==0) WRITE(IWR,'(A)') '  Jarvis Root Stress Model'
     ELSEIF( INDEX(ADUM(1:),'zeng').NE.0 ) THEN
       IF ( INDEX(ADUM(1:),'scaling').NE.0 ) THEN
         IPLF_P(IP) = 4
<<<<<<< HEAD
         IF(ME==0) WRITE(IWR,'(A)') '  Zeng Root Stress Model with Scaling'
=======
         NULL_ENTRY = 0
         CALL CHKDPR(ISTART,ICOMMA,CHDUM,INDX_RT)
!         IF(ME==0) WRITE(IWR,'(A)') '  Zeng Root Stress Model with Scaling'
         CALL RDDPR(ISTART,ICOMMA,CHDUM,RSF)
         IF (NULL_ENTRY==1) THEN
           RSD_P(4,IP) = 1.0
           IF(ME==0) WRITE(IWR,'(A,F8.4)') '  Zeng Root Stress Model with Default Scaling Factor of 1.'
         ELSE
           RSD_P(4,IP) = RSF
           IF(ME==0) WRITE(IWR,'(A,F8.4)') '  Zeng Root Stress Model with Scaling Factor of ',RSF 
         ENDIF
>>>>>>> v523
       ELSE
         IPLF_P(IP) = 3
         IF(ME==0) WRITE(IWR,'(A)') '  Zeng Root Stress Model without Scaling'
       ENDIF
     ELSE
       IF(ME==0) WRITE(IWR,'(A)') '  No Available Root Stress Model (Vrugt, Jarivs or Zeng) Identified'
     ENDIF
   ELSE
     IF(ME==0) WRITE(IWR,'(A)') '  No Root Stress Model'
   ENDIF
!
!---  Read root (Z) depth characteristics  ---
!
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    ISTART = 1
    VARB = 'Max. Root Depth (zm)'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,RSD_P(1,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',RSD_P(1,IP)
    INDX = 0
    IUNM = 1
    CALL RDUNIT(UNTS,RSD_P(1,IP),INDX)
    IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RSD_P(1,IP),', m)'

    IF( IPLF_P(IP) < 3 ) THEN
!   z* and pz are only needed for Vrugt and Jarvis models 
!
!---    Read root z* characteristics  ---
!
      VARB = 'Null Root Depth (z*) for Vrugt/Jarvis models'
      IUNM = 1
      CALL RDDPR(ISTART,ICOMMA,CHDUM,RSD_P(2,IP))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',RSD_P(2,IP)
      INDX = 0
      CALL RDUNIT(UNTS,RSD_P(2,IP),INDX)
      IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RSD_P(2,IP),', m)'
!
!---    Read root pz characteristics  ---
!
      VARB = 'Root Depth fit parameter (pz) for Vrugt/Jarvis models'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,RSD_P(3,IP))
      IF(ME==0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR), &
        ': ',RSD_P(3,IP)
!
!---  Zeng root stress model
!
    ELSE
!
!---  Read roota (root distribution parameter a)  ---
!
      VARB = 'Zeng Root Distribution Parameter A'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,RSD_P(2,IP))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS) 
      IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',RSD_P(2,IP) 
      IUNM = -1
      INDX = 0
      CALL RDUNIT(UNTS,RSD_P(2,IP),INDX)
      IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RSD_P(2,IP),', 1/m)'   
!
!---  Read rootb (root distribution parameter b)  ---
!
      VARB = 'Zeng Root Distribution Parameter B'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,RSD_P(3,IP))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',RSD_P(3,IP)
      IUNM = -1
      INDX = 0
      CALL RDUNIT(UNTS,RSD_P(3,IP),INDX)
      IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',RSD_P(3,IP),', 1/m)'

    ENDIF 
!
!---  Read plant limiting functions
!
!---  Vrugt root stress model
!
  IF( IPLF_P(IP).EQ.1 ) THEN
!
!---  Read first stress point head  ---
!
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    ISTART = 1
    VARB = 'Vrugt Plant Limiting Fun. Root Stress-Point 1 (Capillary Head)'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(1,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',PLF_P(1,IP)
    PLF_P(1,IP) = abs(PLF_P(1,IP))
    INDX = 0
    IUNM = 1
    CALL RDUNIT(UNTS,PLF_P(1,IP),INDX)
    IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',PLF_P(1,IP),', m)'
!
!---  Read second stress point head  ---
!
    VARB = 'Vrugt Plant Limiting Fun. Root Stress-Point 2 (Capillary Head)'
    IUNM = 1
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(2,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',PLF_P(2,IP)
    PLF_P(2,IP) = abs(PLF_P(2,IP))
    INDX = 0
    CALL RDUNIT(UNTS,PLF_P(2,IP),INDX)
    IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',PLF_P(2,IP),', m)'
!
!---  Read third stress point head  ---
!
    VARB = 'Vrugt Plant Limiting Fun. Root Stress-Point 3 (Capillary Head)'
    IUNM = 1
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(3,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',PLF_P(3,IP)
    PLF_P(3,IP) = abs(PLF_P(3,IP))
    INDX = 0
    CALL RDUNIT(UNTS,PLF_P(3,IP),INDX)
    IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',PLF_P(3,IP),', m)'
!
!---  Read fourth stress point head  ---
!
    VARB = 'Vrugt Plant Limiting Fun. Root Stress-Point 4 (Capillary Head)'
    IUNM = 1
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(4,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',PLF_P(4,IP)
    PLF_P(4,IP) = abs(PLF_P(4,IP))
    INDX = 0
    CALL RDUNIT(UNTS,PLF_P(4,IP),INDX)
    IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',PLF_P(4,IP),', m)'
!
!---  Jarvis root stress model
!
  ELSEIF( IPLF_P(IP).EQ.2 ) THEN
!
!---  Read first Jarvis stress point  ---
!
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    ISTART = 1
    VARB = 'Jarvis Plant Limiting Fun. Wilting-Point Water Content'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(1,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF (PLF_P(1,IP)<0 .OR. PLF_P(1,IP)>1) THEN
      INDX = 4
      CHMSG = 'Parameter out of bounds (0~1): ' // VARB
      CALL WRMSGS( INDX )
    ELSE
      IF(ME==0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PLF_P(1,IP)
    ENDIF
!
!---  Read second Jarvis stress point  ---
!
    VARB = 'Jarvis Plant Limiting Fun. Soil Water Content: Critical Point 1'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(2,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF (PLF_P(2,IP)<0 .OR. PLF_P(2,IP)>1) THEN
      INDX = 4
      CHMSG = 'Parameter out of bounds (0~1): ' // VARB
      CALL WRMSGS( INDX )
    ELSE
      IF(ME==0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB,': ',PLF_P(2,IP)
    ENDIF
!
!---  Read third Jarvis stress point  ---
!
    VARB = 'Jarvis Plant Limiting Fun. Soil Water Content: Critical Point 2'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(3,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF (PLF_P(3,IP)<0 .OR. PLF_P(3,IP)>1) THEN
      INDX = 4
      CHMSG = 'Parameter out of bounds (0~1): ' // VARB
      CALL WRMSGS( INDX )
    ELSE
      IF(ME==0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB,': ',PLF_P(3,IP)
    ENDIF
!
!---  Read fourth Jarvis stress point  ---
!
    VARB = 'Jarvis Plant Limiting Fun. Saturated Water Content'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(4,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF (PLF_P(4,IP)<0 .OR. PLF_P(4,IP)>1) THEN
      INDX = 4
      CHMSG = 'Parameter out of bounds (0~1): ' // VARB
      CALL WRMSGS( INDX )
    ELSE
      IF(ME==0) WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PLF_P(4,IP)
    ENDIF
!
!--- Zeng root stress model
!
  ELSEIF( IPLF_P(IP).EQ.3 .OR. IPLF_P(IP).EQ.4) THEN
!
!---      Read matric potential based on stomatal conditions  ---
!
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    ISTART = 1

    VARB = 'Matric Potential When Stomata Fully Closed'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(1,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &  
        ': ', PLF_P(1,IP)
    PLF_P(1,IP) = abs(PLF_P(1,IP))
    INDX = 0
    IUNM = 1
    CALL RDUNIT(UNTS,PLF_P(1,IP),INDX)
    IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',PLF_P(1,IP),', m)'

    VARB = 'Matric Potential When Stomata Fully Open'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,PLF_P(2,IP))
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &  
        ': ', PLF_P(2,IP)
    PLF_P(2,IP) = abs(PLF_P(2,IP))
    INDX = 0
    IUNM = 1
    CALL RDUNIT(UNTS,PLF_P(2,IP),INDX)
    IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',PLF_P(2,IP),', m)'

 ENDIF

    IF( NCROP_P(IP) > 0 ) THEN
!
!---  Read number of crop coefficient ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Crop Coefficients'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NCROPCX)
      IF(ME==0) WRITE(IWR,'(2X,2A,I2)') VARB(1:IVR), &
                   ': ',NCROPCX
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE(CHDUM)

! Loop through number of coefficients 
      ISTART = 1
      DO IC = 1,NCROPCX
!
!---  Read crop coefficient day of year ---
!
        VARB = 'Crop coefficient, time'
        IUNS = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CROP_P(1,IC,IP))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        IF(ME==0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
          ': ',CROP_P(1,IC,IP)
        INDX = 0
        CALL RDUNIT(UNTS,CROP_P(1,IC,IP),INDX)
        IF(ME==0) WRITE(IWR,'(A,1PE11.4,A)') ' (',CROP_P(1,IC,IP),', s)'
        IF (CROP_P(1,IC,IP)>365.25*24*60*60) THEN
          INDX = 4
          write(XIP,FMT),IP
          CHMSG = 'Crop coefficient time out of range ( >1 year/365.25 day) for plant #'//XIP  
          CALL WRMSGS( INDX )          
        ENDIF
!
!---  Read crop coefficient ---
!
        VARB = 'Crop Coefficient '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CROP_P(2,IC,IP))
        IF(ME==0) WRITE(IWR,'(2X,A,I2,A,1PE11.4)') VARB(1:IVR),IC, &
          ': ',CROP_P(2,IC,IP)
      ENDDO
    ELSE
      ! No crop coefficient are specified in input file
      IF (NCROP_P(IP) == 0) THEN
      ! "0," was specified in the input file
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      ENDIF
!      ELSEIF (NCROP_P(IP) == -2) THEN 
      ! constant 1 was used.
      IF(ME==0) WRITE(IWR,'(2X,A)') 'No crop coefficients specified for this plant.'
      CROP_P(1,1,IP) = 100.0*24*60*60
      CROP_P(2,1,IP) = 1.0
      CROP_P(1,2,IP) = 300.0*24*60*60
      CROP_P(2,2,IP) = 1.0
    ENDIF
!
!---  Read next plant type  ---
!
    IF( IP.LT.NPLANT .AND. ME==0 )  WRITE(IWR,'(/)')
  500  CONTINUE

  ISUB_LOG = ISUB_LOG-1
!
!---  End of RDPLNT group ---
!
  RETURN
  END
