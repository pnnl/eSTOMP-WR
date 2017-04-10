!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSCLF1
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
!     Read input file for rock/soil scaling factor information.
!
!     1 -- Saturated Hydraulic Conductivity
!     2 -- Diffusive Porosity
!     3 -- van Genuchten "alpha" parameter
!     3 -- Brooks and Corey "psi" parameter
!     4 -- van Genuchten "n" parameter
!     4 -- Brooks and Corey "lambda" parameter
!     5 -- Residual saturation
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 24 April 2001.
!     Last Modified by MD White, PNNL, 24 April 2001.
!     $Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM
      CHARACTER*512 CHDUM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RDSCLF1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Write card information to output file  ---
!
      CARD = 'Scaling Factor Card'
      ICD = INDEX( CARD,'  ' )-1
      if(me.eq.0) WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read scaling factor equation types  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Saturated Hydraulic Conductivity Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(1) = 2
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(1) = 1
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
      VARB = 'Diffusive Porosity Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(2) = 2
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(2) = 1
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
      VARB = 'van Genuchten "alpha" or Brooks/Corey "psi" ' // &
      'Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(3) = 2
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(3) = 1
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
      VARB = 'van Genuchten "n" or Brooks/Corey "lambda" ' // &
      'Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(4) = 2
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(4) = 1
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
      VARB = 'Residual Saturation Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(5) = 2
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(5) = 1
        if(me.eq.0) WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
!
!---  Read input lines until all rock/soil types are found  ---
!
      N = 0
      IJK = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = 1, NROCK
        IF( ADUM .EQ. ROCK(M)) THEN
           IROCK = M
           GOTO 200
        ENDIF
  100 CONTINUE
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: '//ADUM(1:NCH)
      CALL WRMSGS( INDX )
      GOTO 10
  200 CONTINUE
      if(me.eq.0) WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      N = N+1
!
!---  Read saturated hydraulic conductivity (intrinsic permeability)
!     scaling factor ---
!
      IDFLT = 1
      VARB = 'Saturated Hydraulic Conductivity Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(1,IROCK))
!
!---  Read fracture saturated hydraulic conductivity
!     (intrinsic permeability) scaling factor ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR. &
      INDEX(ADUM(1:),'dp').NE.0 .OR. &
      INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture Saturated Hydraulic Conductivity' // &
        ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(6,IROCK))
      ENDIF
!
!---  Read diffusive porosity scaling factor  ---
!
      IDFLT = 1
      VARB = 'Diffusive Porosity Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(2,IROCK))
!
!---  Read fracture diffusive porosity scaling factor  ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR. &
      INDEX(ADUM(1:),'dp').NE.0 .OR. &
      INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture Diffusive Porosity' // &
        ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(7,IROCK))
      ENDIF
!
!---  Read van Genuchten "alpha" or Brooks/Corey "psi"
!     scaling factor ---
!
      IDFLT = 1
      VARB = 'van Genuchten "alpha" Brooks/Corey "psi" Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(3,IROCK))
!
!---  Read fracture van Genuchten "alpha" or Brooks/Corey "psi"
!     scaling factor ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR. &
      INDEX(ADUM(1:),'dp').NE.0 .OR. &
      INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture van Genuchten "alpha" Brooks/Corey "psi"' // &
        ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(8,IROCK))
      ENDIF
!
!---  Read van Genuchten "n" or Brooks/Corey "lambda"
!     scaling factor ---
!
      IDFLT = 1
      VARB = 'van Genuchten "n" Brooks/Corey "lambda" Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(4,IROCK))
!
!---  Read fracture van Genuchten "n" or Brooks/Corey "lambda"
!     scaling factor ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR. &
      INDEX(ADUM(1:),'dp').NE.0 .OR. &
      INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture van Genuchten "n" Brooks/Corey "lambda"' // &
        ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(9,IROCK))
      ENDIF
!
!---  Read residual saturation scaling factor ---
!
      IDFLT = 1
      VARB = 'Residual Saturation Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(5,IROCK))
!
!---  Read fracture residual saturation scaling factor ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR. &
      INDEX(ADUM(1:),'dp').NE.0 .OR. &
      INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture Residual Saturation' // &
        ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(10,IROCK))
      ENDIF
!
!---  Read scaling parameters for Mualem-Anisotropy
!     Relative Permeability ---
!
      IF( IRPL(IROCK).EQ.301 ) THEN
        IDFLT = 1
        VARB = 'van Genuchten "m" or Brooks/Corey "lambda"' // &
        ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(11,IROCK))
        IDFLT = 1
        VARB = 'Horizontal Pore-Scale Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(12,IROCK))
        IDFLT = 1
        VARB = 'Vertical Pore-Scale Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(13,IROCK))
      ENDIF
      IF( N .LT. NROCK .and. me.eq.0) WRITE(IWR,'(/)')
      GOTO 10
 500  CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDSCLF1 group ---
!
      RETURN
      END