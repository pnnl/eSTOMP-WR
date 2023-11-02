  !----------------------Subroutine--------------------------------------!
!
  SUBROUTINE SFIN
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
!     Surface flux and source term integrator
!
!   1	heat flux - UQV, VQV, WQV
!   2   aqueous volumetric flux - ULV, VLV, WLV
!   3   gas volumetric flux - UGV, VGV, WGV
!   4   NAPL volumetric flux - UNV, VNV, WNV
!   5   aqueous mass flux - ULM, VLM, WLM
!   6   gas mass flux - UGM, VGM, WGM
!   7   NAPL mass flux - UNM, VNM, WNM
!   8   salt mass flux - USM, VSM, WSM
!   9   dissolved-oil mass flux - ULO, VLO, WLO
!  10   condensate water mass flux - UWM, VWM, WWM
!  11   gas oil mass flux - UGOM, VGOM, WGOM
!  12   aqueous oil mass flux - ULOM, VLOM, WLOM
!  13   total oil mass flux - UOM, VOM, WOM
!  20   gas-advective heat flux - UGAQ, VGAQ, WGAQ
!  21   gas-advective water-mass flux - UGAW, VGAW, WGAW
!  22   gas-advective air-mass flux - UGAA, VGAA, WGAA
!  25   gas-diffusive heat flux - UGDQ, VGDQ, WGDQ
!  26   gas-diffusive water-mass flux - UGDW, VGDW, WGDW
!  27   gas-diffusive air-mass flux - UGDA, VGDA, WGDA
!  28   gas CO2 mass flux - UGAM, VGAM, WGAM
!  29   aqueous CO2 mass flux - ULAM, VLAM, WLAM
!  30   total CO2 mass flux - UAM, VAM, WAM
!  31   gas-advective oil-mass flux - UAGO, VAGO, WAGO
!  32   gas-diffusive oil-mass flux - UDGO, VDGO, WDGO
!  33   gas-total oil-mass flux - UGO, VGO, WGO
!  34   surface actual evaporation - AE
!  35   surface potential evaportion - PE
!  36   surface actual transpiration - AT
!  37   surface potential transpiration - PT
!  38   surface net total radiation - NTR
!  39   surface net short-wave radiation - NSWR
!  40   surface net long-wave radiation - NLWR
!  41   surface water-mass balance - WMB
!  42   surface rain-water runoff - RWRO
!  43   aqueous water mass flux - ULWM, VLWM, WLWM
!  44   gas water mass flux - UGWM, VGWM, WGWM
!  45   total water mass flux - UWM, VWM, WWM
!  46   aqueous-advective gas-component mass flux
!  47   aqueous-diffusive gas-component mass flux
!  48   gas-advective gas-component mass flux
!  49   gas-diffusive gas-component mass flux
!  50   total-advective gas-component mass flux
!  51   total-diffusive gas-component mass flux
!  52   surface evapotranspiration
! 101 to 100+NSOLU
!       solute flux - UC, VC, WC
! 101+NSOLU to 100+NSOLU+NEQC   
!       conservation-component species flux - UCC, VCC, WCC
! 101+NSOLU+NEQC to 100+NSOLU+NEQC+NEQK
!       kinetic-component species flux - UKC, VKC, WKC
  
  
  
  
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle,  February, 1993.
!     Last Modified by MD White, Battelle, June 18, 1995.
!     Last Modified by MD White, PNNL, 30 May 2002.
!     Last Modified by CV Freedman, PNNL, 7 January 2003.
!     Last Modified by CV Freedman, PNNL, 16 January 2003.
  
  
  
  
!     $Id: sfin.F,v 1.20 2009/05/15 14:58:59 d3c002 Exp $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE TRNSPT
  USE SOURC
  USE SOLTN
  USE OUTPU
  USE GRID
! USE FLUXT
  USE FLUXS
  USE FLUXP
  USE FLUXD
  USE FILES
! USE FDVT
  USE FDVP
  USE CONST
! USE BCVT
  USE BCVP
  USE BCV
! use bcvg
! use bcvs
  use grid_mod
  USE REACT
  USE PLT_ATM
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
!#include "tcgmsg.fh"
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  INTEGER NCX(LSF),ISFCX(6)
  LOGICAL IFLAG
  REAL*8:: idr(3),max_idr
  integer:: id_max
  integer:: vx_ceil,vy_ceil,vz_ceil
  REAL*8::et_fx,area_tmp
  real*8, dimension(:), allocatable::area_et(:)
!
!----------------------Executable Lines--------------------------------!
!
  me = ga_nodeid()
  SUBNMX = '/SFIN'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(181)(1:1),'$').EQ.0 ) CVS_ID(181) = &
   '$Id: sfin.F,v 1.20 2009/05/15 14:58:59 d3c002 Exp $' 
  ICSN = ICSN+ICSNX
!
!---  Integrate water mass, air mass, VOC mass, and energy source terms
!
!  IF( IEQW.GT.0 ) THEN
!    DO 10 N = 1,num_nodes
!      SRCIW(N) = SRCIW(N) + SRCW(2,N)*DT
!     10   CONTINUE
!  ENDIF
!  IF( IEQA.GT.0 ) THEN
!    DO 12 N = 1,num_nodes
!      SRCIA(N) = SRCIA(N) + SRCA(2,N)*DT
!     12   CONTINUE
!  ENDIF
!  IF( IEQT.GT.0 ) THEN
!    DO 16 N = 1,num_nodes
!      SRCIT(N) = SRCIT(N) + SRCT(2,N)*DT
!     16   CONTINUE
!  ENDIF
!  IF( IEQS.GT.0 ) THEN
!    DO 18 N = 1,num_nodes
!      SRCIS(N) = SRCIS(N) + SRCS(2,N)*DT
!     18   CONTINUE
!  ENDIF
  IF( NSF.EQ.0 ) THEN
    ICSN = ICSN-ICSNX
    SUBNM = SUBNM(1:ICSN)
    RETURN
  ENDIF
!
!---  Initialize surface file counter  ---
!
  DO 65 NS = 1,NSF
    NCX(NS) = 0
     65 CONTINUE
!
!---  Surface file  ---
!
  IF( IHSF.EQ.0 ) THEN
!
!---    Skip the default surface file if its unused  ---
!
    NSTART = 1
    IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---    Loop over the number of surface-flux files  ---
!
    DO 75 NSG = NSTART,NSFGP
!
!---      Open surface files  ---
!
      if(me.eq.0) OPEN(UNIT=ISF(NSG),FILE=FNSF(NSG),STATUS='UNKNOWN', &
           FORM='FORMATTED')
      CLOSE(UNIT=ISF(NSG),STATUS='DELETE')
      if(me.eq.0) OPEN(UNIT=ISF(NSG),FILE=FNSF(NSG),STATUS='NEW', &
        FORM='FORMATTED')
!
!---      Write header  ---
!
      if(me.eq.0) WRITE(ISF(NSG),'(A,//)')' Welcome to ...'
      if(me.eq.0) WRITE(ISF(NSG),'(A)')   '                           eSTOMP'
      if(me.eq.0) WRITE(ISF(NSG),'(A,//)')'                 A scalable version of' 
      if(me.eq.0) WRITE(ISF(NSG),'(A,//)')'        Subsurface Transport Over' &
        // ' Multiple Phases'
      if(me.eq.0) WRITE(ISF(NSG),'(A)')   ' This file was produced by eSTOMP,' &
        // ' a derivative work of STOMP'
      if(me.eq.0) WRITE(ISF(NSG),'(A)')   ' STOMP was developed by the Pacific Northwest' &
        // ' Laboratory, with'
      if(me.eq.0) WRITE(ISF(NSG),'(A)')   ' support from the VOC-Arid' &
        // ' Integrated Demonstration Project,'
      if(me.eq.0) WRITE(ISF(NSG),'(A)')   ' Office of Technology Development,' &
        // ' U.S. Department of Energy.'
      if(me.eq.0) WRITE(ISF(NSG),'(A)')   ' Results from this version of STOMP' &
        // ' should not be used for'
      if(me.eq.0) WRITE(ISF(NSG),'(A,/)') ' license related applications.'
      if(me.eq.0) WRITE(ISF(NSG),'(A,/)') ' For inquiries or assistance:' &
        // '  Call (509) 372-6070'
      if(me.eq.0) WRITE(ISF(NSG),'(A,//)')'                      ---  SURFACE ' &
        // ' ---'
      if(me.eq.0) WRITE(ISF(NSG),'(2A)') 'Version: ',CH_VRSN
      if(me.eq.0) WRITE(ISF(NSG),'(2A)') 'Date: ','Date system call inactive.'
      if(me.eq.0) WRITE(ISF(NSG),'(2A)') 'Time: ','Time system call inactive.'
  
      if(me.eq.0) WRITE(ISF(NSG),'(A,I4)') 'Number of Surfaces: ',ISFGP(NSG)
      if(me.eq.0) WRITE(ISF(NSG),'(A,/)') 'Surface Variables: '
!
!---  Loop over the defined surfaces  ---
!
      NCH = INDEX(UNTM,'  ')-1
      if(me.eq.0) WRITE(ISF(NSG),'(4A)') 'Time',',',UNTM(1:NCH),','
      DO 70 NS = 1,NSF
!
!---  Surface not associated with surface file,
!     skip output  ---
!
        IF( NSG.NE.ISFF(NS) ) GOTO 70
        NCH1 = INDEX(UNSF(1,NS),'  ')-1
        NCH2 = INDEX(UNSF(2,NS),'  ')-1
        IF( ISFT(NS).EQ.1 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Heat Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.2 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous Volumetric Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.3 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas Volumetric Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.4 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'NAPL Volumetric Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.5 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.6 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.7 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'NAPL Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.8 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Salt Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.9 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Dissolved-Oil Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.10 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Condensate Water Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.11 ) THEN
          IF( IOM.EQ.36 ) THEN
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas CH4 Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ELSEIF( IOM.EQ.35 ) THEN
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas CO2 Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ELSE
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas Oil Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ENDIF
        ELSEIF( ISFT(NS).EQ.12 ) THEN
          IF( IOM.EQ.36 ) THEN
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous CH4 Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ELSEIF( IOM.EQ.35 ) THEN
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous CO2 Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ELSE
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous Oil Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ENDIF
        ELSEIF( ISFT(NS).EQ.13 ) THEN
          IF( IOM.EQ.36 ) THEN
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Total CH4 Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ELSEIF( IOM.EQ.35 ) THEN
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Total CO2 Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ELSE
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Total Oil Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ENDIF
        ELSEIF( ISFT(NS).EQ.20 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Advective Heat Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.21 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Advective Water-Mass Flux', &
            ',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.22 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Advective Air-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.25 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive Heat Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.26 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive Water-Mass Flux', &
            ',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.27 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive Air-Mass Flux', &
            ',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.28 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas CO2 Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.29 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous CO2 Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.30 ) THEN
          IF( IOM.EQ.32 ) THEN
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Total CO2 Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ELSE
            if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Total Air Mass Flux',',', &
              UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
          ENDIF
        ELSEIF( ISFT(NS).EQ.31 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Advective Oil-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.32 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive Oil-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.33 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Total Oil-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.34 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Actual Evaporation,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.35 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Potential Evaporation,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.36 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Actual Transpiration,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.37 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Potential Transpiration,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.38 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Net Total Radiaion,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.39 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Net Short-Wave Radiaion,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.40 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Net Long-Wave Radiaion,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.41 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Water-Mass Balance,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.42 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface Rain-Water Runoff,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.43 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous Water Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.44 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas Water Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.45 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Total Water Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.46 ) THEN
          IGC = ISFGC(NS)
          NCH0 = INDEX(GCNM(IGC),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous-Advective ' //  &
            GCNM(IGC)(1:NCH0) // '-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.47 ) THEN
          IGC = ISFGC(NS)
          NCH0 = INDEX(GCNM(IGC),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Aqueous-Diffusive ' //  &
            GCNM(IGC)(1:NCH0) // '-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.48 ) THEN
          IGC = ISFGC(NS)
          NCH0 = INDEX(GCNM(IGC),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Advective ' //  &
            GCNM(IGC)(1:NCH0) // '-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.49 ) THEN
          IGC = ISFGC(NS)
          NCH0 = INDEX(GCNM(IGC),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive ' //  &
            GCNM(IGC)(1:NCH0) // '-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.50 ) THEN
          IGC = ISFGC(NS)
          NCH0 = INDEX(GCNM(IGC),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Total-Advective ' //  &
            GCNM(IGC)(1:NCH0) // '-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).EQ.51 ) THEN
          IGC = ISFGC(NS)
          NCH0 = INDEX(GCNM(IGC),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(6A)') 'Total-Diffusive ' //  &
            GCNM(IGC)(1:NCH0) // '-Mass Flux',',', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
!ET-BH
        ELSEIF( ISFT(NS).EQ.52 ) THEN
          if(me.eq.0) WRITE(ISF(NSG),'(5A)') 'Surface actual evapotranspiration,', &
            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).GT.100 .AND. ISFT(NS).LE.(100+NSOLU) ) THEN
          NSL = ISFT(NS)-100
          NCH = INDEX(SOLUT(NSL),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(7A)') 'Solute Flux (',SOLUT(NSL)(1:NCH), &
            '),',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
  
        ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND.  &
          ISFT(NS).LE.(100+NSOLU+NEQC) ) THEN
          NSL = ISFT(NS)-100
          NCH = INDEX(SOLUT(NSL),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(7A)') 'Conservation Component ' //  &
            'Species Flux (',SOLUT(NSL)(1:NCH), &
            '),',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ELSEIF( ISFT(NS).GT.(100+NSOLU+NEQC) .AND.  &
          ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
          NSL = ISFT(NS)-100
          NCH = INDEX(SOLUT(NSL),'  ')-1
          if(me.eq.0) WRITE(ISF(NSG),'(7A)') 'Kinetic Component ' //  &
            'Species Flux (',SOLUT(NSL)(1:NCH), &
            '),',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
        ENDIF
     70     CONTINUE
      if(me.eq.0) WRITE(ISF(NSG),'(/)')
     75   CONTINUE
  ENDIF
!
!---  Skip the default surface file if its unused  ---
!
  NSTART = 1
  IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---  Loop over the number of surface-flux files  ---
!
  DO  95 NSG = NSTART,NSFGP
    NCSX = 0
    NCUX = 0
!
!---    Write header every ten time steps  ---
!
    IF( IHSF.EQ.0 .OR. IHSF.EQ.10 ) THEN
      IHSF = 0
      if(me.eq.0) WRITE(ISF(NSG),9001) '    Time      '
      DO 80 NS = 1,NSF
!
!---        Surface not associated with surface file,
!           skip output  ---
!
        IF( NSG.NE.ISFF(NS) ) GOTO 80
        NCSX = NCSX + 1
        I = ABS(ISFT(NS))
        IF( I.GT.100 .AND. I.LE.(100+NSOLU) ) THEN
          I = 100
        ELSEIF( I.GT.(100+NSOLU) .AND. I.LE.(100+NSOLU+NEQC) ) THEN
          I = 102
        ELSEIF( I.GT.(100+NSOLU+NEQC) .AND.  &
          I.LE.(100+NSOLU+NEQC+NEQK) ) THEN
          I = 103
        ENDIF
        I = MIN( ABS(ISFT(NS)),100 )            
        J = ABS( ISFD(NS) )
        IF( ISFD(NS).EQ.4 .OR. ISFT(NS).EQ.52) J = 1
        IF( NS.EQ.NSF .OR. NCSX.EQ.ISFGP(NSG) ) THEN
          if(me.eq.0) WRITE(ISF(NSG),9002) CHSF(I,J)//'R','(',NS,')  '
          if(me.eq.0) WRITE(ISF(NSG),9012) CHSF(I,J)//'I','(',NS,')  '
        ELSE
          if(me.eq.0) WRITE(ISF(NSG),9002) CHSF(I,J)//'R','(',NS,')  '
          if(me.eq.0) WRITE(ISF(NSG),9002) CHSF(I,J)//'I','(',NS,')  '
        ENDIF
     80     CONTINUE
      if(me.eq.0) WRITE(ISF(NSG),9003) ' [',UNTM(1:8),'] '
      DO 90 NS = 1,NSF
!
!---        Surface not associated with surface file,
!           skip output  ---
!
        IF( NSG.NE.ISFF(NS) ) GOTO 90
        NCUX = NCUX + 1
        IF( NS.EQ.NSF .OR. NCUX.EQ.ISFGP(NSG) ) THEN
          if(me.eq.0) WRITE(ISF(NSG),9003) '[',UNSF(1,NS)(1:8),']  '
          if(me.eq.0) WRITE(ISF(NSG),9013) '[',UNSF(2,NS)(1:8),']  '
        ELSE
          if(me.eq.0) WRITE(ISF(NSG),9003) '[',UNSF(1,NS)(1:8),']  '
          if(me.eq.0) WRITE(ISF(NSG),9003) '[',UNSF(2,NS)(1:8),']  '
        ENDIF
     90     CONTINUE
    ENDIF
     95 CONTINUE
!
!---  Loop over the defined surfaces  ---
!
  allocate(area_et(nsf))
  area_et = 0.0
  DO 7000 NS = 1,NSF
    SF(1,NS) = 0.D+0
!    IF( ISFD(NS).EQ.4 ) THEN
!      NSFDOMX = NSFDOM(NS)
!    ELSE
      NSFDOMX = 1
!    ENDIF
!    write(*,*) 'NS: ', NS
    DO 6900 NC = 1,NSFDOMX
!    IF( ISFD(NS).EQ.4 ) THEN
!      ISFDX = ISFDOM(4,NC,NS)
!      DO 200 I = 1,3
!        ISFCX((I-1)*2+1) = ISFDOM(I,NC,NS)
!        ISFCX((I-1)*2+2) = ISFDOM(I,NC,NS)
!    200     CONTINUE
!    ELSE
      ISFDX = ISFD(NS)
!      DO 220 I = 1,6
!        ISFCX(I) = ISFC(I,NS)
!    220     CONTINUE
!    ENDIF
!
     
!      DO 1600 I = ISFCX(1),ISFCX(2)
!        DO 1500 J = ISFCX(3),ISFCX(4)
!          DO 1400 K = ISFCX(5),ISFCX(6)
!            N = ND(I,J,K)
     v_x = 0.D+0
     v_y = 0.D+0
     v_z = 0.D+0
     area_et(ns) = 0.D+0
     do nx = 1, num_loc_nodes
       n = id_l2g(nx)
       if(ixp(n) == 0.or.isfc(ns,n)<=0) cycle
!       write(*,*) 'nx,n:',nx,n
       if (isft(ns)==52) then
          et_fx = evap_trans(2,n) * dxgf(n) * dygf(n)  ! m/s*m*m
          area_et(ns) = area_et(ns) + dxgf(n) * dygf(n)
       endif
       do ifcx = 1,6
!            NPZ = NSZ(N)
         icnx = nd2cnx(ifcx,n)
         if(icnx > 0) then
            v_x = unvxc(icnx)
            v_y = unvyc(icnx)
            v_z = unvzc(icnx)
         elseif(icnx < 0) then
           icnx = abs(icnx)
           if(icnx <= num_bcnx) then
             v_x = uvxb(icnx)
             v_y = uvyb(icnx)
             v_z = uvzb(icnx)
            else
             icnx = icnx -num_bcnx
             v_x = uvxb_zf(icnx)
             v_y = uvyb_zf(icnx)
             v_z = uvzb_zf(icnx)
           endif
         endif
         isfdx = isfd(ns)
!         write(*,*) 'vx*isfdx,vy*isfdx,vz*isfdx: ',v_x*isfdx,v_y*isfdx,v_z*isfdx
!BH account for bfg
         if (ics.ne.3 .and. ics .ne. 8) then
            if(abs(v_x*isfdx) == 1.d0 .or. abs(v_y*isfdx) == 2.d0 .or. abs(v_z*isfdx) == 3.d0) then
               go to 9000
            else
               go to 9100
            endif
         else
            vx_ceil = ceiling(abs(v_x*isfdx))
            vy_ceil = ceiling(abs(v_y*isfdx))
            vz_ceil = ceiling(abs(v_z*isfdx))
!            write(*,*) 'vx_ceil,vy_ceil,vz_ceil:',vx_ceil,vy_ceil,vz_ceil
            if(vx_ceil == 1 .or. vy_ceil == 2 .or. vz_ceil == 3) then       
                go to 9000
            else
                go to 9100
            endif
         endif
!BH
         9000 continue         
         q_fx = 0.0d0
         c_fx = 0.0d0
         icnx = nd2cnx(ifcx,n)
!         print *, 'icnx: ',icnx
!         write(*,*) 'nx,n,ifcx,icnx:',nx,n,ifcx,icnx
         if(icnx > 0) then
            areaxx = areac(icnx)
            id_up = conn_up(icnx)
            id_dn = conn_dn(icnx)
            if(n.eq.id_dn .and. isfdx < 0) cycle
            if(n.eq.id_up .and. isfdx > 0) cycle
!            q_fx = q_flux(1,icnx)
            idirx = abs(isfdx)
!            write(*,*) 'isfdx: ',isfdx,' idirx: ',idirx
            if(isfdx > 0 ) then
                q_fx = q_flux_nd(idirx,id_up)
            else
                q_fx = q_flux_nd(idirx,id_up)
            endif
            if (ics.eq.3 .or. ics .eq. 8) then  
               if (vx_ceil == 1) then
                 q_fx = sqrt(v_x**2/(v_x**2+v_y**2+v_z**2))*q_fx
               else if (vy_ceil == 2) then
                 q_fx = sqrt(v_y**2/(v_x**2+v_y**2+v_z**2))*q_fx
! Fluxes from horizontal directions are dominant
! Top and bottom faces are not fixed yet. -BH
!               else if (vz_ceil == 3) then
!                 qx = q_flux_nd(1,id_up)
!                 qy = q_flux_nd(2,id_up)
!                 qz = q_flux_nd(3,id_up)
!                 do ifcx_z = 1,4
!                    icnx_z = nd2cnx(ifcx_z,n)
!                    if (icnx_z>0) then
!                      v_tmp(1) = unvxc(icnx_z)
!                      v_tmp(2) = unvyc(icnx_z)
!                      v_tmp(3) = unvzc(icnx_z)
!                      if
!                      qz = qz+sqrt(v_z**2/(v_x**2+v_y**2+v_z**2))*qx + &
!                         sqrt(v_y**2/(v_x**2+v_y**2+v_z**2))*qy
               endif
            endif          
!            write(*,*) 'q_fx:',q_fx
!            write(*,*) 'isft:',isft(ns)
            if( isft(ns).gt.100 .and.  &
               isft(ns).le.(100+nsolu) ) then
              nsl = isft(ns)-100
              if(isfdx > 0) then
                c_fx = c_flux_nd(idirx,nsl,id_up)
              else
                c_fx = c_flux_nd(idirx,nsl,id_up)
              endif
!              write(*,*) '0 c_fx:',c_fx
            elseif (isft(ns).gt.(100+nsolu) .and. &
              isft(ns).le.(100+nsolu+neqc+neqk)) then
              nsl = isft(ns)-100
              if(isfdx > 0) then
                c_fx = c_flux_nd(idirx,nsl,id_up)
              else
                c_fx = c_flux_nd(idirx,nsl,id_up)
              endif
!              write(*,*) '1 c_fx:',c_fx
            endif
            if(q_fx > 0.d0) then
              rholx = rhol(2,id_dn)
              xlwx =  xlw(2,id_dn)
            else
              rholx = rhol(2,id_up)
              xlwx = xlw(2,id_up)
            endif
         elseif(icnx < 0) then
           if((v_x+v_y+v_z)*isfdx <= 0) cycle
           icnx = abs(icnx)
           if(icnx <= num_bcnx) then
             areaxx = areab(icnx)
             q_fx = q_flux_b(1,icnx)
             if( isft(ns).gt.100 .and.  &
                isft(ns).le.(100+nsolu) ) then
               nsl = isft(ns)-100
               c_fx = c_flux_b(nsl,icnx)
             elseif (isft(ns).gt.(100+nsolu) .and. &
               isft(ns).le.(100+nsolu+neqc+neqk)) then
                nsl = isft(ns)-100
               c_fx = c_flux_b(nsl,icnx)
             endif
             if(q_fx > 0.d0) then
                rholx = rholb(2,icnx)
                xlwx = xlwb(2,icnx)
             else
                rholx = rhol(2,n)
                xlwx = xlw(2,n)
             endif
           else
             if((vx+vy+vz)*isfdx <= 0) cycle
             icnx = icnx -num_bcnx
             areaxx = areab_zf(icnx)
             v_x = uvxb_zf(icnx)
             v_y = uvyb_zf(icnx)
             v_z = uvzb_zf(icnx)
            if( isft(ns).gt.100 .and.  &
               isft(ns).le.(100+nsolu) ) then
               nsl = isft(ns)-100
               c_fx = 0.d0
            elseif (isft(ns).gt.(100+nsolu) .and. &
               isft(ns).le.(100+nsolu+neqc+neqk)) then
               nsl = isft(ns)-100
               c_fx = 0.d0
             endif
             q_fx = 0.d0
             rholx = rhol(2,n)
             rhogx = rhog(2,n)
             xlwx = xlw(2,n)
           endif 
         endif
!            IF( ISFSN(NS).EQ.1 ) THEN
              TLTZX = 1.D+0
!            ELSE
!              TLTZX = TLTZ(NPZ)
!            ENDIF
            IF( ISFT(NS).EQ.1 ) THEN
              sfx = tq_fx*areaxx
            ELSEIF( ISFT(NS).EQ.2 ) THEN
              sfx = q_fx*areaxx
            ELSEIF( ISFT(NS).EQ.3 ) THEN
              sfx = gq_fx*areaxx
            ELSEIF( ISFT(NS).EQ.4 ) THEN
              sfx = nq_fx*areaxx
            ELSEIF( ISFT(NS).EQ.5 ) THEN
              sfx = q_fx*areaxx*rholx
            ELSEIF( ISFT(NS).EQ.6 ) THEN
              sfx = gq_fx*areaxx*rhogx
            ELSEIF( ISFT(NS).EQ.7 ) THEN
              sfx = nq_fx*areaxx*rhonx
            ELSEIF( ISFT(NS).EQ.8 ) THEN
              sfx = sq_fx*areaxx
!            ELSEIF( ISFT(NS).EQ.9 ) THEN
!              SFX = WLO(1,NPZ)*AFZ(NPZ)
            ELSEIF( ISFT(NS).EQ.10 ) THEN

              sfx = q_fx*areaxx*rholx*xlwx + (dgw_fx-dla_fx)*areaxx*wtmw
! ET-BH
            ELSEIF( ISFT(NS).EQ.52 ) THEN
              sfx = et_fx 
!
!---            Gas-CH4 Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.11 ) THEN
!              SFX = AFZ(NPZ)*WGO(1,NPZ)
!
!---            Aqueous-CH4 Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.12 ) THEN
!              SFX = AFZ(NPZ)*WLO(1,NPZ)
!
!---            Total-CO2 Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.13 ) THEN
!              SFX = AFZ(NPZ)*(WGO(1,NPZ)+WLO(1,NPZ))
!            ELSEIF( ISFT(NS).EQ.20 ) THEN
!              sfx = (hgwx*xgwx+hgax*xgax)*rhogx*gq_fx*areaxx
!            ELSEIF( ISFT(NS).EQ.21 ) THEN
!              sfx = xgwx*rhogx*gq_fx*areaxx
!            ELSEIF( ISFT(NS).EQ.22 ) THEN
!              sfx = xgax*rhogx*gq_fx*areaxx
!            ELSEIF( ISFT(NS).EQ.25 ) THEN
!              sfx = dgw_fx*(dhgwx*wtmw-dhgax*wtma)*areaxx
!            ELSEIF( ISFT(NS).EQ.26 ) THEN
!              sfx = wtmw*dgw_fx*areaxx
!            ELSEIF( ISFT(NS).EQ.27 ) THEN
!              sfx = -wtma*dgw_fx*areaxx
!
!---            Gas-CO2 Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.28 ) THEN
!              sfx = areaxx*(gq_fx*xgax*rhogx-wtma*dgw_fx)
!
!---            Aqueous-CO2 Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.29 ) THEN
!              sfx = areaxx*(q_fx*xlax*rholx-wtma*dla_fx)
!
!---            Total-CO2 Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.30 ) THEN
!              sfx = areaxx*(q_fx*xlax*rholx+gq_fx*xgax*rhogx- &
!                wtma*dgw_fx+wtma*dla_fx)
!
!---            Gas-Advective Oil-Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.31 ) THEN
!              SFX = VAR*WG(1,NPZ)*AFZ(NPZ)
!
!---            Gas-Diffusive Oil-Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.32 ) THEN
!              SFX = WTMO*WDGO(1,NPZ)*AFZ(NPZ)
!
!---            Gas-Total Oil-Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.33 ) THEN
!              SFX = VAR*WG(1,NPZ)*AFZ(NPZ) &
!                + WTMO*WDGO(1,NPZ)*AFZ(NPZ)
!
!---            Aqueous-Water Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.43 ) THEN
!              IF( IOM.EQ.36 ) THEN
!                SFX = AFZ(NPZ)*WLW(1,NPZ)
!              ENDIF
!
!---            Gas-Water Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.44 ) THEN
!              IF( IOM.EQ.36 ) THEN
!                SFX = AFZ(NPZ)*WGW(1,NPZ)
!              ENDIF
!
!---            Total-Water Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.45 ) THEN
!              IF( IOM.EQ.36 ) THEN
!                SFX = AFZ(NPZ)*(WLW(1,NPZ) + WGW(1,NPZ))
!              ENDIF
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.46 ) THEN
!              IGC = ISFGC(NS)
!              SFX = (WLC(IGC,1,NPZ) - WDLC(IGC,1,NPZ)*GCPP(1,IGC)) &
!                *AFZ(NPZ)
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.47 ) THEN
!              IGC = ISFGC(NS)
!              SFX = WDLC(IGC,1,NPZ)*GCPP(1,IGC)*AFZ(NPZ)
!
!---            Gas-Advective Gas-Component Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.48 ) THEN
!              IGC = ISFGC(NS)
!              SFX = (WGC(IGC,1,NPZ) - WDGC(IGC,1,NPZ)*GCPP(1,IGC)) &
!                *AFZ(NPZ)
!
!---            Gas-Diffusive Gas-Component Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.49 ) THEN
!              IGC = ISFGC(NS)
!              SFX = WDGC(IGC,1,NPZ)*GCPP(1,IGC)*AFZ(NPZ)
!
!---            Total-Advective Gas-Component Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.50 ) THEN
!              IGC = ISFGC(NS)
!              SFX = (WGC(IGC,1,NPZ) + WLC(IGC,1,NPZ) -  &
!                (WDGC(IGC,1,NPZ) + WDLC(IGC,1,NPZ))*GCPP(1,IGC)) &
!                *AFZ(NPZ)
!
!---            Total-Diffusive Gas-Component Mass Flux  ---
!
!            ELSEIF( ISFT(NS).EQ.51 ) THEN
!              IGC = ISFGC(NS)
!              SFX = (WDGC(IGC,1,NPZ) + WDLC(IGC,1,NPZ))*GCPP(1,IGC) &
!                *AFZ(NPZ)
            ELSEIF( ISFT(NS).GT.100 .AND.  &
              ISFT(NS).LE.(100+NSOLU) ) THEN
              sfx = areaxx*c_fx
            ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND.  &
              ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) .AND. &
              ISLC(40).EQ.1 ) THEN
              sfx = areaxx*c_fx*1.D-3
            ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. &
              ISFT(NS).LE.(100+(2*NSOLU)) ) THEN
              sfx = areaxx*c_fx*1.d-3
            ENDIF
            RSFDX = REAL(ISFDX)
            IF (ISFT(NS) .NE. 52) THEN
              SF(1,NS) = SF(1,NS)+SFX*SIGN(1.D+0,RSFDX)/(TLTZX+SMALL)
            ENDIF
!   1400         CONTINUE
!   1500       CONTINUE
!   1600     CONTINUE
!        endif
       9100 continue
      enddo
      IF (ISFT(NS)==52) THEN
         SF(1,NS) = SF(1,NS)+SFX
      ENDIF
    enddo
   6900 CONTINUE
   SF(2,NS) = SF(2,NS) + SF(1,NS)*DT
   7000 CONTINUE
  VAR = TM
  IF( UNTM.NE.'null' ) THEN
    INDX = 1
    IUNS = 1
    CALL RDUNIT(UNTM,VAR,INDX)
  ENDIF
  IF( ABS(VAR).LT.1.D-99 ) VAR = 0.D+0
!
!---    Skip the default surface file if its unused  ---
!
  NSTART = 1
  IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---    Loop over surface-flux files, writing surface-flux
!       time  ---
!
  DO 7500 NSG = NSTART,NSFGP
    if(me.eq.0) WRITE(ISF(NSG),9005) VAR,' '
   7500 CONTINUE
  DO 8000 NS = 1,NSF
    VAR = SF(1,NS)
    call ga_dgop(1,var,1,'+')
    if (isft(ns)==52) then
      area_tmp = area_et(ns)
      call ga_dgop(1,area_tmp,1,'+')
      VAR = VAR/(area_tmp+small)
    endif
    IF( UNSF(1,NS).NE.'null' ) THEN
      INDX = 1
      IF( ISFT(NS).EQ.1 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -3
      ELSEIF( ISFT(NS).EQ.2 ) THEN
        IUNM = 3
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.3 ) THEN
        IUNM = 3
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.4 ) THEN
        IUNM = 3
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.5 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.6 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.-6 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.7 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.8 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.9 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.11 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.12 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.13 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.20 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -3
      ELSEIF( ISFT(NS).EQ.21 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.22 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.25 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -3
      ELSEIF( ISFT(NS).EQ.26 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.27 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.28 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.29 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.30 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.31 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.32 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.33 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.34 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.35 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.36 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.37 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.38 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -3
      ELSEIF( ISFT(NS).EQ.39 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -3
      ELSEIF( ISFT(NS).EQ.40 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -3
      ELSEIF( ISFT(NS).EQ.41 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.42 ) THEN
        IUNM = 3
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.43 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.44 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.45 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).EQ.52 ) THEN
        IUNM = 1
        IUNS = -1
!
!---      Aqueous-advective, aqueous-diffusive, gas-advective, 
!         gas-diffusive, total-advective, and total-diffusive
!         gas-component mass flux  ---
!
      ELSEIF( ISFT(NS).GE.45 .AND. ISFT(NS).LE.51 ) THEN
        IUNKG = 1
        IUNS = -1
      ELSEIF( ISFT(NS).GT.100 .AND. ISFT(NS).LE.(100+NSOLU) ) THEN
        IUNS = -1
  
      ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND.  &
        ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
        IUNMOL = 1
        IUNS = -1
      ENDIF
      CALL RDUNIT(UNSF(1,NS),VAR,INDX)
    ENDIF
    IF( ABS(VAR).LT.1.D-99 ) VAR = 0.D+0
!
!---    Skip the default surface file if its unused  ---
!
    NSTART = 1
    IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---    Loop over surface-flux files, writing surface-flux
!       rates  ---
!
    DO 7800 NSG = NSTART,NSFGP
!
!---      Surface not associated with surface file,
!         skip output  ---
!
      IF( NSG.EQ.ISFF(NS) .and. me.eq.0) WRITE(ISF(NSG),9005) VAR,' '
   7800   CONTINUE
    VAR = SF(2,NS)
    call ga_dgop(1,var,1,'+')
    if (isft(ns)==52) then
      area_tmp = area_et(ns)
      call ga_dgop(1,area_tmp,1,'+')
      VAR = VAR/(area_tmp+small)
    endif
    IF( UNSF(2,NS).NE.'null' ) THEN
      INDX = 1
      IF( ISFT(NS).EQ.1 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -2
      ELSEIF( ISFT(NS).EQ.2 ) THEN
        IUNM = 3
      ELSEIF( ISFT(NS).EQ.3 ) THEN
        IUNM = 3
      ELSEIF( ISFT(NS).EQ.4 ) THEN
        IUNM = 3
      ELSEIF( ISFT(NS).EQ.5 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.6 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.-6 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.7 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.8 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.9 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.11 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.12 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.13 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.20 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -2
      ELSEIF( ISFT(NS).EQ.21 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.22 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.25 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -2
      ELSEIF( ISFT(NS).EQ.26 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.27 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.28 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.29 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.30 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.31 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.32 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.33 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.34 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.35 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.36 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.37 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.38 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -2
      ELSEIF( ISFT(NS).EQ.39 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -2
      ELSEIF( ISFT(NS).EQ.40 ) THEN
        IUNM = 2
        IUNKG = 1
        IUNS = -2
      ELSEIF( ISFT(NS).EQ.41 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.42 ) THEN
        IUNM = 3
      ELSEIF( ISFT(NS).EQ.43 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.44 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.45 ) THEN
        IUNKG = 1
      ELSEIF( ISFT(NS).EQ.52 ) THEN
        IUNM = 1
!
!---      Aqueous-advective, aqueous-diffusive, gas-advective, 
!         gas-diffusive, total-advective, and total-diffusive
!         gas-component mass flux  ---
!
      ELSEIF( ISFT(NS).GE.45 .AND. ISFT(NS).LE.51 ) THEN
        IUNKG = 1
  
!
!---      Conservation component species and kinetic component
!         species molar flux  ---
!
      ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND.  &
        ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
        IUNMOL = 1
  
      ENDIF
      CALL RDUNIT(UNSF(2,NS),VAR,INDX)
    ENDIF
    IF( ABS(VAR).LT.1.D-99 ) VAR = 0.D+0
!
!---    Skip the default surface file if its unused  ---
!
    NSTART = 1
    IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---    Loop over surface-flux files, writing surface-flux
!       integrals  ---
!
    DO 7900 NSG = NSTART,NSFGP
!
!---      Surface not associated with surface file,
!         skip output  ---
!
      IF( ISFF(NS).EQ.NSG ) THEN
        NCX(ISFF(NS)) = NCX(ISFF(NS)) + 1
        IF( NS.EQ.NSF .OR. NCX(ISFF(NS)).EQ.ISFGP(NSG) ) THEN
          if(me.eq.0) WRITE(ISF(NSG),9015) VAR
        ELSE
          if(me.eq.0) WRITE(ISF(NSG),9005) VAR,' '
        ENDIF
      ENDIF
   7900   CONTINUE
   8000 CONTINUE
  IHSF = IHSF + 1
  deallocate(area_et) 
!
!---  Format Statements  ---
!
   9001 FORMAT(A,$)
   9002 FORMAT(X,A,A,I3,A,$)
   9003 FORMAT(2X,A,A,A,$)
   9005 FORMAT(1PE13.6,A,$)
   9012 FORMAT(X,A,A,I3,A)
   9013 FORMAT(2X,A,A,A)
   9015 FORMAT(1PE13.6)
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SFIN group
!
  RETURN
  END
