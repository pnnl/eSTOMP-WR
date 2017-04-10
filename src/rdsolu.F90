!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSOLU
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
!     Read input file for solution control information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL November 1992.
!     Last Modified by MD White, PNNL, June 9, 1994.
!     Last Modified by MD White, PNNL, October 28, 1999.
!     Last Modified by MD White, PNNL, November 11, 1999.
!     Last Modified by MD White, PNNL, 5 October 2001.
!     $Id: rdsolu.F90,v 1.1.1.1 2009/03/30 18:42:52 d3m045 Exp $
!
!     Solution Control Variable Definitions
!
!     ISLC(1):   0    Patankar Solute Transport
!                1    Leonard-TVD Solute Transport
!                2    Roe's Superbee Solute Transport
!                3    Upwind Solute Transport
!     ISLC(2):   0    Zero Vapor Diffusion
!                1    Constant Vapor Diffusion Coefficient
!                2    Variable Vapor Diffusion Coefficient
!                3    Enhanced Vapor Diffusion Coefficient
!                4    Enhanced Vapor Diffusion Coefficient (Thermal)
!     ISLC(3):   0    Zero Tortuosity
!                1    Variable Tortuosity
!     ISLC(4):   0    Zero Aqueous-Phase Diffusion
!                1    Constant Aqueous-Phase Diffusion Coefficient
!                2    Variable Aqueous-Phase Diffusion Coefficient
!     ISLC(5):   0    No Soil Freezing
!                1    Soil Freezing Conditions
!     ISLC(6):   0    Patankar Salt Transport
!                1    Leonard Flux Limiter (TVD) Salt Transport
!     ISLC(7):   0    Without Osmotic Pressure Effects
!                1    With Osmotic Pressure Effects
!                2    With Osmotic Pressure and Surface Tension Effects
!                3    With Surface Tension Effects
!     ISLC(8):   0    Patankar Oil Transport
!                1    Leonard-TVD Oil Transport
!                2    Roe's Superbee Oil Transport
!                3    Upwind Oil Transport
!     ISLC(9):   0    Water Properties
!                1    Fluid Properties
!     ISLC(10):  0    Patankar Dissolved Air Transport
!                1    Leonard Flux Limiter (TVD) Dissolved Air Transport
!     ISLC(11):  0    First-order time differencing
!                1    Second-order time differencing
!     ISLC(12):  0    Diffusive Dissolved-Oil Transport
!                1    Diffusive-Dispersive Dissolved-Oil Transport
!     ISLC(13):  0    No Particle-Displacing Bubbles
!                1    Particle-Displacing Bubbles
!     ISLC(14):  0    No SPLIB summary output
!                1    SPLIB summary output
!     ISLC(15):  0    No Adaptive grid
!                1    Adaptive grid
!     ISLC(16):  0    No Density Dependent Solute Transport
!                1    Density Dependent Solute Transport
!     ISLC(17):  0    No Courant Number Control
!                1    Courant Number Control
!                2    Special Vadose Zone Courant Number Control
!     ISLC(18):  0    Intermediate Restart Files with Plot Files
!                1    No Intermediate Restart Files
!                2    No Restart Files
!     ISLC(19):  0    No Scaling Factors
!                1    Scaling Factors
!     ISLC(20):  0    No Inverse (UCode)
!                1    Inverse (UCode)
!     ISLC(21):  0    Restart: Current Operational Mode
!                1    Restart: Water Operational Mode
!                2    Restart: Water-Air Operational Mode
!                3    Restart: Water-Air-Energy Operational Mode
!                4    Restart: Water-Oil Operational Mode
!                5    Restart: Water-Oil-Air Operational Mode
!                6    Restart: Water-Oil-Air-Energy Operational Mode
!                7    Restart: Water-Oil-Alcohol Operational Mode
!               11    Restart: Water-Salt Operational Mode
!               12    Restart: Water-Air-Salt Operational Mode
!               13    Restart: Water-Air-Salt-Energy Operational Mode
!     ISLC(22):  0    Default Salt Functions
!     ISLC(23):  0    Advective Solute Transport
!                1    No Aqueous Advective Solute Transport
!               10    No Gaseous Advective Solute Transport
!              100    No NAPL Advective Solute Transport
!               11    No Aqueous-Gaseous Solute Transport
!              101    No Aqueous-NAPL Advective Solute Transport
!              110    No Gaseous-NAPL Advective Solute Transport
!              111    No Advective Solute Transport
!     ISLC(24):  0    No Plants
!                1    Single Temperature Plants
!                2    Multiple Temperature Plants
!               10    No Plants (w/ Time-Lag Scheme)
!               11    Single Temperature Plants (w/ Time-Lag Scheme)
!               12    Multiple Temperature Plants (w/ Time-Lag Scheme)
!     ISLC(25):  0    No Poynting Effect (CO2 Solubility)
!                1    Poynting Effect (CO2 Solubility)
!     ISLC(26):  0    No Rainfall Interception by Plants
!                1    Rainfall Interception by Plants
!     ISLC(27):  0    Aqueous Molar Density Gradient Diffusion
!                1    Aqueous Mole Fraction Gradient Diffusion
!     ISLC(28):  0    Gas Molar Density Gradient Diffusion
!                1    Gas Mole Fraction Gradient Diffusion
!     ISLC(29):  0    NAPL Molar Density Gradient Diffusion
!                1    NAPL Mole Fraction Gradient Diffusion
!     ISLC(30):  0    Nonisothermal
!                1    Isothermal
!     ISLC(31):  0    Equilibrium Hydrate Saturation
!                1    First-Order Kinetic Hydrate Saturation
!     ISLC(32):  0    Nonisobrine
!                1    Isobrine
!     ISLC(40):  0    No Reactive Transport
!                1    ECKEChem Reactive Transport
!     ISLC(41):  0    No hydrate inhibitor effects
!                1    Hydrate inhibitor effects
!     ISLC(42):  0    No initial species concentration guessing
!                1    Initial species concentration guessing
!     ISLC(43):  0    No porosity alteration with precipitation
!                1    Porosity alteration with precipitation
!                2    Porosity alteration with precipitation perko
!     ISLC(44):  0    Invoke vapor pressure lowering
!                1    No vapor pressure lowering
!     ISLC(45):  0    CO2 Solution 
!                1    No CO2 Solution (Iso-CO2)
!     ISLC(46):  0    No Kinetic Volatilization
!                1    Kinetic Volatilization
!     ISLC(47):  0    Flow Calculation 
!                1    No Flow Calculation
!     ISLC(48):  0    Static Domain 
!                1    Dynamic Domain
!     ISLC(49):  0    No NAPL Surface Spill 
!                1    NAPL Surface Spill
!     ISLC(56):  0    No effective mineral reaction area
!                1    Effective mineral reaction area scaled by water saturation
!     ISLC(57):  0    No reduced equilibrium equations (eckechem)
!                1    Reduced equilibrium equations (eckechem)
!     ISLC(58):  0    No mixing coefficient provided in file read for TST formulatio
!                1    Mixing coefficient provided in file read for TST formulation (
!     ISLC(59):  0    Use default value for minimum concentration (1.D-30) in eckech
!                1    Provide minimum concentration value for eckechem
!     ISLC(60):  0    Use default non-log formulation for solving chemistry in eckec
!                1    Use log formulation for solving chemistry in eckechem
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE REACT
      USE NCG_PT
      USE JACOB
      USE FLUXS
      USE FILES
      USE FDVH
      USE CONST
      USE BUFFEREDREAD
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



      PARAMETER( LCV=20,LCS=8 )
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER NCV(LCV),NCS(LCS)
      CHARACTER*64 UNTS,CHIFV(LCV),CHIFS(LCS)
      CHARACTER*128 ADUM,FDUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE CHIFS,CHIFV,NCV,NCS
      DATA CHIFS /'harmonic','geometric','arithmetic','upwind', &
                 'downstream','null', &
                 'neiber downstream','null'/ 
      DATA CHIFV /'thermal conductivity','aqueous density', &
                 'gas density','napl density','aqueous viscosity', &
                 'gas viscosity','napl viscosity', &
                 'aqueous relative permeability', &
                 'gas relative permeability', &
                 'napl relative permeability', &
                 'intrinsic permeability','water gas diffusion', &
                 'oil gas diffusion','air aqueous diffusion', &
                 'oil aqueous diffusion','solute diffusion', &
                 'hydraulic dispersion','salt aqueous diffusion', &
                 'effective permeability','air gas diffusion'/ 
      DATA NCV /20,15,11,12,17,13,14,29,25,26,22,19,17,21,21, &
               16,20,22,22,17/
      DATA NCS /8,9,10,6,10,4,17,4/
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDSOLU'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      if(.not.allocated(cvs_id)) allocate(cvs_id(400))
      IF( INDEX(CVS_ID(161)(1:1),'$').EQ.0 ) CVS_ID(161) = &
      '$Id: rdsolu.F90,v 1.1.1.1 2009/03/30 18:42:52 d3m045 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  if(me.eq.0) WRITE card information to ouput file  ---
!
      CARD = 'Solution Control Card'
      ICD = INDEX( CARD,'  ' )-1
      if(me.eq.0) WRITE (ISC,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read Execution Option  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      if(me.eq.0) WRITE(ISC,'(/,A,$)') 'Execution Option: '
      VARB = 'Execution Option'
      ISTART = 1
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'normal').NE.0 ) THEN
        IEO = 1
        IF( INDEX(ADUM(1:),'no flow').NE.0 ) THEN
          ISLC(47) = 1
          if(me.eq.0) WRITE(ISC,'(A)') 'Normal'
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ No Flow'
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Static Domain'
        ELSEIF( INDEX(ADUM(1:),'dynamic').NE.0 ) THEN
          ISLC(48) = 1
          if(me.eq.0) WRITE(ISC,'(A)') '  Normal'
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Dynamic Domain'
        ELSE
          if(me.eq.0) WRITE(ISC,'(A)') '  Normal'
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Static Domain'
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'restart').NE.0 ) THEN
        IEO = 2
        IF( INDEX(ADUM(1:),'no flow').NE.0 ) THEN
          ISLC(47) = 1
          if(me.eq.0) WRITE(ISC,'(A)') 'Restart'
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ No Flow'
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Static Domain'
        ELSEIF( INDEX(ADUM(1:),'dynamic').NE.0 ) THEN
          ISLC(48) = 1
          if(me.eq.0) WRITE(ISC,'(A)') 'Restart'
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Dynamic Domain'
        ELSE
          if(me.eq.0) WRITE(ISC,'(A)') 'Restart'
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Static Domain'
        ENDIF
        IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          IDFLT = 1
          FDUM = FNRS
          VARB = 'Restart File Name'
          CALL RDCHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
          if(me.eq.0) WRITE(ISC,'(2X,3A)') VARB(1:IVR),': ',FDUM(1:NCHF)
          FNRS = FDUM(1:NCHF)
        ENDIF
        IF( INDEX(ADUM(1:),'mode').NE.0 ) THEN
          VARB = 'Restart File Operational Mode'
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISLC(21))
          if(me.eq.0) WRITE(ISC,'(2X,2A,I3)') VARB(1:IVR),': ',ISLC(21)
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'initial').NE.0 ) THEN
        IEO = 3
        if(me.eq.0) WRITE(ISC,'(A)') 'Initial Conditions'
      ELSE
        INDX = 4
        NCH = INDEX( ADUM(1:),'  ' )-1
        CHMSG = 'Unrecognized Execution Option: ' // ADUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
      IF( INDEX(ADUM(1:),'second').NE.0  .AND.&
       INDEX(ADUM(1:),'order').NE.0 ) THEN
        IF( IOM.EQ.30 ) THEN
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Second-Order Time Differencing ' //&
           'Not Implemented for This Operational Mode'
          CALL WRMSGS( INDX )
        ENDIF
        if(me.eq.0) WRITE(ISC,'(A)') '  w/ Second-Order Time Differencing'
        ISLC(11) = 1
      ELSE
        if(me.eq.0) WRITE(ISC,'(A)') '  w/ First-Order Time Differencing'
        ISLC(11) = 0
      ENDIF
!
!---  Scaling Factor Option  ---
!
      IF( INDEX(ADUM(1:),'scaling').NE.0 ) THEN
        if(me.eq.0) WRITE(ISC,'(A)') '  w/ Scaling Factors'
        ISLC(19) = 1
      ENDIF
!
!---  Inverse (UCode) Option  ---
!
      IF( INDEX(ADUM(1:),'inverse').NE.0 .OR.&
       INDEX(ADUM(1:),'ucode').NE.0 ) THEN
        if(me.eq.0) WRITE(ISC,'(A)') '  w/ Inverse (UCode)'
        ISLC(20) = 1
      ENDIF
!
!---  Linear System Solver Option  ---
!
      NLBD = LBD
      NLSP = LSP
      NLPT = LPT
      IF( INDEX(CHDUM(1:),'summary').NE.0 ) ISLC(14) = 1
!      IF( NLBD.EQ.1 .AND. NLSP.EQ.0 .AND. NLPT.EQ.0) THEN
!      if(index(adum(1:),'bd').ne.0 ) then
!        if(me.eq.0) WRITE(ISC,'(A)') 'Linear System Solver: Direct Banded'
!        ILES = 1
!      ELSEIF( NLBD.EQ.0 .AND. NLSP.EQ.1 .AND. NLPT.EQ.0) THEN
!      elseif(index(adum(1:),'splib').ne.0 ) then
!        if(me.eq.0) WRITE(ISC,'(A)') 'Linear System Solver: SPLIB'
!        if(me.eq.0) WRITE(ISC,'(A)') '  Preconditioner: ILU(k)'
!        if(me.eq.0) WRITE(ISC,'(A)') '  Solver: BiCGStab'
!        ILES = 3
!      ELSEIF( NLBD.EQ.0 .AND. NLSP.EQ.0 .AND. NLPT.EQ.1) THEN
!      ELSEIF(index(adum(1:),'petsc').ne.0) then
        if(me.eq.0) WRITE(ISC,'(A)') 'Linear System Solver: PETSc'
        if(me.eq.0) WRITE(ISC,'(A)') '  Preconditioner: block Jacobi w/ILU(0)'
        if(me.eq.0) WRITE(ISC,'(A)') '  Solver: BiCGStab'
        ILES = 5
!      ELSE
!        INDX = 4
!        CHMSG = 'Unrecognized Linear System Solver Option'
!        CALL WRMSGS( INDX )
!      ENDIF
      petsc_option = 0
      if (index(adum(1:),'petsc').ne.0) then
        if(me.eq.0) WRITE(ISC,'(A)') 'User provided PETSc solver convergency options '
        petsc_option = 1
        VARB = 'The Relative Convergence Tolerance'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RTOL)
        if(me.eq.0) WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',RTOL
        if(me.eq.0) WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',RTOL
        VARB = 'The Absolution Convergence Tolerance'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,ATOL)
        if(me.eq.0) WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',ATOL
        if(me.eq.0) WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',ATOL
      endif
!
!---  Read Operational Mode  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Operational Mode'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'transport').NE.0 ) LC = 1
      IF( INDEX(ADUM(1:),'eckechem').NE.0 )  LR = 1
      NEQ = 0
!
!---  Hydrate inhibitors  ---
!
!      I_INH = 0
!      IF( INDEX(ADUM(1:),'nacl').NE.0 ) I_INH = 1
!      IF( INDEX(ADUM(1:),'cacl2').NE.0 ) I_INH = 2
!      IF( INDEX(ADUM(1:),'kcl').NE.0 ) I_INH = 3
!      IF( INDEX(ADUM(1:),'nabr').NE.0 ) I_INH = 4
!      IF( INDEX(ADUM(1:),'kbr').NE.0 ) I_INH = 5
!      IF( INDEX(ADUM(1:),'hcoona').NE.0 ) I_INH = 6
!      IF( INDEX(ADUM(1:),'hcook').NE.0 ) I_INH = 7
!      IF( INDEX(ADUM(1:),'hcoocs').NE.0 ) I_INH = 8
!      IF( INDEX(ADUM(1:),'k2co3').NE.0 ) I_INH = 9
!      IF( INDEX(ADUM(1:),'methanol').NE.0 ) I_INH = 10
!      IF( INDEX(ADUM(1:),'ethanol').NE.0 ) I_INH = 11
!      IF( INDEX(ADUM(1:),'glycerol').NE.0 ) I_INH = 12
!      IF( INDEX(ADUM(1:),'meg').NE.0 ) I_INH = 13
!      IF( INDEX(ADUM(1:),'deg').NE.0 ) I_INH = 14
!      IF( INDEX(ADUM(1:),'teg').NE.0 ) I_INH = 15
!      IGAS = 0
!      IAQU = 0
!      INAPL = 0
!
!---  H2O-CO2-CH4-HCO2-HCH4-NaCl-E Operational Mode  ---
!
!      IF( (INDEX(ADUM(1:),'energy').NE.0 .OR.&
!       INDEX(ADUM(1:),'-e').NE.0) .AND.&
!       (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'mco2').NE.0 .AND.&
!       INDEX(ADUM(1:),'mch4').NE.0 .AND.&
!       INDEX(ADUM(1:),'hco2').NE.0 .AND.&
!       INDEX(ADUM(1:),'hch4').NE.0 .AND.&
!       I_INH.NE.0 ) THEN
!        IEQT = 1
!        IEQW = 2
!        IEQA = 3
!        IEQDO = 4
!        IEQHA = 5
!        IEQHO = 6
!        IEQS = 7
!        NEQ = 7
!        NPH = 3
!        IGAS = 1
!        IAQU = 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'H2O-CO2-CH4-HCO2-HCH4-NaCl-E (STOMP-HYD-KNC)'
!        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) THEN
!          ISLC(30) = 1
!          IEQT = 0
!          IEQW = IEQW-1
!          IEQA = IEQA-1
!          IEQDO = IEQDO-1
!          IEQHA = IEQHA-1
!          IEQHO = IEQHO-1
!          IEQS = IEQS-1
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isothermal Option'
!        ENDIF
!        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
!          ISLC(32) = 1
!          IEQS = 0
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isobrine Option'
!        ENDIF
!        IF( INDEX(ADUM(1:),'iso-co2').NE.0 ) THEN
!          ISLC(45) = 1
!          IEQA = 0
!          IEQHA = 0
!          IEQDO = IEQDO-1
!          IEQHO = IEQHO-2
!          IF( ISLC(32).EQ.0 ) IEQS = IEQS-2
!          NEQ = NEQ-2
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isocarbon Option'
!        ENDIF
!        IF( INDEX(ADUM(1:),'inhibitor').NE.0 ) THEN
!          ISLC(41) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Inhibitor Effects'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Inhibitor Effects'
!        ENDIF
!        IOM = 39
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
!          ISLC(25) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Poynting Effect for ' //&
!           'CO2 Solubility'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Poynting Effect for ' //&
!           'CO2 Solubility'
!        ENDIF
!
!---  H2O-CO2-CH4-NaCl-E Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR.&
!       INDEX(ADUM(1:),'-e').NE.0) .AND.&
!       (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'co2').NE.0 .AND.&
!       INDEX(ADUM(1:),'ch4').NE.0 .AND.&
!       I_INH.NE.0 ) THEN
!        IEQT = 1
!        IEQW = 2
!        IEQA = 3
!        IEQDO = 4
!        IEQS = 5
!        NEQ = 5
!        NPH = 3
!        IGAS = 1
!        IAQU = 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'H2O-CO2-CH4-NaCl-E (STOMP-HYD)'
!        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) THEN
!          ISLC(30) = 1
!          IEQT = 0
!          IEQW = IEQW-1
!          IEQA = IEQA-1
!          IEQDO = IEQDO-1
!          IEQS = IEQS-1
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isothermal Option'
!        ENDIF
!        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
!          ISLC(32) = 1
!          IEQS = 0
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isobrine Option'
!        ENDIF
!        IF( INDEX(ADUM(1:),'iso-co2').NE.0 ) THEN
!          ISLC(45) = 1
!          IEQA = 0
!          IEQDO = IEQDO-1
!          IF( ISLC(32).EQ.0 ) IEQS = IEQS-1
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isocarbon Option'
!        ENDIF
!        IF( INDEX(ADUM(1:),'inhibitor').NE.0 ) THEN
!          ISLC(41) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Inhibitor Effects'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Inhibitor Effects'
!        ENDIF
!        IOM = 36
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
!          ISLC(25) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Poynting Effect for ' //&
!           'CO2 Solubility'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Poynting Effect for ' //&
!           'CO2 Solubility'
!        ENDIF
!
!---  H2O-Air-CO2-NaCl-E Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR.&
!       INDEX(ADUM(1:),'-e').NE.0) .AND.&
!       (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'air').NE.0 .AND.&
!       INDEX(ADUM(1:),'co2').NE.0 .AND.&
!       (INDEX(ADUM(1:),'salt').NE.0 .OR.&
!       INDEX(ADUM(1:),'nacl').NE.0) ) THEN
!        IEQT = 1
!        IEQW = 2
!        IEQA = 3
!        IEQO = 4
!        IEQS = 5
!        NEQ = 5
!        NPH = 3
!        IGAS = 1
!        IAQU = 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operation Mode: ' // &
!         'H2O-CO2-Air-NaCl-E (STOMP-CO2ae)'
!        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) THEN
          ISLC(30) = 1
!          IEQT = 0
!          IEQW = 1
!          IEQA = 2
!          IEQO = 3
!          IEQS = 4
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isothermal Option'
!        ENDIF
!        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
!          ISLC(32) = 1
!          IEQS = 0
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isobrine Option'
!        ENDIF
!        IOM = 35
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
!          ISLC(25) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Poynting Effect for ' //&
!           'CO2 Solubility'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Poynting Effect for ' //&
!           'CO2 Solubility'
!        ENDIF
!
!---  H2O-CO2-Dissolved CO2-NaCl Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'dissolved co2').NE.0 .AND. &
!       (INDEX(ADUM(1:),'salt').NE.0 .OR.&
!       INDEX(ADUM(1:),'nacl').NE.0) ) THEN
!        IEQW = 1
!        IEQA = 2
!        IEQDA = 3
!        IEQS = 4
!        NEQ = 4
!        NPH = 2
!        IGAS = 1
!        IAQU = 1
!        IOM = 34
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'H2O-CO2-Dissolved CO2-NaCl (STOMP-CO2d)'
!        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
!          ISLC(25) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Poynting Effect for ' //&
!           'CO2 Solubility'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Poynting Effect for ' //&
!           'CO2 Solubility'
!        ENDIF
!
!---  H2O-CO2-NaCl-E Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR.&
!       INDEX(ADUM(1:),'-e').NE.0) .AND.&
!       (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'co2').NE.0 .AND.&
!       (INDEX(ADUM(1:),'salt').NE.0 .OR.&
!       INDEX(ADUM(1:),'nacl').NE.0) ) THEN
!        IEQT = 1
!        IEQW = 2
!        IEQA = 3
!        IEQS = 4
!        NEQ = 4
!        NPH = 2
!        IGAS = 1
!        IAQU = 1
!        IOM = 33
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'H2O-CO2-NaCl-E (STOMP-CO2e)'
!        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) THEN
!          ISLC(30) = 1
!          IEQT = 0
!          IEQW = 1
!          IEQA = 2
!          IEQS = 3
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isothermal Option'
!        ENDIF
!        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
!          ISLC(32) = 1
!          IEQS = 0
!          NEQ = NEQ-1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Isobrine Option'
!        ENDIF
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
!          ISLC(25) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Poynting Effect for ' //&
!           'CO2 Solubility'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Poynting Effect for ' // &
!          'CO2 Solubility'
!        ENDIF
!
!---  H2O-CO2-NaCl Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'co2').NE.0 .AND.&
!       (INDEX(ADUM(1:),'salt').NE.0 .OR.&
!       INDEX(ADUM(1:),'nacl').NE.0) ) THEN
!        IEQW = 1
!        IEQA = 2
!        IEQS = 3
!        NEQ = 3
!        NPH = 2
!        IGAS = 1
!        IAQU = 1
!        IOM = 32
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'H2O-CO2-NaCl (STOMP-CO2)'
!        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
!          ISLC(25) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Poynting Effect for ' //&
!           'CO2 Solubility'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Poynting Effect for ' //&
!           'CO2 Solubility'
!        ENDIF
!
!---  Water-Air-Salt-Energy (H2O-Air-NaCl-E) Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR.&
!       INDEX(ADUM(1:),'-e').NE.0) .AND.&
!       (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'air').NE.0 .AND.&
!       (INDEX(ADUM(1:),'salt').NE.0 .OR.&
!       INDEX(ADUM(1:),'nacl').NE.0) ) THEN
!        IEQT = 1
!        IEQW = 2
!        IEQA = 3
!        IEQS = 4
!        NEQ = 4
!        NPH = 2
!        IGAS = 1
!        IAQU = 1
!        IOM = 13
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Air-Salt-Energy (STOMP-WASE)'
!        IF( INDEX(ADUM(1:),'lfl' ).NE.0 ) THEN
!          ISLC(6) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Dissolved Salt'
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Leonard Flux Limiting Transport'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Dissolved Salt'
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Patankar Transport'
!        ENDIF
!        IF( INDEX(ADUM(1:),'osmotic' ).NE.0 ) THEN
!          IF( INDEX(ADUM(1:),'surface').NE.0 .OR.&
!           INDEX(ADUM(1:),'tension').NE.0 ) THEN
!            ISLC(7) = 2
!            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Osmotic Pressure'
!!            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Surface Tension Effects'
!          ELSE
!            ISLC(7) = 1
!            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Osmotic Pressure Effects'
!          ENDIF
!        ELSEIF( INDEX(ADUM(1:),'surface').NE.0 .OR.&
!         INDEX(ADUM(1:),'tension').NE.0 ) THEN
!          ISLC(7) = 3
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Surface Tension Effects'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Osmotic Pressure Effects'
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Surface Tension Effects'
!        ENDIF
!
!---  Water-Air-Salt (H2O-Air-NaCl) Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'air').NE.0 .AND.&
!       (INDEX(ADUM(1:),'salt').NE.0 .OR.&
!       INDEX(ADUM(1:),'nacl').NE.0) ) THEN
!        IEQW = 1
!        IEQA = 2
!        IEQS = 3
!        NEQ = 3
!        NPH = 2
!        IGAS = 1
!        IAQU = 1
!        IOM = 12
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Air-Salt Mode (STOMP-WAS)'
!        IF( INDEX(ADUM(1:),'lfl' ).NE.0 ) THEN
!          ISLC(6) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Dissolved Salt'
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Leonard Flux Limiting Transport'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Dissolved Salt'
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Patankar Transport'
!        ENDIF
!        IF( INDEX(ADUM(1:),'osmotic' ).NE.0 ) THEN
!          IF( INDEX(ADUM(1:),'surface').NE.0 .OR.&
!           INDEX(ADUM(1:),'tension').NE.0 ) THEN
!            ISLC(7) = 2
!            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Osmotic Pressure'
!            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Surface Tension Effects'
!          ELSE
!            ISLC(7) = 1
!            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Osmotic Pressure Effects'
!          ENDIF
!        ELSEIF( INDEX(ADUM(1:),'surface').NE.0 .OR.&
!         INDEX(ADUM(1:),'tension').NE.0 ) THEN
!          ISLC(7) = 3
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Surface Tension Effects'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Osmotic Pressure Effects'
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Surface Tension Effects'
!        ENDIF
!
!---  Water-Salt (H2O-NaCl) Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       (INDEX(ADUM(1:),'salt').NE.0 .OR.&
!       INDEX(ADUM(1:),'nacl').NE.0) ) THEN
!        IEQW = 1
!        IEQS = 2
!        NEQ = 2
!        NPH = 1
!        IAQU = 1
!        IOM = 11
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Salt (STOMP-WS)'
!        IF( INDEX(ADUM(1:),'lfl' ).NE.0 ) THEN
!          ISLC(6) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Dissolved Salt'
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Leonard Flux Limiting Transport'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Dissolved Salt'
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Patankar Transport'
!        ENDIF
!        IF( INDEX(ADUM(1:),'osmotic' ).NE.0 ) THEN
!          IF( INDEX(ADUM(1:),'surface').NE.0 .OR.&
!           INDEX(ADUM(1:),'tension').NE.0 ) THEN
!            ISLC(7) = 2
!            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Osmotic Pressure and ' //&
!             'Surface Tension Effects'
!          ELSE
!            ISLC(7) = 1
!            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Osmotic Pressure Effects'
!          ENDIF
!        ELSEIF( INDEX(ADUM(1:),'surface').NE.0 .OR.&
!         INDEX(ADUM(1:),'tension').NE.0 ) THEN
!          ISLC(7) = 3
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Surface Tension Effects'
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ No Osmotic Pressure Effects' //&
!          'and No Surface Tension Effects'
!        ENDIF
!
!---  Water-Oil-Dissolved Oil-Surfactant Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'oil').NE.0 .AND.&
!       (INDEX(ADUM(1:),'dissolution').NE.0 .OR.&
!       INDEX(ADUM(1:),'dissolved oil').NE.0) .AND.&
!       INDEX(ADUM(1:),'surfactant').NE.0 ) THEN
!        IEQW = 1
!        IEQO = 2
!        IEQD = 3
!        IEQS = 4
!        NEQ = 4
!        NPH = 2
!        IAQU = 1
!        IOM = 9
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Oil Mode (STOMP-WODS)'
!        if(me.eq.0) WRITE(ISC,'(A)') '  w/ Kinetic Dissolution'
!        if(me.eq.0) WRITE(ISC,'(A)') '  w/ Surfactant Transport'
!
!---  Water-Oil-Dissolved Oil Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'oil').NE.0 .AND.&
!       (INDEX(ADUM(1:),'dissolution').NE.0 .OR.&
!       INDEX(ADUM(1:),'dissolved oil').NE.0) ) THEN
!        IEQW = 1
!        IEQO = 2
!        IEQD = 3
!        NEQ = 3
!        NPH = 2
!        IAQU = 1
!        INAPL = 1
!        IOM = 8
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Oil Mode (STOMP-WOD)'
!        if(me.eq.0) WRITE(ISC,'(A)') '  w/ Kinetic Dissolution'
!
!---  Water-Oil-Alcohol Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'oil').NE.0 .AND.&
!       INDEX(ADUM(1:),'alcohol').NE.0 ) THEN
!        IEQW = 1
!        IEQO = 2
!        IEQALC = 3
!        NEQ = 3
!        NPH = 2
!        IAQU = 1
!        INAPL = 1
!        IOM = 7
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Oil-Alcohol Mode (STOMP-WOH)'
!
!---  Water-Air-Oil-Energy Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR. &
!       INDEX(ADUM(1:),'-e ').NE.0) .AND.&
!       (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'air').NE.0 .AND.&
!       INDEX(ADUM(1:),'oil').NE.0 ) THEN
!        IEQT = 1
!        IEQW = 2
!        IEQA = 3
!        IEQO = 4
!        NEQ = 4
!        NPH = 3
!        IGAS = 1
!        IAQU = 1
!        INAPL = 1
!        IOM = 6
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Oil-Air-Energy Mode (STOMP-WOAE)'
!
!---  Water-Air-Oil Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'air').NE.0 .AND.&
!       INDEX(ADUM(1:),'oil').NE.0 ) THEN
!        IEQW = 1
!        IEQA = 2
!        IEQO = 3
!        NEQ = 3
!        NPH = 3
!        IGAS = 1
!        IAQU = 1
!        INAPL = 1
!        IOM = 5
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Oil-Air Mode (STOMP-WOA)'
!        IF( INDEX(ADUM(1:),'kinetic').NE.0 .AND.&
!         INDEX(ADUM(1:),'volatil').NE.0 ) THEN
!          ISLC(46) = 1
!          IEQDO = 4
!          NEQ = 4
!          ISVC = NEQ
!          ISVF = 2*NEQ + 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Kinetic Volatilization Option'
!        ENDIF
!
!---  Water-Oil Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'oil').NE.0 ) THEN
!        IEQW = 1
!        IEQO = 2
!        NEQ = 2
!        NPH = 2
!        IAQU = 1
!        INAPL = 1
!        IOM = 4
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Oil Mode (STOMP-WO)'
!        IF( INDEX(ADUM(1:),'partition').NE.0 .OR.&
!         INDEX(ADUM(1:),'kinetic').NE.0 ) THEN
!          IOM = 24
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Kinetic Solute Partitioning Mode'
!        ENDIF
!
!---  Water-Air-Energy (H2O-Air-E) Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR. &
!       INDEX(ADUM(1:),'-e ').NE.0) .AND.&
!       (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       INDEX(ADUM(1:),'air').NE.0 ) THEN
!        IEQT = 1
!        IEQW = 2
!        IEQA = 3
!        NEQ = 3
!        NPH = 2
!        IGAS = 1
!        IAQU = 1
!        IOM = 3
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Air-Energy Mode (STOMP-WAE)'
!        IF( INDEX(ADUM(1:),'ice' ).NE.0 )  THEN
!          ISLC(5) = 1
!          TMN = -60.0D+0
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/Ice'
!        ENDIF
!        IF( INDEX(ADUM(1:),'time-lag' ).NE.0 )  THEN
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Time-Lag for Shuttleworth-Wallace' //&
!           'Boundary Condition Scheme'
!          ISLC(24) = 10
!        ENDIF
!
!---  Water-NComponent-Energy (H2O-NComponent-E) Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR. &
!       INDEX(ADUM(1:),'-e ').NE.0) .AND.&
!       (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       (INDEX(ADUM(1:),'ncomponent').NE.0 .OR.&
!       INDEX(ADUM(1:),'n-component').NE.0 .OR.&
!       INDEX(ADUM(1:),'n component').NE.0) ) THEN
!        IEQT = 1
!        IEQW = 2
!        allocate(ieqgc(10))
!        DO 30 M = 1,LNGC
!          IEQGC(M) = M+2
!   30   CONTINUE
!        NEQ = 2+LNGC
!        NPH = 2
!        IGAS = 1
!        IAQU = 1
!        IOM = 30
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-NComponent-Energy Mode (STOMP-WNE)'
!
!---  Water-Air (H2O-Air) Operational Mode or
!     Water-Hydrogen (H2O-H) Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
!       INDEX(ADUM(1:),'h2o').NE.0) .AND.&
!       ( INDEX(ADUM(1:),'air').NE.0 .OR. &
!       INDEX(ADUM(1:),'hydrogen').NE.0 ) ) THEN
!        IEQW = 1
!        IEQA = 2
!        NEQ = 2
!        NPH = 2
!        IGAS = 1
!        IAQU = 1
!        IOM = 2
!        ISVC = NEQ
!        ISVF = 2*NEQ + 1
!        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
!         'Water-Air Mode (STOMP-WA)'
!        IF( INDEX(ADUM(1:),'invariant').NE.0 )  THEN
!          ISLC(9) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Invariant Fluid Properties'
!        ENDIF
!        IF( ( INDEX(ADUM(1:),'bubble').NE.0 .OR.&
!         INDEX(ADUM(1:),'particle').NE.0 .OR.&
!         INDEX(ADUM(1:),'displacing').NE.0 )  )  THEN
!          ISLC(13) = 1
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Particle-Displacing Bubbles'
!        ENDIF
!
!---  Water (H2O) Operational Mode  ---
!
!      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
      IF( (INDEX(ADUM(1:),'water').NE.0 .OR.&
       INDEX(ADUM(1:),'h2o').NE.0) ) THEN
        LUK = 1
        IEQW = 1
        NEQ = 1
        NPH = 1
        IAQU = 1
        IOM = 1
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
         'Water Mode (STOMP-W)'
        IF( INDEX(ADUM(1:),'5512').NE.0 ) THEN
          IOM = 5512
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ NUREG-5512 Transport'
        ENDIF
        IF( INDEX( ADUM(1:),'electrolyte' ).NE.0 ) THEN
          ISLC(16) = 1
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Electrolyte Solute Transport'
        ENDIF
!
!---  Fluid Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'fluid').NE.0 ) THEN
        LUK = 1
        IEQW = 1
        NEQ = 1
        NPH = 1
        IAQU = 1
        IOM = 1
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        ISLC(9) = 1
        if(me.eq.0) WRITE(ISC,'(A)') 'Operational Mode: ' // &
         'Water Mode (STOMP-W)'
        if(me.eq.0) WRITE(ISC,'(A)') '  w/ Constant Fluid Properties'
        IF( INDEX( ADUM(1:),'electrolyte' ).NE.0 ) THEN
          ISLC(16) = 1
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Electrolyte Solute Transport'
        ENDIF
      ELSE
        INDX = 4
        NCH = INDEX( ADUM(1:),'  ' )-1
        CHMSG = 'Unrecognized Operational Mode: ' // ADUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Transport Options  ---
!
      IF( INDEX(ADUM(1:),'transport').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'courant').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'vadose').NE.0 ) THEN
            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Vadose-Zone, Courant-Number ' //&
             'Limited Transport'
            ISLC(17) = 2
            ICRNT = 1
          ELSE
            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Courant-Number Limited Transport'
            ISLC(17) = 1
            ICRNT = 1
          ENDIF
        ENDIF
        IF( INDEX(ADUM(1:),'no aqu').NE.0 ) THEN
          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Aqueous Advective Solute Transport'
          ISLC(23) = ISLC(23)+1
        ENDIF
        IF( INDEX(ADUM(1:),'no gas').NE.0 ) THEN
          if(me.eq.0) WRITE(ISC,'(A)') '  w/o Gaseous Advective Solute Transport'
          ISLC(23) = ISLC(23)+10
        ENDIF
        IF( INDEX(ADUM(1:),'no napl').NE.0 ) THEN
          if(me.eq.0) WRITE(ISC,'(A)') '  w/o NAPL Advective Solute Transport'
          ISLC(23) = ISLC(23)+100
        ENDIF
      ENDIF
!
!---  NAPL Surface Spill Options  ---
!
!      IF( INDEX(ADUM(1:),'surface spill').NE.0 ) THEN
!        if(me.eq.0) WRITE(ISC,'(A)') '  w/ Surface Spill'
!        ISLC(49) = 1
!      ENDIF
!
!---  Vapor-Pressure-Lowering Options  ---
!
!      IF( INDEX(ADUM(1:),'no vapor').NE.0 ) THEN
!        if(me.eq.0) WRITE(ISC,'(A)') '  w/o Vapor Pressure Lowering'
!        ISLC(44) = 1
!      ENDIF
!
!---  Reactive Transport Options  ---
!
      IF( INDEX(ADUM(1:),'eckechem').NE.0 ) THEN
        if(me.eq.0) WRITE(ISC,'(A)') '  w/ ECKEChem'
        ISLC(40) = 1
        IF( INDEX(ADUM(1:),'reduced').NE.0 ) ISLC(57) = 1
        IF( INDEX(ADUM(1:),'reduced').NE.0 .AND. &
            INDEX(ADUM(1:),'kinetic').NE.0 ) ISLC(58) = 2
        IF( INDEX(ADUM(1:),'min conc').NE.0 ) ISLC(59) = 1
        IF( INDEX(ADUM(1:),'log').NE.0 ) ISLC(60) = 1
!
!---    Courant number control  ---
!
        IF( INDEX(ADUM(1:),'courant').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'vadose').NE.0 ) THEN
            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Vadose-Zone, Courant-Number ' //&  
           'Limited Transport'
            ISLC(17) = 2
            ICRNT = 1
          ELSE
            if(me.eq.0) WRITE(ISC,'(A)') '  w/ Courant-Number Limited Transport'
            ISLC(17) = 1
            ICRNT = 1
          ENDIF
        ENDIF
!
!---    Advection-diffusion transport scheme  ---
!
        IF( INDEX(ADUM(1:),'tvd').NE.0 .OR.&
         INDEX(ADUM(1:),'leonard').NE.0 ) THEN
          ISLC(1) = 1
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Leonard-TVD Solute Transport'
        ELSEIF( INDEX(ADUM(1:),'roe').NE.0 .OR.&
         INDEX(ADUM(1:),'superbee').NE.0 ) THEN
          ISLC(1) = 2
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Roe''s Superbee Solute Transport'
        ELSEIF( INDEX(ADUM(1:),'first-order').NE.0 .OR.&
         INDEX(ADUM(1:),'upwind').NE.0 ) THEN
          ISLC(1) = 3
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ First-Order Upwind' // &
          ' Solute Transport'
        ELSE
          ISLC(1) = 0
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Patankar Solute Transport'
        ENDIF
!
!---    Guess species concentrations on first call  ---
!
        IF( INDEX(ADUM(1:),'guess').NE.0 ) THEN
          ISLC(42) = 1
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Initial Species Concentration Guessing'
        ENDIF
!perko kinetics for benchmark problem in Perko et al. "decalcification of 
!cracked cement structures".  Comput Geosci (2015) 19:673-693
!        IF( INDEX(ADUM(1:),'perko').NE.0 ) THEN
!          ISLC(43) = 3
!        ENDIF 
!
!---    Porosity alteration  ---
!
!        IF( INDEX(ADUM(1:),'porosity perko').NE.0 ) THEN
!          ISLC(43) = 2 
!          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Porosity Alteration with Precipitation'
!        ELSEIF( INDEX(ADUM(1:),'porosity').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'porosity').NE.0 ) THEN
          ISLC(43) = 1
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Porosity Alteration with Precipitation'
        ENDIF
!
!---    Effective reaction area ---
!
        IF( INDEX(ADUM(1:),'area').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'constant surface').NE.0 ) THEN
          ISLC(56) = 2
            if(me.eq.0) WRITE(IWR,'(2A)') '  w/ Mineral surface ' // &
              'area held constant at initial value'
          ELSE
          ISLC(56) = 1
          if(me.eq.0) WRITE(IWR,'(A)') '  w/ Mineral effective ' // &
            'surface area scales with water saturation'
          ENDIF
        ENDIF
      ENDIF
!
!---  Solute Transport  ---
!
      IF( INDEX(ADUM(1:),'transport').NE.0 ) THEN
        NEQ = NEQ + 1
        IEQC = NEQ
        ISVT = 1
        IF( INDEX(ADUM(1:),'tvd').NE.0 .OR. &
        INDEX(ADUM(1:),'leonard').NE.0 ) THEN
          ISLC(1) = 1
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Leonard-TVD Solute Transport'
        ELSEIF( INDEX(ADUM(1:),'roe').NE.0 .OR. &
        INDEX(ADUM(1:),'superbee').NE.0 ) THEN
          ISLC(1) = 2
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Roe''s Superbee Solute Transport'
        ELSEIF( INDEX(ADUM(1:),'first-order').NE.0 .OR.&
         INDEX(ADUM(1:),'upwind').NE.0 ) THEN
          ISLC(1) = 3
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ First-Order Upwind' // &
          ' Solute Transport'
        ELSE
          ISLC(1) = 0
          if(me.eq.0) WRITE(ISC,'(A)') '  w/ Patankar Solute Transport'
        ENDIF
      ENDIF
!
!---  Maximum Courant number  ---
!
      CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Maximum Courant Number'
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CRNTMXT)
        if(me.eq.0) WRITE(ISC,'(2X,A,1PE11.4)') VARB(1:IVR),CRNTMXT
      ENDIF
!
!---  Parameter size checks  ---
!todo complete the following
!      NLT = LT
      NLL = LL
!      NLG = LG
!      NLN = LN
      NLC = LC
!      NLD = LD
!      NLS = LS
!      NALC = LALC
!      NLFC = LFW
!      IF( IEQT.GT.0 .AND. NLT.EQ.0 ) THEN
!        INDX = 5
!        CHMSG = 'Energy Equation Solved w/ Parameter LT = 0'
!        CALL WRMSGS( INDX )
      IF( IAQU.GT.0 .AND. NLL.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Aqueous Phase w/ Parameter LL = 0'
        CALL WRMSGS( INDX )
!      ELSEIF( IGAS.GT.0 .AND. NLG.EQ.0 ) THEN
!        INDX = 5
!        CHMSG = 'Gas Phase w/ Parameter LG = 0'
!        CALL WRMSGS( INDX )
!      ELSEIF( INAPL.GT.0 .AND. NLN.EQ.0 ) THEN
!        INDX = 5
!        CHMSG = 'NAPL w/ Parameter LN = 0'
!        CALL WRMSGS( INDX )
      ELSEIF( IEQC.GT.0 .AND. NLC.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Solute Equation Solved w/ Parameter LC = 0'
        CALL WRMSGS( INDX )
!      ELSEIF( IEQD.GT.0 .AND. NLD.EQ.0 ) THEN
!        INDX = 5
!        CHMSG = 'Dissolved-Oil Equation w/ Parameter LD = 0'
!        CALL WRMSGS( INDX )
!      ELSEIF( IEQALC.GT.0 .AND. NALC.EQ.0 ) THEN
!        INDX = 5
!        CHMSG = 'Alcohol Equation w/ Parameter LALC = 0'
!        CALL WRMSGS( INDX )
!      ELSEIF( IEQS.GT.0 .AND. NLS.EQ.0 ) THEN
!        INDX = 5
!        IF( IOM.EQ.11 .OR. IOM.EQ.12 .OR. IOM.EQ.13 ) THEN
!          CHMSG = 'Salt Equation Solved w/ Parameter LS = 0'
!        ELSEIF( IOM.EQ.8 .OR. IOM.EQ.9 ) THEN
!          CHMSG = 'Surfactant Equation Solved w/ Parameter LS = 0'
!        ELSE
!          CHMSG = 'Parameter LS = 0'
!        ENDIF
!        CALL WRMSGS( INDX )
!      ELSEIF( ISLC(5).GT.0 .AND. NLFC.EQ.0 ) THEN
!        INDX = 5
!        CHMSG = 'Freezing Conditions w/ Parameter LFW = 0'
!        CALL WRMSGS( INDX )
      ENDIF
      LPH = 1
      IF( NPH.GT.LPH ) THEN
        INDX = 5
        CHMSG = 'Number of Phases > Parameter LPH'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Skip time limit reads for initial conditions simulation  ---
!
      IF( IEO.EQ.3 ) THEN
        ICSN = ICSN-ICSNX
        SUBNM = SUBNM(1:ICSN)
        RETURN
!      ELSEIF( IEO.EQ.2 ) THEN
!        INDX = 1
!        CALL RDRST( INDX )
      ENDIF
!
!---  Read Execution Periods  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Execution Periods'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NEPD)
      LEPD = MAX(1,NEPD)
      IF( NEPD.LE.-3 ) THEN
        MEPD = 1
        NEPD = ABS(NEPD)
        if(me.eq.0) WRITE(ISC,'(A)') 'Cyclic Execution Periods'
!
!---    If no initial time record is read for a restart simulation,
!       then obtain initial time record from the restart file  ---
!
        VARB = 'Simulation Start Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) WRITE(ISC,'(/,2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPSX
        ELSE
          TMPSX = VAR
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) WRITE(ISC,'(/,2X,4A,1PE11.4)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',TMPSX
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,TMPSX,INDX)
        ENDIF
!
!---    Simulation stop time  ---
!
        VARB = 'Simulation Stop Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,TMPEX)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0) WRITE (ISC,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),': ', &
        TMPEX
        INDX = 0
        IUNS = 1
        CALL RDUNIT(UNTS,TMPEX,INDX)
      ELSEIF( NEPD.GE.1 ) THEN
        MEPD = 0
        if(me.eq.0) WRITE(ISC,'(A)') 'Noncyclic Execution Periods'
      ELSEIF( NEPD.EQ.0 ) THEN
        INDX = 4
        CHMSG = 'No Execution Periods'
        CALL WRMSGS( INDX )
      ELSE
        INDX = 4
        CHMSG = 'Number of Cyclic Execution Periods < 4'
        CALL WRMSGS( INDX )
      ENDIF
      if(me.eq.0) WRITE(ISC,'(2A,I6)') VARB(1:IVR),': ',NEPD
      lepd = nepd
      IF( NEPD.GT.LEPD ) THEN
        INDX = 5
        CHMSG = 'Number of Execution Periods > Parameter LEPD'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the number of execution periods
!
      if(.not.allocated(tmps)) allocate(tmps(nepd))
      if(.not.allocated(tmpe)) allocate(tmpe(nepd))
      if(.not.allocated(tmpd)) allocate(tmpd(nepd))
      if(.not.allocated(tmpx)) allocate(tmpx(nepd))
      if(.not.allocated(tmpa)) allocate(tmpa(nepd))
      if(.not.allocated(nrim)) allocate(nrim(nepd))
      if(.not.allocated(rsdm)) allocate(rsdm(nepd))
      if(.not.allocated(tmpc)) allocate(tmpc(nepd))
      if(.not.allocated(tcrntmxt)) allocate(tcrntmxt(nepd))
      tmps = 0.d0
      tmpe = 0.d0
      tmpd = 0.d0
      tmpx = 0.d0
      tmpa = 0.d0
      nrim = 0
      rsdm = 0.d0
      tmpc = 2.d-1
      tcrntmxt = -1.d0
      IF( IEO.EQ.2 ) THEN
        INDX = 1
        CALL RDRST( INDX )
      ENDIF
      DO 100 N = 1,NEPD
        if(me.eq.0) WRITE(ISC,'(/,A,I6)') '  Execution Period No. ',N
!
!---  Read Solution Time Limits  ---
!
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---  If no initial time record is read for a restart simulation,
!     then obtain initial time record from the restart file  ---
!
        VARB = 'Execution Period Start Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) WRITE(ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPS(N)
        ELSE
          TMPS(N) = VAR
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',TMPS(N)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,TMPS(N),INDX)
        ENDIF
!
!---  Maximum time  ---
!
        VARB = 'Execution Period Stop Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,TMPE(N))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0) WRITE (ISC,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),': ', &
        TMPE(N)
        INDX = 0
        IUNS = 1
        CALL RDUNIT(UNTS,TMPE(N),INDX)
!
!---  If no time step record is read for a restart simulation,
!     then obtain the initial time step from the restart file  ---
!
        VARB = 'Initial Time Step'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPD(N)
        ELSEIF( ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) &
           WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': Unspecified'
          TMPD(N) = 0.D+0
        ELSE
          TMPD(N) = VAR
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) &
           WRITE (ISC,'(2X,4A,1PE11.4)')VARB(1:IVR),', ',UNTS(1:NCH), &
          ': ',TMPD(N)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,TMPD(N),INDX)
        ENDIF
!
!---  If no maximum time step record is read for a restart simulation,
!     then obtain the maximum time step from the restart file  ---
!
!
        VARB = 'Maximum Time Step'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) &
          WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPX(N)
        ELSE
          TMPX(N) = VAR
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) &
          WRITE (ISC,'(2X,4A,1PE11.4)')VARB(1:IVR),', ',UNTS(1:NCH), &
          ': ',TMPX(N)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,TMPX(N),INDX)
        ENDIF
!
!---  If no time acceleration record is read for a restart simulation,
!     then obtain the time step acceleration from the restart file  ---
!
        VARB = 'Time Step Accleration Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          if(me.eq.0) &
         WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TMPA(N)
        ELSE
          TMPA(N) = VAR
          if(me.eq.0) &
          WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TMPA(N)
        ENDIF
!
!---  If no maximum number of Newton-Raphson iterations per time step
!     is read for a restart simulation, then obtain the information
!     from the restart file ---
!
        VARB = 'Maximum Newton Iterations per Step'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          if(me.eq.0) WRITE (ISC,'(2X,2A,I4)') VARB(1:IVR),': ',NRIM(N)
        ELSE
          NRIM(N) = IVAR
          if(me.eq.0) WRITE (ISC,'(2X,2A,I4)') VARB(1:IVR),': ',NRIM(N)
        ENDIF
!
!---  If no maximum convergence residual is read for a restart
!     simulation, then obtain the information from the restart file ---
!
        VARB = 'Maximum Convergence Residual'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          if(me.eq.0) WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',RSDM(N)
        ELSE
          RSDM(N) = VAR
          if(me.eq.0) WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',RSDM(N)
        ENDIF
!
!---  Maximum Courant number  ---
!
        if(crntmxt < 0.d0) then
!          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
!          IF( INDX.EQ.1 ) THEN
            VARB = 'Maximum Courant Number'
            IDFLT = 1
            tcrntmxt(n) = 1.d0
            CALL RDDPR(ISTART,ICOMMA,CHDUM,TCRNTMXT(N))
            tcrntmxt = dabs(tcrntmxt)
            if(me.eq.0) WRITE(ISC,'(2X,A,1PE11.4)') VARB(1:IVR),TCRNTMXT(N)
!          ENDIF
        endif
!
!---  If no time step cut factor is read for a restart simulation,
!     then obtain the time step cut factor from the restart file  ---
!
!        VARB = 'Time Step Cut Factor'
!        CALL CHKDPR(ISTART,ICOMMA,CHDUM,INDX)
!        IF( INDX.EQ.1 ) THEN
!          CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
!          IF( IEO.EQ.2.AND.N.EQ.1.AND.ICOMMA.EQ.ISTART ) THEN
!            if(me.eq.0) WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TMPC(N)
!          ELSE
!            TMPC(N) = VAR
!            if(me.eq.0) WRITE (ISC,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TMPC(N)
!          ENDIF
!        ENDIF
  100 CONTINUE
!
!---  Assign initial time values  ---
!
      IF( MEPD.EQ.1 ) THEN
        TM = TMPSX
        TMMX = TMPEX
        TMZ = MOD(TM,TMPS(NEPD))
        DO 102 N = 1,NEPD
          IF( TMPS(N).LE.TMZ .AND. TMPE(N).GT.TMZ ) THEN
            DT = TMPD(N)
            DTMX = TMPX(N)
            DTAF = TMPA(N)
            DTCF = TMPC(N)
            NRIMX = NRIM(N)
            RSDMX = RSDM(N)
            IEPD = N
          ENDIF
  102   CONTINUE
      ELSE
        DO 104 N = 1,NEPD
          IF( TMPS(N).LT.TM ) THEN
            TM = TMPS(N)
            DT = TMPD(N)
            DTMX = TMPX(N)
            DTAF = TMPA(N)
            DTCF = TMPC(N)
            NRIMX = NRIM(N)
            RSDMX = RSDM(N)
            IEPD = N
          ENDIF
          IF( TMPE(N).GT.TMMX ) THEN
            TMMX = TMPE(N)
          ENDIF
  104   CONTINUE
      ENDIF
!
!---  Check for interlocking time periods  ---
!
      DO 120 N = 1,NEPD
        DO 110 M = 1,NEPD
          IF( M.NE.N ) THEN
            IF( (TMPS(N).LT.TMPS(M) .AND. TMPE(N).LT.TMPE(N)) .OR. &
            (TMPS(N).GT.TMPS(M) .AND. TMPE(N).GT.TMPE(N)) ) THEN
              INDX = 4
              CHMSG = 'Interlocking Execution Periods'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
  110   CONTINUE
  120 CONTINUE
!
!---  Assign initial time values  ---
!
      IF( DTAF.NE.ZERO ) DT = DT/DTAF
      DTO = DT
!
!---  Read maximum execution time, maximum clock time, and
!     maximum number of time steps  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
!
!---  Count the number of commas in this line  ---
!
      NC = 0
      ISX = 1
  130 CONTINUE
      ICX = INDEX( CHDUM(ISX:),',' )
      IF( ICX.GT.0 ) THEN
        ISX = ISX+ICX
        NC = NC+1
        GOTO 130
      ENDIF
!
!---  Skip maximum cpu and maximum clock time input  ---
!
      IF( NC.EQ.1 ) GOTO 132
      VARB = 'Maximum CPU Time'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,CPUMX)
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0) WRITE (ISC,'(/,4A,1PE11.4)') VARB(1:IVR),', ', &
      UNTS(1:NCH),': ',CPUMX
      INDX = 0
      IUNS = 1
      CALL RDUNIT(UNTS,CPUMX,INDX)
!
!---  Skip maximum clock time input  ---
!
      IF( NC.EQ.2 ) GOTO 132
      VARB = 'Maximum Clock Time, '
      CALL RDDPR(ISTART,ICOMMA,CHDUM,CLKMX)
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0) WRITE (ISC,'(4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),': ',CLKMX
      INDX = 0
      IUNS = 1
      CALL RDUNIT(UNTS,CLKMX,INDX)
  132 CONTINUE
      VARB = 'Maximum Number of Time Steps'
      CALL RDINT(ISTART,ICOMMA,CHDUM,MXSTEP)
      if(me.eq.0) WRITE (ISC,'(/,2A,I6)') VARB(1:IVR),': ',MXSTEP
!
!---  Reactive transport sequence iterations  ---
!
      IF( ISLC(40).EQ.1 ) THEN
        ISVT = 1
        NRTSI = 1
        CALL CHKINT( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          VARB = 'Number of Reactive Transport Sequence Iterations'
           IDFLT = 1
           CALL RDINT(ISTART,ICOMMA,CHDUM,NRTSI)
          if(me.eq.0) WRITE (ISC,'(2A,I6)') VARB(1:IVR),': ',NRTSI
        ENDIF
      ENDIF
!
!---  Solution Control Options  ---
!
      if(me.eq.0) WRITE(ISC,'(/,A)') 'Solution Control Options'
!      IF( IOM.EQ.1 .OR. IOM.EQ.11 .OR. IOM.EQ.5512 ) GOTO 180
!
!---  Read Aqueous Diffusion Option ---
!
!      IF( IEQW.GT.0 ) THEN
!        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
!        CALL LCASE( CHDUM )
!        ISTART = 1
!        VARB = 'Aqueous Phase Diffusion Option'
!        ADUM = 'zero'
!        IDFLT = 1
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!        IF( INDEX(ADUM(1:),'zero').NE.0 ) THEN
!          ISLC(4) = 0
!          if(me.eq.0) WRITE(ISC,'(2X,2A)') VARB(1:IVR),': Zero Diffusion'
!        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
!          ISLC(4) = 1
!          if(me.eq.0) WRITE(ISC,'(2X,2A)') VARB(1:IVR),': Constant Coefficients'
!          IF( IEQA.GT.0 ) THEN
!            VARB = 'Air Diffusion Coefficient'
!            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLAC)
!            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!            if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),': ', &
!            UNTS(1:NCH),': ',DFLAC
!            INDX = 0
!            IUNM = 2
!            IUNS = -1
!            CALL RDUNIT(UNTS,DFLAC,INDX)
!          ENDIF
!          IF( IEQO.GT.0 ) THEN
!            VARB = 'Oil Diffusion Coefficient'
!            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLOC)
!            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!            if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),': ', &
!            UNTS(1:NCH),': ',DFLOC
!            INDX = 0
!            IUNM = 2
!            IUNS = -1
!            CALL RDUNIT(UNTS,DFLOC,INDX)
!          ENDIF
!          IF( IEQS.GT.0 ) THEN
!            VARB = 'Salt Diffusion Coefficient'
!            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLSC)
!            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!            if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),': ', &
!            UNTS(1:NCH),': ',DFLSC
!            INDX = 0
!            IUNM = 2
!            IUNS = -1
!            CALL RDUNIT(UNTS,DFLSC,INDX)
!          ENDIF
!        ELSEIF( INDEX(ADUM(1:),'variable').NE.0 ) THEN
!          ISLC(4) = 2
!          if(me.eq.0) WRITE(ISC,'(2X,2A)') VARB(1:IVR),': Variable Coefficients'
!        ENDIF
!
!---    Aqueous diffusion gradient option  ---
!
!        VARB = 'Aqueous Diffusion Gradient Option'
!        CALL CHKCHR(ISTART,ICOMMA,CHDUM,INDX)
!        IF( INDX.EQ.1 ) THEN
!          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!          IF( INDEX(ADUM(1:),'mol').NE.0 .AND. &
!          INDEX(ADUM(1:),'frac').NE.0 ) THEN
!            ISLC(27) = 1
!            if(me.eq.0) WRITE(ISC,'(/,A)') VARB(1:IVR),': Mole Fraction Gradient'
!          ENDIF
!        ENDIF
!        IF( IOM.EQ.3 ) ISLC(27) = 1
!      ENDIF
!
!---  Read Gas Diffusion Option ---
!
!      IF( IGAS.GT.0 ) THEN
!        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
!        CALL LCASE( CHDUM )
!        ISTART = 1
!        VARB = 'Vapor Diffusion Option'
!        ADUM = 'zero'
!        IDFLT = 1
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!        IF( INDEX(ADUM(1:),'zero').NE.0 ) THEN
!          ISLC(2) = 0
!          if(me.eq.0) WRITE(ISC,'(2X,2A)') VARB(1:IVR),' : Zero Diffusion'
!        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
!          ISLC(2) = 1
!          if(me.eq.0) WRITE(ISC,'(2X,2A)') VARB(1:IVR),' : Constant Coefficients'
!          IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
!            IF( ISLC(45).EQ.0 ) THEN
!              VARB = 'CO2 Vapor Diffusion Coefficient'
!              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGAC)
!              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!              if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),', ', &
!              UNTS(1:NCH),': ',DFGAC
!              INDX = 0
!              IUNM = 2
!              IUNS = -1
!              CALL RDUNIT(UNTS,DFGAC,INDX)
!            ENDIF
!            VARB = 'CH4 Vapor Diffusion Coefficient'
!            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGOC)
!            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!            if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),', ', &
!            UNTS(1:NCH),': ',DFGOC
!            INDX = 0
!            IUNM = 2
!            IUNS = -1
!             CALL RDUNIT(UNTS,DFGOC,INDX)
!          ELSE
!            IF( IEQW.GT.0 ) THEN
!              VARB = 'Water Vapor Diffusion Coefficient'
!              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGWC)
!              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!              if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),', ', &
!              UNTS(1:NCH),': ',DFGWC
!              INDX = 0
!              IUNM = 2
!              IUNS = -1
!              CALL RDUNIT(UNTS,DFGWC,INDX)
!            ENDIF
!            IF( IEQO.GT.0 ) THEN
!              VARB = 'Oil Vapor Diffusion Coefficient'
!              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGOC)
!              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!              if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),', ', &
!              UNTS(1:NCH),': ',DFGOC
!              INDX = 0
!              IUNM = 2
!              IUNS = -1
!              CALL RDUNIT(UNTS,DFGOC,INDX)
!            ENDIF
!          ENDIF
!        ELSEIF( INDEX(ADUM(1:),'variable').NE.0 ) THEN
!          ISLC(2) = 2
!          if(me.eq.0) WRITE(ISC,'(2X,2A)') VARB(1:IVR),': Variable Coefficients'
!        ELSEIF( INDEX(ADUM(1:),'enhanced').NE.0 ) THEN
!          ISLC(2) = 3
!          if(me.eq.0) WRITE(ISC,'(2X,2A)') VARB(1:IVR),': Enhanced Vapor ' // &
!          'Diffusion Coefficients'
!          IF( IEQW.GT.0 ) THEN
!            VARB = 'Clay Mass Fraction'
!            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGWC)
!            if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',DFGWC
!          ENDIF
!        ENDIF
!
!---  Particle-displacing bubble to gas-phase effective air mass
!     transfer coefficient for air  ---
!
!        IF( ISLC(13).EQ.1 ) THEN
!          VARB = 'Bubble to Gas Effective Air Mass ' // &
!           'Transfer Coefficient'
!          CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGOC)
!          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!          if(me.eq.0) WRITE(ISC,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH), &
!          ': ',DFGOC
!          INDX = 0
!          IUNM = 2
!          IUNS = -1
!          CALL RDUNIT(UNTS,DFGOC,INDX)
!        ENDIF
!
!---    Gas diffusion gradient option  ---
!
!        VARB = 'Gas Diffusion Gradient Option'
!        CALL CHKCHR(ISTART,ICOMMA,CHDUM,INDX)
!        IF( INDX.EQ.1 ) THEN
!          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!          IF( INDEX(ADUM(1:),'mol').NE.0 .AND. &
!          INDEX(ADUM(1:),'frac').NE.0 ) THEN
!            ISLC(28) = 1
!            if(me.eq.0) WRITE(ISC,'(/,A)') VARB(1:IVR),': Mole Fraction Gradient'
!          ENDIF
!        ENDIF
!        IF( IOM.EQ.3 ) ISLC(28) = 1
!      ENDIF
!
!---  Read Hydrate Dissociation Option  ---
!
!      IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
!        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
!        CALL LCASE( CHDUM )
!        ISTART = 1
!        VARB = 'Hydrate Dissociation Option'
!        ADUM = 'kinetic'
!        IDFLT = 1
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!        IF( INDEX(ADUM(1:),'kinetic').NE.0 ) THEN
!          if(me.eq.0) WRITE(ISC,'(2X,2A)') VARB(1:IVR),': Kinetic'
!          ISLC(31) = 1
!          VARB = 'Hydrate Formation-Dissociation Rate Constant'
!          IDFLT = 1
!          CHFDR = 1.925D-5
!          CALL RDDPR(ISTART,ICOMMA,CHDUM,CHFDR)
!          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!          if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
!          ': ',CHFDR
!          INDX = 0
!          IUNS = -1
!          CALL RDUNIT(UNTS,CHFDR,INDX)
!          if(me.eq.0) WRITE(ISC,'(A,1PE11.4,A)') ' (',CHFDR,', 1/s)'
!          CHFDR = MAX( CHFDR,EPSL )
!        ELSE
!          if(me.eq.0) WRITE(ISC,'(2X,A,1X,A)') VARB(1:IVR),': Equilibrium'
!        ENDIF
!      ENDIF  
!
!---  Read Hydrate Molecular Exchange Rate Constant  ---
!
!      IF( IOM.EQ.39 ) THEN
!        CHMER = 1.D+20
!        VARB = 'Hydrate Molecular Exchange Rate Constant (Half Life)'
!        IDFLT = 1
!        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHMER)
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!        if(me.eq.0) WRITE(ISC,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
!        ': ',CHMER
!        INDX = 0
!        IUNS = 1
!        CALL RDUNIT(UNTS,CHMER,INDX)
!        if(me.eq.0) WRITE(ISC,'(A,1PE11.4,A)') ' (',CHMER,', s)'
!        CHMER = MAX( CHMER,EPSL )
!      ENDIF
!  180 CONTINUE
!
!---  Read fluid density and viscosity  ---
!
      IF( ISLC(9).EQ.1 ) THEN
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        IDFLT = 1
        VARB = 'Fluid Density'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RHOLI)
        IDFLT = 1
        UNTS = 'kg/m^3'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',RHOLI
        INDX = 0
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNTS,RHOLI,INDX)
        if(me.eq.0) WRITE(ISC,'(A,1PE11.4,A)') ' (',RHOLI,', kg/m^3)'
        VARB = 'Fluid Viscosity'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VISLI)
        IDFLT = 1
        UNTS = 'Pa s'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',VISLI
        INDX = 0
        IUNM = -1
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNTS,VISLI,INDX)
        if(me.eq.0) WRITE(ISC,'(A,1PE11.4,A)') ' (',VISLI,', Pa s)'
      ENDIF
      IF( ISLC(2).GE.1 .OR. ISLC(4).GE.1 &
      .OR. IEQC.GT.0 .OR. IEQS.GT.0 .OR.  &
      IEQD.GT.0 .OR. ISLC(40).GE.1 ) THEN
        ISLC(3) = 1
      ELSE
        ISLC(3) = 0
      ENDIF
!
!---  Read electrolyte functions  ---
!
      IF( ISLC(16).EQ.1 ) THEN
        if(.not.allocated(elc_dcf)) allocate(elc_dcf(4))
        if(.not.allocated(elc_vcf)) allocate(elc_vcf(4))
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        IDFLT = 1
        VARB = 'Electrolyte Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ELC_SOL)
        if(me.eq.0) WRITE(ISC,'(/,2X,3A)') VARB(1:IVR),': ',ELC_SOL(1:NCH)
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Electrolyte Density Function Option'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX( ADUM(1:),'leijnse' ).NE.0 ) THEN
          if(me.eq.0) WRITE(ISC,'(A)') 'Leijnse Mass-Fraction Exponential Function'
          VARB = 'Leijnse Exponential Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(1))
          IDF_ELC = 1
        ELSEIF( INDEX( ADUM(1:),'fourth' ).NE.0 ) THEN
          VARB = 'Electrolyte Density Function Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = -3
          ELC_DUN = 1.D+0
          CALL RDUNIT(UNTS,ELC_DUN,INDX)
          if(me.eq.0) WRITE(ISC,'(A)') 'Fourth-Order Polynomial'
          VARB = 'Polynomial "a" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(1))
          VARB = 'Polynomial "b" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(2))
          VARB = 'Polynomial "c" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(3))
          VARB = 'Polynomial "d" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(4))
          IDF_ELC = 2
        ELSE
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Unrecognized Electrolyte Density Option: ' //&
           ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Electrolyte Viscosity Function Option'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX( ADUM(1:),'leijnse' ).NE.0 ) THEN
          if(me.eq.0) WRITE(ISC,'(A)') 'Leijnse Mass-Fraction Empirical Function'
          VARB = 'Electrolyte Viscosity Function Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = -3
          ELC_VUN = 1.D+0
          CALL RDUNIT(UNTS,ELC_VUN,INDX)
          VARB = 'Leijnse "a" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(1))
          VARB = 'Leijnse "b" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(2))
          VARB = 'Leijnse "c" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(3))
          VARB = 'Leijnse "d" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(4))
          IVF_ELC = 1
        ELSEIF( INDEX( ADUM(1:),'fourth' ).NE.0 ) THEN
          VARB = 'Electrolyte Viscosity Function Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = -3
          ELC_VUN = 1.D+0
          CALL RDUNIT(UNTS,ELC_VUN,INDX)
          if(me.eq.0) WRITE(ISC,'(A)') 'Fourth-Order Polynomial'
          VARB = 'Polynomial "a" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(1))
          VARB = 'Polynomial "b" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(2))
          VARB = 'Polynomial "c" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(3))
          VARB = 'Polynomial "d" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(4))
          IVF_ELC = 2
        ELSE
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Unrecognized Electrolyte Viscosity Option: ' // &
          ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Read Interfacial Average Options ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      if(me.eq.0) WRITE(ISC,'(/,A)') 'Interfacial Averaging Schemes:'
      VARB = 'Number of Interfacial Average Lines'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      DO 300 N = 1,NLIN
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Interfacial Average Variable Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        DO 210 M = 1,LCV
          IF( INDEX(ADUM(1:),CHIFV(M)(1:10)).NE.0 ) GOTO 220
  210   CONTINUE
        INDX = 4
        NCH = INDEX( ADUM(1:),'  ' )-1
        CHMSG = 'Unrecognized Interfacial Avg. Variable: '//ADUM(1:NCH)
        CALL WRMSGS( INDX )
  220   CONTINUE
        VARB = 'Interfacial Averaging Scheme'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        DO 230 MM = LCS,1,-1
          IF( INDEX(ADUM(1:),CHIFS(MM)(1:NCS(MM))).NE.0 ) GOTO 240
  230   CONTINUE
        INDX = 4
        NCH = INDEX( ADUM(1:),'  ' )-1
        CHMSG = 'Unrecognized Interfacial Avg. Scheme: '//ADUM(1:NCH)
        CALL WRMSGS( INDX )
  240   CONTINUE
        IDMN(M) = MM
        IF( MM.EQ.5 .OR. MM.EQ.7 ) THEN
          VARB = 'Weighting Factor'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,WFMN(M))
          WFMN(M) = MAX( -1.D+0,WFMN(M) )
          WFMN(M) = MIN( 1.D+0,WFMN(M) )
        ENDIF
  300 CONTINUE
      DO 310 N = 1,LCV
        IF( IDMN(N).EQ.5 .OR. IDMN(N).EQ.7 ) THEN
          if(me.eq.0) WRITE(ISC,'(2X,4A,1PE11.4)') CHIFV(N)(1:NCV(N)),': ', &
         CHIFS(IDMN(N))(1:NCS(IDMN(N))),', Weighting Factor: ',WFMN(N)
        ELSE
          if(me.eq.0) WRITE(ISC,'(2X,3A)') CHIFV(N)(1:NCV(N)),': ', &
          CHIFS(IDMN(N))(1:NCS(IDMN(N)))
        ENDIF
  310 CONTINUE
!
! --- Read minimum concentration for reactive chemistry in eckechem
!
      CMIN = 1.D-30
!      if(islc(43) >= 2) cmin=1.d-20
      IF( ISLC(59).EQ.1 )THEN
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Minimum Aqueous Concentration in Eckechem'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CMIN)
        CMIN = MAX(CMIN,1.D-110)
        if(me.eq.0)WRITE(ISC,'(A,A,1PE11.4,A)') VARB(1:IVR),': ',CMIN
      ENDIF
      IF( IOM.NE.1 ) THEN
        INDX = 18
        CHMSG = 'Incompatible Operational Mode'
        CALL WRMSGS( INDX )
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDSOLU group  ---
!
      RETURN
      END
