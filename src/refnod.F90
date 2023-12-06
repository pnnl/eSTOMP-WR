!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE REFNOD
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
!     Prints convergence and variable information to the screen and
!     output file.
!
!   1	aqueous pressure ' PL '
!   2	gas pressure ' PG '
!   3	NAPL pressure ' PN '
!   4	temperature ' T  '
!   5	phase condition 'PHCN'
!   6	aqueous gauge pressure 'GPL '
!   7	gas gauge pressure 'GPG '
!   8	NAPL gauge pressure 'GPN '
!   9	apparent aqueous saturation 'ASL '
!   10	apparent total-liquid saturation 'AST '
!   11	aqueous saturation ' SL '
!   12	gas saturation ' SG '
!   13	NAPL saturation ' SN '
!   14	total-liquid saturation ' ST '
!   15	aqueous moisture content 'MCL '
!   16	NAPL moisture content 'MCN '
!   17	total-liquid moisture content 'MCT '
!   18	effective trapped NAPL 'ESNT'
!   19	effective trapped gas 'ESGT'
!   20	diffusive porosity 'PORD'
!   21	gas water mass fraction 'XGW '
!   22	gas air mass fraction 'XGA '
!   23	gas oil mass fraction 'XGO '
!   24	aqueous water mass fraction 'XLW '
!   25	aqueous air mass fraction 'XLA '
!   26	aqueous oil mass fraction 'XLO '
!   27	aqueous hydraulic head 'HHL '
!   28	gas hydraulic head 'HHG '
!   29	NAPL hydraulic head 'HHN '
!   30	rock/soil type 'RSZN'
!   31	aqueous relative permeability 'RKL '
!   32	gas relative permeability 'RKG '
!   33	NAPL relative permeability 'RKN '
!   34	aqueous density 'RHOL'
!   35	gas density 'RHOG'
!   36	NAPL density 'RHON'
!   37	total water mass 'TMW '
!   38	total air mass 'TMA '
!   39	total oil mass 'TMO '
!   40	water mass source integral 'SRIW'
!   41	air mass source integral 'SRIA'
!   42	oil mass source integral 'SRIO'
!   43	energy source integral 'SRIT'
!   44	x thermal conductivity 'THKX'
!   45	y thermal conductivity 'THKY'
!   46	z thermal conductivity 'THKZ'
!   47	salt volumetric concentration ' CS '
!   48	salt aqueous concentration 'CSL '
!   49	aqueous courant 'CRNL'
!   50	total salt mass 'TMS '
!   51	x aqueous volumetric flux ' UL '
!   52	y aqueous volumetric flux ' VL '
!   53	z aqueous volumetric flux ' WL '
!   54	x gas volumetric flux ' UG '
!   55	y gas volumetric flux ' VG '
!   56	z gas volumetric flux ' WG '
!   57	x NAPL volumetric flux ' UN '
!   58	y NAPL volumetric flux ' VN '
!   59	z NAPL volumetric flux ' WN '
!   60	x heat flux ' UQ '
!   61	y heat flux ' VQ '
!   62	z heat flux ' WQ '
!   63	matric potential head 'MPH '
!   64	x salt flux ' US '
!   65	y salt flux ' VS '
!   66	z salt flux ' WS '
!   67	xnc salt flux 'USNC'
!   68	ync salt flux 'VSNC'
!   69	znc salt flux 'WSNC'
!   70	gas water mole fraction 'XMGW'
!   71	gas air mole fraction 'XMGA'
!   72	gas oil mole fraction 'XMGO'
!   73	gas water concentration 'CGW '
!   74	gas air concentration 'CGA '
!   75	gas oil concentration 'CGO '
!   76	aqueous water concentration 'CLW '
!   77	aqueous air concentration 'CLA '
!   78	aqueous oil concentration 'CLO '
!   79	gas courant 'CRNG'
!   80	ice pressure 'PI '
!   81	ice saturation 'SI '
!   82	ice density 'RHOF'
!   83	aqueous matrix 'DSLM'
!   84	aqueous fracture 'DSLF'
!   85	gas matrix 'DSGM'
!   86	gas fracture 'DSGF'
!   87	xnc aqueous volumetric flux 'ULNC'
!   88	ync aqueous volumetric flux 'VLNC'
!   89	znc aqueous volumetric flux 'WLNC'
!   90	xnc gas volumetric flux 'UGNC'
!   91	ync gas volumetric flux 'VGNC'
!   92	znc gas volumetric flux 'WGNC'
!   93	xnc NAPL volumetric flux 'UNNC'
!   94	ync NAPL volumetric flux 'VNNC'
!   95	znc NAPL volumetric flux 'WNNC'
!   96	xnc heat flux 'UQNC'
!   97	ync heat flux 'VQNC'
!   98	znc heat flux 'WQNC'
!   99	NAPL courant 'CRNN'
!   101	osmotic pressure 'POSM'
!   102	osmotic efficiency factor 'OEC '
!   103	aqueous alcohol concentration 'CLA '
!   104	NAPL alcohol concentration 'CNA '
!   105	trapped gas saturation 'SGT '
!   106	trapped NAPL saturation 'SNT '
!   107	aqueous trapped gas 'SGTL'
!   108	NAPL trapped gas 'SGTN'
!   109	dissolved-aqueous oil concentration 'CLO '
!   110	salt aqueous mass fraction 'XLS '
!   111	surfactant volumetric concentration 'CS '
!   112	surfactant aqueous concentration 'CLS '
!   113	surfactant aqueous mass fraction 'XLS '
!   114	x surfactant flux ' US '
!   115	y surfactant flux ' VS '
!   116	z surfactant flux ' WS '
!   117	xnc surfactant flux 'USNC'
!   118	ync surfactant flux 'VSNC'
!   119	znc surfactant flux 'WSNC'
!   120	x dissolved-oil flux 'ULO '
!   121	y dissolved-oil flux 'VLO '
!   122	z dissolved-oil flux 'WLO '
!   123	xnc dissolved-oil flux 'ULOC'
!   124	ync dissolved-oil flux 'VLOC'
!   125	znc dissolved-oil flux 'WLOC'
!   126	NAPL-aqueous trapping number 'TPNL'
!   127	minimum effect aqueous saturation 'ESLM'
!   128	water vapor partial pressure 'PVW '
!   129	air partial pressure 'PVA '
!   130	oil vapor partial pressure 'PVO '
!   131	gas-aqueous scaling factor 'BGL '
!   132	free-NAPL aqueous interfacial area 'ANFL'
!   133	trapped-NAPL aqueous interfacial area 'ANTL'
!   134	aqueous solute coefficient 'HKL '
!   135	free-NAPL solute coefficient 'HKNF'
!   136	trapped-NAPL solute coefficient 'HKNT'
!   137	undefined
!   138	undefined
!   139	undefined
!   140	water mass source rate 'SRCW'
!   141	air mass source rate 'SRCA'
!   142	oil mass source rate 'SRCO'
!   143	energy source rate 'SRCQ'
!   144	aqueous well depth 'PLWB'
!   145	well flow rate 'QLW '
!   146	well flow integral 'QLWI'
!   147	salt mass source rate 'SRCS'
!   148	salt mass source integral 'SRIS'
!   149	scanning path 'PATH'
!   150	aqueous air or co2 saturation 'DAPS'
!   151	bubble void fraction 'BVF '
!   152	bubble air mass fraction 'XBA '
!   153	mineralized co2 'MCO2 '
!   154	napl well flow rate 'QNW '
!   155	napl well flow integral 'QNWI'
!   156	total well flow rate 'QTW '
!   157	total well flow integral 'QTWI'
!   158	undefined
!   159	undefined
!   160	undefined
!   161	NAPL dissolved water concentration 'CNW '
!   162	NAPL dissolved water mole fraction 'XMNW'
!   163	NAPL dissolved water mass fraction 'XNW '
!   164	NAPL dissolved oil concentration 'CNO '
!   165	NAPL dissolved oil mole fraction 'XMNO'
!   166	NAPL dissolved oil mass fraction 'XNO '
!   167	total alcohol mass 'TMA '
!   168	aqueous dissolved water mass fraction 'XLW '
!   169	alcohol mass source integral 'SRIA'
!   170	alcohol mass source rate 'SRCA'
!   171	integrated NAPL and aqueous dissolved alcohol 'IMA '
!   172	integrated aqueous dissolved alcohol 'IMLA'
!   173	integrated NAPL dissolved water 'IMNW'
!   174	integrated NAPL dissolved oil 'IMNO'
!   175	integrated NAPL dissolved alcohol 'IMNA'
!   176	aqueous viscosity 'VISL'
!   177	monitoring well water depth or total-liquid well depth 'WDT '
!   178	aqueous well depth 'WDL '
!   179	NAPL well depth 'WDN '
!   180	monitoring well pressure ' PW '
!   181	monitoring well aqueous saturation 'SLW '
!   182	monitoring well water-vapor mass fraction or well NAPL saturation 'XGWW'
!   183	monitoring well dissolved-air mass fraction or dissolved-oil mass fraction 'XLAW'
!   184	monitoring well axial aqueous flux or well total-liquid pumping rate 'UL_W'
!   185	monitoring well axial gas flux or well aqueous pumping rate 'UG_W'
!   186	monitoring well vertical aqueous flux or well NAPL pumping rate 'WL_W'
!   187	monitoring well vertical gas flux or well total-liquid pumping integral 'WG_W'
!   188	integrated well aqueous pumping 'IPLW'
!   189	integrated mineral CO2 or well NAPL pumping integral 'IPNW'
!   190	integrated trapped gas air 'IMGT'
!   191	integrated water mass 'IMW '
!   192	integrated air mass 'IMA '
!   193	integrated oil mass 'IMO '
!   194	integrated aqueous water 'IMLW'
!   195	integrated aqueous air 'IMLA'
!   196	integrated aqueous oil 'IMLO'
!   197	integrated gas water 'IMGW'
!   198	integrated gas air 'IMGA'
!   199	integrated gas oil 'IMGO'
!   200	reserved to control plot file output
!   201	x aqueous relative permeability 'RKLX'
!   202	y aqueous relative permeability 'RKLY'
!   203	z aqueous relative permeability 'RKLZ'
!   204	aqueous co2 mole fraction 'XMLA'
!   205 aqueous salt mole fraction 'XMLS'
!   206 atmospheric temperature ' TA '
!   207 atmospheric relative humidity ' RH '
!   208 atmospheric solar radiation ' RN '
!   209 atmospheric wind speed ' WS '
!   210 residual NAPL saturation 'SNR '
!   211 mobile NAPL saturation 'SNM '
!   212 free NAPL saturation 'SNF '
!   213 surface temperature 'T_S'
!   214 surface vapor pressure 'PV_S'
!   215 actual evaporation rate 'E_SA'
!   216 potential evaporation rate 'PE_SA'
!   217 actual transpiration rate 'T_SA'
!   218 potential transpiration rate 'PT_SA'
!   219 saturated co2 aqueous mass fraction 'SXLA'
!   220 aqueous alcohol mole fraction 'XMLA'
!   221 NAPL alcohol mode fraction 'XMNA'
!   222 aqueous alcohol mass fraction 'XLA '
!   223 NAPL alcohol mass fraction 'XNA '
!   224 atmospheric pressure, ' PA '
!   225 surface aqueous pressure, 'PL_S'
!   226 surface gas pressure, 'PG_S'
!   227 surface aqueous saturation, 'SL_S
!   228 surface latent heat flux, 'QL_S'
!   229 surface sensible heat flux, 'QH_S'
!   230 surface net long-wave radiation, 'RL_S'
!   231 surface net short-wave radiation, 'RS_S'
!   232 surface ground heat flux, 'QG_S'
!   233 surface water mass balance kg/s, 'WB_S'
!   234 plant temperature, 'T_P' or 'TPXX'
!   235 plant temperature, 'TPXX'
!   236 plant temperature, 'TPXX'
!   237 plant temperature, 'TPXX'
!   238 plant temperature, 'TPXX'
!   239 rainfall interception mass, 'RFIM'
!   240 sorbed oil mass, kg oil, 'TSO '
!   241 sorbed oil mass fraction kg oil/kg soil, 'XSO '
!   242 sorbed oil volumetric concentration kg oil/m^3, 'CSO '
!   243 bare-soil aerodynamic resistance s/m, 'RABS'
!   244 surface volumetric precipitation rate m^3/s, 'PV_S'
!   245 surface mass precipitation rate kg/s, 'PM_S'
!   246 atmospheric precipitation rate kg/s, 'PM_A'
!   247 x-direction intrinsic permeability m^2, ' UK '
!   248 y-direction intrinsic permeability m^2, ' VK '
!   249 z-direction intrinsic permeability m^2, ' WK '
!   250 hydrate water mass fraction, 'XHW '
!   251 hydrate CO2	mass fraction, 'XHA '
!   252 hydrate CH4 mass fraction, 'XHO '
!   253 hydrate density kg/m^3, 'RHOH'
!   254 hydrate saturation, ' SH '
!   255 hydrate pressure Pa, ' PH '
!   256	integrated hydrate water mass 'IMHW'
!   257	integrated hydrate CO2 mass 'IMHA'
!   258	integrated hydrate CH4 mass 'IMHO'
!   259	integrated aqueous CH4 mass 'IMLO'
!   260	integrated gas CH4 mass 'IMGO'
!   261	integrated water or H2O mass source 'IMSW'
!   262	integrated air or CO2 mass source 'IMSA'
!   263	integrated oil or CH4 mass source 'IMSO'
!   264 precipitated salt saturation ' SS '
!   265 hydrate water mole fraction, 'XMHW'
!   266 hydrate CO2	mole fraction, 'XMHA'
!   267 hydrate CH4 mole fraction, 'XMHO'
!   268 plant stomatal resistance s/m, 'RS_P' or 'RSPX'
!   269 plant stomatal resistance s/m, 'RSPX'
!   270 plant stomatal resistance s/m, 'RSPX'
!   271 plant stomatal resistance s/m, 'RSPX'
!   272 plant stomatal resistance s/m, 'RSPX'
!   273 fracture adjacent index, 'FAI '
!   274 rain-water runoff volumetric rate m^3/s, 'RWRO'
!   275 well-node top pressure Pa, 'WNTP'
!   276 well-node bottom pressure Pa, 'WNBP'
!   277	differential integrated hydrate water mass 'DMHW'
!   278	differential integrated hydrate CO2 mass 'DMHA'
!   279	differential integrated hydrate CH4 mass 'DMHO'
!   280	differential integrated aqueous CH4 mass 'DMLO'
!   281	differential integrated gas CH4 mass 'DMGO'
!   282	differential integrated water or H2O mass 'DMW'
!   283	differential integrated air or CO2 mass 'DMA'
!   284	differential integrated oil or CH4 mass 'DMO'
!   285 source-well pressure Pa, 'SWP'
!   286 source-well temperature C, 'SWT'
!   287 aqueous spill head/height m, 'HLSP'
!   288 NAPL spill head/height m, 'HNSP'
!   289 Evapotranspiration 'ET'
!   401	solute volumetric concentration 'C   '
!   402	solute aqueous concentration 'CL  '
!   403	solute gas concentration 'CG  '
!   404	solute NAPL concentration 'CN  '
!   405	solute aqueous mole fraction 'YL  '
!   406	solute gas mole fraction 'YG  '
!   407	solute NAPL mole fraction 'YN  '
!   408	x solute flux 'UC  '
!   409	y solute flux 'VC  '
!   410	z solute flux 'WC  '
!   411	solute source 'SRC '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, February, 1993.
!     Last Modified by WE Nichols, Battelle, PNNL, March 22, 2001.
!     Last Modified by MD White, PNNL, 1 August 2002.
!     $Id: refnod.F,v 1.37 2008/02/13 01:05:53 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE REACT
      USE OUTPU
      USE GRID
      USE FILES
      use fdvp
      use grid_mod
      USE COUP_WELL
      use plt_atm
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





      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  VSKP
      INTEGER, DIMENSION(:), ALLOCATABLE ::  JSKP

      CHARACTER*7  FORM1
      CHARACTER*23 FORM2
      CHARACTER*9  FORM3
      CHARACTER*12 FORM4
      CHARACTER*25 FORM5
      CHARACTER*14 FORM6
      CHARACTER*16 FORM7
      CHARACTER*14 FORM8
      CHARACTER*5  FORM9
      CHARACTER*28 FORM10
      CHARACTER*43 FORM11
      CHARACTER*39 FORM12
      CHARACTER*49 FORM13
      CHARACTER*12 FORM14
      CHARACTER*40 FORM15
      CHARACTER*14 FORM16
      CHARACTER*4  FORM17
      CHARACTER*14 FORM18
      CHARACTER*5  FORM19
      CHARACTER*10 FORM23
      CHARACTER*64 SPACES
      CHARACTER*64 TXTX,STRX
      CHARACTER*4096 N_IWR,N_ISC
      LOGICAL :: use_ga  
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2,FORM3,FORM4,FORM5,FORM6,FORM7,FORM8,FORM9
      SAVE FORM10,FORM11,FORM12,FORM13,FORM14,FORM15,FORM16,FORM17
      SAVE FORM18,FORM19,FORM23,SPACES
      SAVE DMHWX,DMHAX,DMHOX,DMLOX,DMGOX,DMWX,DMAX,DMOX
      DATA FORM1 /'(/,A,$)'/
      DATA FORM2 /'(3X,A,3X,A,5X,A,7X,A,$)'/
      DATA FORM3 /'(3X,2A,$)'/
      DATA FORM4 /'(4X,A4,A,$)'/
!      DATA FORM5 /'(A,I3,A,I3,A,I3,A,I8,A,$)'/
      DATA FORM5 /'(A,I8,A,$)'/
      DATA FORM6 /'(1X,1PE10.3,$)'/
      DATA FORM7 /'(17X,3A,5X,4A,$)'/
      DATA FORM8 /'(1X,A,A7,2A,$)'/
      DATA FORM9 /'(A,$)'/
      DATA FORM10 /'(3X,A,3X,A,5X,A,7X,A,3X,A,$)'/
      DATA FORM11 /'(1X,I6,1X,I8,1X,1PE12.5,1X,1PE12.5,1X,I2,$)'/
      DATA FORM12 /'(1X,I6,1X,I8,1X,1PE12.5,1X,1PE12.5,A,$)'/
      DATA FORM13 /'(1X,I6,1X,I8,1X,1PE12.5,1X,1PE12.5,1X,I2,1X,I6,$)'/
      DATA FORM14 /'(4X,A4,A,$)'/
      DATA FORM15 /'( 3X,A,3X,A,5X,A,7X,A,3X,A,1X,A,$)'/
      DATA FORM16 /'(1X,1PE10.3,$)'/
      DATA FORM17 /'(1X)'/
      DATA FORM18 /'(1X,A,A7,2A,$)'/
      DATA FORM19 /'(A,$)'/
      DATA FORM23 /'(10X,2A,$)'/
      DATA SPACES /'                                                 '/
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/REFNOD'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(169)(1:1),'$').EQ.0 ) CVS_ID(169) = &
        '$Id: refnod.F,v 1.37 2008/02/13 01:05:53 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      EPSL = 1.D-14
!     if(me.eq.0) then
      ALLOCATE( VSKP(1:(LOUPV-33*(LSOLU))),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VSKP'
        CALL WRMSGS( INDX )
      ENDIF
      VSKP = 0.D+0
      ALLOCATE( JSKP(1:(LOUPV-33*(LSOLU))),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JSKP'
        CALL WRMSGS( INDX )
      ENDIF
      JSKP = 0

      DO 2 N = 1,400
        JSKP(N) = 0
    2 CONTINUE
!
!---  Compute format spacings according to the number of significant
!     digits requested  ---
!
      IVAR = (ISGNS+3)/2
      WRITE(FORM4(2:2),'(I1)') MAX( 4,ISGNS+3-IVAR )
      WRITE(FORM6(8:9),'(I2)') MAX( 10,ISGNS+6 )
      WRITE(FORM6(11:11),'(I1)') MIN( 9,ISGNS-1 )
      IVAR = (ISGNS-2)/2
      WRITE(FORM8(2:2),'(I1)') MAX( 1,IVAR )
!      WRITE(FORM9(2:3),'(I2)') MAX( 11,ISGNS+7 )
      IVAR = (ISGNO+3)/2
      WRITE(FORM14(2:2),'(I1)') MAX( 4,ISGNO+3-IVAR )
      WRITE(FORM16(8:9),'(I2)') MAX( 10,ISGNO+6 )
      WRITE(FORM16(11:11),'(I1)') MIN( 9,ISGNO-1 )
      IVAR = (ISGNO-2)/2
      WRITE(FORM18(2:2),'(I1)') MAX( 1,ISGNO-2-IVAR )
!      WRITE(FORM19(2:3),'(I2)') MAX( 11,ISGNO+7 )
!
!---  Write main header line  ---
!
      IF( ICNO.EQ.-1 ) THEN
        if(me.eq.0) write(iwr,'(//,A)') ' ---  Reference Node Output Record  ---'
        ICNO = 10
      ENDIF
      IF( ICNS.EQ.-1 ) THEN
        if(me.eq.0) write(isc,'(//,A)') ' ---  Reference Node Output Record  ---'
        ICNS = 10
      ENDIF
!
!---  Write subheader lines the output file ---
!

      IF( ICNO.EQ.10 ) THEN
        ICNO = 0
        if(me.eq.0) write(iwr,FORM1) 'Reference Node(s)'
        DO 10 M = 1,NREF
          n = ndref(m)
          nx = 0
          if(n.ne.0)Nx = loc2nat(n)
          call ga_igop(1,nx,1,'max')
!          I = ID(N)
!          J = JD(N)
!          K = KD(N)
          if(me.eq.0) write(iwr,FORM5) ' (',Nx,')'
   10   CONTINUE
        if(me.eq.0) write(iwr,FORM17)
        IN_IWR = 0
        IF( ISLC(48).EQ.1 ) THEN
          if(me.eq.0) write(iwr,FORM15) 'Step','Node','Time','Timestep','Itr', &
          'Active'
          INX = 1
          WRITE(N_IWR(INX:INX+49),'(A)') SPACES(1:50)
          INX = INX+50
        ELSEIF( ISLC(15).EQ.1 ) THEN
          if(me.eq.0) write(iwr,FORM2) 'Step','Node','Time','Timestep'
          INX = 1
          WRITE(N_IWR(INX:INX+37),'(A)') SPACES(1:38)
          INX = INX+38
          IX = (LGRL-1)*3/2
          JX = (LGRL-1)*3-IX+1
          IX = IX+4
          IC = ICOUNT(IX)
          IF( IC.EQ.1 ) THEN
            WRITE(FORM3(2:2),'(I1)') IX
            if(me.eq.0) write(iwr,FORM3) 'Itr',SPACES(1:JX)
            WRITE(N_IWR(INX:INX+JX+2),'(A)') SPACES(1:JX+3)
            INX = INX+JX+3
          ELSEIF( IC.EQ.2 ) THEN
            WRITE(FORM23(2:3),'(I2)') IX
            if(me.eq.0) write(iwr,FORM23) 'Itr',SPACES(1:JX)
            WRITE(N_IWR(INX:INX+JX+2),'(A)') SPACES(1:JX+3)
            INX = INX+JX+3
          ENDIF
        ELSEIF( ISLC(17).NE.0 ) THEN
          if(me.eq.0) write(iwr,FORM2) 'Step','Node','Time','Timestep'
          INX = 1
          WRITE(N_IWR(INX:INX+37),'(A)') SPACES(1:38)
          INX = INX+38
          IX = (NSOLU)*4/2
          JX = (NSOLU)*4-IX+1
          IX = IX+4
          IC = ICOUNT(IX)
          IF( IC.EQ.1 ) THEN
            WRITE(FORM3(2:2),'(I1)') IX
            if(me.eq.0) write(iwr,FORM3) 'Itr',SPACES(1:JX)
            WRITE(N_IWR(INX:INX+JX+2),'(A)') SPACES(1:JX+3)
            INX = INX+JX+3
          ELSEIF( IC.EQ.2 ) THEN
            WRITE(FORM23(2:3),'(I2)') IX
            if(me.eq.0) write(iwr,FORM23) 'Itr',SPACES(1:JX)
            WRITE(N_IWR(INX:INX+JX+2),'(A)') SPACES(1:JX+3)
            INX = INX+JX+3
          ENDIF
        ELSE
          if(me.eq.0) write(iwr,FORM10) 'Step','Node','Time','Timestep','Itr'
          INX = 1
          WRITE(N_IWR(INX:INX+43),'(A)') SPACES(1:44)
          INX = INX+44
        ENDIF
        DO 20 NV = 1,NVREF
          IRNV = IREF(NV)
          IRNV_CW = IREF_CW(NV) ! Added for coupled well - Bryan
          IRNVGC = IREFGC(NV)
          ISX = ISGNO+7
          ITX = 4
          TXTX = TRIM(CHREF(IRNV))
!
!---      Gas components  ---
!
          IF( IRNVGC.GT.0 ) THEN
            NCH = INDEX( TXTX(1:),'  ')-1
            IF( IRNVGC.GT.9 ) THEN
              WRITE( TXTX(NCH+1:NCH+3),'(I2)' ) IRNVGC
              ITX = NCH+2
            ELSE
              WRITE( TXTX(NCH+1:NCH+2),'(I1)' ) IRNVGC
              ITX = NCH+1
            ENDIF
          ENDIF
          CALL CNTRTXT( TXTX,STRX,ITX,ISX )
          if(me.eq.0) write(iwr,'(A,$)') STRX(1:ISX)
!
!---      Reactive species  ---
!
          INDX = (400+(NSOLU*33)+((NEQC+NEQK)*33))
          IF( IRNV.GT.INDX ) THEN
            IF( MOD((IRNV-INDX),33).EQ.0 ) THEN
              NSP = ((IRNV-INDX)/33)
            ELSE
              NSP = ((IRNV-INDX)/33) + 1
            ENDIF
            IF( NSP.GT.NSPL+NSPS+NSPE ) THEN
              NCH = INDEX(SPNMG(NSP-NSPL-NSPS)(1:),'  ')-1 
              TXTX = SPNMG(NSP-NSPL-NSPS)(1:NCH)
              ITX = MIN(NCH,ISX)
            ELSEIF( NSP.GT.NSPL+NSPS ) THEN
              NCH = INDEX(SPNME(NSP-NSPL-NSPS)(1:),'  ')-1
              TXTX = SPNME(NSP-NSPL-NSPS)(1:NCH)
              ITX = MIN(NCH,ISX)
            ELSEIF( NSP.GT.NSPL ) THEN
              NCH = INDEX(SPNMS(NSP-NSPL)(1:),'  ')-1 
              TXTX = SPNMS(NSP-NSPL)(1:NCH)
              ITX = MIN(NCH,ISX)
            ELSE
              NCH = INDEX(SPNML(NSP)(1:),'  ')-1 
              TXTX = SPNML(NSP)(1:NCH)
              ITX = MIN(NCH,ISX)
            ENDIF
            CALL CNTRTXT( TXTX,STRX,ITX,ISX )
            WRITE(N_IWR(INX:INX+ISX-1),'(A)') STRX(1:ISX)
            INX = INX+ISX
            IN_IWR = 1
!
!---      Solutes, conservation component species, and 
!         kinetic component species  ---
!
          ELSEIF( IRNV.GT.400 ) THEN
            IF( MOD((IRNV-400),33).EQ.0 ) THEN
              NSL = ((IRNV-400)/33)
            ELSE
              NSL = ((IRNV-400)/33) + 1
            ENDIF
            NCH = INDEX(SOLUT(NSL)(1:),'  ')-1 
            TXTX = SOLUT(NSL)(1:NCH)
            ITX = MIN(NCH,ISX)
            CALL CNTRTXT( TXTX,STRX,ITX,ISX )
            WRITE(N_IWR(INX:INX+ISX-1),'(A)') STRX(1:ISX)
            INX = INX+ISX
            IN_IWR = 1
          ELSE
            WRITE(N_IWR(INX:INX+ISX-1),'(A)') SPACES(1:ISX)
            INX = INX+ISX
          ENDIF
   20   CONTINUE
        if(me.eq.0) write(iwr,FORM17)
        IF( IN_IWR.EQ.1 .and. me.eq.0) write(iwr,'(A)') N_IWR(1:INX-1)
        IF( ISLC(48).EQ.1 ) THEN
          if(me.eq.0) write(iwr,FORM7) '[',UNTM(1:6),']','[',UNTM(1:6),']', &
          SPACES(1:13)
        ELSEIF( ISLC(15).EQ.1 ) THEN
          IC = (LGRL-1)*3 + 8
          if(me.eq.0) write(iwr,FORM7) '[',UNTM(1:6),']','[',UNTM(1:6),']', &
          SPACES(1:IC)
        ELSEIF( ISLC(17).NE.0 ) THEN
          IC = (NSOLU)*3 + 8
          if(me.eq.0) write(iwr,FORM7) '[',UNTM(1:6),']','[',UNTM(1:6),']', &
          SPACES(1:IC)
        ELSE
          if(me.eq.0) write(iwr,FORM7) '[',UNTM(1:6),']','[',UNTM(1:6),']', &
          SPACES(1:6)
        ENDIF
        DO 30 NV = 1,NVREF
          IRNV = IREF(NV)
          IF( UNREF(IRNV) .NE. 'null' ) THEN
            IVAR = (ISGNO-2)/2
            IC = MAX( 1,IVAR )
            if(me.eq.0) write(iwr,FORM18) '[',UNREF(IRNV)(1:7),']',SPACES(1:IC)
          ELSE
            IVAR = (ISGNO-2)/2
            IC = MAX( 11,ISGNO+7 )
            if(me.eq.0) write(iwr,FORM19) SPACES(1:IC)
          ENDIF
   30   CONTINUE
        if(me.eq.0) write(iwr,FORM17)
      ENDIF
!
!---  Write subheader lines the screen ---
!
      IF( ICNS.EQ.10 ) THEN
        ICNS = 0
        if(me.eq.0) write(isc,FORM1) 'Reference Node(s)'
        DO 40 M = 1,NREF
          N = NDREF(M)
          nx = 0
          if(n.ne.0)nx = loc2nat(n)
          call ga_igop(1,nx,1,'max')
!          I = ID(N)
!          J = JD(N)
!          K = KD(N)
          if(me.eq.0) write(isc,FORM5) ' (',Nx,')'
   40   CONTINUE
        IN_ISC = 0
        if(me.eq.0) write(isc,FORM17)
        IF( ISLC(48).EQ.1 ) THEN
          if(me.eq.0) write(isc,FORM15) 'Step','Node','Time','Timestep','Itr', &
          'Active'
          INX = 1
          WRITE(N_ISC(INX:INX+49),'(A)') SPACES(1:50)
          INX = INX+50
        ELSEIF( ISLC(15).EQ.1 ) THEN
          if(me.eq.0) write(isc,FORM2) 'Step','Node','Time','Timestep'
          INX = 1
          WRITE(N_ISC(INX:INX+37),'(A)') SPACES(1:38)
          INX = INX+38
          IX = (LGRL-1)*3/2
          JX = (LGRL-1)*3-IX+1
          IX = IX+4
          IC = ICOUNT(IX)
          IF( IC.EQ.1 ) THEN
            WRITE(FORM3(2:2),'(I1)') IX
            if(me.eq.0) write(isc,FORM3) 'Itr',SPACES(1:JX)
            WRITE(N_ISC(INX:INX+JX+2),'(A)') SPACES(1:JX+3)
            INX = INX+JX+3
          ELSEIF( IC.EQ.2 ) THEN
            WRITE(FORM23(2:3),'(I2)') IX
            if(me.eq.0) write(isc,FORM23) 'Itr',SPACES(1:JX)
            WRITE(N_ISC(INX:INX+JX+2),'(A)') SPACES(1:JX+3)
            INX = INX+JX+3
          ENDIF
        ELSEIF( ISLC(17).NE.0 ) THEN
          if(me.eq.0) write(isc,FORM2) 'Step','Node','Time','Timestep'
          INX = 1
          WRITE(N_ISC(INX:INX+37),'(A)') SPACES(1:38)
          INX = INX+38
          IX = (NSOLU)*4/2
          JX = (NSOLU)*4-IX+1
          IX = IX+4
          IC = ICOUNT(IX)
          IF( IC.EQ.1 ) THEN
            WRITE(FORM3(2:2),'(I1)') IX
            if(me.eq.0) write(isc,FORM3) 'Itr',SPACES(1:JX)
            WRITE(N_ISC(INX:INX+JX+2),'(A)') SPACES(1:JX+3)
            INX = INX+JX+3
          ELSEIF( IC.EQ.2 ) THEN
            WRITE(FORM23(2:3),'(I2)') IX
            if(me.eq.0) write(isc,FORM23) 'Itr',SPACES(1:JX)
            WRITE(N_ISC(INX:INX+JX+2),'(A)') SPACES(1:JX+3)
            INX = INX+JX+3
          ENDIF
        ELSE
          if(me.eq.0) write(isc,FORM10) 'Step','Node','Time','Timestep','Itr'
          INX = 1
          WRITE(N_ISC(INX:INX+43),'(A)') SPACES(1:44)
          INX = INX+44
        ENDIF
        DO 50 NV = 1,NVREF
          IRNV = IREF(NV)
          IRNV_CW = IREF_CW(NV) ! Coupled well - Bryan
          IRNVGC = IREFGC(NV)
          ISX = ISGNS+7
          ITX = 4
          TXTX = CHREF(IRNV)
!
!---      Gas components  ---
!
          IF( IRNVGC.GT.0 ) THEN
            NCH = INDEX( TXTX(1:),'  ')-1
            IF( IRNVGC.GT.9 ) THEN
              WRITE( TXTX(NCH+1:NCH+3),'(I2)' ) IRNVGC
              ITX = NCH+2
            ELSE
              WRITE( TXTX(NCH+1:NCH+2),'(I1)' ) IRNVGC
              ITX = NCH+1
            ENDIF
          ENDIF
          CALL CNTRTXT( TXTX,STRX,ITX,ISX )
          if(me.eq.0) write(isc,'(A,$)') STRX(1:ISX)
!
!---      Reactive species  ---
!
          INDX = (400+(NSOLU*33)+((NEQC+NEQK)*33))
          IF( IRNV.GT.INDX ) THEN
            IF( MOD((IRNV-INDX),33).EQ.0 ) THEN
              NSP = ((IRNV-INDX)/33)
            ELSE
              NSP = ((IRNV-INDX)/33) + 1
            ENDIF
            IF( NSP.GT.NSPL+NSPS+NSPE) THEN
              NCH = INDEX(SPNMG(NSP-NSPL-NSPS)(1:),'  ')-1 
              TXTX = SPNMG(NSP-NSPL-NSPS)(1:NCH)
              ITX = MIN(NCH,ISX)
            ELSEIF( NSP.GT.NSPL+NSPS ) THEN
              NCH = INDEX(SPNME(NSP-NSPL-NSPS)(1:),'  ')-1
              TXTX = SPNME(NSP-NSPL-NSPS)(1:NCH)
              ITX = MIN(NCH,ISX)
            ELSEIF( NSP.GT.NSPL ) THEN
              NCH = INDEX(SPNMS(NSP-NSPL)(1:),'  ')-1 
              TXTX = SPNMS(NSP-NSPL)(1:NCH)
              ITX = MIN(NCH,ISX)
            ELSE
              NCH = INDEX(SPNML(NSP)(1:),'  ')-1 
              TXTX = SPNML(NSP)(1:NCH)
              ITX = MIN(NCH,ISX)
            ENDIF
            CALL CNTRTXT( TXTX,STRX,ITX,ISX )
            WRITE(N_ISC(INX:INX+ISX-1),'(A)') STRX(1:ISX)
            INX = INX+ISX
            IN_ISC = 1
!
!---      Solutes, conservation component species, and 
!         kinetic component species  ---
!
          ELSEIF( IRNV.GT.400 ) THEN
            IF( MOD((IRNV-400),33).EQ.0 ) THEN
              NSL = ((IRNV-400)/33)
            ELSE
              NSL = ((IRNV-400)/33) + 1
            ENDIF
            NCH = INDEX(SOLUT(NSL)(1:),'  ')-1 
            TXTX = SOLUT(NSL)(1:NCH)
            ITX = MIN(NCH,ISX)
            CALL CNTRTXT( TXTX,STRX,ITX,ISX )
            WRITE(N_ISC(INX:INX+ISX-1),'(A)') STRX(1:ISX)
            INX = INX+ISX
            IN_ISC = 1
          ELSE
            WRITE(N_ISC(INX:INX+ISX-1),'(A)') SPACES(1:ISX)
            INX = INX+ISX
          ENDIF
   50   CONTINUE
        if(me.eq.0) write(isc,FORM17)
        IF( IN_ISC.EQ.1 .and.me.eq.0) write(isc,'(A)') N_ISC(1:INX-1)
        IF( ISLC(48).EQ.1 ) THEN
          if(me.eq.0) write(isc,FORM7) '[',UNTM(1:6),']','[',UNTM(1:6),']', &
          SPACES(1:13)
        ELSEIF( ISLC(15).EQ.1 ) THEN
          IC = (LGRL-1)*3 + 8
          if(me.eq.0) write(isc,FORM7) '[',UNTM(1:6),']','[',UNTM(1:6),']', &
          SPACES(1:IC)
        ELSEIF( ISLC(17).NE.0 ) THEN
          IC = (NSOLU)*3 + 8
          if(me.eq.0) write(isc,FORM7) '[',UNTM(1:6),']','[',UNTM(1:6),']', &
          SPACES(1:IC)
        ELSE
          if(me.eq.0) write(isc,FORM7) '[',UNTM(1:6),']','[',UNTM(1:6),']', &
          SPACES(1:6)
        ENDIF
        DO 60 NV = 1,NVREF
          IRNV = IREF(NV)
          IF( UNREF(IRNV) .NE. 'null' ) THEN
            IVAR = (ISGNS-2)/2
            IC = MAX( 1,ISGNS-2-IVAR )
            if(me.eq.0) write(isc,FORM8) '[',UNREF(IRNV)(1:7),']',SPACES(1:IC)
          ELSE
            IVAR = (ISGNS-2)/2
            IC = MAX( 11,ISGNS+7 )
            if(me.eq.0) write(isc,FORM9) SPACES(1:IC)
          ENDIF
   60   CONTINUE
        if(me.eq.0) write(isc,FORM17)
      ENDIF
!     endif
!
!---  Write reference number information
!
      IF( MOD( (NSTEP-NRST),IFQO ).EQ.0 .OR. &
      IFQO.LT.1.AND.ABS(TMPR-TM)/EPSL.LE.EPSL .OR. & 
      MOD( (NSTEP-NRST),IFQS ).EQ.0 .OR. &
      IFQO.LT.1.AND.ABS(TMPR-TM)/EPSL.LE.EPSL ) THEN
        NINAC = NFLD-NXP
        DO 900 M = 1,NREF
          N = NDREF(M)
          nx = 0
          if(n.ne.0)nx = loc2nat(n)
          call ga_igop(5,nx,1,'max')
!          NPX = NSX(N)
!          NPY = NSY(N)
!          NPZ = NSZ(N)
!          NQX = NSX(N)+1
!          NQY = NSY(N)+IFLD
!          NQZ = NSZ(N)+IJFLD
          TMY = TM
          DTY = DT
          IF( UNTM .NE. 'null') THEN
            INDX = 4
            IUNS = 1
            CALL RDUNIT(UNTM,TMY,INDX)
            IUNS = 1
            CALL RDUNIT(UNTM,DTY,INDX)
          ENDIF
!
!---  Output file  ---
!
          IF( MOD( (NSTEP-NRST),IFQO ).EQ.0 .OR. &
            IFQO.LT.1.AND.ABS(TMPR-TM)/EPSL.LE.EPSL ) THEN
            IF( ISLC(48).EQ.1 ) THEN
              if(me.eq.0) write(iwr,FORM13) NSTEP,N,TMY,DTY,NITER,NINAC
!
!---  Courant number control output  ---
!
            ELSEIF( ISLC(17).NE.0 ) THEN
              if(me.eq.0) write(iwr,FORM12) NSTEP,Nx,TMY,DTY,' ['
              if(me.eq.0) write(iwr,'(I3,A,$)') NITER,'/'
              DO 62 MM = 1,NSOLU-1
                if(me.eq.0) write(iwr,'(I3,A,$)') N_CRN(MM),'/'
   62         CONTINUE
              if(me.eq.0) then
                if(nsolu.ne.0) then
                 write(iwr,'(I3,A,$)') N_CRN(NSOLU),']'
                else
                 write(iwr,'(I3,A,$)') N_CRN(nsolu+1),']'
                endif
              endif
            ELSE
              if(me.eq.0) write(iwr,FORM11) NSTEP,Nx,TMY,DTY,NITER
            ENDIF
          ENDIF
!
!---  Standard output (screen)  ---
!
          IF( MOD( (NSTEP-NRST),IFQS ).EQ.0 ) THEN
            IF( ISLC(48).EQ.1 ) THEN
              if(me.eq.0) write(isc,FORM13) NSTEP,N,TMY,DTY,NITER,NINAC
!
!---  Courant number control output  ---
!
            ELSEIF( ISLC(17).NE.0 ) THEN
              if(me.eq.0) write(isc,FORM12) NSTEP,Nx,TMY,DTY,' ['
              if(me.eq.0) write(isc,'(I3,A,$)') NITER,'/'
              DO 64 MM = 1,NSOLU-1
                if(me.eq.0) write(isc,'(I3,A,$)') N_CRN(MM),'/'
   64         CONTINUE
              if(me.eq.0) then
                if(nsolu.ne.0) then
                 write(isc,'(I3,A,$)') N_CRN(NSOLU),']'
                else
                 write(isc,'(I3,A,$)') N_CRN(NSOLU+1),']'
                endif
              endif
            ELSE
              if(me.eq.0)write(isc,FORM11) NSTEP,Nx,TMY,DTY,NITER
            ENDIF
          ENDIF
          DO 800 NV = 1,NVREF
            n = ndref(m)
            nx = 0
            if(n.ne.0)Nx = loc2nat(n)
            IRNV = IREF(NV)
!
!---      Check for integrated mass index ---
!
            IRNVX = 0
            INDX = (400+(NSOLU*33)+((NEQC+NEQK)*33))
            IF( IRNV.GT.400 .AND. IRNV.LE.INDX )THEN
             IF( MOD((IRNV-400),33).NE.0 ) THEN
               IRNVX = MOD((IRNV-400),33)
              ENDIF
            ELSEIF( IRNV.GT.INDX .AND.IRNV.LE.(INDX+NSPR*33) ) THEN
             IF( MOD((IRNV-INDX),33).NE.0 ) THEN
               IRNVX = MOD((IRNV-INDX),33)
              ENDIF
            ENDIF
!           IRNVX = MOD((IRNV-INDX),33)
            IRNVGC = IREFGC(NV)
            IRNV_CW = IREF_CW(NV) ! Coupled well - Bryan
            var = 0.d0
            iunm = 0
            iunkg = 0
            iuns = 0
            iunk = 0
            iunmol = 0
            iunmx = 0
            iunkgx = 0
            iunsx = 0
            iunkx = 0
            iunmolx = 0
            IF(NX.NE.0.OR.IRNVX.EQ.23.OR.IRNV.EQ.191 &
               .OR.IRNVX.EQ.6) &
               CALL REFVAR( VAR,VSKP,IRNV,IRNVGC,IRNV_CW,JSKP,N )
!              CALL REFVAR( VAR,VSKP,IRNV,IRNVGC,JSKP,N )
            varx = var
            var = abs(var)
            call ga_dgop(1,varx,1,'+')
!            if(irnvx.eq.6.or.irnvx.eq.23)then
!              call ga_dgop(1,var,1,'+')
!            else
              call ga_dgop(1,var,1,'max')
!            endif
            var = sign(var,varx)
            iunmx = iunm
            iunkgx = iunkg
            iunsx = iuns
            iunkx = iunk
            iunmolx = iunmol
            iunm = abs(iunm)
            iunkg = abs(iunkg)
            iuns = abs(iuns)
            iunk = abs(iunk)
            iunmol = abs(iunmol)
            call ga_igop(1,iunmx,1,'+')
            call ga_igop(1,iunkgx,1,'+')
            call ga_igop(1,iunsx,1,'+')
            call ga_igop(1,iunkx,1,'+')
            call ga_igop(1,iunmolx,1,'+')
            call ga_igop(1,iunm,1,'absmax')
            call ga_igop(1,iunkg,1,'absmax')
            call ga_igop(1,iuns,1,'absmax')
            call ga_igop(1,iunk,1,'absmax')
            call ga_igop(1,iunmol,1,'absmax')
            call ga_sync()
            iunm = sign(iunm,iunmx)
            iunkg = sign(iunkg,iunkgx)
            iuns = sign(iuns,iunsx)
            iunk = sign(iunk,iunkx)
            iunmol = sign(iunmol,iunmolx)
            IF( UNREF(IRNV) .NE. 'null' .and.me.eq.0) THEN
              INDX = 4
!           write(*,*) 'UNREF(IRNV)',UNREF(IRNV),'Var',var
              CALL RDUNIT(UNREF(IRNV),VAR,INDX)
            ENDIF
            IF( MOD( (NSTEP-NRST),IFQO ).EQ.0 .and. me.eq.0.OR. &
              IFQO.LT.1.AND.ABS(TMPR-TM)/EPSL.LE.EPSL.AND.ME.EQ.0) &
              write(iwr,FORM16) VAR
            IF( MOD( (NSTEP-NRST),IFQS ).EQ.0 .and. me.eq.0) write(isc,FORM6) VAR

  800     CONTINUE
          IF( MOD( (NSTEP-NRST),IFQO ).EQ.0 .and. me.eq.0.OR. &
              IFQO.LT.1.AND.ABS(TMPR-TM)/EPSL.LE.EPSL.AND.ME.EQ.0) &
              write(iwr,FORM17)
          IF( MOD( (NSTEP-NRST),IFQS ).EQ.0 .and. me.eq.0) write(isc,FORM17)
  900   CONTINUE
      ENDIF
      IF( MOD( (NSTEP-NRST),IFQO ).EQ.0 ) ICNO = ICNO + 1
      IF( MOD( (NSTEP-NRST),IFQS ).EQ.0 ) ICNS = ICNS + 1

      DEALLOCATE( VSKP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: VSKP'
        CALL WRMSGS( INDX )
      ENDIF
      DEALLOCATE( JSKP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: JSKP'
        CALL WRMSGS( INDX )
      ENDIF

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of REFNOD group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
       SUBROUTINE REFVAR( VAR,VSKP,IRNV,IRNVGC,IRNV_CW,JSKP,N )
!      SUBROUTINE REFVAR( VAR,VSKP,IRNV,IRNVGC,JSKP,N )
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
!     Reference node variable conversion.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 29 May 2001.
!     Last Modified by MD White, PNNL, 29 May 2001.
!     Last Modified by MD White, PNNL, 1 August 2002.
!     $Id: refnod.F,v 1.37 2008/02/13 01:05:53 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE REACT
      USE PORMED
      USE HYST
      USE GRID
      USE FLUXD
      USE FDVP
      USE FDVH
      USE CONST
      use fluxp
      use grid_mod
      USE COUP_WELL
      use plt_atm
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
      REAL*8 VSKP(*)
      INTEGER JSKP(*)
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/REFVAR'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(169)(1:1),'$').EQ.0 ) CVS_ID(169) = &
      '$Id: refnod.F,v 1.37 2008/02/13 01:05:53 d3c002 Exp $' 
        ICSN = ICSN+ICSNX
!
!---  Assign surface indices ---
!
!      NPX = NSX(N)
!      NPY = NSY(N)
!      NPZ = NSZ(N)
!      NQX = NSX(N)+1
!      NQY = NSY(N)+IFLD
!      NQZ = NSZ(N)+IJFLD
!
!---  Convert reference node variable ---
!
     IF( IRNV.LE.32 ) THEN
!
!---  Aqueous pressure (absolute)  ---
!
        IF( IRNV.EQ.1 ) THEN
          IUNM = -1
          IUNKG = 1
          IUNS = -2
          VAR = PL(2,N) + PATM
!
!---  Gas pressure (absolute)  ---
!
        ELSEIF( IRNV.EQ.2 ) THEN
          IUNM = -1
          IUNKG = 1
          IUNS = -2
          VAR = PG(2,N) + PATM
!
!---  NAPL pressure (absolute)  ---
!
        ELSEIF( IRNV.EQ.3 ) THEN
          IUNM = -1
          IUNKG = 1
          IUNS = -2
!          VAR = PN(2,N) + PATM
          VAR = PATM
!
!---  Temperature  ---
!
        ELSEIF( IRNV.EQ.4 ) THEN
          IUNK = 1
          VAR = T(2,N)
!
!---  Phase condition  ---
!
        ELSEIF( IRNV.EQ.5 ) THEN
          VAR = REAL( NPHAZ(2,N) )
!
!---  Aqueous pressure (gauge)  ---
!
       ELSEIF( IRNV.EQ.6 ) THEN
         IUNM = -1
         IUNKG = 1
         IUNS = -2
         VAR = PL(2,N)
!       
!---  Gas pressure (gauge)  ---
!
       ELSEIF( IRNV.EQ.7 ) THEN
         IUNM = -1
         IUNKG = 1
         IUNS = -2
         VAR = PG(2,N)
!       
!---  NAPL pressure (gauge)  ---
!
       ELSEIF( IRNV.EQ.8 ) THEN
         IUNM = -1
         IUNKG = 1
         IUNS = -2
         VAR = PN(2,N)
!       
!---  Apparent aqueous saturation  ---
!
       ELSEIF( IRNV.EQ.9 ) THEN
         VAR = ASL(N)
!
!---  Apparent total-liquid saturation  ---
!
       ELSEIF( IRNV.EQ.10 ) THEN
         VAR = AST(N)
!
!---  Actual aqueous saturation  ---
!
       ELSEIF( IRNV.EQ.11 ) THEN
         VAR = SL(2,N)
!
!---  Actual gas saturation  ---
!
       ELSEIF( IRNV.EQ.12 ) THEN
         VAR = SG(2,N)
!
!---  Actual NAPL saturation  ---
!
       ELSEIF( IRNV.EQ.13 ) THEN
         VAR = SN(2,N)
!
!---  Actual total-liquid saturation  ---
!
       ELSEIF( IRNV.EQ.14 ) THEN
         VAR = SL(2,N) + SN(2,N)
!
!---  Aqueous moisture content  ---
!
       ELSEIF( IRNV.EQ.15 ) THEN
         VAR = SL(2,N)*PORD(2,N)
!
!---  NAPL moisture content  ---
!
!      ELSEIF( IRNV.EQ.16 ) THEN
!        VAR = SN(2,N)*PORD(2,N)
!
!---  Gravimetric moisture content  ---
!
       ELSEIF( IRNV.EQ.16 ) THEN
         VAR = (SL(2,N)*PORD(2,N))/((RHOS(N)*(1-PORD(2,N)))/RHOL(2,N))
!
!---  Total-liquid moisture content  ---
!
       ELSEIF( IRNV.EQ.17 ) THEN
         VAR = (SL(2,N)+SN(2,N))*PORD(2,N)
!
!---  Apparent trapped-NAPL saturation  ---
!
       ELSEIF( IRNV.EQ.18 ) THEN
         VAR = ASNT(N)
!
!---  Apparent trapped-gas saturation  ---
!
       ELSEIF( IRNV.EQ.19 ) THEN
         VAR = ASGT(N)
!
!---  Diffusive porosity  ---
!
      ELSEIF( IRNV.EQ.20 ) THEN
        VAR = PORD(2,N)
!
!---  Gas dissolved water mass fraction
!     (water-vapor mass fraction)  ---
!
       ELSEIF( IRNV.EQ.21 ) THEN
         VAR = XGW(2,N)
!
!---  Aqueous water mass fraction  ---
!
       ELSEIF( IRNV.EQ.24 ) THEN
         VAR = XLW(2,N)
!
!---  Aqueous hydraulic head  ---
!
       ELSEIF( IRNV.EQ.27 ) THEN
         IUNM = 1
         VAR = PL(2,N)/RHORL/GRAV + ZP(N)
!         VAR = PL(2,N)/RHORL/GRAV
!
!---  Gas hydraulic head  ---
!
       ELSEIF( IRNV.EQ.28 ) THEN
         IUNM = 1
         VAR = PG(2,N)/RHORL/GRAV
!       
!---  Rock/soil type  ---
!
       ELSEIF( IRNV.EQ.30 ) THEN
         VAR = REAL( IZ(N) )
!
!---  Aqueous relative permeability ---
!
       ELSEIF( IRNV.EQ.31 ) THEN
         VAR = (RKL(1,2,N)*RKL(2,2,N)*RKL(3,2,N))**(1./3.)
       ENDIF
     ENDIF
!
      IF( IRNV.GT.32 .AND. IRNV.LE.64 ) THEN
!
!---  NAPL relative permeability ---
!
        IF( IRNV.EQ.33 ) THEN
!        VAR = RKN(2,N)
!
!---  Aqueous density ---
!
        ELSEIF( IRNV.EQ.34 ) THEN
          IUNM = -3
          IUNKG = 1
          VAR = RHOL(2,N)
!
!---  Gas density ---
!
        ELSEIF( IRNV.EQ.35 ) THEN
          IUNM = -3
          IUNKG = 1
          VAR = RHOG(2,N)
!
!---  NAPL density ---
!
        ELSEIF( IRNV.EQ.36 ) THEN
          IUNM = -3
          IUNKG = 1
!          VAR = RHON(2,N)
!
!---  Total water mass ---
!
        ELSEIF( IRNV.EQ.37 ) THEN
          IUNKG = 1
          VAR = 0.D+0
          VAR = VAR + &
          PORD(2,N)*VOL(N)*XLW(2,N)*SL(2,N)*RHOL(2,N)
          IF( IEQA.GT.0 ) VAR = VAR + &
          PORD(2,N)*VOL(N)*XGW(2,N)*SG(2,N)*RHOG(2,N)
!         IF( IOM.EQ.7 ) VAR = PORD(2,N)*VOL(N)* &
!        (XLW(2,N)*SL(2,N)*RHOL(2,N)+XNW(2,N)*SN(2,N)*RHON(2,N))
!         IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
!          VAR = PORD(2,N)*VOL(N)*(XLW(2,N)*SL(2,N)*RHOL(2,N) + &
!          XGW(2,N)*SG(2,N)*RHOG(2,N) + XHW(2,N)*SH(2,N)*RHOH(2,N))
!        ENDIF
!
!---  Total air, CO2, or gas component mass  ---
!
        ELSEIF( IRNV.EQ.38 ) THEN
!          IUNKG = 1
!          VAR = 0.D+0
!          VAR = VAR + &
!          PORD(2,N)*VOL(N)*XLA(2,N)*SL(2,N)*RHOL(2,N)
!          IF( IEQA.GT.0 ) VAR = VAR + &
!          PORD(2,N)*VOL(N)*XGA(2,N)*SG(2,N)*RHOG(2,N)
!          IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
!            VAR = PORD(2,N)*VOL(N)*(XLA(2,N)*SL(2,N)*RHOL(2,N) + &
!            XGA(2,N)*SG(2,N)*RHOG(2,N) + XHA(2,N)*SH(2,N)*RHOH(2,N) + &
!            XNA(2,N)*SN(2,N)*RHON(2,N))
!          ELSEIF( IOM.EQ.30 ) THEN
!            VAR = PORD(2,N)*VOL(N)*(XLC(IRNVGC,2,N)*SL(2,N)*RHOL(2,N) + &
!            XGC(IRNVGC,2,N)*SG(2,N)*RHOG(2,N))
!          ENDIF
!
!---  Total oil or CH4 mass ---
!
        ELSEIF( IRNV.EQ.39 ) THEN
!          IUNKG = 1
!          VAR = 0.D+0
!          VAR = VAR + &
!          PORD(2,N)*VOL(N)*XLO(2,N)*SL(2,N)*RHOL(2,N)
!          IF( IEQA.GT.0 ) VAR = VAR + &
!          PORD(2,N)*VOL(N)*XGO(2,N)*SG(2,N)*RHOG(2,N)
!          IF( IEQO.GT.0 ) VAR = VAR + &
!          PORD(2,N)*VOL(N)*SN(2,N)*RHON(2,N) + &
!          (1.D+0-PORT(2,N))*VOL(N)*XSO(2,N)*RHOS(IZ(N))
!          IF( IOM.EQ.7 ) VAR = PORD(2,N)*VOL(N)* &
!          (XLO(2,N)*SL(2,N)*RHOL(2,N)+XNO(2,N)*SN(2,N)*RHON(2,N))
!          IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
!            VAR = PORD(2,N)*VOL(N)*(XLO(2,N)*SL(2,N)*RHOL(2,N) + &
!            XGO(2,N)*SG(2,N)*RHOG(2,N) + XHO(2,N)*SH(2,N)*RHOH(2,N))
!          ENDIF
!
!---  Water mass source integral ---
!
        ELSEIF( IRNV.EQ.40 ) THEN
          IUNKG = 1
          VAR = SRCIW(N)
!
!---  Air or gas component mass source integral  ---
!
        ELSEIF( IRNV.EQ.41 ) THEN
!          IUNKG = 1
!          IF( IOM.EQ.30 ) THEN
!            VAR = SRCIGC(N,IRNVGC)
!          ELSE
!            VAR = SRCIA(N)
!          ENDIF
!
!---  Oil mass source integral  ---
!
        ELSEIF( IRNV.EQ.42 ) THEN
!
!---  Energy source integral  ---
!
        ELSEIF( IRNV.EQ.43 ) THEN
!
!---  X-dir. equivalent thermal conductivity  ---
!
        ELSEIF( IRNV.EQ.44 ) THEN
!
!---  Y-dir. equivalent thermal conductivity  ---
!
        ELSEIF( IRNV.EQ.45 ) THEN
!
!---  Z-dir. equivalent thermal conductivity  ---
!
        ELSEIF( IRNV.EQ.46 ) THEN
!
!---  Salt volumetric concentration  ---
!
        ELSEIF( IRNV.EQ.47 ) THEN
!
!---  Salt aqueous concentration  ---
!
        ELSEIF( IRNV.EQ.48 ) THEN
!
!---  Aqueous-flux Courant number  ---
!
        ELSEIF( IRNV.EQ.49 ) THEN
        VAR = CRNTL(N)
!
!---  Total salt mass  ---
!
        ELSEIF( IRNV.EQ.50 ) THEN
!
!---  X aqueous volumetric flux  ---
!
        ELSEIF( IRNV.EQ.51 ) THEN
          IUNM = 1
          IUNS = -1
          VAR = Q_FLUX_ND(1,N)
!
!---  Y aqueous volumetric flux  ---
!
        ELSEIF( IRNV.EQ.52 ) THEN
          IUNM = 1
          IUNS = -1
          VAR = Q_FLUX_ND(2,N)
!
!---  Z aqueous volumetric flux  ---
!
        ELSEIF( IRNV.EQ.53 ) THEN
          IUNM = 1
          IUNS = -1
          VAR = Q_FLUX_ND(3,N)
!
!---  X gas volumetric flux  ---
!
        ELSEIF( IRNV.EQ.54 ) THEN
!
!---  Y gas volumetric flux  ---
!
        ELSEIF( IRNV.EQ.55 ) THEN
!
!---  Z gas volumetric flux  ---
!
        ELSEIF( IRNV.EQ.56 ) THEN
!
!---  X NAPL volumetric flux  ---
!
      ELSEIF( IRNV.EQ.57 ) THEN
!
!---  Y NAPL volumetric flux  ---
!
      ELSEIF( IRNV.EQ.58 ) THEN
!
!---  Z NAPL volumetric flux  ---
!
      ELSEIF( IRNV.EQ.59 ) THEN
!
!---  X heat flux  ---
!
        ELSEIF( IRNV.EQ.60 ) THEN
!
!---  Y heat flux  ---
!
        ELSEIF( IRNV.EQ.61 ) THEN
!
!---  Z heat flux  ---
!
        ELSEIF( IRNV.EQ.62 ) THEN
!
!---  Matric Potential  ---
!
        ELSEIF( IRNV.EQ.63 ) THEN
          IUNM = 1
          VAR = MIN(PL(2,N)-PG(2,N),0.D+0)/RHORL/GRAV
!
!---  X salt flux  ---
!
        ELSEIF( IRNV.EQ.64 ) THEN
        ENDIF
      ENDIF
!
      IF( IRNV.GT.64 .AND. IRNV.LE.96 ) THEN
!
!---  Y salt flux  ---
!
        IF( IRNV.EQ.65 ) THEN
!
!---  Z salt flux  ---
!
        ELSEIF( IRNV.EQ.66 ) THEN
!
!---  X node-centered salt flux  ---
!
        ELSEIF( IRNV.EQ.67 ) THEN
!
!---  Y node-centered salt flux  ---
!
        ELSEIF( IRNV.EQ.68 ) THEN
!
!---  Z node-centered salt flux  ---
!
        ELSEIF( IRNV.EQ.69 ) THEN
!
!---  Gas water mole fraction  ---
!
        ELSEIF( IRNV.EQ.70 ) THEN
!
!---  Gas air or gas component mole fraction  ---
!
        ELSEIF( IRNV.EQ.71 ) THEN
!
!---  Aqueous water concentration  ---
!
        ELSEIF( IRNV.EQ.76 ) THEN
          IUNM = -3
          IUNKG = 1
          VAR = XLW(2,N)*RHOL(2,N)
!
!---  X node-centered aqueous volumetric flux  ---
!
        ELSEIF( IRNV.EQ.87 ) THEN
          IUNM = 1
          IUNS = -1
          VAR = vnc(1,n)
!
!---  Y node-centered aqueous volumetric flux  ---
!
        ELSEIF( IRNV.EQ.88 ) THEN
          IUNM = 1
          IUNS = -1
          VAR = vnc(2,n)
!
!---  Z node-centered aqueous volumetric flux  ---
!
        ELSEIF( IRNV.EQ.89 ) THEN
          IUNM = 1
          IUNS = -1
          VAR = vnc(3,n)
!		  print *, "VNC(3,n) = ", vnc(3,n)
!
!---  Osmotic pressure  ---
!
        ELSEIF( IRNV.EQ.101 ) THEN
          VAR = POSM(2,N)
          IUNM = -1
          IUNKG = 1
          IUNS = -2
!
!---  Minimum effective aqueous saturation  ---
!
        ELSEIF( IRNV.EQ.127 ) THEN
          VAR = ASLMIN(2,N)
!
!---  Water-vapor partial pressure  ---
!
        ELSEIF( IRNV.EQ.128 ) THEN
          IUNM = -1
          IUNKG = 1
          IUNS = -2
          VAR = PVW(2,N)
        ENDIF
      ENDIF
!
      IF( IRNV.GT.128 .AND. IRNV.LE.160 ) THEN
!
!---  Water mass source rate  ---
!
        IF( IRNV.EQ.140 ) THEN
          IUNKG = 1
          IUNS = -1
          VAR = SRCW(2,N)
!
!---    Coupled-well pressure    ---   ! coupled well - Bryan
!
        IF( L_CW.EQ.1 ) THEN

         VAR = P_CW_G(IRNV_CW) + PATM
        endif
       ELSEIF( IRNV.EQ.140 ) THEN
        IUNKG = 1
        IUNS = -1
        VAR = SRCW(2,N)
!
!---  Aqueous well depth  ---
!
        ELSEIF( IRNV.EQ.144 ) THEN
          VAR = 0.D+0
          DO 72 NS = 1,NSR
            IF( ISRT(NS).GE.20 .AND. ISRT(NS).LE.29 ) THEN
!              DO 70 I = ISRDM(1,NS),ISRDM(2,NS)
!              DO 70 J = ISRDM(3,NS),ISRDM(4,NS)
!              DO 70 K = ISRDM(5,NS),ISRDM(6,NS)
!                IF( ND(I,J,K).EQ.N ) THEN
!                  VAR = (PLWB(2,NS)-PGW(2,NS))/RHORL/GRAV
!                  GOTO 74
!                ENDIF
!   70         CONTINUE
            ENDIF
   72     CONTINUE
   74     CONTINUE
          IUNM = 1
!
!---  Aqueous well flow rate  ---
!
        ELSEIF( IRNV.EQ.145 ) THEN
          VAR = 0.D+0
          DO 82 NS = 1,NSR
            IF( ISRT(NS).GE.20 .AND. ISRT(NS).LE.29 ) THEN
!              DO 80 I = ISRDM(1,NS),ISRDM(2,NS)
!              DO 80 J = ISRDM(3,NS),ISRDM(4,NS)
!              DO 80 K = ISRDM(5,NS),ISRDM(6,NS)
!                IF( ND(I,J,K).EQ.N ) THEN
!                  VAR = QLW(3,NS)
!                  GOTO 84
!                ENDIF
!   80       CONTINUE
            ENDIF
   82     CONTINUE
   84     CONTINUE
          IUNM = 3
          IUNS = -1
!
!---  Aqueous well flow integral  ---
!
        ELSEIF( IRNV.EQ.146 ) THEN
          VAR = 0.D+0
          DO 92 NS = 1,NSR
            IF( ISRT(NS).GE.20 .AND. ISRT(NS).LE.29 ) THEN
!              DO 90 I = ISRDM(1,NS),ISRDM(2,NS)
!              DO 90 J = ISRDM(3,NS),ISRDM(4,NS)
!              DO 90 K = ISRDM(5,NS),ISRDM(6,NS)
!                IF( ND(I,J,K).EQ.N ) THEN
!                  VAR = QLW(1,NS)
!                  GOTO 94
!                ENDIF
!   90         CONTINUE
            ENDIF
   92     CONTINUE
   94     CONTINUE
          IUNM = 3
!
!---  Scanning path  ---
!
        ELSEIF( IRNV.EQ.149 ) THEN
          VAR = REAL( IPH(2,N) )
!
!---  Total well flow rate   ---
!
        ELSEIF( IRNV.EQ.156 ) THEN
          VAR = 0.D+0
          DO 122 NS = 1,NSR
            IF( ISRT(NS).GE.20 .AND. ISRT(NS).LE.29 ) THEN
!              DO 120 I = ISRDM(1,NS),ISRDM(2,NS)
!              DO 120 J = ISRDM(3,NS),ISRDM(4,NS)
!              DO 120 K = ISRDM(5,NS),ISRDM(6,NS)
!                IF( ND(I,J,K).EQ.N ) THEN
!                  VAR = QTW(3,NS)
!                  GOTO 124
!                ENDIF
!  120       CONTINUE
            ENDIF
  122     CONTINUE
  124     CONTINUE
          IUNM = 3
          IUNS = -1
!
!---  Total well flow integral   ---
!
        ELSEIF( IRNV.EQ.157 ) THEN
          VAR = 0.D+0
          DO 132 NS = 1,NSR
            IF( ISRT(NS).GE.20 .AND. ISRT(NS).LE.29 ) THEN
!              DO 130 I = ISRDM(1,NS),ISRDM(2,NS)
!              DO 130 J = ISRDM(3,NS),ISRDM(4,NS)
!              DO 130 K = ISRDM(5,NS),ISRDM(6,NS)
!                IF( ND(I,J,K).EQ.N ) THEN
!                  VAR = QTW(1,NS)
!                  GOTO 134
!                ENDIF
!  130       CONTINUE
            ENDIF
  132     CONTINUE
  134     CONTINUE
          IUNM = 3
        ENDIF
      ENDIF
      IF( IRNV.GT.160 .AND. IRNV.LE.200 ) THEN
!
!---  Aqueous viscosity  ---
!
        IF( IRNV.EQ.176 ) THEN
          IUNM = -1
          IUNKG = 1
          IUNS = -1
          VAR = VISL(2,N)
!
!---  Matric potential  ---
!
        ELSEIF( IRNV.EQ.178 ) THEN
          IUNM = 1
          IF( IOM.EQ.2 ) THEN
            VAR = (PL(2,N)-PG(2,N))/RHORL/GRAV
          ELSEIF( IOM.EQ.4 ) THEN
           VAR = PI(IEQW+2,N)
          ELSE
           VAR = (PL(2,N)-PG(2,N))/RHORL/GRAV
          ENDIF

!
!---  Integrated water mass  ---
!
        ELSEIF( IRNV.EQ.191 ) THEN
          IUNKG = 1
          IF( JSKP(191).EQ.0 ) THEN
            VAR = 0.D+0
            DO 191 LX = 1,num_loc_nodes
              L = ID_L2G(LX)
              IF( IXP(L).LE.0 ) GOTO 191
              VAR = VAR + &
              PORD(2,L)*VOL(L)*XLW(2,L)*SL(2,L)*RHOL(2,L)
  191       CONTINUE
            CALL GA_DGOP(1,VAR,1,'+')
            VSKP(191) = VAR
            JSKP(191) = 1
          ELSE
            VAR = VSKP(191)
          ENDIF
!
!---  Integrated aqueous water mass  ---
!
        ELSEIF( IRNV.EQ.194 ) THEN
          IUNKG = 1
          IF( JSKP(194).EQ.0 ) THEN
            VAR = 0.D+0
            DO 194 L = 1,num_nodes
              IF( IXP(L).LE.0 ) GOTO 194
              VAR = VAR + &
              PORD(2,L)*VOL(L)*XLW(2,L)*SL(2,L)*RHOL(2,L)
  194       CONTINUE
            VSKP(194) = VAR
            JSKP(194) = 1
          ELSE
            VAR = VSKP(194)
          ENDIF
        
        ENDIF
      ENDIF
!      IF( IRNV.GT.200 .AND. IRNV.LE.240 ) THEN
!
!---  X aqueous relative permeability ---
!
      IF( IRNV.EQ.201 ) THEN
        VAR = RKL(1,2,N)
!
!---  Y aqueous relative permeability ---
!
      ELSEIF( IRNV.EQ.202 ) THEN
        VAR = RKL(2,2,N)
!
!---  Z aqueous relative permeability ---
!
      ELSEIF( IRNV.EQ.203 ) THEN
        VAR = RKL(3,2,N)

      ENDIF
      IF( IRNV.GT.240 .AND. IRNV.LE.280 ) THEN
!
!---  X-Direction Intrinsic Permeability  ---
!
      ELSEIF( IRNV.EQ.247 ) THEN
        IUNM = 2
        INDX = 1
        VAR = PERM(INDX,N)*PERMRF(2,N)
!
!---  Y-Direction Intrinsic Permeability  ---
!
      ELSEIF( IRNV.EQ.248 ) THEN
        IUNM = 2
        INDX = 2
        VAR = PERM(INDX,N)*PERMRF(2,N)
!
!---  Z-Direction Intrinsic Permeability  ---
!
      ELSEIF( IRNV.EQ.249 ) THEN
        IUNM = 2
        INDX = 3
        VAR = PERM(INDX,N)*PERMRF(2,N)
!
!---  Integrated water mass source  ---
!
      ELSEIF( IRNV.EQ.261 ) THEN
        IUNKG = 1
        IF( JSKP(261).EQ.0 ) THEN
          VAR = 0.D+0
          DO 261 L = 1,num_nodes
            IF( IXP(L).LE.0 ) GOTO 261
            VAR = VAR + SRCIW(L)
  261     CONTINUE
          VSKP(261) = VAR
          JSKP(261) = 1
        ELSE
          VAR = VSKP(261)
        ENDIF
!
!---  Differential integrated water mass  ---
!
      ELSEIF( IRNV.EQ.282 ) THEN
        IUNKG = 1
        IF( JSKP(282).EQ.0 ) THEN
          VAR = 0.D+0
          DO 282 L = 1,num_nodes
            IF( IXP(L).LE.0 ) GOTO 282
              VAR = VAR + &
              PORD(2,L)*VOL(L)*XLW(2,L)*SL(2,L)*RHOL(2,L)
  282     CONTINUE
          IF( NSTEP-NRST.EQ.0 ) THEN
            DMWX = VAR
            VAR = 0.D+0
          ELSE
            VAR = VAR-DMWX
          ENDIF
          VSKP(282) = VAR
          JSKP(282) = 1
        ELSE
          VAR = VSKP(282)
        ENDIF
!
!---  Evapotranspiration  ---
!
      ELSEIF( IRNV.EQ.289 ) THEN
        IUNM = 1
        IUNS = -1
        VAR = evap_trans(2,N)      
      ENDIF
    
!
!---  Coupled-well mass rate   ---
!
      IF( IRNV.EQ.349 ) THEN
        IUNKG = 1
        IUNS = -1
        VAR = G_QM_CW(1,IRNV_CW)
!
!---  Coupled-well mass integral  ---
!
      ELSEIF( IRNV.EQ.350 ) THEN
        IUNKG = 1
        VAR = G_QM_CW(2,IRNV_CW)
      ENDIF

!
!---  Solute, conservation-component species, and
!     kinetic-component species reference-node output ---
!
      INDX = (400+(NSOLU*33)+((NEQC+NEQK)*33))
      IF( IRNV.GT.400 .AND. IRNV.LE.INDX ) THEN
        IF( MOD((IRNV-400),33).EQ.0 ) THEN
          NSL = ((IRNV-400)/33)
          IRNVX = 33
        ELSE
          NSL = ((IRNV-400)/33) + 1
          IRNVX = MOD((IRNV-400),33)
        ENDIF
        IF( NSL.GT.NSOLU ) IUNMOL = 1
        IF( IRNVX.EQ.1 ) THEN
          IUNM = -3
          VAR = C(NSL,N)
        ELSEIF( IRNVX.EQ.2 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = C(NSL,N)*YL(NSL,N)/(SL(2,N)*PORD(2,N))
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 if(nsp.le.nspl) var = var + EQ_C(M,NEQ)*SP_CX
                enddo
                var = var*YL(NSL,N)/(SL(2,N)*PORD(2,N))
               endif
                IF( ISLC(40).EQ.1 ) THEN
                  IF( IMMB(NSL-NSOLU).EQ.1 ) &
                    VAR = C(NSL,N)/(SL(2,N)*PORD(2,N))
                ENDIF
            endif
          ELSE
            VAR = 0.D+0
          ENDIF
        ELSEIF( IRNVX.EQ.3 ) THEN
          IUNM = -3
          IF( SG(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = C(NSL,N)*YG(NSL,N)/(SG(2,N)*PORD(2,N))
          ELSE
            VAR = 0.D+0
          ENDIF
        ELSEIF( IRNVX.EQ.4 ) THEN
        ELSEIF( IRNVX.EQ.5 ) THEN
          VAR = YL(NSL,N)
        ELSEIF( IRNVX.EQ.6 ) THEN
          VAR = 0.D+0
          DO 610 LX = 1,NUM_LOC_NODES
              L = ID_L2G(LX)
              IF( IXP(L).LE.0 ) GOTO 610
            VAR = VAR + C(NSL,L)*YL(NSL,L)*VOL(L)
  610     CONTINUE
          CALL GA_DGOP(1,VAR,1,'+')
        ELSEIF( IRNVX.EQ.7 ) THEN
          VAR = YN(NSL,N)
        ELSEIF( IRNVX.EQ.8 ) THEN
          IUNM = -2
          IUNS = -1
          VAR = C_FLUX_ND(1,NSL,N)
        ELSEIF( IRNVX.EQ.9 ) THEN
          IUNM = -2
          IUNS = -1
          VAR = C_FLUX_ND(2,NSL,N)
!          VAR = VC(NPY,NSL)
        ELSEIF( IRNVX.EQ.10 ) THEN
          IUNM = -2
          IUNS = -1
          VAR = C_FLUX_ND(3,NSL,N)
!          VAR = WC(NPZ,NSL)
        ELSEIF( IRNVX.EQ.11 ) THEN
            VAR = SRCIC(NSL,N)
        ELSEIF( IRNVX.EQ.12 ) THEN
          IUNM = -3
          VAR = YL(NSL,N) + YN(NSL,N) + YG(NSL,N)
        ELSEIF( IRNVX.EQ.13 ) THEN
          IUNM = -3
          VAR = C(NSL,N)
        ELSEIF( IRNVX.EQ.14 ) THEN
          IUNM = -3
          VAR = CNL(NSL,N)
        ELSEIF( IRNVX.EQ.15 ) THEN
          IUNM = -3
          VAR = YG(NSL,N)
        ELSEIF( IRNVX.EQ.16 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = UC(NPX,NSL)
        ELSEIF( IRNVX.EQ.17 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = VC(NPY,NSL)
        ELSEIF( IRNVX.EQ.18 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = WC(NPZ,NSL)
        ELSEIF( IRNVX.EQ.19 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = UCN(NPX,NSL)
        ELSEIF( IRNVX.EQ.20 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = VCN(NPY,NSL)
        ELSEIF( IRNVX.EQ.21 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = WCN(NPZ,NSL)
        ELSEIF( IRNVX.EQ.22 ) THEN
          IUNS = -1
          VAR = YN(NSL,npz)
        ELSEIF( IRNVX.EQ.23 ) THEN
          VAR = 0.D+0
          DO 620 LX = 1,NUM_LOC_NODES
              L = ID_L2G(LX)
              IF( IXP(L).LE.0 ) GOTO 620
              VAR = VAR + C(NSL,L)*VOL(L)
  620     CONTINUE
          CALL GA_DGOP(1,VAR,1,'+')
!Add solute/species conc per total volume
!solid
        ELSEIF( IRNVX.EQ.26 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = C(NSL,N)*(1.d0-YL(NSL,N)-YG(NSL,N))
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 nspbx = nspl
                 nspex = nspl+nsps
                 if(nsp.ge.nspbx .and. nsp.le.nspex) &
                    var = var + EQ_C(M,NEQ)*SP_CX
                enddo
               endif
            endif
          ELSE
            VAR = 0.D+0
          ENDIF
!exchanged
        ELSEIF( IRNVX.EQ.27 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 nspbx = nspl+nsps
                 nspex = nspl+nsps+nspe
                 if(nsp.ge.nspbx .and. nsp.le.nspex) &
                    var = var + EQ_C(M,NEQ)*SP_CX
                enddo
               endif
            endif
          ELSE
            VAR = 0.D+0
          ENDIF
!gas
        ELSEIF( IRNVX.EQ.28 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = C(NSL,N)*YG(NSL,N)/(SL(2,N)*PORD(2,N))
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 nspbx = nspl+nsps+nspe
                 nspex = nspbx+nspg
                 if(nsp.ge.nspbx .and. nsp.le.nspex) &
                    var = var + EQ_C(M,NEQ)*SP_CX
                enddo
               endif
            endif
          ELSE
            VAR = 0.D+0
          ENDIF
!napl
        ELSEIF( IRNVX.EQ.29 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 nspbx = nspl+nsps+nspe+nspg
                 nspex = nspbx+nspn
                 if(nsp.ge.nspbx .and. nsp.le.nspex) &
                    var = var + EQ_C(M,NEQ)*SP_CX
                enddo
               endif
            endif
          ELSE
            VAR = 0.D+0
          ENDIF
!Add solute/species conc per total aqueous volume
!solid
        ELSEIF( IRNVX.EQ.30 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = C(NSL,N)*(1.d0-YL(NSL,N)-YG(NSL,N))/(SL(2,N)*PORD(2,N))
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 nspbx = nspl
                 nspex = nspl+nsps
                 if(nsp.ge.nspbx .and. nsp.le.nspex) &
                    var = var + EQ_C(M,NEQ)*SP_CX
                enddo
                var = var/(SL(2,N)*PORD(2,N))
               endif
            endif
          ELSE
            VAR = 0.D+0
          ENDIF
!exchanged
        ELSEIF( IRNVX.EQ.31 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 nspbx = nspl+nsps
                 nspex = nspl+nsps+nspe
                 if(nsp.ge.nspbx .and. nsp.le.nspex) &
                    var = var + EQ_C(M,NEQ)*SP_CX
                enddo
                var = var/(SL(2,N)*PORD(2,N))
               endif
            endif
          ELSE
            VAR = 0.D+0
          ENDIF
!gas
        ELSEIF( IRNVX.EQ.32 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = C(NSL,N)*YG(NSL,N)/(SL(2,N)*PORD(2,N))
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 nspbx = nspl+nsps+nspe
                 nspex = nspbx+nspg
                 if(nsp.ge.nspbx .and. nsp.le.nspex) &
                    var = var + EQ_C(M,NEQ)*SP_CX
                enddo
                var = var/(SL(2,N)*PORD(2,N))
               endif
            endif
          ELSE
            VAR = 0.D+0
          ENDIF
!napl
        ELSEIF( IRNVX.EQ.33 ) THEN
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            if(nsl.gt.nsolu) then
               neq = nsl-nsolu
               if(neq<=neqc) then
                var = 0.d0
                DO  M = 1,IEQ_C(1,NEQ)
                 NSP = IEQ_C(M+1,NEQ)
                 IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                   SP_CX = 0.D+0
                 ELSE
                   SP_CX = SP_C(NSP,N)
                 ENDIF
                 nspbx = nspl+nsps+nspe+nspg
                 nspex = nspbx+nspn
                 if(nsp.ge.nspbx .and. nsp.le.nspex) &
                    var = var + EQ_C(M,NEQ)*SP_CX
                enddo
                var = var/(SL(2,N)*PORD(2,N))
               endif
            endif
          ELSE
            VAR = 0.D+0
          ENDIF

        ENDIF
        IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
      ENDIF
!
!---  Reactive species reference-node output ---
!
      INDX = (400+(NSOLU*33)+((NEQC+NEQK)*33))
      IF( IRNV.GT.INDX .AND.  &
      IRNV.LE.(INDX+NSPR*33) ) THEN
        IF( MOD((IRNV-INDX),33).EQ.0 ) THEN
          NSP = ((IRNV-INDX)/33)
          IRNVX = 33
        ELSE
          NSP = ((IRNV-INDX)/33) + 1
          IRNVX = MOD((IRNV-INDX),33)
        ENDIF
        IF( IRNVX.EQ.1 ) THEN
          IUNMOL = 1
          IUNM = -3
          VAR = SP_C(NSP,N)*1.D-3
        ELSEIF( IRNVX.EQ.2 ) THEN
          IUNMOL = 1
          IUNM = -3
          IF( SL(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = SP_C(NSP,N)/(SL(2,N)*PORD(2,N))
          ELSE
            VAR = 0.D+0
          ENDIF
          VAR = VAR*1.D-3
        ELSEIF( IRNVX.EQ.3 ) THEN
          IUNMOL = 1
          IUNM = -3
          IF( SG(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = SP_C(NSP,N)/(SG(2,N)*PORD(2,N))
          ELSE
            VAR = 0.D+0
          ENDIF
          VAR = VAR*1.D-3
        ELSEIF( IRNVX.EQ.4 ) THEN
          IUNMOL = 1
          IUNM = -3
          IF( SN(2,N)*PORD(2,N).GT.SMALL ) THEN
            VAR = SP_C(NSP,N)/(SN(2,N)*PORD(2,N))
          ELSE
            VAR = 0.D+0
          ENDIF
          VAR = VAR*1.D-3
        ELSEIF( IRNVX.EQ.5 ) THEN
          VAR = YL(NS,N)
        ELSEIF( IRNVX.EQ.6 ) THEN
          VAR = 0.D+0
          DO 710 L = 1,NUM_NODES
            VAR = VAR + C(NS,L)*YL(NS,L)*VOL(L)
  710     CONTINUE
        ELSEIF( IRNVX.EQ.7 ) THEN
        ELSEIF( IRNVX.EQ.8 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = UC(NPX,NS)
        ELSEIF( IRNVX.EQ.9 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = VC(NPY,NS)
        ELSEIF( IRNVX.EQ.10 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = WC(NPZ,NS)
        ELSEIF( IRNVX.EQ.11 ) THEN
!          VAR = SRCIC(N,NS)
        ELSEIF( IRNVX.EQ.12 ) THEN
          IUNM = -3
          VAR = YL(NS,N) + YN(N,NS) + YG(N,NS)
        ELSEIF( IRNVX.EQ.13 ) THEN
          IUNM = -3
          VAR = SP_C(NSP,N)
        ELSEIF( IRNVX.EQ.14 ) THEN
          IUNM = -3
!          VAR = CNL(N,NS)
        ELSEIF( IRNVX.EQ.15 ) THEN
          IUNM = -3
          VAR = YG(N,NS)
        ELSEIF( IRNVX.EQ.16 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = UC(NPX,NS)
        ELSEIF( IRNVX.EQ.17 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = VC(NPY,NS)
        ELSEIF( IRNVX.EQ.18 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = WC(NPZ,NS)
        ELSEIF( IRNVX.EQ.19 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = UCN(NPX,NS)
        ELSEIF( IRNVX.EQ.20 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = VCN(NPY,NS)
        ELSEIF( IRNVX.EQ.21 ) THEN
          IUNM = -2
          IUNS = -1
!          VAR = WCN(NPZ,NS)
        ELSEIF( IRNVX.EQ.22 ) THEN
          IUNS = -1
        ELSEIF( IRNVX.EQ.23 ) THEN
          VAR = 0.D+0
          IUNMOL = 1
          VAR = 0.D+0
          DO 720 LX = 1,num_loc_nodes
            L = ID_L2G(LX)
            VAR = VAR + SP_C(NSP,L)*VOL(L)
  720     CONTINUE
          CALL GA_DGOP(1,VAR,1,'+')
          VAR = VAR*1.D-3
        ELSEIF( IRNVX.EQ.24 ) THEN
          NSP_M = NSP-NSPL
          IUNM = 2
          VAR = SP_AREA(NSP_M,N)
        ELSEIF( IRNVX.EQ.25 ) THEN
          IUNMOL = 1
          IUNS = -1
          NSP_M = NSP-NSPL
          VAR = SP_RATE(NSP_M,N)
          VAR = VAR*1.D-3
        ELSEIF( IRNVX.EQ.26 ) THEN
          NSP_M = NSP-NSPL
          VAR = RS_S(3,NSP_M,N)
        ELSEIF( IRNVX.EQ.27 ) THEN
          VAR = c_ph(n)
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of REFVAR group ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRCVS
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
!     Write CVS Identifcations for the called subroutines to the
!     output file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, on 2 July 2004.
!     Last Modified by MD White, PNNL, on 2 July 2004.
!     $Id: refnod.F,v 1.37 2008/02/13 01:05:53 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE FILES
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

      LOGICAL :: use_ga

!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRCVS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(169)(1:1),'$').EQ.0 ) CVS_ID(169) = &
     '$Id: refnod.F,v 1.37 2008/02/13 01:05:53 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Write CVS header line  ---
!
      if(me.eq.0) write(iwr,'(/,A,/)') ' ---  Configuration Version Record  ---'
!
!---  Loop over files  ---
!
      DO 100 N = 1,LFILES
        IF( INDEX(CVS_ID(N)(1:),'$Id').NE.0 ) THEN
          NCH = INDEX(CVS_ID(N)(1:),'  ')-1
          if(me.eq.0) write(iwr,'(2X,A)') CVS_ID(N)(1:NCH)
        ENDIF
  100 CONTINUE
!
!---  Write CVS tailer line  ---
!
      if(me.eq.0) write(iwr,'(/,A)') ' ---  End of Configuration ' // &
      'Version Record  ---'
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRCVS group  ---
!
      RETURN
      END
