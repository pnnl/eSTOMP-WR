!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDOUUN( INDX )
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
!     Read reference node and plot file output variable units.
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
!   201	x aqueous relative permeability 'RKLX' 'null'
!   202	y aqueous relative permeability 'RKLY' 'null'
!   203	z aqueous relative permeability 'RKLZ' 'null'
!   204	aqueous co2 mole fraction 'XMLA' 'null'
!   205 aqueous salt mole fraction 'XMLS' 'null'
!   206 atmospheric temperature ' TA ', 'k'
!   207 atmospheric relative humidity ' RH '
!   208 atmospheric solar radiation ' RN ', 'w/m^2', 'kg/s^3'
!   209 atmospheric wind speed ' WS ', 'm/s'
!   210 residual NAPL saturation 'SNR '
!   211 mobile NAPL saturation 'SNM '
!   212 free NAPL saturation 'SNF '
!   213 surface temperature 'T_S', 'k'
!   214 surface vapor pressure 'PV_S', 'pa'
!   215 actual evaporation rate 'E_SA', 'kg/s'
!   216 potential evaporation rate 'PE_SA', 'kg/s'
!   217 actual transpiration rate 'T_SA', 'kg/s'
!   218 potential transpiration rate 'PT_SA', 'kg/s'
!   219 saturated co2 aqueous mass fraction 'SXLA'
!   220 aqueous alcohol mole fraction 'XMLA'
!   221 NAPL alcohol mode fraction 'XMNA'
!   222 aqueous alcohol mass fraction 'XLA '
!   223 NAPL alcohol mass fraction 'XNA '
!   224 atmospheric pressure, ' PA ', 'pa'
!   225 surface aqueous pressure, 'PL_S', 'pa'
!   226 surface gas pressure, 'PG_S', 'pa'
!   227 surface aqueous saturation, 'SL_S
!   228 surface latent heat flux, 'QL_S', 'w/m^2', 'kg/s^3'
!   229 surface sensible heat flux, 'QH_S', 'w/m^2', 'kg/s^3'
!   230 surface net long-wave radiation, 'RL_S', 'w/m^2', 'kg/s^3'
!   231 surface net short-wave radiation, 'RS_S', 'w/m^2', 'kg/s^3'
!   232 surface net total radiation, 'RT_S', 'w/m^2', 'kg/s^3'
!   233 surface water mass balance kg/s, 'WB_S'
!   234 plant temperature, 'T_P' or 'TPXX', k
!   235 plant temperature, 'TPXX', k
!   236 plant temperature, 'TPXX', k
!   237 plant temperature, 'TPXX', k
!   238 plant temperature, 'TPXX', k
!   239 rainfall interception mass, 'RFIM', kg
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
!   264 precipitated salt saturation, ' SS '
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
!   401	solute volumetric concentration 'C   ' '1/m^3'
!   402	solute aqueous concentration 'CL  ' '1/m^3'
!   403	solute gas concentration 'CG  ' '1/m^3'
!   404	solute NAPL concentration 'CN  ' '1/m^3'
!   405	solute aqueous mole fraction 'YL  ' 'null'
!   406	solute gas mole fraction 'YG  ' 'null'
!   407	solute NAPL mole fraction 'YN  ' 'null'
!   408	x solute flux 'UC  ' '1/s'
!   409	y solute flux 'VC  ' '1/s'
!   410	z solute flux 'WC  ' '1/s'
!   411	solute source 'SRC ' 'null'
!   434 species volumetric concentration 'SP' 'mol/m^3'
!   435 species aqueous concentration 'SPL' 'mol/m^3'
!   436 species mineral area 'SPMA ' 'm^2'
!   437 species mineral area 'SPMR ' 'mol/s'
!   456 species integrated Mass 'SPIM ' 'mol'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, November, 1995.
!     Last Modified by MD White, Battelle, November 21, 1995.
!     $Id: rdouun.F,v 1.29 2008/02/13 01:03:51 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE react
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



      PARAMETER (LUNS=400+33+33)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IUM(LUNS),IUKG(LUNS),IUS(LUNS),IUK(LUNS),IUMOL(LUNS)
      SAVE IUM,IUKG,IUS,IUK,IUMOL
!
!----------------------Data Statements---------------------------------!
!
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUM /   &          
      -1,-1,-1, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0,-3,-3,-3, 0, 0, 0, 0, &
       0, 0, 2, 1, 1, 1,-3,-3, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, &
       0, 0, 1,-2,-2,-2,-2,-2,-2, 0, 0, 0,-3,-3,-3,-3,-3,-3, 0,-1, &
       0,-3, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, &
      -1, 0,-3,-3, 0, 0, 0, 0,-3, 0,-3,-3, 0,-2,-2,-2,-2,-2,-2,-2, &
      -2,-2,-2,-2,-2, 0, 0,-1,-1, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 2, 1, 3, 3, 0, 0, 0, 0, 0,-3,-3, 3, 3, 3, 3, 0, 0, 0, &
      -3, 0, 0,-3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1, 1, 1,-1, &
       0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0, &
       0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0,-3,-1, 3, 0, 0, 2, 2, 2, 0, 0, 0,-3, 0,-1, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3,-1,-1, 0, 0, 0, 0, &
       0, 0, 0, 0,-1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
      -3,-3,-3,-3, 0, 0, 0,-2,-2,-2, 0,-3,-3,-3,-3,-2,-2,-2,-2,-2, &
      -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-3,-3,-3,-3, 0, 0, 0, &
      -2,-2,-2, 0,-3,-3,-3,-3,-2,-2,-2,-2,-2,-2, 0, 0, 2, 0,-3,-3, &
      -3,-3,-3,-3,-3,-3 /

!      -3,-3,-3,-3, 0, 0, 0,-2,-2,-2, 0,-3,-3,-3,-3,-2,-2,-2,-2,-2, &
!      -2, 0, 0,-3,-3, 2, 0, 0, 0, 0,-2,-2,-2,-3,-3,-3,-3,-3,-2,-2, &
!      -2,-2,-2,-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, &
!       0, 0, 0, 0, 0, 0/
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUS / &
        -2,-2,-2, 0, 0,-2,-2,-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0,-2,-3,-3,-3, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-3, &
      -3,-3, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-2, &
       0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-3,-3,-3, 0, 0, &
      -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1, &
      -1,-1,-1,-1,-1, 0, 0,-2,-2, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0,-1, &
      -1,-1,-3, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0,-1, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0,-1, 0, 0, 0,-2, &
       0, 0, 0,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0,-3,-1, 0, 0, 0, 0,-2,-1,-1,-1,-1, 0, 0, &
       0, 0, 0,-2,-2,-2, 0,-3,-3,-3,-3,-3,-1, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0,-2, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-2,-2, 0, 0, 0, 0, &
       0, 0, 0, 0,-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1, &
      -1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
      -1,-1,-1, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1,-1,-1, 0, 0,-1, 0, 0, &
       0, 0, 0, 0, 0, 0 /

!       0, 0, 0, 0, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0,-1,-1,-1,-1,-1, &
!      -1,-1, 0, 0, 0, 0,-1, 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0,-1,-1, &
!      -1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0, 0, &
!       0, 0, 0, 0, 0, 0/
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUKG / &
       1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, &
       1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, &
       1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, &
       0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, &
       1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, &
       1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, &
       1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, &
       1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, &
       0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, &
       0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, &
       0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, &
       0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, &
       1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, &
       1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0/
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUK / &
       0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0/
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUMOL / &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, &
       1, 1, 1, 1, 1, 1 /

!       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
!       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, &
!       0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, &
!       0, 0, 0, 0, 0, 0/
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RDOUUN'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(156)(1:1),'$').EQ.0 ) CVS_ID(156) = &
     '$Id: rdouun.F,v 1.29 2008/02/13 01:03:51 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      JNDX = INDX
      KNDX = 400+NSOLU*33
      IF( INDX.GT.KNDX ) THEN
        IF( MOD((INDX-KNDX),33).EQ.0 ) THEN
!          JNDX = 446
        ELSE
          JNDX = MOD((INDX-KNDX),33)+433
        ENDIF
        if(indx > 400+(NSOLU*33)+((NEQC+NEQK)*33) &
            .and. (jndx == 459.or.jndx==460)) jndx = 446
      ELSEIF( INDX.GT.400 ) THEN
        IF( MOD((INDX-400),33).EQ.0 ) THEN
          JNDX = 433
        ELSE
          JNDX = MOD((INDX-400),33)+400
        ENDIF
      ENDIF
      IUNM = IUM(JNDX)
      IUNKG = IUKG(JNDX)
      IUNS = IUS(JNDX)
      IUNMOL = IUMOL(JNDX)
      IUNK = IUK(JNDX)
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDOUUN group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSFUN( INDX )
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
!     Read surface flux rate and integral units.
!     Positive INDX indicates rate units.
!     Negative INDX indicates integral units.
!
!  1    Heat Flux
!  2    Aqueous Volumetric Flux
!  3    Gas Volumetric Flux
!  4    NAPL Volumetric Flux
!  5    Aqueous Mass Flux
!  6    Gas Mass Flux
!  7    NAPL Mass Flux
!  8    Salt Mass Flux
!  9    Aqueous Oil Mass Flux
!  11   Condensate Water Mass Flux
!  11   Gas CH4 Mass Flux
!  12   Aqueous CH4 Mass Flux
!  13   CH4 Mass Flux
!  20   Gas-Advective Heat Flux
!  21   Gas-Advective Water-Mass Flux
!  22   Gas-Advective Air-Mass Flux
!  25   Gas-Diffusive Heat Flux
!  26   Gas-Diffusive Water-Mass Flux
!  27   Gas-Diffusive Air-Mass Flux
!  28   Gas CO2 Mass Flux
!  29   Aqueous CO2 Mass Flux
!  30   Total CO2 Mass Flux
!  31   Gas-Advective Oil-Mass Flux
!  32   Gas-Diffusive Oil-Mass Flux
!  33   Gas-Total Oil-Mass Flux
!  34   Actual Evaporation
!  35   Potential Evaporation
!  36   Actual Transpiration
!  37   Potential Transpiration
!  38   Net Radiation
!  39   Net Short-Wave Radiation
!  40   Net Long-Wave Radiation
!  41   surface water-mass balance
!  42   surface rain-water runoff
!  43   Aqueous Water Mass Flux
!  44   Gas Water Mass Flux
!  45   Total Water Mass Flux
!  46   Aqueous-advective gas-component mass flux
!  47   Aqueous-diffusive gas-component mass flux
!  48   Gas-advective gas-component mass flux
!  49   Gas-diffusive gas-component mass flux
!  50   Total-advective gas-component mass flux
!  51   Total-diffusive gas-component mass flux
!  >100 Solute Flux
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, November, 1995.
!     Last Modified by MD White, Battelle, November 21, 1995.
!     Last Modified by MD White, Battelle, 10 December 2003.
!     $Id: rdouun.F,v 1.29 2008/02/13 01:03:51 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



      PARAMETER (LUNS=102)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IUM(LUNS),IUKG(LUNS),IUS(LUNS),IUK(LUNS),IUMOL(LUNS)
      SAVE IUM,IUKG,IUS,IUK,IUMOL
!
!----------------------Data Statements---------------------------------!
!
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUM /                                                       & 
       2, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, &
       0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, &
       0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUS /                                                     &
      -3,-1,-1,-1,-1,-1,-1,-1,-1, 0,-1,-1,-1, 0, 0, 0, 0, 0, 0,-3, &
      -1,-1, 0, 0,-3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-3,-3,-3, &
       0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
      -1,-1 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUKG /                                                    &
       1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, &
       1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
       0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUK /                                                     &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0 /
!        1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
      DATA IUMOL /                                                   &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
       0, 1 /
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RDSFUN'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(156)(1:1),'$').EQ.0 ) CVS_ID(156) = &
     '$Id: rdouun.F,v 1.29 2008/02/13 01:03:51 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      JNDX = ABS(INDX)
!
!---  Solute surface flux units  ---
!
      IF( JNDX.GT.100 .AND. JNDX.LE.(100+NSOLU) ) THEN
        JNDX = 101
!
!---  Conservation-component species or kinetic-component species
!     surface flux units  ---
!
      ELSEIF( JNDX.GT.(100+NSOLU) .AND. &
        JNDX.LE.(100+NSOLU+NEQC+NEQK) ) THEN
        JNDX = 102
      ENDIF
      IUNM = IUM(JNDX)
      IUNKG = IUKG(JNDX)
      IUNS = IUS(JNDX)
      IF( INDX.LT.0 ) IUNS = IUNS+1
      IUNMOL = IUMOL(JNDX)
      IUNK = IUK(JNDX)
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDSFUN group
!
      RETURN
      END
