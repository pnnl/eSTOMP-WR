

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPLOT
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
!     Write plot files
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
!   232 surface net total radiation, 'RT_S'
!   233 surface water mass balance kg/s, 'WB_S'
!   234 plant temperature C, 'T_P' or 'TPX'
!   235 plant temperature C, 'TPX'
!   236 plant temperature C, 'TPX'
!   237 plant temperature C, 'TPX'
!   238 plant temperature C, 'TPX'
!   239 rainfall interception mass, 'RFIM'
!   240 sorbed oil mass, kg oil, 'TSO '
!   241 sorbed oil mass fraction kg oil/kg soil, 'XSO '
!   242 sorbed oil volumetric concentration kg oil/m^3, 'CSO '
!   243 bare-soil aerodynamic resistance s/m, 'RABS'
!   244 surface volumetric precipitation rate m^3/s, 'PV_S'
!   245 surface mass precipitation rate kg/s, 'PM_S'
!   246 atmospheric stability parameter, 'ASP '
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
!   264	precipitated salt saturation ' SS '
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
!     Written by MD White, Battelle, February, 1993.
!     Last Modified by MD White, Battelle, October 15, 1997.
!     Last Modified by MD White, PNNL, 1 August 2002.




!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE REACT
      USE OUTPU
      USE GRID
      USE FLUXD
      USE FILES
      USE FDVP
      USE CONST
      use grid_mod
      USE SIO
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
#ifdef USE_H5HUT
     include "mpif.h"
#endif



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*4 FORM1
      CHARACTER*16 FN,FORM2
      CHARACTER*64 SPNMX
      logical t_ok
      integer idx,ldim, dim1,dim2, iflg, dflg
      character*64 t_string
      character*64 varname
      LOGICAL :: use_ga

      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DVAR
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2
      DATA FORM1 / '(I )' /
      DATA FORM2 / '(10(1PE16.9,1X))' /
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPLOT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
      '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

#ifdef USE_H5HUT
     if (sio_openw("plot_", MPI_COMM_WORLD, NSTEP, TM) /= 0) then
        write(*,*) "Warning: Unable to open hdf file"
     endif
     if (sio_write_string("Version",trim(CH_VRSN))) then
        write(*,*) "Warning: Unable to writer vesion"
     endif
! Vicky: 12/19/12
! These are apparently not set and perhaps have no memory
! because I get bizarre moving crashes if the are not commented out
     if (sio_write_string("Date",trim(CHDATE))) then
        write(*,*) "Warning: Unable to write date"
     endif
     if (sio_write_string("Time",trim(CHTIME))) then
        write(*,*) "Warning: Unable to write time"
     endif
#endif


#ifndef USE_H5HUT
     if(me.eq.0) then
      WRITE(FORM2(8:9),'(I2)') MAX( 10,ISGNP+6 )
      WRITE(FORM2(11:11),'(I1)') MIN( 9,ISGNP-1 )

!
!---  Dynamic memory allocation  ---
!
!      ALLOCATE( DVAR(1:LFD),STAT=ISTAT )
!      IF( ISTAT.NE.0 ) THEN
!        INDX = 3
!        CHMSG = 'Allocation Error: DVAR'
!        CALL WRMSGS( INDX )
!      ENDIF

!
!---  Create a new plot file with number of time steps as the file name
!     extension  ---
      N = 1
      NS = NSTEP
   10 NS = NS/10
      IF( NS.GE.1 ) THEN
        N = N + 1
        GOTO 10
      ENDIF
      FN(1:5) = 'plot.'
      WRITE(FORM1(3:3),'(I1)') N
      WRITE( FN(6:),FORM1) NSTEP
      OPEN(UNIT=IPL, FILE=FN, STATUS='UNKNOWN', FORM='FORMATTED')
      CLOSE(UNIT=IPL, STATUS='DELETE')
      OPEN(UNIT=IPL, FILE=FN, STATUS='NEW', FORM='FORMATTED')
!
!---  Write header  ---
!
      WRITE(IPL,'(A,//)')' Welcome to ...'
      WRITE(IPL,'(A)')   '                           eSTOMP'
      WRITE(IPL,'(A,//)')'                   A scalable version of'
      WRITE(IPL,'(A,//)')'        Subsurface Transport Over Multiple &
                                                                Phases'
      WRITE(IPL,'(A)')   ' This file was produced by eSTOMP, a &
                                              derivative work of STOMP.'
      WRITE(IPL,'(A)')   ' STOMP was developed by the Pacific &
                                         Northwest Laboratory, with'
      WRITE(IPL,'(A)')   ' support from the VOC-Arid Integrated &
                                                 Demonstration Project,'
      WRITE(IPL,'(A)')   ' Office of Technology Development, U.S. &
                                                  Department of Energy.'
      WRITE(IPL,'(A)')   ' Results from this version of STOMP should &
                                                        not be used for'
      WRITE(IPL,'(A,/)') ' license related applications.'
      WRITE(IPL,'(A,/)') ' For inquiries or assistance: &
                                                  Call (509) 372-4067'
      WRITE(IPL,'(A,//)')'                        ---  PLOT  ---'

      WRITE(IPL,'(2A)') 'Version: ',CH_VRSN




      WRITE(IPL,'(2A)') ' Date: ','Date system call inactive'
      WRITE(IPL,'(2A,//)') ' Time: ','Time system call inactive'
!      WRITE(IWR,'(2A)') ' Date: ',CHDATE
!      WRITE(IWR,'(2A,//)') ' Time: ',CHTIME

!
!---  Write field data by node numbers
!     write flux data by surface numbers  ---
!
      WRITE(IPL,'(/,A)') '--- Field Variable Data by Node Numbers'
      WRITE(IPL,'(A)') '    Flux Variable Data by Surface Numbers  ---'
      WRITE(IPL,'(/,A,I9)') 'Number of Time Steps = ',NSTEP
      TMIN = TM/60.
      THR = TM/3600.
      TDAY = THR/24.
      TWK = TDAY/7.
      TYR = TDAY/365.25

      WRITE(IPL,'(A,6(1PE13.6,A))') 'Time = ',TM,',s ',TMIN,',min ', &
        THR,',h ',TDAY,',day ',TWK,',wk ',TYR,',yr '

      WRITE(IPL,'(/,A,I6)') 'Number of X or R-Direction Nodes = ',nxdim
      WRITE(IPL,'(A,I6)') 'Number of Y or Theta-Direction Nodes = ',nydim
      WRITE(IPL,'(A,I6)') 'Number of Z-Direction Nodes = ',nzdim
      VAR = 1.D+0
      INDX = 4
      IUNM = 1
      CALL RDUNIT(UNLN,VAR,INDX)
      IDB = INDEX( UNLN(1:),'  ') - 1
!
!---  Variable depth and thickness coordinate system  ---
!
      IF( ICS.EQ.3 .OR. ICS.EQ.11 .OR. ICS.EQ.12 .OR.ICS.EQ.8 ) THEN
        IF( ICS.EQ.3 ) THEN
          WRITE(IPL,'(3A,1PE16.9)') 'X Origin -- Hexahedra Points'
          WRITE(IPL,'(3A,1PE16.9)') 'Y Origin -- Hexahedra Points'
          WRITE(IPL,'(3A,1PE16.9)') 'Z Origin -- Hexahedra Points'
        ELSE
          WRITE(IPL,'(3A,1PE16.9)') 'X Origin -- Surface Positions'
          WRITE(IPL,'(3A,1PE16.9)') 'Y Origin -- Surface Positions'
          WRITE(IPL,'(3A,1PE16.9)') 'Z Origin -- Surface Positions'
        ENDIF
!!        WRITE(IPL,'(/,2A)') 'X-Direction Surface Positions, ', UNLN(1:IDB)
!        WRITE(IPL,FORM2) (((VAR*X(I,J,K),I=1,IFLD+1),J=1,JFLD+1), K=1,KFLD+1)
!!        WRITE(IPL,'(/,2A)') 'Y-Direction Surface Positions, ',UNLN(1:IDB)
!        WRITE(IPL,FORM2) (((VAR*Y(I,J,K),I=1,IFLD+1),J=1,JFLD+1),K=1,KFLD+1)
!!        WRITE(IPL,'(/,2A)') 'Z-Direction Surface Positions, ',UNLN(1:IDB)
!        WRITE(IPL,FORM2) (((VAR*Z(I,J,K),I=1,IFLD+1),J=1,JFLD+1), K=1,KFLD+1)        

        WRITE(IPL,'(/,2A)') 'X-Direction Node Positions, ',UNLN(1:IDB)
        WRITE(IPL,FORM2) (((xbf(I,J,K),I=1,nxdim+1),J=1,nydim+1),&
                                                         K=1,nzdim+1) !BH
        WRITE(IPL,'(/,2A)') 'Y-Direction Node Positions, ', UNLN(1:IDB)
        WRITE(IPL,FORM2) (((ybf(I,J,K),I=1,nxdim+1),J=1,nydim+1),&
                                                         K=1,nzdim+1) !BH
        WRITE(IPL,'(/,2A)') 'Z-Direction Node Positions, ', UNLN(1:IDB)
        WRITE(IPL,FORM2) (((zbf(I,J,K),I=1,nxdim+1),J=1,nydim+1),&
                                                         K=1,nzdim+1) !BH
        GOTO 20
      ENDIF
!      IF( ICS.EQ.2 ) THEN
!        WRITE(IPL,'(3A,1PE16.9)') 'X Origin -- Hexahedra Points'
!        WRITE(IPL,'(3A,1PE16.9)') 'Y Origin -- Hexahedra Points'
!        WRITE(IPL,'(3A,1PE16.9)') 'Z Origin -- Hexahedra Points'
!        WRITE(IPL,'(/,2A)') 'X-Direction Surface Positions, ', UNLN(1:IDB)
!        WRITE(IPL,FORM2) (((VAR*X(I,J,K)*COS(Y(I,J,K)), I=1,IFLD+1),J=1,JFLD+1),K=1,KFLD+1)
!        WRITE(IPL,'(/,2A)') 'Y-Direction Surface Positions, ', UNLN(1:IDB)
!        WRITE(IPL,FORM2) (((VAR*X(I,J,K)*SIN(Y(I,J,K)), &
!         I=1,IFLD+1),J=1,JFLD+1),K=1,KFLD+1)
!        WRITE(IPL,'(/,2A)') 'Z-Direction Surface Positions, ', UNLN(1:IDB)
!        WRITE(IPL,FORM2) (((VAR*Z(I,J,K),I=1,IFLD+1),J=1,JFLD+1), K=1,KFLD+1)        
!        GOTO 20
!      ENDIF
      if(ics == 2) then
       WRITE(IPL,'(3A,1PE16.9)') 'X Origin, ',UNLN(1:IDB), &
        ' = ',XMINI*cos(ymini)*VAR
       WRITE(IPL,'(3A,1PE16.9)') 'Y Origin, ',UNLN(1:IDB), &
        ' = ',YMINI*sin(ymini)*VAR
       WRITE(IPL,'(3A,1PE16.9)') 'Z Origin, ',UNLN(1:IDB), &
        ' = ',ZMINI*VAR
      else
       WRITE(IPL,'(3A,1PE16.9)') 'X Origin, ',UNLN(1:IDB), &
        ' = ',XMINI*VAR
       WRITE(IPL,'(3A,1PE16.9)') 'Y Origin, ',UNLN(1:IDB), &
        ' = ',YMINI*VAR
       WRITE(IPL,'(3A,1PE16.9)') 'Z Origin, ',UNLN(1:IDB), &
        ' = ',ZMINI*VAR
      endif
     endif

#endif

#ifndef USE_H5HUT
      ! Currently not included in parallel I/O
      ! Vicky said volume is not usually used.
      ! Also they actually get dumped with the data anyway except volume

      20 CONTINUE 
      ! Add the if condition to make sure all cores are doing the same
      ! Before this only proc 0 is working; after this, all procs are
      ! working - BH
      IF( ICS.NE.3 .AND. ICS.NE.11 .AND. ICS.NE.12 .AND.ICS.NE.8 ) THEN
      IDB = INDEX( UNLN(1:),'  ') - 1
      varname = 'X-Direction Node Positions'
      dim1 = 0
      dim2 = 0
      iflg = 0
      dflg = 1
      ldim = 1
      if(ics /= 2) then 
        call string2idx(ldim,iflg,dflg,'xcent',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNLN(1:IDB),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
      else
       varp_tmp = 0.d0
       do n=1,num_nodes
         varp_tmp(n) = XP(N)*COS(YP(N))
       enddo
       call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
       avar = 0.d0
       call write_var(varname,UNLN(1:IDB),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
      endif

      varname = 'Y-Direction Node Positions'
      VAR = 1.D+0
      IUNM = 1
      indx = 4
      CALL RDUNIT(UNLN,VAR,INDX)
      iflg = 0
      dflg = 1
      ldim = 1
      if(ics /= 2) then
       call string2idx(ldim,iflg,dflg,'ycent',idx,t_ok)
       avar = 0.d0
       call write_var(varname,UNLN(1:IDB),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
      else
       varp_tmp = 0.d0
       do n=1,num_nodes
         varp_tmp(n) = XP(N)*SIN(YP(N))
       enddo
       call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
       avar = 0.d0
       call write_var(varname,UNLN(1:IDB),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
      endif
      varname = 'Z-Direction Node Positions'
      VAR = 1.D+0
      IUNM = 1
      CALL RDUNIT(UNLN,VAR,INDX)
      iflg = 0
      dflg = 1
      ldim = 1
      call string2idx(ldim,iflg,dflg,'zcent',idx,t_ok)
      avar = 0.d0
      call write_var(varname,UNLN(1:IDB),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
#endif
      ENDIF ! BH
!   20 CONTINUE
      varname = "Node Volume"
      iunm = 1
      IDB = INDEX( UNLN(1:),'  ') - 1
      VAR = 1.D+0
      INDX = 4
      CALL RDUNIT(UNLN,VAR,INDX)
      iflg = 0
      dflg = 1
      ldim = 1
      call string2idx(ldim,iflg,dflg,'volume',idx,t_ok)
      var = var**3
      avar = 0.d0
      call write_var(varname,UNLN(1:IDB)//'^3',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
      DO 500 NV = 1,NVPLOT
        VAR = 1.D+0
        IPNV = IPLOT(NV)
        IPNVGC = IPLOTGC(NV)
        IF( IPNV.LE.16 ) THEN
          CALL WRPL_1( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.16 .AND. IPNV.LE.32 ) THEN
          CALL WRPL_2( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.32 .AND. IPNV.LE.48 ) THEN
          CALL WRPL_3( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.48 .AND. IPNV.LE.64 ) THEN
          CALL WRPL_4( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.64 .AND. IPNV.LE.80 ) THEN
          CALL WRPL_5( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.80 .AND. IPNV.LE.96 ) THEN
          CALL WRPL_6( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.96 .AND. IPNV.LE.112 ) THEN
          CALL WRPL_7( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.112 .AND. IPNV.LE.128 ) THEN
          CALL WRPL_8( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.128 .AND. IPNV.LE.144 ) THEN
          CALL WRPL_9( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.144 .AND. IPNV.LE.160 ) THEN
          CALL WRPL_10( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.160 .AND. IPNV.LE.176 ) THEN
          CALL WRPL_11( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.176 .AND. IPNV.LE.192 ) THEN
          CALL WRPL_12( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.192 .AND. IPNV.LE.208 ) THEN
          CALL WRPL_13( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.208 .AND. IPNV.LE.224 ) THEN
          CALL WRPL_14( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.224 .AND. IPNV.LE.240 ) THEN
          CALL WRPL_15( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.240 .AND. IPNV.LE.256 ) THEN
          CALL WRPL_16( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.256 .AND. IPNV.LE.272 ) THEN
          CALL WRPL_17( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.273 .AND. IPNV.LE.289 ) THEN
          CALL WRPL_18( FORM2,IPNV,IPNVGC )
        ELSEIF( IPNV.GT.340 .AND. IPNV.LE.400 ) THEN ! Copied from wrplot.F90 in estomp33 - Bryan
          CALL WRPL_20( FORM2,IPNV,IPNVGC )
        ENDIF
!
!---    Solute, conservation-component species, and
!       kinetic-component species plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.400 .AND. IPNV.LE.INDX ) THEN
          NSL = ((IPNV-400)/33) + 1
          IF( NSL.GT.NSOLU ) IUNMOL = 1
          IPNVX = MOD((IPNV-400),33)
          IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
          IF( IPNVX.EQ.1 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = SOLUT(NSL)(1:IDB)//' Concentration, '
            iflg = 0
            dflg = 1
            ldim = 2
            dim1 = nsl 
            call string2idx(ldim,iflg,dflg,'solute_conc',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)

!use global array
!            WRITE(IPL,FORM2) (VAR*C(N,NSL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.2 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = 'Aqueous '//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              varp_tmp(n) = c(nsl,n)*yl(nsl,n)/(sl(2,n)*pord(2,n)+small)
              if(islc(40).eq.1) then
!                if(nsl.gt.nsolu.and.immb(nsl-nsolu).ne.1) then !BH
                 if(nsl.gt.nsolu) then  !BH
                  neq = nsl-nsolu
                  if (immb(neq).ne.1) then  !BH
                   if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      if(nsp.le.nspl) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varx = varx*YL(NSL,N)/(SL(2,N)*PORD(2,N))
                    varp_tmp(n) = varx
                  endif
                 endif
                endif !BH
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0 
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!            WRITE(IPL,FORM2) ( VAR*C(N,NSL)*YL(N,NSL)/ &
!            (SL(2,N)*PORD(2,N)+SMALL),N=1,NFLD )
          ELSEIF( IPNVX.EQ.3 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            if(me.eq.0)WRITE(IPL,'(/,4A)') 'Gas ',SOLUT(NSL)(1:IDB), &
            ' Concentration,',UNPLOT(IPNV)
!            WRITE(IPL,FORM2) (VAR*C(N,NSL)*YG(N,NSL)/ &
!            (SG(2,N)*PORD(2,N)+SMALL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.4 ) THEN
          ELSEIF( IPNVX.EQ.5 ) THEN
            varname = 'Mole Fraction of '//SOLUT(NSL)(1:IDB)//' in the Aqueous Phase'
!            if(me.eq.0)WRITE(IPL,'(/,3A)') 'Mole Fraction of ',SOLUT(NSL)(1:IDB), &
!            ' in the Aqueous Phase, '
            iflg = 0
            dflg = 1
            ldim = 2
            dim1 = nsl
            call string2idx(ldim,iflg,dflg,'yl',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!            WRITE(IPL,FORM2) (YL(N,NSL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.6 ) THEN
            if(me.eq.0)WRITE(IPL,'(/,3A)') 'Mole Fraction of ',SOLUT(NSL)(1:IDB), &
            ' in the Gas Phase, '
!            WRITE(IPL,FORM2) (YG(N,NSL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.7 ) THEN
            varname = 'Solute Inventory of '//SOLUT(NSL)(1:IDB)
            var = 1.d0
            iflg = 0
            dflg = 1
            ldim = 2
            dim1 = nsl
            call string2idx(ldim,iflg,dflg,'yn',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!            WRITE(IPL,FORM2) (YN(N,NSL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.11 ) THEN
            varname = 'Source Integral of '//SOLUT(NSL)(1:IDB)
            INDX = 4
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            iflg = 0
            dflg = 1
            ldim = 2
            dim1 = nsl
            call string2idx(ldim,iflg,dflg,'srcic',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!            if(me.eq.0)WRITE(IPL,'(/,4A)') 'Source Integral of ',SOLUT(NSL)(1:IDB), &
!            ', ',UNPLOT(IPNV)
!            WRITE(IPL,FORM2) (VAR*SRCIC(N,NSL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.12 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            if(me.eq.0)WRITE(IPL,'(/,3A)') SOLUT(NSL)(1:IDB), &
            ' Total Concentration, ',UNPLOT(IPNV)
!            DO 252 N = 1,NFLD
!              DVAR(N) = YL(N,NSL)+YG(N,NSL)
!  252       CONTINUE
!            WRITE(IPL,FORM2) (VAR*DVAR(N),N=1,NFLD)
          ELSEIF( IPNVX.EQ.13 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = SOLUT(NSL)(1:IDB)//' Aqueous Concentration'
            iflg = 0
            dflg = 1
            ldim = 2
            dim1 = nsl
            call string2idx(ldim,iflg,dflg,'solute_conc',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!            WRITE(IPL,FORM2) (VAR*C(N,NSL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.14 ) THEN
          ELSEIF( IPNVX.EQ.15 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            if(me.eq.0)WRITE(IPL,'(/,3A)') SOLUT(NSL)(1:IDB), &
            ' Sorbed Concentration, ',UNPLOT(IPNV)
!            WRITE(IPL,FORM2) (VAR*YG(N,NSL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.22 ) THEN
          ELSEIF( IPNVX.EQ.23 ) THEN
            varname = SOLUT(NSL)(1:IDB)//' Solute Mass'

            VAR = 1.D+0
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varp_tmp = 0.d0
            do n=1,num_loc_nodes
              varp_tmp(n) = varp_tmp(n)+c(nsl,n)*vol(n)
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!              WRITE(IPL,FORM2) ((VAR*C(N,NSL)*VOL(N)),N=1,NFLD)
! solute/species per total volume
!soild
          ELSEIF( IPNVX.EQ.26 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = & 
             'Solid'//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              varp_tmp(n) = c(nsl,n)*(1.d0-yl(nsl,n)-yg(nsl,n)) 
              if(islc(40).eq.1) then
                if(nsl.gt.nsolu) then
                  neq = nsl-nsolu
                  if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      nspbx = nspl
                      nspex = nspbx+nsps
                      if(nsp.ge.nspbx .and. nsp.le.nspex) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varp_tmp(n) = varx
                  endif
                endif
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!exchanged
          ELSEIF( IPNVX.EQ.27 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = &
               'Exchange '//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              if(islc(40).eq.1) then
                if(nsl.gt.nsolu) then
                  neq = nsl-nsolu
                  if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      nspbx = nspl+nsps
                      nspex = nspbx+nspe
                      if(nsp.ge.nspbx .and. nsp.le.nspex) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varp_tmp(n) = varx
                  endif
                endif
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!gas
          ELSEIF( IPNVX.EQ.28 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = & 
             'Gas '//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              varp_tmp(n) = c(nsl,n)*yg(nsl,n)
              if(islc(40).eq.1) then
                if(nsl.gt.nsolu) then
                  neq = nsl-nsolu
                  if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      nspbx = nspl+nsps+nspe
                      nspex = nspbx+nspg
                      if(nsp.ge.nspbx .and. nsp.le.nspex) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varp_tmp(n) = varx
                  endif
                endif
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!napl
          ELSEIF( IPNVX.EQ.29 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = 'NAPL '//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              if(islc(40).eq.1) then
                if(nsl.gt.nsolu) then
                  neq = nsl-nsolu
                  if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      nspbx = nspl+nsps+nspg
                      nspex = nspbx+nspn
                      if(nsp.ge.nspbx .and. nsp.le.nspex) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varp_tmp(n) = varx
                  endif
                endif
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!soild
          ELSEIF( IPNVX.EQ.30 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = &
              'Solid Aqueous '//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              varp_tmp(n) = c(nsl,n)*(1.d0-yl(nsl,n)-yg(nsl,n))/(sl(2,n)*pord(2,n)+small)
              if(islc(40).eq.1) then
                if(nsl.gt.nsolu) then
                  neq = nsl-nsolu
                  if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      nspbx = nspl
                      nspex = nspbx+nsps
                      if(nsp.ge.nspbx .and. nsp.le.nspex) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varx = varx/(SL(2,N)*PORD(2,N))
                    varp_tmp(n) = varx
                  endif
                endif
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!exchange
          ELSEIF( IPNVX.EQ.31 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = &
              'Exchange Aqueous '//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              if(islc(40).eq.1) then
                if(nsl.gt.nsolu) then
                  neq = nsl-nsolu
                  if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      nspbx = nspl+nsps
                      nspex = nspbx+nspe
                      if(nsp.ge.nspbx .and. nsp.le.nspex) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varx = varx/(SL(2,N)*PORD(2,N))
                    varp_tmp(n) = varx
                  endif
                endif
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!gas
          ELSEIF( IPNVX.EQ.32 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = &
              'Gas Aqueous '//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              varp_tmp(n) = c(nsl,n)*yg(nsl,n)/(sl(2,n)*pord(2,n)+small)
              if(islc(40).eq.1) then
                if(nsl.gt.nsolu) then
                  neq = nsl-nsolu
                  if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      nspbx = nspl+nsps+nspe
                      nspex = nspbx+nspg
                      if(nsp.ge.nspbx .and. nsp.le.nspex) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varx = varx/(SL(2,N)*PORD(2,N))
                    varp_tmp(n) = varx
                  endif
                endif
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!napl
          ELSEIF( IPNVX.EQ.33 ) THEN
            INDX = 4
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            IF( NSL.GT.NSOLU ) VAR = VAR*1.D-3
            varname = 'NAPL Aqueous '//SOLUT(NSL)(1:IDB)//' Concentration'
            varp_tmp = 0.d0
            do n=1,num_nodes
              if(islc(40).eq.1) then
                if(nsl.gt.nsolu) then
                  neq = nsl-nsolu
                  if(neq<=neqc) then
                    varx = 0.d0
                    DO  M = 1,IEQ_C(1,NEQ)
                      NSP = IEQ_C(M+1,NEQ)
                      IF( ABS(SP_C(NSP,N)).LT.1.D-30 ) THEN
                        SP_CX = 0.D+0
                      ELSE
                        SP_CX = SP_C(NSP,N)
                      ENDIF
                      nspbx = nspl+nsps+nspg
                      nspex = nspbx+nspn
                      if(nsp.ge.nspbx .and. nsp.le.nspex) varx = varx + EQ_C(M,NEQ)*SP_CX
                    ENDDO
                    varx = varx/(SL(2,N)*PORD(2,N))
                    varp_tmp(n) = varx
                  endif
                endif
              endif
            enddo
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)


          ENDIF
        ENDIF
!
!---    Reactive species plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.INDX .AND.  &
        IPNV.LE.(INDX+NSPR*33) ) THEN
          NSL = ((IPNV-400)/33) + 1
          NSP = ((IPNV-INDX)/33) + 1
          IPNVX = MOD((IPNV-INDX),33)
          IF( NSP.GT.NSPL+NSPS+NSPE ) THEN
            IDB = INDEX( SPNMG(NSP-NSPL-NSPS)(1:),'  ') - 1
            SPNMX = SPNMS(NSP-NSPL-NSPS)
          ELSEIF( NSP.GT.NSPL+NSPS ) THEN
            IDB = INDEX( SPNME(NSP-NSPL-NSPS)(1:),'  ') - 1
            SPNMX = SPNME(NSP-NSPL-NSPS)
          ELSEIF( NSP.GT.NSPL ) THEN
            IDB = INDEX( SPNMS(NSP-NSPL)(1:),'  ') - 1
            SPNMX = SPNMS(NSP-NSPL)
          ELSE
            IDB = INDEX( SPNML(NSP)(1:),'  ') - 1
            SPNMX = SPNML(NSP)
          ENDIF
          IF( IPNVX.EQ.1 ) THEN
            INDX = 4
            IUNMOL = 1
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            varname = 'Volumetric '//SPNMX(1:IDB)//' Concentration'

            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
              NSP_M = NSP-NSPL
              IF( ISP_MN(NSP).EQ.1 ) THEN
                do n=1,num_nodes
                  varx =(sp_c(nsp,n)+sp_cmn(nsp_m,n)) 
                  varp_tmp(n) = varx
                enddo
              ELSE
                do n=1,num_nodes
                  varx =(sp_c(nsp,n)) 
                  varp_tmp(n) = varx
                enddo
              ENDIF
            ELSE
              do n=1,num_nodes
                varx =(sp_c(nsp,n)) 
                varp_tmp(n) = varx
              enddo
            ENDIF
            iflg = 0
            dflg = 1
            ldim = 1
            dim1 = nsl
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
!              NSP_M = NSP-NSPL
!              IF( ISP_MN(NSP).EQ.1 ) THEN
!                WRITE(IPL,FORM2) ( VAR*(SP_C(N,NSP)+ &
!                SP_CMN(N,NSP_M)),N=1,NFLD )
!              ELSE
!                WRITE(IPL,FORM2) ( VAR*SP_C(N,NSP),N=1,NFLD )
!              ENDIF
!            ELSE
!              WRITE(IPL,FORM2) ( VAR*SP_C(N,NSP),N=1,NFLD )
!            ENDIF
          ELSEIF( IPNVX.EQ.2 ) THEN
            INDX = 4
            IUNM = -3
            IUNMOL = 1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            varname = 'Aqueous '//SPNMX(1:IDB)//' Concentration'
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
              varp_tmp = 0.d0
              NSP_M = NSP-NSPL
              IF( ISP_MN(NSP).EQ.1 ) THEN
                do n=1,num_nodes
                  varx =sp_c(nsp,n)+sp_cmn(nsp_m,n)/ &
                    (sl(2,n)*pord(2,n)+small)
                  varp_tmp(n) = varx
                enddo
              ELSE
                do n=1,num_nodes
                  varx =sp_c(nsp,n)/(sl(2,n)*pord(2,n)+small)
                  varp_tmp(n) = varx
                enddo
              ENDIF
            ELSE
              do n=1,num_nodes
                varx =sp_c(nsp,n)/(sl(2,n)*pord(2,n)+small)
                varp_tmp(n) = varx
              enddo
            ENDIF
            iflg = 0
            dflg = 1
            ldim = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0
            call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!            INDX = 4
!            IUNMOL = 1
!            IUNM = -3
!            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
!            VAR = VAR*1.D-3
!            if(me.eq.0)WRITE(IPL,'(/,4A)') 'Aqueous ',SPNMX(1:IDB), &
!            ' Concentration, ',UNPLOT(IPNV)
!            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
!              NSP_M = NSP-NSPL
!              IF( ISP_MN(NSP).EQ.1 ) THEN
!                WRITE(IPL,FORM2) ( VAR*(SP_C(N,NSP)+SP_CMN(N,NSP_M))/ &
!                (SL(2,N)*PORD(2,N)+SMALL),N=1,NFLD )
!              ELSE
!                WRITE(IPL,FORM2) ( VAR*SP_C(N,NSP)/ &
!                (SL(2,N)*PORD(2,N)+SMALL),N=1,NFLD )
!              ENDIF
!            ELSE
!              WRITE(IPL,FORM2) ( VAR*SP_C(N,NSP)/ &
!              (SL(2,N)*PORD(2,N)+SMALL),N=1,NFLD )
!            ENDIF
          ELSEIF( IPNVX.EQ.3 ) THEN
            INDX = 4
            IUNMOL = 1
            IUNM = -3
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            if(me.eq.0)WRITE(IPL,'(/,4A)') 'Gas ',SPNMX(1:IDB), &
            ' Concentration,',UNPLOT(IPNV)
!            WRITE(IPL,FORM2) (VAR*SP_C(N,NSP)/ &
!            (SG(2,N)*PORD(2,N)+SMALL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.4 ) THEN
          ELSEIF( IPNVX.EQ.11 ) THEN
            INDX = 4
            IUNMOL = 1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            if(me.eq.0)WRITE(IPL,'(/,4A)') 'Source Integral of ',SPNMX(1:IDB), &
            ', ',UNPLOT(IPNV)
!            WRITE(IPL,FORM2) (VAR*SRCIC(N,NSL),N=1,NFLD)
          ELSEIF( IPNVX.EQ.24 ) THEN
            INDX = 4
            IUNM = 2
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            varname = SPNMX(1:IDB)//' Area'
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
              NSP_M = NSP-NSPL
              do n=1,num_nodes
                varx =(sp_area(nsp_m,n)*var)
                varp_tmp(n) = varx
              enddo
              iflg = 0
              dflg = 1
              ldim = 1
              call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
              avar = 0.d0
              call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!             WRITE(IPL,FORM2) ( VAR*(SP_AREA(N,NSP_M)),N=1,NFLD )
            ENDIF
          ELSEIF( IPNVX.EQ.25 ) THEN
            INDX = 4
            IUNM = 0
            IUNMOL = 1
            IUNS = -1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            VAR = VAR*1.D-3
            varname = SPNMX(1:IDB)//' Rate'
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
              NSP_M = NSP-NSPL
              do n=1,num_nodes
                varx =(sp_rate(nsp_m,n)*var)
                varp_tmp(n) = varx
              enddo
              iflg = 0
              dflg = 1
              ldim = 1
              call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
              avar = 0.d0
              call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!             WRITE(IPL,FORM2) ( VAR*(SP_RATE(N,NSP_M)),N=1,NFLD )
              ELSE
            ENDIF
          ELSEIF( IPNVX.EQ.26 ) THEN
            varname = SPNMX(1:IDB)//' Volume Fraction, '//  &
             UNPLOT(IPNV)
!print *,'nspl---',nsp,nspl
            IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
              NSP_M = NSP-NSPL
              do n=1,num_nodes
                varx = rs_s(3,nsp_m,n)
                varp_tmp(n) = varx
              enddo
              iflg = 0
              dflg = 1
              ldim = 1
              call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
              avar = 0.d0
              call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
!              WRITE(IPL,FORM2) ( VAR*(RS_S(3,NSP_M,N)),N=1,NFLD )
            ENDIF
          ELSEIF( IPNVX.EQ.27 ) THEN
            varname = ' pH, '// UNPLOT(IPNV)
              iflg = 0
              dflg = 1
              ldim = 1
              call string2idx(ldim,iflg,dflg,'ph',idx,t_ok)
              avar = 0.d0
              call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form2,0)
          ENDIF
        ENDIF
  500 CONTINUE
!
!---  X-Dir. Velocity or Flux Variables  ---
!
      DO 700 NV = 1,NVPLOT
        VAR = 1.D+0
        IPNV = IPLOT(NV)
        IF( IPNV.EQ.114 ) THEN
        ELSEIF( IPNV.EQ.120 ) THEN
        ENDIF
!
!---    Solute plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.400 .AND. IPNV.LE.INDX ) THEN
          NSL = ((IPNV-400)/33) + 1
          IPNVX = MOD((IPNV-400),33)
          IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
          IF( IPNVX.EQ.8 ) THEN
          ELSEIF( IPNVX.EQ.16 ) THEN
          ELSEIF( IPNVX.EQ.19 ) THEN
          ENDIF
        ENDIF
  700 CONTINUE
!
!---  Y-Dir. Velocity or Flux Variables  ---
!
      DO 800 NV = 1,NVPLOT
        VAR = 1.D+0
        IPNV = IPLOT(NV)
        IF( IPNV.EQ.115 ) THEN
        ELSEIF( IPNV.EQ.121 ) THEN
        ENDIF
!
!---    Solute plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.400 .AND. IPNV.LE.INDX ) THEN
        ENDIF
  800 CONTINUE
!
!---  Z-Dir. Velocity or Flux Variables  ---
!
      DO 900 NV = 1,NVPLOT
        VAR = 1.D+0
        IPNV = IPLOT(NV)
        IF( IPNV.EQ.116 ) THEN
        ELSEIF( IPNV.EQ.122 ) THEN
        ENDIF
!
!---    Solute plot output ---
!
        INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)
        IF( IPNV.GT.400 .AND. IPNV.LE.INDX ) THEN
          NSL = ((IPNV-400)/33) + 1
          IPNVX = MOD((IPNV-400),33)
          IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
          IF( IPNVX.EQ.10 ) THEN
          ELSEIF( IPNVX.EQ.18 ) THEN
          ELSEIF( IPNVX.EQ.21 ) THEN
          ENDIF
        ENDIF
  900 CONTINUE

!
!---  Close plot file  ---
!

#ifdef USE_H5HUT
     if (sio_close() .ne. 0) then
        write(*,*) "Warning: Unable to close HDF file"
     endif
#else
      if(me.eq.0)CLOSE( UNIT=IPL )
#endif

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRPLOT group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_1( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     1                Aqueous pressure
!     2                Gas pressure
!     3    IOM == 3    Surface vapor pressure
!     3                NAPL pressure
!     4                Temperature
!     5                Phase condition
!     6                Aqueous gauge pressure
!     7                Gas gauge pressure
!     8                NAPL gauge pressure
!     9                Apparent aqueous saturation
!     10               Apparent total-liquid saturation
!     11               Aqueous saturation
!     12               Gas saturation
!     13   IOM == 3    Surface temperature
!     13               NAPL saturation
!     14               Total-liquid saturation
!     15               Aqueous moisture content
!     16   IOM == 32   Aqueous CO2 mole fraction
!     16   IOM == 33   Aqueous CO2 mole fraction
!     16   IOM == 34   Aqueous CO2 mole fraction
!     16               NAPL moisture content
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 January 2003.
!     Last Modified by MD White, PNNL, 1 January 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FILES
      USE FDVP
      USE CONST
      USE PORMED
      use grid_mod
      USE SIO
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
      CHARACTER*16 FORM
      INTEGER LDIM, DIM1,DIM2,IDX,IFLG,DFLG
      integer isio ! status from io calls
      character*64 varname
      logical t_ok



      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DVAR
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
       '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.1 ) THEN
        varname = 'Aqueous Pressure'
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'pressure_w',idx,t_ok)
        avar = patm
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)

      ELSEIF( IPNV.EQ.2 ) THEN
        varname(1:) = 'Gas Pressure'
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'pressure_g',idx,t_ok)
        avar = patm
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.3 ) THEN
      ELSEIF( IPNV.EQ.4 ) THEN
        varname = 'Temperature'
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        IF( UNPLOT(IPNV).EQ.'c' ) THEN
          var = 1.d0
          avar = 0.d0
        ELSEIF( UNPLOT(IPNV).EQ.'k' ) THEN
          var = 1.d0
          avar = 273.15d0 
        ELSEIF( UNPLOT(IPNV).EQ.'f' ) THEN
          var = 1.8d0
          avar = 32.d0 
        ELSEIF( UNPLOT(IPNV).EQ.'r' ) THEN
          var = 1.8d0
          avar = 492.d0 
        ENDIF
        call string2idx(ldim,iflg,dflg,'temperature',idx,t_ok)
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.5 ) THEN
        varname = 'Phase Condition'
        iflg = 0
        dflg = 1 
        ldim = 1
        dim1 = 1
        varp_tmp = 0.d0
        do n=1,num_nodes
          varp_tmp(n) = dble( nphaz(2,n) )
          if( ixp(n).eq.0 )varp_tmp(n) = 0.D+0
        enddo
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.6 ) THEN
        varname = 'Aqueous Gauge Pressure'
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'pressure_w',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.7 ) THEN
        varname = 'Gas Gauge Pressure'
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'pressure_g',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.9 ) THEN
        varname = 'Apparent Aqueous Saturation'
        iflg = 0
        dflg = 1
        ldim = 1
        dim1 = 1
        call string2idx(ldim,iflg,dflg,'asl',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.11 ) THEN
        varname = 'Aqueous Saturation'
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'saturation_w',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.12 ) THEN
         varname = 'Gas Saturation'
         iflg = 0
         dflg = 1
         ldim = 2
         dim1 = 2
         call string2idx(ldim,iflg,dflg,'saturation_g',idx,t_ok)
         avar = 0.d0
         call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.15 ) THEN
         varname = 'Aqueous Moisture Content'
         varp_tmp = 0.d0
         do n=1,num_nodes
              varp_tmp(n) = SL(2,N)*PORD(2,N)
         enddo
         iflg = 0
         dflg = 1
         ldim = 1
         dim1 = 1
         call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
         avar = 0.d0
         call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.16 ) THEN
         varname = 'Gravimetric Moisture Content'
         varp_tmp = 0.d0
         do n=1,num_nodes
              varp_tmp(n) =(SL(2,N)*PORD(2,N))/((RHOS(N))*(1-PORD(2,N)))/RHOL(2,N)
         enddo
         iflg = 0
         dflg = 1
         ldim = 1
         dim1 = 1
         call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
         avar = 0.d0
         call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ENDIF

!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRPL_1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_2( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     17   IOM == 32   Aqueous NaCl mole fraction
!     17   IOM == 33   Aqueous NaCl mole fraction
!     17   IOM == 34   Aqueous NaCl mole fraction
!     17               Total-liquid moisture content
!     18               Effective trapped-NAPL saturation
!     19               Effective trapped-gas saturation
!     20               Diffusive porosity
!     21               Gas water mass fraction
!     22               Gas air mass fraction
!     22   IOM == 30   Gas gas component mass fraction
!     22   IOM == 32   Gas CO2 mass fraction
!     22   IOM == 33   Gas CO2 mass fraction
!     22   IOM == 34   Gas CO2 mass fraction
!     22   IOM == 36   Gas CO2 mass fraction
!     23               Gas oil mass fraction
!     23   IOM == 36   Gas CH4 mass fraction
!     24               Aqueous water mass fraction
!     25               Aqueous air mass fraction
!     26               Aqueous oil mass fraction
!     27               Aqueous Hydraulic Head (Fresh Water)
!     28               Gas Hydraulic Head (Fresh Water)
!     29               NAPL Hydraulic Head (Fresh Water)
!     30               Rock/soil type
!     31               Aqueous relative permeability
!     32   IOM == 4    Residual NAPL saturation
!     32               Gas relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 January 2003.
!     Last Modified by MD White, PNNL, 1 January 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FILES
      USE FDVP
      USE CONST
      use grid_mod
      USE SIO
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
      CHARACTER*16 FORM
      integer ldim, dim1,dim2,idx,iflg,dflg
      character*64 varname
      logical t_ok



      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DVAR,VAR1,VAR2,VAR3
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_2'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
      '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.20 ) THEN
        varname = 'Diffusive Porosity'
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2 
        call string2idx(ldim,iflg,dflg,'pord',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
     ELSEIF( IPNV.EQ.24 ) THEN
        varname = 'Aqueous Water Mass Fraction'
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'xlw',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.27 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varname = 'Aqueous Hydraulic Head '//'(Fresh Water)'
        varp_tmp = 0.d0
        do n=1,num_nodes
              varp_tmp(n) = PL(2,N)/RHORL/GRAV + ZP(N)
        enddo
        iflg = 0
        dflg = 1
        ldim = 1
        dim1 = 1
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0 
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.28 ) THEN
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        if(me.eq.0)WRITE(IPL,'(/,2A)') 'Gas Hydraulic Head ' // &
        '(Fresh Water),',UNPLOT(IPNV)
!        WRITE(IPL,FORM) &
!        (VAR*(PG(2,N)/RHORL/GRAV + ZP(N)),N=1,NFLD)
      ELSEIF( IPNV.EQ.30 ) THEN
        varname = 'Rock/Soil Type'
        iflg = 0
        dflg = 1 
        ldim = 1
        dim1 = 1
        varp_tmp = 0.d0
        do n=1,num_nodes
          varp_tmp(n) = dble( iz(n) )
        enddo
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.31 ) THEN
        varname = 'Aqueous Relative Permeability'
        varp_tmp = 0.d0
        do n=1,num_nodes
          varp_tmp(n) = (rkl(1,2,n)*rkl(2,2,n)*rkl(3,2,n))**(1./3.)
        enddo
        iflg = 0
        dflg = 1
        ldim = 1
        dim1 = 1
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ENDIF
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRPL_2 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_3( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     33               NAPL relative permeability
!     34               Aqueous density
!     35               Gas density
!     36               NAPL density
!     37               Total water mass
!     38               Total air mass
!     38    IOM=4      Mobile-NAPL saturation
!     38    IOM=30     Total gas component mass
!     38    IOM=32     Total CO2 mass
!     38    IOM=33     Total CO2 mass
!     38    IOM=34     Total CO2 mass
!     38    IOM=36     Total CO2 mass
!     39               Total oil mass
!     39    IOM=36     Total CH4 mass
!     40               Water mass source integral
!     41               Air mass source integral
!     41    IOM=32     CO2 mass source integral
!     41    IOM=33     CO2 mass source integral
!     41    IOM=34     CO2 mass source integral
!     41    IOM=36     CH4 mass source integral
!     42               Oil mass source integral
!     42    IOM=36     CH4 mass source integral
!     43               Thermal energy source integral
!     44               X-Dir. thermal conductivity
!     45               Y-Dir. thermal conductivity
!     46               Z-Dir. thermal conductivity
!     47               Salt volumetric concentration
!     48               Salt aqueous concentration
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 January 2003.
!     Last Modified by MD White, PNNL, 1 January 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
      USE GRID_MOD
      USE FILES
      USE FDVP
      USE FDVH
      USE CONST
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
      CHARACTER*16 FORM
      CHARACTER*64 varname
      integer ldim, dim1,dim2,idx,iflg,dflg
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DVAR
      logical t_ok
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_3'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
      '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.33 ) THEN
      ELSEIF( IPNV.EQ.34 ) THEN
        varname = 'Aqueous Density'
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'rhol',idx,t_ok)
        avar = 0.0d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.35 ) THEN
        INDX = 4
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') &
        'Gas Density, ',UNPLOT(IPNV)
!        WRITE(IPL,FORM) (VAR*RHOG(2,N),N=1,NFLD)
      ELSEIF( IPNV.EQ.37 ) THEN
        varname = 'Total Water Mass'
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        do n=1,num_nodes
          varp_tmp(n) = 0.D+0
          varp_tmp(n) = varp_tmp(n) + &
            VAR*PORD(2,N)*VOL(N)*XLW(2,N)*SL(2,N)*RHOL(2,N)
        enddo
        iflg = 0
        dflg = 1
        ldim = 1
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.40 ) THEN
        INDX = 4
        IUNKG = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') 'Water Mass Source Integral, ', &
        UNPLOT(IPNV)
!        WRITE(IPL,FORM) (VAR*SRCIW(N),N=1,NFLD)
      ENDIF
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRPL_3 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_4( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     49               Maximum local aqueous Courant number
!     50               Total salt mass
!     51               X-Dir. aqueous Darcy velocity
!     52               Y-Dir. aqueous Darcy velocity
!     53               Z-Dir. aqueous Darcy velocity
!     54               X-Dir. gas Darcy velocity
!     55               Y-Dir. gas Darcy velocity
!     56               Z-Dir. gas Darcy velocity
!     57               X-Dir. NAPL Darcy velocity
!     58               Y-Dir. NAPL Darcy velocity
!     59               Z-Dir. NAPL Darcy velocity
!     60               X-Dir. heat flux
!     61               Y-Dir. heat flux
!     62               Z-Dir. heat flux
!     63               Matric potential head
!     64               X-Dir. salt flux
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 January 2003.
!     Last Modified by MD White, PNNL, 1 January 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE OUTPU
      USE GRID
      USE GRID_MOD
      USE FLUXP
      USE FILES
      USE FDVP
      USE CONST
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
      CHARACTER*16 FORM
      character*64 varname
      integer ldim, dim1,dim2,idx,iflg,dflg
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DVAR
      logical t_ok
      LOGICAL :: use_ga

!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_4'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.49 ) THEN
        varname = 'Maximum Local Aqueous Courant Number '
        iflg = 0
        dflg = 1
        ldim = 1
        dim1 = 1
        call string2idx(ldim,iflg,dflg,'crntl',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.51 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') &
        'X-Dir. Aqueous Darcy Velocity, ',UNPLOT(IPNV)
!        NX = (IFLD+1)*JFLD*KFLD
!        WRITE(IPL,FORM) (VAR*UL(1,N),N=1,NX)
      ELSEIF( IPNV.EQ.52 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') &
        'Y-Dir. Aqueous Darcy Velocity, ',UNPLOT(IPNV)
!        NY = IFLD*(JFLD+1)*KFLD
!        WRITE(IPL,FORM) (VAR*VL(1,N),N=1,NY)
      ELSEIF( IPNV.EQ.53 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        WRITE(IPL,'(/,2A)') &
        'Z-Dir. Aqueous Darcy Velocity, ',UNPLOT(IPNV)
!        NZ = IFLD*JFLD*(KFLD+1)
!        WRITE(IPL,FORM) (VAR*WL(1,N),N=1,NZ)
      ELSEIF( IPNV.EQ.63 ) THEN
        varname = 'Matric Potential Head'
        INDX = 4
        IUNM = 1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varp_tmp = 0.d0
        do n=1,num_nodes
          if(ixp(n) <= 0) cycle
          varp_tmp(n) = MIN(PL(2,N)-PG(2,N),0.D+0)/RHOL(2,N)/GRAV
        enddo
        iflg = 0
        dflg = 1
        ldim = 1
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ENDIF

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRPL_4 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_5( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     65               Y-Dir. salt flux
!     66               Z-Dir. salt flux
!     67               X-Dir. salt flux (node centered)
!     68               Y-Dir. salt flux (node centered)
!     69               Z-Dir. salt flux (node centered)
!     70               Gas water mole fraction
!     71               Gas air mole fraction
!     71    IOM=30     Gas gas component mole fraction
!     72               Gas oil mole fraction
!     73               Gas water concentration
!     74               Gas air concentration
!     74    IOM=30     Gas gas component concentration
!     75               Gas oil concentration
!     76               Aqueous water concentration
!     77               Aqueous air concentration
!     77    IOM=30     Aqueous gas concentration
!     78               Aqueous oil concentration
!     79               Gas Courant number
!     80               Ice pressure
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 January 2003.
!     Last Modified by MD White, PNNL, 8 January 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE OUTPU
      USE GRID
      USE FILES
      USE FDVP
      USE CONST
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
      CHARACTER*16 FORM
      INTEGER LDIM, DIM1,DIM2,IDX,IFLG,DFLG
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_5'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      SUBNM = SUBNM(1:ICSN)


!
!---  End of WRPL_5 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_6( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     81	ice saturation 'SI '
!     82	ice density 'RHOF'
!     83	aqueous matrix 'DSLM'
!     84	aqueous fracture 'DSLF'
!     85	gas matrix 'DSGM'
!     86	gas fracture 'DSGF'
!     87	xnc aqueous volumetric flux 'ULNC'
!     88	ync aqueous volumetric flux 'VLNC'
!     89	znc aqueous volumetric flux 'WLNC'
!     90	xnc gas volumetric flux 'UGNC'
!     91	ync gas volumetric flux 'VGNC'
!     92	znc gas volumetric flux 'WGNC'
!     93	xnc NAPL volumetric flux 'UNNC'
!     94	ync NAPL volumetric flux 'VNNC'
!     95	znc NAPL volumetric flux 'WNNC'
!     96	xnc heat flux 'UQNC'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE OUTPU
      USE GRID
      USE FLUXP
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
      CHARACTER*16 FORM
      logical t_ok
      integer idx,ldim, dim1,dim2, iflg, dflg
      character*64 t_string
      character*64 varname
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_6'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.81 ) THEN
      ELSEIF( IPNV.EQ.87 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varname = 'X-Dir. Aqueous Darcy Velocity '//'(Node Centered)'
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 1
        call string2idx(ldim,iflg,dflg,'velocity_c',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)

!        WRITE(IPL,FORM) &
!        (VAR*0.5D+0*(UL(1,NSX(N))+UL(1,NSX(N)+1)),N=1,NFLD)
      ELSEIF( IPNV.EQ.88 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varname = 'Y-Dir. Aqueous Darcy Velocity '//'(Node Centered)'
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'velocity_c',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
!        WRITE(IPL,FORM) &
!        (VAR*0.5D+0*(VL(1,NSY(N))+VL(1,NSY(N)+IFLD)),N=1,NFLD)
      ELSEIF( IPNV.EQ.89 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varname = 'Z-Dir. Aqueous Darcy Velocity '//'(Node Centered)'
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 3
        call string2idx(ldim,iflg,dflg,'velocity_c',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
!        WRITE(IPL,FORM) &
!        (VAR*0.5D+0*(WL(1,NSZ(N))+WL(1,NSZ(N)+IJFLD)),N=1,NFLD)
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRPL_6 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_7( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     97	ync heat flux 'VQNC'
!     98	znc heat flux 'WQNC'
!     99	NAPL courant 'CRNN'
!     101	osmotic pressure 'POSM'
!     102	osmotic efficiency factor 'OEC '
!     103	aqueous alcohol concentration 'CLA '
!     104	NAPL alcohol concentration 'CNA '
!     105	trapped gas saturation 'SGT '
!     106	trapped NAPL saturation 'SNT '
!     107	aqueous trapped gas 'SGTL'
!     108	NAPL trapped gas 'SGTN'
!     109	dissolved-aqueous oil concentration 'CLO '
!     110	salt aqueous mass fraction 'XLS '
!     111	surfactant volumetric concentration 'CS '
!     112	surfactant aqueous concentration 'CLS '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
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
      CHARACTER*16 FORM
      INTEGER LDIM, DIM1,DIM2,IDX,IFLG,DFLG
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_7'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) =  &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)


!---  End of WRPL_7 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_8( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     113	surfactant aqueous mass fraction 'XLS '
!     114	x surfactant flux ' US '
!     115	y surfactant flux ' VS '
!     116	z surfactant flux ' WS '
!     117	xnc surfactant flux 'USNC'
!     118	ync surfactant flux 'VSNC'
!     119	znc surfactant flux 'WSNC'
!     120	x dissolved-oil flux 'ULO '
!     121	y dissolved-oil flux 'VLO '
!     122	z dissolved-oil flux 'WLO '
!     123	xnc dissolved-oil flux 'ULOC'
!     124	ync dissolved-oil flux 'VLOC'
!     125	znc dissolved-oil flux 'WLOC'
!     126	NAPL-aqueous trapping number 'TPNL'
!     127	minimum effect aqueous saturation 'ESLM'
!     128	water vapor partial pressure 'PVW '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FLUXD
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
      CHARACTER*16 FORM
      INTEGER LDIM, DIM1,DIM2,IDX,IFLG,DFLG
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_8'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!---  End of WRPL_8 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_9( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     129	air partial pressure 'PVA '
!     130	oil vapor partial pressure 'PVO '
!     131	gas-aqueous scaling factor 'BGL '
!     132	free-NAPL aqueous interfacial area 'ANFL'
!     133	trapped-NAPL aqueous interfacial area 'ANTL'
!     134	aqueous solute coefficient 'HKL '
!     135	free-NAPL solute coefficient 'HKNF'
!     136	trapped-NAPL solute coefficient 'HKNT'
!     137	undefined
!     138	undefined
!     139	undefined
!     140	water mass source rate 'SRCW'
!     141	air mass source rate 'SRCA'
!     142	oil mass source rate 'SRCO'
!     143	energy source rate 'SRCQ'
!     144	aqueous well depth 'PLWB'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOURC
      USE SOLTN
      USE OUTPU
      USE GRID
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
      character*64 varname
      CHARACTER*16 FORM
      integer idx,ldim, dim1,dim2, iflg, dflg
      logical t_ok
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_9'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
       '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.140 ) THEN
        varname = 'Water Mass Source Rate '
        INDX = 4
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'srcw',idx,t_ok)
        avar = 0.0d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!
!---  End of WRPL_9 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_10( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     145	well flow rate 'QLW '
!     146	well flow integral 'QLWI'
!     147	salt mass source rate 'SRCS'
!     148	salt mass source integral 'SRIS'
!     149	scanning path 'PATH'
!     150	aqueous air or co2 saturation 'DAPS'
!     151	bubble void fraction 'BVF '
!     152	bubble air mass fraction 'XBA '
!     153	mineralized co2 'MCO2 '
!     154	napl well flow rate 'QNW '
!     155	napl well flow integral 'QNWI'
!     156	total well flow rate 'QTW '
!     157	total well flow integral 'QTWI'
!     158	undefined
!     159	undefined
!     160	undefined
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOURC
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE GRID_MOD
      USE FILES
      USE FDVP
      USE CONST
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
      CHARACTER*16 FORM
      CHARACTER*64 varname
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DVAR
      integer ldim, dim1,dim2,idx,iflg,dflg
      logical t_ok
      LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_10'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.149 ) THEN
        varname = 'Scanning Path (-1=Drying, 1=Wetting) '
        do n=1,num_nodes
          varp_tmp(n) = DBLE(IPH(2,N))
          if(ixp(n).eq.0)varp_tmp(n) = 0.D+0
        enddo
        iflg = 0
        dflg = 1
        ldim = 1
        dim1 = 1
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ENDIF

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!---  End of WRPL_10 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_11( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     161	NAPL dissolved water concentration 'CNW '
!     162	NAPL dissolved water mole fraction 'XMNW'
!     163	NAPL dissolved water mass fraction 'XNW '
!     164	NAPL dissolved oil concentration 'CNO '
!     165	NAPL dissolved oil mole fraction 'XMNO'
!     166	NAPL dissolved oil mass fraction 'XNO '
!     167	total alcohol mass 'TMA '
!     168	aqueous dissolved water mass fraction 'XLW '
!     169	alcohol mass source integral 'SRIA'
!     170	alcohol mass source rate 'SRCA'
!     171	integrated NAPL and aqueous dissolved alcohol 'IMA '
!     172	integrated aqueous dissolved alcohol 'IMLA'
!     173	integrated NAPL dissolved water 'IMNW'
!     174	integrated NAPL dissolved oil 'IMNO'
!     175	integrated NAPL dissolved alcohol 'IMNA'
!     176	aqueous viscosity 'VISL'
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOURC
      USE SOLTN
      USE OUTPU
      USE GRID
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




      character*64 varname
      CHARACTER*16 FORM
      integer ldim, dim1,dim2,idx,iflg,dflg
      integer isio ! status from io calls
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DVAR
      logical t_ok
       LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_11'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
       '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.176 ) THEN
        varname = 'Aqueous Viscosity'
        INDX = 4
        IUNM = -1
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'visl',idx,t_ok)
        avar = 0.0d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ENDIF

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!
!---  End of WRPL_11 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_12( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     177	monitoring well water depth or total-liquid well depth 'WDT '
!     178	aqueous well depth 'WDL '
!     179	NAPL well depth 'WDN '
!     180	monitoring well pressure ' PW '
!     181	monitoring well aqueous saturation 'SLW '
!     182	monitoring well water-vapor mass fraction or well NAPL saturation 'XGWW'
!     183	monitoring well dissolved-air mass fraction or dissolved-oil mass fraction 'XLAW'
!     184	monitoring well axial aqueous flux or well total-liquid pumping rate 'UL_W'
!     185	monitoring well axial gas flux or well aqueous pumping rate 'UG_W'
!     186	monitoring well vertical aqueous flux or well NAPL pumping rate 'WL_W'
!     187	monitoring well vertical gas flux or well total-liquid pumping integral 'WG_W'
!     188	integrated well aqueous pumping 'IPLW'
!     189	integrated mineral CO2 or well NAPL pumping integral 'IPNW'
!     190	integrated trapped gas air 'IMGT'
!     191	integrated water mass 'IMW '
!     192	integrated air mass 'IMA '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE OUTPU
      USE GRID
      USE FILES
      USE FDVP
      USE CONST
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




      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DVAR

      CHARACTER*16 FORM
      INTEGER LDIM, DIM1,DIM2,IDX,IFLG,DFLG
         LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_12'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
       '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)


!---  End of WRPL_12 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_13( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     193	integrated oil mass 'IMO '
!     194	integrated aqueous water 'IMLW'
!     195	integrated aqueous air 'IMLA'
!     196	integrated aqueous oil 'IMLO'
!     197	integrated gas water 'IMGW'
!     198	integrated gas air 'IMGA'
!     199	integrated gas oil 'IMGO'
!     200	reserved to control plot file output
!     201	x aqueous relative permeability 'RKLX'
!     202	y aqueous relative permeability 'RKLY'
!     203	z aqueous relative permeability 'RKLZ'
!     204	aqueous co2 mole fraction 'XMLA'
!     205   aqueous salt mole fraction 'XMLS'
!     206   atmospheric temperature ' TA '
!     207   atmospheric relative humidity ' RH '
!     208   atmospheric solar radiation ' RN '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE GRID
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
      CHARACTER*16 FORM
      character*64 varname
      integer ldim, dim1,dim2,idx,iflg,dflg
      logical t_ok
       LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_13'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.201 ) THEN
        varname = 'X-Aqueous Relative Permeability'
        iflg = 0
        dflg = 1
        ldim = 3
        dim1 = 1
        dim2 = 2
        call string2idx(ldim,iflg,dflg,'rkl',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
!        WRITE(IPL,FORM) (RKL(1,2,N),N=1,NFLD)
      ELSEIF ( IPNV.EQ.202 ) THEN
        varname = 'Y-Aqueous Relative Permeability'
        iflg = 0
        dflg = 1
        ldim = 3
        dim1 = 2
        dim2 = 2
        call string2idx(ldim,iflg,dflg,'rkl',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
!        WRITE(IPL,FORM) (RKL(2,2,N),N=1,NFLD)
      ELSEIF ( IPNV.EQ.203 ) THEN
        varname = 'Z-Aqueous Relative Permeability'
        iflg = 0
        dflg = 1
        ldim = 3
        dim1 = 3
        dim2 = 2
        call string2idx(ldim,iflg,dflg,'rkl',idx,t_ok)
        avar = 0.d0
        call write_var(varname,' ',iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
!        WRITE(IPL,FORM) (RKL(3,2,N),N=1,NFLD)
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!
!---  End of WRPL_13 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_14( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     209 atmospheric wind speed ' WS '
!     210 residual NAPL saturation 'SNR '
!     211 mobile NAPL saturation 'SNM '
!     212 free NAPL saturation 'SNF '
!     213 surface temperature 'T_S'
!     214 surface vapor pressure 'PV_S'
!     215 actual evaporation rate 'E_SA'
!     216 potential evaporation rate 'PE_SA'
!     217 actual transpiration rate 'T_SA'
!     218 potential transpiration rate 'PT_SA'
!     219 saturated co2 aqueous mass fraction 'SXLA'
!     220 aqueous alcohol mole fraction 'XMLA'
!     221 NAPL alcohol mode fraction 'XMNA'
!     222 aqueous alcohol mass fraction 'XLA '
!     223 NAPL alcohol mass fraction 'XNA '
!     224 atmospheric pressure, ' PA '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE HYST
      USE GRID
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
      CHARACTER*16 FORM
      INTEGER LDIM, DIM1,DIM2,IDX,IFLG,DFLG
       LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_14'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!---  End of WRPL_14 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_15( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     225 surface aqueous pressure, 'PL_S'
!     226 surface gas pressure, 'PG_S'
!     227 surface aqueous saturation, 'SL_S
!     228 surface latent heat flux, 'QL_S'
!     229 surface sensible heat flux, 'QH_S'
!     230 surface net long-wave radiation, 'RL_S'
!     231 surface net short-wave radiation, 'RS_S'
!     232 surface net total radiation, 'RT_S'
!     233 surface water mass balance kg/s, 'WB_S'
!     234 plant temperature, 'T_P' or 'TPXX'
!     235 plant temperature, 'TPXX'
!     236 plant temperature, 'TPXX'
!     237 plant temperature, 'TPXX'
!     238 plant temperature, 'TPXX'
!     239 rainfall interception mass, 'RFIM'
!     240 sorbed oil mass, kg oil, 'TSO '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 December 2003.
!     Last Modified by MD White, PNNL, 31 December 2003.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
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
      CHARACTER*16 FORM
      INTEGER LDIM, DIM1,DIM2,IDX,IFLG,DFLG
       LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_15'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!
!---  End of WRPL_15 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_16( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     241 sorbed oil mass fraction kg oil/kg soil, 'XSO '
!     242 sorbed oil volumetric concentration kg oil/m^3, 'CSO '
!     247 x-direction intrinsic permeability m^2, ' UK '
!     248 y-direction intrinsic permeability m^2, ' VK '
!     249 z-direction intrinsic permeability m^2, ' WK '
!     251 hydrate CO2	mass fraction, 'XHA '
!     252 hydrate CH4 mass fraction, 'XHO '
!     253 hydrate density kg/m^3, 'RHOH'
!     254 hydrate saturation, ' SH '
!     255 hydrate pressure Pa, ' PH '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 January 2004.
!     Last Modified by MD White, PNNL, 24 January 2004.
!     Last Modified by MD White, PNNL, 9 September 2004.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE OUTPU
      USE HYST
      USE GRID
      USE GRID_MOD
      USE FILES
      USE FDVP
      USE FDVH
      USE CONST
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
      CHARACTER*16 FORM
      CHARACTER*64 varname
      integer ldim, dim1,dim2,idx,iflg,dflg
      logical t_ok
       LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_16'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.247 ) THEN
        varname = 'X-Direction Intrinsic Permeability'
        INDX = 4
        JNDX = 1
        IUNM = 2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varp_tmp = 0.d0
        do n=1,num_nodes
          varp_tmp(n) = PERM(JNDX,N)*PERMRF(2,N)
        enddo
        iflg = 0
        dflg = 1
        ldim = 1
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.248 ) THEN
        varname = 'Y-Direction Intrinsic Permeability'
        INDX = 4
        JNDX = 2
        IUNM = 2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varp_tmp = 0.d0
        do n=1,num_nodes
          varp_tmp(n) = PERM(JNDX,N)*PERMRF(2,N)
        enddo
        iflg = 0
        dflg = 1
        ldim = 1
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ELSEIF( IPNV.EQ.249 ) THEN
        varname = 'Z-Direction Intrinsic Permeability'
        INDX = 4
        JNDX = 3
        IUNM = 2
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varp_tmp = 0.d0
        do n=1,num_nodes
          varp_tmp(n) = PERM(JNDX,N)*PERMRF(2,N)
        enddo
        iflg = 0
        dflg = 1
        ldim = 1
        call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of WRPL_17 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_17( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     264 precipitated salt saturation, ' SS '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 January 2004.
!     Last Modified by MD White, PNNL, 24 January 2004.
!     Last Modified by MD White, PNNL, 9 September 2004.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE OUTPU
      USE GRID
      USE FILES
      USE FDVH
      USE CONST
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
      CHARACTER*16 FORM
      INTEGER LDIM, DIM1,DIM2,IDX,IFLG,DFLG
       LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_17'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!
!---  End of WRPL_17 group
!

      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRPL_18( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     264 precipitated salt saturation, ' SS '
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 January 2004.
!     Last Modified by MD White, PNNL, 24 January 2004.
!     Last Modified by MD White, PNNL, 9 September 2004.
!     $Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE OUTPU
      USE GRID
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



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*16 FORM
      logical t_ok
      integer idx,ldim, dim1,dim2, iflg, dflg
      character*64 t_string
      character*64 varname
      LOGICAL :: use_ga

!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/WRPL_18'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
        '$Id: wrplot.F,v 1.39 2008/02/13 01:01:07 d3c002 Exp $' 
      ICSN = ICSN+ICSNX

      VAR = 1.D+0
      DIM1 = 0
      DIM2 = 0
      IF( IPNV.EQ.289 ) THEN
        INDX = 4
        IUNM = 1
        IUNS = -1
        CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
        varname = 'Evapotranspiration'
        iflg = 0
        dflg = 1
        ldim = 2
        dim1 = 2
        call string2idx(ldim,iflg,dflg,'et',idx,t_ok)
        avar = 0.d0
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,ldim,dim1,dim2,var,avar,me,ipl,form,0)
      ENDIF

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)

!
!---  End of WRPL_18 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
        SUBROUTINE WRPL_20( FORM,IPNV,IPNVGC )
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
!     Write plot files for the following variables:
!
!     341 aqueous internal energy, 'UL'
!     342 gas internal energy, 'UG'
!     343 nonaqueous liquid phase internal energy, 'UN'
!     344 aqueous thermal conductivity, 'THKL'
!     345 gas thermal conductivity, 'THKG'
!     346 nonaqueous liquid phase thermal conductivity, 'THKN'
!     347 CO2 aqueous diffusion coefficient, 'DFLA'
!     348 H2O gas diffusion coefficient, 'DFGW'
!     349       coupled-well CO2 mass rate, kg/s 'QMRA'
!     350       coupled-well CO2 mass integral, kg 'QMIA'
!     351       coupled-well water mass rate, kg/s 'QMRW'
!     352 coupled-well water mass integral, kg 'QMIW'
!     353 vertical-equilibrium gas-aqueous interface elevation, m, ZI_VE
!     354 vertical-equilibrium gas pressure, Pa, PG_VE
!     355 vertical-equilibrium aqueous pressure, Pa, PL_VE
!     356 vertical-equilibrium gas saturation, SG_VE
!     357 vertical-equilibrium trapped gas saturation, SGT_VE
!     358 vertical-equilibrium aqueous saturation SL_VE
!     359 vertical-equilibrium gas relative permeability, RKL_VE
!     360 vertical-equilibrium aqueous relative permeability, RKG_VE
!     361 vertically-integrated CO2 mass, kg, VIMA
!     362 vertically-integrated CO2 mass, kg/m^2, VIAPA
!     363 vertically-integrated CO2 mass, kg, VIGA
!     364 vertically-integrated CO2 mass, kg/m^2, VIGAPA
!     365 vertically-integrated CO2 mass, kg, VILA
!     366 vertically-integrated CO2 mass, kg/m^2, VILAPA
!     367 integrated precipitated salt mass, kg, IMPS
!     368 mean effective stress, Pa, STRS-M
!     369 x-direction normal strain, X-STRN
!     370 y-direction normal strain, Y-STRN
!     371 z-direction normal strain, Z-STRN
!     372 x-direction displacement, m, X-DSPL
!     373 y-direction displacement, m, Y-DSPL
!     374 z-direction displacement, m, Z-DSPL
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 December 2010.
!
!----------------------Fortran 90 Modules------------------------------!
!
          USE GLB_PAR
          USE SOLTN
          USE OUTPU
          USE HYST
          USE GRID
          USE FILES
         ! USE FDVT
         ! USE FDVS
          USE FDVP
         ! USE FDVG
          USE CONST
          use grid_mod

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
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Type Declarations-------------------------------!
!
          CHARACTER*16 FORM
          logical :: t_ok
          integer dim, dim1,dim2,idx,iflg,dflg
          double precision, dimension(:), allocatable :: dvar,dvar_tmp
          character*64 varname

!
!----------------------Common Blocks-----------------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
!          ISUB_LOG = ISUB_LOG+1
!          SUB_LOG(ISUB_LOG) = '/WRPL_20'
          IF( INDEX(CVS_ID(277)(1:1),'$').EQ.0 ) CVS_ID(277) = &
           '$Id: wrplot.F,v 1.4 2011/09/09 17:15:38 d3c002 Exp $'
        me = ga_nodeid()
!
!---  Dynamic memory allocation  ---
!
          ld_xy = ldx*ldy
          ld_xymin = (ixmax-ixmin+1)*(iymax-iymin+1)
                call ga_igop(1,ld_xy,1,'max')
          allocate(dvar(ld_xy))
          allocate(dvar_tmp(ld_xy))
          VAR = 1.D+0
!
!---  Vertically-integrated CO2 mass  ---
!
          IF( IPNV.EQ.361 ) THEN
            INDX = 4
            IUNKG = 1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            varname = 'Vertically-Integrated CO2 Mass'
            dvar = 0.d0
            varp_tmp = 0.d0
            ijidx = 0
            do icnx = 1,num_cnx
                id_up = conn_up(icnx)
                id_dn = conn_dn(icnx)
                if(unvzc(icnx) == 1.d0) then
                        if(izmin == 1.and.id_dn <= ldx*ldy) then
                        varp_tmp(id_dn) = varp_tmp(id_dn) + VOL(id_dn)*PORD(2,id_dn)* &
          (SL(2,id_dn)*RHOL(2,id_dn)*XLA(2,id_dn) + &
           SG(2,id_dn)*RHOG(2,id_dn)*XGA(2,id_dn))
        endif
         if( id_g2l(id_up) > 0 )then
          varp_tmp(id_up) = varp_tmp(id_dn) + VOL(id_up)*PORD(2,id_up)*&
          (SL(2,id_up)*RHOL(2,id_up)*XLA(2,id_up) + &
           SG(2,id_up)*RHOG(2,id_up)*XGA(2,id_up))
          if(id_up > ldx*ldy*(ldz-1) .and. izmax == nzdim) then
            ijidx = ijidx+1
            dvar(ijidx) = varp_tmp(id_up)
          endif
         else
          if(id_g2l(id_dn) <= 0) cycle
           ijidx = ijidx+1
           dvar(ijidx) = varp_tmp(id_dn)
         endif
!        endif
      endif
      enddo
        if(ga_pz > 1) then
        nga_xy = ga_px*ga_py
        dvar_tmp = 0.d0
        do lpy = 1,ga_py
        do lpx = 1,ga_px
        do lpz = 1,ga_pz
        mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
        if(me.eq.mx_tmp) then
          dvar_tmp = dvar
       endif
        enddo
        call ga_dgop(1,dvar_tmp,ld_xy,'+')
        do lpz = 1,ga_pz
                mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
                if(me.eq.mx_tmp) then
                        dvar = dvar_tmp
                endif
        enddo
        dvar_tmp = 0.d0
        enddo
        enddo
        endif
        varp_tmp = 0.d0
        ijidx = 0
        do i=1,num_nodes
                if(id_g2l(i) > 0) then
        ijidx = ijidx + 1
        if(ijidx > ld_xymin) then
          ijidx = 1
        endif
        varp_tmp(i) = dvar(ijidx)
       endif
      enddo
      iflg = 0
      dflg = 1
      dim = 1
      call string2idx(dim,iflg,dflg,'varp_tmp',idx,t_ok)
      avar = 0.d0
        !     call
        !     get_data_from_ga(form,iflg,dflg,dim,dim1,dim2,idx,ipl,0,var,avar)
!     call ga_sync
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form2,0)
!
!---  Vertically-integrated CO2 mass per area  ---
!
          ELSEIF( IPNV.EQ.362 ) THEN
            INDX = 4
            IUNKG = 1
            IUNM = -2
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            varname = 'Vertically-Integrated CO2 Mass per Area'
            dvar = 0.d0
            varp_tmp = 0.d0
            ijidx = 0
      do icnx = 1,num_cnx
      id_up = conn_up(icnx)
      id_dn = conn_dn(icnx)
      if(unvzc(icnx) == 1.d0) then
        if(izmin == 1.and.id_dn <= ldx*ldy) then
          varp_tmp(id_dn) = varp_tmp(id_dn) + VOL(id_dn)*PORD(2,id_dn)*&
          (SL(2,id_dn)*RHOL(2,id_dn)*XLA(2,id_dn) + &
           SG(2,id_dn)*RHOG(2,id_dn)*XGA(2,id_dn))/areac(icnx)
        endif
         if( id_g2l(id_up) > 0 )then
          varp_tmp(id_up) = varp_tmp(id_dn) + VOL(id_up)*PORD(2,id_up)*&
          (SL(2,id_up)*RHOL(2,id_up)*XLA(2,id_up) + &
           SG(2,id_up)*RHOG(2,id_up)*XGA(2,id_up))/areac(icnx)
          if(id_up > ldx*ldy*(ldz-1) .and. izmax == nzdim) then
            ijidx = ijidx+1
            dvar(ijidx) = varp_tmp(id_up)
          endif
         else
          if(id_g2l(id_dn) <= 0) cycle
           ijidx = ijidx+1
           dvar(ijidx) = varp_tmp(id_dn)
         endif
!        endif
      endif
      enddo
      if(ga_pz > 1) then
        nga_xy = ga_px*ga_py
        dvar_tmp = 0.d0
        do lpy = 1,ga_py
        do lpx = 1,ga_px
        do lpz = 1,ga_pz
                mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
       if(me.eq.mx_tmp) then
          dvar_tmp = dvar
       endif
        enddo
     call ga_dgop(1,dvar_tmp,ld_xy,'+')
        do lpz = 1,ga_pz
                mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
        if(me.eq.mx_tmp) then
                dvar = dvar_tmp
        endif
        enddo
                 dvar_tmp = 0.d0
        enddo
        enddo
!     call ga_dgop(1,dvar,ld_xy,'+')
        endif
        varp_tmp = 0.d0
        ijidx = 0
        do i=1,num_nodes
       if(id_g2l(i) > 0) then
        ijidx = ijidx + 1
        if(ijidx > ld_xymin) then
          ijidx = 1
        endif
        varp_tmp(i) = dvar(ijidx)
       endif
        enddo
        iflg = 0
        dflg = 1
        dim = 1
        call string2idx(dim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
!     call
!     get_data_from_ga(form,iflg,dflg,dim,dim1,dim2,idx,ipl,0,var,avar)
!     call ga_sync
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form2,0)
!        DVAR(N) = DVAR(N) + VOL(NX)*PORD(2,NX)* &
!          (SL(2,NX)*RHOL(2,NX)*XLA(2,NX) + &
!           SG(2,NX)*RHOG(2,NX)*XGA(2,NX))
!           SN(2,NX)*RHON(2,NX)*XNA(2,NX))/AFZ(NSZ(NX))
!
!---  Vertically-integrated gas CO2 mass  ---
!
         ELSEIF( IPNV.EQ.363 ) THEN
           INDX = 4
           IUNKG = 1
           CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
           varname = 'Vertically-Integrated Gas CO2 Mass'
           dvar = 0.d0
           varp_tmp = 0.d0
           ijidx = 0
           do icnx = 1,num_cnx
      id_up = conn_up(icnx)
      id_dn = conn_dn(icnx)
      if(unvzc(icnx) == 1.d0) then
        if(izmin == 1.and.id_dn <= ldx*ldy) then
          varp_tmp(id_dn) = varp_tmp(id_dn) + VOL(id_dn)*PORD(2,id_dn)*&
           SG(2,id_dn)*RHOG(2,id_dn)*XGA(2,id_dn)
        endif
         if( id_g2l(id_up) > 0 )then
          varp_tmp(id_up) = varp_tmp(id_dn) + VOL(id_up)*PORD(2,id_up)*&
           SG(2,id_up)*RHOG(2,id_up)*XGA(2,id_up)
          if(id_up > ldx*ldy*(ldz-1) .and. izmax == nzdim) then
            ijidx = ijidx+1
            dvar(ijidx) = varp_tmp(id_up)
          endif
         else
        if(id_g2l(id_dn) <= 0) cycle
           ijidx = ijidx+1
           dvar(ijidx) = varp_tmp(id_dn)
         endif
!        endif
      endif
      enddo
      if(ga_pz > 1) then
        nga_xy = ga_px*ga_py
        dvar_tmp = 0.d0
        do lpy = 1,ga_py
        do lpx = 1,ga_px
        do lpz = 1,ga_pz
       mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
       if(me.eq.mx_tmp) then
          dvar_tmp = dvar
       endif
        enddo
     call ga_dgop(1,dvar_tmp,ld_xy,'+')
        do lpz = 1,ga_pz
       mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
       if(me.eq.mx_tmp) then
         dvar = dvar_tmp
       endif
        enddo
         dvar_tmp = 0.d0
        enddo
        enddo
!     call ga_dgop(1,dvar,ld_xy,'+')
        endif
        varp_tmp = 0.d0
        ijidx = 0
        do i=1,num_nodes
       if(id_g2l(i) > 0) then
        ijidx = ijidx + 1
        if(ijidx > ld_xymin) then
          ijidx = 1
        endif
        varp_tmp(i) = dvar(ijidx)
       endif
        enddo
        iflg = 0
        dflg = 1
        dim = 1
        call string2idx(dim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
        !     call
        !     get_data_from_ga(form,iflg,dflg,dim,dim1,dim2,idx,ipl,0,var,avar)
!     call ga_sync
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form2,0)
!
!---  Vertically-integrated gas CO2 mass per area  ---
!
          ELSEIF( IPNV.EQ.364 ) THEN
            INDX = 4
            IUNKG = 1
            IUNM = -2
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
         varname  = 'Vertically-Integrated Gas ' // 'CO2 Mass per Area'

            dvar = 0.d0
            varp_tmp = 0.d0
            ijidx = 0
            do icnx = 1,num_cnx
      id_up = conn_up(icnx)
      id_dn = conn_dn(icnx)
      if(unvzc(icnx) == 1.d0) then
        if(izmin == 1.and.id_dn <= ldx*ldy) then
          varp_tmp(id_dn) = varp_tmp(id_dn) + VOL(id_dn)*PORD(2,id_dn)*&
           SG(2,id_dn)*RHOG(2,id_dn)*XGA(2,id_dn)/areac(icnx)
        endif
         if( id_g2l(id_up) > 0 )then
          varp_tmp(id_up) = varp_tmp(id_dn) + VOL(id_up)*PORD(2,id_up)*&
           SG(2,id_up)*RHOG(2,id_up)*XGA(2,id_up)/areac(icnx)
          if(id_up > ldx*ldy*(ldz-1) .and. izmax == nzdim) then
            ijidx = ijidx+1
            dvar(ijidx) = varp_tmp(id_up)
          endif
         else
          if(id_g2l(id_dn) <= 0) cycle
           ijidx = ijidx+1
           dvar(ijidx) = varp_tmp(id_dn)
         endif
!        endif
      endif
      enddo
      if(ga_pz > 1) then
        nga_xy = ga_px*ga_py
        dvar_tmp = 0.d0
        do lpy = 1,ga_py
        do lpx = 1,ga_px
        do lpz = 1,ga_pz
                mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
        if(me.eq.mx_tmp) then
          dvar_tmp = dvar
       endif
       enddo
        call ga_dgop(1,dvar_tmp,ld_xy,'+')
        do lpz = 1,ga_pz
       mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
       if(me.eq.mx_tmp) then
         dvar = dvar_tmp
       endif
        enddo
         dvar_tmp = 0.d0
        enddo
        enddo
!     call ga_dgop(1,dvar,ld_xy,'+')
        endif
        varp_tmp = 0.d0
        ijidx = 0
        do i=1,num_nodes
       if(id_g2l(i) > 0) then
        ijidx = ijidx + 1
        if(ijidx > ld_xymin) then
          ijidx = 1
        endif
        varp_tmp(i) = dvar(ijidx)
       endif
        enddo
        iflg = 0
        dflg = 1
        dim = 1
        call string2idx(dim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
!     call
!     get_data_from_ga(form,iflg,dflg,dim,dim1,dim2,idx,ipl,0,var,avar)
!     call ga_sync
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form2,0)
!
!---  Vertically-integrated aqueous CO2 mass  ---
!
          ELSEIF( IPNV.EQ.365 ) THEN
            INDX = 4
            IUNKG = 1
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            varname = 'Vertically-Integrated Aqueous CO2 Mass'
            dvar = 0.d0
            varp_tmp = 0.d0
            ijidx = 0
            do icnx = 1,num_cnx
      id_up = conn_up(icnx)
      id_dn = conn_dn(icnx)
      if(unvzc(icnx) == 1.d0) then
        if(izmin == 1.and.id_dn <= ldx*ldy) then
          varp_tmp(id_dn) = varp_tmp(id_dn) + VOL(id_dn)*PORD(2,id_dn)*&
           SL(2,id_dn)*RHOL(2,id_dn)*XLA(2,id_dn)
        endif
        if( id_g2l(id_up) > 0 )then
          varp_tmp(id_up) = varp_tmp(id_dn) + VOL(id_up)*PORD(2,id_up)*&
           SL(2,id_up)*RHOL(2,id_up)*XLA(2,id_up)
          if(id_up > ldx*ldy*(ldz-1) .and. izmax == nzdim) then
            ijidx = ijidx+1
            dvar(ijidx) = varp_tmp(id_up)
          endif
         else
          if(id_g2l(id_dn) <= 0) cycle
           ijidx = ijidx+1
           dvar(ijidx) = varp_tmp(id_dn)
         endif
!        endif
      endif
      enddo
      if(ga_pz > 1) then
        nga_xy = ga_px*ga_py
        dvar_tmp = 0.d0
        do lpy = 1,ga_py
        do lpx = 1,ga_px
        do lpz = 1,ga_pz
       mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
       if(me.eq.mx_tmp) then
          dvar_tmp = dvar
       endif
        enddo
     call ga_dgop(1,dvar_tmp,ld_xy,'+')
        do lpz = 1,ga_pz
       mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
       if(me.eq.mx_tmp) then
         dvar = dvar_tmp
       endif
        enddo
         dvar_tmp = 0.d0
        enddo
        enddo
!     call ga_dgop(1,dvar,ld_xy,'+')
        endif
        varp_tmp = 0.d0
        ijidx = 0
        do i=1,num_nodes
       if(id_g2l(i) > 0) then
        ijidx = ijidx + 1
        if(ijidx > ld_xymin) then
          ijidx = 1
        endif
        varp_tmp(i) = dvar(ijidx)
       endif
        enddo
        iflg = 0
        dflg = 1
        dim = 1
        call string2idx(dim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
!     call
!     get_data_from_ga(form,iflg,dflg,dim,dim1,dim2,idx,ipl,0,var,avar)
!     call ga_sync
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form2,0)
!
!---  Vertically-integrated aqueous CO2 mass per area  ---
!
          ELSEIF( IPNV.EQ.366 ) THEN
            INDX = 4
            IUNKG = 1
            IUNM = -2
            CALL RDUNIT(UNPLOT(IPNV),VAR,INDX)
            varname = 'Vertically-Integrated Aqueous ' //   &
      'CO2 Mass per Area'
            dvar = 0.d0
            varp_tmp = 0.d0
            ijidx = 0
      do icnx = 1,num_cnx
      id_up = conn_up(icnx)
      id_dn = conn_dn(icnx)
      if(unvzc(icnx) == 1.d0) then
        if(izmin == 1.and.id_dn <= ldx*ldy) then
          varp_tmp(id_dn) = varp_tmp(id_dn) + VOL(id_dn)*PORD(2,id_dn)*&
           SL(2,id_dn)*RHOL(2,id_dn)*XLA(2,id_dn)/areac(icnx)
        endif
         if( id_g2l(id_up) > 0 )then
          varp_tmp(id_up) = varp_tmp(id_dn) + VOL(id_up)*PORD(2,id_up)*&
           SL(2,id_up)*RHOL(2,id_up)*XLA(2,id_up)/areac(icnx)
          if(id_up > ldx*ldy*(ldz-1) .and. izmax == nzdim) then
            ijidx = ijidx+1
            dvar(ijidx) = varp_tmp(id_up)
          endif
         else
        if(id_g2l(id_dn) <= 0) cycle
           ijidx = ijidx+1
           dvar(ijidx) = varp_tmp(id_dn)
         endif
!        endif
      endif
      enddo
      if(ga_pz > 1) then
        nga_xy = ga_px*ga_py
        dvar_tmp = 0.d0
        do lpy = 1,ga_py
        do lpx = 1,ga_px
        do lpz = 1,ga_pz
       mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
       if(me.eq.mx_tmp) then
          dvar_tmp = dvar
       endif
        enddo
     call ga_dgop(1,dvar_tmp,ld_xy,'+')
        do lpz = 1,ga_pz
       mx_tmp = lpx-1+(lpy-1)*ga_px+(lpz-1)*nga_xy
       if(me.eq.mx_tmp) then
         dvar = dvar_tmp
       endif
        enddo
         dvar_tmp = 0.d0
        enddo
        enddo
!     call ga_dgop(1,dvar,ld_xy,'+')
        endif
        varp_tmp = 0.d0
        ijidx = 0
        do i=1,num_nodes
       if(id_g2l(i) > 0) then
        ijidx = ijidx + 1
        if(ijidx > ld_xymin) then
          ijidx = 1
        endif
        varp_tmp(i) = dvar(ijidx)
       endif
        enddo
        iflg = 0
        dflg = 1
     dim = 1
     call string2idx(dim,iflg,dflg,'varp_tmp',idx,t_ok)
        avar = 0.d0
!     call
!     get_data_from_ga(form,iflg,dflg,dim,dim1,dim2,idx,ipl,0,var,avar)
!     call ga_sync
        call write_var(varname,UNPLOT(IPNV),iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form2,0)
        ENDIF
        deallocate(dvar)
        deallocate(dvar_tmp)
!  ISUB_LOG = ISUB_LOG-1
!
!---  End of WRPL_20 group
        RETURN
    END

      subroutine string2idx(ldim,iflg,dflg,t_string,idx,t_ok)
  use grid_mod
  implicit none
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!  
  integer idx,i,slen,nlen,ldim,iflg,dflg,grid_clen
  logical t_ok
  character (len=*) :: t_string
  t_ok = .false.
!  slen = grid_clen(t_string)
  slen = len_trim(t_string)
  if(ldim.eq.1) then
   if(iflg.eq.1) then
    i = 0
    do while (i.lt.inode_field.and.(.not.t_ok))
      i = i + 1
!      nlen = grid_clen(i_nd_fld_names(i))
      nlen = len_trim(i_nd_fld_names(i))
      if (t_string(1:slen).eq.i_nd_fld_names(i)(1:nlen)) then
        idx = i
        t_ok = .true.
      endif
    enddo
   elseif(dflg.eq.1) then
    i = 0
    do while (i.lt.dnode_field.and.(.not.t_ok))
      i = i + 1
!      nlen = grid_clen(d_nd_fld_names(i))
      nlen = len_trim(d_nd_fld_names(i))
      if (t_string(1:slen).eq.d_nd_fld_names(i)(1:nlen)) then
        idx = i
        t_ok = .true.
      endif
    enddo
   endif
  elseif(ldim.eq.2) then
   if(iflg.eq.1) then
    i = 0
    do while (i.lt.inode_2field.and.(.not.t_ok))
      i = i + 1
!      nlen = grid_clen(i_nd_2fld_names(i))
      nlen = len_trim(i_nd_2fld_names(i))
      if (t_string(1:slen).eq.i_nd_2fld_names(i)(1:nlen)) then
        idx = i
        t_ok = .true.
      endif
    enddo
   elseif(dflg.eq.1) then
    i = 0
    do while (i.lt.dnode_2field.and.(.not.t_ok))
      i = i + 1
!      nlen = grid_clen(d_nd_2fld_names(i))
      nlen = len_trim(d_nd_2fld_names(i))
      if (t_string(1:slen).eq.d_nd_2fld_names(i)(1:nlen)) then
        idx = i
        t_ok = .true.
      endif
    enddo
   endif

  elseif(ldim.eq.3) then
   if(iflg.eq.1) then
    i = 0
    do while (i.lt.inode_3field.and.(.not.t_ok))
      i = i + 1
!      nlen = grid_clen(i_nd_3fld_names(i))
      nlen = len_trim(i_nd_3fld_names(i))
      if (t_string(1:slen).eq.i_nd_3fld_names(i)(1:nlen)) then
        idx = i
        t_ok = .true.
      endif
    enddo
   elseif(dflg.eq.1) then
    i = 0
    do while (i.lt.dnode_3field.and.(.not.t_ok))
      i = i + 1
!      nlen = grid_clen(d_nd_3fld_names(i))
      nlen = len_trim(d_nd_3fld_names(i))
      if (t_string(1:slen).eq.d_nd_3fld_names(i)(1:nlen)) then
        idx = i
        t_ok = .true.
      endif
    enddo
   endif
  endif
!
end subroutine string2idx

subroutine get_data_from_ga(form,iflg,dflg,ldim,dim1,dim2,idx,ipl,irs,var,avar)
  use grid_mod
  implicit none
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
  integer :: iflg,dflg,dim1,one,dim2,ldim,nlayx,ijdim,kx,i,k,idx,idivx,nzdimx
  integer :: i_offset, ix_offset,iy_offset,iz_offset, ipl,irs
  integer :: lo_get(5), hi_get(5), ld(5), lo_put(5), hi_put(5)
  double precision :: var,avar
  integer, dimension(:), allocatable :: ibuf
  double precision, dimension(:), allocatable :: dbuf    
  CHARACTER(len=*) form
  integer :: me
  logical :: use_ga
!
  me = ga_nodeid()
  use_ga = .true.
  nlayx = 4
  ijdim = nxdim*nydim*nlayx
  one = 1
  if (ixmin.eq.1) then
    ix_offset = 0
  else
    ix_offset = gwidth
  endif
  if (iymin.eq.1) then
    iy_offset = 0
  else
    iy_offset = gwidth
  endif
  if (izmin.eq.1) then
    iz_offset = 0
  else
    iz_offset = gwidth
  endif
  one = 1
  i_offset = ix_offset + iy_offset*ldx + iz_offset*ldx*ldy + 1
!
  if(iflg.eq.1) then
    if(ldim.eq.1) then
      lo_put(1) = ixmin
      lo_put(2) = iymin
      lo_put(3) = izmin
      hi_put(1) = ixmax
      hi_put(2) = iymax
      hi_put(3) = izmax
!
      ld(1) = ldx
      ld(2) = ldy
      ld(3) = ldz

      call nga_put(ga_int,lo_put,hi_put,i_nd_fld(idx)%p(i_offset),ld)
      call ga_sync
      if(me.eq.0) then     
        allocate(ibuf(ijdim))
        lo_get(1) = 1
        hi_get(1) = nxdim	   
        lo_get(2) = 1
        hi_get(2) = nydim	   
        ld(1) = nxdim
        ld(2) = nydim
        ld(3) = nlayx
        kx = 0
!        if( nzdim/nlayx .ge.1 ) then
        idivx = nzdim/nlayx
        nzdimx = idivx*nlayx
        if( idivx .ge.1 ) then
          do k=1,nzdimx,nlayx
            lo_get(3) = kx*nlayx+1
            hi_get(3) = (kx+1)*nlayx
            kx = kx + 1
            call nga_get(ga_int,lo_get,hi_get,ibuf,ld)
            if(irs.eq.0) then
              write(ipl,form) (var*ibuf(i),i=1,ijdim)
            else
              write(irs,form)(real(ibuf(i)),i=1,ijdim)
            endif
          enddo
        endif
        if(mod(nzdim,nlayx).ne.0) then
          ijdim = mod(nzdim,nlayx)*nxdim*nydim
          lo_get(3) = kx*nlayx+1
          hi_get(3) = nzdim
          call nga_get(ga_int,lo_get,hi_get,ibuf,ld)
          if(irs.eq.0) then
            write(ipl,form) (var*ibuf(i),i=1,ijdim)
          else
            write(irs,form) (real(ibuf(i)),i=1,ijdim)
          endif
        endif
        deallocate(ibuf)
      endif
    elseif(ldim.eq.2) then
      lo_put(1) = 1
      hi_put(1) = i_nd_2dim1(idx)
      lo_put(2) = ixmin
      lo_put(3) = iymin
      lo_put(4) = izmin
      hi_put(2) = ixmax
      hi_put(3) = iymax
      hi_put(4) = izmax
      ld(1) = i_nd_2dim1(idx)
      ld(2) = ldx
      ld(3) = ldy
      ld(4) = ldz
      call nga_put(ga_int2,lo_put,hi_put,i_nd_2fld(idx)%p(one,i_offset),ld)
      call ga_sync
      if(me.eq.0) then
        allocate(ibuf(ijdim))
        lo_get(1) = dim1
        hi_get(1) = dim1
!        if(irs.gt.0) then
!          lo_get(1) = dim1
!          hi_get(1) = dim2
!        endif
        lo_get(2) = 1
        hi_get(2) = nxdim
        lo_get(3) = 1
        hi_get(3) = nydim
        ld(1) = 1 
        ld(2) = nxdim
        ld(3) = nydim
        ld(4) = nlayx
        kx = 0
        idivx = nzdim/nlayx
        nzdimx = idivx*nlayx
        if( idivx .ge.1 ) then
          do k=1,nzdimx,nlayx
            lo_get(4) = kx*nlayx+1
            hi_get(4) = (kx+1)*nlayx
            kx = kx + 1
            call nga_get(ga_int2,lo_get,hi_get,ibuf,ld)
            if(irs.eq.0) then
              write(ipl,form) (var*ibuf(i),i=1,ijdim)
            else
              write(irs,form) (real(ibuf(i)),i=1,ijdim)
            endif
          enddo
        endif
        if(mod(nzdim,nlayx).ne.0) then
          ijdim = mod(nzdim,nlayx)*nxdim*nydim
          lo_get(4) = kx*nlayx+1
          hi_get(4) = nzdim
          call nga_get(ga_int2,lo_get,hi_get,ibuf,ld)
          if(irs.eq.0) then
            write(ipl,form) (var*ibuf(i),i=1,ijdim)
          else
            write(irs,form) (real(ibuf(i)),i=1,ijdim)
          endif
        endif
        deallocate(ibuf)
      endif
    elseif(ldim.eq.3) then
      lo_put(1) = 1
      hi_put(1) = i_nd_3dim1(idx)
      lo_put(2) = 1
      hi_put(2) = i_nd_3dim2(idx)
      lo_put(3) = ixmin
      lo_put(4) = iymin
      lo_put(5) = izmin
      hi_put(3) = ixmax
      hi_put(4) = iymax
      hi_put(5) = izmax
      ld(1) = i_nd_3dim1(idx)
      ld(2) = i_nd_3dim2(idx)
      ld(3) = ldx
      ld(4) = ldy
      ld(5) = ldz

      call nga_put(ga_int3,lo_put,hi_put,i_nd_3fld(idx)%p(one,one,i_offset),ld)
      call ga_sync
      if(me.eq.0) then
        allocate(ibuf(ijdim))
        lo_get(1) = dim1
        hi_get(1) = dim1
        lo_get(2) = dim2
        hi_get(2) = dim2
        lo_get(3) = 1
        hi_get(3) = nxdim	   
        lo_get(4) = 1
        hi_get(4) = nydim	  
        ld(1) = 1
        ld(2) = 1 
        ld(3) = nxdim
        ld(4) = nydim
        ld(5) = nlayx
        kx = 0
!        if( nzdim/nlayx .ge.1 ) then
        idivx = nzdim/nlayx
        nzdimx = idivx*nlayx
        if( idivx .ge.1 ) then
          do k=1,nzdimx,nlayx
            lo_get(5) = kx*nlayx+1
            hi_get(5) = (kx+1)*nlayx
            kx = kx + 1
            call nga_get(ga_int3,lo_get,hi_get,ibuf,ld)
            if(irs.eq.0) then
              write(ipl,form) (var*ibuf(i),i=1,ijdim)
            else
              write(irs,form) (real(ibuf(i)),i=1,ijdim)
            endif
          enddo
        endif
        if(mod(nzdim,nlayx).ne.0) then
          ijdim = mod(nzdim,nlayx)*nxdim*nydim
          lo_get(5) = kx*nlayx+1
          hi_get(5) = nzdim
          call nga_get(ga_int3,lo_get,hi_get,ibuf,ld)
          if(irs.eq.0) then
            write(ipl,form) (var*ibuf(i),i=1,ijdim)
          else
            write(irs,form) (real(ibuf(i)),i=1,ijdim)
          endif
        endif
        deallocate(ibuf)
      endif
    endif
  endif
!
  if(dflg.eq.1) then
    if(ldim.eq.1) then
      lo_put(1) = ixmin
      lo_put(2) = iymin
      lo_put(3) = izmin
      hi_put(1) = ixmax
      hi_put(2) = iymax
      hi_put(3) = izmax
!
      ld(1) = ldx
      ld(2) = ldy
      ld(3) = ldz
      call nga_put(ga_dbl,lo_put,hi_put,d_nd_fld(idx)%p(i_offset),ld)
      call ga_sync
      if(me.eq.0) then
        allocate(dbuf(ijdim))
        lo_get(1) = 1
        hi_get(1) = nxdim
        lo_get(2) = 1
        hi_get(2) = nydim
        ld(1) = nxdim
        ld(2) = nydim
        ld(3) = nlayx
        kx = 0
        idivx = nzdim/nlayx
        nzdimx = idivx*nlayx
        if( idivx .ge.1 ) then
          do k=1,nzdimx,nlayx
            lo_get(3) = kx*nlayx+1
            hi_get(3) = (kx+1)*nlayx
            kx = kx + 1
            call nga_get(ga_dbl,lo_get,hi_get,dbuf,ld)
            if(irs.eq.0)write(ipl,form) ((var*dbuf(i)+avar),i=1,ijdim)
            if(irs.gt.0) then
              write(irs,form) (dbuf(i),i=1,ijdim)
            endif
          enddo
        endif
        if(mod(nzdim,nlayx).ne.0) then
          ijdim = mod(nzdim,nlayx)*nxdim*nydim
          lo_get(3) = kx*nlayx+1
          hi_get(3) = nzdim
          ld(3) = nzdim-kx*nlayx
          call nga_get(ga_dbl,lo_get,hi_get,dbuf,ld)
          if(irs.eq.0)then
            write(ipl,form) ((var*dbuf(i)+avar),i=1,ijdim)
          else 
            write(irs,form) (dbuf(i),i=1,ijdim)
          endif
        endif
        deallocate(dbuf)
      endif
!      call ga_sync
    elseif(ldim.eq.2) then
      lo_put(1) = 1
      hi_put(1) = d_nd_2dim1(idx)
      lo_put(2) = ixmin
      lo_put(3) = iymin
      lo_put(4) = izmin
      hi_put(2) = ixmax
      hi_put(3) = iymax
      hi_put(4) = izmax
      ld(1) = d_nd_2dim1(idx)
      ld(2) = ldx
      ld(3) = ldy
      ld(4) = ldz
      call nga_put(ga_dbl2,lo_put,hi_put,d_nd_2fld(idx)%p(one,i_offset),ld)
      call ga_sync
      if(me.eq.0) then
        allocate(dbuf(ijdim))
        lo_get(1) = dim1
        hi_get(1) = dim1
        lo_get(2) = 1
        hi_get(2) = nxdim
        lo_get(3) = 1
        hi_get(3) = nydim
        ld(1) = 1 
        ld(2) = nxdim
        ld(3) = nydim
        ld(4) = nlayx
        kx = 0
!        if( nzdim/nlayx .ge.1 ) then
        idivx = nzdim/nlayx
        nzdimx = idivx*nlayx
        if( idivx .ge.1 ) then
          do k=1,nzdimx,nlayx
            lo_get(4) = kx*nlayx+1
            hi_get(4) = (kx+1)*nlayx
            kx = kx + 1
            call nga_get(ga_dbl2,lo_get,hi_get,dbuf,ld)
            if(irs.eq.0)then
              write(ipl,form) ((var*dbuf(i)+avar),i=1,ijdim)
            else
              write(irs,form) (dbuf(i),i=1,ijdim)
            endif

          enddo
        endif

        if(mod(nzdim,nlayx).ne.0) then
          ijdim = mod(nzdim,nlayx)*nxdim*nydim
          lo_get(4) = kx*nlayx+1
          hi_get(4) = nzdim
          call nga_get(ga_dbl2,lo_get,hi_get,dbuf,ld)
          if(irs.eq.0) then
            write(ipl,form) ((var*dbuf(i)+avar),i=1,ijdim)
          else
            write(irs,form) (dbuf(i),i=1,ijdim)
          endif

        endif
        deallocate(dbuf)
      endif
!      call ga_sync
    elseif(ldim.eq.3) then
      lo_put(1) = 1
      hi_put(1) = d_nd_3dim1(idx)
      lo_put(2) = 1
      hi_put(2) = d_nd_3dim2(idx)
      lo_put(3) = ixmin
      lo_put(4) = iymin
      lo_put(5) = izmin
      hi_put(3) = ixmax
      hi_put(4) = iymax
      hi_put(5) = izmax
      ld(1) = d_nd_3dim1(idx)
      ld(2) = d_nd_3dim2(idx)
      ld(3) = ldx
      ld(4) = ldy
      ld(5) = ldz

      call nga_put(ga_dbl3,lo_put,hi_put,d_nd_3fld(idx)%p(one,one,i_offset),ld)
      call ga_sync
      if(me.eq.0) then
        allocate(dbuf(ijdim))
        lo_get(1) = dim1
        hi_get(1) = dim1
        lo_get(2) = dim2
        hi_get(2) = dim2
        lo_get(3) = 1
        hi_get(3) = nxdim	   
        lo_get(4) = 1
        hi_get(4) = nydim	   
        ld(1) = 1
        ld(2) = 1
        ld(3) = nxdim
        ld(4) = nydim
        ld(5) = nlayx
        kx = 0
!        if( nzdim/nlayx .ge.1 ) then
        idivx = nzdim/nlayx
        nzdimx = idivx*nlayx
        if( idivx .ge.1 ) then
          do k=1,nzdimx,nlayx
            lo_get(5) = kx*nlayx+1
            hi_get(5) = (kx+1)*nlayx
            kx = kx + 1
            call nga_get(ga_dbl3,lo_get,hi_get,dbuf,ld)
            if(irs.eq.0)write(ipl,form) ((var*dbuf(i)+avar),i=1,ijdim)
            if(irs.gt.0)write(ipl,form) (dbuf(i),i=1,ijdim)
          enddo
        endif
        if(mod(nzdim,nlayx).ne.0) then
          ijdim = mod(nzdim,nlayx)*nxdim*nydim
          lo_get(5) = kx*nlayx+1
          hi_get(5) = nzdim
          call nga_get(ga_dbl3,lo_get,hi_get,dbuf,ld)
          if(irs.eq.0)write(ipl,form) ((var*dbuf(i)+avar),i=1,ijdim)
          if(irs.gt.0) write(irs,form) (dbuf(i),i=1,ijdim)

        endif
        deallocate(dbuf)
      endif
!      call ga_sync
    endif
  endif
!
  return
end subroutine get_data_from_ga



  !< Wrapper to do serial or parallel write call.
  !! It was intended to be a macro but I don't know how to do  multiline macros in fortran
  !! THe main purpose is to keep the code reasonably clean of ifdefs
  !!
  !!>
  subroutine write_var(varname, units, iflg, dflg, idx,  &
                 ldim, dim1, dim2, var, avar, me, ipl, txtformat, irs)
     USE SIO
     implicit none

     character(*),intent(in)       :: varname    ! name to write to output 
     character(*),intent(in)       :: units      ! index into UNPLOT - memory errors passing string
     integer,intent(in)            :: iflg       ! 1 for integer field
     integer,intent(in)            :: dflg       ! 1 for double field
     integer                       :: idx        ! index into global array (from stringtoindex)
     integer                       :: ldim 
     integer                       :: dim1       ! Seems to be read/write TODO ask Yilin
     integer                       :: dim2       ! For 3D variables
     double precision              :: var 
     double precision              :: avar 
     integer,intent(in)            :: me 
     integer,intent(in)            :: ipl 
     character(*),intent(in)       :: txtformat
     integer                       :: irs        ! restart flag 0=not a restart

     integer istat

#ifdef USE_H5HUT  
     if (units(1:1) .ne. ' ') then
       istat = sio_write_units(trim(varname), trim(units))
     endif
     istat = sio_write_field(varname,iflg,dflg,idx,ldim,dim1,dim2,var,avar)
#else 
     if (me.eq.0) then 
        if (irs.eq.0) then
          if (units(1:1) .ne. ' ') then
             WRITE(ipl,'(/,2A)') trim(varname), ", "//trim(units)
          else
             WRITE(ipl,'(/,2A)') trim(varname)
          endif 
        endif 
     endif 
     call get_data_from_ga(txtformat,iflg,dflg,ldim,dim1,dim2,idx,ipl,irs,var,avar) 
     call ga_sync 
#endif 

  end subroutine write_var
