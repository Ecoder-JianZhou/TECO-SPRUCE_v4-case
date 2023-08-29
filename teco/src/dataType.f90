module mod_data
    implicit none
    ! run simulation settings 
    character(100) :: simu_name ! define the case name of the simulation 
    logical :: do_spinup        ! run spinup or not
    logical :: do_mcmc          ! run mcmc or not
    logical :: do_snow          ! do soil snow process
    logical :: do_soilphy       ! do soil physics
    logical :: do_matrix        ! do matrix run
    logical :: do_EBG           ! run EBG or not based on Ma et al., 2022
    logical :: do_restart       ! have restart file or not
    logical :: do_ndep          ! N deposit
    logical :: do_simu          ! ?
    logical :: do_leap          ! judge leap year or not
    logical :: do_out_hr
    logical :: do_out_day
    logical :: do_out_mon
    logical :: do_out_yr

    integer :: dtimes                 ! 24: hourly simulation
    integer :: nSpecParams            ! How many special parameters
    character(200) :: filepath_in     ! input file path
    character(300) :: climfile        ! climate file name
    character(300) :: snowdepthfile   ! snow depthfile
    character(300) :: restartfile     ! restartfile
    character(300) :: watertablefile  ! Jian: maybe used when not run soil_physical
   
    character(200) :: outdir          ! output path
    character(250) :: outdir_case
    ! fixed output path 
    character(250) :: outDir_nc       = "results_simu_nc_format"
    character(250) :: outDir_csv      = "results_simu_csv_format"
    character(250) :: outDir_h        = "Hourly"
    character(250) :: outDir_d        = "Daily"
    character(250) :: outDir_m        = "Monthly"
    character(250) :: outDir_y        = "Yearly"
    character(250) :: outfile_restart = "restart.nc"
    character(250) :: outDir_spinup   = "results_spinup"
    character(250) :: outfile_spinup  = "results_spinup.nc"
    character(250) :: outDir_mcmc     = "results_MCMC"
    character(250) :: outDir_mcmc_h   = "results_MCMC_hourly"
    character(250) :: outDir_mcmc_d   = "results_MCMC_daily"
    character(250) :: outDir_mcmc_m   = "results_MCMC_monthly"

    
    ! experiment settings
    real :: Ttreat     = 0.        ! Temperature treatment, warming in air and soil temperature
    real :: CO2treat   = 0.        ! CO2 treatmant, up to CO2treat, not add to Ca. CO2
    real :: N_fert     = 0.        ! 5.6 ! (11.2 gN m-2 yr-1, in spring, Duke Forest FACE)

    ! parameters for spin-up
    integer :: nloops                 ! the times of cycling the forcing to reach ss

    ! special parameters from reading file
    ! type spec_data_type
    real :: lat
    real :: lon
    real :: wsmax
    real :: wsmin
    real :: LAIMAX
    real :: LAIMIN
    real :: SLAx
    real :: rdepth
    real :: Rootmax
    real :: Stemmax
    real :: SapR
    real :: SapS
    real :: GLmax
    real :: GRmax
    real :: Gsmax
    real :: stom_n
    real :: a1
    real :: Ds0
    real :: Vcmax0            ! Jian: Vcmax0 and Vcmx0 is same? Vcmax0 is Vcmx0 in consts
    real :: extkU
    real :: xfang
    real :: alpha               
    real :: Tau_Leaf
    real :: Tau_Wood
    real :: Tau_Root          ! turnover rate of plant carbon pools : leaf, wood, root  
    real :: Tau_F
    real :: Tau_C             ! turnover rate of litter carbon pools: fine, coarse 
    real :: Tau_Micro
    real :: Tau_slowSOM
    real :: Tau_Passive       ! turnover rate of soil carbon pools  : fast, slow, passive 
    real :: gddonset
    real :: Q10
    real :: Q10rh             ! Q10rh modified from Ma et al.,2023 for aclimate study, change in transfer module of Q10h
    real :: Rl0
    real :: Rs0
    real :: Rr0
    ! added for parameters in methane module   
    real :: r_me
    real :: Q10pro
    real :: kCH4
    real :: Omax
    real :: CH4_thre
    real :: Tveg
    real :: Tpro_me
    real :: Toxi
    ! add based on Ma et al., 2022
    real :: f
    real :: bubprob
    real :: Vmaxfraction  
    ! add based on Ma et al., 2023
    real :: JV
    real :: Entrpy
    real :: etaL
    real :: etaW
    real :: etaR  ! etaL and etaR are not used.
    real :: f_F2M
    real :: f_C2M
    real :: f_C2S
    real :: f_M2S
    real :: f_M2P
    real :: f_S2P
    real :: f_S2M
    real :: f_P2M
    ! end type spec_data_type
    ! type(spec_data_type) :: spData
    
    ! parameters for forcing data -------------------------------------
    integer iforcing, nforcing, nHours, nDays, nMonths, nYears                                     ! for cycle
    integer iyear,  iday, ihour
    real    radsol, wind, co2ca, par, rain, RH
    real    tair, Dair, eairP, TairK, Tsoil                        ! Jian: not sure different between Dair and eairP. eairP means air water vapour pressure 
    ! integer, parameter :: nterms = 13, max_nlines=1500000           ! year doy hour Tair Tsoil RH VPD Rain WS PAR CO2
    
    type forcing_data_type
        INTEGER :: year
        INTEGER :: doy
        INTEGER :: hour
        real    :: Tair
        real    :: Tsoil
        real    :: RH                   ! Jian: RH seems confused in forcing and soil respiration
        real    :: VPD
        real    :: Rain
        real    :: WS
        real    :: PAR
        real    :: CO2
        real    :: PBOT                 ! unit patm Pa dynamic atmosphere pressure
        real    :: Ndep
    end type forcing_data_type

    type(forcing_data_type), allocatable, save :: forcing(:)
    
    ! type init_date_type
    real :: init_QC(8)
    real :: init_CN0(8)
    real :: init_NSCmin
    real :: init_Storage
    real :: init_nsc
    real :: init_accumulation
    real :: init_SNvcmax
    real :: init_N_deposit
    real :: init_alphaN
    real :: init_NSN
    real :: init_QNminer
    real :: init_N_deficit
    real :: init_thksl(10)
    real :: init_FRLEN(10)
    real :: init_liq_water(10)
    real :: init_fwsoil
    real :: init_topfws
    real :: init_omega
    real :: init_zwt
    real :: init_infilt
    real :: init_sftmp
    real :: init_Tsnow
    real :: init_Twater
    real :: init_Tice
    real :: G
    real :: init_snow_dsim
    real :: init_dcount
    real :: init_dcount_soil
    real :: init_ice_tw
    real :: init_Tsoill(10)
    real :: init_ice(10)
    real :: init_shcap_snow
    real :: init_condu_snow
    real :: init_condu_b
    real :: init_depth_ex
    real :: init_diff_s
    real :: init_diff_snow
    real :: init_albedo_snow
    real :: init_resht
    real :: init_thd_snow_depth
    real :: init_b_bound
    real :: init_infilt_rate
    real :: init_fa
    real :: init_fsub
    real :: init_rho_snow
    real :: init_decay_m
    real :: init_CH4_V(10)
    real :: init_CH4(10)
    real :: init_Vp(10)
    real :: init_bubble_methane_tot
    real :: init_Nbub
    real :: init_depth_1
    ! end type init_date_type

    ! type(init_date_type) initData


    ! constant parameters
    ! Sps is not assigned previous, something is wrong. -JJ
    real :: Sps = 1.                                ! scaling factors for growth, Jian: using in vegetation module. set it to 1
    integer, parameter :: nlayers = 10                ! how many
    real,    parameter :: pi      = 3.1415926
    ! physical constants
    real,    parameter :: tauL(3) = (/0.1, 0.425, 0.00/)          ! leaf transmittance for vis, for NIR, for thermal
    real,    parameter :: rhoL(3) = (/0.1, 0.425, 0.00/)          ! leaf reflectance for vis, for NIR, for thermal
    
    real,    parameter :: emleaf  = 0.96
    real,    parameter :: emsoil  = 0.94
    real,    parameter :: Rconst  = 8.314                         ! universal gas constant (J/mol)
    real,    parameter :: sigma   = 5.67e-8                       ! Steffan Boltzman constant (W/m2/K4)
    real,    parameter :: cpair   = 1010.                         ! heat capapcity of air (J/kg/K)
    real,    parameter :: Patm    = 101325. !1.e5                 ! atmospheric pressure  (Pa)
    real,    parameter :: Trefk   = 293.2                         ! reference temp K for Kc, Ko, Rd
    real,    parameter :: H2OLv0  = 2.501e6                       ! latent heat H2O (J/kg)
    real,    parameter :: AirMa   = 29.e-3                        ! mol mass air (kg/mol)
    real,    parameter :: H2OMw   = 18.e-3                        ! mol mass H2O (kg/mol)
    real,    parameter :: chi     = 0.93                          ! gbH/gbw
    real,    parameter :: Dheat   = 21.5e-6                       ! molecular diffusivity for heat
    ! plant parameters
    real,    parameter :: gsw0    = 1.0e-2                        ! g0 for H2O in BWB model
    real,    parameter :: theta   = 0.9
    real,    parameter :: wleaf   = 0.01                          ! leaf width (m)
    ! thermodynamic parameters for Kc and Ko (Leuning 1990)
    real,    parameter :: conKc0  = 302.e-6                       ! mol mol^-1
    real,    parameter :: conKo0  = 256.e-3                       ! mol mol^-1
    real,    parameter :: Ekc     = 59430.                        ! J mol^-1
    real,    parameter :: Eko     = 36000.                        ! J mol^-1
    ! Erd = 53000.                                  ! J mol^-1
    real,    parameter :: o2ci    = 210.e-3                       ! mol mol^-1
    ! thermodynamic parameters for Vcmax & Jmax (Eq 9, Harley et al, 1992; #1392)
    real,    parameter :: Eavm    = 116300.                       ! J/mol  (activation energy)
    real,    parameter :: Edvm    = 202900.                       ! J/mol  (deactivation energy)
    real,    parameter :: Eajm    = 79500.                        ! J/mol  (activation energy) 
    real,    parameter :: Edjm    = 201000.                       ! J/mol  (deactivation energy)
    ! real :: Entrpy  = 650.                          ! J/mol/K (entropy term, for Jmax & Vcmax)
    ! parameters for temperature dependence of gamma* (revised from von Caemmerer et al 1993)
    real,    parameter :: gam0    = 28.0e-6                       ! mol mol^-1 @ 20C = 36.9 @ 25C
    real,    parameter :: gam1    = .0509
    real,    parameter :: gam2    = .0010
    real,    parameter :: times_storage_use=3*720.   ! 720 hours, 30 days
    real :: rhoS(3) = (/0.1, 0.3,   0.00/)          ! soil reflectance for vis, for NIR, for thermal
    ! end of consts parameters -------------------------------------------------------------------

    ! summary of outputs
    type output_vars_type
        ! carbon fluxes (Kg C m-2 s-1)
        real :: gpp
        real :: nee
        real :: npp
        real :: nppLeaf
        real :: nppWood
        real :: nppStem
        real :: nppRoot
        real :: nppOther           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        real :: ra
        real :: raLeaf
        real :: raStem
        real :: raRoot
        real :: raOther
        real :: rMaint
        real :: rGrowth            ! maintenance respiration and growth respiration
        real :: rh
        real :: nbp                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        real :: wetlandCH4
        real :: wetlandCH4prod
        real :: wetlandCH4cons     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        real :: cLeaf
        real :: cStem
        real :: cRoot
        real :: cOther              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        real :: cLitter
        real :: cLitterCwd          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        real :: cSoil
        real :: cSoilLevels(nlayers)
        real :: cSoilFast
        real :: cSoilSlow
        real :: cSoilPassive           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        real :: CH4(nlayers)          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        real :: fBNF
        real :: fN2O
        real :: fNloss
        real :: fNnetmin
        real :: fNdep                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        real :: nLeaf
        real :: nStem
        real :: nRoot
        real :: nOther
        real :: nLitter
        real :: nLitterCwd
        real :: nSoil
        real :: nMineral                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        real :: hfls
        real :: hfss
        real :: SWnet
        real :: LWnet                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        real :: ec
        real :: tran
        real :: es                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        real :: hfsbl                   ! Snow sublimation
        real :: mrro
        real :: mrros
        real :: mrrob                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        real :: mrso(nlayers)           ! Kg m-2, soil moisture in each soil layer
        real :: tsl(nlayers)            ! K, soil temperature in each soil layer
        real :: tsland                  ! K, surface temperature
        real :: wtd                     ! m, Water table depth
        real :: snd                     ! m, Total snow depth
        real :: lai                     ! m2 m-2, Leaf area index  
    end type output_vars_type

    type(output_vars_type) :: outVars_h          ! hourly outputs
    type(output_vars_type) :: outVars_d          ! daily outputs
    type(output_vars_type) :: outVars_m          ! monthly outputs
    type(output_vars_type) :: outVars_y          ! yearly outputs

    type tot_output_vars_type
        ! carbon fluxes (Kg C m-2 s-1)
        real, allocatable :: gpp(:)
        real, allocatable :: nee(:)
        real, allocatable :: npp(:)
        real, allocatable :: nppLeaf(:)
        real, allocatable :: nppWood(:)
        real, allocatable :: nppStem(:)
        real, allocatable :: nppRoot(:)
        real, allocatable :: nppOther(:)           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        real, allocatable :: ra(:)
        real, allocatable :: raLeaf(:)
        real, allocatable :: raStem(:)
        real, allocatable :: raRoot(:)
        real, allocatable :: raOther(:)
        real, allocatable :: rMaint(:)
        real, allocatable :: rGrowth(:)            ! maintenance respiration and growth respiration
        real, allocatable :: rh(:)
        real, allocatable :: nbp(:)                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        real, allocatable :: wetlandCH4(:)
        real, allocatable :: wetlandCH4prod(:)
        real, allocatable :: wetlandCH4cons(:)     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        real, allocatable :: cLeaf(:)
        real, allocatable :: cStem(:)
        real, allocatable :: cRoot(:)
        real, allocatable :: cOther(:)              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        real, allocatable :: cLitter(:)
        real, allocatable :: cLitterCwd(:)          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        real, allocatable :: cSoil(:)
        real, allocatable :: cSoilLevels(:, :)
        real, allocatable :: cSoilFast(:)
        real, allocatable :: cSoilSlow(:)
        real, allocatable :: cSoilPassive(:)           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        real, allocatable :: CH4(:, :)          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        real, allocatable :: fBNF(:)
        real, allocatable :: fN2O(:)
        real, allocatable :: fNloss(:)
        real, allocatable :: fNnetmin(:)
        real, allocatable :: fNdep(:)                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        real, allocatable :: nLeaf(:)
        real, allocatable :: nStem(:)
        real, allocatable :: nRoot(:)
        real, allocatable :: nOther(:)
        real, allocatable :: nLitter(:)
        real, allocatable :: nLitterCwd(:)
        real, allocatable :: nSoil(:)
        real, allocatable :: nMineral(:)                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        real, allocatable :: hfls(:)
        real, allocatable :: hfss(:)
        real, allocatable :: SWnet(:)
        real, allocatable :: LWnet(:)                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        real, allocatable :: ec(:)
        real, allocatable :: tran(:)
        real, allocatable :: es(:)                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        real, allocatable :: hfsbl(:)                   ! Snow sublimation
        real, allocatable :: mrro(:)
        real, allocatable :: mrros(:)
        real, allocatable :: mrrob(:)                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        real, allocatable :: mrso(:, :)           ! Kg m-2, soil moisture in each soil layer
        real, allocatable :: tsl(:, :)            ! K, soil temperature in each soil layer
        real, allocatable :: tsland(:)                  ! K, surface temperature
        real, allocatable :: wtd(:)                     ! m, Water table depth
        real, allocatable :: snd(:)                     ! m, Total snow depth
        real, allocatable :: lai(:)                     ! m2 m-2, Leaf area index            
    end type tot_output_vars_type
    
    type(tot_output_vars_type) :: tot_outVars_h      ! hourly outputs
    type(tot_output_vars_type) :: tot_outVars_d      ! daily outputs
    type(tot_output_vars_type) :: tot_outVars_m      ! monthly outputs
    type(tot_output_vars_type) :: tot_outVars_y      ! yearly outputs
    type(tot_output_vars_type) :: tot_outVars_spinup ! for spinup yearly results

    ! physical
    real    :: raero                                   ! aerodynamic resistance
    real    :: Rsoilab1, Rsoilab2, Rsoilab3, Rsoilabs  ! calculate in xlayer and used in soil module, QLsoil and Rsoilab3, Rsoilabs seem be calculated both xlayers and soil T dynamic?
    real    :: QLair, QLleaf, QLsoil
    real    :: rhocp, H2OLv, slope, psyc, Cmolar, fw1  ! thermodynamic parameters for air. Maybe calculate in module (veg and soil)?
    real    :: Rsoil, rLAI
    real    :: Hsoil, Hcanop                           ! Sensible heat flux; Hcanop seems not used?
    real    :: Esoil
    real    :: tsoil_layer(11)                         ! not sure why 11 layers?
    real    :: sftmp, Tsnow, Twater, Tice
    real    :: Tsoill(10), rain_d
    ! vegetation
    ! photosynsis
    real :: eJmx0   != Vcmax0*2.7                   ! @20C Leuning 1996 from Wullschleger (1993)
    real :: Vcmx0    
    ! phonology related parameters
    integer :: pheno
    real    :: GDD5
    integer :: onset                                       !flag of phenological stage
    integer :: phenoset
    real    :: ta
    ! NSC relative
    real    :: NSCmin, fnsc, nsc, NSCmax 
    real    :: store, add
    real    :: stor_use, storage, accumulation
    ! Growth
    real    :: L_fall                             ! leaf fall
    real    :: alpha_L, alpha_W, alpha_R          ! allocation fraction to Leaf, stem, and Root             
    real    :: flait                              ! Jian: LAI relavant variable from vegetable to energy
    real    :: StemSap, RootSap
    real    :: LAI, SLA
    real    :: bmroot, bmstem, bmleaf, bmplant
    ! vegetation flux
    real    :: GPP
    real    :: NPP, NPP_L, NPP_W, NPP_R
    real    :: Rauto
    real    :: Rmain,   RmLeaf, RmStem, RmRoot    ! maintanence respiration
    real    :: Rgrowth, RgLeaf, RgStem, RgRoot    ! growth respiration
    ! water cycle
    real :: evap, transp, ET

    ! soil carbon flux
    real :: Rhetero, Rh_pools(5)              ! heterotrophic respiration
    ! soil water
    real :: thksl(10), depth(nlayers)
    real :: wcl(10),   wsc(10)    ! wsc is the output from soil water module
    real :: FRLEN(10) 
    real :: runoff 
    real :: ice_tw, water_tw 
    real :: ice(10), liq_water(10) 
    real :: zwt, phi, Sw
    real :: WILTPT,FILDCP,infilt
    ! .. init from soil thermal module
    real :: diff_snow, diff_s, condu_b
    real :: depth_ex 
    real :: infilt_rate
    real :: fa,fsub,rho_snow,decay_m   
    real :: fwsoil,topfws,omega 
    ! snow process
    real :: snow_depth, snow_depth_e, snow_dsim
    real :: sublim                                 ! snow sublimation
    real :: melt, dcount, dcount_soil
    real :: shcap_snow, condu_snow
    real :: albedo_snow, resht, thd_snow_depth, b_bound
    ! ecosystem flux
    real :: NEE, NEP
    ! pool state
    real :: QC(8), OutC(8), testout(11)  !  leaf,wood,root,fine lit.,coarse lit.,Micr,Slow,Pass
    real :: TauC(8)

    ! Nitrogen
    real   :: SNvcmax,SNgrowth,SNRauto,SNrs            ! nitrogen relavent scalars
    real   :: Rnitrogen       ! respiration
    real   :: N_uptake, N_fixation, N_deposit
    real   :: N_loss,   N_leach, N_vol
    real   :: fNnetmin, N_transfer
    real   :: N_leaf, N_wood, N_root
    real   :: N_LF, N_WF, N_RF
    real   :: QNplant, QNminer
    real   :: QN(8), CN0(8),  CN(8), OutN(8)
    real   :: N_miner, alphaN
    real   :: NSN, N_deficit
    
    ! methane
    real :: ProCH4(nlayers), Pro_sum
    real :: OxiCH4(nlayers), Oxi_sum                    ! CH4 oxidation
    real :: Fdifu(nlayers)                              ! modified based on Ma et al.,2022
    real :: Vp(nlayers), pwater(nlayers),presP(nlayers) ! add based on Ma et al., 2022
    real :: methanebP(nlayers), methaneP(nlayers)
    real :: Rgas
    real :: bubble_methane_tot, Nbub
    real :: dpatm                          ! used only in methane ebullition
    real :: Ebu_sum_unsat,Ebu_sum_sat
    real :: CH4(nlayers), CH4_V(nlayers), CH4V_d(nlayers)
    real :: simuCH4, Ebu_sum, Pla_sum
    

    ! matrix variables
    real mat_B(8,1), mat_A(8,8), mat_e(8,8), mat_k(8,8), mat_x(8,1), mat_Rh, mat_Rh_d ! for matrix
    
    ! alternative input variable
    real,DIMENSION(:), ALLOCATABLE :: snow_in       ! if not run snow process, then read from the input file

    ! ==============================================================================================
    contains

    subroutine read_TECO_model_configs()
        ! read the "TECO_model_configs.nml"
        implicit none
        integer io

        namelist /nml_simu_settings/ simu_name, do_spinup, do_mcmc, do_snow,          & 
            do_soilphy, do_matrix, do_EBG, do_restart, do_ndep, do_simu, do_leap,     &
            do_out_hr, do_out_day, do_out_mon, do_out_yr,                             &
            dtimes,nSpecParams, filepath_in, climfile, snowdepthfile, watertablefile, &
            restartfile, outdir 
        namelist /nml_exps/ Ttreat, CO2treat, N_fert
        namelist /nml_params/ lat, lon, wsmax, wsmin, LAIMAX, LAIMIN, rdepth,        & 
            Rootmax, Stemmax, SapR, SapS, SLAx, GLmax, GRmax, Gsmax, stom_n,         &
            a1, Ds0, Vcmax0, extkU, xfang, alpha, Tau_Leaf, Tau_Wood, Tau_Root,      &
            Tau_F, Tau_C, Tau_Micro, Tau_SlowSOM, Tau_Passive, gddonset, Q10,Q10rh,  &
            Rl0, Rs0, Rr0, r_me, Q10pro, kCH4, Omax, CH4_thre, Tveg, Tpro_me,        & 
            Toxi, f, bubprob, Vmaxfraction, JV, Entrpy, etaL, etaW, etaR, f_F2M,     &
            f_C2M, f_C2S, f_M2S, f_M2P, f_S2P, f_S2M, f_P2M
        namelist /nml_initial_states/ init_QC, init_CN0, init_NSCmin, init_Storage,  &
            init_nsc, init_accumulation, init_SNvcmax, init_N_deposit, init_alphaN,  & 
            init_NSN, init_QNminer, init_N_deficit, init_thksl, init_FRLEN,          &
            init_liq_water, init_fwsoil, init_topfws, init_omega, init_zwt,          &
            init_infilt, init_sftmp, init_Tsnow, init_Twater, init_Tice, G,          &
            init_snow_dsim, init_dcount, init_dcount_soil, init_ice_tw, init_Tsoill, &
            init_ice, init_shcap_snow, init_condu_snow, init_condu_b, init_depth_ex, & 
            init_diff_s, init_diff_snow, init_albedo_snow, init_resht,               &
            init_thd_snow_depth, init_b_bound, init_infilt_rate, init_fa, init_fsub, &
            init_rho_snow, init_decay_m, init_CH4_V, init_CH4, init_Vp,              &
            init_bubble_methane_tot, init_Nbub, init_depth_1
        namelist /nml_spinup/ nloops 

        print *, "# read config nml..."
        open(388, file="TECO_model_configs.nml")
        read(388, nml=nml_simu_settings, iostat=io)
        read(388, nml=nml_exps, iostat=io)
        read(388, nml=nml_params, iostat=io)
        read(388, nml=nml_initial_states, iostat=io)
        read(388, nml=nml_spinup, iostat=io)
        close(388)

        ! write(*,*) "lat = ",   lat
        ! write(*,*) "lon = ",   lon
        ! write(*,*) "wsmax = ", wsmax
        ! write(*,*) "wsmin = ",wsmin
        ! write(*,*) "LAIMAX = ",LAIMAX
        ! write(*,*) "LAIMIN = ",LAIMIN
        ! write(*,*) "SLAx",SLAx
        ! write(*,*) "rdepth",rdepth
        ! write(*,*) "Rootmax",Rootmax
        ! write(*,*) "Stemmax",Stemmax
        ! write(*,*) "SapR",SapR
        ! write(*,*) "SapS",SapS
        ! write(*,*) "GLmax",GLmax
        ! write(*,*) "GRmax",GRmax
        ! write(*,*) "Gsmax",Gsmax
        ! write(*,*) "stom_n",stom_n
        ! write(*,*) "a1",a1
        ! write(*,*) "Ds0",Ds0
        ! write(*,*) "Vcmax0",Vcmax0            ! Jian: Vcmax0 and Vcmx0 is same? Vcmax0 is Vcmx0 in consts
        ! write(*,*) "extkU",extkU
        ! write(*,*) "xfang",xfang
        ! write(*,*) "alpha",alpha             
        ! write(*,*) "Tau_Leaf",Tau_Leaf
        ! write(*,*) "Tau_Wood",Tau_Wood
        ! write(*,*) "Tau_Root",Tau_Root          ! turnover rate of plant carbon pools : leaf, wood, root  
        ! write(*,*) "Tau_F",Tau_F
        ! write(*,*) "Tau_C",Tau_C             ! turnover rate of litter carbon pools: fine, coarse 
        ! write(*,*) "Tau_Micro",Tau_Micro
        ! write(*,*) "Tau_slowSOM",Tau_slowSOM
        ! write(*,*) "Tau_Passive",Tau_Passive       ! turnover rate of soil carbon pools  : fast, slow, passive 
        ! write(*,*) "gddonset",gddonset
        ! write(*,*) "Q10",Q10
        ! write(*,*) "Q10rh",Q10rh            ! Q10rh modified from Ma et al.,2023 for aclimate study, change in transfer module of Q10h
        ! write(*,*) "Rl0",Q10rh
        ! write(*,*) "Rs0",Rs0
        ! write(*,*) "Rr0",Rr0
        ! ! added for parameters in methane module   
        ! write(*,*) "r_me",r_me
        ! write(*,*) "Q10pro",Q10pro
        ! write(*,*) "kCH4",kCH4
        ! write(*,*) "Omax",Omax
        ! write(*,*) "CH4_thre",CH4_thre
        ! write(*,*) "Tveg",Tveg
        ! write(*,*) "Tpro_me",Tpro_me
        ! write(*,*) "Toxi",Toxi
        ! ! add based on Ma et al., 2022
        ! write(*,*) "f",f
        ! write(*,*) "bubprob",bubprob
        ! write(*,*) "Vmaxfraction",Vmaxfraction
        ! ! add based on Ma et al., 2023
        ! write(*,*) "JV",JV
        ! write(*,*) "Entrpy",Entrpy
        ! write(*,*) "etaL",etaL
        ! write(*,*) "etaW",etaW
        ! write(*,*) "etaR",etaR  ! etaL and etaR are not used.
        ! write(*,*) "f_F2M",f_F2M
        ! write(*,*) "f_C2M",f_C2M
        ! write(*,*) "f_C2S",f_C2S
        ! write(*,*) "f_M2S",f_M2S
        ! write(*,*) "f_M2P",f_M2P
        ! write(*,*) "f_S2P",f_S2P
        ! write(*,*) "f_S2M",f_S2M
        ! write(*,*) "f_P2M",f_P2M
        
    end subroutine read_TECO_model_configs

    subroutine initialize()
        implicit none
        integer i
        ! after having read the TECO_model_configs.nml => spData
        eJmx0          = Vcmax0*2.7                     ! @20C Leuning 1996 from Wullschleger (1993)
        QC             = init_QC                   !  leaf,wood,root,fine lit.,coarse lit.,Micr,Slow,Pass
        CN0            = init_CN0
        NSCmin         = init_NSCmin                                                 ! none structural carbon pool
        Storage        = init_Storage                                              ! g C/m2
        nsc            = init_nsc
        stor_use       = Storage/times_storage_use                                       ! 720 hours, 30 days
        N_deposit      = init_N_deposit/8760. ! Nitrogen input (gN/h/m2, )
         
        TauC           = (/Tau_Leaf,Tau_Wood,Tau_Root,Tau_F, &
                           Tau_C,Tau_Micro,Tau_slowSOM,Tau_Passive/)*8760.
        SLA            = SLAx/10000.          ! Convert unit from cm2/g to m2/g
        GLmax          = GLmax/8760.          ! growth rates of plant. Jian: per year to per hour ?
        GRmax          = GRmax/8760.
        Gsmax          = Gsmax/8760.
        accumulation   = init_accumulation               ! accumulative storage C?
        SNvcmax        = init_SNvcmax
        LAI            = LAIMIN
        bmleaf         = QC(1)/0.48
        bmstem         = QC(2)/0.48
        bmroot         = QC(3)/0.48
        bmplant        = bmstem + bmroot + bmleaf
        ! initial values of Nitrogen pools and C/N ratio
        alphaN         = init_alphaN     ! the transfer of N before littering
        NSN            = init_NSN        ! 0.35   ! 6.0 ! 0.35 according to Ma et al., 2022
        QNminer        = init_QNminer    ! 1.2
        N_deficit      = init_N_deficit  ! 0.
        CN             = CN0
        QN             = QC/CN0
        QNplant        = QN(1) + QN(2) + QN(3)
        ! -----------------------------------------------------------------------------------------

        ! for soil conditions, physical processes
        thksl          = init_thksl        ! thickness of every soil layer 
        FRLEN          = init_FRLEN        ! ratio of roots in every layer, Oak Ridge FACE: Shuang
        liq_water      = init_liq_water    ! unit m
        fwsoil         = init_fwsoil       ! update in soilwater module
        topfws         = init_topfws
        omega          = init_omega
        do i=1,10
            wcl(i)     = wsmax/100.
        enddo 
        zwt            = init_zwt
        water_tw       = zwt*0.001 
        WILTPT         = wsmin/100.0
        FILDCP         = wsmax/100.0
        infilt         = init_infilt
        ! soil thermal dynamics in Yuanyuanversion
        sftmp          = init_sftmp
        Tsnow          = init_Tsnow
        Twater         = init_Twater
        Tice           = init_Tice
        G              = G
        Esoil          = 0.5*G
        snow_dsim      = init_snow_dsim
        dcount         = init_dcount
        dcount_soil    = init_dcount_soil
        ice_tw         = init_ice_tw
        Tsoill         = init_Tsoill ! JJ MS thksl 10 20 30 40 50 70 90 110 130 150...  
        ice            = init_ice
        shcap_snow     = init_shcap_snow  ! tuneice worker better
        condu_snow     = init_condu_snow
        condu_b        = init_condu_b     ! yuanyuan soil thermal version value  ... int: this par is not sensitive to CWE
        depth_ex       = init_depth_ex
        diff_s         = init_diff_s
        diff_snow      = init_diff_snow              ! .. int diffusivity of snow not sensitive for ice
        albedo_snow    = init_albedo_snow
        resht          = init_resht
        thd_snow_depth = init_thd_snow_depth
        b_bound        = init_b_bound                            ! b_bound=0.1     !tuneice  not sensitive for ice
        infilt_rate    = init_infilt_rate
        fa             = init_fa
        fsub           = init_fsub
        rho_snow       = init_rho_snow        !tuneice
        decay_m        = init_decay_m         !aging factor on snow melting
        ! methane module. update: Shuang methane bog species even more shallowly rooted than the tundra. add initials for methane module Shuang version
        CH4_V          = init_CH4_V
        CH4            = init_CH4
        ! #1.EBG put this paragraph outside of the time loop, initialization step
        Vp             = init_Vp  !assume in the very beginning no bubbles exist in the first three layers (30cm)
        bubble_methane_tot  = init_bubble_methane_tot
        Nbub                = init_Nbub
        depth(1)            = init_depth_1              !calculate soil depth unit cm
        do i=2,nlayers
            depth(i)=depth(i-1)+THKSL(i)
        enddo

        do i=1,nlayers
            if (depth(i) .le. (-zwt)*0.1) then
                pwater(i) = 1000*9.81*(depth(i)*0.01-(-zwt)*0.001)
            else
                pwater(i) = 0.
            endif
            presP(i) = 101325 + pwater(i)  ! unit Pa
            methanebP(i) = f * presP(i) * Vp(i)/(8.3144621 * (Tsoill(i)+273.15))  !unit mol/layer
            methaneP(i) = CH4(i)/12
            ! gC/layer  /12   unit molC/layer
        enddo

        do i = 1,8
            mat_x(i,1) = QC(i)
        end do
    end subroutine initialize

    subroutine initialize_with_restart()
        implicit none
        integer i
        bmleaf         = QC(1)/0.48
        bmstem         = QC(2)/0.48
        bmroot         = QC(3)/0.48
        bmplant        = bmstem+bmroot+bmleaf
        QNplant        = QN(1) + QN(2) + QN(3)

        do i = 1,8
            mat_x(i,1) = QC(i)
        end do
    end subroutine initialize_with_restart

    subroutine init_hourly()
        implicit none
        ! Jian: whether it needs to define?
    end subroutine init_hourly

    subroutine init_day()
        implicit none
        rain_d                     = 0.
        outVars_d%gpp              = 0.
        outVars_d%nee              = 0.
        outVars_d%npp              = 0.
        outVars_d%nppLeaf          = 0.
        outVars_d%nppWood          = 0.
        outVars_d%nppStem          = 0.
        outVars_d%nppRoot          = 0.
        outVars_d%nppOther         = 0.  
        outVars_d%ra               = 0.
        outVars_d%raLeaf           = 0.
        outVars_d%raStem           = 0.
        outVars_d%raRoot           = 0.
        outVars_d%raOther          = 0.
        outVars_d%rMaint           = 0.
        outVars_d%rGrowth          = 0.
        outVars_d%rh               = 0.
        outVars_d%nbp              = 0.
        outVars_d%wetlandCH4       = 0.
        outVars_d%wetlandCH4prod   = 0.
        outVars_d%wetlandCH4cons   = 0. 
        ! Carbon Pools  (KgC m-2)
        outVars_d%cLeaf            = 0.
        outVars_d%cStem            = 0.
        outVars_d%cRoot            = 0.
        outVars_d%cOther           = 0.
        outVars_d%cLitter          = 0.
        outVars_d%cLitterCwd       = 0.  
        outVars_d%cSoil            = 0.
        outVars_d%cSoilLevels      = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        outVars_d%cSoilFast        = 0.
        outVars_d%cSoilSlow        = 0.
        outVars_d%cSoilPassive     = 0. 
        outVars_d%CH4             = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        ! Nitrogen fluxes (kgN m-2 s-1)
        outVars_d%fBNF             = 0.
        outVars_d%fN2O             = 0.
        outVars_d%fNloss           = 0.
        outVars_d%fNnetmin         = 0.
        outVars_d%fNdep            = 0.  
        ! Nitrogen pools (kgN m-2)
        outVars_d%nLeaf            = 0.
        outVars_d%nStem            = 0.
        outVars_d%nRoot            = 0.
        outVars_d%nOther           = 0.
        outVars_d%nLitter          = 0.
        outVars_d%nLitterCwd       = 0.
        outVars_d%nSoil            = 0.
        outVars_d%nMineral         = 0. 
        ! energy fluxes (W m-2)
        outVars_d%hfls             = 0.
        outVars_d%hfss             = 0.
        outVars_d%SWnet            = 0.
        outVars_d%LWnet            = 0.
        ! water fluxes (kg m-2 s-1)
        outVars_d%ec               = 0.
        outVars_d%tran             = 0.
        outVars_d%es               = 0.   
        outVars_d%hfsbl            = 0.  
        outVars_d%mrro             = 0.
        outVars_d%mrros            = 0.
        outVars_d%mrrob            = 0.   
        ! other
        outVars_d%mrso             = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)  
        outVars_d%tsl              = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        outVars_d%tsland           = 0.                 
        outVars_d%wtd              = 0.           
        outVars_d%snd              = 0.           
        outVars_d%lai              = 0.
    end subroutine init_day

    subroutine init_monthly()
        implicit none
        ! Jian: we need add the monthly outputs according to the SPRUCE-MIP's requirement.
        ! carbon result:
        outVars_m%gpp              = 0.
        outVars_m%nee              = 0.
        outVars_m%npp              = 0.
        outVars_m%nppLeaf          = 0.
        outVars_m%nppWood          = 0.
        outVars_m%nppStem          = 0.
        outVars_m%nppRoot          = 0.
        outVars_m%nppOther         = 0.  
        outVars_m%ra               = 0.
        outVars_m%raLeaf           = 0.
        outVars_m%raStem           = 0.
        outVars_m%raRoot           = 0.
        outVars_m%raOther          = 0.
        outVars_m%rMaint           = 0.
        outVars_m%rGrowth          = 0.
        outVars_m%rh               = 0.
        outVars_m%nbp              = 0.
        outVars_m%wetlandCH4       = 0.
        outVars_m%wetlandCH4prod   = 0.
        outVars_m%wetlandCH4cons   = 0. 
        ! Carbon Pools  (KgC m-2)
        outVars_m%cLeaf            = 0.
        outVars_m%cStem            = 0.
        outVars_m%cRoot            = 0.
        outVars_m%cOther           = 0.
        outVars_m%cLitter          = 0.
        outVars_m%cLitterCwd       = 0.  
        outVars_m%cSoil            = 0.
        outVars_m%cSoilLevels      = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        outVars_m%cSoilFast        = 0.
        outVars_m%cSoilSlow        = 0.
        outVars_m%cSoilPassive     = 0. 
        outVars_m%CH4             = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        ! Nitrogen fluxes (kgN m-2 s-1)
        outVars_m%fBNF             = 0.
        outVars_m%fN2O             = 0.
        outVars_m%fNloss           = 0.
        outVars_m%fNnetmin         = 0.
        outVars_m%fNdep            = 0.  
        ! Nitrogen pools (kgN m-2)
        outVars_m%nLeaf            = 0.
        outVars_m%nStem            = 0.
        outVars_m%nRoot            = 0.
        outVars_m%nOther           = 0.
        outVars_m%nLitter          = 0.
        outVars_m%nLitterCwd       = 0.
        outVars_m%nSoil            = 0.
        outVars_m%nMineral         = 0. 
        ! energy fluxes (W m-2)
        outVars_m%hfls             = 0.
        outVars_m%hfss             = 0.
        outVars_m%SWnet            = 0.
        outVars_m%LWnet            = 0.
        ! water fluxes (kg m-2 s-1)
        outVars_m%ec               = 0.
        outVars_m%tran             = 0.
        outVars_m%es               = 0.   
        outVars_m%hfsbl            = 0.  
        outVars_m%mrro             = 0.
        outVars_m%mrros            = 0.
        outVars_m%mrrob            = 0.   
        ! other
        outVars_m%mrso             = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)  
        outVars_m%tsl              = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        outVars_m%tsland           = 0.                 
        outVars_m%wtd              = 0.           
        outVars_m%snd              = 0.           
        outVars_m%lai              = 0.
    end subroutine init_monthly

    subroutine init_update_year()
        GDD5      = 0.0 
        onset     = 0
        phenoset  = 0
        ! carbon fluxes (Kg C m-2 s-1)
        outVars_y%gpp              = 0.
        outVars_y%nee              = 0.
        outVars_y%npp              = 0.
        outVars_y%nppLeaf          = 0.
        outVars_y%nppWood          = 0.
        outVars_y%nppStem          = 0.
        outVars_y%nppRoot          = 0.
        outVars_y%nppOther         = 0.  
        outVars_y%ra               = 0.
        outVars_y%raLeaf           = 0.
        outVars_y%raStem           = 0.
        outVars_y%raRoot           = 0.
        outVars_y%raOther          = 0.
        outVars_y%rMaint           = 0.
        outVars_y%rGrowth          = 0.
        outVars_y%rh               = 0.
        outVars_y%nbp              = 0.
        outVars_y%wetlandCH4       = 0.
        outVars_y%wetlandCH4prod   = 0.
        outVars_y%wetlandCH4cons   = 0. 
        ! Carbon Pools  (KgC m-2)
        outVars_y%cLeaf            = 0.
        outVars_y%cStem            = 0.
        outVars_y%cRoot            = 0.
        outVars_y%cOther           = 0.
        outVars_y%cLitter          = 0.
        outVars_y%cLitterCwd       = 0.  
        outVars_y%cSoil            = 0.
        outVars_y%cSoilLevels      = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        outVars_y%cSoilFast        = 0.
        outVars_y%cSoilSlow        = 0.
        outVars_y%cSoilPassive     = 0. 
        outVars_y%CH4             = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        ! Nitrogen fluxes (kgN m-2 s-1)
        outVars_y%fBNF             = 0.
        outVars_y%fN2O             = 0.
        outVars_y%fNloss           = 0.
        outVars_y%fNnetmin         = 0.
        outVars_y%fNdep            = 0.  
        ! Nitrogen pools (kgN m-2)
        outVars_y%nLeaf            = 0.
        outVars_y%nStem            = 0.
        outVars_y%nRoot            = 0.
        outVars_y%nOther           = 0.
        outVars_y%nLitter          = 0.
        outVars_y%nLitterCwd       = 0.
        outVars_y%nSoil            = 0.
        outVars_y%nMineral         = 0. 
        ! energy fluxes (W m-2)
        outVars_y%hfls             = 0.
        outVars_y%hfss             = 0.
        outVars_y%SWnet            = 0.
        outVars_y%LWnet            = 0.
        ! water fluxes (kg m-2 s-1)
        outVars_y%ec               = 0.
        outVars_y%tran             = 0.
        outVars_y%es               = 0.   
        outVars_y%hfsbl            = 0.  
        outVars_y%mrro             = 0.
        outVars_y%mrros            = 0.
        outVars_y%mrrob            = 0.   
        ! other
        outVars_y%mrso             = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)  
        outVars_y%tsl              = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        outVars_y%tsland           = 0.                 
        outVars_y%wtd              = 0.           
        outVars_y%snd              = 0.           
        outVars_y%lai              = 0.
    end subroutine init_update_year
    
    subroutine get_forcingdata()
        implicit none
        integer STAT, COUNT
        character(50) commts
        ! define variable for each line
        integer :: tmp_yr, tmp_doy, tmp_h
        real    :: tmp_Ta, tmp_Ts,  tmp_rh, tmp_vpd, tmp_rain, tmp_ws 
        real    :: tmp_par, tmp_co2, tmp_pbot, tmp_ndep

        call ReadLineNumFromFile(climfile, nforcing)  ! get the line number

        allocate(forcing(nforcing))                   ! allocate the array

        COUNT = 0
        OPEN(1,FILE=climfile,status='old',ACTION='read',IOSTAT=STAT)
        read(1,'(a160)') commts
        DO WHILE (.TRUE.)
            COUNT=COUNT+1
            READ(1,*,IOSTAT=STAT, end=993) tmp_yr, tmp_doy, tmp_h,             &
                tmp_Ta,  tmp_Ts,  tmp_rh, tmp_vpd, tmp_rain, tmp_ws, & 
                tmp_par, tmp_co2, tmp_pbot, tmp_ndep
            IF(STAT .NE. 0) EXIT
            forcing(COUNT)%year  = tmp_yr
            forcing(COUNT)%doy   = tmp_doy
            forcing(COUNT)%hour  = tmp_h
            forcing(COUNT)%Tair  = tmp_Ta
            forcing(COUNT)%Tsoil = tmp_Ts
            forcing(COUNT)%RH    = tmp_rh
            forcing(COUNT)%VPD   = tmp_vpd
            forcing(COUNT)%Rain  = tmp_rain
            forcing(COUNT)%WS    = tmp_ws
            forcing(COUNT)%PAR   = tmp_par
            forcing(COUNT)%CO2   = tmp_co2
            forcing(COUNT)%PBOT  = tmp_pbot
            forcing(COUNT)%Ndep  = tmp_ndep
        ENDDO
993     continue
        CLOSE(1)
    end subroutine get_forcingdata

    subroutine get_snowdepth()
        implicit none
        ! real temp_snow_depth(max_nlines)
        integer STAT, COUNT, nrow
        character(50) commts

        ! integer m,n,istat1,lines,yr_length
        real snow_depth_read
        integer tmp_yr, tmp_doy, tmp_hr

        call ReadLineNumFromFile(snowdepthfile, nrow)  ! get the line number
        allocate(snow_in(nrow))

        open(11,file = snowdepthfile, status ='old',ACTION='read', IOSTAT=STAT)
        read(11,'(a160)') commts ! skip 2 lines of input met data file
        COUNT = 0
        do
            COUNT = COUNT + 1
            read (11,*,IOSTAT=STAT, end=1018) tmp_yr,tmp_doy,tmp_hr,snow_depth_read
            IF(STAT .NE. 0) EXIT
            snow_in(COUNT)=snow_depth_read     
        enddo
1018    continue
        close(11)    ! close snow depth file
        return
    end subroutine get_snowdepth

    subroutine ReadLineNumFromFile(filepath, count_lines)
        implicit none
        character(len=*), intent(in) :: filepath
        character(len=100) header, line
        integer STAT, count_lines

        open(38, file=trim(filepath), status="old", action="read", iostat=STAT) ! open file
        read(38, '(a100)') header           ! read the header of the file
        count_lines = 0                     ! initilize the count_lines
        do while(.TRUE.)
            read(38, *, iostat=STAT) line   ! read each line
            if(STAT .ne. 0) exit            ! until the end of the file
            count_lines = count_lines + 1   ! recording the count of the lines
        enddo
        return
    end subroutine ReadLineNumFromFile


    subroutine assign_all_results(hours, days, months, years)
        implicit none
        integer hours, days, months, years
        allocate(tot_outVars_h%gpp(hours))
        allocate(tot_outVars_h%nee(hours))
        allocate(tot_outVars_h%npp(hours))
        allocate(tot_outVars_h%nppLeaf(hours))
        allocate(tot_outVars_h%nppWood(hours))
        allocate(tot_outVars_h%nppStem(hours))
        allocate(tot_outVars_h%nppRoot(hours))
        allocate(tot_outVars_h%nppOther(hours))           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(tot_outVars_h%ra(hours))
        allocate(tot_outVars_h%raLeaf(hours))
        allocate(tot_outVars_h%raStem(hours))
        allocate(tot_outVars_h%raRoot(hours))
        allocate(tot_outVars_h%raOther(hours))
        allocate(tot_outVars_h%rMaint(hours))
        allocate(tot_outVars_h%rGrowth(hours))            ! maintenance respiration and growth respiration
        allocate(tot_outVars_h%rh(hours))
        allocate(tot_outVars_h%nbp(hours))                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(tot_outVars_h%wetlandCH4(hours))
        allocate(tot_outVars_h%wetlandCH4prod(hours))
        allocate(tot_outVars_h%wetlandCH4cons(hours))     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(tot_outVars_h%cLeaf(hours))
        allocate(tot_outVars_h%cStem(hours))
        allocate(tot_outVars_h%cRoot(hours))
        allocate(tot_outVars_h%cOther(hours))              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(tot_outVars_h%cLitter(hours))
        allocate(tot_outVars_h%cLitterCwd(hours))          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(tot_outVars_h%cSoil(hours))
        allocate(tot_outVars_h%cSoilLevels(hours, nlayers))
        allocate(tot_outVars_h%cSoilFast(hours))
        allocate(tot_outVars_h%cSoilSlow(hours))
        allocate(tot_outVars_h%cSoilPassive(hours))           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(tot_outVars_h%CH4(hours, nlayers))          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(tot_outVars_h%fBNF(hours))
        allocate(tot_outVars_h%fN2O(hours))
        allocate(tot_outVars_h%fNloss(hours))
        allocate(tot_outVars_h%fNnetmin(hours))
        allocate(tot_outVars_h%fNdep(hours))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(tot_outVars_h%nLeaf(hours))
        allocate(tot_outVars_h%nStem(hours))
        allocate(tot_outVars_h%nRoot(hours))
        allocate(tot_outVars_h%nOther(hours))
        allocate(tot_outVars_h%nLitter(hours))
        allocate(tot_outVars_h%nLitterCwd(hours))
        allocate(tot_outVars_h%nSoil(hours))
        allocate(tot_outVars_h%nMineral(hours))                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(tot_outVars_h%hfls(hours))
        allocate(tot_outVars_h%hfss(hours))
        allocate(tot_outVars_h%SWnet(hours))
        allocate(tot_outVars_h%LWnet(hours))                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(tot_outVars_h%ec(hours))
        allocate(tot_outVars_h%tran(hours))
        allocate(tot_outVars_h%es(hours))                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(tot_outVars_h%hfsbl(hours))                   ! Snow sublimation
        allocate(tot_outVars_h%mrro(hours))
        allocate(tot_outVars_h%mrros(hours))
        allocate(tot_outVars_h%mrrob(hours))                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        allocate(tot_outVars_h%mrso(hours, nlayers))           ! Kg m-2, soil moisture in each soil layer
        allocate(tot_outVars_h%tsl(hours, nlayers))            ! K, soil temperature in each soil layer
        allocate(tot_outVars_h%tsland(hours))                  ! K, surface temperature
        allocate(tot_outVars_h%wtd(hours))                     ! m, Water table depth
        allocate(tot_outVars_h%snd(hours))                     ! m, Total snow depth
        allocate(tot_outVars_h%lai(hours))

        ! daily results 
        allocate(tot_outVars_d%gpp(days))
        allocate(tot_outVars_d%nee(days))
        allocate(tot_outVars_d%npp(days))
        allocate(tot_outVars_d%nppLeaf(days))
        allocate(tot_outVars_d%nppWood(days))
        allocate(tot_outVars_d%nppStem(days))
        allocate(tot_outVars_d%nppRoot(days))
        allocate(tot_outVars_d%nppOther(days))           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(tot_outVars_d%ra(days))
        allocate(tot_outVars_d%raLeaf(days))
        allocate(tot_outVars_d%raStem(days))
        allocate(tot_outVars_d%raRoot(days))
        allocate(tot_outVars_d%raOther(days))
        allocate(tot_outVars_d%rMaint(days))
        allocate(tot_outVars_d%rGrowth(days))            ! maintenance respiration and growth respiration
        allocate(tot_outVars_d%rh(days))
        allocate(tot_outVars_d%nbp(days))                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(tot_outVars_d%wetlandCH4(days))
        allocate(tot_outVars_d%wetlandCH4prod(days))
        allocate(tot_outVars_d%wetlandCH4cons(days))     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(tot_outVars_d%cLeaf(days))
        allocate(tot_outVars_d%cStem(days))
        allocate(tot_outVars_d%cRoot(days))
        allocate(tot_outVars_d%cOther(days))              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(tot_outVars_d%cLitter(days))
        allocate(tot_outVars_d%cLitterCwd(days))          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(tot_outVars_d%cSoil(days))
        allocate(tot_outVars_d%cSoilLevels(days, nlayers))
        allocate(tot_outVars_d%cSoilFast(days))
        allocate(tot_outVars_d%cSoilSlow(days))
        allocate(tot_outVars_d%cSoilPassive(days))           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(tot_outVars_d%CH4(days, nlayers))          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(tot_outVars_d%fBNF(days))
        allocate(tot_outVars_d%fN2O(days))
        allocate(tot_outVars_d%fNloss(days))
        allocate(tot_outVars_d%fNnetmin(days))
        allocate(tot_outVars_d%fNdep(days))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(tot_outVars_d%nLeaf(days))
        allocate(tot_outVars_d%nStem(days))
        allocate(tot_outVars_d%nRoot(days))
        allocate(tot_outVars_d%nOther(days))
        allocate(tot_outVars_d%nLitter(days))
        allocate(tot_outVars_d%nLitterCwd(days))
        allocate(tot_outVars_d%nSoil(days))
        allocate(tot_outVars_d%nMineral(days))                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(tot_outVars_d%hfls(days))
        allocate(tot_outVars_d%hfss(days))
        allocate(tot_outVars_d%SWnet(days))
        allocate(tot_outVars_d%LWnet(days))                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(tot_outVars_d%ec(days))
        allocate(tot_outVars_d%tran(days))
        allocate(tot_outVars_d%es(days))                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(tot_outVars_d%hfsbl(days))                   ! Snow sublimation
        allocate(tot_outVars_d%mrro(days))
        allocate(tot_outVars_d%mrros(days))
        allocate(tot_outVars_d%mrrob(days))                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        allocate(tot_outVars_d%mrso(days, nlayers))           ! Kg m-2, soil moisture in each soil layer
        allocate(tot_outVars_d%tsl(days, nlayers))            ! K, soil temperature in each soil layer
        allocate(tot_outVars_d%tsland(days))                  ! K, surface temperature
        allocate(tot_outVars_d%wtd(days))                     ! m, Water table depth
        allocate(tot_outVars_d%snd(days))                     ! m, Total snow depth
        allocate(tot_outVars_d%lai(days))

        ! monthly results
        allocate(tot_outVars_m%gpp(months))
        allocate(tot_outVars_m%nee(months))
        allocate(tot_outVars_m%npp(months))
        allocate(tot_outVars_m%nppLeaf(months))
        allocate(tot_outVars_m%nppWood(months))
        allocate(tot_outVars_m%nppStem(months))
        allocate(tot_outVars_m%nppRoot(months))
        allocate(tot_outVars_m%nppOther(months))           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(tot_outVars_m%ra(months))
        allocate(tot_outVars_m%raLeaf(months))
        allocate(tot_outVars_m%raStem(months))
        allocate(tot_outVars_m%raRoot(months))
        allocate(tot_outVars_m%raOther(months))
        allocate(tot_outVars_m%rMaint(months))
        allocate(tot_outVars_m%rGrowth(months))            ! maintenance respiration and growth respiration
        allocate(tot_outVars_m%rh(months))
        allocate(tot_outVars_m%nbp(months))                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(tot_outVars_m%wetlandCH4(months))
        allocate(tot_outVars_m%wetlandCH4prod(months))
        allocate(tot_outVars_m%wetlandCH4cons(months))     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(tot_outVars_m%cLeaf(months))
        allocate(tot_outVars_m%cStem(months))
        allocate(tot_outVars_m%cRoot(months))
        allocate(tot_outVars_m%cOther(months))              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(tot_outVars_m%cLitter(months))
        allocate(tot_outVars_m%cLitterCwd(months))          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(tot_outVars_m%cSoil(months))
        allocate(tot_outVars_m%cSoilLevels(months, nlayers))
        allocate(tot_outVars_m%cSoilFast(months))
        allocate(tot_outVars_m%cSoilSlow(months))
        allocate(tot_outVars_m%cSoilPassive(months))           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(tot_outVars_m%CH4(months, nlayers))          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(tot_outVars_m%fBNF(months))
        allocate(tot_outVars_m%fN2O(months))
        allocate(tot_outVars_m%fNloss(months))
        allocate(tot_outVars_m%fNnetmin(months))
        allocate(tot_outVars_m%fNdep(months))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(tot_outVars_m%nLeaf(months))
        allocate(tot_outVars_m%nStem(months))
        allocate(tot_outVars_m%nRoot(months))
        allocate(tot_outVars_m%nOther(months))
        allocate(tot_outVars_m%nLitter(months))
        allocate(tot_outVars_m%nLitterCwd(months))
        allocate(tot_outVars_m%nSoil(months))
        allocate(tot_outVars_m%nMineral(months))                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(tot_outVars_m%hfls(months))
        allocate(tot_outVars_m%hfss(months))
        allocate(tot_outVars_m%SWnet(months))
        allocate(tot_outVars_m%LWnet(months))                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(tot_outVars_m%ec(months))
        allocate(tot_outVars_m%tran(months))
        allocate(tot_outVars_m%es(months))                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(tot_outVars_m%hfsbl(months))                   ! Snow sublimation
        allocate(tot_outVars_m%mrro(months))
        allocate(tot_outVars_m%mrros(months))
        allocate(tot_outVars_m%mrrob(months))                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        allocate(tot_outVars_m%mrso(months, nlayers))           ! Kg m-2, soil moisture in each soil layer
        allocate(tot_outVars_m%tsl(months, nlayers))            ! K, soil temperature in each soil layer
        allocate(tot_outVars_m%tsland(months))                  ! K, surface temperature
        allocate(tot_outVars_m%wtd(months))                     ! m, Water table depth
        allocate(tot_outVars_m%snd(months))                     ! m, Total snow depth
        allocate(tot_outVars_m%lai(months))

        ! yearly results
        allocate(tot_outVars_y%gpp(years))
        allocate(tot_outVars_y%nee(years))
        allocate(tot_outVars_y%npp(years))
        allocate(tot_outVars_y%nppLeaf(years))
        allocate(tot_outVars_y%nppWood(years))
        allocate(tot_outVars_y%nppStem(years))
        allocate(tot_outVars_y%nppRoot(years))
        allocate(tot_outVars_y%nppOther(years))           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(tot_outVars_y%ra(years))
        allocate(tot_outVars_y%raLeaf(years))
        allocate(tot_outVars_y%raStem(years))
        allocate(tot_outVars_y%raRoot(years))
        allocate(tot_outVars_y%raOther(years))
        allocate(tot_outVars_y%rMaint(years))
        allocate(tot_outVars_y%rGrowth(years))            ! maintenance respiration and growth respiration
        allocate(tot_outVars_y%rh(years))
        allocate(tot_outVars_y%nbp(years))                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(tot_outVars_y%wetlandCH4(years))
        allocate(tot_outVars_y%wetlandCH4prod(years))
        allocate(tot_outVars_y%wetlandCH4cons(years))     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(tot_outVars_y%cLeaf(years))
        allocate(tot_outVars_y%cStem(years))
        allocate(tot_outVars_y%cRoot(years))
        allocate(tot_outVars_y%cOther(years))              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(tot_outVars_y%cLitter(years))
        allocate(tot_outVars_y%cLitterCwd(years))          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(tot_outVars_y%cSoil(years))
        allocate(tot_outVars_y%cSoilLevels(years, nlayers))
        allocate(tot_outVars_y%cSoilFast(years))
        allocate(tot_outVars_y%cSoilSlow(years))
        allocate(tot_outVars_y%cSoilPassive(years))           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(tot_outVars_y%CH4(years, nlayers))          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(tot_outVars_y%fBNF(years))
        allocate(tot_outVars_y%fN2O(years))
        allocate(tot_outVars_y%fNloss(years))
        allocate(tot_outVars_y%fNnetmin(years))
        allocate(tot_outVars_y%fNdep(years))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(tot_outVars_y%nLeaf(years))
        allocate(tot_outVars_y%nStem(years))
        allocate(tot_outVars_y%nRoot(years))
        allocate(tot_outVars_y%nOther(years))
        allocate(tot_outVars_y%nLitter(years))
        allocate(tot_outVars_y%nLitterCwd(years))
        allocate(tot_outVars_y%nSoil(years))
        allocate(tot_outVars_y%nMineral(years))                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(tot_outVars_y%hfls(years))
        allocate(tot_outVars_y%hfss(years))
        allocate(tot_outVars_y%SWnet(years))
        allocate(tot_outVars_y%LWnet(years))                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(tot_outVars_y%ec(years))
        allocate(tot_outVars_y%tran(years))
        allocate(tot_outVars_y%es(years))                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(tot_outVars_y%hfsbl(years))                   ! Snow sublimation
        allocate(tot_outVars_y%mrro(years))
        allocate(tot_outVars_y%mrros(years))
        allocate(tot_outVars_y%mrrob(years))                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        allocate(tot_outVars_y%mrso(years, nlayers))           ! Kg m-2, soil moisture in each soil layer
        allocate(tot_outVars_y%tsl(years, nlayers))            ! K, soil temperature in each soil layer
        allocate(tot_outVars_y%tsland(years))                  ! K, surface temperature
        allocate(tot_outVars_y%wtd(years))                     ! m, Water table depth
        allocate(tot_outVars_y%snd(years))                     ! m, Total snow depth
        allocate(tot_outVars_y%lai(years))
    end subroutine assign_all_results

    subroutine deallocate_all_results()
        if (allocated(tot_outVars_h%gpp))            deallocate(tot_outVars_h%gpp)
        if (allocated(tot_outVars_h%nee))            deallocate(tot_outVars_h%nee)
        if (allocated(tot_outVars_h%npp))            deallocate(tot_outVars_h%npp)
        if (allocated(tot_outVars_h%nppLeaf))        deallocate(tot_outVars_h%nppLeaf)
        if (allocated(tot_outVars_h%nppWood))        deallocate(tot_outVars_h%nppWood)
        if (allocated(tot_outVars_h%nppStem))        deallocate(tot_outVars_h%nppStem)
        if (allocated(tot_outVars_h%nppRoot))        deallocate(tot_outVars_h%nppRoot)
        if (allocated(tot_outVars_h%nppOther))       deallocate(tot_outVars_h%nppOther)           
        if (allocated(tot_outVars_h%ra))             deallocate(tot_outVars_h%ra)
        if (allocated(tot_outVars_h%raLeaf))         deallocate(tot_outVars_h%raLeaf)
        if (allocated(tot_outVars_h%raStem))         deallocate(tot_outVars_h%raStem)
        if (allocated(tot_outVars_h%raRoot))         deallocate(tot_outVars_h%raRoot)
        if (allocated(tot_outVars_h%raOther))        deallocate(tot_outVars_h%raOther)
        if (allocated(tot_outVars_h%rMaint))         deallocate(tot_outVars_h%rMaint)
        if (allocated(tot_outVars_h%rGrowth))        deallocate(tot_outVars_h%rGrowth)           
        if (allocated(tot_outVars_h%rh))             deallocate(tot_outVars_h%rh)
        if (allocated(tot_outVars_h%nbp))            deallocate(tot_outVars_h%nbp)                
        if (allocated(tot_outVars_h%wetlandCH4))     deallocate(tot_outVars_h%wetlandCH4)
        if (allocated(tot_outVars_h%wetlandCH4prod)) deallocate(tot_outVars_h%wetlandCH4prod)
        if (allocated(tot_outVars_h%wetlandCH4cons)) deallocate(tot_outVars_h%wetlandCH4cons)  
        ! Carbon Pools  (KgC m-2)
        if (allocated(tot_outVars_h%cLeaf))        deallocate(tot_outVars_h%cLeaf)
        if (allocated(tot_outVars_h%cStem))        deallocate(tot_outVars_h%cStem)
        if (allocated(tot_outVars_h%cRoot))        deallocate(tot_outVars_h%cRoot)
        if (allocated(tot_outVars_h%cOther))       deallocate(tot_outVars_h%cOther)
        if (allocated(tot_outVars_h%cLitter))      deallocate(tot_outVars_h%cLitter)
        if (allocated(tot_outVars_h%cLitterCwd))   deallocate(tot_outVars_h%cLitterCwd)
        if (allocated(tot_outVars_h%cSoil))        deallocate(tot_outVars_h%cSoil)
        if (allocated(tot_outVars_h%cSoilLevels))  deallocate(tot_outVars_h%cSoilLevels)
        if (allocated(tot_outVars_h%cSoilFast))    deallocate(tot_outVars_h%cSoilFast)
        if (allocated(tot_outVars_h%cSoilSlow))    deallocate(tot_outVars_h%cSoilSlow)
        if (allocated(tot_outVars_h%cSoilPassive)) deallocate(tot_outVars_h%cSoilPassive)
        if (allocated(tot_outVars_h%CH4))         deallocate(tot_outVars_h%CH4)
        ! Nitrogen fluxes (kgN m-2 s-1)
        if (allocated(tot_outVars_h%fBNF))         deallocate(tot_outVars_h%fBNF)
        if (allocated(tot_outVars_h%fN2O))         deallocate(tot_outVars_h%fN2O)
        if (allocated(tot_outVars_h%fNloss))       deallocate(tot_outVars_h%fNloss)
        if (allocated(tot_outVars_h%fNnetmin))     deallocate(tot_outVars_h%fNnetmin)
        if (allocated(tot_outVars_h%fNdep))        deallocate(tot_outVars_h%fNdep)
        ! Nitrogen pools (kgN m-2)
        if (allocated(tot_outVars_h%nLeaf))        deallocate(tot_outVars_h%nLeaf)
        if (allocated(tot_outVars_h%nStem))        deallocate(tot_outVars_h%nStem)
        if (allocated(tot_outVars_h%nRoot))        deallocate(tot_outVars_h%nRoot)
        if (allocated(tot_outVars_h%nOther))       deallocate(tot_outVars_h%nOther)
        if (allocated(tot_outVars_h%nLitter))      deallocate(tot_outVars_h%nLitter)
        if (allocated(tot_outVars_h%nLitterCwd))   deallocate(tot_outVars_h%nLitterCwd)
        if (allocated(tot_outVars_h%nSoil))        deallocate(tot_outVars_h%nSoil)
        if (allocated(tot_outVars_h%nMineral))     deallocate(tot_outVars_h%nMineral)
        ! energy fluxes (W m-2)
        if (allocated(tot_outVars_h%hfls))         deallocate(tot_outVars_h%hfls)
        if (allocated(tot_outVars_h%hfss))         deallocate(tot_outVars_h%hfss)
        if (allocated(tot_outVars_h%SWnet))        deallocate(tot_outVars_h%SWnet)
        if (allocated(tot_outVars_h%LWnet))        deallocate(tot_outVars_h%LWnet)
        ! water fluxes (kg m-2 s-1)
        if (allocated(tot_outVars_h%ec))           deallocate(tot_outVars_h%ec)
        if (allocated(tot_outVars_h%tran))         deallocate(tot_outVars_h%tran)
        if (allocated(tot_outVars_h%es))           deallocate(tot_outVars_h%es)
        if (allocated(tot_outVars_h%hfsbl))        deallocate(tot_outVars_h%hfsbl)
        if (allocated(tot_outVars_h%mrro))         deallocate(tot_outVars_h%mrro)
        if (allocated(tot_outVars_h%mrros))        deallocate(tot_outVars_h%mrros)
        if (allocated(tot_outVars_h%mrrob))        deallocate(tot_outVars_h%mrrob)
        ! other
        if (allocated(tot_outVars_h%mrso))         deallocate(tot_outVars_h%mrso)      
        if (allocated(tot_outVars_h%tsl))          deallocate(tot_outVars_h%tsl)       
        if (allocated(tot_outVars_h%tsland))       deallocate(tot_outVars_h%tsland) 
        if (allocated(tot_outVars_h%wtd))          deallocate(tot_outVars_h%wtd)
        if (allocated(tot_outVars_h%snd))          deallocate(tot_outVars_h%snd)
        if (allocated(tot_outVars_h%lai))          deallocate(tot_outVars_h%lai)

        ! daily results 
        if (allocated(tot_outVars_d%gpp))            deallocate(tot_outVars_d%gpp)
        if (allocated(tot_outVars_d%nee))            deallocate(tot_outVars_d%nee)
        if (allocated(tot_outVars_d%npp))            deallocate(tot_outVars_d%npp)
        if (allocated(tot_outVars_d%nppLeaf))        deallocate(tot_outVars_d%nppLeaf)
        if (allocated(tot_outVars_d%nppWood))        deallocate(tot_outVars_d%nppWood)
        if (allocated(tot_outVars_d%nppStem))        deallocate(tot_outVars_d%nppStem)
        if (allocated(tot_outVars_d%nppRoot))        deallocate(tot_outVars_d%nppRoot)
        if (allocated(tot_outVars_d%nppOther))       deallocate(tot_outVars_d%nppOther)           
        if (allocated(tot_outVars_d%ra))             deallocate(tot_outVars_d%ra)
        if (allocated(tot_outVars_d%raLeaf))         deallocate(tot_outVars_d%raLeaf)
        if (allocated(tot_outVars_d%raStem))         deallocate(tot_outVars_d%raStem)
        if (allocated(tot_outVars_d%raRoot))         deallocate(tot_outVars_d%raRoot)
        if (allocated(tot_outVars_d%raOther))        deallocate(tot_outVars_d%raOther)
        if (allocated(tot_outVars_d%rMaint))         deallocate(tot_outVars_d%rMaint)
        if (allocated(tot_outVars_d%rGrowth))        deallocate(tot_outVars_d%rGrowth)           
        if (allocated(tot_outVars_d%rh))             deallocate(tot_outVars_d%rh)
        if (allocated(tot_outVars_d%nbp))            deallocate(tot_outVars_d%nbp)                
        if (allocated(tot_outVars_d%wetlandCH4))     deallocate(tot_outVars_d%wetlandCH4)
        if (allocated(tot_outVars_d%wetlandCH4prod)) deallocate(tot_outVars_d%wetlandCH4prod)
        if (allocated(tot_outVars_d%wetlandCH4cons)) deallocate(tot_outVars_d%wetlandCH4cons)  
        ! Carbon Pools  (KgC m-2)
        if (allocated(tot_outVars_d%cLeaf))        deallocate(tot_outVars_d%cLeaf)
        if (allocated(tot_outVars_d%cStem))        deallocate(tot_outVars_d%cStem)
        if (allocated(tot_outVars_d%cRoot))        deallocate(tot_outVars_d%cRoot)
        if (allocated(tot_outVars_d%cOther))       deallocate(tot_outVars_d%cOther)
        if (allocated(tot_outVars_d%cLitter))      deallocate(tot_outVars_d%cLitter)
        if (allocated(tot_outVars_d%cLitterCwd))   deallocate(tot_outVars_d%cLitterCwd)
        if (allocated(tot_outVars_d%cSoil))        deallocate(tot_outVars_d%cSoil)
        if (allocated(tot_outVars_d%cSoilLevels))  deallocate(tot_outVars_d%cSoilLevels)
        if (allocated(tot_outVars_d%cSoilFast))    deallocate(tot_outVars_d%cSoilFast)
        if (allocated(tot_outVars_d%cSoilSlow))    deallocate(tot_outVars_d%cSoilSlow)
        if (allocated(tot_outVars_d%cSoilPassive)) deallocate(tot_outVars_d%cSoilPassive)
        if (allocated(tot_outVars_d%CH4))         deallocate(tot_outVars_d%CH4)
        ! Nitrogen fluxes (kgN m-2 s-1)
        if (allocated(tot_outVars_d%fBNF))         deallocate(tot_outVars_d%fBNF)
        if (allocated(tot_outVars_d%fN2O))         deallocate(tot_outVars_d%fN2O)
        if (allocated(tot_outVars_d%fNloss))       deallocate(tot_outVars_d%fNloss)
        if (allocated(tot_outVars_d%fNnetmin))     deallocate(tot_outVars_d%fNnetmin)
        if (allocated(tot_outVars_d%fNdep))        deallocate(tot_outVars_d%fNdep)
        ! Nitrogen pools (kgN m-2)
        if (allocated(tot_outVars_d%nLeaf))        deallocate(tot_outVars_d%nLeaf)
        if (allocated(tot_outVars_d%nStem))        deallocate(tot_outVars_d%nStem)
        if (allocated(tot_outVars_d%nRoot))        deallocate(tot_outVars_d%nRoot)
        if (allocated(tot_outVars_d%nOther))       deallocate(tot_outVars_d%nOther)
        if (allocated(tot_outVars_d%nLitter))      deallocate(tot_outVars_d%nLitter)
        if (allocated(tot_outVars_d%nLitterCwd))   deallocate(tot_outVars_d%nLitterCwd)
        if (allocated(tot_outVars_d%nSoil))        deallocate(tot_outVars_d%nSoil)
        if (allocated(tot_outVars_d%nMineral))     deallocate(tot_outVars_d%nMineral)
        ! energy fluxes (W m-2)
        if (allocated(tot_outVars_d%hfls))         deallocate(tot_outVars_d%hfls)
        if (allocated(tot_outVars_d%hfss))         deallocate(tot_outVars_d%hfss)
        if (allocated(tot_outVars_d%SWnet))        deallocate(tot_outVars_d%SWnet)
        if (allocated(tot_outVars_d%LWnet))        deallocate(tot_outVars_d%LWnet)
        ! water fluxes (kg m-2 s-1)
        if (allocated(tot_outVars_d%ec))           deallocate(tot_outVars_d%ec)
        if (allocated(tot_outVars_d%tran))         deallocate(tot_outVars_d%tran)
        if (allocated(tot_outVars_d%es))           deallocate(tot_outVars_d%es)
        if (allocated(tot_outVars_d%hfsbl))        deallocate(tot_outVars_d%hfsbl)
        if (allocated(tot_outVars_d%mrro))         deallocate(tot_outVars_d%mrro)
        if (allocated(tot_outVars_d%mrros))        deallocate(tot_outVars_d%mrros)
        if (allocated(tot_outVars_d%mrrob))        deallocate(tot_outVars_d%mrrob)
        ! other
        if (allocated(tot_outVars_d%mrso))         deallocate(tot_outVars_d%mrso)      
        if (allocated(tot_outVars_d%tsl))          deallocate(tot_outVars_d%tsl)       
        if (allocated(tot_outVars_d%tsland))       deallocate(tot_outVars_d%tsland) 
        if (allocated(tot_outVars_d%wtd))          deallocate(tot_outVars_d%wtd)
        if (allocated(tot_outVars_d%snd))          deallocate(tot_outVars_d%snd)
        if (allocated(tot_outVars_d%lai))          deallocate(tot_outVars_d%lai)

        ! monthly results
        if (allocated(tot_outVars_m%gpp))            deallocate(tot_outVars_m%gpp)
        if (allocated(tot_outVars_m%nee))            deallocate(tot_outVars_m%nee)
        if (allocated(tot_outVars_m%npp))            deallocate(tot_outVars_m%npp)
        if (allocated(tot_outVars_m%nppLeaf))        deallocate(tot_outVars_m%nppLeaf)
        if (allocated(tot_outVars_m%nppWood))        deallocate(tot_outVars_m%nppWood)
        if (allocated(tot_outVars_m%nppStem))        deallocate(tot_outVars_m%nppStem)
        if (allocated(tot_outVars_m%nppRoot))        deallocate(tot_outVars_m%nppRoot)
        if (allocated(tot_outVars_m%nppOther))       deallocate(tot_outVars_m%nppOther)           
        if (allocated(tot_outVars_m%ra))             deallocate(tot_outVars_m%ra)
        if (allocated(tot_outVars_m%raLeaf))         deallocate(tot_outVars_m%raLeaf)
        if (allocated(tot_outVars_m%raStem))         deallocate(tot_outVars_m%raStem)
        if (allocated(tot_outVars_m%raRoot))         deallocate(tot_outVars_m%raRoot)
        if (allocated(tot_outVars_m%raOther))        deallocate(tot_outVars_m%raOther)
        if (allocated(tot_outVars_m%rMaint))         deallocate(tot_outVars_m%rMaint)
        if (allocated(tot_outVars_m%rGrowth))        deallocate(tot_outVars_m%rGrowth)           
        if (allocated(tot_outVars_m%rh))             deallocate(tot_outVars_m%rh)
        if (allocated(tot_outVars_m%nbp))            deallocate(tot_outVars_m%nbp)                
        if (allocated(tot_outVars_m%wetlandCH4))     deallocate(tot_outVars_m%wetlandCH4)
        if (allocated(tot_outVars_m%wetlandCH4prod)) deallocate(tot_outVars_m%wetlandCH4prod)
        if (allocated(tot_outVars_m%wetlandCH4cons)) deallocate(tot_outVars_m%wetlandCH4cons)  
        ! Carbon Pools  (KgC m-2)
        if (allocated(tot_outVars_m%cLeaf))        deallocate(tot_outVars_m%cLeaf)
        if (allocated(tot_outVars_m%cStem))        deallocate(tot_outVars_m%cStem)
        if (allocated(tot_outVars_m%cRoot))        deallocate(tot_outVars_m%cRoot)
        if (allocated(tot_outVars_m%cOther))       deallocate(tot_outVars_m%cOther)
        if (allocated(tot_outVars_m%cLitter))      deallocate(tot_outVars_m%cLitter)
        if (allocated(tot_outVars_m%cLitterCwd))   deallocate(tot_outVars_m%cLitterCwd)
        if (allocated(tot_outVars_m%cSoil))        deallocate(tot_outVars_m%cSoil)
        if (allocated(tot_outVars_m%cSoilLevels))  deallocate(tot_outVars_m%cSoilLevels)
        if (allocated(tot_outVars_m%cSoilFast))    deallocate(tot_outVars_m%cSoilFast)
        if (allocated(tot_outVars_m%cSoilSlow))    deallocate(tot_outVars_m%cSoilSlow)
        if (allocated(tot_outVars_m%cSoilPassive)) deallocate(tot_outVars_m%cSoilPassive)
        if (allocated(tot_outVars_m%CH4))         deallocate(tot_outVars_m%CH4)
        ! Nitrogen fluxes (kgN m-2 s-1)
        if (allocated(tot_outVars_m%fBNF))         deallocate(tot_outVars_m%fBNF)
        if (allocated(tot_outVars_m%fN2O))         deallocate(tot_outVars_m%fN2O)
        if (allocated(tot_outVars_m%fNloss))       deallocate(tot_outVars_m%fNloss)
        if (allocated(tot_outVars_m%fNnetmin))     deallocate(tot_outVars_m%fNnetmin)
        if (allocated(tot_outVars_m%fNdep))        deallocate(tot_outVars_m%fNdep)
        ! Nitrogen pools (kgN m-2)
        if (allocated(tot_outVars_m%nLeaf))        deallocate(tot_outVars_m%nLeaf)
        if (allocated(tot_outVars_m%nStem))        deallocate(tot_outVars_m%nStem)
        if (allocated(tot_outVars_m%nRoot))        deallocate(tot_outVars_m%nRoot)
        if (allocated(tot_outVars_m%nOther))       deallocate(tot_outVars_m%nOther)
        if (allocated(tot_outVars_m%nLitter))      deallocate(tot_outVars_m%nLitter)
        if (allocated(tot_outVars_m%nLitterCwd))   deallocate(tot_outVars_m%nLitterCwd)
        if (allocated(tot_outVars_m%nSoil))        deallocate(tot_outVars_m%nSoil)
        if (allocated(tot_outVars_m%nMineral))     deallocate(tot_outVars_m%nMineral)
        ! energy fluxes (W m-2)
        if (allocated(tot_outVars_m%hfls))         deallocate(tot_outVars_m%hfls)
        if (allocated(tot_outVars_m%hfss))         deallocate(tot_outVars_m%hfss)
        if (allocated(tot_outVars_m%SWnet))        deallocate(tot_outVars_m%SWnet)
        if (allocated(tot_outVars_m%LWnet))        deallocate(tot_outVars_m%LWnet)
        ! water fluxes (kg m-2 s-1)
        if (allocated(tot_outVars_m%ec))           deallocate(tot_outVars_m%ec)
        if (allocated(tot_outVars_m%tran))         deallocate(tot_outVars_m%tran)
        if (allocated(tot_outVars_m%es))           deallocate(tot_outVars_m%es)
        if (allocated(tot_outVars_m%hfsbl))        deallocate(tot_outVars_m%hfsbl)
        if (allocated(tot_outVars_m%mrro))         deallocate(tot_outVars_m%mrro)
        if (allocated(tot_outVars_m%mrros))        deallocate(tot_outVars_m%mrros)
        if (allocated(tot_outVars_m%mrrob))        deallocate(tot_outVars_m%mrrob)
        ! other
        if (allocated(tot_outVars_m%mrso))         deallocate(tot_outVars_m%mrso)      
        if (allocated(tot_outVars_m%tsl))          deallocate(tot_outVars_m%tsl)       
        if (allocated(tot_outVars_m%tsland))       deallocate(tot_outVars_m%tsland) 
        if (allocated(tot_outVars_m%wtd))          deallocate(tot_outVars_m%wtd)
        if (allocated(tot_outVars_m%snd))          deallocate(tot_outVars_m%snd)
        if (allocated(tot_outVars_m%lai))          deallocate(tot_outVars_m%lai)

        ! yearly results
        if (allocated(tot_outVars_y%gpp))            deallocate(tot_outVars_y%gpp)
        if (allocated(tot_outVars_y%nee))            deallocate(tot_outVars_y%nee)
        if (allocated(tot_outVars_y%npp))            deallocate(tot_outVars_y%npp)
        if (allocated(tot_outVars_y%nppLeaf))        deallocate(tot_outVars_y%nppLeaf)
        if (allocated(tot_outVars_y%nppWood))        deallocate(tot_outVars_y%nppWood)
        if (allocated(tot_outVars_y%nppStem))        deallocate(tot_outVars_y%nppStem)
        if (allocated(tot_outVars_y%nppRoot))        deallocate(tot_outVars_y%nppRoot)
        if (allocated(tot_outVars_y%nppOther))       deallocate(tot_outVars_y%nppOther)           
        if (allocated(tot_outVars_y%ra))             deallocate(tot_outVars_y%ra)
        if (allocated(tot_outVars_y%raLeaf))         deallocate(tot_outVars_y%raLeaf)
        if (allocated(tot_outVars_y%raStem))         deallocate(tot_outVars_y%raStem)
        if (allocated(tot_outVars_y%raRoot))         deallocate(tot_outVars_y%raRoot)
        if (allocated(tot_outVars_y%raOther))        deallocate(tot_outVars_y%raOther)
        if (allocated(tot_outVars_y%rMaint))         deallocate(tot_outVars_y%rMaint)
        if (allocated(tot_outVars_y%rGrowth))        deallocate(tot_outVars_y%rGrowth)           
        if (allocated(tot_outVars_y%rh))             deallocate(tot_outVars_y%rh)
        if (allocated(tot_outVars_y%nbp))            deallocate(tot_outVars_y%nbp)                
        if (allocated(tot_outVars_y%wetlandCH4))     deallocate(tot_outVars_y%wetlandCH4)
        if (allocated(tot_outVars_y%wetlandCH4prod)) deallocate(tot_outVars_y%wetlandCH4prod)
        if (allocated(tot_outVars_y%wetlandCH4cons)) deallocate(tot_outVars_y%wetlandCH4cons)  
        ! Carbon Pools  (KgC m-2)
        if (allocated(tot_outVars_y%cLeaf))        deallocate(tot_outVars_y%cLeaf)
        if (allocated(tot_outVars_y%cStem))        deallocate(tot_outVars_y%cStem)
        if (allocated(tot_outVars_y%cRoot))        deallocate(tot_outVars_y%cRoot)
        if (allocated(tot_outVars_y%cOther))       deallocate(tot_outVars_y%cOther)
        if (allocated(tot_outVars_y%cLitter))      deallocate(tot_outVars_y%cLitter)
        if (allocated(tot_outVars_y%cLitterCwd))   deallocate(tot_outVars_y%cLitterCwd)
        if (allocated(tot_outVars_y%cSoil))        deallocate(tot_outVars_y%cSoil)
        if (allocated(tot_outVars_y%cSoilLevels))  deallocate(tot_outVars_y%cSoilLevels)
        if (allocated(tot_outVars_y%cSoilFast))    deallocate(tot_outVars_y%cSoilFast)
        if (allocated(tot_outVars_y%cSoilSlow))    deallocate(tot_outVars_y%cSoilSlow)
        if (allocated(tot_outVars_y%cSoilPassive)) deallocate(tot_outVars_y%cSoilPassive)
        if (allocated(tot_outVars_y%CH4))         deallocate(tot_outVars_y%CH4)
        ! Nitrogen fluxes (kgN m-2 s-1)
        if (allocated(tot_outVars_y%fBNF))         deallocate(tot_outVars_y%fBNF)
        if (allocated(tot_outVars_y%fN2O))         deallocate(tot_outVars_y%fN2O)
        if (allocated(tot_outVars_y%fNloss))       deallocate(tot_outVars_y%fNloss)
        if (allocated(tot_outVars_y%fNnetmin))     deallocate(tot_outVars_y%fNnetmin)
        if (allocated(tot_outVars_y%fNdep))        deallocate(tot_outVars_y%fNdep)
        ! Nitrogen pools (kgN m-2)
        if (allocated(tot_outVars_y%nLeaf))        deallocate(tot_outVars_y%nLeaf)
        if (allocated(tot_outVars_y%nStem))        deallocate(tot_outVars_y%nStem)
        if (allocated(tot_outVars_y%nRoot))        deallocate(tot_outVars_y%nRoot)
        if (allocated(tot_outVars_y%nOther))       deallocate(tot_outVars_y%nOther)
        if (allocated(tot_outVars_y%nLitter))      deallocate(tot_outVars_y%nLitter)
        if (allocated(tot_outVars_y%nLitterCwd))   deallocate(tot_outVars_y%nLitterCwd)
        if (allocated(tot_outVars_y%nSoil))        deallocate(tot_outVars_y%nSoil)
        if (allocated(tot_outVars_y%nMineral))     deallocate(tot_outVars_y%nMineral)
        ! energy fluxes (W m-2)
        if (allocated(tot_outVars_y%hfls))         deallocate(tot_outVars_y%hfls)
        if (allocated(tot_outVars_y%hfss))         deallocate(tot_outVars_y%hfss)
        if (allocated(tot_outVars_y%SWnet))        deallocate(tot_outVars_y%SWnet)
        if (allocated(tot_outVars_y%LWnet))        deallocate(tot_outVars_y%LWnet)
        ! water fluxes (kg m-2 s-1)
        if (allocated(tot_outVars_y%ec))           deallocate(tot_outVars_y%ec)
        if (allocated(tot_outVars_y%tran))         deallocate(tot_outVars_y%tran)
        if (allocated(tot_outVars_y%es))           deallocate(tot_outVars_y%es)
        if (allocated(tot_outVars_y%hfsbl))        deallocate(tot_outVars_y%hfsbl)
        if (allocated(tot_outVars_y%mrro))         deallocate(tot_outVars_y%mrro)
        if (allocated(tot_outVars_y%mrros))        deallocate(tot_outVars_y%mrros)
        if (allocated(tot_outVars_y%mrrob))        deallocate(tot_outVars_y%mrrob)
        ! other
        if (allocated(tot_outVars_y%mrso))         deallocate(tot_outVars_y%mrso)      
        if (allocated(tot_outVars_y%tsl))          deallocate(tot_outVars_y%tsl)       
        if (allocated(tot_outVars_y%tsland))       deallocate(tot_outVars_y%tsland) 
        if (allocated(tot_outVars_y%wtd))          deallocate(tot_outVars_y%wtd)
        if (allocated(tot_outVars_y%snd))          deallocate(tot_outVars_y%snd)
        if (allocated(tot_outVars_y%lai))          deallocate(tot_outVars_y%lai)
    end subroutine deallocate_all_results

    subroutine deallocate_date_type()
        if (allocated(forcing)) deallocate(forcing)
        if (allocated(snow_in)) deallocate(snow_in)
    end subroutine deallocate_date_type
end module mod_data