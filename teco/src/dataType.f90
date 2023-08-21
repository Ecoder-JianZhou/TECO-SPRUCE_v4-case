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

    integer :: dtimes                 ! 24: hourly simulation
    character(200) :: filepath_in     ! input file path
    character(100) :: climfile        ! climate file name
    character(100) :: snowdepthfile   ! snow depthfile
    character(100) :: restartfile     ! restartfile
    character(100) :: watertablefile  ! Jian: maybe used when not run soil_physical
   
    character(100) :: outdir          ! output path
    ! fixed output path 
    character(50), parameter :: outDir_nc       = "results_nc_format"
    character(50), parameter :: outDir_csv      = "results_csv_format"
    character(50), parameter :: outDir_h        = "Hourly"
    character(50), parameter :: outDir_d        = "Daily"
    character(50), parameter :: outDir_m        = "Monthly"
    character(50), parameter :: outfile_restart = "restart"
    character(50), parameter :: outDir_spinup   = "results_spinup"
    character(50), parameter :: outfile_spinup  = "results_spinup.nc"
    
    ! experiment settings
    real :: Ttreat     = 0.        ! Temperature treatment, warming in air and soil temperature
    real :: CO2treat   = 0.        ! CO2 treatmant, up to CO2treat, not add to Ca. CO2
    real :: N_fert     = 0.        ! 5.6 ! (11.2 gN m-2 yr-1, in spring, Duke Forest FACE)

    ! parameters for spin-up
    integer :: nloops                 ! the times of cycling the forcing to reach ss

    ! special parameters from reading file
    type spec_data_type
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
    end type spec_data_type

    type(spec_data_type) :: spData
    
    ! parameters for forcing data -------------------------------------
    integer iforcing, nforcing, nHours, nDays, nMonths, nYears                                     ! for cycle
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

    ! constant parameters
    ! Sps is not assigned previous, something is wrong. -JJ
    real :: Sps = 1.                                ! scaling factors for growth, Jian: using in vegetation module. set it to 1
    integer, parameter :: nlayers = 10                ! how many
    real,    parameter :: pi      = 3.1415926
    ! physical constants
    real,    parameter :: tauL(3) = (/0.1, 0.425, 0.00/)          ! leaf transmittance for vis, for NIR, for thermal
    real,    parameter :: rhoL(3) = (/0.1, 0.425, 0.00/)          ! leaf reflectance for vis, for NIR, for thermal
    real,    parameter :: rhoS(3) = (/0.1, 0.3,   0.00/)          ! soil reflectance for vis, for NIR, for thermal
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
    ! end of consts parameters -------------------------------------------------------------------

    ! summary of outputs
    type output_vars_type
        ! carbon fluxes (Kg C m-2 s-1)
        real, pointer :: gpp(:)
        real, pointer :: nee(:)
        real, pointer :: npp(:)
        real, pointer :: nppLeaf(:)
        real, pointer :: nppWood(:)
        real, pointer :: nppStem(:)
        real, pointer :: nppRoot(:)
        real, pointer :: nppOther(:)           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        real, pointer :: ra(:)
        real, pointer :: raLeaf(:)
        real, pointer :: raStem(:)
        real, pointer :: raRoot(:)
        real, pointer :: raOther(:)
        real, pointer :: rMaint(:)
        real, pointer :: rGrowth(:)            ! maintenance respiration and growth respiration
        real, pointer :: rh(:)
        real, pointer :: nbp(:)                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        real, pointer :: wetlandCH4(:)
        real, pointer :: wetlandCH4prod(:)
        real, pointer :: wetlandCH4cons(:)     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        real, pointer :: cLeaf(:)
        real, pointer :: cStem(:)
        real, pointer :: cRoot(:)
        real, pointer :: cOther(:)              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        real, pointer :: cLitter(:)
        real, pointer :: cLitterCwd(:)          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        real, pointer :: cSoil(:)
        real, pointer :: cSoilLevels(:, :)
        real, pointer :: cSoilFast(:)
        real, pointer :: cSoilSlow(:)
        real, pointer :: cSoilPassive(:)           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        real, pointer :: cCH4(:, :)          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        real, pointer :: fBNF(:)
        real, pointer :: fN2O(:)
        real, pointer :: fNloss(:)
        real, pointer :: fNnetmin(:)
        real, pointer :: fNdep(:)                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        real, pointer :: nLeaf(:)
        real, pointer :: nStem(:)
        real, pointer :: nRoot(:)
        real, pointer :: nOther(:)
        real, pointer :: nLitter(:)
        real, pointer :: nLitterCwd(:)
        real, pointer :: nSoil(:)
        real, pointer :: nMineral(:)                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        real, pointer :: hfls(:)
        real, pointer :: hfss(:)
        real, pointer :: SWnet(:)
        real, pointer :: LWnet(:)                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        real, pointer :: ec(:)
        real, pointer :: tran(:)
        real, pointer :: es(:)                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        real, pointer :: hfsbl(:)                   ! Snow sublimation
        real, pointer :: mrro(:)
        real, pointer :: mrros(:)
        real, pointer :: mrrob(:)                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        real, pointer :: mrso(:, :)           ! Kg m-2, soil moisture in each soil layer
        real, pointer :: tsl(:, :)            ! K, soil temperature in each soil layer
        real, pointer :: tsland(:)                  ! K, surface temperature
        real, pointer :: wtd(:)                     ! m, Water table depth
        real, pointer :: snd(:)                     ! m, Total snow depth
        real, pointer :: lai(:)                     ! m2 m-2, Leaf area index            
    end type output_vars_type
    
    type(output_vars_type) :: outVars_h   ! hourly outputs
    type(output_vars_type) :: outVars_d   ! daily outputs
    type(output_vars_type) :: outVars_m   ! monthly outputs
    type(output_vars_type) :: outVars_y   ! yearly outputs
    type(output_vars_type) :: outVars_spinup ! for spinup yearly results

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
    real    :: Tsoill(10)
    ! vegetation
    ! photosynsis
    real :: eJmx0   != Vcmax0*2.7                   ! @20C Leuning 1996 from Wullschleger (1993)
    real :: Vcmx0    
    ! phonology related parameters
    integer :: pheno
    real    :: GDD5
    integer :: onset                                       !flag of phenological stage
    integer :: phenoset
    ! NSC relative
    real    :: NSCmin, fnsc, nsc, NSCmax 
    real    :: store, add
    real    :: stor_use, storage, accumulation
    ! Growth
    real    :: L_fall                             ! leaf fall
    real    :: alpha_L, alpha_W, alpha_R          ! allocation fraction to Leaf, stem, and Root             
    real    :: flait                              ! Jian: LAI relavant variable from vegetable to energy
    real    :: StemSap, RootSap
    real    :: LAI
    real    :: bmroot, bmstem, bmleaf, bmplant
    ! vegetation flux
    real    :: GPP
    real    :: NPP, NPP_L, NPP_W, NPP_R
    real    :: Rauto
    real    :: Rmain,   RmLeaf, RmStem, RmRoot    ! maintanence respiration
    real    :: Rgrowth, RgLeaf, RgStem, RgRoot    ! growth respiration
    ! water cycle
    real :: evap, transp, ET, G

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
    real,DIMENSION(:), ALLOCATABLE :: snow_in       ! if not run snow process
    
    
    type(forcing_data_type) :: forcing 

    ! ==============================================================================================
    contains

    subroutine read_TECO_model_configs()
        ! read the "TECO_model_configs.nml"
        implicit none
        real :: lat, lon, wsmin, wsmax, LAIMAX, LAIMIN, SLA
        real :: rdepth, Rootmax, Stemmax, SapR, SapS, GLmax, GRmax, Gsmax
        real :: stom_n, a1, Ds0, Vcmax0, extkU, xfang, alpha
        real :: Tau_Leaf, Tau_Wood, Tau_Root, Tau_F, Tau_C
        real :: Tau_Micro, Tau_slowSOM, Tau_Passive   
        real :: gddonset, Q10, Q10rh, Rl0, Rs0, Rr0
        ! added for parameters in methane module   
        real :: r_me, Q10pro, kCH4, Omax, CH4_thre
        real :: Tveg, Tpro_me, Toxi
        ! add based on Ma et al., 2022
        real :: f, bubprob, Vmaxfraction  
        ! add based on Ma et al., 2023
        real :: JV, Entrpy, etaL, etaW, etaR  ! etaL and etaR are not used.
        real :: f_F2M, f_C2M, f_C2S, f_M2S, f_M2P
        real :: f_S2P, f_S2M, f_P2M

        ! initial states
        real :: init_QC, init_CN0, init_NSCmin, init_Storage
        real :: init_nsc, init_accumulation, init_SNvcmax, init_N_deposit, init_alphaN
        real :: init_NSN, init_QNminer, init_N_deficit, init_thksl, init_FRLEN
        real :: init_liq_water, init_fwsoil, init_topfws, init_omega, init_zwt
        real :: init_infilt, init_sftmp, init_Tsnow, init_Twater, init_Tice, G
        real :: init_snow_dsim, init_dcount, init_dcount_soil, init_ice_tw, init_Tsoill
        real :: init_ice, init_shcap_snow, init_condu_snow, init_condu_b, init_depth_ex
        real :: init_diff_s, init_diff_snow, init_albedo_snow, init_resht
        real :: init_thd_snow_depth, init_b_bound, init_infilt_rate, init_fa, init_fsub
        real :: init_rho_snow, init_decay_m, init_CH4_V, init_CH4, init_Vp
        real :: init_bubble_methane_tot, init_Nbub, init_depth_1

        namelist /nml_simu_settings/ simu_name, do_spinup, do_mcmc,          & 
            do_snow, do_soilphy, do_matrix, do_EBG, do_restart, do_ndep,     &
            do_simu, do_leap, dtimes, filepath_in,  climfile, snowdepthfile, & 
            watertablefile, restartfile, outdir
        namelist /nml_exps/ Ttreat, CO2treat, N_fert
        namelist /nml_params/ lat, lon, wsmax, wsmin, LAIMAX, LAIMIN, rdepth,  & 
            Rootmax, Stemmax, SapR, SapS, SLA, GLmax, GRmax, Gsmax, stom_n,    &
            a1, Ds0, Vcmx0, extkU, xfang, alpha, Tau_Leaf, Tau_Wood, Tau_Root, &
            Tau_F, Tau_C, Tau_Micro, Tau_SlowSOM, Tau_Passive, gddonset, Q10,  &
            Rl0, Rs0, Rr0, r_me, Q10pro, kCH4, Omax, CH4_thre, Tveg, Tpro_me,  & 
            Toxi, f, bubprob, Vmaxfraction
        namelist /nml_initial_states/ init_QC, init_CN0, init_NSCmin, init_Storage, &
            init_nsc, init_accumulation, init_SNvcmax, init_N_deposit, init_alphaN, & 
            init_NSN, init_QNminer, init_N_deficit, init_thksl, init_FRLEN,         &
            init_liq_water, init_fwsoil, init_topfws, init_omega, init_zwt,         &
            init_infilt, init_sftmp, init_Tsnow, init_Twater, init_Tice, G,         &
            init_snow_dsim, init_dcount, init_dcount_soil, init_ice_tw, init_Tsoill, &
            init_ice, init_shcap_snow, init_condu_snow, init_condu_b, init_depth_ex, & 
            init_diff_s, init_diff_snow, init_albedo_snow, init_resht,               &
            init_thd_snow_depth, init_b_bound, init_infilt_rate, init_fa, init_fsub, &
            init_rho_snow, init_decay_m, init_CH4_V, init_CH4, init_Vp,              &
            init_bubble_methane_tot, init_Nbub, init_depth_1
        namelist /nml_spinup/ nloops 

        open(388, file="TECO_model_configs.nml")
        read(388, nml=nml_simu_settings)
        read(388, nml=nml_exps)
        read(388, nml=nml_params)
        read(388, nml=nml_initial_states)
        read(388, nml=nml_spinup)
        close(388)

        spData%lat         = lat
        spData%lon         = lon
        spData%wsmax       = wsmax
        spData%wsmin       = wsmin
        spData%LAIMAX      = LAIMAX
        spData%LAIMIN      = LAIMIN
        spData%SLAx        = SLAx
        spData%rdepth      = rdepth
        spData%Rootmax     = Rootmax
        spData%Stemmax     = Stemmax
        spData%SapR        = SapR
        spData%SapS        = SapS
        spData%GLmax       = GLmax
        spData%GRmax       = GRmax
        spData%Gsmax       = Gsmax
        spData%stom_n      = stom_n
        spData%a1          = a1
        spData%Ds0         = Ds0
        spData%Vcmax0      = Vcmax0        ! Jian: Vcmax0 and Vcmx0 is same? Vcmax0 is Vcmx0 in consts
        spData%extkU       = extkU
        spData%xfang       = xfang
        spData%alpha       = alpha  
        spData%Tau_Leaf    = Tau_Leaf
        spData%Tau_Wood    = Tau_Wood
        spData%Tau_Root    = Tau_Root      ! turnover rate of plant carbon pools : leaf, wood, root  
        spData%Tau_F       = tau_F
        spData%Tau_C       = tauC          ! turnover rate of litter carbon pools: fine, coarse 
        spData%Tau_Micro   = Tau_Micro
        spData%Tau_slowSOM = Tau_SlowSOM
        spData%Tau_Passive = Tau_Passive   ! turnover rate of soil carbon pools  : fast, slow, passive 
        spData%gddonset    = gddonset
        spData%Q10         = Q10
        spData%Q10rh       = Q10rh         ! Q10rh modified from Ma et al.,2023 for aclimate study, change in transfer module of Q10h
        spData%Rl0         = Rl0
        spData%Rs0         = Rs0
        spData%Rr0         = Rr0
        ! added for parameters in methane module   
        spData%r_me        = r_me
        spData%Q10pro      = Q10pro
        spData%kCH4        = kCH4
        spData%Omax        = Omax
        spData%CH4_thre    = CH4_thre
        spData%Tveg        = Tveg
        spData%Tpro_me     = Tpro_me
        spData%Toxi        = Toxi
        ! add based on Ma et al., 2022
        spData%f            = f
        spData%bubprob      = bubprob
        spData%Vmaxfraction = Vmaxfraction 
        ! add based on Ma et al., 2023
        spData%JV           = JV
        spData%Entrpy       = Entrpy
        spData%etaL         = etaL
        spData%etaW         = etaW
        spData%etaR         = etaR   ! etaL and etaR are not used.
        spData%f_F2M        = f_F2M
        spData%f_C2M        = f_C2M
        spData%f_C2S        = f_C2S
        spData%f_M2S        = f_M2S
        spData%f_M2P        = f_M2P
        spData%f_S2P        = f_S2P
        spData%f_S2M        = f_S2M
        spData%f_P2M        = f_P2M
    end subroutine read_TECO_model_configs

    subroutine initialize()
        ! nday4out       = 0
        eJmx0          = Vcmax0*2.7                     ! @20C Leuning 1996 from Wullschleger (1993)
        QC             = (/300.,500.,250.,200.,300.,322.,28340.,23120./)!(/450.,380.,250.,119.,300.,322.,38340.,23120./)    !  leaf,wood,root,fine lit.,coarse lit.,Micr,Slow,Pass
        CN0            = (/50.,350.,60.,40.,300.,10.,20.,12./)
        NSCmin         = 1.                                                 ! none structural carbon pool
        Storage        = 60!32.09                                              ! g C/m2
        nsc            = 85.35
        stor_use       = Storage/times_storage_use                                       ! 720 hours, 30 days
        N_deposit      = 2.34/8760. ! Nitrogen input (gN/h/m2, )
        ! N_deposit      = 0.153/8760.
        ! N_deposit      = 0.234/8760.
        ! the unit of residence time is transformed from yearly to hourly
        ! tauC           = (/tau_L,tau_W,tau_R,tau_F,tau_C,tau_Micr,tau_Slow,tau_Pass/)*8760. 
        TauC           = (/Tau_Leaf,Tau_Wood,Tau_Root,Tau_F,Tau_C,Tau_Micro,Tau_slowSOM,Tau_Passive/)*8760.
        SLA            = SLAx/10000.          ! Convert unit from cm2/g to m2/g
        GLmax          = GLmax/8760.          ! growth rates of plant. Jian: per year to per hour ?
        GRmax          = GRmax/8760.
        Gsmax          = Gsmax/8760.
        accumulation   = 0.0               ! accumulative storage C?
        SNvcmax        = 1.0
        LAI            = LAIMIN
        bmleaf         = QC(1)/0.48
        bmstem         = QC(2)/0.48
        bmroot         = QC(3)/0.48
        bmplant        = bmstem+bmroot+bmleaf
        ! initial values of Nitrogen pools and C/N ratio
        alphaN         = 0.0    ! the transfer of N before littering
        NSN            = 0.35   ! 6.0 ! 0.35 according to Ma et al., 2022
        QNminer        = 1.2
        N_deficit      = 0.
        CN             = CN0
        QN             = QC/CN0
        QNplant        = QN(1) + QN(2) + QN(3)
        ! -----------------------------------------------------------------------------------------

        ! for soil conditions, physical processes
        thksl          = (/10.,10.,10.,10.,10.,20.,20.,20.,20.,20./)        ! thickness of every soil layer
        ! FRLEN          = (/0.75,0.2,0.02,0.015,0.005,0.0,0.0,0.0,0.0,0.0/)  ! ratio of roots in every layer, Oak Ridge FACE: Shuang
        FRLEN          = (/0.75,0.2,0.02,0.02,0.01,0.0,0.0,0.0,0.0,0.0/)
        ! liq_water      = (/0.01, 0.056, 0.056, 0.056, 0.056, 0.056, 0.056,0.056,0.056,0.056/)    ! unit m
        liq_water      = (/0.0355, 0.056, 0.056, 0.056, 0.056, 0.113, 0.113,0.113,0.113,0.113/)    ! unit m
        fwsoil         = 1.0                                                ! update in soilwater module
        topfws         = 1.0
        omega          = 1.0
        do i=1,10
            wcl(i)     = wsmax/100.
        enddo 
        zwt            = 0.0
        water_tw       = zwt*0.001 
        WILTPT         = wsmin/100.0
        FILDCP         = wsmax/100.0
        infilt         = 0.
        ! soil thermal dynamics in Yuanyuanversion
        sftmp          = -0.
        Tsnow          = -20.
        Twater         = 0.0
        Tice           = 0.0
        G              = 20.5
        Esoil          = 0.5*G
        snow_dsim      = 0.575
        dcount         = 50.
        dcount_soil    = 50.
        ice_tw         = 0.0   
        Tsoill         = (/ -0.09, 0.73, 1.3, 1.95, 2.3, 3., 4., 4.5, 5., 5.98/)  ! JJ MS thksl 10 20 30 40 50 70 90 110 130 150...  
        ! ice            = (/0.1, 0.0, 0., 0., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/) 
        ice            = (/0.021, 0.0, 0., 0., 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)
        shcap_snow     = 1000000.  ! tuneice worker better
        condu_snow     = 0.1
        condu_b        = 0.08  ! yuanyuan soil thermal version value  ... int: this par is not sensitive to CWE
        depth_ex       = 0.05
        diff_s         = 1.
        diff_snow      = 1.8    ! .. int diffusivity of snow not sensitive for ice
        albedo_snow    = 0.7
        resht          = 40.
        thd_snow_depth = 4.0
        b_bound        = 100.                            ! b_bound=0.1     !tuneice  not sensitive for ice
        infilt_rate    = 0.001
        fa             = 1
        fsub           = 0.1
        ! rho_snow=100.
        rho_snow       = 80.        !tuneice
        decay_m        = 2.2192      !aging factor on snow melting
        !----------------------------------------------------------------
        
        ! methane module. update: Shuang methane bog species even more shallowly rooted than the tundra. add initials for methane module Shuang version
        CH4_V  = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        ! CH4  = (/0.0952,0.1232,0.2128,0.3024,0.352,0.8,0.8,0.86,0.86,0.86/)
        CH4    = (/0.000152,0.05,0.6,0.7,0.7,1.7,1.7,1.7,1.7,1.7/)
        !!!!  #1.EBG put this paragraph outside of the time loop, initialization step
        Vp(1:3)=0.  !assume in the very beginning no bubbles exist in the first three layers (30cm)
        Vp(4:6)=0.001!0.005
        Vp(7:10)=0.01  !unit m3
        bubble_methane_tot  = 0.
        ! f = 0.1
        Nbub = 100.
        ! bubprob = 0.1
        ! Vmaxfraction = 0.1
        depth(1)=10.0                                  !calculate soil depth unit cm
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
        ! for daily initials in methane module              
        ! ********* for daily initials in soil thermal module
        soilt_d_simu = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./) 
        ice_d_simu   = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./) 
        soilt_d_obs  = (/0.,0.,0.,0.,0.,0.,0./) 
        zwt_d        = 0.0
        ! --------------------------------------------------
        simuCH4_d    = 0.0
        Pro_sum_d    = 0.0
        Oxi_sum_d    = 0.0
        Fdifu1_d     = 0.0
        Ebu_sum_d    = 0.0
        Pla_sum_d    = 0.0
        CH4V_d       = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)              
        !*****************************               
        !   *** ..int
        ! THE FIRST PART:  coupled canopy and soil model
        rh_d_old = 0.0
        diff_d = 0.0
        gpp_d_old  = 0.0   ! daily
        gpp_ra = 0.0   ! daily
        npp_d_old  = 0.0   ! daily
        NEP_d  = 0.0
        NEE_d  = 0.0
        ! rain_d,transp_d,evap_d
        transp_d = 0.0   ! daily
        Hcanop_d = 0.0   ! daily
        evap_d   = 0.0   ! daily
        ta       = 0.0         ! daily 
        omega_d  = 0.0
        Ts       = 0.0         ! daily
        rain_d   = 0.0     ! daily
        runoff_d = 0.0    ! daily
        LE_d     = 0.0
        RaL      = 0.0
        RaS      = 0.0
        RaR      = 0.0
        Rauto    = 0.0
        Rh_d     = 0.0
        N_up_d   = 0.
        N_fix_d  = 0.
        N_dep_d  = 0.
        N_leach_d= 0.
        N_vol_d  = 0.
        PAR_d    = 0.
        VPD_d    = 0.0
        RECO_d   = 0.0
        RLEAV_d  = 0.0 
        RWOOD_d  = 0.0
        RROOT_d  = 0.0
        GL_d     = 0.0
        GW_d     = 0.0
        GR_d     = 0.0
        LFALL_d  = 0.0
        NUP_d    = 0.0
        NVOL_d   = 0.
        NLEACH_d = 0.0
        NMIN_d   = 0.0
        N_LG_d   = 0.0
        N_WG_d   = 0.0
        N_RG_d   = 0.0
        N_LF_d   = 0.0
        N_WF_d   = 0.0
        N_RF_d   = 0.0
        WFALL_d  = 0.0
        RFALL_d  = 0.0
        mat_Rh_d = 0.0              ! Jian: for matrix model
        ! initialization of daily results according to SPRUCE-MIP
        ! carbon fluxes
        gpp_d             = 0.0
        npp_d             = 0.0
        nppLeaf_d         = 0.0
        nppWood_d         = 0.0
        nppStem_d         = 0.0           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        nppRoot_d         = 0.0
        nppOther_d        = 0.0
        ra_d              = 0.0
        raLeaf_d          = 0.0
        raStem_d          = 0.0
        raRoot_d          = 0.0
        raOther_d         = 0.0
        rMaint_d          = 0.0           ! maintenance respiration
        rGrowth_d         = 0.0           ! growth respiration
        rh_d              = 0.0           ! heterotrophic respiration
        nbp_d             = 0.0           ! NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        wetlandCH4_d      = 0.0           ! wetland net fluxes of CH4
        wetlandCH4prod_d  = 0.0           ! wetland net fluxes of CH4 production
        wetlandCH4cons_d  = 0.0           ! wetland net fluxes of CH4 consumption
        ! Carbon Pools  (KgC m-2)
        cLeaf_d           = 0.0
        cStem_d           = 0.0
        cRoot_d           = 0.0
        cOther_d          = 0.0           ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        cLitter_d         = 0.0           ! litter (excluding coarse woody debris), Jian: fine litter in TECO?
        cLitterCwd_d      = 0.0           ! cLitterCwd: carbon in coarse woody debris
        cSoil_d           = 0.0           ! cSoil: soil organic carbon (Jian: total soil carbon);
        cSoilLevels_d     = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)           ! cSoilLevels(depth-specific soil organic carbon, Jian: depth?);
        cSoilFast_d       = 0.0           ! cSoilPools (different pools without depth)
        cSoilSlow_d       = 0.0 
        cSoilPassive_d    = 0.0 
        cCH4_d            = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)           ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        fBNF_d            = 0.0           ! fBNF: biological nitrogen fixation;
        fN2O_d            = 0.0           ! fN2O: loss of nitrogen through emission of N2O;
        fNloss_d          = 0.0           ! fNloss:Total loss of nitrogen to the atmosphere and from leaching;
        fNnetmin_d        = 0.0           ! net mineralizaiton
        fNdep_d           = 0.0           ! deposition of N
        ! Nitrogen pools (kgN m-2)
        nLeaf_d           = 0.0
        nStem_d           = 0.0
        nRoot_d           = 0.0
        nOther_d          = 0.0
        nLitter_d         = 0.0
        nLitterCwd_d      = 0.0
        nSoil_d           = 0.0
        nMineral_d        = 0.0           ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        hfls_d            = 0.0           ! Sensible heat flux;
        hfss_d            = 0.0           ! Latent heat flux;
        SWnet_d           = 0.0           ! Net shortwave radiation;
        LWnet_d           = 0.0           ! Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        ec_d              = 0.0
        tran_d            = 0.0
        es_d              = 0.0           ! Canopy evaporation; Canopy transpiration; Soil evaporation
        hfsbl_d           = 0.0           ! Snow sublimation
        mrro_d            = 0.0
        mrros_d           = 0.0
        mrrob_d           = 0.0           ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        mrso_d            = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)             ! Kg m-2, soil moisture in each soil layer
        tsl_d             = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)             ! K, soil temperature in each soil layer
        tsland_d          = 0.0                                           ! K, surface temperature
        wtd_d             = 0.0                                           ! m, Water table depth
        snd_d             = 0.0                                           ! m, Total snow depth
        lai_d             = 0.0                                           ! m2 m-2, Leaf area index
        ! not used in SPRUCE-MIP
    end subroutine init_day

    subroutine init_monthly()
        implicit none
        ! Jian: we need add the monthly outputs according to the SPRUCE-MIP's requirement.
        ! carbon result:
        gpp_m             = 0.0
        npp_m             = 0.0
        nppLeaf_m         = 0.0
        nppWood_m         = 0.0
        nppStem_m         = 0.0           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        nppRoot_m         = 0.0
        nppOther_m        = 0.0
        ra_m              = 0.0
        raLeaf_m          = 0.0
        raStem_m          = 0.0
        raRoot_m          = 0.0
        raOther_m         = 0.0 
        rMaint_m          = 0.0           ! maintenance respiration
        rGrowth_m         = 0.0           ! growth respiration
        rh_m              = 0.0           ! heterotrophic respiration
        nbp_m             = 0.0           ! NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        wetlandCH4_m      = 0.0           ! wetland net fluxes of CH4
        wetlandCH4prod_m  = 0.0           ! wetland net fluxes of CH4 production
        wetlandCH4cons_m  = 0.0           ! wetland net fluxes of CH4 consumption
        ! Carbon Pools  (KgC m-2)
        cLeaf_m           = 0.0
        cStem_m           = 0.0
        cRoot_m           = 0.0
        cOther_m          = 0.0  ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        cLitter_m         = 0.0  ! litter (excluding coarse woody debris), Jian: fine litter in TECO?
        cLitterCwd_m      = 0.0  ! cLitterCwd: carbon in coarse woody debris
        cSoil_m           = 0.0  ! cSoil: soil organic carbon (Jian: total soil carbon);
        cSoilLevels_m     = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)  ! cSoilLevels(depth-specific soil organic carbon, Jian: depth?);
        cSoilFast_m       = 0.0  ! cSoilPools (different pools without depth)
        cSoilSlow_m       = 0.0  
        cSoilPassive_m    = 0.0
        cCH4_m            = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)  ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        fBNF_m            = 0.0  ! fBNF: biological nitrogen fixation;
        fN2O_m            = 0.0  ! fN2O: loss of nitrogen through emission of N2O;
        fNloss_m          = 0.0  ! fNloss:Total loss of nitrogen to the atmosphere and from leaching;
        fNnetmin_m        = 0.0  ! net mineralizaiton
        fNdep_m           = 0.0  ! deposition of N
        ! Nitrogen pools (kgN m-2)
        nLeaf_m           = 0.0
        nStem_m           = 0.0
        nRoot_m           = 0.0
        nOther_m          = 0.0
        nLitter_m         = 0.0
        nLitterCwd_m      = 0.0
        nSoil_m           = 0.0
        nMineral_m        = 0.0  ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        hfls_m            = 0.0  ! Sensible heat flux;
        hfss_m            = 0.0  ! Latent heat flux;
        SWnet_m           = 0.0  ! Net shortwave radiation;
        LWnet_m           = 0.0  !    Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        ec_m              = 0.0
        tran_m            = 0.0
        es_m              = 0.0  ! Canopy evaporation; Canopy transpiration; Soil evaporation
        hfsbl_m           = 0.0  ! Snow sublimation
        mrro_m            = 0.0
        mrros_m           = 0.0
        mrrob_m           = 0.0  ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        mrso_m            = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)             ! Kg m-2, soil moisture in each soil layer
        tsl_m             = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)             ! K, soil temperature in each soil layer
        tsland_m          = 0.                                            ! K, surface temperature
        wtd_m             = 0.                                            ! m, Water table depth
        snd_m             = 0.                                            ! m, Total snow depth
        lai_m             = 0.                                            ! m2 m-2, Leaf area index
        ! not used in SPRUCE-MIP
    end subroutine init_monthly

    subroutine init_year()
        implicit none
        ! GDD5      = 0.0; 
        ! onset     = 0;   
        ! phenoset  = 0; 
        !  diff_yr=0.0; gpp_yr=0.0
        ! R_Ntr_yr = 0.;  NPP_yr    = 0.0; Rh_yr =0.0;  Rh4_yr=0.0; Rh5_yr=0.0
        ! Rh6_yr   = 0.0; Rh7_yr    = 0.0; Rh8_yr=0.0;  Ra_yr =0.0; GL_yr=0.0
        ! GW_yr    = 0.0; GR_yr     = 0.0; Pool1=0.0;   Pool2=0.0; Pool3=0.0
        ! Pool4    = 0.0; Pool5     = 0.0; Pool6=0.0;   Pool7=0.0; Pool8=0.0
        ! out1_yr  = 0.0; out2_yr   = 0.0; out3_yr=0.0; out4_yr = 0.0; out5_yr = 0.0
        ! out6_yr  = 0.0; out7_yr   = 0.0; out8_yr   = 0.0;  NEE_yr    = 0.0
        ! ! water fluxes
        ! rain_yr  = 0.0; transp_yr = 0.0; evap_yr   = 0.0; runoff_yr = 0.0
        ! ! Nitrogen fluxes
        ! N_up_yr  = 0;   N_fix_yr  = 0.; N_dep_yr=0.; N_leach_yr=0.; N_vol_yr=0.
        ! ! ============================== test variable
        ! fwsoil_yr=0.;  omega_yr=0.
        ! topfws_yr=0.
        ! hoy=0
    end subroutine init_year

    subroutine init_update_year()
        GDD5      = 0.0 
        onset     = 0
        phenoset  = 0
        ! carbon fluxes (Kg C m-2 s-1)
        gpp_y            = 0.
        npp_y            = 0.
        nppLeaf_y        = 0.
        nppWood_y        = 0.
        nppStem_y        = 0.
        nppRoot_y        = 0.
        nppOther_y       = 0.  ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        ra_y             = 0.
        raLeaf_y         = 0.
        raStem_y         = 0.
        raRoot_y         = 0.
        raOther_y        = 0.
        rMaint_y         = 0.
        rGrowth_y        = 0.                                            ! maintenance respiration and growth respiration
        rh_y             = 0.
        nbp_y            = 0.                                                  ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        wetlandCH4_y     = 0.
        wetlandCH4prod_y = 0.
        wetlandCH4cons_y = 0.                ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        cLeaf_y          = 0.
        cStem_y          = 0.
        cRoot_y          = 0.
        cOther_y         = 0.                          ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        cLitter_y        = 0.
        cLitterCwd_y     = 0.                                       ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        cSoil_y          = 0.
        cSoilLevels_y    = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)
        cSoilFast_y      = 0.
        cSoilSlow_y      = 0.
        cSoilPassive_y   = 0.                                       ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        cCH4_y           = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)                                                         ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        fBNF_y           = 0.
        fN2O_y           = 0.
        fNloss_y         = 0.
        fNnetmin_y       = 0.
        fNdep_y          = 0.               ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        nLeaf_y          = 0.
        nStem_y          = 0.
        nRoot_y          = 0.
        nOther_y         = 0.
        nLitter_y        = 0.
        nLitterCwd_y     = 0.
        nSoil_y          = 0.
        nMineral_y       = 0.                 ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        hfls_y           = 0.
        hfss_y           = 0.
        SWnet_y          = 0.
        LWnet_y          = 0.                              ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        ec_y             = 0.
        tran_y           = 0.
        es_y             = 0.                                          ! Canopy evaporation; Canopy transpiration; Soil evaporation
        hfsbl_y          = 0.                                                      ! Snow sublimation
        mrro_y           = 0.
        mrros_y          = 0.
        mrrob_y          = 0.                                     ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        mrso_y           = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)                                                  ! Kg m-2, soil moisture in each soil layer
        tsl_y            = (/0.,0.,0.,0.,0.,0.,0.,0.,0.,0./)                                                  ! K, soil temperature in each soil layer
        tsland_y         = 0.                                                      ! K, surface temperature
        wtd_y            = 0.                                                     ! m, Water table depth
        snd_y            = 0.                                                    ! m, Total snow depth
        lai_y            = 0.                                                    ! m2 m-2, Leaf area index
        test_gpp_y       = (/0.,0.,0.,0.,0.,0.,0.,0.,0./)
    end subroutine init_update_year

    subroutine assign_all_results(hours, days, months, years)
        implicit none
        integer hours, days, months, years
        allocate(all_gpp_h(hours))
        allocate(all_npp_h(hours))
        allocate(all_nppLeaf_h(hours))
        allocate(all_nppWood_h(hours)) 
        allocate(all_nppStem_h(hours))
        allocate(all_nppRoot_h(hours))
        allocate(all_nppOther_h(hours))                  ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(all_ra_h(hours))
        allocate(all_raLeaf_h(hours))
        allocate(all_raStem_h(hours))
        allocate(all_raRoot_h(hours))
        allocate(all_raOther_h(hours))
        allocate(all_rMaint_h(hours))
        allocate(all_rGrowth_h(hours))                                             ! maintenance respiration and growth respiration
        allocate(all_rh_h(hours))
        allocate(all_nbp_h(hours))                                                    ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(all_wetlandCH4_h(hours))
        allocate(all_wetlandCH4prod_h(hours))
        allocate(all_wetlandCH4cons_h(hours))                ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(all_cLeaf_h(hours))
        allocate(all_cStem_h(hours))
        allocate(all_cRoot_h(hours))
        allocate(all_cOther_h(hours))                             ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(all_cLitter_h(hours))
        allocate(all_cLitterCwd_h(hours))                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(all_cSoil_h(hours))
        allocate(all_cSoilLevels_h(hours,nlayers))
        allocate(all_cSoilFast_h(hours))
        allocate(all_cSoilSlow_h(hours))
        allocate(all_cSoilPassive_h(hours))                            ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(all_cCH4_h(hours,nlayers))                                                        ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(all_fBNF_h(hours))
        allocate(all_fN2O_h(hours))
        allocate(all_fNloss_h(hours))
        allocate(all_fNnetmin_h(hours))
        allocate(all_fNdep_h(hours))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(all_nLeaf_h(hours))
        allocate(all_nStem_h(hours))
        allocate(all_nRoot_h(hours))
        allocate(all_nOther_h(hours))
        allocate(all_nLitter_h(hours))
        allocate(all_nLitterCwd_h(hours))
        allocate(all_nSoil_h(hours))
        allocate(all_nMineral_h(hours))                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(all_hfls_h(hours))
        allocate(all_hfss_h(hours))
        allocate(all_SWnet_h(hours))
        allocate(all_LWnet_h(hours))                               ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(all_ec_h(hours))
        allocate(all_tran_h(hours))
        allocate(all_es_h(hours))                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(all_hfsbl_h(hours))                                                         ! Snow sublimation
        allocate(all_mrro_h(hours))
        allocate(all_mrros_h(hours))
        allocate(all_mrrob_h(hours))                                        ! Total runoff; Surface runoff; Subsurface runoff
        ! Other
        allocate(all_mrso_h(hours,nlayers))                                                   ! Kg m-2, soil moisture in each soil layer
        allocate(all_tsl_h(hours,nlayers))                                                    ! K, soil temperature in each soil layer
        allocate(all_tsland_h(hours))                                                          ! K, surface temperature
        allocate(all_wtd_h(hours))                                                             ! m, Water table depth
        allocate(all_snd_h(hours))                                                             ! m, Total snow depth
        allocate(all_lai_h(hours))  
        allocate(all_gdd5_h(hours))
        allocate(all_onset_h(hours))  
        allocate(all_storage_h(hours))
        allocate(all_add_h(hours)) 
        allocate(all_accumulation_h(hours))
        allocate(all_test_h(hours,9))                                                        ! m2 m-2, Leaf area index

        ! daily: 
        ! ---------------------------------------------------------------------
        ! carbon fluxes (Kg C m-2 s-1)
        allocate(all_gpp_d(days))
        allocate(all_npp_d(days))
        allocate(all_nppLeaf_d(days))
        allocate(all_nppWood_d(days))
        allocate(all_nppStem_d(days))
        allocate(all_nppRoot_d(days))
        allocate(all_nppOther_d(days))   ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(all_ra_d(days))
        allocate(all_raLeaf_d(days))
        allocate(all_raStem_d(days))
        allocate(all_raRoot_d(days))
        allocate(all_raOther_d(days))
        allocate(all_rMaint_d(days))
        allocate(all_rGrowth_d(days))                                             ! maintenance respiration and growth respiration
        allocate(all_rh_d(days))
        allocate(all_nbp_d(days))                                                    ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(all_wetlandCH4_d(days))
        allocate(all_wetlandCH4prod_d(days))
        allocate(all_wetlandCH4cons_d(days))                ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(all_cLeaf_d(days))
        allocate(all_cStem_d(days))
        allocate(all_cRoot_d(days))
        allocate(all_cOther_d(days))                            ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(all_cLitter_d(days))
        allocate(all_cLitterCwd_d(days))                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(all_cSoil_d(days))
        allocate(all_cSoilLevels_d(days,nlayers))
        allocate(all_cSoilFast_d(days))
        allocate(all_cSoilSlow_d(days))
        allocate(all_cSoilPassive_d(days))                            ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(all_cCH4_d(days,nlayers))                                                          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(all_fBNF_d(days))
        allocate(all_fN2O_d(days))
        allocate(all_fNloss_d(days))
        allocate(all_fNnetmin_d(days))
        allocate(all_fNdep_d(days))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(all_nLeaf_d(days))
        allocate(all_nStem_d(days))
        allocate(all_nRoot_d(days))
        allocate(all_nOther_d(days))
        allocate(all_nLitter_d(days))
        allocate(all_nLitterCwd_d(days))
        allocate(all_nSoil_d(days))
        allocate(all_nMineral_d(days))                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(all_hfls_d(days))
        allocate(all_hfss_d(days))
        allocate(all_SWnet_d(days))
        allocate(all_LWnet_d(days))                               ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(all_ec_d(days))
        allocate(all_tran_d(days))
        allocate(all_es_d(days))                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(all_hfsbl_d(days))                                                         ! Snow sublimation
        allocate(all_mrro_d(days))
        allocate(all_mrros_d(days))
        allocate(all_mrrob_d(days))                                        ! Total runoff; Surface runoff; Subsurface runoff
        ! Other
        allocate(all_mrso_d(days,nlayers))                                                   ! Kg m-2, soil moisture in each soil layer
        allocate(all_tsl_d(days,nlayers))                                                    ! K, soil temperature in each soil layer
        allocate(all_tsland_d(days))                                                          ! K, surface temperature
        allocate(all_wtd_d(days))                                                             ! m, Water table depth
        allocate(all_snd_d(days))                                                             ! m, Total snow depth
        allocate(all_lai_d(days))                                                             ! m2 m-2, Leaf area index


        ! monthly
        ! carbon fluxes (Kg C m-2 s-1)
        allocate(all_gpp_m(months))
        allocate(all_npp_m(months))
        allocate(all_nppLeaf_m(months))
        allocate(all_nppWood_m(months))
        allocate(all_nppStem_m(months))
        allocate(all_nppRoot_m(months))
        allocate(all_nppOther_m(months))   ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(all_ra_m(months))
        allocate(all_raLeaf_m(months))
        allocate(all_raStem_m(months))
        allocate(all_raRoot_m(months))
        allocate(all_raOther_m(months))
        allocate(all_rMaint_m(months))
        allocate(all_rGrowth_m(months))                                             ! maintenance respiration and growth respiration
        allocate(all_rh_m(months))
        allocate(all_nbp_m(months))                                                    ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(all_wetlandCH4_m(months))
        allocate(all_wetlandCH4prod_m(months))
        allocate(all_wetlandCH4cons_m(months))                ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(all_cLeaf_m(months))
        allocate(all_cStem_m(months))
        allocate(all_cRoot_m(months))
        allocate(all_cOther_m(months))                             ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(all_cLitter_m(months))
        allocate(all_cLitterCwd_m(months))                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(all_cSoil_m(months))
        allocate(all_cSoilLevels_m(months,nlayers))
        allocate(all_cSoilFast_m(months))
        allocate(all_cSoilSlow_m(months))
        allocate(all_cSoilPassive_m(months))                           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(all_cCH4_m(months,nlayers))                                                       ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(all_fBNF_m(months))
        allocate(all_fN2O_m(months))
        allocate(all_fNloss_m(months))
        allocate(all_fNnetmin_m(months))
        allocate(all_fNdep_m(months))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(all_nLeaf_m(months))
        allocate(all_nStem_m(months))
        allocate(all_nRoot_m(months))
        allocate(all_nOther_m(months))
        allocate(all_nLitter_m(months))
        allocate(all_nLitterCwd_m(months))
        allocate(all_nSoil_m(months))
        allocate(all_nMineral_m(months))                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(all_hfls_m(months))
        allocate(all_hfss_m(months))
        allocate(all_SWnet_m(months))
        allocate(all_LWnet_m(months))                                ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(all_ec_m(months))
        allocate(all_tran_m(months))
        allocate(all_es_m(months))                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(all_hfsbl_m(months))                                                         ! Snow sublimation
        allocate(all_mrro_m(months))
        allocate(all_mrros_m(months))
        allocate(all_mrrob_m(months)) 
        ! Other
        allocate(all_mrso_m(months,nlayers))                                                   ! Kg m-2, soil moisture in each soil layer
        allocate(all_tsl_m(months,nlayers))                                                    ! K, soil temperature in each soil layer
        allocate(all_tsland_m(months))                                                          ! K, surface temperature
        allocate(all_wtd_m(months))                                                             ! m, Water table depth
        allocate(all_snd_m(months))                                                             ! m, Total snow depth
        allocate(all_lai_m(months))                                                             ! m2 m-2, Leaf area index
    end subroutine assign_all_results

    subroutine deallocate_all_results()
        deallocate(all_gpp_h)
        deallocate(all_npp_h)
        deallocate(all_nppLeaf_h)
        deallocate(all_nppWood_h) 
        deallocate(all_nppStem_h)
        deallocate(all_nppRoot_h)
        deallocate(all_nppOther_h)                  ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        deallocate(all_ra_h)
        deallocate(all_raLeaf_h)
        deallocate(all_raStem_h)
        deallocate(all_raRoot_h)
        deallocate(all_raOther_h)
        deallocate(all_rMaint_h)
        deallocate(all_rGrowth_h)                                             ! maintenance respiration and growth respiration
        deallocate(all_rh_h)
        deallocate(all_nbp_h)                                                    ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        deallocate(all_wetlandCH4_h)
        deallocate(all_wetlandCH4prod_h)
        deallocate(all_wetlandCH4cons_h)                ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        deallocate(all_cLeaf_h)
        deallocate(all_cStem_h)
        deallocate(all_cRoot_h)
        deallocate(all_cOther_h)                             ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        deallocate(all_cLitter_h)
        deallocate(all_cLitterCwd_h)                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        deallocate(all_cSoil_h)
        deallocate(all_cSoilLevels_h)
        deallocate(all_cSoilFast_h)
        deallocate(all_cSoilSlow_h)
        deallocate(all_cSoilPassive_h)                            ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        deallocate(all_cCH4_h)                                                        ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        deallocate(all_fBNF_h)
        deallocate(all_fN2O_h)
        deallocate(all_fNloss_h)
        deallocate(all_fNnetmin_h)
        deallocate(all_fNdep_h)                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        deallocate(all_nLeaf_h)
        deallocate(all_nStem_h)
        deallocate(all_nRoot_h)
        deallocate(all_nOther_h)
        deallocate(all_nLitter_h)
        deallocate(all_nLitterCwd_h)
        deallocate(all_nSoil_h)
        deallocate(all_nMineral_h)                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        deallocate(all_hfls_h)
        deallocate(all_hfss_h)
        deallocate(all_SWnet_h)
        deallocate(all_LWnet_h)                               ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        deallocate(all_ec_h)
        deallocate(all_tran_h)
        deallocate(all_es_h)                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        deallocate(all_hfsbl_h)                                                         ! Snow sublimation
        deallocate(all_mrro_h)
        deallocate(all_mrros_h)
        deallocate(all_mrrob_h)                                        ! Total runoff; Surface runoff; Subsurface runoff
        ! Other
        deallocate(all_mrso_h)                                                   ! Kg m-2, soil moisture in each soil layer
        deallocate(all_tsl_h)                                                    ! K, soil temperature in each soil layer
        deallocate(all_tsland_h)                                                          ! K, surface temperature
        deallocate(all_wtd_h)                                                             ! m, Water table depth
        deallocate(all_snd_h)                                                             ! m, Total snow depth
        deallocate(all_lai_h)       
        deallocate(all_gdd5_h)
        deallocate(all_onset_h) 
        deallocate(all_storage_h)
        deallocate(all_add_h) 
        deallocate(all_accumulation_h)  
        deallocate(all_test_h)                                                  ! m2 m-2, Leaf area index

        ! daily: 
        ! ---------------------------------------------------------------------
        ! carbon fluxes (Kg C m-2 s-1)
        deallocate(all_gpp_d)
        deallocate(all_npp_d)
        deallocate(all_nppLeaf_d)
        deallocate(all_nppWood_d)
        deallocate(all_nppStem_d)
        deallocate(all_nppRoot_d)
        deallocate(all_nppOther_d)   ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        deallocate(all_ra_d)
        deallocate(all_raLeaf_d)
        deallocate(all_raStem_d)
        deallocate(all_raRoot_d)
        deallocate(all_raOther_d)
        deallocate(all_rMaint_d)
        deallocate(all_rGrowth_d)                                             ! maintenance respiration and growth respiration
        deallocate(all_rh_d)
        deallocate(all_nbp_d)                                                    ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        deallocate(all_wetlandCH4_d)
        deallocate(all_wetlandCH4prod_d)
        deallocate(all_wetlandCH4cons_d)                ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        deallocate(all_cLeaf_d)
        deallocate(all_cStem_d)
        deallocate(all_cRoot_d)
        deallocate(all_cOther_d)                            ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        deallocate(all_cLitter_d)
        deallocate(all_cLitterCwd_d)                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        deallocate(all_cSoil_d)
        deallocate(all_cSoilLevels_d)
        deallocate(all_cSoilFast_d)
        deallocate(all_cSoilSlow_d)
        deallocate(all_cSoilPassive_d)                            ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        deallocate(all_cCH4_d)                                                          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        deallocate(all_fBNF_d)
        deallocate(all_fN2O_d)
        deallocate(all_fNloss_d)
        deallocate(all_fNnetmin_d)
        deallocate(all_fNdep_d)                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        deallocate(all_nLeaf_d)
        deallocate(all_nStem_d)
        deallocate(all_nRoot_d)
        deallocate(all_nOther_d)
        deallocate(all_nLitter_d)
        deallocate(all_nLitterCwd_d)
        deallocate(all_nSoil_d)
        deallocate(all_nMineral_d)                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        deallocate(all_hfls_d)
        deallocate(all_hfss_d)
        deallocate(all_SWnet_d)
        deallocate(all_LWnet_d)                               ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        deallocate(all_ec_d)
        deallocate(all_tran_d)
        deallocate(all_es_d)                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        deallocate(all_hfsbl_d)                                                         ! Snow sublimation
        deallocate(all_mrro_d)
        deallocate(all_mrros_d)
        deallocate(all_mrrob_d)                                        ! Total runoff; Surface runoff; Subsurface runoff
        ! Other
        deallocate(all_mrso_d)                                                   ! Kg m-2, soil moisture in each soil layer
        deallocate(all_tsl_d)                                                    ! K, soil temperature in each soil layer
        deallocate(all_tsland_d)                                                          ! K, surface temperature
        deallocate(all_wtd_d)                                                             ! m, Water table depth
        deallocate(all_snd_d)                                                             ! m, Total snow depth
        deallocate(all_lai_d)                                                             ! m2 m-2, Leaf area index


        ! monthly
        ! carbon fluxes (Kg C m-2 s-1)
        deallocate(all_gpp_m)
        deallocate(all_npp_m)
        deallocate(all_nppLeaf_m)
        deallocate(all_nppWood_m)
        deallocate(all_nppStem_m)
        deallocate(all_nppRoot_m)
        deallocate(all_nppOther_m)   ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        deallocate(all_ra_m)
        deallocate(all_raLeaf_m)
        deallocate(all_raStem_m)
        deallocate(all_raRoot_m)
        deallocate(all_raOther_m)
        deallocate(all_rMaint_m)
        deallocate(all_rGrowth_m)                                             ! maintenance respiration and growth respiration
        deallocate(all_rh_m)
        deallocate(all_nbp_m)                                                    ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        deallocate(all_wetlandCH4_m)
        deallocate(all_wetlandCH4prod_m)
        deallocate(all_wetlandCH4cons_m)                ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        deallocate(all_cLeaf_m)
        deallocate(all_cStem_m)
        deallocate(all_cRoot_m)
        deallocate(all_cOther_m)                             ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        deallocate(all_cLitter_m)
        deallocate(all_cLitterCwd_m)                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        deallocate(all_cSoil_m)
        deallocate(all_cSoilLevels_m)
        deallocate(all_cSoilFast_m)
        deallocate(all_cSoilSlow_m)
        deallocate(all_cSoilPassive_m)                           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        deallocate(all_cCH4_m)                                                       ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        deallocate(all_fBNF_m)
        deallocate(all_fN2O_m)
        deallocate(all_fNloss_m)
        deallocate(all_fNnetmin_m)
        deallocate(all_fNdep_m)                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        deallocate(all_nLeaf_m)
        deallocate(all_nStem_m)
        deallocate(all_nRoot_m)
        deallocate(all_nOther_m)
        deallocate(all_nLitter_m)
        deallocate(all_nLitterCwd_m)
        deallocate(all_nSoil_m)
        deallocate(all_nMineral_m)                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        deallocate(all_hfls_m)
        deallocate(all_hfss_m)
        deallocate(all_SWnet_m)
        deallocate(all_LWnet_m)                                ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        deallocate(all_ec_m)
        deallocate(all_tran_m)
        deallocate(all_es_m)                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        deallocate(all_hfsbl_m)                                                         ! Snow sublimation
        deallocate(all_mrro_m)
        deallocate(all_mrros_m)
        deallocate(all_mrrob_m) 
        ! Other
        deallocate(all_mrso_m)                                                   ! Kg m-2, soil moisture in each soil layer
        deallocate(all_tsl_m)                                                    ! K, soil temperature in each soil layer
        deallocate(all_tsland_m)                                                          ! K, surface temperature
        deallocate(all_wtd_m)                                                             ! m, Water table depth
        deallocate(all_snd_m)                                                             ! m, Total snow depth
        deallocate(all_lai_m)                                                             ! m2 m-2, Leaf area index
    end subroutine deallocate_all_results
    

!     ! some functions to get parameters or some input data (eg. forcing data, observation data) 
!     !=================================================================
!     subroutine get_params()   
!         ! Jian: read parameters from the parameter file
!         implicit none
!         parafile = TRIM(parafile)
!         ! open and read input file for getting climate data
!         open(10,file=parafile,status='old')
!         read(10,11)commts
!         read(10,*)lat,longi,wsmax,wsmin
!         read(10,11)commts
!         read(10,*)LAIMAX,LAIMIN,rdepth,Rootmax,Stemmax    
!         read(10,11)commts
!         read(10,*)SapR,SapS,SLAx,GLmax,GRmax,Gsmax,stom_n
!         read(10,11)commts
!         read(10,*)a1,Ds0,Vcmax0,extkU,xfang,alpha
!         read(10,11)commts
!         read(10,*)Tau_Leaf,Tau_Wood,Tau_Root,Tau_F,Tau_C,Tau_Micro,Tau_slowSOM,Tau_Passive
!         read(10,11)commts
!         read(10,*)gddonset,Q10,Rl0,Rs0,Rr0
!         ! added for pars in methane module
!         read(10,11)commts
!         read(10,*)r_me,Q10pro,kCH4,Omax,CH4_thre,Tveg,Tpro_me,Toxi                      !this line is for MCMEME     
!         read(10,11)commts
!         read(10,*)f,bubprob,Vmaxfraction
! 11  format(a132)
!         close(10)
!     end subroutine get_params
    
    subroutine get_forcingdata()
        implicit none
        real temp_forcing(nterms, max_nlines)
        integer STAT, COUNT, n, m
        COUNT = 0
        m     = 1
        OPEN(1,FILE=climatefile,status='old',ACTION='read',IOSTAT=STAT)
        read(1,'(a160)') commts
        DO WHILE (.TRUE.)
            COUNT=COUNT+1
            READ(1,*,IOSTAT=STAT) (temp_forcing(n,COUNT), n=1, nterms)
            IF(STAT .NE. 0) EXIT
        ENDDO
        nforcing = COUNT - 1
        CLOSE(1)
        ! initialize the data type of forcing: Tair Tsoil RH VPD Rain WS PAR CO2
        allocate(forcing%year(nforcing))
        allocate(forcing%doy(nforcing))
        allocate(forcing%hour(nforcing))
        allocate(forcing%Tair(nforcing))
        allocate(forcing%Tsoil(nforcing))
        allocate(forcing%RH(nforcing))
        allocate(forcing%VPD(nforcing))
        allocate(forcing%Rain(nforcing))
        allocate(forcing%WS(nforcing))
        allocate(forcing%PAR(nforcing))
        allocate(forcing%CO2(nforcing))
        allocate(forcing%PBOT(nforcing))
        allocate(forcing%Ndep(nforcing))
        ! ----------------------------------------------------------------------
        forcing%year  = int(temp_forcing(1,:))
        forcing%doy   = int(temp_forcing(2,:))
        forcing%hour  = int(temp_forcing(3,:))
        forcing%Tair  = temp_forcing(4,:)
        forcing%Tsoil = temp_forcing(5,:)
        forcing%RH    = temp_forcing(6,:)
        forcing%VPD   = temp_forcing(7,:)
        forcing%Rain  = temp_forcing(8,:)
        forcing%WS    = temp_forcing(9,:)
        forcing%PAR   = temp_forcing(10,:)
        forcing%CO2   = temp_forcing(11,:)
        forcing%PBOT  = temp_forcing(12,:)
        forcing%Ndep  = temp_forcing(13,:)
    end subroutine get_forcingdata

    subroutine get_snowdepth()
        implicit none
        real temp_snow_depth(max_nlines)
        integer istat1, m

        ! integer m,n,istat1,lines,yr_length
        real snow_depth_read
        integer year,doy,hour

        open(11,file = snowdepthfile, status ='old',ACTION='read', IOSTAT=istat1)
        read(11,'(a160)') commts ! skip 2 lines of input met data file
        m = 0  ! to record the lines in a file
        do
            m=m+1
            read (11,*,IOSTAT=istat1)year,doy,hour,snow_depth_read
            if(istat1<0)exit
            temp_snow_depth(m)=snow_depth_read     
        enddo
        close(11)    ! close snow depth file
        allocate(snow_in(m-1))
        snow_in = temp_snow_depth(1:m-1)
        return
    end subroutine get_snowdepth
end module mod_data