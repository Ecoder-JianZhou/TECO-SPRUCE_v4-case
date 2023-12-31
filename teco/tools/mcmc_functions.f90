module mcmc_functions
    ! some functions that both driver module and MCMC module.
    use mod_data
    implicit none

    ! parameters and observation files

    integer npar, nDAsimu, ncov, nRand
    real search_scale
    logical :: do_mc_out_hr, do_mc_out_day, do_mc_out_mon, do_mc_out_yr

    real mc_lat, mc_Longitude, mc_wsmax, mc_wsmin
    real mc_LAIMAX, mc_LAIMIN, mc_rdepth, mc_Rootmax, mc_Stemmax
    real mc_SapR, mc_SapS, mc_SLA, mc_GLmax, mc_GRmax, mc_Gsmax, mc_stom_n
    real mc_a1, mc_Ds0, mc_Vcmx0, mc_extkU, mc_xfang, mc_alpha
    real mc_Tau_Leaf, mc_Tau_Wood, mc_Tau_Root, mc_Tau_F
    real mc_Tau_C,  mc_Tau_Micro, mc_Tau_SlowSOM, mc_Tau_Passive
    real mc_gddonset, mc_Q10, mc_Rl0, mc_Rs0, mc_Rr0
    real mc_r_me, mc_Q10pro, mc_kCH4, mc_Omax, mc_CH4_thre
    real mc_Tveg, mc_Tpro_me, mc_Toxi
    real mc_f, mc_bubprob, mc_Vmaxfraction
    real mc_Q10rh, mc_JV, mc_Entrpy
    real mc_etaL, mc_etaW, mc_etaR
    real mc_f_F2M, mc_f_C2M, mc_f_C2S, mc_f_M2S
    real mc_f_M2P, mc_f_S2P, mc_f_S2M, mc_f_P2M

    real, allocatable :: parval(:), parmin(:), parmax(:)
    character(20), allocatable :: parnames(:)

    ! observational file path
    character(500) :: obsfile_gpp_d, obsfile_nee_d, obsfile_reco_d
    character(500) :: obsfile_gpp_h, obsfile_nee_h, obsfile_reco_h
    ! methane   
    character(500) :: obsfile_ch4_h
    ! c pools
    character(500) :: obsfile_cleaf, obsfile_cwood 

    character(500) :: obsfile_anpp_y, obsfile_bnpp_y 
    character(500) :: obsfile_lai_h, obsfile_npp_y, obsfile_reco_y 

    ! variables for calculating the cost in MCMC processes
    type interCostVariable
        character(300) :: filepath
        logical :: existOrNot
        real, allocatable :: obsData(:,:)
        real, allocatable :: mdData(:,:)
    end type interCostVariable

    type allCostVariables
    ! default variables, you can add the variable names here. (year, doy, hour, value, std.)
        ! carbon flux 
        type(interCostVariable) :: gpp_d
        type(interCostVariable) :: nee_d
        type(interCostVariable) :: reco_d
        type(interCostVariable) :: gpp_h
        type(interCostVariable) :: nee_h
        type(interCostVariable) :: reco_h
        ! methane
        type(interCostVariable) :: ch4_h
        ! c pools
        type(interCostVariable) :: cleaf    ! foliage
        type(interCostVariable) :: cwood
        ! for different species
        type(interCostVariable) :: anpp_y
        type(interCostVariable) :: bnpp_y
        type(interCostVariable) :: lai_h
        type(interCostVariable) :: npp_y
        type(interCostVariable) :: reco_y
    end type allCostVariables

    type(allCostVariables) :: vars4MCMC      ! define a allCostVariables first

    ! variables for marking the cycle number
    integer mc_itime_gpp_d, mc_itime_nee_d, mc_itime_reco_d
    integer mc_itime_gpp_h, mc_itime_nee_h, mc_itime_reco_h
    integer mc_itime_ch4_h, mc_itime_cleaf, mc_itime_cwood
    integer mc_itime_anpp_y, mc_itime_bnpp_y, mc_itime_lai_h
    integer mc_itime_npp_y, mc_itime_reco_y
    integer mc_iyear,  mc_iday, mc_ihour

    contains

    subroutine mcmc_functions_init()
        implicit none
        mc_itime_gpp_d  = 1
        mc_itime_nee_d  = 1 
        mc_itime_reco_d = 1
        mc_itime_gpp_h  = 1
        mc_itime_nee_h  = 1
        mc_itime_reco_h = 1
        mc_itime_ch4_h  = 1
        mc_itime_cleaf  = 1
        mc_itime_cwood  = 1

        mc_itime_anpp_y = 1
        mc_itime_bnpp_y = 1 
        mc_itime_lai_h  = 1
        mc_itime_npp_y  = 1
        mc_itime_reco_y = 1

        mc_iyear = 1
        mc_iday  = 1
        mc_ihour = 1
    end subroutine mcmc_functions_init

    subroutine readConfsNml()
    ! default nml file name of "TECO_MCMC_configs.nml"
        implicit none
        character(20) :: parnames_1, parnames_2, parnames_3, parnames_4, parnames_5 
        character(20) :: parnames_6, parnames_7, parnames_8, parnames_9, parnames_10

        character(20) :: parnames_11, parnames_12, parnames_13, parnames_14, parnames_15 
        character(20) :: parnames_16, parnames_17, parnames_18, parnames_19, parnames_20

        character(20) :: parnames_21, parnames_22, parnames_23, parnames_24, parnames_25 
        character(20) :: parnames_26, parnames_27, parnames_28, parnames_29, parnames_30

        character(20) :: parnames_31, parnames_32, parnames_33, parnames_34, parnames_35 
        character(20) :: parnames_36, parnames_37, parnames_38, parnames_39, parnames_40

        character(20) :: parnames_41, parnames_42, parnames_43, parnames_44, parnames_45 
        character(20) :: parnames_46, parnames_47, parnames_48, parnames_49, parnames_50

        character(20) :: parnames_51, parnames_52, parnames_53, parnames_54, parnames_55 
        character(20) :: parnames_56, parnames_57, parnames_58, parnames_59, parnames_60 

        namelist /nml_parval/ mc_lat, mc_Longitude, mc_wsmax, mc_wsmin,            &                                                    
                mc_LAIMAX, mc_LAIMIN, mc_rdepth, mc_Rootmax, mc_Stemmax,           &                        
                mc_SapR, mc_SapS, mc_SLA, mc_GLmax, mc_GRmax, mc_Gsmax, mc_stom_n, &            
                mc_a1, mc_Ds0, mc_Vcmx0, mc_extkU, mc_xfang, mc_alpha,             &                    
                mc_Tau_Leaf, mc_Tau_Wood, mc_Tau_Root, mc_Tau_F, &
                mc_Tau_C,  mc_Tau_Micro, mc_Tau_SlowSOM, mc_Tau_Passive, &            
                mc_gddonset, mc_Q10, mc_Rl0, mc_Rs0, mc_Rr0, &        
                mc_r_me, mc_Q10pro, mc_kCH4, mc_Omax, mc_CH4_thre, &
                mc_Tveg, mc_Tpro_me, mc_Toxi, &
                mc_f, mc_bubprob, mc_Vmaxfraction, &                                    
                mc_Q10rh, mc_JV, mc_Entrpy, &                                
                mc_etaL, mc_etaW, mc_etaR, &
                mc_f_F2M, mc_f_C2M, mc_f_C2S, mc_f_M2S, &
                mc_f_M2P, mc_f_S2P, mc_f_S2M, mc_f_P2M
        namelist /nml_parmin/ mc_lat, mc_Longitude, mc_wsmax, mc_wsmin, &                                                    
                mc_LAIMAX, mc_LAIMIN, mc_rdepth, mc_Rootmax, mc_Stemmax, &                        
                mc_SapR, mc_SapS, mc_SLA, mc_GLmax, mc_GRmax, mc_Gsmax, mc_stom_n, &            
                mc_a1, mc_Ds0, mc_Vcmx0, mc_extkU, mc_xfang, mc_alpha, &                    
                mc_Tau_Leaf, mc_Tau_Wood, mc_Tau_Root, mc_Tau_F, &
                mc_Tau_C,  mc_Tau_Micro, mc_Tau_SlowSOM, mc_Tau_Passive, &            
                mc_gddonset, mc_Q10, mc_Rl0, mc_Rs0, mc_Rr0, &        
                mc_r_me, mc_Q10pro, mc_kCH4, mc_Omax, mc_CH4_thre, &
                mc_Tveg, mc_Tpro_me, mc_Toxi, &
                mc_f, mc_bubprob, mc_Vmaxfraction, &                                    
                mc_Q10rh, mc_JV, mc_Entrpy, &                                
                mc_etaL, mc_etaW, mc_etaR, &
                mc_f_F2M, mc_f_C2M, mc_f_C2S, mc_f_M2S, &
                mc_f_M2P, mc_f_S2P, mc_f_S2M, mc_f_P2M
        namelist /nml_parmax/ mc_lat, mc_Longitude, mc_wsmax, mc_wsmin, &                                                    
                mc_LAIMAX, mc_LAIMIN, mc_rdepth, mc_Rootmax, mc_Stemmax, &                        
                mc_SapR, mc_SapS, mc_SLA, mc_GLmax, mc_GRmax, mc_Gsmax, mc_stom_n, &            
                mc_a1, mc_Ds0, mc_Vcmx0, mc_extkU, mc_xfang, mc_alpha, &                    
                mc_Tau_Leaf, mc_Tau_Wood, mc_Tau_Root, mc_Tau_F, &
                mc_Tau_C,  mc_Tau_Micro, mc_Tau_SlowSOM, mc_Tau_Passive, &            
                mc_gddonset, mc_Q10, mc_Rl0, mc_Rs0, mc_Rr0, &        
                mc_r_me, mc_Q10pro, mc_kCH4, mc_Omax, mc_CH4_thre, &
                mc_Tveg, mc_Tpro_me, mc_Toxi, &
                mc_f, mc_bubprob, mc_Vmaxfraction, &                                    
                mc_Q10rh, mc_JV, mc_Entrpy, &                                
                mc_etaL, mc_etaW, mc_etaR, &
                mc_f_F2M, mc_f_C2M, mc_f_C2S, mc_f_M2S, &
                mc_f_M2P, mc_f_S2P, mc_f_S2M, mc_f_P2M
        namelist /nml_obsfiles/ obsfile_gpp_d, obsfile_nee_d, obsfile_reco_d, &
                obsfile_gpp_h, obsfile_nee_h, obsfile_reco_h, obsfile_ch4_h, &
                obsfile_cleaf, obsfile_cwood, obsfile_anpp_y, obsfile_bnpp_y, & 
                obsfile_lai_h, obsfile_npp_y, obsfile_reco_y 

        namelist /nml_param_names/parnames_1, parnames_2, parnames_3, parnames_4, parnames_5, & 
                parnames_6, parnames_7, parnames_8, parnames_9, parnames_10, &
                parnames_11, parnames_12, parnames_13, parnames_14, parnames_15, & 
                parnames_16, parnames_17, parnames_18, parnames_19, parnames_20, &
                parnames_21, parnames_22, parnames_23, parnames_24, parnames_25, &
                parnames_26, parnames_27, parnames_28, parnames_29, parnames_30, &
                parnames_31, parnames_32, parnames_33, parnames_34, parnames_35, &
                parnames_36, parnames_37, parnames_38, parnames_39, parnames_40, &
                parnames_41, parnames_42, parnames_43, parnames_44, parnames_45, & 
                parnames_46, parnames_47, parnames_48, parnames_49, parnames_50, &
                parnames_51, parnames_52, parnames_53, parnames_54, parnames_55, &
                parnames_56, parnames_57, parnames_58, parnames_59, parnames_60

        namelist /nml_mcmc_settings/ nDAsimu, search_scale, ncov, nRand, &
                do_mc_out_hr, do_mc_out_day, do_mc_out_mon, do_mc_out_yr
        
        allocate(parnames(npar))

        allocate(parval(npar), parmin(npar), parmax(npar))
        open(83, file=mcmc_conf_file)
        read(83, nml=nml_mcmc_settings)
        read(83, nml=nml_obsfiles)
        read(83, nml=nml_param_names)
        read(83, nml=nml_parval)
        call giveValues2par(parval)
        read(83, nml=nml_parmin)
        call giveValues2par(parmin)
        read(83, nml=nml_parmax)
        call giveValues2par(parmax)
        
        close(83)

        parnames(1)  = parnames_1
        parnames(2)  = parnames_2
        parnames(3)  = parnames_3
        parnames(4)  = parnames_4
        parnames(5)  = parnames_5 
        parnames(6)  = parnames_6
        parnames(7)  = parnames_7
        parnames(8)  = parnames_8
        parnames(9)  = parnames_9
        parnames(10) = parnames_10
        parnames(11)  = parnames_11
        parnames(12)  = parnames_12
        parnames(13)  = parnames_13
        parnames(14)  = parnames_14
        parnames(15)  = parnames_15 
        parnames(16)  = parnames_16
        parnames(17)  = parnames_17
        parnames(18)  = parnames_18
        parnames(19)  = parnames_19
        parnames(20)  = parnames_20
        parnames(21)  = parnames_21
        parnames(22)  = parnames_22
        parnames(23)  = parnames_23
        parnames(24)  = parnames_24
        parnames(25)  = parnames_25 
        parnames(26)  = parnames_26
        parnames(27)  = parnames_27
        parnames(28)  = parnames_28
        parnames(29)  = parnames_29
        parnames(30)  = parnames_30
        parnames(31)  = parnames_31
        parnames(32)  = parnames_32
        parnames(33)  = parnames_33
        parnames(34)  = parnames_34
        parnames(35)  = parnames_35 
        parnames(36)  = parnames_36
        parnames(37)  = parnames_37
        parnames(38)  = parnames_38
        parnames(39)  = parnames_39
        parnames(40)  = parnames_40
        parnames(41)  = parnames_41
        parnames(42)  = parnames_42
        parnames(43)  = parnames_43
        parnames(44)  = parnames_44
        parnames(45)  = parnames_45 
        parnames(46)  = parnames_46
        parnames(47)  = parnames_47
        parnames(48)  = parnames_48
        parnames(49)  = parnames_49
        parnames(50)  = parnames_50        
        parnames(51)  = parnames_51
        parnames(52)  = parnames_52
        parnames(53)  = parnames_53
        parnames(54)  = parnames_54
        parnames(55)  = parnames_55 
        parnames(56)  = parnames_56
        parnames(57)  = parnames_57
        parnames(58)  = parnames_58
        parnames(59)  = parnames_59
        parnames(60)  = parnames_60


        ! give the filepath to each variable
        vars4MCMC%gpp_d%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_gpp_d))
        vars4MCMC%nee_d%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_nee_d))
        vars4MCMC%reco_d%filepath = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_reco_d))
        vars4MCMC%gpp_h%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_gpp_h))
        vars4MCMC%nee_h%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_nee_h))
        vars4MCMC%reco_h%filepath = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_reco_h))
        ! methane   
        vars4MCMC%ch4_h%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_ch4_h))
        ! c pools
        vars4MCMC%cleaf%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_cleaf)) ! foliage
        vars4MCMC%cwood%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_cwood))

        vars4MCMC%anpp_y%filepath = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_anpp_y))
        vars4MCMC%bnpp_y%filepath = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_bnpp_y))
        vars4MCMC%lai_h%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_lai_h))
        vars4MCMC%npp_y%filepath  = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_npp_y))
        vars4MCMC%reco_y%filepath = adjustl(trim(filepath_in))//"/"//adjustl(trim(obsfile_reco_y))
        
    end subroutine readConfsNml

    subroutine readObsData()
        implicit none
        logical toExistOrNot
        integer toCountLines
        ! existornot, data 

        ! gpp_d
        INQUIRE(FILE=vars4MCMC%gpp_d%filepath, EXIST=toExistOrNot)
        vars4MCMC%gpp_d%existOrNot = toExistOrNot
        if (vars4MCMC%gpp_d%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%gpp_d%filepath, toCountLines)
            allocate(vars4MCMC%gpp_d%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%gpp_d%filepath, toCountLines, vars4MCMC%gpp_d%obsData)
            allocate(vars4MCMC%gpp_d%mdData(toCountLines, 4))
        endif
        ! nee_d
        INQUIRE(FILE=vars4MCMC%nee_d%filepath, EXIST=toExistOrNot)
        vars4MCMC%nee_d%existOrNot = toExistOrNot
        if (vars4MCMC%nee_d%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%nee_d%filepath, toCountLines)
            allocate(vars4MCMC%nee_d%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%nee_d%filepath, toCountLines, vars4MCMC%nee_d%obsData)
            allocate(vars4MCMC%nee_d%mdData(toCountLines, 4))
        endif
        ! reco_d
        INQUIRE(FILE=vars4MCMC%reco_d%filepath, EXIST=toExistOrNot)
        vars4MCMC%reco_d%existOrNot = toExistOrNot
        if (vars4MCMC%reco_d%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%reco_d%filepath, toCountLines)
            allocate(vars4MCMC%reco_d%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%reco_d%filepath, toCountLines, vars4MCMC%reco_d%obsData)
            allocate(vars4MCMC%reco_d%mdData(toCountLines, 4))
        endif
        ! gpp_h
        INQUIRE(FILE=vars4MCMC%gpp_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%gpp_h%existOrNot = toExistOrNot
        if (vars4MCMC%gpp_h%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%gpp_h%filepath, toCountLines)
            allocate(vars4MCMC%gpp_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%gpp_h%filepath, toCountLines, vars4MCMC%gpp_h%obsData)
            allocate(vars4MCMC%gpp_h%mdData(toCountLines, 4))
        endif
        ! nee_h
        INQUIRE(FILE=vars4MCMC%nee_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%nee_h%existOrNot = toExistOrNot
        if (vars4MCMC%nee_h%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%nee_h%filepath, toCountLines)
            allocate(vars4MCMC%nee_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%nee_h%filepath, toCountLines, vars4MCMC%nee_h%obsData)
            allocate(vars4MCMC%nee_h%mdData(toCountLines, 4))
        endif
        ! reco_h
        INQUIRE(FILE=vars4MCMC%reco_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%reco_h%existOrNot = toExistOrNot
        if (vars4MCMC%reco_h%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%reco_h%filepath, toCountLines)
            allocate(vars4MCMC%reco_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%reco_h%filepath, toCountLines, vars4MCMC%reco_h%obsData)
            allocate(vars4MCMC%reco_h%mdData(toCountLines, 4))
        endif
        ! ch4_h
        INQUIRE(FILE=vars4MCMC%ch4_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%ch4_h%existOrNot = toExistOrNot
        if (vars4MCMC%ch4_h%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%ch4_h%filepath, toCountLines)
            allocate(vars4MCMC%ch4_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%ch4_h%filepath, toCountLines, vars4MCMC%ch4_h%obsData)
            allocate(vars4MCMC%ch4_h%mdData(toCountLines, 4))
        endif
        ! cleaf
        INQUIRE(FILE=vars4MCMC%cleaf%filepath, EXIST=toExistOrNot)
        vars4MCMC%cleaf%existOrNot = toExistOrNot
        if (vars4MCMC%cleaf%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%cleaf%filepath, toCountLines)
            allocate(vars4MCMC%cleaf%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%cleaf%filepath, toCountLines, vars4MCMC%cleaf%obsData)
            allocate(vars4MCMC%cleaf%mdData(toCountLines, 4))
        endif
        ! cwood
        INQUIRE(FILE=vars4MCMC%cwood%filepath, EXIST=toExistOrNot)
        vars4MCMC%cwood%existOrNot = toExistOrNot
        if (vars4MCMC%cwood%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%cwood%filepath, toCountLines)
            allocate(vars4MCMC%cwood%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%cwood%filepath, toCountLines, vars4MCMC%cwood%obsData)
            allocate(vars4MCMC%cwood%mdData(toCountLines, 4))
        endif

        ! anpp_y
        INQUIRE(FILE=vars4MCMC%anpp_y%filepath, EXIST=toExistOrNot)
        vars4MCMC%anpp_y%existOrNot = toExistOrNot
        if (vars4MCMC%anpp_y%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%anpp_y%filepath, toCountLines)
            allocate(vars4MCMC%anpp_y%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%anpp_y%filepath, toCountLines, vars4MCMC%anpp_y%obsData)
            allocate(vars4MCMC%anpp_y%mdData(toCountLines, 4))
        endif

        ! bnpp_y
        INQUIRE(FILE=vars4MCMC%bnpp_y%filepath, EXIST=toExistOrNot)
        vars4MCMC%bnpp_y%existOrNot = toExistOrNot
        if (vars4MCMC%bnpp_y%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%bnpp_y%filepath, toCountLines)
            allocate(vars4MCMC%bnpp_y%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%bnpp_y%filepath, toCountLines, vars4MCMC%bnpp_y%obsData)
            allocate(vars4MCMC%bnpp_y%mdData(toCountLines, 4))
        endif

        ! lai_h
        INQUIRE(FILE=vars4MCMC%lai_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%lai_h%existOrNot = toExistOrNot
        if (vars4MCMC%lai_h%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%lai_h%filepath, toCountLines)
            allocate(vars4MCMC%lai_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%lai_h%filepath, toCountLines, vars4MCMC%lai_h%obsData)
            allocate(vars4MCMC%lai_h%mdData(toCountLines, 4))
        endif

        ! npp_y
        INQUIRE(FILE=vars4MCMC%npp_y%filepath, EXIST=toExistOrNot)
        vars4MCMC%npp_y%existOrNot = toExistOrNot
        if (vars4MCMC%npp_y%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%npp_y%filepath, toCountLines)
            allocate(vars4MCMC%npp_y%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%npp_y%filepath, toCountLines, vars4MCMC%npp_y%obsData)
            allocate(vars4MCMC%npp_y%mdData(toCountLines, 4))
        endif

        ! reco_y
        INQUIRE(FILE=vars4MCMC%reco_y%filepath, EXIST=toExistOrNot)
        vars4MCMC%reco_y%existOrNot = toExistOrNot
        if (vars4MCMC%reco_y%existOrNot) then
            call ReadLineNumFromFile(vars4MCMC%reco_y%filepath, toCountLines)
            allocate(vars4MCMC%reco_y%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%reco_y%filepath, toCountLines, vars4MCMC%reco_y%obsData)
            allocate(vars4MCMC%reco_y%mdData(toCountLines, 4))
        endif

    end subroutine readObsData

    subroutine renewMDpars()
        implicit none
        lat         = parval(1)
        lon         = parval(2)
        wsmax       = parval(3)
        wsmin       = parval(4)                                            
        LAIMAX      = parval(5)
        LAIMIN      = parval(6)
        rdepth      = parval(7)
        Rootmax     = parval(8)
        Stemmax     = parval(9)                                    
        SapR        = parval(10)
        SapS        = parval(11)
        SLAx        = parval(12)
        GLmax       = parval(13)
        GRmax       = parval(14)
        Gsmax       = parval(15)
        stom_n      = parval(16)         
        a1          = parval(17)
        Ds0         = parval(18)
        Vcmax0      = parval(19)
        extkU       = parval(20)
        xfang       = parval(21)
        alpha       = parval(22)    
        Tau_Leaf    = parval(23)
        Tau_Wood    = parval(24)
        Tau_Root    = parval(25)
        Tau_F       = parval(26)
        Tau_C       = parval(27)
        Tau_Micro   = parval(28)
        Tau_SlowSOM = parval(29)
        Tau_Passive = parval(30)    
        gddonset    = parval(31)
        Q10         = parval(32)
        Rl0         = parval(33)     
        Rs0         = parval(34)    
        Rr0         = parval(35)                    
        r_me        = parval(36)
        Q10pro      = parval(37)
        kCH4        = parval(38)
        Omax         = parval(39)
        CH4_thre     = parval(40)
        Tveg         = parval(41)
        Tpro_me      = parval(42)
        Toxi         = parval(43)        
        f            = parval(44)
        bubprob      = parval(45)
        Vmaxfraction = parval(46)                                    
        Q10rh        = parval(47)
        JV           = parval(48)
        Entrpy       = parval(49)                    
        etaL         = parval(50)
        etaW         = parval(51)
        etaR         = parval(52)
        f_F2M        = parval(53)
        f_C2M        = parval(54)
        f_C2S        = parval(55)
        f_M2S        = parval(56)
        f_M2P        = parval(57)
        f_S2P        = parval(58)
        f_S2M        = parval(59)
        f_P2M        = parval(60)
    end subroutine renewMDpars


    ! subroutine giveValues2var(filepath, existOrNot, data)
    !     implicit none
    !     character(500) filepath
    !     logical existOrNot
    !     real, allocatable :: data(:, :)
    !     integer count_lines

    !     INQUIRE(FILE=filepath, EXIST=existOrNot)
    !     if(existOrNot)then
    !         call ReadLineNumFromFile(filepath, count_lines)
    !         allocate(data(count_lines, 5))
    !         call ReadObsDataFromFile(filepath, count_lines, data)
    !     end if
    !     return
    ! end subroutine giveValues2var

    subroutine giveValues2par(arr_par)
        implicit none
        real, intent(inout) :: arr_par(:)

        arr_par(1)  = mc_lat
        arr_par(2)  = mc_Longitude 
        arr_par(3)  = mc_wsmax 
        arr_par(4)  = mc_wsmin                                                      
        arr_par(5)  = mc_LAIMAX
        arr_par(6)  = mc_LAIMIN    
        arr_par(7)  = mc_rdepth    
        arr_par(8)  = mc_Rootmax    
        arr_par(9)  = mc_Stemmax                                            
        arr_par(10) = mc_SapR    
        arr_par(11) = mc_SapS     
        arr_par(12) = mc_SLA        
        arr_par(13) = mc_GLmax    
        arr_par(14) = mc_GRmax    
        arr_par(15) = mc_Gsmax    
        arr_par(16) = mc_stom_n                                            
        arr_par(17) = mc_a1       
        arr_par(18) = mc_Ds0        
        arr_par(19) = mc_Vcmx0    
        arr_par(20) = mc_extkU    
        arr_par(21) = mc_xfang    
        arr_par(22) = mc_alpha                         
        arr_par(23) = mc_Tau_Leaf   
        arr_par(24) = mc_Tau_Wood   
        arr_par(25) = mc_Tau_Root   
        arr_par(26) = mc_Tau_F       
        arr_par(27) = mc_Tau_C       
        arr_par(28) = mc_Tau_Micro   
        arr_par(29) = mc_Tau_SlowSOM 
        arr_par(30) = mc_Tau_Passive                             
        arr_par(31) = mc_gddonset    
        arr_par(32) = mc_Q10         
        arr_par(33) = mc_Rl0        
        arr_par(34) = mc_Rs0        
        arr_par(35) = mc_Rr0                            
        arr_par(36) = mc_r_me   
        arr_par(37) = mc_Q10pro   
        arr_par(38) = mc_kCH4    
        arr_par(39) = mc_Omax   
        arr_par(40) = mc_CH4_thre 
        arr_par(41) = mc_Tveg  
        arr_par(42) = mc_Tpro_me 
        arr_par(43) = mc_Toxi               
        arr_par(44) = mc_f    
        arr_par(45) = mc_bubprob  
        arr_par(46) = mc_Vmaxfraction                                        
        arr_par(47) = mc_Q10rh  
        arr_par(48) = mc_JV   
        arr_par(49) = mc_Entrpy                                
        arr_par(50) = mc_etaL   
        arr_par(51) = mc_etaW  
        arr_par(52) = mc_etaR   
        arr_par(53) = mc_f_F2M   
        arr_par(54) = mc_f_C2M  
        arr_par(55) = mc_f_C2S 
        arr_par(56) = mc_f_M2S  
        arr_par(57) = mc_f_M2P 
        arr_par(58) = mc_f_S2P  
        arr_par(59) = mc_f_S2M  
        arr_par(60) = mc_f_P2M 

    end subroutine giveValues2par

    subroutine GetSimuData(get_iyear, get_iday, get_ihour)
        implicit none
        integer get_iyear, get_iday, get_ihour
        integer i
        ! vars4MCMC%
        mc_iyear = get_iyear
        mc_iday  = get_iday
        mc_ihour = get_ihour + 1

        ! do i = 1, 20
        !     write(*,*)vars4MCMC%gpp_d%obsData(i, :)
        !     write(*,*)int(vars4MCMC%gpp_d%obsData(i, 1))
        !     write(*,*)int(vars4MCMC%gpp_d%obsData(i, 2))
        !     write(*,*)int(vars4MCMC%gpp_d%obsData(i, 3))
        ! enddo
        ! stop

        ! gpp_d
        if(vars4MCMC%gpp_d%existOrNot)then
            ! write(*,*) "test here: ", vars4MCMC%gpp_d%obsData
            if(mc_itime_gpp_d<=size(vars4MCMC%gpp_d%obsData, dim=1))then
                do while(vars4MCMC%gpp_d%obsData(mc_itime_gpp_d, 1) .lt. forcing(1)%year)
                    vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 4) = -9999
                    mc_itime_gpp_d = mc_itime_gpp_d + 1
                enddo

                if(vars4MCMC%gpp_d%obsData(mc_itime_gpp_d, 1) .eq. mc_iyear .and. &
                vars4MCMC%gpp_d%obsData(mc_itime_gpp_d, 2) .eq. mc_iday  .and. &
                vars4MCMC%gpp_d%obsData(mc_itime_gpp_d, 3) .eq. mc_ihour) then
                    vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 1) = mc_iyear
                    vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 2) = mc_iday
                    vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 3) = mc_ihour
                    vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 4) = outVars_d%gpp*86400000
                    mc_itime_gpp_d = mc_itime_gpp_d + 1
                endif
            endif
        endif
        ! nee_d
        if(vars4MCMC%nee_d%existOrNot)then
            if(mc_itime_nee_d <= size(vars4MCMC%nee_d%obsData,dim=1))then
                do while(vars4MCMC%nee_d%obsData(mc_itime_nee_d, 1) .lt. forcing(1)%year)
                    vars4MCMC%nee_d%mdData(mc_itime_nee_d, 4) = -9999
                    mc_itime_nee_d = mc_itime_nee_d + 1
                enddo
                if(vars4MCMC%nee_d%obsData(mc_itime_nee_d, 1) .eq. mc_iyear .and. &
                vars4MCMC%nee_d%obsData(mc_itime_nee_d, 2) .eq. mc_iday  .and. &
                vars4MCMC%nee_d%obsData(mc_itime_nee_d, 3) .eq. mc_ihour) then
                    vars4MCMC%nee_d%mdData(mc_itime_nee_d, 1) = mc_iyear
                    vars4MCMC%nee_d%mdData(mc_itime_nee_d, 2) = mc_iday
                    vars4MCMC%nee_d%mdData(mc_itime_nee_d, 3) = mc_ihour
                    vars4MCMC%nee_d%mdData(mc_itime_nee_d, 4) = outVars_d%nbp*86400000    ! the same in TECO model
                    mc_itime_nee_d = mc_itime_nee_d + 1
                endif
            endif
        endif
        ! reco_d
        if(vars4MCMC%reco_d%existOrNot)then
            if(mc_itime_reco_d <= size(vars4MCMC%reco_d%obsData, dim=1))then
                do while(vars4MCMC%reco_d%obsData(mc_itime_reco_d, 1) .lt. forcing(1)%year)
                    vars4MCMC%reco_d%mdData(mc_itime_reco_d, 4) = -9999
                    mc_itime_reco_d = mc_itime_reco_d + 1
                enddo

                if(vars4MCMC%reco_d%obsData(mc_itime_reco_d, 1) .eq. mc_iyear .and. &
                vars4MCMC%reco_d%obsData(mc_itime_reco_d, 2) .eq. mc_iday  .and. &
                vars4MCMC%reco_d%obsData(mc_itime_reco_d, 3) .eq. mc_ihour) then
                    vars4MCMC%reco_d%mdData(mc_itime_reco_d, 1) = mc_iyear
                    vars4MCMC%reco_d%mdData(mc_itime_reco_d, 2) = mc_iday
                    vars4MCMC%reco_d%mdData(mc_itime_reco_d, 3) = mc_ihour
                    vars4MCMC%reco_d%mdData(mc_itime_reco_d, 4) = (outVars_d%rh + outVars_d%ra)*86400000
                    mc_itime_reco_d = mc_itime_reco_d + 1
                endif
            endif
        endif
        ! gpp_h
        if(vars4MCMC%gpp_h%existOrNot)then
            if(mc_itime_gpp_h <= size(vars4MCMC%gpp_h%obsData, dim=1) )then
                do while(vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 1) .lt. forcing(1)%year)
                    vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 4) = -9999
                    mc_itime_gpp_h = mc_itime_gpp_h + 1
                enddo

                if(vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 1) .eq. mc_iyear .and. &
                vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 2) .eq. mc_iday  .and. &
                vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 3) .eq. mc_ihour) then
                    ! write(*,*) "test gpp hourly", mc_itime_gpp_h
                    ! write(*,*) vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 1), mc_iyear
                    ! write(*,*) vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 2), mc_iday
                    ! write(*,*) vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 3), mc_ihour
                    ! write(*,*) outVars_h%gpp, vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 4)
                    
                    vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 1) = mc_iyear
                    vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 2) = mc_iday
                    vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 3) = mc_ihour
                    vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 4) = outVars_h%gpp*3600000
                    mc_itime_gpp_h = mc_itime_gpp_h + 1
                endif
            endif
        endif
        ! nee_h
        if(vars4MCMC%nee_h%existOrNot)then
            if(mc_itime_nee_h <= size(vars4MCMC%nee_h%obsData, dim=1)) then
                do while(vars4MCMC%nee_h%obsData(mc_itime_nee_h, 1) .lt. forcing(1)%year)
                    vars4MCMC%nee_h%mdData(mc_itime_nee_h, 4) = -9999
                    mc_itime_nee_h = mc_itime_nee_h + 1
                enddo
                if(vars4MCMC%nee_h%obsData(mc_itime_nee_h, 1) .eq. mc_iyear .and. &
                vars4MCMC%nee_h%obsData(mc_itime_nee_h, 2) .eq. mc_iday  .and. &
                vars4MCMC%nee_h%obsData(mc_itime_nee_h, 3) .eq. mc_ihour) then
                    vars4MCMC%nee_h%mdData(mc_itime_nee_h, 1) = mc_iyear
                    vars4MCMC%nee_h%mdData(mc_itime_nee_h, 2) = mc_iday
                    vars4MCMC%nee_h%mdData(mc_itime_nee_h, 3) = mc_ihour
                    vars4MCMC%nee_h%mdData(mc_itime_nee_h, 4) = outVars_h%nbp*3600000
                    mc_itime_nee_h = mc_itime_nee_h + 1
                endif
            endif
        endif
        ! reco_h
        if(vars4MCMC%reco_h%existOrNot)then
            if(mc_itime_reco_h <= size(vars4MCMC%reco_h%obsData, dim=1))then
                do while(vars4MCMC%reco_h%obsData(mc_itime_reco_h, 1) .lt. forcing(1)%year)
                    vars4MCMC%reco_h%mdData(mc_itime_reco_h, 4) = -9999
                    mc_itime_reco_h = mc_itime_reco_h + 1
                enddo

                if(vars4MCMC%reco_h%obsData(mc_itime_reco_h, 1) .eq. mc_iyear .and. &
                vars4MCMC%reco_h%obsData(mc_itime_reco_h, 2) .eq. mc_iday  .and. &
                vars4MCMC%reco_h%obsData(mc_itime_reco_h, 3) .eq. mc_ihour) then
                    vars4MCMC%reco_h%mdData(mc_itime_reco_h, 1) = mc_iyear
                    vars4MCMC%reco_h%mdData(mc_itime_reco_h, 2) = mc_iday
                    vars4MCMC%reco_h%mdData(mc_itime_reco_h, 3) = mc_ihour
                    vars4MCMC%reco_h%mdData(mc_itime_reco_h, 4) = (outVars_h%rh + outVars_h%ra)*3600000
                    mc_itime_reco_h = mc_itime_reco_h + 1
                endif
            endif
        endif
        ! ch4_h
        if(vars4MCMC%ch4_h%existOrNot)then
            if(mc_itime_ch4_h <= size(vars4MCMC%ch4_h%obsData, dim=1))then
                do while(vars4MCMC%ch4_h%obsData(mc_itime_ch4_h, 1) .lt. forcing(1)%year)
                    vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 4) = -9999
                    mc_itime_ch4_h = mc_itime_ch4_h + 1
                enddo

                if(vars4MCMC%ch4_h%obsData(mc_itime_ch4_h, 1) .eq. mc_iyear .and. &
                vars4MCMC%ch4_h%obsData(mc_itime_ch4_h, 2) .eq. mc_iday  .and. &
                vars4MCMC%ch4_h%obsData(mc_itime_ch4_h, 3) .eq. mc_ihour) then
                    vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 1) = mc_iyear
                    vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 2) = mc_iday
                    vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 3) = mc_ihour
                    vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 4) = sum(CH4)*3600000
                    mc_itime_ch4_h = mc_itime_ch4_h + 1
                endif
            endif
        endif
        ! cleaf
        if(vars4MCMC%cleaf%existOrNot)then
            if(mc_itime_cleaf <= size(vars4MCMC%cleaf%obsData, dim=1))then
                do while(vars4MCMC%cleaf%obsData(mc_itime_cleaf, 1) .lt. forcing(1)%year)
                    vars4MCMC%cleaf%mdData(mc_itime_cleaf, 4) = -9999
                    mc_itime_cleaf = mc_itime_cleaf + 1
                enddo

                if(vars4MCMC%cleaf%obsData(mc_itime_cleaf, 1) .eq. mc_iyear .and. &
                vars4MCMC%cleaf%obsData(mc_itime_cleaf, 2) .eq. mc_iday  .and. &
                vars4MCMC%cleaf%obsData(mc_itime_cleaf, 3) .eq. mc_ihour) then
                    vars4MCMC%cleaf%mdData(mc_itime_cleaf, 1) = mc_iyear
                    vars4MCMC%cleaf%mdData(mc_itime_cleaf, 2) = mc_iday
                    vars4MCMC%cleaf%mdData(mc_itime_cleaf, 3) = mc_ihour
                    vars4MCMC%cleaf%mdData(mc_itime_cleaf, 4) = QC(1) 
                    mc_itime_cleaf = mc_itime_cleaf + 1
                endif
            endif
        endif
        ! cwood
        if(vars4MCMC%cwood%existOrNot)then
            if(mc_itime_cwood <= size(vars4MCMC%cwood%obsData, dim=1)) then
                do while(vars4MCMC%cwood%obsData(mc_itime_cwood, 1) .lt. forcing(1)%year)
                    vars4MCMC%cwood%mdData(mc_itime_cwood, 4) = -9999
                    mc_itime_cwood = mc_itime_cwood + 1
                enddo
                if(vars4MCMC%cwood%obsData(mc_itime_cwood, 1) .eq. mc_iyear .and. &
                vars4MCMC%cwood%obsData(mc_itime_cwood, 2) .eq. mc_iday  .and. &
                vars4MCMC%cwood%obsData(mc_itime_cwood, 3) .eq. mc_ihour) then
                    vars4MCMC%cwood%mdData(mc_itime_cwood, 1) = mc_iyear
                    vars4MCMC%cwood%mdData(mc_itime_cwood, 2) = mc_iday
                    vars4MCMC%cwood%mdData(mc_itime_cwood, 3) = mc_ihour
                    vars4MCMC%cwood%mdData(mc_itime_cwood, 4) = QC(2)
                    mc_itime_cwood = mc_itime_cwood + 1
                endif
            endif
        endif

        ! anpp_y
        if(vars4MCMC%anpp_y%existOrNot)then
            if(mc_itime_anpp_y <= size(vars4MCMC%anpp_y%obsData, dim=1)) then
                do while(vars4MCMC%anpp_y%obsData(mc_itime_anpp_y, 1) .lt. forcing(1)%year)
                    vars4MCMC%anpp_y%mdData(mc_itime_anpp_y, 4) = -9999
                    mc_itime_anpp_y = mc_itime_anpp_y + 1
                enddo
                if (vars4MCMC%anpp_y%obsData(mc_itime_anpp_y, 2) .lt. 0) then 
                    vars4MCMC%anpp_y%obsData(mc_itime_anpp_y, 2) = 365
                endif
                if(vars4MCMC%anpp_y%obsData(mc_itime_anpp_y, 1) .eq. mc_iyear .and. &
                vars4MCMC%anpp_y%obsData(mc_itime_anpp_y, 2) .eq. mc_iday  .and. &
                vars4MCMC%anpp_y%obsData(mc_itime_anpp_y, 3) .eq. mc_ihour) then
                    vars4MCMC%anpp_y%mdData(mc_itime_anpp_y, 1) = mc_iyear
                    vars4MCMC%anpp_y%mdData(mc_itime_anpp_y, 2) = mc_iday
                    vars4MCMC%anpp_y%mdData(mc_itime_anpp_y, 3) = mc_ihour
                    vars4MCMC%anpp_y%mdData(mc_itime_anpp_y, 4) = (outVars_y%nppLeaf + outVars_y%nppStem)*3600000*365*24
                    mc_itime_anpp_y = mc_itime_anpp_y + 1
                endif
            endif
        endif

        ! bnpp_y
        if(vars4MCMC%bnpp_y%existOrNot)then
            if(mc_itime_bnpp_y <= size(vars4MCMC%bnpp_y%obsData, dim=1)) then
                do while(vars4MCMC%bnpp_y%obsData(mc_itime_bnpp_y, 1) .lt. forcing(1)%year)
                    vars4MCMC%bnpp_y%mdData(mc_itime_bnpp_y, 4) = -9999
                    mc_itime_bnpp_y = mc_itime_bnpp_y + 1
                enddo
                if (vars4MCMC%bnpp_y%obsData(mc_itime_bnpp_y, 2) .lt. 0) then 
                    vars4MCMC%bnpp_y%obsData(mc_itime_bnpp_y, 2) = 365
                endif
                if(vars4MCMC%bnpp_y%obsData(mc_itime_bnpp_y, 1) .eq. mc_iyear .and. &
                vars4MCMC%bnpp_y%obsData(mc_itime_bnpp_y, 2) .eq. mc_iday  .and. &
                vars4MCMC%bnpp_y%obsData(mc_itime_bnpp_y, 3) .eq. mc_ihour) then
                    vars4MCMC%bnpp_y%mdData(mc_itime_bnpp_y, 1) = mc_iyear
                    vars4MCMC%bnpp_y%mdData(mc_itime_bnpp_y, 2) = mc_iday
                    vars4MCMC%bnpp_y%mdData(mc_itime_bnpp_y, 3) = mc_ihour
                    vars4MCMC%bnpp_y%mdData(mc_itime_bnpp_y, 4) = outVars_y%nppRoot*3600000*365*24
                    mc_itime_bnpp_y = mc_itime_bnpp_y + 1
                endif
            endif
        endif

        ! lai_h
        if(vars4MCMC%lai_h%existOrNot)then
            if(mc_itime_lai_h <= size(vars4MCMC%lai_h%obsData, dim=1)) then
                do while(vars4MCMC%lai_h%obsData(mc_itime_lai_h, 1) .lt. forcing(1)%year)
                    vars4MCMC%lai_h%mdData(mc_itime_lai_h, 4) = -9999
                    mc_itime_lai_h = mc_itime_lai_h + 1
                enddo
                if(vars4MCMC%lai_h%obsData(mc_itime_lai_h, 1) .eq. mc_iyear .and. &
                vars4MCMC%lai_h%obsData(mc_itime_lai_h, 2) .eq. mc_iday  .and. &
                vars4MCMC%lai_h%obsData(mc_itime_lai_h, 3) .eq. mc_ihour) then
                    vars4MCMC%lai_h%mdData(mc_itime_lai_h, 1) = mc_iyear
                    vars4MCMC%lai_h%mdData(mc_itime_lai_h, 2) = mc_iday
                    vars4MCMC%lai_h%mdData(mc_itime_lai_h, 3) = mc_ihour
                    vars4MCMC%lai_h%mdData(mc_itime_lai_h, 4) = outVars_h%lai
                    mc_itime_lai_h = mc_itime_lai_h + 1
                endif
            endif
        endif

        ! npp_y
        if(vars4MCMC%npp_y%existOrNot)then
            if(mc_itime_npp_y <= size(vars4MCMC%npp_y%obsData, dim=1)) then
                do while(vars4MCMC%npp_y%obsData(mc_itime_npp_y, 1) .lt. forcing(1)%year)
                    vars4MCMC%npp_y%mdData(mc_itime_npp_y, 4) = -9999
                    mc_itime_npp_y = mc_itime_npp_y + 1
                enddo
                if (vars4MCMC%npp_y%obsData(mc_itime_npp_y, 2) .lt. 0) then 
                    vars4MCMC%npp_y%obsData(mc_itime_npp_y, 2) = 365
                endif
                if(vars4MCMC%npp_y%obsData(mc_itime_npp_y, 1) .eq. mc_iyear .and. &
                vars4MCMC%npp_y%obsData(mc_itime_npp_y, 2) .eq. mc_iday  .and. &
                vars4MCMC%npp_y%obsData(mc_itime_npp_y, 3) .eq. mc_ihour) then
                    vars4MCMC%npp_y%mdData(mc_itime_npp_y, 1) = mc_iyear
                    vars4MCMC%npp_y%mdData(mc_itime_npp_y, 2) = mc_iday
                    vars4MCMC%npp_y%mdData(mc_itime_npp_y, 3) = mc_ihour
                    vars4MCMC%npp_y%mdData(mc_itime_npp_y, 4) = outVars_y%npp*3600000*365*24
                    mc_itime_npp_y = mc_itime_npp_y + 1
                endif
            endif
        endif

        ! reco_y
        if(vars4MCMC%reco_y%existOrNot)then
            if(mc_itime_reco_y <= size(vars4MCMC%reco_y%obsData, dim=1)) then
                do while(vars4MCMC%reco_y%obsData(mc_itime_reco_y, 1) .lt. forcing(1)%year)
                    vars4MCMC%reco_y%mdData(mc_itime_reco_y, 4) = -9999
                    mc_itime_reco_y = mc_itime_reco_y + 1
                enddo
                if(vars4MCMC%reco_y%obsData(mc_itime_reco_y, 1) .eq. mc_iyear .and. &
                vars4MCMC%reco_y%obsData(mc_itime_reco_y, 2) .eq. mc_iday  .and. &
                vars4MCMC%reco_y%obsData(mc_itime_reco_y, 3) .eq. mc_ihour) then
                    vars4MCMC%reco_y%mdData(mc_itime_reco_y, 1) = mc_iyear
                    vars4MCMC%reco_y%mdData(mc_itime_reco_y, 2) = mc_iday
                    vars4MCMC%reco_y%mdData(mc_itime_reco_y, 3) = mc_ihour
                    vars4MCMC%reco_y%mdData(mc_itime_reco_y, 4) = QC(2)
                    mc_itime_reco_y = mc_itime_reco_y + 1
                endif
            endif
        endif
           
    end subroutine GetSimuData

    

    ! subroutine ReadLineNumFromFile(filepath, count_lines)
    !     implicit none
    !     character(len=*), intent(in) :: filepath
    !     character(len=100) header, line
    !     integer STAT, count_lines

    !     open(38, file=trim(filepath), status="old", action="read", iostat=STAT) ! open file
    !     read(38, '(a100)') header           ! read the header of the file
    !     count_lines = 0                     ! initilize the count_lines
    !     do while(.TRUE.)
    !         read(38, *, iostat=STAT) line   ! read each line
    !         if(STAT .ne. 0) exit            ! until the end of the file
    !         count_lines = count_lines + 1   ! recording the count of the lines
    !     enddo
    !     return
    ! end subroutine ReadLineNumFromFile

    subroutine ReadObsDataFromFile(filepath, count_lines, resData)
        ! Jian: note that this subroutine is used to read the observational data. 
        ! The observational file must be .txt format, and with 5 columns: year, doy, hour, value, std.
        implicit none
        character(len=*), intent(in) :: filepath
        character(len=100) header
        integer STAT, count_lines, iline, n
        real resData(count_lines, 5), readData(5) ! 5 colunms: year, doy, hour, value, std.

        OPEN(34, FILE=trim(filepath), status='old', ACTION='read', IOSTAT=STAT) ! open file
        read(34, '(a100)') header
        iline = 1
        do
            read(34,*,iostat=STAT, end=567) (readData(n), n = 1, 5)
            if(STAT .ne. 0) exit
            resData(iline, :) = readData
            iline = iline + 1
        end do
567     continue
        close(34)
        return
    end subroutine ReadObsDataFromFile

end module mcmc_functions