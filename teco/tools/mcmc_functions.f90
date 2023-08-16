module mcmc_functions
    use mod_data
    implicit none

    ! parameters and observation files
    integer npar
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
    
    ! type parameters4MCMC
    !     real lat, Longitude, wsmax, wsmin														
    !     real LAIMAX, LAIMIN, rdepth, Rootmax, Stemmax						
    !     real SapR, SapS, SLA, GLmax, GRmax, Gsmax, stom_n				
    !     real a1, Ds0, Vcmx0, extkU, xfang, alpha					
    !     real Tau_Leaf, Tau_Wood, Tau_Root, Tau_F
    !     real Tau_C,  Tau_Micro, Tau_SlowSOM, Tau_Passive			
    !     real gddonset, Q10, Rl0, Rs0, Rr0						
    !     real r_me, Q10pro, kCH4, Omax, CH4_thre
    !     real Tveg, Tpro_me, Toxi			
    !     real f, bubprob, Vmaxfraction    								
    !     real Q10rh, JV, Entrpy								
    !     real etaL, etaW, etaR
    !     real f_F2M, f_C2M, f_C2S, f_M2S
    !     real f_M2P, f_S2P, f_S2M, f_P2M
    ! end type parameters4MCMC

    ! type(parameters4MCMC) parmin, parmax, parval

    ! observational file path
    character(500) :: obsfile_gpp_d, obsfile_nee_d, obsfile_reco_d
    character(500) :: obsfile_gpp_h, obsfile_nee_h, obsfile_reco_h
    ! methane   
    character(500) :: obsfile_ch4_h
    ! c pools
    character(500) :: obsfile_cleaf, obsfile_cwood 

    ! variables for calculating the cost in MCMC processes
    type interCostVariable
        character(200) :: filepath
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
    end type allCostVariables

    type(allCostVariables) :: vars4MCMC      ! define a allCostVariables first

    ! variables for marking the cycle number
    integer mc_itime_gpp_d, mc_itime_nee_d, mc_itime_reco_d
    integer mc_itime_gpp_h, mc_itime_nee_h, mc_itime_reco_h
    integer mc_itime_ch4_h, mc_itime_cleaf, mc_itime_cwood
    integer mc_iyear,  mc_iday, mc_ihour

    contains

    subroutine mcmc_functions_init()
        implicit none
        mc_itime_gpp_d  = 0
        mc_itime_nee_d  = 0 
        mc_itime_reco_d = 0
        mc_itime_gpp_h  = 0
        mc_itime_nee_h  = 0
        mc_itime_reco_h = 0
        mc_itime_ch4_h  = 0
        mc_itime_cleaf  = 0
        mc_itime_cwood  = 0
        mc_iyear = 0
        mc_iday  = 0
        mc_ihour = 0
    end subroutine mcmc_functions_init

    subroutine readConfsNml()
    ! default nml file name of "TECO_MCMC_configs.nml"
        implicit none
        namelist /nml_parval/ mc_lat, mc_Longitude, mc_wsmax, mc_wsmin, &													
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
                obsfile_cleaf, obsfile_cwood

        npar = 60 ! total 60 parameters

        allocate(parval(npar), parmin(npar), parmax(npar))
        open(83, file="TECO_MCMC_configs.nml")
        read(83, nml=nml_parval)
        call giveValues2par(parval)
        read(83, nml=nml_parmin)
        call giveValues2par(parmin)
        read(83, nml=nml_parmax)
        call giveValues2par(parmax)

        read(83, nml=nml_obsfiles)
        close(83)

        ! give the filepath to each variable
        vars4MCMC%gpp_d%filepath  = obsfile_gpp_d
        vars4MCMC%nee_d%filepath  = obsfile_nee_d 
        vars4MCMC%reco_d%filepath = obsfile_reco_d
        vars4MCMC%gpp_h%filepath  = obsfile_gpp_h
        vars4MCMC%nee_h%filepath  = obsfile_nee_h 
        vars4MCMC%reco_h%filepath = obsfile_reco_h
        ! methane   
        vars4MCMC%ch4_h%filepath  = obsfile_ch4_h
        ! c pools
        vars4MCMC%cleaf%filepath  = obsfile_cleaf ! foliage
        vars4MCMC%cwood%filepath  = obsfile_cwood
        
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
            call ReadLineNumFromObsFile(vars4MCMC%gpp_d%filepath, toCountLines)
            allocate(vars4MCMC%gpp_d%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%gpp_d%filepath, toCountLines, vars4MCMC%gpp_d%obsData)
            allocate(vars4MCMC%gpp_d%mdData(toCountLines, 4))
        endif
        ! nee_d
        INQUIRE(FILE=vars4MCMC%nee_d%filepath, EXIST=toExistOrNot)
        vars4MCMC%nee_d%existOrNot = toExistOrNot
        if (vars4MCMC%nee_d%existOrNot) then
            call ReadLineNumFromObsFile(vars4MCMC%nee_d%filepath, toCountLines)
            allocate(vars4MCMC%nee_d%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%nee_d%filepath, toCountLines, vars4MCMC%nee_d%obsData)
            allocate(vars4MCMC%nee_d%mdData(toCountLines, 4))
        endif
        ! reco_d
        INQUIRE(FILE=vars4MCMC%reco_d%filepath, EXIST=toExistOrNot)
        vars4MCMC%reco_d%existOrNot = toExistOrNot
        if (vars4MCMC%reco_d%existOrNot) then
            call ReadLineNumFromObsFile(vars4MCMC%reco_d%filepath, toCountLines)
            allocate(vars4MCMC%reco_d%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%reco_d%filepath, toCountLines, vars4MCMC%reco_d%obsData)
            allocate(vars4MCMC%reco_d%mdData(toCountLines, 4))
        endif
        ! gpp_h
        INQUIRE(FILE=vars4MCMC%gpp_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%gpp_h%existOrNot = toExistOrNot
        if (vars4MCMC%gpp_h%existOrNot) then
            call ReadLineNumFromObsFile(vars4MCMC%gpp_h%filepath, toCountLines)
            allocate(vars4MCMC%gpp_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%gpp_h%filepath, toCountLines, vars4MCMC%gpp_h%obsData)
            allocate(vars4MCMC%gpp_h%mdData(toCountLines, 4))
        endif
        ! nee_h
        INQUIRE(FILE=vars4MCMC%nee_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%nee_h%existOrNot = toExistOrNot
        if (vars4MCMC%nee_h%existOrNot) then
            call ReadLineNumFromObsFile(vars4MCMC%nee_h%filepath, toCountLines)
            allocate(vars4MCMC%nee_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%nee_h%filepath, toCountLines, vars4MCMC%nee_h%obsData)
            allocate(vars4MCMC%nee_h%mdData(toCountLines, 4))
        endif
        ! reco_h
        INQUIRE(FILE=vars4MCMC%reco_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%reco_h%existOrNot = toExistOrNot
        if (vars4MCMC%reco_h%existOrNot) then
            call ReadLineNumFromObsFile(vars4MCMC%reco_h%filepath, toCountLines)
            allocate(vars4MCMC%reco_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%reco_h%filepath, toCountLines, vars4MCMC%reco_h%obsData)
            allocate(vars4MCMC%reco_h%mdData(toCountLines, 4))
        endif
        ! ch4_h
        INQUIRE(FILE=vars4MCMC%ch4_h%filepath, EXIST=toExistOrNot)
        vars4MCMC%ch4_h%existOrNot = toExistOrNot
        if (vars4MCMC%ch4_h%existOrNot) then
            call ReadLineNumFromObsFile(vars4MCMC%ch4_h%filepath, toCountLines)
            allocate(vars4MCMC%ch4_h%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%ch4_h%filepath, toCountLines, vars4MCMC%ch4_h%obsData)
            allocate(vars4MCMC%ch4_h%mdData(toCountLines, 4))
        endif
        ! cleaf
        INQUIRE(FILE=vars4MCMC%cleaf%filepath, EXIST=toExistOrNot)
        vars4MCMC%cleaf%existOrNot = toExistOrNot
        if (vars4MCMC%cleaf%existOrNot) then
            call ReadLineNumFromObsFile(vars4MCMC%cleaf%filepath, toCountLines)
            allocate(vars4MCMC%cleaf%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%cleaf%filepath, toCountLines, vars4MCMC%cleaf%obsData)
            allocate(vars4MCMC%cleaf%mdData(toCountLines, 4))
        endif
        ! cwood
        INQUIRE(FILE=vars4MCMC%cwood%filepath, EXIST=toExistOrNot)
        vars4MCMC%cwood%existOrNot = toExistOrNot
        if (vars4MCMC%cwood%existOrNot) then
            call ReadLineNumFromObsFile(vars4MCMC%cwood%filepath, toCountLines)
            allocate(vars4MCMC%cwood%obsData(toCountLines, 5))
            call ReadObsDataFromFile(vars4MCMC%cwood%filepath, toCountLines, vars4MCMC%cwood%obsData)
            allocate(vars4MCMC%cwood%mdData(toCountLines, 4))
        endif
        
    end subroutine readObsData

    subroutine renewMDpars()
        implicit none
        lat         = parval(1)
        Longi       = parval(2)
        wsmax       = parval(3)
        wsmin  		= parval(4)											
        LAIMAX      = parval(5)
        LAIMIN	    = parval(6)
        rdepth	    = parval(7)
        Rootmax	    = parval(8)
        Stemmax		= parval(9)									
        SapR        = parval(10)
        SapS        = parval(11)
        SLA	        = parval(12)
        GLmax       = parval(13)
        GRmax	    = parval(14)
        Gsmax	    = parval(15)
        stom_n		= parval(16)									
        a1          = parval(17)
        Ds0	        = parval(18)
        Vcmx0	    = parval(19)
        extkU	    = parval(20)
        xfang	    = parval(21)
        alpha	 	= parval(22)				
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
        Rr0		    = parval(35)					
        r_me        = parval(36)
        Q10pro      = parval(37)
        kCH4        = parval(38)
        Omax        = parval(39)
        CH4_thre    = parval(40)
        Tveg        = parval(41)
        Tpro_me     = parval(42)
        Toxi   	    = parval(43)		
        f           = parval(44)
        bubprob      = parval(45)
        Vmaxfraction = parval(46)									
        Q10rh        = parval(47)
        JV           = parval(48)
        Entrpy		 = parval(49)					
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
    !         call ReadLineNumFromObsFile(filepath, count_lines)
    !         allocate(data(count_lines, 5))
    !         call ReadObsDataFromFile(filepath, count_lines, data)
    !     end if
    !     return
    ! end subroutine giveValues2var

    subroutine giveValues2par(arr_par)
        implicit none
        real, dimension(npar) :: arr_par

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

    

    subroutine GetSimuData()
        implicit none
        ! vars4MCMC%

        ! gpp_d
        if(vars4MCMC%gpp_d%existOrNot)then
            if(vars4MCMC%gpp_d%obsData(mc_itime_gpp_d, 1) .eq. mc_iyear .and. &
               vars4MCMC%gpp_d%obsData(mc_itime_gpp_d, 2) .eq. mc_iday  .and. &
               vars4MCMC%gpp_d%obsData(mc_itime_gpp_d, 3) .eq. mc_ihour) then
                mc_itime_gpp_d = mc_itime_gpp_d + 1
                vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 1) = mc_iyear
                vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 2) = mc_iday
                vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 3) = mc_ihour
                vars4MCMC%gpp_d%mdData(mc_itime_gpp_d, 4) = gpp_d
            endif
        endif
        ! nee_d
        if(vars4MCMC%nee_d%existOrNot)then
            if(vars4MCMC%nee_d%obsData(mc_itime_nee_d, 1) .eq. mc_iyear .and. &
               vars4MCMC%nee_d%obsData(mc_itime_nee_d, 2) .eq. mc_iday  .and. &
               vars4MCMC%nee_d%obsData(mc_itime_nee_d, 3) .eq. mc_ihour) then
                mc_itime_nee_d = mc_itime_nee_d + 1
                vars4MCMC%nee_d%mdData(mc_itime_nee_d, 1) = mc_iyear
                vars4MCMC%nee_d%mdData(mc_itime_nee_d, 2) = mc_iday
                vars4MCMC%nee_d%mdData(mc_itime_nee_d, 3) = mc_ihour
                vars4MCMC%nee_d%mdData(mc_itime_nee_d, 4) = nbp_d    ! the same in TECO model
            endif
        endif
        ! reco_d
        if(vars4MCMC%reco_d%existOrNot)then
            if(vars4MCMC%reco_d%obsData(mc_itime_reco_d, 1) .eq. mc_iyear .and. &
               vars4MCMC%reco_d%obsData(mc_itime_reco_d, 2) .eq. mc_iday  .and. &
               vars4MCMC%reco_d%obsData(mc_itime_reco_d, 3) .eq. mc_ihour) then
                mc_itime_reco_d = mc_itime_reco_d + 1
                vars4MCMC%reco_d%mdData(mc_itime_reco_d, 1) = mc_iyear
                vars4MCMC%reco_d%mdData(mc_itime_reco_d, 2) = mc_iday
                vars4MCMC%reco_d%mdData(mc_itime_reco_d, 3) = mc_ihour
                vars4MCMC%reco_d%mdData(mc_itime_reco_d, 4) = rh_d + ra_d
            endif
        endif
        ! gpp_h
        if(vars4MCMC%gpp_h%existOrNot)then
            if(vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 1) .eq. mc_iyear .and. &
               vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 2) .eq. mc_iday  .and. &
               vars4MCMC%gpp_h%obsData(mc_itime_gpp_h, 3) .eq. mc_ihour) then
                mc_itime_gpp_h = mc_itime_gpp_h + 1
                vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 1) = mc_iyear
                vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 2) = mc_iday
                vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 3) = mc_ihour
                vars4MCMC%gpp_h%mdData(mc_itime_gpp_h, 4) = gpp_h
            endif
        endif
        ! nee_h
        if(vars4MCMC%nee_h%existOrNot)then
            if(vars4MCMC%nee_h%obsData(mc_itime_nee_h, 1) .eq. mc_iyear .and. &
               vars4MCMC%nee_h%obsData(mc_itime_nee_h, 2) .eq. mc_iday  .and. &
               vars4MCMC%nee_h%obsData(mc_itime_nee_h, 3) .eq. mc_ihour) then
                mc_itime_nee_h = mc_itime_nee_h + 1
                vars4MCMC%nee_h%mdData(mc_itime_nee_h, 1) = mc_iyear
                vars4MCMC%nee_h%mdData(mc_itime_nee_h, 2) = mc_iday
                vars4MCMC%nee_h%mdData(mc_itime_nee_h, 3) = mc_ihour
                vars4MCMC%nee_h%mdData(mc_itime_nee_h, 4) = nbp_h
            endif
        endif
        ! reco_h
        if(vars4MCMC%reco_h%existOrNot)then
            if(vars4MCMC%reco_h%obsData(mc_itime_reco_h, 1) .eq. mc_iyear .and. &
               vars4MCMC%reco_h%obsData(mc_itime_reco_h, 2) .eq. mc_iday  .and. &
               vars4MCMC%reco_h%obsData(mc_itime_reco_h, 3) .eq. mc_ihour) then
                mc_itime_reco_h = mc_itime_reco_h + 1
                vars4MCMC%reco_h%mdData(mc_itime_reco_h, 1) = mc_iyear
                vars4MCMC%reco_h%mdData(mc_itime_reco_h, 2) = mc_iday
                vars4MCMC%reco_h%mdData(mc_itime_reco_h, 3) = mc_ihour
                vars4MCMC%reco_h%mdData(mc_itime_reco_h, 4) = rh_h + ra_h
            endif
        endif
        ! ch4_h
        if(vars4MCMC%ch4_h%existOrNot)then
            if(vars4MCMC%ch4_h%obsData(mc_itime_ch4_h, 1) .eq. mc_iyear .and. &
               vars4MCMC%ch4_h%obsData(mc_itime_ch4_h, 2) .eq. mc_iday  .and. &
               vars4MCMC%ch4_h%obsData(mc_itime_ch4_h, 3) .eq. mc_ihour) then
                mc_itime_ch4_h = mc_itime_ch4_h + 1
                vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 1) = mc_iyear
                vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 2) = mc_iday
                vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 3) = mc_ihour
                vars4MCMC%ch4_h%mdData(mc_itime_ch4_h, 4) = sum(CH4)
            endif
        endif
        ! cleaf
        if(vars4MCMC%cleaf%existOrNot)then
            if(vars4MCMC%cleaf%obsData(mc_itime_cleaf, 1) .eq. mc_iyear .and. &
               vars4MCMC%cleaf%obsData(mc_itime_cleaf, 2) .eq. mc_iday  .and. &
               vars4MCMC%cleaf%obsData(mc_itime_cleaf, 3) .eq. mc_ihour) then
                mc_itime_cleaf = mc_itime_cleaf + 1
                vars4MCMC%cleaf%mdData(mc_itime_cleaf, 1) = mc_iyear
                vars4MCMC%cleaf%mdData(mc_itime_cleaf, 2) = mc_iday
                vars4MCMC%cleaf%mdData(mc_itime_cleaf, 3) = mc_ihour
                vars4MCMC%cleaf%mdData(mc_itime_cleaf, 4) = QC(1) 
            endif
        endif
        ! cwood
        if(vars4MCMC%cwood%existOrNot)then
            if(vars4MCMC%cwood%obsData(mc_itime_cwood, 1) .eq. mc_iyear .and. &
               vars4MCMC%cwood%obsData(mc_itime_cwood, 2) .eq. mc_iday  .and. &
               vars4MCMC%cwood%obsData(mc_itime_cwood, 3) .eq. mc_ihour) then
                mc_itime_cwood = mc_itime_cwood + 1
                vars4MCMC%cwood%mdData(mc_itime_cwood, 1) = mc_iyear
                vars4MCMC%cwood%mdData(mc_itime_cwood, 2) = mc_iday
                vars4MCMC%cwood%mdData(mc_itime_cwood, 3) = mc_ihour
                vars4MCMC%cwood%mdData(mc_itime_cwood, 4) = QC(2)
            endif
        endif
           
    end subroutine GetSimuData

    

    subroutine ReadLineNumFromObsFile(filepath, count_lines)
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
    end subroutine ReadLineNumFromObsFile

    subroutine ReadObsDataFromFile(filepath, count_lines, resData)
        ! Jian: note that this subroutine is used to read the observational data. 
        ! The observational file must be .txt format, and with 5 columns: year, doy, hour, value, std.
        implicit none
        character(len=*), intent(in) :: filepath
        character(len=100) header
        integer STAT, count_lines, iline, n
        real resData(count_lines, 5) ! 5 colunms: year, doy, hour, value, std.

        OPEN(34, FILE=trim(filepath), status='old', ACTION='read', IOSTAT=STAT) ! open file
        read(34, '(a100)') header
        iline = 0
        do while (.TRUE.)
            iline = iline + 1
            read(34,*,iostat=STAT) (resData(iline, n), n = 1, 5)
            if(STAT .ne. 0)exit
        end do
        return
    end subroutine ReadObsDataFromFile

end module mcmc_functions