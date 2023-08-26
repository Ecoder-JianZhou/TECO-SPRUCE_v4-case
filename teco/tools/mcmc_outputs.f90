module MCMC_outputs
    use mod_data
    implicit none
    ! This part of results will be stored in CSV-format

    type mcmc_outVars_type
        ! carbon fluxes (Kg C m-2 s-1)
        real, allocatable :: gpp(:, :)
        real, allocatable :: nee(:, :)
        real, allocatable :: npp(:, :)
        real, allocatable :: nppLeaf(:, :)
        real, allocatable :: nppWood(:, :)
        real, allocatable :: nppStem(:, :)
        real, allocatable :: nppRoot(:, :)
        real, allocatable :: nppOther(:, :)           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        real, allocatable :: ra(:, :)
        real, allocatable :: raLeaf(:, :)
        real, allocatable :: raStem(:, :)
        real, allocatable :: raRoot(:, :)
        real, allocatable :: raOther(:, :)
        real, allocatable :: rMaint(:, :)
        real, allocatable :: rGrowth(:, :)            ! maintenance respiration and growth respiration
        real, allocatable :: rh(:, :)
        real, allocatable :: nbp(:, :)                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        real, allocatable :: wetlandCH4(:, :)
        real, allocatable :: wetlandCH4prod(:, :)
        real, allocatable :: wetlandCH4cons(:, :)     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        real, allocatable :: cLeaf(:, :)
        real, allocatable :: cStem(:, :)
        real, allocatable :: cRoot(:, :)
        real, allocatable :: cOther(:, :)              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        real, allocatable :: cLitter(:, :)
        real, allocatable :: cLitterCwd(:, :)          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        real, allocatable :: cSoil(:, :)
        real, allocatable :: cSoilLevels(:, :, :)
        real, allocatable :: cSoilFast(:, :)
        real, allocatable :: cSoilSlow(:, :)
        real, allocatable :: cSoilPassive(:, :)           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        real, allocatable :: CH4(:, :, :)          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        real, allocatable :: fBNF(:, :)
        real, allocatable :: fN2O(:, :)
        real, allocatable :: fNloss(:, :)
        real, allocatable :: fNnetmin(:, :)
        real, allocatable :: fNdep(:, :)                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        real, allocatable :: nLeaf(:, :)
        real, allocatable :: nStem(:, :)
        real, allocatable :: nRoot(:, :)
        real, allocatable :: nOther(:, :)
        real, allocatable :: nLitter(:, :)
        real, allocatable :: nLitterCwd(:, :)
        real, allocatable :: nSoil(:, :)
        real, allocatable :: nMineral(:, :)                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        real, allocatable :: hfls(:, :)
        real, allocatable :: hfss(:, :)
        real, allocatable :: SWnet(:, :)
        real, allocatable :: LWnet(:, :)                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        real, allocatable :: ec(:, :)
        real, allocatable :: tran(:, :)
        real, allocatable :: es(:, :)                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        real, allocatable :: hfsbl(:, :)                   ! Snow sublimation
        real, allocatable :: mrro(:, :)
        real, allocatable :: mrros(:, :)
        real, allocatable :: mrrob(:, :)                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        real, allocatable :: mrso(:, :, :)           ! Kg m-2, soil moisture in each soil layer
        real, allocatable :: tsl(:, :, :)            ! K, soil temperature in each soil layer
        real, allocatable :: tsland(:, :)                  ! K, surface temperature
        real, allocatable :: wtd(:, :)                     ! m, Water table depth
        real, allocatable :: snd(:, :)                     ! m, Total snow depth
        real, allocatable :: lai(:, :)                     ! m2 m-2, Leaf area index            
    end type mcmc_outVars_type

    real, allocatable :: tot_paramsets(:,:), upg_paramsets(:,:) sel_paramsets(:,:) 
    type(mcmc_outVars_type) sel_paramsets_outs_h
    type(mcmc_outVars_type) sel_paramsets_outs_d
    type(mcmc_outVars_type) sel_paramsets_outs_m

contains

    subroutine init_mcmc_outputs(nDAsimu, npar4DA)
        implicit none
        integer, intent(in) :: nDAsimu, npar4DA

        allocate(tot_paramsets(nDAsimu,npar4DA))
        allocate(sel_paramsets(100, npar4DA))    ! select 500 parameter sets
        call allocate_mcmc_outs_type(100, nHours,  sel_paramsets_outs_h)
        call allocate_mcmc_outs_type(100, nDays,   sel_paramsets_outs_d)
        call allocate_mcmc_outs_type(100, nMonths, sel_paramsets_outs_m)

    end subroutine init_mcmc_outputs

    subroutine mcmc_param_outputs(nUpgraded, npar4DA, parnames, DAparidx)
        implicit none
        integer, intent(in) :: nUpgraded, npar4DA
        integer nBuilt_in, ipar, nline, iline
        character(250) :: outfile_mc_ParamSets
        character(*), intent(in) :: parnames(:)
        integer, intent(in) :: DAparidx(:)
        character(20), allocatable :: DA_parname(:)
        character(1200) :: header_line

        allocate(DA_parname(npar4DA))
        header_line = ""
        do ipar = 1, npar4DA
            DA_parname(ipar) = parnames(DAparidx(ipar))
            header_line   = trim(header_line)//","//trim(parnames(DAparidx(ipar)))
        enddo

        ! delete the built-in
        nBuilt_in = int(0.1*nUpgraded)
        allocate(upg_paramsets(nUpgraded - nBuilt_in, npar4DA))
        upg_paramsets = tot_paramsets(nBuilt_in:nUpgraded, :)

        outfile_mc_ParamSets = adjustl(trim(outDir_mcmc))//"/"//adjustl(trim("total_parameter_sets.txt"))

        open(118, file=outfile_mc_ParamSets, status='replace')
        write(118, *) header_line(2:)
        do iline = 1, size(upg_paramsets, 1)
            write(118, '(*(ES10.3,:,","))') upg_paramsets(iline,:)
        enddo
        close(118)

        ! 

        ! deallocate
        deallocate(DA_parname)
    end subroutine

    subroutine mcmc_simu_outputs()
        implicit none
        character(250) :: outfile_totparamset, outfile_selparamset, outfile_simu
        ! outdir_mcmc
        ! open(293, file=)

    end subroutine mcmc_outputs

    subroutine allocate_mcmc_outs_type(ntime, nSimuLen, dataType)
        implicit none
        ! 
        integer, intent(in) :: ntime, nSimuLen
        type(mcmc_outVars_type), intent(out) :: dataType
        allocate(dataType%gpp(ntime, nSimuLen))
        allocate(dataType%nee(ntime, nSimuLen))
        allocate(dataType%npp(ntime, nSimuLen))
        allocate(dataType%nppLeaf(ntime, nSimuLen))
        allocate(dataType%nppWood(ntime, nSimuLen))
        allocate(dataType%nppStem(ntime, nSimuLen))
        allocate(dataType%nppRoot(ntime, nSimuLen))
        allocate(dataType%nppOther(ntime, nSimuLen))           ! According to SPRUCE-MIP, stem means above ground woody tissues which is different from wood tissues.
        allocate(dataType%ra(ntime, nSimuLen))
        allocate(dataType%raLeaf(ntime, nSimuLen))
        allocate(dataType%raStem(ntime, nSimuLen))
        allocate(dataType%raRoot(ntime, nSimuLen))
        allocate(dataType%raOther(ntime, nSimuLen))
        allocate(dataType%rMaint(ntime, nSimuLen))
        allocate(dataType%rGrowth(ntime, nSimuLen))            ! maintenance respiration and growth respiration
        allocate(dataType%rh(ntime, nSimuLen))
        allocate(dataType%nbp(ntime, nSimuLen))                ! heterotrophic respiration. NBP(net biome productivity) = GPP - Rh - Ra - other losses  
        allocate(dataType%wetlandCH4(ntime, nSimuLen))
        allocate(dataType%wetlandCH4prod(ntime, nSimuLen))
        allocate(dataType%wetlandCH4cons(ntime, nSimuLen))     ! wetland net fluxes of CH4, CH4 production, CH4 consumption
        ! Carbon Pools  (KgC m-2)
        allocate(dataType%cLeaf(ntime, nSimuLen))
        allocate(dataType%cStem(ntime, nSimuLen))
        allocate(dataType%cRoot(ntime, nSimuLen))
        allocate(dataType%cOther(ntime, nSimuLen))              ! cOther: carbon biomass in other plant organs(reserves, fruits), Jian: maybe NSC storage in TECO?
        allocate(dataType%cLitter(ntime, nSimuLen))
        allocate(dataType%cLitterCwd(ntime, nSimuLen))          ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(dataType%cSoil(ntime, nSimuLen))
        allocate(dataType%cSoilLevels(ntime, nSimuLen, nlayers))
        allocate(dataType%cSoilFast(ntime, nSimuLen))
        allocate(dataType%cSoilSlow(ntime, nSimuLen))
        allocate(dataType%cSoilPassive(ntime, nSimuLen))           ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(dataType%CH4(ntime, nSimuLen, nlayers))          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(dataType%fBNF(ntime, nSimuLen))
        allocate(dataType%fN2O(ntime, nSimuLen))
        allocate(dataType%fNloss(ntime, nSimuLen))
        allocate(dataType%fNnetmin(ntime, nSimuLen))
        allocate(dataType%fNdep(ntime, nSimuLen))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(dataType%nLeaf(ntime, nSimuLen))
        allocate(dataType%nStem(ntime, nSimuLen))
        allocate(dataType%nRoot(ntime, nSimuLen))
        allocate(dataType%nOther(ntime, nSimuLen))
        allocate(dataType%nLitter(ntime, nSimuLen))
        allocate(dataType%nLitterCwd(ntime, nSimuLen))
        allocate(dataType%nSoil(ntime, nSimuLen))
        allocate(dataType%nMineral(ntime, nSimuLen))                ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(dataType%hfls(ntime, nSimuLen))
        allocate(dataType%hfss(ntime, nSimuLen))
        allocate(dataType%SWnet(ntime, nSimuLen))
        allocate(dataType%LWnet(ntime, nSimuLen))                   ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(dataType%ec(ntime, nSimuLen))
        allocate(dataType%tran(ntime, nSimuLen))
        allocate(dataType%es(ntime, nSimuLen))                      ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(dataType%hfsbl(ntime, nSimuLen))                   ! Snow sublimation
        allocate(dataType%mrro(ntime, nSimuLen))
        allocate(dataType%mrros(ntime, nSimuLen))
        allocate(dataType%mrrob(ntime, nSimuLen))                   ! Total runoff; Surface runoff; Subsurface runoff
        ! other
        allocate(dataType%mrso(ntime, nSimuLen, nlayers))           ! Kg m-2, soil moisture in each soil layer
        allocate(dataType%tsl(ntime, nSimuLen, nlayers))            ! K, soil temperature in each soil layer
        allocate(dataType%tsland(ntime, nSimuLen))                  ! K, surface temperature
        allocate(dataType%wtd(ntime, nSimuLen))                     ! m, Water table depth
        allocate(dataType%snd(ntime, nSimuLen))                     ! m, Total snow depth
        allocate(dataType%lai(ntime, nSimuLen))
    end subroutine allocate_mcmc_outs_type

    subroutine deallocate_mcmc_outs_type(dataType)
        type(mcmc_outVars_type), intent(inout) :: dataType

        if (allocated(dataType%gpp))            deallocate(dataType%gpp)
        if (allocated(dataType%nee))            deallocate(dataType%nee)
        if (allocated(dataType%npp))            deallocate(dataType%npp)
        if (allocated(dataType%nppLeaf))        deallocate(dataType%nppLeaf)
        if (allocated(dataType%nppWood))        deallocate(dataType%nppWood)
        if (allocated(dataType%nppStem))        deallocate(dataType%nppStem)
        if (allocated(dataType%nppRoot))        deallocate(dataType%nppRoot)
        if (allocated(dataType%nppOther))       deallocate(dataType%nppOther)           
        if (allocated(dataType%ra))             deallocate(dataType%ra)
        if (allocated(dataType%raLeaf))         deallocate(dataType%raLeaf)
        if (allocated(dataType%raStem))         deallocate(dataType%raStem)
        if (allocated(dataType%raRoot))         deallocate(dataType%raRoot)
        if (allocated(dataType%raOther))        deallocate(dataType%raOther)
        if (allocated(dataType%rMaint))         deallocate(dataType%rMaint)
        if (allocated(dataType%rGrowth))        deallocate(dataType%rGrowth)           
        if (allocated(dataType%rh))             deallocate(dataType%rh)
        if (allocated(dataType%nbp))            deallocate(dataType%nbp)                
        if (allocated(dataType%wetlandCH4))     deallocate(dataType%wetlandCH4)
        if (allocated(dataType%wetlandCH4prod)) deallocate(dataType%wetlandCH4prod)
        if (allocated(dataType%wetlandCH4cons)) deallocate(dataType%wetlandCH4cons)  
        ! Carbon Pools  (KgC m-2)
        if (allocated(dataType%cLeaf))        deallocate(dataType%cLeaf)
        if (allocated(dataType%cStem))        deallocate(dataType%cStem)
        if (allocated(dataType%cRoot))        deallocate(dataType%cRoot)
        if (allocated(dataType%cOther))       deallocate(dataType%cOther)
        if (allocated(dataType%cLitter))      deallocate(dataType%cLitter)
        if (allocated(dataType%cLitterCwd))   deallocate(dataType%cLitterCwd)
        if (allocated(dataType%cSoil))        deallocate(dataType%cSoil)
        if (allocated(dataType%cSoilLevels))  deallocate(dataType%cSoilLevels)
        if (allocated(dataType%cSoilFast))    deallocate(dataType%cSoilFast)
        if (allocated(dataType%cSoilSlow))    deallocate(dataType%cSoilSlow)
        if (allocated(dataType%cSoilPassive)) deallocate(dataType%cSoilPassive)
        if (allocated(dataType%CH4))         deallocate(dataType%CH4)
        ! Nitrogen fluxes (kgN m-2 s-1)
        if (allocated(dataType%fBNF))         deallocate(dataType%fBNF)
        if (allocated(dataType%fN2O))         deallocate(dataType%fN2O)
        if (allocated(dataType%fNloss))       deallocate(dataType%fNloss)
        if (allocated(dataType%fNnetmin))     deallocate(dataType%fNnetmin)
        if (allocated(dataType%fNdep))        deallocate(dataType%fNdep)
        ! Nitrogen pools (kgN m-2)
        if (allocated(dataType%nLeaf))        deallocate(dataType%nLeaf)
        if (allocated(dataType%nStem))        deallocate(dataType%nStem)
        if (allocated(dataType%nRoot))        deallocate(dataType%nRoot)
        if (allocated(dataType%nOther))       deallocate(dataType%nOther)
        if (allocated(dataType%nLitter))      deallocate(dataType%nLitter)
        if (allocated(dataType%nLitterCwd))   deallocate(dataType%nLitterCwd)
        if (allocated(dataType%nSoil))        deallocate(dataType%nSoil)
        if (allocated(dataType%nMineral))     deallocate(dataType%nMineral)
        ! energy fluxes (W m-2)
        if (allocated(dataType%hfls))         deallocate(dataType%hfls)
        if (allocated(dataType%hfss))         deallocate(dataType%hfss)
        if (allocated(dataType%SWnet))        deallocate(dataType%SWnet)
        if (allocated(dataType%LWnet))        deallocate(dataType%LWnet)
        ! water fluxes (kg m-2 s-1)
        if (allocated(dataType%ec))           deallocate(dataType%ec)
        if (allocated(dataType%tran))         deallocate(dataType%tran)
        if (allocated(dataType%es))           deallocate(dataType%es)
        if (allocated(dataType%hfsbl))        deallocate(dataType%hfsbl)
        if (allocated(dataType%mrro))         deallocate(dataType%mrro)
        if (allocated(dataType%mrros))        deallocate(dataType%mrros)
        if (allocated(dataType%mrrob))        deallocate(dataType%mrrob)
        ! other
        if (allocated(dataType%mrso))         deallocate(dataType%mrso)      
        if (allocated(dataType%tsl))          deallocate(dataType%tsl)       
        if (allocated(dataType%tsland))       deallocate(dataType%tsland) 
        if (allocated(dataType%wtd))          deallocate(dataType%wtd)
        if (allocated(dataType%snd))          deallocate(dataType%snd)
        if (allocated(dataType%lai))          deallocate(dataType%lai)
    end subroutine deallocate_mcmc_outs_type

end module MCMC_outputs