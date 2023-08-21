module mod_spinup
    use mod_data
    use driver

    !-------------------------------------------------------------------------------------------------------
    integer iloop

    contains
    subroutine run_spinup()
        implicit none
        write(*,*)"This is spinup", nloops
        do iloop = 1, nloops
            write(*,*) "iloop: ", iloop
            call teco_simu()
            call update_spinup_values()
        enddo
        
    end subroutine run_spinup
    
    !-----------------------------------------------------------------------
    subroutine update_spinup_values()
        implicit none 
        tot_outVars_spinup%gpp(iloop)            = outVars_y%gpp
        tot_outVars_spinup%npp(iloop)            = outVars_y%npp
        tot_outVars_spinup%ra(iloop)             = outVars_y%ra
        tot_outVars_spinup%rh(iloop)             = outVars_y%rh
        tot_outVars_spinup%wetlandCH4(iloop)     = outVars_y%wetlandCH4
        tot_outVars_spinup%wetlandCH4prod(iloop) = outVars_y%wetlandCH4prod
        tot_outVars_spinup%wetlandCH4cons(iloop) = outVars_y%wetlandCH4cons
        ! Carbon Pools  (KgC m-2)
        tot_outVars_spinup%cLeaf(iloop)          = outVars_y%cLeaf
        tot_outVars_spinup%cStem(iloop)          = outVars_y%cStem
        tot_outVars_spinup%cRoot(iloop)          = outVars_y%cRoot
        tot_outVars_spinup%cOther(iloop)         = outVars_y%cOther             
        tot_outVars_spinup%cLitter(iloop)        = outVars_y%cLitter
        tot_outVars_spinup%cLitterCwd(iloop)     = outVars_y%cLitterCwd                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        tot_outVars_spinup%cSoil(iloop)          = outVars_y%cSoil
        tot_outVars_spinup%cSoilFast(iloop)      = outVars_y%cSoilFast
        tot_outVars_spinup%cSoilSlow(iloop)      = outVars_y%cSoilSlow
        tot_outVars_spinup%cSoilPassive(iloop)   = outVars_y%cSoilPassive                            ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        tot_outVars_spinup%CH4(iloop,:)          = outVars_y%CH4                                                          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        tot_outVars_spinup%fBNF(iloop)           = outVars_y%fBNF
        tot_outVars_spinup%fN2O(iloop)           = outVars_y%fN2O
        tot_outVars_spinup%fNloss(iloop)         = outVars_y%fNloss
        tot_outVars_spinup%fNnetmin(iloop)       = outVars_y%fNnetmin
        tot_outVars_spinup%fNdep(iloop)          = outVars_y%fNdep                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        tot_outVars_spinup%nLeaf(iloop)          = outVars_y%nLeaf
        tot_outVars_spinup%nStem(iloop)          = outVars_y%nStem
        tot_outVars_spinup%nRoot(iloop)          = outVars_y%nRoot
        tot_outVars_spinup%nOther(iloop)         = outVars_y%nOther
        tot_outVars_spinup%nLitter(iloop)        = outVars_y%nLitter
        tot_outVars_spinup%nLitterCwd(iloop)     = outVars_y%nLitterCwd
        tot_outVars_spinup%nSoil(iloop)          = outVars_y%nSoil
        tot_outVars_spinup%nMineral(iloop)       = outVars_y%nMineral                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        tot_outVars_spinup%hfls(iloop)           = outVars_y%hfls
        tot_outVars_spinup%hfss(iloop)           = outVars_y%hfss                                                       ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        tot_outVars_spinup%ec(iloop)             = outVars_y%ec
        tot_outVars_spinup%tran(iloop)           = outVars_y%tran
        tot_outVars_spinup%es(iloop)             = outVars_y%es                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        tot_outVars_spinup%hfsbl(iloop)          = outVars_y%hfsbl                                                         ! Snow sublimation
        tot_outVars_spinup%mrro(iloop)           = outVars_y%mrro
        tot_outVars_spinup%mrros(iloop)          = outVars_y%mrros
        tot_outVars_spinup%mrrob(iloop)          = outVars_y%mrrob
        tot_outVars_spinup%lai(iloop)            = outVars_y%lai
    end subroutine update_spinup_values

    !-----------------------------------------------------------------------
    subroutine init_spinup_variables()
        implicit none
        allocate(tot_outVars_spinup%gpp(nloops))
        allocate(tot_outVars_spinup%npp(nloops))
        allocate(tot_outVars_spinup%ra(nloops))
        allocate(tot_outVars_spinup%rh(nloops)) 
        allocate(tot_outVars_spinup%wetlandCH4(nloops))
        allocate(tot_outVars_spinup%wetlandCH4prod(nloops))
        allocate(tot_outVars_spinup%wetlandCH4cons(nloops))
        ! Carbon Pools  (KgC m-2)
        allocate(tot_outVars_spinup%cLeaf(nloops))
        allocate(tot_outVars_spinup%cStem(nloops))
        allocate(tot_outVars_spinup%cRoot(nloops))
        allocate(tot_outVars_spinup%cOther(nloops))                             
        allocate(tot_outVars_spinup%cLitter(nloops))
        allocate(tot_outVars_spinup%cLitterCwd(nloops))                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        allocate(tot_outVars_spinup%cSoil(nloops))
        allocate(tot_outVars_spinup%cSoilFast(nloops))
        allocate(tot_outVars_spinup%cSoilSlow(nloops))
        allocate(tot_outVars_spinup%cSoilPassive(nloops))                            ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        allocate(tot_outVars_spinup%CH4(nloops,nlayers))                                                          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        allocate(tot_outVars_spinup%fBNF(nloops))
        allocate(tot_outVars_spinup%fN2O(nloops))
        allocate(tot_outVars_spinup%fNloss(nloops))
        allocate(tot_outVars_spinup%fNnetmin(nloops))
        allocate(tot_outVars_spinup%fNdep(nloops))                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        allocate(tot_outVars_spinup%nLeaf(nloops))
        allocate(tot_outVars_spinup%nStem(nloops))
        allocate(tot_outVars_spinup%nRoot(nloops))
        allocate(tot_outVars_spinup%nOther(nloops))
        allocate(tot_outVars_spinup%nLitter(nloops))
        allocate(tot_outVars_spinup%nLitterCwd(nloops))
        allocate(tot_outVars_spinup%nSoil(nloops))
        allocate(tot_outVars_spinup%nMineral(nloops))                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        allocate(tot_outVars_spinup%hfls(nloops))
        allocate(tot_outVars_spinup%hfss(nloops))                                                       ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        allocate(tot_outVars_spinup%ec(nloops))
        allocate(tot_outVars_spinup%tran(nloops))
        allocate(tot_outVars_spinup%es(nloops))                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        allocate(tot_outVars_spinup%hfsbl(nloops))                                                         ! Snow sublimation
        allocate(tot_outVars_spinup%mrro(nloops))
        allocate(tot_outVars_spinup%mrros(nloops))
        allocate(tot_outVars_spinup%mrrob(nloops))
        allocate(tot_outVars_spinup%lai(nloops))
    end subroutine init_spinup_variables
    
    !-----------------------------------------------------------------------
    subroutine deallo_spinup_variables()
        implicit none
        if(allocated(tot_outVars_spinup%gpp))            deallocate(tot_outVars_spinup%gpp)
        if(allocated(tot_outVars_spinup%npp))            deallocate(tot_outVars_spinup%npp)
        if(allocated(tot_outVars_spinup%ra))             deallocate(tot_outVars_spinup%ra)
        if(allocated(tot_outVars_spinup%rh))             deallocate(tot_outVars_spinup%rh) 
        if(allocated(tot_outVars_spinup%wetlandCH4))     deallocate(tot_outVars_spinup%wetlandCH4)
        if(allocated(tot_outVars_spinup%wetlandCH4prod)) deallocate(tot_outVars_spinup%wetlandCH4prod)
        if(allocated(tot_outVars_spinup%wetlandCH4cons)) deallocate(tot_outVars_spinup%wetlandCH4cons)
        ! Carbon Pools  (KgC m-2)
        if(allocated(tot_outVars_spinup%cLeaf))        deallocate(tot_outVars_spinup%cLeaf)
        if(allocated(tot_outVars_spinup%cStem))        deallocate(tot_outVars_spinup%cStem)
        if(allocated(tot_outVars_spinup%cRoot))        deallocate(tot_outVars_spinup%cRoot)
        if(allocated(tot_outVars_spinup%cOther))       deallocate(tot_outVars_spinup%cOther)                             
        if(allocated(tot_outVars_spinup%cLitter))      deallocate(tot_outVars_spinup%cLitter)
        if(allocated(tot_outVars_spinup%cLitterCwd))   deallocate(tot_outVars_spinup%cLitterCwd)                                         ! litter (excluding coarse woody debris), Jian: fine litter in TECO?, cLitterCwd: carbon in coarse woody debris
        if(allocated(tot_outVars_spinup%cSoil))        deallocate(tot_outVars_spinup%cSoil)
        if(allocated(tot_outVars_spinup%cSoilFast))    deallocate(tot_outVars_spinup%cSoilFast)
        if(allocated(tot_outVars_spinup%cSoilSlow))    deallocate(tot_outVars_spinup%cSoilSlow)
        if(allocated(tot_outVars_spinup%cSoilPassive)) deallocate(tot_outVars_spinup%cSoilPassive)                            ! cSoil: soil organic carbon (Jian: total soil carbon); cSoilLevels(depth-specific soil organic carbon, Jian: depth?); cSoilPools (different pools without depth)
        if(allocated(tot_outVars_spinup%CH4))         deallocate(tot_outVars_spinup%CH4)                                                          ! methane concentration
        ! Nitrogen fluxes (kgN m-2 s-1)
        if(allocated(tot_outVars_spinup%fBNF))     deallocate(tot_outVars_spinup%fBNF)
        if(allocated(tot_outVars_spinup%fN2O))     deallocate(tot_outVars_spinup%fN2O)
        if(allocated(tot_outVars_spinup%fNloss))   deallocate(tot_outVars_spinup%fNloss)
        if(allocated(tot_outVars_spinup%fNnetmin)) deallocate(tot_outVars_spinup%fNnetmin)
        if(allocated(tot_outVars_spinup%fNdep))    deallocate(tot_outVars_spinup%fNdep)                   ! fBNF: biological nitrogen fixation; fN2O: loss of nitrogen through emission of N2O; fNloss:Total loss of nitrogen to the atmosphere and from leaching; net mineralizaiton and deposition of N
        ! Nitrogen pools (kgN m-2)
        if(allocated(tot_outVars_spinup%nLeaf))      deallocate(tot_outVars_spinup%nLeaf)
        if(allocated(tot_outVars_spinup%nStem))      deallocate(tot_outVars_spinup%nStem)
        if(allocated(tot_outVars_spinup%nRoot))      deallocate(tot_outVars_spinup%nRoot)
        if(allocated(tot_outVars_spinup%nOther))     deallocate(tot_outVars_spinup%nOther)
        if(allocated(tot_outVars_spinup%nLitter))    deallocate(tot_outVars_spinup%nLitter)
        if(allocated(tot_outVars_spinup%nLitterCwd)) deallocate(tot_outVars_spinup%nLitterCwd)
        if(allocated(tot_outVars_spinup%nSoil))      deallocate(tot_outVars_spinup%nSoil)
        if(allocated(tot_outVars_spinup%nMineral))   deallocate(tot_outVars_spinup%nMineral)                    ! nMineral: Mineral nitrogen pool
        ! energy fluxes (W m-2)
        if(allocated(tot_outVars_spinup%hfls)) deallocate(tot_outVars_spinup%hfls)
        if(allocated(tot_outVars_spinup%hfss)) deallocate(tot_outVars_spinup%hfss)                                                       ! Sensible heat flux; Latent heat flux; Net shortwave radiation; Net longwave radiation
        ! water fluxes (kg m-2 s-1)
        if(allocated(tot_outVars_spinup%ec))    deallocate(tot_outVars_spinup%ec)
        if(allocated(tot_outVars_spinup%tran))  deallocate(tot_outVars_spinup%tran)
        if(allocated(tot_outVars_spinup%es))    deallocate(tot_outVars_spinup%es)                                              ! Canopy evaporation; Canopy transpiration; Soil evaporation
        if(allocated(tot_outVars_spinup%hfsbl)) deallocate(tot_outVars_spinup%hfsbl)                                                         ! Snow sublimation
        if(allocated(tot_outVars_spinup%mrro))  deallocate(tot_outVars_spinup%mrro)
        if(allocated(tot_outVars_spinup%mrros)) deallocate(tot_outVars_spinup%mrros)
        if(allocated(tot_outVars_spinup%mrrob)) deallocate(tot_outVars_spinup%mrrob)
        if(allocated(tot_outVars_spinup%lai))   deallocate(tot_outVars_spinup%lai)
    end subroutine deallo_spinup_variables

    !-----------------------------------------------------------------------
end module mod_spinup