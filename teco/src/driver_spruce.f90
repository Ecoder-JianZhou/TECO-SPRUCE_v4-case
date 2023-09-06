! ----------- driver module for three types of species in SPRUCE site ----------
! ---------------- Tree, Shrub and sphagnum ------------------------------------
! ------------------------ 09/01/2023 ------------------------------------------

module driver_multi_specs
    use mod_data
    use mod_vegetation
    use mod_soil
    use mod_transfer
    use mod_upAndSum
    ! use mcmc_functions
    implicit none
    integer daysOfyear, hoursOfmonth, hoursOfYear, daysOfmonth(12) 
    integer iTotHourly, iTotDaily, iTotMonthly  ! used to cycle the record of different frequences of the results
contains
    subroutine teco_multi_specs_simu()
        implicit none
        integer year0, first_year                               ! year0: record the current year to judge whether a new year
        real    Difference                                      ! GPP-Rauto-NPP. Jian: no sure whether for balance? 
        real    RaLeaf,RaStem,RaRoot                            ! for summary the automatic respiration of leaf, stem, root
        integer dlayer                                          ! to run cycle of each layer.
        real    Q_soil                                          ! total soil carbon
        real    RECOh                                           ! ecosytem respiration
        real    ETh, Th, Eh                                     ! record hourly ET, transp, evap. Jian: why not use original variable?
        real    INTh,ROh,DRAINh,LEh,SHh                         ! 
        real    VPDh, LWH
        real    esat1 

        first_year = forcing(1)%year ! mark the fist year of the forcing
        do iforcing = 1, nforcing ! cycle all of the forcing data
            if (iforcing .eq. 1) then
                year0 = first_year             ! Jian: record whether it is a new year.
                iTotHourly  = 1
                iTotDaily   = 1
                iTotMonthly = 1
            endif
            iyear = forcing(iforcing)%year                      ! force%year
            iday  = forcing(iforcing)%doy                    
            ihour = forcing(iforcing)%hour
            ! if it is a new year

            if ((iday .eq. 1) .and. (ihour .eq. 0)) call init_update_year()
            if (do_simu .and. (iday .eq. 1) .and. (ihour .eq. 0)) write(*,*)iyear
            if ((iyear .eq. 1974) .and. (iday .eq. 1) .and. (ihour .eq. 0))then
                ! 1974 remove 99% of tree biomass
                QC(1)    = 0.1 * QC(1)
                QC(2)    = 0.1 * QC(2)
                QC(3)    = 0.1 * QC(3)
                QN(1)    = 0.1 * QN(1)
                QN(2)    = 0.1 * QN(2)
                QN(3)    = 0.1 * QN(3)
                bmleaf   = 0.1 * bmleaf
                bmstem   = 0.1 * bmstem
                bmroot   = 0.1 * bmroot
                nsc      = 0.1 * nsc
                nsn      = 0.1 * nsn
                storage  = 0.1 * storage
                lai      = LAIMIN!0.1 * lai
                stor_use = 0.1 * stor_use
            endif

            ! leap year
            if (do_leap) then
                if(iday .eq. 1) call isLeap_update_daysOfyear()
            else
                daysOfyear = 365
            endif

            ! for update the results of monthly and yearly
            call update_hoursOfYear_daysOfmonth_initMonthly()
            ! call update_summary_init_monthly()

            ! initialize the daily variables to run hourly simulaiton.
            if (ihour .eq. 0) then
                ! a new day simulation.
                if (do_snow) then 
                    if (iyear .eq. first_year .and. iday .eq. 1.) then
                        ta     = -12.85                         ! since changed the ta criteria (0. to 1.e-10)) in calculating melt
                        rain_d = 0.                             ! dbmemo
                    endif
                    call snow_d()                               ! Jian: update snow_dsim snow_d(rain_d,lat,days,ta,snow_dsim,fa,fsub,rho_snow,melt,dcount,decay_m)                            
                    snow_depth_e = snow_dsim
                endif
                StemSap = AMIN1(Stemmax,SapS*bmStem)            ! Stemmax and SapS were input from parameter file, what are they? Unit? Maximum stem biomass? -JJJJJJJJJJJJJJJJJJJJJJ 
                RootSap = AMIN1(Rootmax,SapR*bmRoot)
                NSCmax  = 0.05*(StemSap+RootSap+QC(1))          ! Jian: update the NSCmax each step? and fixed NSCmin  = 5.? 
                if(Ta.gt.5.0) GDD5 = GDD5+Ta
                call init_day()                                 ! Jian: initilize the daily data.
            endif

            ! forcing data --------------------------------------------------------------------------------
            Tair  = forcing(iforcing)%Tair                      ! Tair
            Tsoil = forcing(iforcing)%Tsoil                     ! SLT
            co2ca = forcing(iforcing)%CO2*1.0E-6                ! CO2 concentration,ppm-->1.0E-6
            if (co2ca .lt. 0) co2ca = 380.0*1.0E-6              ! Jian: if no CO2 (-9999), then use the default value 
            Tair  = Tair  + Ttreat                              ! Jian: whether it has the treatment
            Tsoil = Tsoil + Ttreat
            if (CO2treat .ne. 0.) co2ca = CO2treat*1.0E-6 
            ! ----------------------------------------------------------                
            RH     = forcing(iforcing)%RH
            Dair   = forcing(iforcing)%VPD                      ! air water vapour defficit? Unit Pa
            rain   = forcing(iforcing)%Rain                     ! rain fal per hour
            wind   = ABS(forcing(iforcing)%WS)                  ! wind speed m s-1
            PAR    = forcing(iforcing)%PAR                      ! Unit ? umol/s/m-2
            radsol = forcing(iforcing)%PAR                      ! unit ? PAR actually  Jian: or incoming shortwave/longwave radiation?
            dpatm  = forcing(iforcing)%PBOT
            if (do_ndep) N_deposit = forcing(iforcing)%Ndep*3600
            ! Ajust some unreasonable values 
            RH     = AMAX1(0.01,AMIN1(99.99,RH))                ! relative humidity
            esat1  = 610.78*exp(17.27*Tair/(Tair + 237.3))      ! intermediate parameter
            eairP  = esat1*RH/100.                              ! Added for SPRUCE, due to lack of VPD data. Jian: ? SPRUCE has the data? !air water vapour pressure
            Dair   = esat1-eairP                                ! Jian: confused that SPRUCE has the VPD data, why calculate it again?
            radsol = AMAX1(radsol,0.01)

            ! intially added for soil thermal/ soil water
            if (do_snow) then
                snow_depth = snow_depth_e
            else
                snow_depth = snow_in(iforcing)                  ! read from input file
            endif
            if (snow_depth .lt. 0.0) snow_depth = 0.0   
            snow_depth = snow_depth*100.                        ! change from m to cm  
            
            ! Jian: G and Esoil?
            if (do_soilphy) then 
                GOTO 160
            endif
            if(radsol.gt.10.0) then
                G = -25.0
            else
                G = 20.5
            endif
            if (isnan(G)) stop
            Esoil = 0.05*radsol
            if(radsol.LE.10.0) Esoil = 0.5*G
160 continue        
            ! for daily mean conditions 
            ta     = ta + tair/24.0                             ! sum of a day, for calculating daily mean temperature, snow_d and soilwater
            rain_d = rain_d+rain                                
            ! calculating scaling factor of NSC
            if(NSC.le.NSCmin)fnsc=0.0
            if(NSC.ge.NSCmax)fnsc=1.0
            if((NSC.lt.NSCmax).and.(NSC.gt.NSCmin))then 
                fnsc=(NSC-NSCmin)/(NSCmax-NSCmin)
            endif
            ! update vcmx0 and eJmx0 according to C/N of leaves
            Vcmx0 = Vcmax0*SNvcmax*1.0e-6
            eJmx0 = 1.67*Vcmx0 ! Weng 02/21/2011 Medlyn et al. 2002 
            eJmx0 = JV*Vcmx0   ! added for acclimation study,replace 1.67 with JV Feb 19 2019 Shuang    
            
            call canopy()      ! run canopy module
            ! run soil water processes
            call soilwater()                      
            ET = evap+transp
            
            ! ! Jian: to update module
            call respiration()
            ! THE Third Part: update LAI
            call plantgrowth()

            ! THE Fourth PART: simulating C influx allocation in pools
            call TCS_CN()  
            ! if (do_matrix) call matrix_struct() 
            call methane()       !update single value of Rh_pools,Tsoil,zwt,wsc 
           
            ! update NSC
            Rauto      = Rmain + Rgrowth + Rnitrogen
            NSC        = NSC + GPP - Rauto - (NPP-add)-store
            Difference = GPP - Rauto - NPP
            if(NSC<0)then
                bmstem = bmstem + NSC/0.48
                NPP    = Amax1(NPP + NSC, 0.) 
                NSN    = NSN    - NSC/CN(2)
                NSC    = 0.
            endif

            ! Rhetero=Rh_f + Rh_c + Rh_Micr + Rh_Slow + Rh_Pass
            Rhetero = Rh_pools(1) + Rh_pools(2) + Rh_pools(3) &
                &   + Rh_pools(4) + Rh_pools(5)
            Rsoil   = Rhetero+RmRoot+RgRoot+Rnitrogen
            NEE     = Rauto+Rhetero - GPP
            Q_soil  = QC(6) + QC(7) + QC(8)
            bmleaf  = QC(1)/0.48
            bmstem  = QC(2)/0.48
            bmroot  = QC(3)/0.48
            bmplant = bmleaf+bmroot+bmstem
            LAI     = bmleaf*SLA
            ! NMIN_d  = NMIN_d+N_miner
            ! output hourly
            Recoh   = Rhetero+Rauto
            ETh     = ET !*1000.
            Th      = transp !*1000.
            Eh      = evap !*1000.
            INTh    = -9999
            VPDh    = Dair/1000.
            ROh     = runoff !*1000.
            DRAINh  = -9999
            LEh     = ETh*((2.501-0.00236*Tair)*1000.0)/3600.
            SHh     = -9999
            LWh     = -9999
            NEP     = -NEE

            call updateHourly()
            call summaryHourly(iTotHourly)
            call updateDaily()
            call updateMonthly(hoursOfmonth)

            call updateYearly(hoursOfYear)

            if(ihour .eq. 23) then
                if((GDD5.gt.gddonset) .and. phenoset.eq.0) then
                    pheno    = iday    ! pheno=days
                    phenoset = 1
                endif
            endif

            if (iforcing .eq. 1) then
                ! i_record = 1
                ! nday4out = 0
            endif
            if (ihour .eq. 23) then
                call summaryDaily(iTotDaily)
            end if
            call update_summary_monthly()
            
            if (do_mcmc) call GetSimuData(iyear, iday, ihour)
                 
            if (iforcing < nforcing)then
                if (forcing(iforcing+1)%year>iyear) then            
                    year0        = iyear                      ! update the record of year (year0)
                    storage      = accumulation
                    stor_use     = Storage/times_storage_use
                    accumulation = 0.0
                    onset        = 0
                endif
            else
                year0        = iyear                          ! update the record of year (year0)
                storage      = accumulation
                stor_use     = Storage/times_storage_use
                accumulation = 0.0
                onset        = 0
            endif
        enddo
    end subroutine teco_multi_specs_simu

end module driver_multi_specs