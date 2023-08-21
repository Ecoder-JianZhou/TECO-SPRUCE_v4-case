program TECO
    use mod_data
    use mod_spinup
    use mod_mcmc
    use driver
    use mod_ncd_io
    ! to run TECO simulation, spin-up and data simulation
    implicit none
    ! ! character(len=32) :: cmdChar_folder, out_csv, out_nc, out_nc_daily, out_nc_hourly, out_nc_monthly
    ! character(len=1000) :: outDir_nc, outfile 

    call read_TECO_model_configs()  ! get the file of "TECO_model_configs.nml", including parameters

    ! create the TECO case, mainly creating the output dir
    call createNewCase()


    if (do_spinup)then
        ! outDir_sp  = adjustl(trim(outdir))//"/res_spinup_nc"
        ! outFile_sp = adjustl(trim(outDir_sp))//"/results_spinup.nc"
        ! call CreateFolder(adjustl(trim(outDir_sp)))
        call init_spinup_variables()
    endif

    ! call get_params()                           ! read parameters values
    call get_forcingdata()                      ! read forcing data
    if (.not. do_snow) call get_snowdepth()
    
    ! nHours  = nforcing
    ! nDays   = nHours/24.
    ! nYears  = int(nforcing/(365*24))
    ! nMonths = nYears*12 
    ! Write(*,*)nHours, nDays, nMonths, nYears
    call assign_all_results(nHours, nDays, nMonths, nYears)
    if (.not. do_snow) call get_snowdepth()
    
    call initialize()                           ! initializations
    if (do_restart)then
        call read_restart(restartfile)
        call initialize_with_restart()
    endif
    ! itest = 0
    if (do_spinup) then
        call run_spinup()
        call write_spinup_res()
        call deallo_spinup_variables()
    endif 
    if (do_mcmc) then
        ! call read_obs()
        call init_mcmc()
        call run_mcmc()
        call check_mcmc()
    endif
    
    ! itest = 1
    call teco_simu()
    ! writing output file...
!     i_record = 1
!     do idayOfnyear=1,nday4out
!         ! write(662,6602)i,record_yr(i_record),idayOfnyear,(Simu_dailyflux14(j,i_record),j=1,14)
!         write(662,6602)i_record,record_yr(i_record),idayOfnyear,(Simu_dailyflux14_2023(j,i_record),j=1,28)
!         i_record=i_record+1
!     enddo
    
! 6602     format(3(i7,","),27(f15.4,","),(f15.4))
!     close(662)
    call spruce_mip_cmip6Format()
    call write_restart()
    ! end of the simulation, then deallocate the forcing_data
    deallocate(forcing%year)
    deallocate(forcing%doy)
    deallocate(forcing%hour)
    deallocate(forcing%Tair)
    deallocate(forcing%Tsoil)
    deallocate(forcing%RH)
    deallocate(forcing%VPD)
    deallocate(forcing%Rain)
    deallocate(forcing%WS)
    deallocate(forcing%PAR)
    deallocate(forcing%CO2)
    deallocate(forcing%PBOT)
    deallocate(forcing%Ndep)
    ! deallocate the snow_in
    if (.not. do_snow) deallocate(snow_in)
    call deallocate_all_results()
end program TECO

subroutine createNewCase()
    ! create a new case to run the TECO model
    !   * create the output path
    character(1000) :: new_outdir,   new_outdir_nc, new_outdir_csv
    character(1000) :: new_outdir_h, new_outdir_d,  new_outdir_m
    character(1000) :: new_outDir_spinup

    new_outdir = adjustl(trim(outdir))//"/"//adjustl(trim(simu_name))
    call CreateFolder(adjustl(trim(new_outdir)))
    new_outdir_nc  = adjustl(trim(new_outdir))//"/"//adjustl(trim(outDir_nc))
    new_outdir_csv = adjustl(trim(new_outdir))//"/"//adjustl(trim(outDir_csv))
    call CreateFolder(adjustl(trim(new_outdir_nc)))
    call CreateFolder(adjustl(trim(new_outdir_csv)))
    new_outdir_h = adjustl(trim(new_outdir_nc))//"/"//adjustl(trim(outDir_h))
    new_outdir_d = adjustl(trim(new_outdir_nc))//"/"//adjustl(trim(outDir_d))
    new_outdir_m = adjustl(trim(new_outdir_nc))//"/"//adjustl(trim(outDir_m))
    call CreateFolder(adjustl(trim(new_outdir_h)))
    call CreateFolder(adjustl(trim(new_outdir_d)))
    call CreateFolder(adjustl(trim(new_outdir_m)))

    if (do_spinup)then
        new_outDir_spinup = adjustl(trim(new_outdir))//"/"//adjustl(trim(outDir_spinup))
        call CreateFolder(adjustl(trim(new_outDir_spinup)))
    endif

    outFile_restart = adjustl(trim(outdir))//"/restart.nc"

end subroutine createNewCase

subroutine CreateFolder(path_new)
    implicit none
    character(len=*), INTENT(in) :: path_new
    character (len=:), allocatable :: cmdChar
    logical :: dirExists
    ! ----------------------------------------------------
    allocate(character(len=6+len(path_new)) :: cmdChar)
    cmdChar = "mkdir "//path_new
    inquire( file=trim(path_new)//'/.', exist=dirExists )  ! Works with gfortran, but not ifort
    ! inquire( directory=newDirPath, exist=dirExists )         ! Works with ifort, but not gfortran
    if (.not. dirExists) call system(cmdChar)
    deallocate(cmdChar)
end subroutine CreateFolder