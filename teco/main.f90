program TECO
    use mod_data
    use mod_spinup
    use mod_mcmc
    use driver
    use mod_ncd_io
    use mcmc_functions

    implicit none
    integer :: count_mode 
    character(50) :: str_mode
    
    print *, ""
    write(*,*) "# -----------------------------------------"
    call read_TECO_model_configs()  ! get the file of "TECO_model_configs.nml", including parameters

    ! check the three mode: do_simu; do_mcmc; do_spinup
    count_mode = 0
    if (do_simu)   count_mode = count_mode + 1
    if (do_mcmc)   count_mode = count_mode + 1
    if (do_spinup) count_mode = count_mode + 1

    if (count_mode .eq. 0) then
        print *, "# You must choose a run mode."
        print *, "# Please make sure one of the three modes is True in file of TECO_model_configs.nml"
        print *, "#    *do_simu; *do_mcmc; *do_spinup"
        print *, ""
        stop
    elseif (count_mode .gt. 1) then
        print *, "# You can only select one mode out of the three, please check your file."
        print *, "# Please check the file of TECO_model_configs.nml"
        print *, "#    *do_simu; *do_mcmc; *do_spinup"
        print *, ""
        stop
    else
        continue
    endif

    write(*,*) "# Start to run the case of """, adjustl(trim(simu_name)), """"
    ! update the in-out path and create the revelent ouput paths
    call createNewCase() 

    call get_forcingdata()                      ! read forcing data
    nHours  = nforcing                          
    nDays   = int(nHours/24.)
    nYears  = int(nforcing/(365*24))
    nMonths = nYears*12
    call assign_all_results(nHours, nDays, nMonths, nYears) 

    if (.not. do_snow) call get_snowdepth()
    call initialize()
    if (do_restart)then
        call read_restart(restartfile)     ! this module in "writeOutput2nc.f90"
        call initialize_with_restart()
    endif
    
    if(do_simu)then
        print *, "# Start to run simulation mode."
        call teco_simu()                    ! run simulation
        call spruce_mip_cmip6Format()  
    elseif(do_spinup)then
        call init_spinup_variables()    ! initilize the spin-up variables
        call run_spinup()               ! run spin-up loops
        call write_spinup_res()         ! write the results of SPIN-UP
        call write_restart()            ! write the result file
        call deallo_spinup_variables()  ! deallocate the variables of SPIN-UP
    elseif(do_mcmc) then
        call init_mcmc()                ! initilize the MCMC 
        call run_mcmc()                 ! run MCMC
        call deallocate_mcmc()          ! deallocate the MCMC variables 
    endif
     
    ! end of the simulation, then deallocate the forcing_data
    call deallocate_date_type()
    call deallocate_all_results()
end program TECO

subroutine createNewCase()
    use mod_data
    use mcmc_functions
    implicit none
    ! create a new case to run the TECO model
    !   * create the output path

    print *, "# Update and create the output dirs"

    ! update the full path of input file
    climfile        = adjustl(trim(filepath_in))//"/"//adjustl(trim(climfile))       ! climate file name
    snowdepthfile   = adjustl(trim(filepath_in))//"/"//adjustl(trim(snowdepthfile))  ! snow depthfile
    restartfile     = adjustl(trim(filepath_in))//"/"//adjustl(trim(restartfile))    ! restartfile
    watertablefile  = adjustl(trim(filepath_in))//"/"//adjustl(trim(watertablefile)) ! Jian: maybe used when not run soil_physical
    ! check the inputfile
    call check_inputfile(climfile, "climate file")
    if (.not. do_snow)    call check_inputfile(snowdepthfile,  "The file of snowdepth")
    if (do_restart)       call check_inputfile(restartfile,    "The file of restart")
    if (.not. do_soilphy) call check_inputfile(watertablefile, "The file of water table")
    ! create the outdir
    call CreateFolder(adjustl(trim(outdir)))

    ! update and create the output dir of case
    outdir_case = adjustl(trim(outdir))//"/"//adjustl(trim(simu_name))
    call CreateFolder(adjustl(trim(outdir_case)))

    ! update and create the output dir of each format outputs
    outDir_nc  = adjustl(trim(outdir_case))//"/"//adjustl(trim(outDir_nc))
    call CreateFolder(adjustl(trim(outDir_nc)))
    outDir_csv = adjustl(trim(outdir_case))//"/"//adjustl(trim(outDir_csv))
    call CreateFolder(adjustl(trim(outDir_csv)))

    ! update and create the output for each time frequency of nc-format outputs
    outDir_h = adjustl(trim(outdir_nc))//"/"//adjustl(trim(outDir_h))
    outDir_d = adjustl(trim(outdir_nc))//"/"//adjustl(trim(outDir_d))
    outDir_m = adjustl(trim(outdir_nc))//"/"//adjustl(trim(outDir_m))
    outDir_y = adjustl(trim(outdir_nc))//"/"//adjustl(trim(outDir_y))
    
    call CreateFolder(adjustl(trim(outDir_h)))
    call CreateFolder(adjustl(trim(outDir_d)))
    call CreateFolder(adjustl(trim(outDir_m)))
    call CreateFolder(adjustl(trim(outDir_y)))

    if (do_spinup)then
        outDir_spinup = adjustl(trim(outdir_case))//"/"//adjustl(trim(outDir_spinup))
        call CreateFolder(adjustl(trim(outDir_spinup)))
        outfile_restart = adjustl(trim(outDir_spinup))//"/restart.nc"
    endif

    if (do_mcmc)then
        outDir_mcmc = adjustl(trim(outdir_case))//"/"//adjustl(trim(outDir_mcmc))
        call CreateFolder(adjustl(trim(outDir_mcmc)))
        if (do_mc_out_hr)then
            outDir_mcmc_h = adjustl(trim(outDir_mcmc))//"/"//adjustl(trim(outDir_mcmc_h))
            call CreateFolder(adjustl(trim(outDir_mcmc_h)))
        endif
        if (do_mc_out_day) then
            outDir_mcmc_d = adjustl(trim(outDir_mcmc))//"/"//adjustl(trim(outDir_mcmc_d))
            call CreateFolder(adjustl(trim(outDir_mcmc_d)))
        endif
        if (do_mc_out_mon) then
            outDir_mcmc_m = adjustl(trim(outDir_mcmc))//"/"//adjustl(trim(outDir_mcmc_m))
            call CreateFolder(adjustl(trim(outDir_mcmc_m)))
        endif
    endif

    if(do_restart)then
        restartfile = adjustl(trim(filepath_in))//adjustl(trim(restartfile))
    endif
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

subroutine check_inputfile(filepath, whatfile)
    implicit none
    character(*), intent(in) :: filepath, whatfile
    logical :: file_exists

    inquire(file=filepath, exist=file_exists)
    
    if (file_exists) then
        print *, "# ", whatfile," exists: "
        print *, "#     ", filepath
    else
        print *, "# ", whatfile," does not exist: "
        print *, "#     ", filepath
        stop
    end if
end subroutine check_inputfile