program TECO
    use mod_data
    use mod_spinup
    use mod_mcmc
    use driver
    use mod_ncd_io

    implicit none

    call read_TECO_model_configs()  ! get the file of "TECO_model_configs.nml", including parameters
    
    call createNewCase()            ! update the in-out path and create the revelent ouput paths

    if (do_spinup) call init_spinup_variables() ! initilize the spin-up variables

    call get_forcingdata()                      ! read forcing data
    nHours  = nforcing                          
    nDays   = int(nHours/24.)
    nYears  = int(nforcing/(365*24))
    nMonths = nYears*12

    if (.not. do_snow) call get_snowdepth()

    call assign_all_results(nHours, nDays, nMonths, nYears) 
    
    call initialize()                      ! initializations

    if (do_restart)then
        call read_restart(restartfile)     ! this module in "writeOutput2nc.f90"
        call initialize_with_restart()
    endif

    if (do_spinup) then
        call run_spinup()
        call write_spinup_res()
        call deallo_spinup_variables()
    endif 

    if (do_mcmc) then
        call init_mcmc()
        call run_mcmc()
        call check_mcmc()
    endif
    
    call teco_simu()                    ! run simulation
    call spruce_mip_cmip6Format()       
    call write_restart()
    ! end of the simulation, then deallocate the forcing_data
    call deallocate_date_type()
    call deallocate_all_results()
end program TECO

subroutine createNewCase()
    use mod_data
    implicit none
    ! create a new case to run the TECO model
    !   * create the output path

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
        outDir_spinup = adjustl(trim(outdir))//"/"//adjustl(trim(outDir_spinup))
        call CreateFolder(adjustl(trim(outDir_spinup)))
        outfile_restart = adjustl(trim(outDir_spinup))//"/restart.nc"
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
        print *, whatfile," exists: ", filepath
    else
        print *, whatfile," does not exist: ", filepath
        stop
    end if
end subroutine check_inputfile