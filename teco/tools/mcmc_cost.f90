module mcmc_cost
    use mod_data
    implicit none
    type CostVariables
    ! default variables, you can add the variable names here. (year, doy, hour)
        ! carbon flux 
        real, allocatable :: gpp_d(:,:) 
        real, allocatable :: nee_d(:,:)
        real, allocatable :: reco_d(:,:)
        real, allocatable :: gpp_h(:,:)
        real, allocatable :: nee_h(:,:)
        real, allocatable :: reco_h(:,:)
        ! methane
        real, allocatable :: ch4_h(:,:)
        ! c pools
        real, allocatable :: cleaf(:,:) ! foliage
        real, allocatable :: cwood(:,:)
    end type CostVariables

    contains
    subroutine GetObsData()
        implicit none

    end subroutine GetObsData

    subroutine GetSimuData()
    
    end subroutine GetSimuData

    subroutine CalculateLine

    subroutine ReadObsDataFromFile(filepath, resData, num_lines)
        implicit none
        character(len=*), intent(in) :: filepath
        character(len=200) commts
        integer STAT, num_lines

        ! year, doy, hour, value, std
        OPEN(34, FILE=trim(filepath), status='old', ACTION='read', IOSTAT=STAT)
        read(1,'(a200)') commts
        num_lines = 0
        do while(.TRUE.)
            num_lines = num_lines + 1
            read(34,*,IOSTAT=STAT) lines
            if(STAT .ne. 0) EXIT
        end do
        
        read(34, '(a160)') commts
        do while (.TRUE.)
            read(34,*,iostat=STAT) line

        end do
    end subroutine ReadObsDataFromFile

end module mcmc_tools