module mod_mcmc
    use driver
    use mod_data
    implicit none
    integer npar4DA, nDAsimu, do_cov4newpar, covexist
    real, allocatable :: DAparmin(:), DAparmax(:),  DAparidx(:),  DApar(:), DApar_old(:)
    real, allocatable :: MDparval(:), gamma(:,:), gamnew(:,:)
    real seach_scale, fact_rejet


    contains
    subroutine init_mcmc(npar, parmin, parmax, parval)!, df_search_scale)
        implicit none
        integer npar, ipar
        real, dimension(npar) :: parmin, parmax, parval
        real, allocatable :: temp_parmin(:), temp_parmax(:), temp_paridx(:), temp_parval(:)
        ! real df_search_scale

        allocate(temp_parmin(npar), temp_parmax(npar))
        allocate(temp_paridx(npar), temp_parval(npar))
        allocate(MDparval(npar))        ! record the parameters set for model simulation

        MDparval = parval
        npar4DA  = 0 ! record the number of parameters for data assimilation
        do ipar in 1, npar
            if (parmin(ipar) .ne. parmax(ipar)) then
                npar4DA              = npar4DA + 1
                temp_paridx(npar4DA) = ipar
                temp_parmin(npar4DA) = parmin(ipar)
                temp_parmax(npar4DA) = parmax(ipar)
                temp_parval(npar4DA) = parval(ipar)
            endif
        enddo

        allocate(DAparmin(npar4DA), DAparmax(npar4DA), DAparidx(npar4DA))
        allocate(DApar(npar4DA),    DApar_old(npar4DA))

        DAparmin  = temp_parmin(:npar4DA)
        DAparmax  = temp_parmax(:npar4DA)
        DAparidx  = temp_paridx(:npar4DA)
        DApar     = temp_parval(:npar4DA)
        DApar_old = DApar                   ! mark as old parameters

        deallocate(temp_parmin, temp_parmax, temp_parval, temp_paridx)

        search_scale  = 0.05                 ! df_search_scale
        nDAsimu       = 50000                ! how many time to do data assimilation
        covexist      = 0
        do_cov4newpar = 1
        fact_rejet    = 2.4/sqrt(real(npar4DA))

    end subroutine init_mcmc

    subroutine check_mcmc()
    ! deallocate some variables and summary the information of MCMC
        implicit none
        deallocate(DAparmin, DAparmax, DAparidx, DApar, DApar_old, MDparval)
    end subroutine check_mcmc

    subroutine run_mcmc()
        implicit none
        integer iDAsimu, ipar ! iDAsimu: ith simu of DA
        integer temp_upgraded, upgraded
        real rand
        integer, parameter :: nc = 100, ncov = 500

        write(*,*)"Start to run mcmc ..."

        do iDAsimu = 1, nDAsimu
            ! generate parameters 
            call generate_newPar()
            ! update the parameters
            do ipar = 1, npar4DA
                parval(DAparidx(ipar)) = DApar(ipar)
            enddo
            ! call update parameters

            ! run the model
            call teco_simu()
            
            temp_upgraded = upgraded
            call costFuncObs()

            if (upgraded .gt. temp_upgraded) then
                new =  new + 1  ! new is for what?
                if (covexist .eq. 1)then
                    coefac = coefnorm
                    coefhistory(new, :) = coefnorm
                else
                    coefac = coef
                    do ipar = 1, npar4DA
                        coefnorm(ipar) = (coef(ipar)-coefmin(ipar))/(coefmax(ipar)-coefmin(ipar))
                    enddo
                endif
                coefhistory(new, :) = coefnorm 
                if(new .ge. ncov)new=0

                if(upgraded .gt. 1500 .and. k3 .lt. 800) then ! k3 is for what?
                    call random_number(rand)  ! get a rand number
                    if(rand .gt. 0.95) then
                        k3 = k3 + 1
                        if (do_methane_da) then

                        elseif (do_da)then
                        
                        endif

                    endif

                endif
            else
                reject = reject + 1
            endif
            ! return the initial value after each run
            ! fwsoil  = fwsoil_initial
            ! topfws  = topfws_initial
            ! omega   = omega_initial
            ! wcl     = wcl_initial
            ! Storage = Storage_initial
            ! nsc     = nsc_initial
            ! QC      = QC_initial
            ! Maybe need to add other method to return the initial values

            ! updates of the covariance matrix
            if (covexist .eq. 0 .and. mod(upgraded, ncov).eq.0 .and. upgraded .ne. 0)then
                covexist = 1
                coefac   = coefnorm ! coefnorm: normized values between min and max values
                call varcov(coefhistory, gamnew, npar4DA, ncov) !
                if(.not.(all(gamnew==0.)))then
                    gamma = gamnew
                    call racine_mat(gamma, gamnew, npar4DA)
                    gamma=gamnew
                endif
            endif

            if(mod(upgraded, ncov).eq.0 .and. covexist.eq.1 .and. upgraded .ne. 0)then
                call varcov(coefhistory, gamnew, npar4DA, ncov)
                if(.not.(all(gamnew==0.)))then
                    gamma = gamnew
                    call racine_mat(gamma, gamnew, npar4DA)
                    gamma = gamnew
                endif
            endif
        enddo

        ! deallocate
        deallocate(DAparmin)
        deallocate(DAparmax)
        deallocate(DAparidx)
        deallocate(DApar)
    end subroutine run_mcmc

    subroutine read_obs()
        implicit none
        ! write(*,*)dfsdf
    end subroutine read_obs

    subroutine generate_newPar()
        implicit none
        integer igenPar, parflag
        real rand_harvest, rand

        DApar_old = DApar               ! mark as old parameters   
        
        if (do_cov4newpar .eq. 1)then 
            write(*,*) "run covexist ..."
            if (covexist .eq. 1)then
                parflag = 1                 ! mark            
                do while(parflag .gt. 0)
                    call gengaussvect(fact_rejet*gamma, coefac, coefnorm, npar4DA)          ! generate the new cov parameters
                    parflag = 0                                                         
                    do igenPar = 1, npar4DA                                                 ! check the cov 
                        if(coefnorm(igenPar).lt.0. .or. coefnorm(k1).gt.1.)then
                            parflag=parflag+1
                            write(*,*)'out of range',parflag
                        endif
                    enddo
                enddo
                do k1 = 1, npar4DA
                    DApar(k1) = coefmin(k1) + coefnorm(k1) * (coefmax(k1)-coefmin(k1))
                enddo
            else
                write (*,*) "covexist=0"
                do igenPar = 1, npar4DA     ! for each parameters
999                 continue
                    call random_number(rand_harvest)    
                    rand = rand_harvest - 0.5           ! create a random number in [-0.5, 0.5]
                    DApar(igenPar) = DApar_old(igenPar) + rand*(DAparmin(igenPar)) * search_scale   ! create new parameter
                    if((DApar(igenPar) .gt. DAparmax(i)) &
                        &   .or. (DApar(igenPar) .lt. DAparmin(igenPar))) goto 999                  ! judge the range of new parameter
                enddo
            endif
        endif
        return
    end subroutine generate_newPar

    subroutine generate_newPar_cov()
        implicit none
        integer parflag
        fact_rejet = 2.4/squrt
        do while(parflag .gt. 0)
            call gengaussvect()
        enddo
    end subroutine generate_newPar_cov

    subroutine gengaussvect(gamma_racine,xold,xnew,npara)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Generation of a random vector from a multivariate  !!
    !! normal distribution with mean zero and covariance  !!
    !! matrix gamma.									  !!
    !! Beware!!! In order to improve the speed of the	  !!
    !! algorithms, the subroutine use the Square root	  !!
    !! matrix of gamma									  !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer npara
        real gamma_racine(npara,npara)
        real x(npara),xold(npara),xnew(npara)
        
        do i=1,npara
            x(i)=rangauss(25)
        enddo
        
        x = matmul(gamma_racine, x)
        xnew = xold + x
    end subroutine gengaussvect

    function rangauss(idum)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Generation of a random number from a standard	  !!
    !! normal distribution. (Numerical Recipes)           !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer idum
        real v1, v2, r, fac, gset
        real r_num
        
        data iset/0/
        if(iset==0) then
1	        CALL random_number(r_num)
            v1=2.*r_num-1
            CALL random_number(r_num)
            v2=2.*r_num-1
            r=(v1)**2+(v2)**2
            if(r>=1) go to 1
            fac=sqrt(-2.*log(r)/r)
            gset=v1*fac
            rangauss=v2*fac
            iset=1
        else
            rangauss=gset
            iset=0
        end if
        return
    end function

    subroutine varcov(tab,varcovar,npara,ncov)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! variance matrix of a matrix of data				  !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer npara,ncov
        real tab(ncov,npara),tab2(ncov,npara)
        real varcovar(npara,npara)
        
        call centre(tab,tab2,npara,ncov)
        
        varcovar = matmul(transpose(tab2), tab2)*(1./real(ncov))
        
        end subroutine varcov



    subroutine centre(mat,mat_out,npara,ncov)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Compute the centered matrix, ie. the matrix minus  !!
    !! the column means									  !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer npara,ncov
        real mat(ncov,npara),mat_out(ncov,npara)
        real mean

        do i=1,npara
            mat_out(:,i) = mat(:,i) - mean(mat(:,i),ncov)
        enddo

    end subroutine centre

    Function mean(tab,ncov)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! mean of a vector									  !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer ncov
        real tab(ncov)
        real mean,mean_tt
        mean_tt=0.
        do i=1,ncov
        mean_tt=mean_tt+tab(i)/real(ncov)
        enddo
        mean=mean_tt
    End Function
    
end module mod_mcmc