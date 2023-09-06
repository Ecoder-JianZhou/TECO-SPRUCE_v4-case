module mod_mcmc
    use driver
    use mod_data
    use mcmc_functions
    use MCMC_outputs

    implicit none

    ! integer nDAsimu
    ! real search_scale

    integer npar4DA, iDAsimu, upgraded, ipar, covexist
    real, allocatable :: DAparmin(:), DAparmax(:), DApar(:), DApar_old(:)
    integer, allocatable :: DAparidx(:)
    real, allocatable :: MDparval(:), gamma(:,:), gamnew(:,:), coefhistory(:,:), coefnorm(:), coefac(:)
    real fact_rejet
    real J_last, J_new, accept_rate
    integer new, reject
    logical do_cov2createNewPars
    ! integer, parameter :: nc = 100, ncov = 500

    contains
    subroutine init_mcmc()
        implicit none
        real, allocatable :: temp_parmin(:), temp_parmax(:), temp_parval(:)
        integer, allocatable :: temp_paridx(:)

        npar     = nSpecParams

        ! read the nml file of MCMC configs (eg. TECO_MCMC_configs.nml)
        call readConfsNml()
        ! read the observational data
        call readObsData() ! return a type array of vars4MCMC

        ! handle the parameters for MCMC
        allocate(temp_parmin(npar), temp_parmax(npar))  ! allocate the temporary parmin value
        allocate(temp_paridx(npar), temp_parval(npar))  ! mark the index of parameters for MCMC
        allocate(MDparval(npar))                        ! record the parameters set for model simulation

        MDparval = parval                               ! parameters for running model
        npar4DA  = 0 ! record the number of parameters for data assimilation
        do ipar = 1, npar
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

        ! give some values to the parameters for MCMC
        covexist      = 0
        fact_rejet    = 2.4/sqrt(real(npar4DA))

        ! record
        allocate(coefhistory(ncov, npar4DA))
        ! create the coefnorm for generating the new parameters
        allocate(coefnorm(npar4DA)) 
        allocate(coefac(npar4DA))
        do ipar = 1, npar4DA
            coefnorm(ipar) = 0.5
            coefac(ipar)   = coefnorm(ipar)
        enddo
        allocate(gamnew(npar4DA, npar4DA))
        J_last = 900000.0
        ! init the outputs
        call init_mcmc_outputs(nDAsimu, npar4DA)
    end subroutine init_mcmc

    subroutine run_mcmc()
        implicit none
        integer temp_upgraded
        real rand
        
        print *, "# Start to run mcmc ..."

        call generate_newPar()

        do iDAsimu = 1, nDAsimu
            write(*,*) iDAsimu, "/", nDAsimu, J_last, J_new, upgraded, accept_rate
            call mcmc_functions_init()  ! initialize the mc_itime ... variables
            call initialize()           ! initialize the TECO model 
            ! generate parameters 
            call generate_newPar()
            ! update the parameters
            do ipar = 1, npar4DA
                parval(DAparidx(ipar)) = DApar(ipar)
            enddo
            
            call renewMDpars()          ! call update parameters in TECO model

            call teco_simu()            ! run the model
            
            temp_upgraded = upgraded
            call costFuncObs()          ! calculate the cost between observations and simulations

            ! if upgraded is updated in costFuncObs
            if (upgraded .gt. temp_upgraded) then
                new =  new + 1  ! new is for what?
                if (covexist .eq. 1)then
                    coefac = coefnorm                           ! coefac is old parameter sets? coef is the new one; coefnorm 
                    coefhistory(new, :) = coefnorm              ! coefhistory used to create new coef matrix
                else
                    do ipar = 1, npar4DA
                        coefnorm(ipar) = (DApar(ipar)-DAparmin(ipar))/(DAparmax(ipar)-DAparmin(ipar))
                    enddo
                endif
                coefhistory(new, :) = coefnorm 
                if(new .ge. ncov)new=0
                ! update the parameters sets
                tot_paramsets(upgraded,:) = DApar
                if (do_mc_out_hr) then
                    call mcmc_update_outputs(upgraded, tot_paramsets_outs_h, tot_outVars_h)
                endif
                if (do_mc_out_day)then
                    call mcmc_update_outputs(upgraded, tot_paramsets_outs_d, tot_outVars_d)
                endif
                if (do_mc_out_mon) then
                    call mcmc_update_outputs(upgraded, tot_paramsets_outs_m, tot_outVars_m)
                endif
            else
                reject = reject + 1
            endif

            ! updates of the covariance matrix
            if (.not. do_cov2createNewPars .and. mod(upgraded, ncov).eq.0 .and. upgraded .ne. 0)then
                do_cov2createNewPars = .True.
                coefac   = coefnorm          ! coefnorm: normized values between min and max values
                call varcov(coefhistory, gamnew, npar4DA, ncov) !
                if(.not.(all(gamnew==0.)))then
                    gamma = gamnew
                    call racine_mat(gamma, gamnew, npar4DA)
                    gamma = gamnew
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

        ! summary
        call mcmc_param_outputs(upgraded, npar4DA, parnames, DAparidx)

        ! deallocate
        deallocate(DAparmin)
        deallocate(DAparmax)
        deallocate(DAparidx)
        deallocate(DApar)
    end subroutine run_mcmc

    subroutine generate_newPar()
        ! This subroutine is used to generate the new parameters to run MCMC
        ! Based on the Shuang's code, it need to use the coef to generate the new parameters.
        implicit none
        integer igenPar, parflag
        real rand_harvest, rand

        DApar_old = DApar                   ! mark as old parameters 
        
        if (do_cov2createNewPars) then
            parflag = 1                 ! mark
            do while(parflag .gt. 0)    ! create the new coefnorm
                ! create the new coefnorm based on old one of coefac
                call gengaussvect(fact_rejet*gamma, coefac, coefnorm, npar4DA)          ! generate the new cov parameters
                parflag = 0
                do igenPar = 1, npar4DA                                                 ! check the cov 
                    if(coefnorm(igenPar).lt.0. .or. coefnorm(igenPar).gt.1.)then
                        parflag=parflag+1
                        write(*,*)'out of range',parflag
                    endif
                enddo
            enddo
            ! create the new parameters from 
            do ipar = 1, npar4DA
                DApar(ipar) = DAparmin(ipar) + coefnorm(ipar) * (DAparmax(ipar)-DAparmin(ipar))
            enddo
        else ! do not run cov to create new parameters, just random selections
            do igenPar = 1, npar4DA     ! for each parameters
999             continue
                call random_number(rand_harvest)    
                rand = rand_harvest - 0.5           ! create a random number in [-0.5, 0.5]
                DApar(igenPar) = DApar_old(igenPar) + rand*(DAparmax(igenPar) - DAparmin(igenPar)) * search_scale   ! create new parameter
                if((DApar(igenPar) .gt. DAparmax(igenPar)) &
                    &   .or. (DApar(igenPar) .lt. DAparmin(igenPar))) then 
                    goto 999                  ! judge the range of new parameter
                endif
            enddo
        endif
        return   ! mainly return the DApar, meanwhile update the coefnorm
    end subroutine generate_newPar

    subroutine costFuncObs()
        implicit none
        real J_cost, delta_J, cs_rand
        J_new = 0
        ! vars4MCMC
        ! gpp_d
        if(vars4MCMC%gpp_d%existOrNot)then
            ! write(*,*) "testMD: ", vars4MCMC%gpp_d%mdData(:,4)
            ! write(*,*) "testOD: ", vars4MCMC%gpp_d%obsData(:,4)
            call CalculateCost(vars4MCMC%gpp_d%mdData(:,4), vars4MCMC%gpp_d%obsData(:,4),&
                 vars4MCMC%gpp_d%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! nee_d
        if(vars4MCMC%nee_d%existOrNot)then
            call CalculateCost(vars4MCMC%nee_d%mdData(:,4), vars4MCMC%nee_d%obsData(:,4),&
                 vars4MCMC%nee_d%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! reco_d
        if(vars4MCMC%reco_d%existOrNot)then
            call CalculateCost(vars4MCMC%reco_d%mdData(:,4), vars4MCMC%reco_d%obsData(:,4),&
                 vars4MCMC%reco_d%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! gpp_h
        if(vars4MCMC%gpp_h%existOrNot)then
            call CalculateCost(vars4MCMC%gpp_h%mdData(:,4), vars4MCMC%gpp_h%obsData(:,4),&
                 vars4MCMC%gpp_h%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! write(*,*) "here3",J_new
        ! nee_h
        if(vars4MCMC%nee_h%existOrNot)then
            call CalculateCost(vars4MCMC%nee_h%mdData(:,4), vars4MCMC%nee_h%obsData(:,4),&
                 vars4MCMC%nee_h%obsData(:,5), J_cost)
            J_new = J_new + J_cost*100
        endif
        ! write(*,*) "here4",J_new
        ! reco_h
        if(vars4MCMC%reco_h%existOrNot)then
            ! write(*,*)vars4MCMC%reco_h%filepath
            ! write(*,*)vars4MCMC%reco_h%mdData(:,4)
            ! write(*,*)vars4MCMC%reco_h%obsData(:,4)
            ! write(*,*)vars4MCMC%reco_h%obsData(:,5)
            ! stop
            call CalculateCost(vars4MCMC%reco_h%mdData(:,4), vars4MCMC%reco_h%obsData(:,4),&
                 vars4MCMC%reco_h%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! write(*,*) "here5",J_new
        ! ch4_h
        ! write(*,*)vars4MCMC%ch4_h%mdData(:,4)
        !     write(*,*)vars4MCMC%ch4_h%obsData(:,4)
        !     write(*,*)vars4MCMC%ch4_h%obsData(:,5)
        if(vars4MCMC%ch4_h%existOrNot)then
            call CalculateCost(vars4MCMC%ch4_h%mdData(:,4), vars4MCMC%ch4_h%obsData(:,4),&
                 vars4MCMC%ch4_h%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! write(*,*) "here6",J_new
        ! cleaf
        if(vars4MCMC%cleaf%existOrNot)then
            call CalculateCost(vars4MCMC%cleaf%mdData(:,4), vars4MCMC%cleaf%obsData(:,4),&
                 vars4MCMC%cleaf%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! write(*,*) "here7",J_new
        ! cwood
        if(vars4MCMC%cwood%existOrNot)then
            call CalculateCost(vars4MCMC%cwood%mdData(:,4), vars4MCMC%cwood%obsData(:,4),&
                 vars4MCMC%cwood%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! write(*,*) "here1",J_new
        ! anpp_y
        ! write(*,*)vars4MCMC%anpp_y%mdData(:,4)
        !     write(*,*)vars4MCMC%anpp_y%obsData(:,4)
        !     write(*,*)vars4MCMC%anpp_y%obsData(:,5)
        if(vars4MCMC%anpp_y%existOrNot)then
            call CalculateCost(vars4MCMC%anpp_y%mdData(:,4), vars4MCMC%anpp_y%obsData(:,4),&
                 vars4MCMC%anpp_y%obsData(:,5), J_cost)
            J_new = J_new + J_cost/2000
        endif
        ! write(*,*) "here10",J_new

        ! bnpp_y
        if(vars4MCMC%bnpp_y%existOrNot)then
            call CalculateCost(vars4MCMC%bnpp_y%mdData(:,4), vars4MCMC%bnpp_y%obsData(:,4),&
                 vars4MCMC%bnpp_y%obsData(:,5), J_cost)
            J_new = J_new + J_cost/100
        endif
        ! write(*,*) "here9",J_new
        ! lai_h
        if(vars4MCMC%lai_h%existOrNot)then
            call CalculateCost(vars4MCMC%lai_h%mdData(:,4), vars4MCMC%lai_h%obsData(:,4),&
                 vars4MCMC%lai_h%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! write(*,*) "here8",J_new
        ! npp_y
        ! write(*,*)vars4MCMC%npp_y%mdData(:,4)
        !     write(*,*)vars4MCMC%npp_y%obsData(:,4)
        !     write(*,*)vars4MCMC%npp_y%obsData(:,5)
        if(vars4MCMC%npp_y%existOrNot)then
            call CalculateCost(vars4MCMC%npp_y%mdData(:,4), vars4MCMC%npp_y%obsData(:,4),&
                 vars4MCMC%npp_y%obsData(:,5), J_cost)
            J_new = J_new + J_cost/1000
        endif
        ! write(*,*) "here7",J_new
        ! reco_y
        if(vars4MCMC%reco_y%existOrNot)then
            call CalculateCost(vars4MCMC%reco_y%mdData(:,4), vars4MCMC%reco_y%obsData(:,4),&
                 vars4MCMC%reco_y%obsData(:,5), J_cost)
            J_new = J_new + J_cost
        endif
        ! write(*,*) "here2",J_new
        if(J_new .eq. 0) then ! no data is available
            delta_J = -0.1
        else
            delta_J = J_new - J_last
        endif

        delta_J = delta_J

        call random_number(cs_rand)
        if(AMIN1(1.0, exp(-delta_J)) .gt. cs_rand)then
            upgraded = upgraded + 1
            J_last = J_new
        endif
        accept_rate = real(upgraded)/real(iDAsimu)
    end subroutine costFuncObs

    subroutine CalculateCost(datMod4MCMC, datObs4MCMC, stdObs4MCMC, JCost)
        ! calculate the cost of the observation and simulation, and update the number of updated
        implicit none
        real, intent(in) :: datMod4MCMC(:), datObs4MCMC(:), stdObs4MCMC(:)
        integer nLine, iLine, nCost
        real JCost, dObsSimu, std4cal

        nLine = size(datObs4MCMC)
        nCost = 0
        JCost = 0.

        do iLine = 1, nLine
            if(datObs4MCMC(iLine) .gt. -999 .and. datMod4MCMC(iLine) .gt. -999)then
                nCost    = nCost + 1   
                dObsSimu = datMod4MCMC(iLine) - datObs4MCMC(iLine) 
                if (stdObs4MCMC(iLine) < 0.001) then 
                    std4cal = 0.5
                else
                    std4cal = stdObs4MCMC(iLine)
                endif
                JCost    = JCost + (dObsSimu*dObsSimu)/(2*std4cal)
            endif
        enddo
        if(nCost .gt. 0) JCost=JCost/real(nCost)
        return ! JCost
    end subroutine CalculateCost

    subroutine racine_mat(M, Mrac,npara)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Square root of a matrix							  !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer npara,i, nrot
        real M(npara,npara),Mrac(npara,npara)
        real valpr(npara),vectpr(npara,npara)
        Mrac=0.
        call jacobi(M,npara,npara,valpr,vectpr,nrot)
        do i=1,npara
        if(valpr(i).ge.0.) then
                Mrac(i,i)=sqrt(valpr(i))
        else
                print*, 'WARNING!!! Square root of the matrix is undefined.'
                print*, ' A negative eigenvalue has been set to zero - results may be wrong'
                Mrac=M
                return
        endif
        enddo
        Mrac=matmul(matmul(vectpr, Mrac),transpose(vectpr))

    end subroutine racine_mat

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Extraction of the eigenvalues and the eigenvectors !!
    !! of a matrix (Numerical Recipes)					  !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE jacobi(a,n,np,d,v,nrot)
        INTEGER :: n,np,nrot
        REAL :: a(np,np),d(np),v(np,np)
        INTEGER, PARAMETER :: NMAX=500
        INTEGER :: i,ip,iq,j
        REAL :: c,g,h,s,sm,t,tau,theta,tresh,b(NMAX),z(NMAX)
        
        do ip=1,n
            do iq=1,n
                v(ip,iq)=0.
            end do
            v(ip,ip)=1.
        end do
        
        do ip=1,n
            b(ip)=a(ip,ip)
            d(ip)=b(ip)
            z(ip)=0.
        end do
        
        nrot=0
        do i=1,50
            sm=0.
            do ip=1,n-1
                do iq=ip+1,n
                    sm=sm+abs(a(ip,iq))
                end do
            end do
            if(sm.eq.0.)return
            if(i.lt.4)then
                tresh=0.2*sm/n**2
            else
                tresh=0.
            endif
            do ip=1,n-1
                do iq=ip+1,n
                    g=100.*abs(a(ip,iq))
                    if((i.gt.4).and.(abs(d(ip))+g.eq.abs(d(ip))).and.(abs(d(iq))+g.eq.abs(d(iq))))then
                        a(ip,iq)=0.
                    else if(abs(a(ip,iq)).gt.tresh)then
                        h=d(iq)-d(ip)
                        if(abs(h)+g.eq.abs(h))then
                            t=a(ip,iq)/h
                        else
                            theta=0.5*h/a(ip,iq)
                            t=1./(abs(theta)+sqrt(1.+theta**2))
                            if(theta.lt.0.) then
                                t=-t
                            endif
                        endif
                        c=1./sqrt(1+t**2)
                        s=t*c
                        tau=s/(1.+c)
                        h=t*a(ip,iq)
                        z(ip)=z(ip)-h
                        z(iq)=z(iq)+h
                        d(ip)=d(ip)-h
                        d(iq)=d(iq)+h
                        a(ip,iq)=0.
                        do j=1,ip-1
                            g=a(j,ip)
                            h=a(j,iq)
                            a(j,ip)=g-s*(h+g*tau)
                            a(j,iq)=h+s*(g-h*tau)
                        end do
                        do j=ip+1,iq-1
                            g=a(ip,j)
                            h=a(j,iq)
                            a(ip,j)=g-s*(h+g*tau)
                            a(j,iq)=h+s*(g-h*tau)
                        end do
                        do j=iq+1,n
                            g=a(ip,j)
                            h=a(iq,j)
                            a(ip,j)=g-s*(h+g*tau)
                            a(iq,j)=h+s*(g-h*tau)
                        end do
                        do j=1,n
                            g=v(j,ip)
                            h=v(j,iq)
                            v(j,ip)=g-s*(h+g*tau)
                            v(j,iq)=h+s*(g-h*tau)
                        end do
                        nrot=nrot+1
                    endif
                end do
            end do
            do ip=1,n
                b(ip)=b(ip)+z(ip)
                d(ip)=b(ip)
                z(ip)=0.
            end do
        end do
        print*, 'too many iterations in jacobi'
        return
    END subroutine jacobi

    subroutine gengaussvect(gamma_racine,xold,xnew,npara)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Generation of a random vector from a multivariate  !!
    !! normal distribution with mean zero and covariance  !!
    !! matrix gamma.									  !!
    !! Beware!!! In order to improve the speed of the	  !!
    !! algorithms, the subroutine use the Square root	  !!
    !! matrix of gamma									  !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer npara, i
        real gamma_racine(npara,npara)
        real x(npara),xold(npara),xnew(npara)
        
        do i=1,npara
            x(i)=rangauss(25)
        enddo
        
        x = matmul(gamma_racine, x)
        xnew = xold + x
    end subroutine gengaussvect

   real function rangauss(idum)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! Generation of a random number from a standard	  !!
    !! normal distribution. (Numerical Recipes)           !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer idum
        real v1, v2, r, fac, gset
        real r_num
        integer :: iset
        
        ! data iset/0/
        iset = 0
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
        integer npara,i,ncov
        real mat(ncov,npara),mat_out(ncov,npara)
        ! real mean

        do i=1,npara
            mat_out(:,i) = mat(:,i) - mean(mat(:,i),ncov)
        enddo

    end subroutine centre

    real function mean(tab,ncov)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! mean of a vector									  !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer ncov, incov
        real tab(ncov)
        real mean_tt
        mean_tt=0.
        do incov=1,ncov
        mean_tt=mean_tt+tab(incov)/real(ncov)
        enddo
        mean=mean_tt
    End Function mean

    ! real function mean(tab, ncov)
    !     integer ncov
    !     real tab(ncov)
    ! end function mean
    
    subroutine deallocate_mcmc()
    ! deallocate some variables and summary the information of MCMC
        implicit none
        if(allocated(DAparmin)) deallocate(DAparmin)
        if(allocated(DAparmax)) deallocate(DAparmax)
        if(allocated(DAparidx)) deallocate(DAparidx)
        if(allocated(DApar)) deallocate(DApar)
        if(allocated(DApar_old)) deallocate(DApar_old)
        if(allocated(MDparval)) deallocate(MDparval)

        if(allocated(parval)) deallocate(parval)
        if(allocated(parmin)) deallocate(parmin)
        if(allocated(parmax)) deallocate(parmax)
        
        if(allocated(vars4MCMC%gpp_d%obsData))  deallocate(vars4MCMC%gpp_d%obsData)
        if(allocated(vars4MCMC%nee_d%obsData))  deallocate(vars4MCMC%nee_d%obsData)
        if(allocated(vars4MCMC%reco_d%obsData)) deallocate(vars4MCMC%reco_d%obsData)
        if(allocated(vars4MCMC%gpp_h%obsData))  deallocate(vars4MCMC%gpp_h%obsData)
        if(allocated(vars4MCMC%nee_h%obsData))  deallocate(vars4MCMC%nee_h%obsData)
        if(allocated(vars4MCMC%reco_h%obsData)) deallocate(vars4MCMC%reco_h%obsData)
        if(allocated(vars4MCMC%ch4_h%obsData))  deallocate(vars4MCMC%ch4_h%obsData)
        if(allocated(vars4MCMC%cleaf%obsData))  deallocate(vars4MCMC%cleaf%obsData)
        if(allocated(vars4MCMC%cwood%obsData))  deallocate(vars4MCMC%cwood%obsData)

        if(allocated(vars4MCMC%anpp_y%obsData))  deallocate(vars4MCMC%anpp_y%obsData)
        if(allocated(vars4MCMC%bnpp_y%obsData))  deallocate(vars4MCMC%bnpp_y%obsData)
        if(allocated(vars4MCMC%lai_h%obsData))  deallocate(vars4MCMC%lai_h%obsData)
        if(allocated(vars4MCMC%npp_y%obsData))  deallocate(vars4MCMC%npp_y%obsData)
        if(allocated(vars4MCMC%reco_y%obsData))  deallocate(vars4MCMC%reco_y%obsData)

        if(allocated(vars4MCMC%gpp_d%mdData))  deallocate(vars4MCMC%gpp_d%mdData)
        if(allocated(vars4MCMC%nee_d%mdData))  deallocate(vars4MCMC%nee_d%mdData)
        if(allocated(vars4MCMC%reco_d%mdData)) deallocate(vars4MCMC%reco_d%mdData)
        if(allocated(vars4MCMC%gpp_h%mdData))  deallocate(vars4MCMC%gpp_h%mdData)
        if(allocated(vars4MCMC%nee_h%mdData))  deallocate(vars4MCMC%nee_h%mdData)
        if(allocated(vars4MCMC%reco_h%mdData)) deallocate(vars4MCMC%reco_h%mdData)
        if(allocated(vars4MCMC%ch4_h%mdData))  deallocate(vars4MCMC%ch4_h%mdData)
        if(allocated(vars4MCMC%cleaf%mdData))  deallocate(vars4MCMC%cleaf%mdData)
        if(allocated(vars4MCMC%cwood%mdData))  deallocate(vars4MCMC%cwood%mdData)

        if(allocated(vars4MCMC%anpp_y%mdData))  deallocate(vars4MCMC%anpp_y%mdData)
        if(allocated(vars4MCMC%bnpp_y%mdData))  deallocate(vars4MCMC%bnpp_y%mdData)
        if(allocated(vars4MCMC%lai_h%mdData))  deallocate(vars4MCMC%lai_h%mdData)
        if(allocated(vars4MCMC%npp_y%mdData))  deallocate(vars4MCMC%npp_y%mdData)
        if(allocated(vars4MCMC%reco_y%mdData))  deallocate(vars4MCMC%reco_y%mdData)


        if(allocated(coefhistory)) deallocate(coefhistory)
        if(allocated(coefnorm))    deallocate(coefnorm)
        if(allocated(coefac))      deallocate(coefac)

        if(allocated(gamnew))   deallocate(gamnew)
        if(allocated(parnames)) deallocate(parnames)

        ! in MCMC_outputs module
        if(allocated(tot_paramsets)) deallocate(tot_paramsets)
        if(allocated(sel_paramsets)) deallocate(sel_paramsets)
        if (do_mc_out_hr)then
            call deallocate_mcmc_outs_type(sel_paramsets_outs_h)
            call deallocate_mcmc_outs_type(tot_paramsets_outs_h)
        endif
        if (do_mc_out_day)then
            call deallocate_mcmc_outs_type(sel_paramsets_outs_d)
            call deallocate_mcmc_outs_type(tot_paramsets_outs_d)
        endif
        if (do_mc_out_mon)then
            call deallocate_mcmc_outs_type(sel_paramsets_outs_m)
            call deallocate_mcmc_outs_type(tot_paramsets_outs_m)
        endif
    end subroutine deallocate_mcmc
end module mod_mcmc
