      subroutine diagenesis (tstart,tend)
      
       include 'common_geo.inc'
       include 'common.inc'
       include 'common_drive.inc'

       dimension spguess(ncomp,nx)	

	real*8 v_out, v_int
	real*8 tstart,tend
	real*8 lgstabserror, lgstrelerror, KBinfo_time
	integer nt, KBinfo_100
!	real*8 dtold


c***********************************************************************
c     FORWARD Reaction-Transport Code, based on operator splitting     *
c     and a reaction network created from within maple                 *
c     original version by Pierre                                       *
c     recent changes by CM:                                            *
c     - linked to optimization (interfacing)                           *
c     - included time step estimator based on CFL and Nu               *
c	- included dynamic timestepper (Parisa, Carl)                    *
c     - dealing differently with bad results                           *
c       - created warning flag                                         *
c       - set limits to catch NAN and out of bound concentration values*
c     - set depth to 0 at first node (not at dx/2)                     *
c     - changed sign of depth (i.e. distance from upper boundary)      *
c	- added depth dependent parameters (porosity, adv, disp.)        *
c	- made variable grid                                             *
c     - modified the transport matrix coefficients                     *
c       - to deal with variable grid                                   *
c       - defining fluxes at the boundary itself, not half a box inside*
c       - implemented different boundary conditions                    *
c     - also looping over boundary nodes                               *
c     - not reimposing boundary conditions after rxn                   * 
c	- strang splitting, different solvers (strang, SNIA, steady st.) *  
c     suggestions:                                                     *
c     + log transform concentrations in the reaction part              *
c                                                                      *
c     = check timestepper and interpolation                            *
c                                                                      *
c     CM, July 2002                                                    *
c***********************************************************************

c***********************************************************************
c  START OF INITIALIZATION                                             *
c  initalcond = routine prescribing the i.c. (maple)                   *
c     only used here if really starting at 0 again (i.e. time<delt)    *
c  obtain depth dependent parameters (porarea, advdiffcoeff)           *
c  this could be done in the main program only if porosity, area,      *
c  diffusion and advection velocities are not part of the optimization *
c  because I don't want to exclude this it is repeated here            *
c  however, this is only done when starting at t=0, because I assume   *
c  these properties not to vary with time (see governing equation)     *
c***********************************************************************
       if (tstart.lt.delt) then	
	   tsmaxnwcounter=0 !number of exceed newt iter in tstep; DRA
	   tsfxmax=0.0d0 !max resid funct in tstep; DRA
	   tserrmax=0.0d0 !max rel error in tstep; DRA
	   nmaxnw=0 ! max number of newt iters in one tstep; DRA
	   call porarea()
         call initialcond()
	   call advdiffcoeff()
	   time=tstart
	   call boundaries() ! added Jan'03
	! set minimum conc to 1e-20
		do j=1,nx,2
			do i=1,ncomp
				if(sp(i,j).lt.10.0d-20) sp(i,j)=10.0d-20
			enddo
		enddo		
       end if ! if (tstart.lt.delt)

	do j=1,nx,2
		write(99,*) x(j),(sp(i,j),i=1,ncomp)
	enddo
c***********************************************************************
c  STEADY STATE - TRANSIENT SELECTOR                                   *
c    if nsstate = 1: steady state calculation                          *
c    if nsstate = 2: steady state calculation, then transient          *
c    else: transient only                                              *
c    nsstate is defined in drivervalues.f, not in maple                *
c    nssnow is set to 1 for ss calc (transcoeff)
c***********************************************************************
c       if (nsstate.eq.1) then
c       if ((nsstate.eq.1).or.(nsstate.eq.2)) then
c	   nssnow=1 ! steady state calculation (for transcoeff.f)

c	   if(istst.eq.1)call steadystate() ! transp(C-N) = Rxn + drdc*delC
!	   if(istst.eq.2)call globess()	
!	   if(istst.eq.3)call steadystate2() ! jacobian, func, all in one

c	   if (nsstate.eq.1) return
c	   return
c	 end if !if steady state
	 nssnow = 0 ! indicates transient calculation

c***********************************************************************
c  initial TIME STEP SELECTOR                                          *
c  deltsave saves the original (maple) timestep, which is restored     *
c  before leaving this subroutine. needed if delt is changed during    *
c  the simulation (e.g. to avoid jumping over tend)                    *
c***********************************************************************
	deltsave = delt
	nt = 0
	time = tstart
	mtime=time
	call getdelt(nt,time,tend,spguess)
!	call getdelt(nt,time,tend,dtold,spguess)

c***********************************************************************
c     KBinfo_time=next time to produce dislplay output about model     *
c     last simulated time. Useful in long simulations where output is  *
c     only produced at the end, to check how the model run is going    *
c     Added 02 August, 2005, DRA                                       *
c***********************************************************************

	KBinfo_time=tend/50

!	first time to produce output about model current simulated time

c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
c_/      Introducing MultiG POC concetration for bioturbated layer     _/
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

          call MultiG()

c***********************************************************************
c  START OF TIME LOOP                                                  *
c***********************************************************************
	do while (time.lt.tend)
999	  nt = nt+1
	  time = time + delt
	  mtime = time
c	  write(*,*) 'mtime= ',mtime 
	  call advdiffcoeff() !! in case temp so advdiffcoeff depend on time DRA

C	  call boundaries() !! in case they depend on time
C	  call biogeo()  !! in case they depend on time

c	  write(*,*) time, KBinfo_time, delt
      
	
	if (time.le.KBinfo_time.and.KBinfo_time.lt.time+delt)then
	 KBinfo_100=int(100*KBinfo_time/tend)
	 write(*,*) 'Simulation at ',KBinfo_100,' %,  time=',time
c	 write(*,*) 'Total simulation time=',tend
	 KBinfo_time=KBinfo_time+tend/50
	end if


	if (time.le.v_out.and.v_out.lt.time+delt)then
	 write(*,*) 'Writing output to files at time=',time
	 write(87,*) 'Generating output at time=',time
	 write(87,*) 'Last delta-t=',delt
	end if

c***********************************************************************
c  START OF TRANSPORT                                                  *
c     ncomp     = total number of variables (Maple)                    *
c     nx        = total number of grid points (node&faces) (maple)     *
c     co        = concentration array (temporary array)                *
c     spguess   = concentration prior to the transport step            *
c                 (used in the batch reactor procedure)                *
c     transport = routine for advection/ dispersion scheme             *
c***********************************************************************
        do k = 1,ncomp
          do j = 1,nx,2 
            co(j) = sp(k,j)
            spguess(k,j) = sp(k,j)
          end do 
          call transport(k,nt)
          do j = 1,nx,2
		if(k.eq.1.or.k.eq.12) then	
c		write(777,*) sp(k,j),co(j)
      	endif
            sp(k,j) = co(j)
          end do
c		pause 'end of transport'
        end do
c	  pause
c***********************************************************************
c  START OF REACTION                                                   *
c     fmax  = maximum calculated residual in current time step         *
c             (residual x time step) for all components in the entire  *
c             spatial domain                                           *                
c     emax  = maximum calculated change in concentration for all       *
c             components in the entire spatial domain.                 *
c     spold = the concentration profile immediately after transport    *
c     iflag = flag to indicate problems in solver                      *
c             (0=no problem, 1=negative concentrations,                *
c              2=exceeding iterations, 3= 1&2)                         *
c             warning is given only once for all species and all nodes *
c     isol  = selector for solver                                      *
c             (1=newton with relaxation, 2=newton with linesearch)     *
c     newtonsub = newton root finding (isol = 1)                       *
c     newt      = newton with additional linesearch (isol = 2)         *
c***********************************************************************
        iflag = 0	
        do k=1,ncomp
          do j = 1,nx,2
	      spold(k,j) = sp(k,j)
	      sp(k,j) = spguess(k,j) 
	    end do
        end do

        depth =  0.d0 ! depth in the sediment column
!	  call rates(1)
!        call out(1,nt,time,depth,v_out,v_int)

!       do j = 3,nx-2,2  ! outer space loop for reaction network
	  newtonflag=0		!MT
	  newtoncounter=0	!MT
        do j = 1,nx,2    ! outer space loop for reaction network	
c		write(*,*) j
          if (isol.eq.1) call newtonsub(j,iflag,ftol,etol,newton,ilud)
c         if (isol.eq.2) call newt(check,j,iflag,ftol,etol,newton,ilud)
		if (iflag.eq.2) then				!MT
			newtonflag=2					!MT
			newtoncounter=newtoncounter+1	!MT
		endif								!MT
c***********************************************************************
c  DEAL WITH BAD RESULTS                                               *
c     if a concentration gets VERY large or negative, set concentration*
c     back to ±1e30. This can happen during optimization if the        *
c     parameters are chosen very poorly. clumsy if statements should   *
c     also set the concentration to these values if one runs into a NAN*
c***********************************************************************
         if (tsmaxnwcounter.lt.newtoncounter) then !DRA
	      tsmaxnwcounter=newtoncounter           !DRA
	   end if                                    !DRA

		do i=1,ncomp
	      call limits(i,j)
          end do
	  
        end do  ! end of space loop
	if (time.le.v_out.and.v_out.lt.time+delt)then
		write(87,"(A,f7.0)") 'Number of times exceeded Newton
     + iterations since last output: ', tsmaxnwcounter
		tsmaxnwcounter=0
	    write(87,"(A,f14.8)") 'Maximum residual function since
     + last output: ',tsfxmax
		tsfxmax=0.0d0
	    write(87,*) 'Maximum number of Newton iterations since
     + last output: ',nmaxnw
		nmaxnw=0
	    write(87,*)
	end if

        if (iflag.eq.1) then
	     write(*,*) 'negative concentrations!'
		 write(87,*) 'Error!  Found negative concentrations!'
	  end if

        if (newtonflag.eq.2) then		!MT
		write(*,*) 'exceeding newton iterations'
		write(*,*) time, delt, newtoncounter	! MT
		time=time-delt			! MT  	
		nt=nt-1					! MT
		delt=delt/2			! MT
		write(87,*) 'Exceeding newton iterations'
		write(87,*) 'at time',time
		write(87,*) 'Already ',newtoncounter,' times exceeded'
		write(87,*) 'Halving delta-t to ',delt
		goto 999				! MT
	  endif
        if (iflag.eq.3) then
		write(*,*) 'conc<0 & exceeding newton it.'
	    write(87,*) 'Error: conc<0 & exceeding newton it.'
	  end if

		do j=1,nx,2
			do i=1,ncomp
				if(sp(i,j).lt.10.0d-30) then
				    sp(i,j)=10.0d-30
c					write(*,*) 'conc under 1e-30 reset to this limit'
				end if
			enddo
		enddo		





c***********************************************************************
c   transport again if doing strang splitting                          *
c   - update co with the current concentrations                        *
c   - send to transport (timestep is adapted in transcoeff.f)          *
c   - update concentrations                                            *
c***********************************************************************
        if (isplit.eq.1) then
          do k = 1,ncomp
            do j = 1,nx,2 
              co(j) = sp(k,j)
            end do 
            call transport(k,nt)
            do j = 1,nx,2
              sp(k,j) = co(j)
            end do
          end do
	  end if

c***********************************************************************
c   OUTPUT                                                             *
c     v_out  = first time for writing to data file                     *
c     v_int  = time interval between data output                       *
c     out    = routine handling the output files (Maple)               *
c***********************************************************************
         do j = 1,nx,2 
           call rates(j)
           call out(j,nt,time,x(j),v_out,v_int) 
	   end do

c***********************************************************************
c   update timestep                                                    *
c***********************************************************************
	  if (time.lt.tend) then 
	    call getdelt(nt,time,tend,spguess)
!	    call getdelt(nt,time,tend,dtold,spguess)
	  end if



	   		
      end do          ! end of time loop
	delt = deltsave ! restore the original (maple-) timestep

      return
      end
