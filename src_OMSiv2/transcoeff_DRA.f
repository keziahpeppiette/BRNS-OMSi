c                  TRANSPORT coefficients

c***********************************************************************
c	the transport equation is discretized and written in matrix form *
c     Amat*Cvect = dvect, where Amat turns out to be tridiagonal       *
c     the steady state coefficients are similar to the transient ones  *
c     except that components due to the time derivative are not added  *
c     see separate documentation file for details what is solved (&how)*
c                                                                      *
c     CM, July 2002                                                    *
c***********************************************************************

      SUBROUTINE transcoeff_DRA(k,aa,bb,c,di,dxp,dxm,dxa)

	include 'common_geo.inc'
      include 'common.inc'
      include 'common_drive.inc'

      real*8 DI(nx),AA(nx),C(nx),BB(nx),alphirr(nx),RR(nx)
	real*8 dxp(nx+1),dxm(nx+1),dxa(nx+1),porsol(nx+1)

	integer itransp,isolid
	real*8 vap,vam,va,ma,mb,mc,gradD,grade,gradv,ep,em,ea,gb,gac,
     +     	   gd, vvv, deltaxx, deltaxup, deltaxdownm, eta, etap1
     +             alph0, xirr, alphafactor

c***********************************************************************
c	no transport ?                                                   *
c     if itransp is <> 0 after the call to notransport, the species is *
c     not being transported at all. If so, set all coefficients to 0,  *
c     note that in the transient code you never get here for a species *
c     that is not transported (saves time), and it is done here only   *
c     for the steady state problem                                     *
c                                                                      *
c     CM, July 2002                                                    *
c***********************************************************************
      itransp = 0					
c	call notransport(k,itransp)	! itransp set to 1 if not transported
c	if (itransp.ne.0) then      
!        do j=1,nx,2  
c
c            
c        do j=3,nx-2  ! ,2       ! only interior, else tridag fails       
c	    aa(j) = 0.0D0           
c          bb(j) = 1.0d0 ! 0.0D0          
c          c(j)  = 0.0D0
c          di(j) = co(j) ! 0.0d0
c        end do


c	  bb(1) = 1.0d0
c	  di(1) = sp(k,1)
c	  aa(1) = 0.d0
c	  c(1)  = 0.d0
c	  bb(nx) = 1.0d0
c	  di(nx) = sp(k,nx)
c	  aa(nx) = 0.d0
c	  c(nx)  = 0.d0
c	  return
c      end if


c	if (i_irrsp.eq.1) then  !!! diff bioirr for each species
c
c		call bioirr()  !! in this level of nesting in case there are
c  	  				   !! time changing values for bioirr coeffs.
c		alph0=alph0_mat(k)
c		xirratt=xirratt_mat(k)
c		xirr1=xirr1_mat(k)
c		xirr2=xirr2_mat(k)
c	end if
c
c	if (ixirr.eq.0) then
c			do i=1,nx
c				alphirr(i)=0.d0
c			enddo
c	else if (ixirr.eq.1) then
        alph0=1.0d1 ! VK's hard coded bioirrigation. 17/06/09
	xirr=3.5d0
        if (k.eq.9) then
	     alphafactor = 0.01
        elseif (k.eq.11) then
	     alphafactor = 0.01
        else
	     alphafactor = 1.0
        endif

	do i=1,nx
	     alphirr(i)=alphafactor*alph0*exp(-x(i)/xirr)
	enddo
c	else if (ixirr.eq.2) then
c			do i=1,nx
c				alphirr(i)=alph0*exp((xirr1-x(i))/xirr2)/
c    &                      (1+exp((xirr1-x(i))/xirr2))
c    			enddo
c	end if


c***********************************************************************
c	regular transport                                                *
c     - half timestep if doing strang splitting                        *
c     - check if a species is a solid (isolid=1) or a solute,          *
c       because this has an effect on the selection which transport    *
c       parameters and theta (por*area or (1-por)*area) is used        *
c     - then calculate the coefficients in the interior of the domain  *
c     - then calculate the boundary nodes                              *
c     - add time dependence                                            *
c     - double timestep again (restore) if doing strang splitting      *
c                                                                      *
c     CM, July 2002                                                    *
c***********************************************************************


c***********************************************************************
c     half time step if doing strang splitting (restore at the bottom) *
c***********************************************************************
      if (isplit.eq.1) then
	  delt = delt*0.5d0
	end if

c***********************************************************************
c     solid species?                                                   *
c***********************************************************************
      isolid = 0
	call issolid(k,isolid)      ! 1 indicates solid

c***********************************************************************
c     interior nodes                                                   *
c***********************************************************************

	  if (isolid.eq.1) then
c         do i=2,nx-1
c	   dtdxp = delt/(2.d0*dxp(i)*dxa(i))
c	   dtdxm = delt/(2.d0*dxm(i)*dxa(i))
c         c1 = (disp(k,i-1)*por(i-1)+disp(k,i)*por(i))/2.d0
c         c2 = (disp(k,i+1)*por(i+1)+disp(k,i)*por(i))/2.d0
c         AA(I) = -c1*dtdxm
c         C(I) = -c2*dtdxp
c         BB(I) = 1.d0*por(i)+c2*dtdxp+c1*dtdxm
c         RR(I) =  1.d0*por(i)-c2*dtdxp-c1*dtdxm
c	   DI(I)= -C(I)*CO(I+1)+RR(I)*CO(I)
c     &  -AA(I)*CO(I-1)
c         enddo
         do i=3,nx-2,2
	   porsol(i-1) = 1.d0 - por(i-1)
	   porsol(i+1) = 1.d0 - por(i+1)
	   porsol(i) = 1.d0 - por(i)
	   dtdxp = delt/(2.d0*dxp(i)*dxa(i))
	   dtdxm = delt/(2.d0*dxm(i)*dxa(i))
         c1 = (disp(k,i-1)*porsol(i-1))
         c2 = (disp(k,i+1)*porsol(i+1))
         AA(I) = -c1*dtdxm
         C(I) = -c2*dtdxp
         BB(I) = 1.d0*porsol(i)+c2*dtdxp+c1*dtdxm
         RR(I) =  1.d0*porsol(i)-c2*dtdxp-c1*dtdxm
	   DI(I)= -C(I)*CO(I+2)+RR(I)*CO(I)
     +            -AA(I)*CO(I-2)
         enddo
	  else
          do j=3,nx-2  !  , 2
	    deltaxx=x(j+1)-x(j-1)
	    deltaxdown=x(j)-x(j-2)
	    deltaxup=x(j+2)-x(j)
		vvv= vd(j)
	    eta=por(j)
		ai=alphirr(j)
		eta_D_p=(  (por(j))*disp(k,j) + 
     +	     		(por(j+1))*disp(k,j+1)  )/2   !bug? it was por(j-1)
		eta_D_m=(  (por(j))*disp(k,j) + 
     +     		(por(j-1))*disp(k,j-1)  )/2   !bug? it was disp(k,j+1)

	  aa(j) = ( -eta*vvv/(deltaxdown) -  eta_D_m/(deltaxx*deltaxdown 
     +              ))*delt
	  bb(j) =  eta + ai*delt*eta+ ( eta_D_p/(deltaxx*deltaxup) + 
     +             eta_D_m/(deltaxx*deltaxdown) + eta*vvv/(deltaxdown))
     +             *delt

        c(j) = -eta_D_p*delt/(deltaxx*deltaxup)
	  di(j) = eta*sp(k,j) + ai*eta*spb(k,1)*delt
		  end do
	   end if


c***********************************************************************
c     boundary nodes                                                   *
c***********************************************************************
      do ii=1,2

	  if (ii.eq.1) then 
		j=1
		deltaxup=(x(3)-x(1))
	  endif		
	  if (ii.eq.2) then
		j=nx
		deltaxdown=(x(nx)-x(nx-2))
	  endif	

c***********************************************************************
        ! fixed concentration
c***********************************************************************
        if (ibc(k,ii).eq.0) then 
          aa(j) = 0.0d0
	    c(j) = 0.0d0
	    bb(j) = 1.0d0
	    di(j) = spb(k,ii)
	  else       
	   	deltaxx=x(j+1)-x(j-1)
           
c***********************************************************************
          ! flux or gradient condition
c***********************************************************************
          if (isolid.eq.1) then	! get the right velocity and eta
	      vvv = vs(j)
		  eta=(1-por(j))
		  etap1=(1-por(j+1))
		eta_D_p=(  (1-por(j))*disp(k,j) + 
     +     		(1-por(j+1))*disp(k,j+1)  )/2
		eta_D_m=(  (1-por(j))*disp(k,j) + 
     +  		(1-por(j-1))*disp(k,j-1)  )/2


	    else
	      vvv=vd(j)
		  eta=por(j)
		  etap1=por(j+1)
		eta_D_p=(  (por(j))*disp(k,j) + 
     +	        	(por(j+1))*disp(k,j+1)  )/2   !bug? it was por(j-1)
		eta_D_m=(  (por(j))*disp(k,j) + 
     +   		(por(j-1))*disp(k,j-1)  )/2   !bug? it was disp(k,j+1)

          end if


c***********************************************************************
          ! known gradient
	    if (ibc(k,ii).eq.1) then 
            aa(j)=(-vvv/deltaxx - disp(k,j)/(deltaxx*2*(x(j)-x(j-1)))) 
     +            *delt
	    bb(j)= 1 + (vvv/deltaxx + disp(k,j)/(deltaxx*2*(x(j)-x(j-1)
     +            )))*delt
	      c(j)=0.
	      di(j)=sp(k,j)+spb(k,ii)*disp(k,j)*2*delt/deltaxx

	   aa(nx) = 1.0
         c(nx) = 0.0
         bb(nx) = -1.0
         di(nx) = 0.d0

	    end if
c***********************************************************************
	    ! known flux (advective and diffusive)
	    if(ibc(k,ii).eq.2) then 
		  if (j.eq.1) then
	       porsol(1) = 1.d0 - por(1)
	       porsol(3) = 1.d0 - por(3)
	       vel_t=vs(1)*porsol(1)
 	       cb=(disp(k,1)*porsol(1)+disp(k,3)*porsol(3))/2.d0
             vterm=vel_t*delt/dxa(1)-vel_t*vel_t*delt/(2.d0*cb)
             aa(1) = 0.d0
             c(1) = -cb*delt/(dxa(1)*dxa(1))
             BB(1) = 1.d0*porsol(1)+cb*delt/(dxa(1)*dxa(1))+vterm 
             RR(1) =  1.d0*porsol(1)-cb*delt/(dxa(1)*dxa(1))-vterm
	       di(1) = RR(1)*co(1)-c(1)*co(3)+
     +          spb(k,1)*(2.d0*delt/dxa(1)-vel_t*delt/cb)


		  else  !!last node not updated for variable porosity/disp coeff
		    aa(j)=-eta_D_m*delt/(deltaxx*deltaxdown)
			bb(j)=eta + eta*vvv*delt/deltaxx +eta_D_m*delt 
     +			/(deltaxx*deltaxdown)
			c(j)=0.
			di(j)=spb(k,ii)*delt*2/deltaxx+eta*area(j)*sp(k,j)
		  end if  !!last node not updated for variable porosity/disp coeff		    	 
	    end if
	  end if
	end do
c***********************************************************************
c     restore time step if doing strang splitting                      *
c***********************************************************************
      if (isplit.eq.1) then
	  delt = delt*2.0d0
	end if

!	if (k.eq.1) then
!	  write(*,*) 'show'
!	if (co(nx-2).lt.0.505d0) then
!		  write(*,*) 'show'
!      end if
!	end if

      RETURN
      END  
