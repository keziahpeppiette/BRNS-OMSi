c                  TRANSPORT
c***********************************************************************
c	species are transported unless the flag itransp is set .ne.0     *
c     the subroutine notransport is created from maple to indicate the *
c	species that are not transported                                 *
c     the transport is a linear problem, set up as                     *
c     Amat*Cvect = dvect, where Amat turns out to be tridiagonal       *
c     Amat and dvect are determined by the diffusion dispersion coeff.,*
c     the advection velocity, and the time step.                       *
c     see the subroutine transcoeff on how they are defined in detail  *
c***********************************************************************

      SUBROUTINE transport(k,nt)

	include 'common_geo.inc'
      include 'common.inc'
      include 'common_drive.inc'

      real*8 DI(nx),AA(nx),C(nx),BB(nx)
	real*8 dxp(nx+1),dxm(nx+1),dxa(nx+1)

	integer itransp,isolid

      itransp = 0					! 0 indicates that species is transported
	call notransport(k,itransp)	! set to 1 if not transported

	if (itransp.eq.0) then          ! regular transport
             isolid = 0
	call issolid(k,isolid)      ! 1 indicates solid

	 if (isolid.eq.1) then	! call tvd
	  call tvd(k,nt,dxp,dxm,dxa)
	  call transcoeff_DRA(k,aa,bb,c,di,dxp,dxm,dxa)	! define Amat and d
	  call TRIDAG(AA,BB,C,di,co,nx)	! solve Amat*C=d for C
c	  call TRIDAG_pierre(AA,BB,C,di,co,nx)	! solve Amat*C=d for C
c	write(*,*) k,co(1),co(2),co(3)
c	if(k.eq.12) then
c      write(*,*) co(1),co(3),'co after diffusion'
c	endif
       else
        call transcoeff_DRA(k,aa,bb,c,di,dxp,dxm,dxa)	! define Amat and d
	  call TRIDAG(AA,BB,C,di,co,nx)	! solve Amat*C=d for C
	 endif

	else                            ! no transport, but
	  do j=1,nx,2                   ! transfer the current conc.
	    co(j)=sp(k,j)
	  end do
	end if
         
      RETURN
      END  
