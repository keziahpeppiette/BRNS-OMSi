      subroutine tvd(k,nt,dxp,dxm,dxa)

      include 'common_geo.inc'
      include 'common.inc'

			
      real*8 fl(0:nx+1),cfct(0:nx+1)
	real*8 dxp(nx+1),dxm(nx+1),dxa(nx+1),porsol(nx+1)


	dxa(1)=x(3)-x(1)
	porsol(1) = 1.d0 - por(1)
      do i=2,nx-1
      dxp(i)= x(i+2)-x(i)
	dxm(i)= x(i)-x(i-2)
	dxa(i)=(dxp(i)+dxm(i))/2.d0
	porsol(i) = 1.d0 - por(i)
	enddo

      do j = 1,nx,2
      cfct(j)= sp(k,j)
      end do

c	if(k.eq.12) then
c	if(intv.eq.0) then
c	do j=2,nx
c      cfct(j)= 0.d0
c	enddo
c
c       dsp=2.d0*dxa(1)
c       divn=vs(1)+3.d0*disp(k,1)/dsp
c	 fl_om= spb(k,1) 
c       cfct(1)=(fl_om+4.d0*disp(k,1)*cfct(2)/dsp-
c     +	 disp(k,1)*cfct(3)/dsp)/((1.d0-por(1))*divn)
c	 write(*,*) cfct(1),'cfct1'
c
c	intv = 1
c      endif
c	endif

      do j = 3,nx-2,2
      dtdxa = delt/(dxa(j))
      dtdxp = delt/(dxp(j))
      cfl = abs(vs(j)*dtdxp)

      if(j.eq.3) then
	  f = cfct(3)-cfct(1)
	  rg = 0.d0
        philen = max(0.d0,min(2.d0,2.d0*rg,(2.d0-cfl+
     1           rg*(1.d0+cfl))/3.d0))
        fl(2) = vs(2)*porsol(2)* (cfct(1) + 0.5*(1.0-cfl)*philen*f)
	endif

          f = cfct(j+2)-cfct(j)
          if (dabs(f).gt.1.e-35) then
            rg = (cfct(j)-cfct(j-2))/f
            philen = max(0.d0,min(2.d0,2.d0*rg,(2.d0-cfl+
     1           rg*(1.d0+cfl))/3.d0))
          else
            philen = 0.0
          endif
       fl(j+1) = vs(j+1)*porsol(j+1)* (cfct(j) + 0.5*(1.0-cfl)*philen*f)
	 cfct(j) = cfct(j) - (dtdxa/porsol(j))*(fl(j+1)-fl(j-1)) 
      end do

	do i=1,nx,2
	co(i)=cfct(i)
	enddo

      return
      end
