c
c      SUBROUTINE MULTIG
c
      subroutine MultiG()
      include 'common_geo.inc'
      include 'common.inc'
      real*8 POC(200,nx),SUMPOC(1,nx),POC0,aG,bG,AaG,BbG,z,zz,agebiot(1,
     +nx),POCrcm(1,nx),age_for_nbtl,agenonbiot(1,nx),ageall(1,nx),FF(200
     +,nx),last_z

      real*8 emin,emax,ne,k(200,1),kk(200,1),F(200,1),fsum,G1in,G0in,GAM
     +MP,GAMMQ,GAMMLN 
      real*8 porX,attpor
      integer nGG,nG,xx,i,last_x

      write(*,*) 'Calling MultiG!'
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
c_/                                                                  _/
c_/                        Defining k and F values                   _/
c_/                                                                  _/
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
C Add values here
      ! por0, inita, nu are all gloabl variables.

      POC0=spb(1,1)
      attpor = 1.0d-1
      porX=0.75
      por0=0.85
C
      write(*,*) 'Check if values are correct'
      write(*,*) 'w0',w0,'db0',db0,'xbiot',xbiot,'POC0',POC0

      PRINT *,'Calculate k and F using Gamma Incomplete Funciton'
      nGG=200
      emin=-15
      emax=-log10(inita)+2!ok

      k(1,1)=10**emin!ok
      kk(1,1)=10**emin!ok
      F(1,1)=gammp(nu,inita*k(1,1))! lower gamma function
      k(200,1)=10**emax!ok
      kk(200,1)=10**emax!ok
      F(200,1)=GAMMQ(nu,inita*k(200,1))! upper gamma function
      fsum=0.0
      write(*,*) ' '
      write(*,*) 'Enter Gamma inc loop'
      write(*,*) ' '
C      
      do i=2,nGG-1
             ne=emin+(i-1)*(emax-emin)/(nGG-1) !  PRINT *,'i',i,'ne',ne
             kk(i,1)=10**ne ! The kk values are correct and checked with
!            MATLAB
C            PRINT *,kk
             G0in=inita*kk(i-1,1)!Checked with ML and ok
             G1in=inita*kk(i,1)!Checked with ML and ok, would also work 
C            if put into gammp as formula instead of variable
             G0=GAMMQ(nu,G0in)
             G1=GAMMQ(nu,G1in)
C            PRINT *,'i',i,'G0',G0,'G1',G1
             F(i,1)=(G0-G1)
             k(i,1)=kk(i-1,1)+(kk(i,1)-kk(i-1,1))/2
      enddo
C
      do i=1,nGG !  Summing up all Fractions, F 
              fsum=fsum+F(i,1)
      enddo
C
      write(*,*) ''
      write(*,*) 'Sum of F', fsum
      write(*,*) ''
3099  format(1x,e14.7)

      open (unit=11,file="sumpoc.dat",status="replace")
      close(11)
      open (unit=11,file="poc.dat",status="replace")
      close(11)
C
C
      write(*,*) 'w0',w0,'db0',db0,'xbiot',xbiot,'POC0',POC0
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
c_/                                                                  _/
c_/                      Start depth profile loop                    _/        
c_/                                                                  _/
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
       agebiot=0.0
       agenonbiot=0.0
       SUMPOC=0.0
       POCrcm=0.0

      do xx=1,nx  !!!!!!!!!!!!!!!!!!!!!  depth xx loop !!!!!!!!!!!!!!!
C
      z=x(xx)
C
       IF (z.le.xbiot) then ! --Bioturbated layer--  zbio=10cm@xx=105
       WRITE(11,*) 'BTL xx:',xx,' z:',z 
C
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
c_/                                                                  _/
c_/                    Calculate POC profile for MG                  _/
c_/                                                                  _/
c_/                        Start fractions loop                      _/
c_/                                                                  _/
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
       do nG=1,nGG
C
       aG=(w0-sqrt(w0**2+4*db0*k(nG,1)))/(2*db0)
       bG=(w0+sqrt(w0**2+4*db0*k(nG,1)))/(2*db0)

       BbG=(exp(aG*xbiot)*POC0*aG*F(nG,1))/(-exp(aG*xbiot)*aG+exp(b
     +G*xbiot)*bG)
       AaG=(F(nG,1)*POC0)-BbG
C
        ! This next part is important as it removes extreme values
        ! produced for AaG and BbG
       IF (AaG.ne.AaG) then ! Check if value is NaN
               AaG=0.1D-19
               write(*,*) 'AaG exceeded max at ',xx
       ELSE IF (BbG.ne.BbG) then! Check if value is NaN
               BbG=0.1D-19
               write(*,*) 'BbG exceeded max at',xx
       ENDIF
C
       POC(nG,xx)=AaG*exp(aG*z)+BbG*exp(bG*z);
       FF(nG,xx)=POC(nG,xx)/POC0

C       Debugging, prints out list to chck if POC in BTL is correct
C        write(999,*) xx,'z',z,'nG',nG,'aG',aG,'bG',bG,'AaG',AaG,'BbG',Bb
C     +G,'POC',POC(nG,xx)
C
C       Sum up all POC fraction to one fraction.
        SUMPOC(1,xx)=SUMPOC(1,xx)+POC(nG,xx)
C
       enddo! nG - ENDS INNER LOOP - FRACTION LOOP

C      - Calculates implied age of approximated RCM -  
       agebiot(1,xx)=-inita*(exp(log(SUMPOC(1,xx)/POC0)/nu)-1)/
     +exp(log(SUMPOC(1,xx)/POC0)/nu)
       agebiot(1,xx)=dabs(agebiot(1,xx))
       POCrcm(1,xx)=POC0*(inita/(inita+agebiot(1,xx)))**nu
       last_x=xx
       last_z=x(xx)!Needed for age calc in NBTL. It needs to start from
                   !0, not from 10cm. Thus (z-last_z) instead of just z
                   !when agenonbiot is calc.
       age_for_nbtl=agebiot(1,last_x)
!      PRINT *,'age for nbtl',age_for_nbtl
!      PRINT *,'last z',last_z

       ELSE !  -- Non bioturbated layer --
       WRITE(11,*) 'NBTL xx:',xx,' z:',z 
       
       !exponential decreasing porosity por=por0*exp(-attpor*z)!agenonbiot(1,xx)=age_for_nbtl+((z-last_z)+(por0/attpor)*(exp(-att
!      +por*(z-last_z))-1))/(w0*(1-por0)) ! See comment above 
     
      agenonbiot(1,xx)=age_for_nbtl+(((1-porX)*(z-last_z)+1.0/attpor*(po
     +r0-porX)*(exp(-attpor*(z-last_z))-1.0))/(w0*(1-por0))) 

        agenonbiot(1,xx)=dabs(agenonbiot(1,xx))
       SUMPOC(1,xx)=POC0*(inita/(inita+agenonbiot(1,xx)))**nu
!      PRINT *,'z-last_z:',z-last_z
       ENDIF

       write(12,*) 'agebiot',agebiot(1,xx),'POCrcm',POCrcm(1,xx),'agenon
     +biot',agenonbiot(1,xx),'SUMPOC',SUMPOC(1,xx)
C      
       enddo! XX - ENDS OUTER LOOP - DEPTH LOOP
C
       PRINT *, 'last_x',last_x
C
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
c_/                                                                  _/
c_/  This expression should transfer the concentration for POC to    _/ 
c_/  BRNS such that is can be used as the non-varying and also non-  _/
c_/  transported (notransfport.f) steady state solution. To test this_/
c_/  sp33 shuold be called in an another subroutine to check if it go_/
c_/  transported correctly. spi is the initial value of POC, while   _/
c_/  spb is the boudary condition, aka the feeding value at the SWI. _/
c_/                                                                  _/
C      Combine the Two Ages of the two layers      
        do i=1,last_x
              ageall(1,i)=agebiot(1,i)
        enddo
        do i=last_x+1,nx
              ageall(1,i)=agenonbiot(1,i)
        enddo
C      Transfer calculated Age and POC to BRNS, as initial condition and
C      concentration of whole profile.
       if (ic.eq.2) then
          do i=1,nx,2
             sp(32,i)=ageall(1,i)
             sp(1,i)=SUMPOC(1,i)
          enddo
       endif
c_/                                                                  _/
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
2008     format(1x,e14.7,2x,f12.4)
C
C      PRINT *,'spi from MG',(spi(32,i), i=1,nx)
C
      write(33,*) 'z',z,'sumpoc',sumpoc,'age',ageall
C
      ! Write out all POC fractions
      open (unit=11,file="poc.dat",action="write",status="replace")
      write (11,3099) POC
      close(11)
      ! Write out the sum of all POC fractions
      open (unit=11,file="sumpoc.dat",action="write",status="replace")
      write (11,3099) (SUMPOC(1,i), i=1,nx)
      close(11)
C
C     MULTIG SCRIPT ENDS HERE
      PRINT *,'End of MultiG'
      END SUBROUTINE
c
c
c
c
c
c
c
c
c
C
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
c_/                                                                 c_/
c_/  Additional subroutines used to calculate the incomplete gamma   _/
c_/  function. Scripts are taken from the web.                       _/ 
c_/                                                                 c_/
c_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

C+
C Project     :	Course FORTRAN Codes
C
C Name        :	GAMMLN
C
C Purpose     :	Numerical Recipes natural log of the gamma function.
C
C Explanation :	This function calculates the natural log of the gamma 
C               function for XX > 0, where the gamma function, GAMMA(a), 
C               is given by:
C
C                   GAMMA(XX) = INT_0^INFTY t**(a-1) EXP(-t) dt.
C
C Use         :	Result = GAMMLN(XX)
C
C Inputs      : XX:     Scalar containing the value passed to the gamma function.
C
C Outputs     :	Result: ln(GAMMA(XX)).
C
C Calls       :	None.
C
C Common      :	None.
C
C Restrictions:	FORTRAN 77 coding.
C
C Side effects:	None.
C
C Category    :	Data fitting.
C
C Prev. Hist. :	Based on the GAMMLN function of Numerical Recipes for FORTRAN 77.
C
C Written     :	Donald G. Luttermoser, ETSU/Physics, 2 October 2013.
C
C Modified    :	Version 1, Donald G. Luttermoser, ETSU/Physics, 2 Oct 2013
C			Initial program.
C
C Version     :	Version 1,  2 October 2013.
C
C-
      FUNCTION GAMMLN(XX)
      REAL*8 GAMMLN, XX
      INTEGER J
      DOUBLE PRECISION SER, STP, TMP, X, Y, COF(6)
      SAVE COF, STP
C
      DATA COF, STP/76.18009172947146D0, -86.50532032941677D0,
     +    24.01409824083091D0, -1.231739572450155D0, 
     +    0.1208650973866179D-2, -0.5395239384953D-5, 
     +    2.5066282746310005D0/
C
      X = XX
      Y = X
      TMP = X + 5.5D0
      TMP = (X + 0.5D0) * LOG(TMP) - TMP
      SER = 1.000000000190015D0
C
      DO 11 J = 1, 6
          Y = Y + 1.D0
          SER = SER + COF(J)/Y
 11   CONTINUE
C
      GAMMLN = TMP + LOG(STP * SER / X)
C
      RETURN
      END
C
C
      FUNCTION GAMMP(a,x)
      REAL*8 a,GAMMP,x
C       USES gcf,gser
C       Returns the incomplete gamma function P(a,x).
      REAL*8 gammcf,gamser,gln
C       if(x.lt.0..or.a.le.0.)!pause ’bad arguments in GAMMP’ 
C      PRINT *,'put',a,x
      if(x.lt.a+1.)then !Use the series representation.
              call gser(gamser,a,x,gln)
              GAMMP=gamser
      else !Use the continued fraction representation
              call gcf(gammcf,a,x,gln)
              GAMMP=1.-gammcf ! and take its complement
      endif
      return 
      END
C
C
C+
C Project     :	Course FORTRAN Codes
C
C Name        :	GAMMQ
C
C Purpose     :	Numerical Recipes incomplete gamma function Q.
C
C Explanation :	This function actually returns the complement of the 
C               the incomplete gamma function, 1 - P(a,x), where P(a,x) is 
C               actually the incomplete gamma function defined by
C
C                   P(a,x) = (1./GAMMA(a)) INT_0^x EXP(-t) t**(a-1) dt,
C
C               where GAMMA(a) is the gamma function defined by
C
C                   GAMMA(a) = INT_0^INFTY t**(a-1) EXP(-t) dt.
C
C Use         :	Result = GAMMQ(A, X)
C
C Inputs      :	A:      Scalar containing the value of the exponential of 
C                       the gamma function.
C
C		X:	Scalar containing the upper limit of the integral of 
C                       the incomplete gamma function.
C
C Outputs     :	Result: Returns the value of the compliment of the incomplete
C                       gamma function.
C
C Calls       :	Subroutines GCF, GSER.
C
C Common      :	None.
C
C Restrictions:	FORTRAN 77 coding.
C
C Side effects:	None.
C
C Category    :	Data fitting.
C
C Prev. Hist. :	Based on the GAMMQ function of Numerical Recipes for FORTRAN 77.
C
C Written     :	Donald G. Luttermoser, ETSU/Physics, 2 October 2013.
C
C Modified    :	Version 1, Donald G. Luttermoser, ETSU/Physics, 2 Oct 2013
C			Initial program.
C
C Version     :	Version 1,  2 October 2013.
C
C-
      FUNCTION GAMMQ(A, X)
C
      REAL*8 A, GAMMQ, X
C
C Define internal parameters.
C
      REAL*8 GAMMCF, GAMSER, GLN
C
C Check passed parameters.
C
      IF ((X .LT. 0.) .OR. (A .LE. 0.)) THEN
          PRINT *, 'Bad arguments in GAMMQ.'
          GAMMQ = 0.
          RETURN
      ENDIF
C
      IF (X .LT. A+1.) THEN
          CALL GSER(GAMSER, A, X, GLN)
          GAMMQ = 1. - GAMSER
      ELSE
          CALL GCF(GAMMCF, A, X, GLN)
          GAMMQ = GAMMCF
      ENDIF
C
      RETURN
      END
C
C
C+
C Project     :	Course FORTRAN Codes
C
C Name        :	GCF
C
C Purpose     :	Numerical Recipes incomplete gamma function continued fraction.
C
C Explanation :	This subroutine calculates the continued fraction solution for 
C               the incomplete gamma function given by Eq. (6.2.6) in Numerical
C               Recipes-- The Art of Scientific Computing (1986) by Press, et al.
C
C Use         :	CALL GCF(GAMMCF, A, X, GLN)
C
C Inputs      :	A:      Scalar containing the value of the exponential of 
C                       the gamma function.
C
C		X:	Scalar containing the upper limit of the integral of 
C                       the incomplete gamma function.
C
C Outputs     :	GAMMCF: Returns the value of the continued fraction solution 
C                       for the incomplete gamma function.
C
C               GLN:    Natural log of the gamma function.
C
C Calls       :	Function GAMMLN.
C
C Common      :	None.
C
C Restrictions:	FORTRAN 77 coding.
C
C Side effects:	None.
C
C Category    :	Data fitting.
C
C Prev. Hist. :	Based on the GSER subroutine of Numerical Recipes for FORTRAN 77.
C
C Written     :	Donald G. Luttermoser, ETSU/Physics, 2 October 2013.
C
C Modified    :	Version 1, Donald G. Luttermoser, ETSU/Physics, 2 Oct 2013
C			Initial program.
C
C Version     :	Version 1,  2 October 2013.
C
C-
      SUBROUTINE GCF(GAMMCF, A, X, GLN)
C
      INTEGER ITMAX
      REAL*8 A, GAMMCF, GLN, X, EPS, FPMIN
      PARAMETER (ITMAX=100, EPS=3.E-7, FPMIN=1.E-30)
C
      INTEGER I
      REAL*8 AN, B, C, D, DEL, H, GAMMLN
C
      GLN = GAMMLN(A)
      B = X + 1. - A
      C = 1. / FPMIN
      D = 1. / B
      H = D
      DO 11 I = 1, ITMAX
          AN = -I * (I - A)
          B = B + 2.
          D = AN*D + B
          IF (ABS(D) .LT. FPMIN) D = FPMIN
          C = B + AN/C
          IF (ABS(C) .LT. FPMIN) C = FPMIN
          D = 1. / D
          DEL = D * C
          H = H * DEL
          IF (ABS(DEL-1.) .LT. EPS) GOTO 1
 11   CONTINUE
C
      PRINT *, 'A too large, ITMAX too small in GCF.'
  1   GAMMCF = EXP(-X + A*LOG(X) - GLN) * H
C
      RETURN
      END
C
C
C+
C Project     :	Course FORTRAN Codes
C
C Name        :	GSER
C
C Purpose     :	Numerical Recipes incomplete gamma function series solution.
C
C Explanation :	This subroutine calculates the series solution for the 
C               incomplete gamma function:
C
C                   GAMSER = EXP(-x) x**a * 
C                       SUM_(n=0)^INFTY (GAMMA(A)/GAMMA(a+1+n)) x**n
C
C               where GAMMA(a) is the gamma function defined by
C
C                   GAMMA(a) = INT_0^INFTY t**(a-1) EXP(-t) dt.
C
C Use         :	CALL GSER(GAMSER, A, X, GLN)
C
C Inputs      :	A:      Scalar containing the value of the exponential of 
C                       the gamma function.
C
C		X:	Scalar containing the upper limit of the integral of 
C                       the incomplete gamma function.
C
C Outputs     :	GAMSER: Returns the value of the series defined above.
C
C               GLN:    Natural log of the gamma function.
C
C Calls       :	Function GAMMLN.
C
C Common      :	None.
C
C Restrictions:	FORTRAN 77 coding.
C
C Side effects:	None.
C
C Category    :	Data fitting.
C
C Prev. Hist. :	Based on the GSER subroutine of Numerical Recipes for FORTRAN 77.
C
C Written     :	Donald G. Luttermoser, ETSU/Physics, 2 October 2013.
C
C Modified    :	Version 1, Donald G. Luttermoser, ETSU/Physics, 2 Oct 2013
C			Initial program.
C
C Version     :	Version 1,  2 October 2013.
C
C-
      SUBROUTINE GSER(GAMSER, A, X, GLN)
C
      INTEGER ITMAX
      REAL*8 A, GAMSER, GLN, X, EPS
C
      PARAMETER (ITMAX=100, EPS=3.E-7)
C
      INTEGER N
      REAL*8 AP, DEL, SUM, GAMMLN
C
C Call the GAMMLN function.
C
C      PRINT *,'From GSER',A,X
      GLN = GAMMLN(A)
      IF (X .LE. 0.) THEN
          IF (X .LT. 0.) PRINT *, 'X < 0 in GSER.'
          GAMSER = 0.
          RETURN
      ENDIF
C
      AP = A
      SUM = 1. / A
      DEL = SUM
C
      DO 11 N = 1, ITMAX
          AP = AP + 1.
          DEL = DEL * X / AP
          SUM = SUM + DEL
          IF (ABS(DEL) .LT. ABS(SUM)*EPS) GOTO 1
 11   CONTINUE
C
      PRINT *, 'A too large, ITMAX too small in GSER.'
C
  1   GAMSER = SUM * EXP(-X + A*LOG(X) - GLN)
C
      RETURN
      END
cc_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
cc_/         Safety net for Nan or extreme values for POC             _/
cc_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
c      do xx=1,nx
c       do nG=1,200
c
c       IF (POC(nG,xx).ne.POC(nG,xx)) then ! Check if value is NaN
c               POC(nG,xx)=0.1D-19
c               write(*,*) 'POC(nG,xx) exceeded max at ',xx
c       ENDIF
c
c            if ((POC(nG,xx).lt.1.0d30).and.(POC(nG,xx).gt.-1.0d30)) then
c            ! do nothing. but if NAN enter in ...else...
c             else
c               write(*,*) 'out of bounds...reset to +/-1e30', nG,xx,
c     &               POC(nG,xx)
c       write(25,*) 'out of bounds...reset to +/-1e30',nG,xx,POC(nG,xx)
c
c               if(POC(nG,xx).gt.0.d0) then
c                 POC(nG,xx) = 1.0d30
c               else
c                 if (POC(nG,xx).lt.0.d0) then
c                   POC(nG,xx) = -1.0d30
c                 else
c                   POC(nG,xx) = 0.0d0
c                 end if
c               end if
cc               write(*,*) j,i
c             end if
c      if (dabs(POC(nG,xx)).lt.1.0d-20) then
c           if (POC(nG,xx).lt.0.0) then
c                POC(nG,xx)=-1.0d-20
c           else
c                POC(nG,xx)=1.0d-20
c           endif
c      end if
c      enddo ! nG
c      enddo ! xx
cc     



cc_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
cc_/         Safety net for Nan or extreme values for POC             _/
cc_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
c      do xx=1,nx
c
c       IF (sumpoc(1,xx).ne.sumpoc(1,xx)) then ! Check if value is NaN
c               sumpoc(1,xx)=0.1D-19
c               write(*,*) 'sumpoc(1,xx) exceeded max at ',xx
c       ENDIF
c
c       if ((sumpoc(1,xx).lt.1.0d30).and.(sumpoc(1,xx).gt.-1.0d30)) then
c            ! do nothing. but if NAN enter in ...else...
c             else
c               write(*,*) 'out of bounds...reset to +/-1e30',xx,
c     &               sumpoc(1,xx)
c      write(25,*) 'out of bounds...reset to +/-1e30',xx,sumpoc(1,xx)
c
c               if(sumpoc(1,xx).gt.0.d0) then
c                 sumpoc(1,xx) = 1.0d30
c               else
c                 if (sumpoc(1,xx).lt.0.d0) then
c                   sumpoc(1,xx) = -1.0d30
c                   PRINT *, 'lower than 0'
c                 else
c                   sumpoc(1,xx) = 0.0d0
c                 end if
c               end if
cc               write(*,*) j,i
c             end if
c      if (dabs(sumpoc(1,xx)).lt.1.0d-20) then
c           if (sumpoc(1,xx).lt.0.0) then
c                sumpoc(1,xx)=-1.0d-20
c           else
c                sumpoc(1,xx)=1.0d-20
c           endif
c      end if
c      enddo ! xx
cc_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/


