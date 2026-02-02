c      
c     SUBROUTINE jacobian
c      
      subroutine jacobian(pd,j)
        include 'common_geo.inc'
        include 'common.inc'
        dimension pd(ncomp,ncomp)
        call switches(j)
         pd(4,8) = -569.D0/78.D0*por(j)/(-1+por(j))/delt
         pd(21,49) = 0
         pd(22,43) = 0
         pd(25,28) = -1/delt
         pd(26,55) = 0
         pd(27,4) = 0
         pd(27,46) = -61520.D0/14667.D0/delt
         pd(30,50) = 0
         pd(35,22) = 0
         pd(36,1) = 0
         pd(42,48) = 0
         pd(50,9) = 0
         pd(52,25) = 0
         pd(55,42) = -9649.D0/9778.D0*por(j)/(-1+por(j))
         pd(13,3) = -3.D0/2.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp
     +(1,j)*sw11*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw
     +04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-
     +sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-
     +sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(-
     +sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw0
     +5*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(
     +sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1-sw02-(0.1D1-s
     +w02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)
     +/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*
     +sp(6,j)/kmso4))-3.D0/2.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*s
     +p(1,j)*sw09*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-s
     +w04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1
     +-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1
     +-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10
     ++(0.1D1-sw10)*sp(6,j)/kmso4)
         pd(13,48) = 0
         pd(15,30) = 0
         pd(16,41) = 0
         pd(19,49) = 0
         pd(26,10) = por(j)**2/(-1+por(j))**2/delt
         pd(33,19) = 0
         pd(34,51) = 0
         pd(36,43) = 0
         pd(37,34) = 0
         pd(48,10) = 0
         pd(48,55) = 0
         pd(51,8) = 0
         pd(53,55) = 0
         pd(7,49) = 0
         pd(9,51) = 0
         pd(16,55) = 0
         pd(22,15) = 0
         pd(26,27) = 0
         pd(28,3) = -por(j)/(-1+por(j))/delt/16
         pd(33,36) = 0
         pd(36,28) = 0
         pd(37,50) = 0
         pd(43,16) = 0
         pd(48,27) = 0
         pd(51,25) = 0
         pd(3,36) = -69.D0/689.D0*(-1+por(j))/por(j)/delt
         pd(9,8) = 0
         pd(10,47) = 0
         pd(14,53) = 0
         pd(15,49) = 0
         pd(17,8) = -53.D0/12.D0/delt
         pd(20,26) = 0
         pd(23,36) = 0
         pd(29,20) = 0
         pd(38,19) = 0
         pd(40,13) = 0
         pd(41,39) = 0
         pd(49,10) = 0
         pd(49,55) = 0
         pd(2,27) = 2*kdi*sw19-53.D0/6.D0/delt
         pd(3,53) = 0
         pd(8,22) = 0
         pd(9,25) = 0
         pd(20,43) = sp(20,j)
         pd(23,53) = 0
         pd(29,37) = 0
         pd(38,36) = -5088.D0/4889.D0*(-1+por(j))/por(j)/delt
         pd(40,30) = 0
         pd(49,27) = 0
         pd(3,8) = -23.D0/26.D0/delt
         pd(4,24) = 0
         pd(6,33) = (-1+por(j))/por(j)*kh2smno2pr*(sp(12,j)+sp(13,j))/8
         pd(11,34) = 0
         pd(18,20) = sp(18,j)
         pd(25,1) = 0
         pd(25,44) = 0
         pd(27,20) = 16.D0/3.D0/delt
         pd(35,39) = 0
         pd(50,26) = 0
         pd(52,42) = -660.D0/4889.D0*por(j)/(-1+por(j))
         pd(1,9) = 0
         pd(3,25) = 0
         pd(4,40) = 11310.D0/4889.D0*por(j)/(-1+por(j))/delt
         pd(6,50) = -68239.D0/234672.D0/delt
         pd(12,39) = 0
         pd(18,37) = 0
         pd(21,38) = 0
         pd(25,18) = 0
         pd(27,36) = -9246472.D0/777351.D0*(-1+por(j))/por(j)/delt
         pd(32,2) = 0
         pd(39,5) = 0
         pd(39,50) = 0
         pd(44,19) = 0
         pd(47,12) = 0
         pd(50,43) = 38596.D0/41843.D0/delt
         pd(6,6) = 0
         pd(24,9) = ksfp*(sp(5,j)+sp(34,j)+sp(35,j))
         pd(28,20) = -por(j)/(-1+por(j))/delt/4
         pd(36,35) = 0
         pd(43,33) = 0
         pd(45,18) = 1/delt
         pd(48,44) = 0.1004232D8/0.29622451D8*por(j)/(-1+por(j))/delt
         pd(1,31) = 0
         pd(3,31) = -69.D0/689.D0*(-1+por(j))/por(j)/delt
         pd(5,21) = 0
         pd(11,11) = ksfe*por(j)
         pd(14,3) = 0
         pd(15,35) = 0
         pd(28,36) = 0.1155809D7/0.2072936D7/delt
         pd(31,38) = 0
         pd(32,8) = 0
         pd(36,38) = 0
         pd(43,50) = 0
         pd(45,35) = 0
         pd(54,6) = 0
         pd(5,54) = 0
         pd(6,12) = (-1+por(j))/por(j)*kh2smno2pr*sp(33,j)/8+(-1+por(j))
     +/por(j)*kh2sfeoh3*sp(5,j)/8+(-1+por(j))/por(j)*kh2smno2*sp(4,j)/8
         pd(12,4) = 0
         pd(24,15) = 0
         pd(29,54) = 0
         pd(34,12) = 2*kh2sfeoh3pr*sp(35,j)
         pd(40,2) = 0
         pd(49,44) = 0
         pd(17,55) = 0
         pd(21,16) = 0
         pd(24,32) = 0
         pd(30,16) = -por(j)/(-1+por(j))/delt/106
         pd(34,29) = -1/delt
         pd(42,14) = 0
         pd(46,30) = 0
         pd(55,8) = 0
         pd(1,26) = 0
         pd(2,7) = 15725.D0/8268.D0/delt
         pd(2,50) = -4435535.D0/704016.D0/delt
         pd(9,46) = 0
         pd(12,55) = 0
         pd(13,14) = -3/delt
         pd(16,51) = 0
         pd(18,54) = 0
         pd(19,15) = 0
         pd(39,22) = 0
         pd(44,36) = 0
         pd(47,29) = 0
         pd(53,21) = 0
         pd(7,17) = 0
         pd(13,31) = 0
         pd(15,17) = 0
         pd(19,32) = 0
         pd(20,5) = 0
         pd(23,23) = 0
         pd(31,16) = 0
         pd(33,2) = 0
         pd(35,44) = 0
         pd(37,17) = por(j)/(-1+por(j))/delt
         pd(39,39) = -sw25*(kBSidis*bd+(kBSidis-kBSidis*bd)*exp(-ad*x_po
     +s))*(0.1D1-sp(40,j)/BSisat)-1/delt
         pd(41,16) = 0
         pd(44,53) = 0
         pd(47,46) = 0
         pd(53,38) = 0
         pd(1,55) = 0
         pd(14,20) = 0
         pd(26,32) = 0
         pd(28,53) = 0
         pd(31,55) = 0
         pd(32,25) = 0
         pd(33,40) = 0
         pd(45,52) = 0
         pd(52,2) = 0
         pd(54,23) = 0
         pd(1,45) = 0
         pd(4,2) = 2*nu/(inita+sp(32,j))*sp(1,j)*sw11*(-(0.1D1-sw02)/kmo
     +2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-
     +(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j
     +)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-(0.1D1-sw02)/k
     +mo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*
     +(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3
     +,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)
     +*sp(5,j)/kmfeoh3)-sw09*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*
     +(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0
     +.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw
     +06)*sp(4,j)/kmmno2)-sw07*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo
     +2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*
     +(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-
     +sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+
     +(0.1D1-sw10)*sp(6,j)/kmso4))+2*nu/(inita+sp(32,j))*sp(1,j)*sw09*(-
     +(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j
     +)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.
     +1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*
     +(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3
     +,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(
     +0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(s
     +w08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso
     +4)+2*nu/(inita+sp(32,j))*sp(1,j)*sw07*(-(0.1D1-sw02)/kmo2+sw03*(0.
     +1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw0
     +2)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*
     +(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmf
     +eoh3)-por(j)/(-1+por(j))*kmnox*sp(10,j)-2*por(j)/(-1+por(j))/delt
         pd(8,33) = 0
         pd(14,36) = 0
         pd(17,35) = 0
         pd(22,37) = 0
         pd(26,49) = 0
         pd(32,42) = 0
         pd(38,2) = 0
         pd(46,8) = 0
         pd(50,3) = 0
         pd(52,19) = 0
         pd(54,40) = -2800.D0/4889.D0*por(j)/(-1+por(j))
         pd(1,25) = 0
         pd(9,30) = 0
         pd(11,44) = 0
         pd(24,49) = 0
         pd(27,31) = 8.D0/53.D0*(-1+por(j))*(86*por(j)-53)/por(j)**2/del
     +t
         pd(30,33) = 0
         pd(34,45) = 0
         pd(35,5) = 1
         pd(42,31) = 0
         pd(46,47) = 370392.D0/0.3055625D7*(-1+por(j))/por(j)/delt
         pd(48,4) = 0
         pd(51,2) = 0
         pd(55,25) = 0
         pd(8,39) = 0
         pd(10,37) = 0
         pd(14,42) = 0
         pd(16,9) = 0
         pd(17,41) = -4611.D0/4889.D0*(-1+por(j))/por(j)/delt
         pd(20,15) = 0
         pd(23,25) = 0
         pd(29,9) = 0
         pd(32,48) = 0
         pd(38,8) = 0
         pd(41,28) = 0
         pd(54,46) = 2800.D0/4889.D0*por(j)/(-1+por(j))
         pd(1,6) = 0
         pd(9,3) = 0
         pd(10,41) = 0
         pd(14,47) = 0
         pd(16,14) = 0
         pd(17,45) = -4611.D0/4889.D0/delt
         pd(20,20) = sp(43,j)
         pd(23,30) = 0
         pd(29,14) = 0
         pd(32,53) = 0
         pd(36,13) = 0
         pd(38,13) = 0
         pd(41,33) = 0
         pd(49,4) = 0
         pd(54,51) = 0
         pd(4,14) = 15725.D0/4134.D0*por(j)/(-1+por(j))/delt
         pd(18,9) = 0
         pd(21,55) = 0
         pd(22,47) = 0
         pd(25,33) = 0
         pd(27,10) = 8/delt
         pd(27,52) = 0
         pd(35,28) = 1
         pd(42,53) = 0
         pd(50,15) = 0
         pd(52,31) = 0
         pd(55,48) = 124763.D0/97780.D0
         pd(3,2) = -46.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*s
     +p(1,j)*sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)
         pd(6,27) = kdi*sw19/2
         pd(11,28) = 0
         pd(18,14) = 0
         pd(25,38) = 0
         pd(27,15) = 904.D0/159.D0/delt
         pd(35,33) = 0
         pd(50,20) = -234672.D0/41843.D0*sw32*kkaoli*sp(42,j)**2*sp(40,j
     +)**2/sp(20,j)**7/kspkaoli
         pd(52,36) = 8375.D0/4889.D0
         pd(55,53) = 0
         pd(5,38) = 0
         pd(7,55) = 0
         pd(22,21) = 0
         pd(28,9) = 0
         pd(43,22) = 0
         pd(48,33) = 0
         pd(51,31) = 0
         pd(51,50) = 2614.D0/14667.D0*por(j)/(-1+por(j))
         pd(15,51) = 0
         pd(22,26) = 0
         pd(23,7) = 0
         pd(24,3) = 0
         pd(28,14) = 99.D0/424.D0*por(j)/(-1+por(j))/delt
         pd(43,27) = 0
         pd(48,38) = -0.1953390234D0*por(j)/(-1+por(j))*sw31*ksmect*sp(2
     +2,j)**0.165D0/sp(38,j)**0.67D0*sp(42,j)**0.17D1*sp(40,j)**4/sp(20,
     +j)**6/kspsmect+0.627645D7/0.29622451D8*por(j)/(-1+por(j))/delt
         pd(2,33) = 0
         pd(10,26) = 0
         pd(16,37) = 0
         pd(17,30) = 0
         pd(20,49) = 0
         pd(29,43) = 0
         pd(34,1) = 0
         pd(38,42) = -1848.D0/24445.D0/delt
         pd(40,36) = 0
         pd(49,33) = 0
         pd(53,4) = 0
         pd(2,38) = -836875.D0/58668.D0/delt
         pd(9,35) = 0
         pd(10,31) = (-1+por(j))**2/por(j)**2/delt
         pd(19,3) = 0
         pd(20,54) = 0
         pd(29,48) = 0
         pd(34,6) = 0
         pd(36,36) = 0
         pd(38,47) = -29568.D0/611125.D0*(-1+por(j))/por(j)/delt
         pd(40,41) = -1
         pd(46,52) = 1403.D0/1000.D0*(-1+por(j))/por(j)*sw29*kpyrox*(0.1
     +D1-sp(40,j)**2*sp(11,j)**0.46D0*sp(38,j)**0.84D0*sp(22,j)**0.7D0/s
     +p(20,j)**4/ksppyrox)
         pd(49,38) = 0
         pd(53,9) = 0
         pd(2,39) = -1741519.D0/0.156448D7*(-1+por(j))/por(j)/delt
         pd(4,46) = 249925.D0/29334.D0*por(j)/(-1+por(j))/delt
         pd(11,55) = 0
         pd(12,44) = 0
         pd(18,43) = 0
         pd(19,4) = 0
         pd(23,13) = 0
         pd(27,41) = 5568.D0/4889.D0*(-1+por(j))/por(j)/delt
         pd(36,37) = 1/delt
         pd(38,48) = -94584.D0/122225.D0*(-1+por(j))/por(j)/delt
         pd(39,11) = 0
         pd(40,42) = 0
         pd(44,25) = 0
         pd(46,53) = 0
         pd(47,18) = 0
         pd(50,49) = 0
         pd(53,10) = 0
         pd(2,44) = -385645.D0/58668.D0/delt
         pd(4,51) = 0
         pd(9,40) = 0
         pd(12,49) = 0
         pd(13,8) = 0
         pd(16,46) = 0
         pd(18,48) = 0
         pd(19,9) = 0
         pd(23,9) = 0
         pd(39,16) = 0
         pd(40,47) = 0
         pd(43,5) = 0
         pd(44,30) = 0
         pd(47,23) = 0
         pd(50,54) = 0
         pd(53,15) = 0
         pd(5,27) = 16*por(j)/(-1+por(j))*kdi*sw19+16*por(j)/(-1+por(j))
     +/delt
         pd(8,6) = 0
         pd(10,5) = 0
         pd(14,9) = 0
         pd(28,42) = -15711.D0/19556.D0*por(j)/(-1+por(j))/delt
         pd(31,44) = 0
         pd(32,14) = 0
         pd(45,41) = -1044.D0/4889.D0*(-1+por(j))/por(j)/delt
         pd(54,12) = 0
         pd(2,1) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))*sw11*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-s
     +w02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw0
     +4+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-
     +sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp
     +(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(0.1D1-sw0
     +2-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/
     +kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw
     +02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04
     ++(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-s
     +w07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(
     +4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw
     +10)*sp(6,j)/kmso4))+(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sw09*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*
     +sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(
     +0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/k
     +mmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05
     +*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw0
     +2)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-s
     +w06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(
     +0.1D1-sw10)*sp(6,j)/kmso4)
         pd(5,32) = 0
         pd(10,10) = (-1+por(j))/por(j)*kmnco3precip*sw12*sp(17,j)/KsMnC
     +O3
         pd(14,14) = -(0.1D1-sw17)*kdis*sw18*(ch4eq-sp(7,j))-1/delt
         pd(15,43) = 0
         pd(28,47) = -4176.D0/122225.D0/delt
         pd(31,49) = 0.21D0*sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j))
     +**0.35D0*sp(40,j)**0.23D1*sp(46,j)**0.17D1/sp(49,j)**0.7D0*sp(22,j
     +)**0.7D0/kspplagio
         pd(32,19) = 0
         pd(45,46) = 3845.D0/4889.D0/delt
         pd(54,17) = 0
         pd(16,27) = 0
         pd(21,22) = sw15*kcaldiss*sp(21,j)*sp(17,j)/kspcal-por(j)/(-1+p
     +or(j))*sw20*kcalppt*sp(17,j)/kspcal
         pd(24,38) = 0
         pd(30,22) = 0
         pd(34,35) = 2*kh2sfeoh3pr*(sp(12,j)+sp(13,j))
         pd(42,20) = 2*sp(44,j)*sp(20,j)
         pd(44,3) = 0
         pd(46,36) = -94001.D0/39112.D0*(-1+por(j))/por(j)/delt
         pd(55,14) = 0
         pd(11,39) = 0
         pd(16,32) = 0
         pd(21,27) = 0
         pd(24,43) = 0
         pd(30,27) = 0
         pd(34,40) = 0
         pd(42,25) = 0
         pd(44,8) = 0
         pd(46,41) = 46299.D0/244450.D0*(-1+por(j))/por(j)/delt
         pd(51,43) = -231.D0/4889.D0*por(j)/(-1+por(j))
         pd(55,19) = 0
         pd(5,6) = 16*por(j)/(-1+por(j))/delt
         pd(7,22) = 0
         pd(13,37) = 0
         pd(15,21) = 0
         pd(19,38) = 0
         pd(29,3) = 0
         pd(31,22) = 0.49D0*sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j))
     +**0.35D0*sp(40,j)**0.23D1*sp(46,j)**0.17D1*sp(49,j)**0.3D0/sp(22,j
     +)**0.3D0/kspplagio
         pd(33,8) = 0
         pd(35,50) = 0
         pd(37,23) = 53.D0/6.D0/delt
         pd(39,45) = 0
         pd(41,22) = 0
         pd(53,44) = 0
         pd(5,11) = 0
         pd(7,27) = -kdi*sw19-1/delt
         pd(13,42) = 0
         pd(15,25) = 0
         pd(19,43) = 0
         pd(20,14) = 0
         pd(26,4) = -por(j)/(-1+por(j))*kmnage-por(j)/(-1+por(j))/delt
         pd(29,8) = 0
         pd(33,13) = 0
         pd(35,55) = 0
         pd(37,28) = 0
         pd(41,27) = 0
         pd(53,49) = 0
         pd(2,17) = 88759.D0/8268.D0/delt
         pd(3,41) = 0
         pd(8,10) = 0
         pd(8,54) = 0
         pd(9,13) = 0
         pd(10,52) = 0
         pd(16,22) = 0
         pd(17,13) = -53.D0/12.D0/delt
         pd(20,31) = 0
         pd(23,41) = 0
         pd(29,25) = 0
         pd(36,12) = 0
         pd(36,46) = 0
         pd(38,24) = 0
         pd(40,18) = 0
         pd(41,44) = 0
         pd(49,15) = 0
         pd(4,13) = kh2sfeoh3*sp(5,j)+kh2smno2pr*sp(33,j)-41.D0/6.D0*por
     +(j)/(-1+por(j))/delt
         pd(18,8) = 0
         pd(21,54) = 0
         pd(25,32) = 0
         pd(27,9) = 0
         pd(27,51) = 0
         pd(30,55) = 0
         pd(35,27) = 0
         pd(42,52) = 0
         pd(50,14) = 0
         pd(52,30) = 0
         pd(55,47) = -32159.D0/244450.D0
         pd(1,7) = 0
         pd(9,4) = 0
         pd(10,42) = 0
         pd(14,48) = 0
         pd(16,15) = 0
         pd(17,3) = 53.D0/12.D0/delt
         pd(20,21) = 0
         pd(23,31) = 0
         pd(29,15) = 0
         pd(32,54) = 0
         pd(36,14) = 0
         pd(38,14) = 0
         pd(41,34) = 0
         pd(49,5) = 0
         pd(54,52) = 0
         pd(4,3) = 2*nu/(inita+sp(32,j))*sp(1,j)*sw11*(-sw03*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06
     +)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/k
     +mo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+
     +(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(-sw03*(0.1D1-sw02-(0.1D1-sw02)
     +*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw0
     +2)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmm
     +no2)-sw07*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw0
     +4)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-s
     +w04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*
     +sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))+2*nu/(inita+s
     +p(32,j))*sp(1,j)*sw09*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2
     +)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/km
     +o2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-s
     +w03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05
     +*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(s
     +w06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeo
     +h3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+2*nu/(inita+sp(32,j))*sp(1,
     +j)*sw07*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)
     +/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw0
     +4)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp
     +(5,j)/kmfeoh3)+809.D0/78.D0*por(j)/(-1+por(j))/delt
         pd(21,44) = 0
         pd(22,38) = 0
         pd(25,24) = 0
         pd(26,50) = 0
         pd(30,45) = 0
         pd(35,17) = 0
         pd(42,43) = 0
         pd(50,4) = 0
         pd(52,20) = 0
         pd(55,37) = 5015.D0/4889.D0
         pd(5,53) = 0
         pd(6,11) = -1/delt/2
         pd(24,14) = 0
         pd(28,25) = kfess0*sp(27,j)/8-1/delt/16
         pd(31,26) = 0
         pd(43,38) = 0
         pd(45,23) = 10.D0/13.D0*(-1+por(j))/por(j)/delt
         pd(48,49) = 0
         pd(2,32) = -(-1+por(j))/por(j)*nu/(inita+sp(32,j))**2*sp(1,j)*s
     +w11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(
     +4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3
     +)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0
     +.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw
     +09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-s
     +w02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-s
     +w02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j
     +)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4
     +,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)
     +-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.
     +1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(s
     +w10+(0.1D1-sw10)*sp(6,j)/kmso4))-(-1+por(j))/por(j)*nu/(inita+sp(3
     +2,j))**2*sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(
     +0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/k
     +mno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw0
     +6+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-s
     +w04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw0
     +3*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j
     +)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*s
     +p(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)
         pd(10,25) = 0
         pd(17,29) = 53.D0/6.D0*(-1+por(j))/por(j)/delt
         pd(20,48) = 0
         pd(29,42) = 0
         pd(38,41) = -1848.D0/24445.D0*(-1+por(j))/por(j)/delt
         pd(40,35) = ksfsi*sp(40,j)
         pd(49,32) = 0
         pd(53,3) = 0
         pd(6,1) = 0
         pd(15,52) = 0
         pd(22,27) = 0
         pd(24,4) = 0
         pd(28,15) = -113.D0/424.D0*por(j)/(-1+por(j))/delt
         pd(43,28) = 0
         pd(45,13) = -1/delt
         pd(48,39) = 0.50086071D8/0.148112255D9/delt
         pd(3,48) = 0
         pd(8,17) = 0
         pd(9,20) = 0
         pd(10,16) = 0
         pd(17,20) = -53.D0/12.D0/delt
         pd(20,38) = 0
         pd(23,20) = 0
         pd(23,48) = 0
         pd(29,32) = 0
         pd(38,31) = 0
         pd(40,25) = 0
         pd(41,51) = 0
         pd(49,22) = 0
         pd(2,55) = 0
         pd(7,5) = -(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*
     +sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp
     +(4,j)/kmmno2))*(0.1D1-sw08)/kmfeoh3*(sw10+(0.1D1-sw10)*sp(6,j)/kms
     +o4)/2
         pd(13,19) = 0
         pd(15,6) = 0
         pd(19,20) = 0
         pd(31,4) = 0
         pd(37,5) = 0
         pd(39,27) = 0
         pd(40,55) = 0
         pd(41,4) = 0
         pd(44,41) = 0
         pd(45,1) = 12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*s
     +w11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(
     +4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3
     +)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0
     +.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw
     +09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-s
     +w02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-s
     +w02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j
     +)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4
     +,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)
     +-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.
     +1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(s
     +w10+(0.1D1-sw10)*sp(6,j)/kmso4))+12.D0/53.D0*(-1+por(j))/por(j)*nu
     +/(inita+sp(32,j))*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*
     +(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/
     +kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw
     +06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-
     +sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw
     +03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,
     +j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*
     +sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+12.D0/53.D0*(-
     +1+por(j))/por(j)*nu/(inita+sp(32,j))*sw05*(0.1D1-sw02-(0.1D1-sw02)
     +*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0
     +.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)+12.D
     +0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sw07*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2
     +)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*
     +sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.
     +1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw0
     +8+(0.1D1-sw08)*sp(5,j)/kmfeoh3)
         pd(47,34) = 0
         pd(51,49) = 0
         pd(53,26) = 0
         pd(5,26) = 0
         pd(8,5) = 0
         pd(10,4) = 0
         pd(14,8) = 0
         pd(28,41) = -261.D0/4889.D0/delt
         pd(31,43) = 0
         pd(32,13) = 0
         pd(43,55) = 0
         pd(45,40) = 1044.D0/4889.D0/delt
         pd(54,11) = 0
         pd(2,45) = -5655.D0/4889.D0/delt
         pd(4,52) = 0
         pd(9,41) = 0
         pd(12,50) = 0
         pd(13,9) = 0
         pd(16,47) = 0
         pd(18,49) = 0
         pd(19,10) = 0
         pd(39,17) = 0
         pd(40,48) = 0
         pd(43,6) = 0
         pd(44,31) = 0
         pd(47,24) = 0
         pd(53,16) = 0
         pd(3,26) = -69.D0/689.D0*(-1+por(j))/por(j)/delt
         pd(7,33) = 0
         pd(28,31) = -3.D0/424.D0*(86*por(j)-53)/por(j)/delt
         pd(31,33) = 0
         pd(32,3) = 0
         pd(39,51) = 0
         pd(43,45) = sp(20,j)**3
         pd(45,30) = 0
         pd(54,1) = 0
         pd(11,49) = 0
         pd(13,2) = -3*kaomo2*sp(7,j)-3.D0/2.D0*(-1+por(j))/por(j)*nu/(i
     +nita+sp(32,j))*sp(1,j)*sw11*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/
     +kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw
     +03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1
     +D1-sw06)*sp(4,j)/kmmno2)-sw07*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02
     +)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+
     +sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0
     +.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw
     +09*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw0
     +4+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-
     +sw07*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)
     +*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(s
     +w04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2
     +))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)
     +/kmso4))-3.D0/2.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*
     +sw09*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)
     +*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(s
     +w04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2
     +)-sw07*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw0
     +4)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*
     +(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmn
     +o2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,
     +j)/kmso4)
         pd(24,54) = 0
         pd(30,38) = 0
         pd(34,50) = 0
         pd(35,10) = 0
         pd(42,36) = 0
         pd(48,9) = 0
         pd(51,7) = 0
         pd(55,30) = 0
         pd(5,5) = 0
         pd(13,36) = 0
         pd(19,37) = 0
         pd(20,10) = 0
         pd(29,2) = 0
         pd(31,21) = 0
         pd(33,7) = 0
         pd(35,49) = 0
         pd(37,22) = 0
         pd(39,44) = 0
         pd(41,21) = 0
         pd(53,43) = 0
         pd(11,40) = 0
         pd(16,33) = 0
         pd(21,28) = 0
         pd(24,44) = 0
         pd(30,28) = 0
         pd(42,26) = 0
         pd(44,9) = 0
         pd(46,42) = 46299.D0/244450.D0/delt
         pd(51,44) = -231.D0/4889.D0*por(j)/(-1+por(j))
         pd(55,20) = 0
         pd(7,12) = -1/delt
         pd(13,26) = 0
         pd(15,13) = 0
         pd(19,27) = 0
         pd(31,11) = 0
         pd(37,12) = 0
         pd(39,34) = 0
         pd(41,11) = 0
         pd(44,48) = 0
         pd(45,8) = -10.D0/13.D0/delt
         pd(47,41) = 0
         pd(53,33) = 0
         pd(7,43) = 0
         pd(15,1) = 0
         pd(15,39) = 0
         pd(22,9) = 0
         pd(26,21) = 0
         pd(33,30) = 0
         pd(37,45) = 0
         pd(48,21) = -148124220.D0/0.207357157D9/delt
         pd(51,19) = 0
         pd(7,48) = 0
         pd(9,50) = 0
         pd(22,14) = 0
         pd(26,26) = 0
         pd(28,2) = -por(j)/(-1+por(j))/delt/4
         pd(33,35) = 0
         pd(37,49) = 0
         pd(43,15) = 0
         pd(48,26) = 0
         pd(51,24) = 0
         pd(5,17) = 0
         pd(9,36) = 0
         pd(13,49) = 0
         pd(15,31) = 0
         pd(19,50) = 0
         pd(26,11) = por(j)/(-1+por(j))*kfemno2pr*sp(33,j)
         pd(33,20) = 0
         pd(34,52) = 0
         pd(36,44) = 0
         pd(37,35) = 0
         pd(48,11) = 0
         pd(51,9) = 0
         pd(7,38) = 0
         pd(13,54) = 0
         pd(19,55) = 0
         pd(22,4) = 0
         pd(26,16) = 0
         pd(33,25) = 0
         pd(37,40) = 0
         pd(38,53) = 0
         pd(48,16) = 0
         pd(51,14) = 0
         pd(3,19) = 0
         pd(4,35) = 0
         pd(6,44) = -5933.D0/19556.D0/delt
         pd(12,34) = 0
         pd(18,31) = 0
         pd(21,32) = 0
         pd(25,12) = 0
         pd(25,55) = 0
         pd(44,13) = 0
         pd(47,6) = 0
         pd(47,51) = 0
         pd(50,37) = -40120.D0/41843.D0*(-1+por(j))/por(j)/delt
         pd(52,53) = 0
         pd(1,8) = 0
         pd(3,24) = 0
         pd(6,49) = 0
         pd(12,38) = 0
         pd(18,36) = 0
         pd(21,37) = 0
         pd(25,17) = 0
         pd(27,35) = 0
         pd(32,1) = 0
         pd(39,4) = 0
         pd(39,49) = 0
         pd(44,18) = 0
         pd(47,11) = 0
         pd(50,42) = 78224.D0/41843.D0*sw32*kkaoli*sp(42,j)*sp(40,j)**2/
     +sp(20,j)**6/kspkaoli+38596.D0/41843.D0/delt
         pd(1,48) = 0
         pd(3,9) = 0
         pd(4,25) = 53.D0/3.D0/delt
         pd(6,34) = 0
         pd(11,35) = 0
         pd(12,25) = 0
         pd(18,21) = 0
         pd(20,2) = 0
         pd(25,2) = -kfeso2*sp(25,j)
         pd(25,45) = 0
         pd(27,21) = -9246472.D0/777351.D0*(-1+por(j))/por(j)/delt
         pd(35,40) = 0
         pd(36,16) = 0
         pd(50,27) = 0
         pd(52,43) = -660.D0/4889.D0*por(j)/(-1+por(j))
         pd(3,14) = 69.D0/689.D0/delt
         pd(4,30) = 0
         pd(6,39) = -401889.D0/0.78224D7*(-1+por(j))/por(j)/delt
         pd(12,29) = 0
         pd(18,26) = 0
         pd(25,7) = 0
         pd(25,50) = 0
         pd(27,26) = 4.D0/159.D0*(-1+por(j))*(569*por(j)-371)/por(j)**2/
     +delt
         pd(47,1) = 0
         pd(50,32) = 0
         pd(52,48) = 10933.D0/9778.D0
         pd(2,12) = -53.D0/6.D0/delt
         pd(6,22) = -7735.D0/9778.D0/delt
         pd(8,49) = 0
         pd(11,24) = 0
         pd(12,15) = 0
         pd(16,18) = 0
         pd(17,49) = 0
         pd(21,10) = 0
         pd(24,26) = 0
         pd(30,10) = 0
         pd(34,23) = 0
         pd(42,10) = 0
         pd(46,24) = 0
         pd(55,2) = 0
         pd(4,18) = 65.D0/6.D0*por(j)/(-1+por(j))/delt
         pd(12,19) = 0
         pd(17,54) = 0
         pd(21,15) = 0
         pd(22,51) = 0
         pd(24,31) = 0
         pd(30,15) = -por(j)/(-1+por(j))/delt/106
         pd(34,28) = -1/delt
         pd(46,29) = 0
         pd(55,7) = 0
         pd(5,55) = 0
         pd(6,13) = (-1+por(j))/por(j)*kh2smno2pr*sp(33,j)/8+(-1+por(j))
     +/por(j)*kh2sfeoh3*sp(5,j)/8+(-1+por(j))/por(j)*kh2smno2*sp(4,j)/8+
     +1/delt/4
         pd(12,5) = 0
         pd(24,16) = 0
         pd(29,55) = 0
         pd(34,13) = 2*kh2sfeoh3pr*sp(35,j)
         pd(40,3) = 0
         pd(46,14) = 0
         pd(49,45) = 0
         pd(8,44) = 0
         pd(11,3) = 0
         pd(12,10) = 0
         pd(18,4) = 0
         pd(21,5) = 0
         pd(24,21) = 0
         pd(30,5) = 0
         pd(34,18) = 0
         pd(40,8) = 0
         pd(42,5) = 0
         pd(46,19) = 0
         pd(49,50) = 0
         pd(5,48) = 0
         pd(8,27) = 0
         pd(14,31) = 0
         pd(15,55) = 0
         pd(22,31) = 0
         pd(23,2) = 0
         pd(26,43) = 0
         pd(32,36) = 0
         pd(33,51) = 0
         pd(46,2) = 0
         pd(51,35) = 0
         pd(52,13) = 0
         pd(54,34) = 0
         pd(1,44) = 0
         pd(4,1) = 2*nu/(inita+sp(32,j))*sw11*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1
     +D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2
     +-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp
     +(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/km
     +o2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02
     +)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(
     +0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(s
     +w08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(0.1D1-sw02-(0.1D1-sw02)*sp
     +(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D
     +1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-
     +sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(
     +3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(
     +0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)
     +*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0
     +.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw
     +08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4
     +))+2*nu/(inita+sp(32,j))*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno
     +3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw0
     +2)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+
     +(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/
     +kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04
     +)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1
     +-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+2*nu/(i
     +nita+sp(32,j))*sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmn
     +o3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+
     +(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)
         pd(8,32) = 0
         pd(12,3) = 0
         pd(14,35) = 0
         pd(17,34) = 0
         pd(22,36) = -(-1+por(j))/por(j)*sw21*karadis*(0.1D1-sp(22,j)*sp
     +(17,j)/kspara)-(-1+por(j))/por(j)/delt
         pd(26,48) = 0
         pd(32,41) = 0
         pd(36,42) = 0
         pd(38,1) = 0
         pd(46,7) = 0
         pd(50,2) = 0
         pd(51,40) = 231.D0/4889.D0*por(j)/(-1+por(j))
         pd(52,18) = 0
         pd(54,39) = 2793.D0/4889.D0
         pd(14,21) = 0
         pd(26,33) = por(j)/(-1+por(j))*kfemno2pr*sp(11,j)+por(j)/(-1+po
     +r(j))*kh2smno2pr*(sp(12,j)+sp(13,j))
         pd(28,54) = 0
         pd(32,26) = 0
         pd(33,41) = 0
         pd(45,53) = 0
         pd(52,3) = 0
         pd(54,24) = 0
         pd(1,42) = 0
         pd(5,43) = 0
         pd(10,21) = 0
         pd(14,26) = 0
         pd(17,25) = 53.D0/6.D0*(-1+por(j))/por(j)/delt
         pd(26,38) = 0
         pd(32,31) = 0
         pd(33,46) = 0
         pd(52,8) = 0
         pd(54,29) = 0
         pd(5,42) = 0
         pd(15,16) = sp(20,j)
         pd(22,25) = 0
         pd(23,6) = 0
         pd(24,2) = 0
         pd(28,13) = por(j)/(-1+por(j))/delt/4
         pd(43,26) = 0
         pd(48,37) = -148124220.D0/0.207357157D9/delt
         pd(1,46) = 0
         pd(3,46) = 0
         pd(8,15) = 0
         pd(9,18) = 0
         pd(16,26) = 0
         pd(17,18) = 53.D0/12.D0/delt
         pd(20,36) = 0
         pd(23,46) = 0
         pd(29,30) = 0
         pd(36,32) = 0
         pd(38,29) = 0
         pd(40,23) = 0
         pd(41,49) = 0
         pd(49,20) = 0
         pd(2,37) = 0.86293733D8/0.13474084D8*(-1+por(j))/por(j)/delt
         pd(9,34) = 0
         pd(10,30) = 0
         pd(16,40) = 0
         pd(19,2) = 0
         pd(20,53) = 0
         pd(29,47) = 0
         pd(34,5) = -1/delt
         pd(38,46) = -1848.D0/24445.D0/delt
         pd(40,40) = ksfsi*(sp(5,j)+sp(34,j)+sp(35,j))
         pd(49,37) = 0
         pd(53,8) = 0
         pd(3,18) = 0
         pd(4,34) = 0
         pd(6,43) = -5411.D0/9778.D0/delt
         pd(12,33) = 0
         pd(18,30) = 0
         pd(21,31) = 0
         pd(25,11) = kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(13,j)/sp(20,j)/Ks
     +FeS-por(j)/(-1+por(j))*kfesprecip*sw13*sp(13,j)/sp(20,j)/KsFeS
         pd(25,54) = 0
         pd(44,12) = 0
         pd(47,5) = 0
         pd(50,36) = -40120.D0/41843.D0*(-1+por(j))/por(j)/delt
         pd(52,52) = 1
         pd(7,39) = 0
         pd(13,55) = 0
         pd(15,36) = 0
         pd(22,5) = 0
         pd(26,17) = 0
         pd(33,26) = 0
         pd(37,41) = 0
         pd(48,17) = 0
         pd(51,15) = 0
         pd(14,43) = 0
         pd(16,10) = 0
         pd(17,42) = -277561.D0/19556.D0/delt
         pd(20,16) = 0
         pd(23,26) = -6.D0/53.D0/delt
         pd(29,10) = 0
         pd(32,49) = 0
         pd(36,55) = 0
         pd(38,9) = 0
         pd(41,29) = 0
         pd(54,47) = 1792.D0/4889.D0
         pd(3,43) = 0
         pd(8,12) = 0
         pd(9,15) = 0
         pd(10,54) = 0
         pd(16,6) = 0
         pd(16,24) = 0
         pd(17,15) = 1/delt/12
         pd(20,33) = 0
         pd(23,43) = 0
         pd(29,27) = 0
         pd(38,26) = 0
         pd(40,20) = 0
         pd(41,46) = 0
         pd(49,17) = 0
         pd(4,15) = -811.D0/4134.D0*por(j)/(-1+por(j))/delt
         pd(6,23) = (-1+por(j))/por(j)/delt/16
         pd(11,25) = 0
         pd(18,10) = 0
         pd(25,34) = 0
         pd(27,11) = 28.D0/3.D0/delt
         pd(27,53) = 0
         pd(35,29) = 1
         pd(42,11) = 0
         pd(42,54) = 0
         pd(50,16) = 0
         pd(52,32) = 0
         pd(55,49) = 0
         pd(11,38) = 0
         pd(12,28) = 0
         pd(16,31) = 0
         pd(21,26) = 0
         pd(24,42) = 0
         pd(30,26) = 1/delt/106
         pd(34,39) = 0
         pd(36,49) = 0
         pd(42,24) = 0
         pd(44,7) = 0
         pd(46,40) = -1403.D0/500.D0*(-1+por(j))/por(j)*sw29*kpyrox*sp(5
     +2,j)*sp(40,j)*sp(11,j)**0.46D0*sp(38,j)**0.84D0*sp(22,j)**0.7D0/sp
     +(20,j)**4/ksppyrox-46299.D0/244450.D0/delt
         pd(51,42) = -231.D0/4889.D0*por(j)/(-1+por(j))
         pd(55,18) = 0
         pd(7,10) = 0
         pd(13,24) = 0
         pd(15,11) = 0
         pd(19,25) = 0
         pd(31,9) = 0
         pd(37,10) = 0
         pd(39,32) = 0
         pd(41,9) = 0
         pd(44,46) = sp(20,j)**4
         pd(45,6) = -12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*
     +sp(1,j)*sw11*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3
     +)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0
     +.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/k
     +mo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)
     +*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,
     +j)/kmfeoh3))*(0.1D1-sw10)/kmso4+12.D0/53.D0*(-1+por(j))/por(j)*nu/
     +(inita+sp(32,j))*sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno
     +3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw0
     +2)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+
     +(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/
     +kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04
     +)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1
     +-sw08)*sp(5,j)/kmfeoh3))*(0.1D1-sw10)/kmso4
         pd(47,39) = 0
         pd(53,31) = 0
         pd(5,10) = 0
         pd(7,26) = 0
         pd(13,41) = 0
         pd(19,42) = 0
         pd(26,3) = 0
         pd(29,7) = 0
         pd(33,12) = 0
         pd(35,54) = 0
         pd(37,27) = 0
         pd(41,26) = 0
         pd(53,48) = 0
         pd(1,15) = 0
         pd(5,47) = 0
         pd(8,26) = 0
         pd(14,30) = 0
         pd(23,1) = 0
         pd(26,42) = 0
         pd(32,35) = 0
         pd(33,50) = 0
         pd(46,1) = 0
         pd(52,12) = 0
         pd(54,33) = 0
         pd(2,3) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw11*(
     +-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw
     +05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*
     +(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j
     +)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(-sw03*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-
     +sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(s
     +w08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso
     +4))+(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*(-sw03*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(
     +0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.
     +1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(
     +2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)
     +)*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/
     +kmso4)+809.D0/156.D0/delt
         pd(8,45) = 0
         pd(11,4) = 0
         pd(12,11) = 0
         pd(17,46) = 203785.D0/58668.D0/delt
         pd(21,6) = 0
         pd(22,44) = 0
         pd(24,22) = 0
         pd(30,6) = 0
         pd(34,19) = 0
         pd(40,9) = 0
         pd(42,6) = 0
         pd(46,20) = 1403.D0/250.D0*(-1+por(j))/por(j)*sw29*kpyrox*sp(52
     +,j)*sp(40,j)**2*sp(11,j)**0.46D0*sp(38,j)**0.84D0*sp(22,j)**0.7D0/
     +sp(20,j)**5/ksppyrox
         pd(49,51) = 0
         pd(2,8) = -569.D0/156.D0/delt
         pd(2,40) = 5655.D0/4889.D0/delt
         pd(4,47) = -36192.D0/24445.D0/delt
         pd(12,45) = 0
         pd(13,4) = -3.D0/2.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp
     +(1,j)*sw11*(-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3
     +))*(0.1D1-sw06)/kmmno2+sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/
     +kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04
     +)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/k
     +mfeoh3)-sw09*(-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmn
     +o3))*(0.1D1-sw06)/kmmno2+sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j
     +)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw
     +04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)
     +/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))-3.D0/2.D0*(-1+por(j)
     +)/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*(-sw05*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(
     +sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2+sw07*sw05*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*
     +sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmn
     +o2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)
     +/kmso4)
         pd(16,42) = 0
         pd(18,44) = 0
         pd(19,5) = 0
         pd(23,14) = 0
         pd(38,49) = 0
         pd(39,12) = 0
         pd(40,43) = 0
         pd(43,1) = 0
         pd(44,26) = 0
         pd(46,54) = 0
         pd(47,19) = 0
         pd(50,50) = -1/delt
         pd(53,11) = 0
         pd(1,51) = 0
         pd(7,7) = -kaom*sp(6,j)
         pd(13,21) = 0
         pd(15,8) = 0
         pd(19,22) = 0
         pd(31,6) = 0
         pd(37,7) = por(j)/(-1+por(j))/delt
         pd(39,29) = 0
         pd(41,6) = 0
         pd(44,43) = 0
         pd(45,3) = 12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*s
     +p(1,j)*sw11*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-s
     +w04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1
     +-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1
     +-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(
     +-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw
     +05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*
     +(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j
     +)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)
     +*sp(6,j)/kmso4))+12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j)
     +)*sp(1,j)*sw09*(-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D
     +1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.
     +1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(
     +0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.
     +1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(s
     +w10+(0.1D1-sw10)*sp(6,j)/kmso4)-12.D0/53.D0*(-1+por(j))/por(j)*nu/
     +(inita+sp(32,j))*sp(1,j)*sw05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j
     +)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)+12.D
     +0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw07*(-sw03
     +*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw
     +03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06
     ++(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)
     ++16.D0/13.D0/delt
         pd(47,36) = 0
         pd(53,28) = 0
         pd(5,28) = -32/delt
         pd(10,6) = 0
         pd(14,10) = 0
         pd(15,40) = 0
         pd(28,43) = -5411.D0/9778.D0*por(j)/(-1+por(j))/delt
         pd(31,45) = 0
         pd(32,15) = 0
         pd(45,42) = -15711.D0/4889.D0/delt
         pd(54,13) = 0
         pd(3,7) = 69.D0/689.D0/delt
         pd(4,23) = 569.D0/78.D0/delt
         pd(6,32) = 0
         pd(11,33) = 0
         pd(18,19) = -keq4
         pd(25,43) = 0
         pd(35,38) = 0
         pd(50,25) = 0
         pd(52,41) = -660.D0/4889.D0
         pd(3,12) = 0
         pd(4,28) = 106.D0/3.D0/delt
         pd(6,37) = 0.1155809D7/0.2072936D7*(-1+por(j))/por(j)/delt
         pd(18,24) = 0
         pd(20,4) = 0
         pd(25,5) = 0
         pd(25,48) = 0
         pd(27,24) = 0
         pd(50,30) = 0
         pd(52,46) = -660.D0/4889.D0*por(j)/(-1+por(j))
         pd(6,5) = (-1+por(j))/por(j)*kh2sfeoh3*(sp(12,j)+sp(13,j))/8+(-
     +1+por(j))/por(j)*kfeage/16+(-1+por(j))/por(j)/delt/16
         pd(7,21) = 0
         pd(15,20) = sp(16,j)
         pd(16,1) = 0
         pd(24,8) = 0
         pd(28,19) = 0
         pd(36,34) = 0
         pd(43,32) = 0
         pd(45,17) = 1529.D0/689.D0/delt
         pd(48,43) = 0.1004232D8/0.29622451D8*por(j)/(-1+por(j))/delt
         pd(1,37) = 0
         pd(5,52) = 0
         pd(6,10) = -3.D0/8.D0/delt
         pd(15,24) = 0
         pd(16,5) = 0
         pd(24,13) = 0
         pd(28,24) = 0
         pd(31,25) = 0
         pd(43,37) = 0
         pd(45,22) = -15470.D0/4889.D0/delt
         pd(48,48) = -8787030.D0/0.29622451D8/delt
         pd(4,4) = 2*nu/(inita+sp(32,j))*sp(1,j)*sw11*(-sw05*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/k
     +mo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2+sw07*s
     +w05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06
     +)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(-sw05*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2+sw07
     +*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw
     +06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)
     +*sp(6,j)/kmso4))+2*nu/(inita+sp(32,j))*sp(1,j)*sw09*(-sw05*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2+sw
     +07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-
     +sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw1
     +0)*sp(6,j)/kmso4)-2*nu/(inita+sp(32,j))*sp(1,j)*sw07*sw05*(0.1D1-s
     +w02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j
     +)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2*(sw
     +08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-kfemno2*sp(11,j)-kmnage-1/delt
         pd(21,45) = 0
         pd(22,39) = 0
         pd(26,51) = 0
         pd(27,42) = 83792.D0/4889.D0/delt
         pd(30,46) = 0
         pd(35,18) = 0
         pd(42,44) = sp(20,j)**2
         pd(50,5) = 0
         pd(52,21) = 8375.D0/4889.D0
         pd(55,38) = -2975.D0/9778.D0*por(j)/(-1+por(j))
         pd(4,9) = 0
         pd(6,18) = 1/delt/4
         pd(11,20) = 0
         pd(21,50) = 0
         pd(27,5) = 0
         pd(27,47) = 89088.D0/122225.D0*(-1+por(j))/por(j)/delt
         pd(30,51) = 0
         pd(35,23) = 0
         pd(36,2) = 0
         pd(50,10) = 0
         pd(50,55) = 0
         pd(52,26) = 0
         pd(55,43) = -9649.D0/9778.D0*por(j)/(-1+por(j))
         pd(7,45) = 0
         pd(22,11) = 0
         pd(26,23) = 0
         pd(33,32) = 0
         pd(36,22) = 0
         pd(36,24) = 0
         pd(43,12) = 0
         pd(48,23) = 0
         pd(51,21) = -3180.D0/4889.D0
         pd(7,50) = 0
         pd(9,52) = 0
         pd(22,16) = 0
         pd(28,4) = 0
         pd(36,8) = 0
         pd(36,29) = 0
         pd(37,51) = 0
         pd(43,17) = 0
         pd(48,28) = 0
         pd(51,26) = 0
         pd(1,54) = 0
         pd(10,15) = 0
         pd(14,19) = 0
         pd(26,31) = 1/delt
         pd(28,52) = 0
         pd(31,54) = 0
         pd(32,24) = 0
         pd(33,39) = 0
         pd(45,51) = 0
         pd(52,1) = 0
         pd(54,22) = 5900.D0/4889.D0*por(j)/(-1+por(j))
         pd(10,19) = 0
         pd(14,24) = 0
         pd(15,50) = 0
         pd(26,36) = 0
         pd(32,29) = 0
         pd(33,44) = 0
         pd(52,6) = 0
         pd(54,27) = 0
         pd(1,24) = 0
         pd(9,29) = 0
         pd(11,43) = 0
         pd(16,36) = 0
         pd(24,48) = 0
         pd(27,30) = 0
         pd(30,32) = 0
         pd(34,44) = 0
         pd(35,4) = 0
         pd(42,30) = 0
         pd(46,46) = 46299.D0/244450.D0/delt
         pd(48,3) = 0
         pd(51,1) = 0
         pd(55,24) = 0
         pd(4,39) = -1741519.D0/782240.D0/delt
         pd(11,48) = 0
         pd(13,1) = -3.D0/2.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sw
     +11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-s
     +w02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-s
     +w02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j
     +)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4
     +,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)
     +-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.
     +1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw0
     +9*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw
     +02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,
     +j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-
     +sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1
     +D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw
     +10+(0.1D1-sw10)*sp(6,j)/kmso4))-3.D0/2.D0*(-1+por(j))/por(j)*nu/(i
     +nita+sp(32,j))*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmn
     +o3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+
     +(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw0
     +4)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*
     +(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/
     +kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(
     +5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)
         pd(24,53) = 0
         pd(30,37) = 1/delt/106
         pd(34,49) = 0
         pd(35,9) = 0
         pd(42,35) = 0
         pd(46,51) = 0
         pd(48,8) = 0
         pd(51,6) = 0
         pd(55,29) = 0
         pd(3,27) = 0
         pd(28,32) = 0
         pd(31,34) = 0
         pd(32,4) = 0
         pd(36,6) = 0
         pd(39,52) = 0
         pd(43,46) = 0
         pd(45,31) = -(-1+por(j))*(2907*por(j)-1378)/por(j)**2/delt/689
         pd(54,2) = 0
         pd(1,32) = 0
         pd(3,32) = -46.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*
     +*2*sp(1,j)*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D
     +1-sw04)*sp(3,j)/kmno3)
         pd(5,22) = 0
         pd(8,1) = 0
         pd(11,12) = 0
         pd(14,4) = 0
         pd(23,10) = 0
         pd(28,37) = 0.1155809D7/0.2072936D7/delt
         pd(31,39) = 0
         pd(32,9) = 0
         pd(36,39) = 0
         pd(43,51) = 0
         pd(45,36) = 0.3183549D7/0.3368521D7*(-1+por(j))/por(j)/delt
         pd(54,7) = 0
         pd(2,14) = 15725.D0/8268.D0/delt
         pd(12,17) = 0
         pd(17,51) = 0
         pd(21,12) = 0
         pd(22,48) = 0
         pd(24,28) = 0
         pd(30,12) = 0
         pd(34,25) = -1/delt
         pd(46,26) = 0
         pd(55,4) = 0
         pd(2,18) = 65.D0/12.D0/delt
         pd(21,17) = sw15*kcaldiss*sp(21,j)*sp(22,j)/kspcal-por(j)/(-1+p
     +or(j))*sw20*kcalppt*sp(22,j)/kspcal
         pd(24,33) = 0
         pd(30,17) = -por(j)/(-1+por(j))/delt/106
         pd(34,30) = 0
         pd(42,15) = 0
         pd(46,31) = 0
         pd(55,9) = 0
         pd(3,23) = 23.D0/26.D0*(-1+por(j))/por(j)/delt
         pd(6,48) = 18913.D0/195560.D0*(-1+por(j))/por(j)/delt
         pd(18,35) = 0
         pd(20,13) = 0
         pd(21,36) = 0
         pd(25,16) = 0
         pd(39,3) = 0
         pd(44,17) = 0
         pd(47,10) = 0
         pd(47,55) = 0
         pd(50,41) = 19040.D0/41843.D0*(-1+por(j))/por(j)/delt
         pd(6,4) = (-1+por(j))/por(j)*kh2smno2*(sp(12,j)+sp(13,j))/8
         pd(15,54) = 0
         pd(22,30) = 0
         pd(24,7) = 0
         pd(28,18) = por(j)/(-1+por(j))/delt/4
         pd(36,33) = 0
         pd(43,31) = 0
         pd(45,16) = 840.D0/689.D0/delt
         pd(48,42) = -0.1006291939D1*por(j)/(-1+por(j))*sw31*ksmect*sp(2
     +2,j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)**0.7D0*sp(40,j)**4/sp(20,j
     +)**6/kspsmect+0.1004232D8/0.29622451D8*por(j)/(-1+por(j))/delt
         pd(3,13) = 0
         pd(4,29) = 65.D0/3.D0/delt
         pd(6,38) = -12875.D0/19556.D0/delt
         pd(18,25) = 0
         pd(25,6) = 0
         pd(25,49) = 0
         pd(27,25) = 8.D0/3.D0*(-1+por(j))/por(j)*kpyr*(sp(12,j)+sp(13,j
     +))+4.D0/3.D0*(-1+por(j))/por(j)/delt
         pd(50,31) = 0
         pd(52,47) = -2112.D0/24445.D0
         pd(5,37) = 0
         pd(7,54) = 0
         pd(22,20) = 0
         pd(23,19) = 0
         pd(28,8) = -por(j)/(-1+por(j))/delt/16
         pd(37,55) = 0
         pd(43,21) = 0
         pd(48,32) = 0
         pd(51,30) = 0
         pd(3,3) = 46.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp
     +(1,j)*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmn
     +o3-23.D0/26.D0/delt
         pd(4,19) = 0
         pd(6,28) = 0
         pd(11,29) = -1.D0+por(j)
         pd(12,20) = sp(13,j)
         pd(18,15) = 0
         pd(22,52) = 0
         pd(25,39) = 0
         pd(27,16) = 56.D0/159.D0/delt
         pd(35,34) = 1
         pd(50,21) = -40120.D0/41843.D0*(-1+por(j))/por(j)/delt
         pd(52,37) = 8375.D0/4889.D0
         pd(55,54) = 0
         pd(7,44) = 0
         pd(22,10) = 0
         pd(26,22) = 0
         pd(33,31) = -(-1+por(j))/por(j)
         pd(36,21) = 0
         pd(36,23) = 0
         pd(37,46) = 0
         pd(40,52) = 0
         pd(43,11) = 0
         pd(48,22) = -0.9766951171D-1*por(j)/(-1+por(j))*sw31*ksmect/sp(
     +22,j)**0.835D0*sp(38,j)**0.33D0*sp(42,j)**0.17D1*sp(40,j)**4/sp(20
     +,j)**6/kspsmect+0.14812422D9/0.207357157D9*por(j)/(-1+por(j))/delt
         pd(51,20) = 0
         pd(4,10) = -por(j)/(-1+por(j))*kmnox*sp(2,j)-62.D0/3.D0*por(j)/
     +(-1+por(j))/delt
         pd(6,19) = 0
         pd(11,21) = 0
         pd(18,5) = 0
         pd(21,51) = 0
         pd(27,6) = 0
         pd(27,48) = -151304.D0/73335.D0*(-1+por(j))/por(j)/delt
         pd(30,52) = 0
         pd(35,24) = 0
         pd(50,11) = 0
         pd(52,27) = 0
         pd(55,44) = -9649.D0/9778.D0*por(j)/(-1+por(j))
         pd(1,3) = 0
         pd(5,18) = 0
         pd(7,34) = 0
         pd(9,37) = 0
         pd(11,13) = 0
         pd(13,50) = 0
         pd(15,32) = 0
         pd(19,51) = 0
         pd(26,12) = por(j)/(-1+por(j))*kh2smno2pr*sp(33,j)
         pd(33,21) = 0
         pd(34,53) = 0
         pd(37,36) = -1/delt
         pd(48,12) = 0
         pd(51,10) = 0
         pd(1,43) = 0
         pd(8,31) = 0
         pd(12,2) = 0
         pd(14,34) = 0
         pd(17,33) = 0
         pd(22,35) = 0
         pd(26,47) = 0
         pd(32,40) = 0
         pd(33,55) = 0
         pd(36,41) = 0
         pd(46,6) = 0
         pd(50,1) = 0
         pd(51,39) = -92169.D0/0.19556D7
         pd(52,17) = 0
         pd(54,38) = 1750.D0/4889.D0*por(j)/(-1+por(j))
         pd(1,23) = 0
         pd(9,28) = 0
         pd(11,42) = 0
         pd(24,47) = 0
         pd(30,31) = 1/delt/106
         pd(34,43) = 0
         pd(35,3) = 0
         pd(42,29) = 0
         pd(46,45) = 46299.D0/244450.D0/delt
         pd(48,2) = 0
         pd(55,23) = 0
         pd(1,41) = 0
         pd(10,20) = 0
         pd(14,25) = 0
         pd(17,24) = 0
         pd(26,37) = 0
         pd(32,30) = 0
         pd(33,45) = 0
         pd(52,7) = 0
         pd(54,28) = 0
         pd(12,24) = 0
         pd(21,21) = -sw15*kcaldiss*(0.1D1-sp(22,j)*sp(17,j)/kspcal)-1/d
     +elt
         pd(24,37) = 0
         pd(30,21) = 1/delt/106
         pd(34,34) = -1/delt
         pd(42,19) = 0
         pd(44,2) = 0
         pd(46,35) = 0
         pd(55,13) = 0
         pd(5,33) = 0
         pd(10,11) = 0
         pd(14,15) = 0
         pd(15,44) = 0
         pd(28,48) = 18913.D0/195560.D0/delt
         pd(31,50) = 0
         pd(32,20) = 0
         pd(45,47) = -16704.D0/122225.D0*(-1+por(j))/por(j)/delt
         pd(54,18) = 0
         pd(2,13) = -41.D0/12.D0/delt
         pd(11,9) = 0
         pd(12,16) = 0
         pd(17,50) = -3616667.D0/704016.D0/delt
         pd(21,11) = 0
         pd(24,27) = 0
         pd(30,11) = 0
         pd(34,24) = 0
         pd(46,25) = 0
         pd(55,3) = 0
         pd(3,33) = 0
         pd(5,23) = 0
         pd(8,2) = 0
         pd(10,1) = 0
         pd(14,5) = 0
         pd(23,11) = -6.D0/53.D0*por(j)/(-1+por(j))*kfeco3precip*sw14*sp
     +(17,j)/KsFeCO3
         pd(28,38) = -12875.D0/19556.D0*por(j)/(-1+por(j))/delt
         pd(31,40) = 0.161D1*sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j)
     +)**0.35D0*sp(40,j)**0.13D1*sp(46,j)**0.17D1*sp(49,j)**0.3D0*sp(22,
     +j)**0.7D0/kspplagio
         pd(32,10) = 0
         pd(36,40) = 0
         pd(43,52) = 0
         pd(45,37) = 0.3183549D7/0.3368521D7*(-1+por(j))/por(j)/delt
         pd(54,8) = 0
         pd(6,14) = 99.D0/424.D0/delt
         pd(8,40) = 0
         pd(10,38) = 0
         pd(12,6) = 0
         pd(21,1) = 0
         pd(24,17) = 0
         pd(30,1) = 0
         pd(34,14) = 0
         pd(40,4) = 0
         pd(42,1) = 0
         pd(46,15) = 0
         pd(49,46) = 0
         pd(2,31) = -(-1+por(j))*(178329*por(j)-89570)/por(j)**2/delt/82
     +68
         pd(10,24) = 0
         pd(17,28) = 53.D0/3.D0*(-1+por(j))/por(j)/delt
         pd(20,47) = 0
         pd(29,41) = 0
         pd(36,51) = 0
         pd(38,40) = 8.D0/5.D0*(-1+por(j))/por(j)*sw28*kolive*sp(51,j)*s
     +p(38,j)**0.16D1*sp(11,j)**0.4D0/sp(20,j)**4/kspolive+1848.D0/24445
     +.D0/delt
         pd(40,34) = ksfsi*sp(40,j)
         pd(49,31) = 0
         pd(53,2) = 0
         pd(2,36) = 0.86293733D8/0.13474084D8*(-1+por(j))/por(j)/delt
         pd(9,33) = 0
         pd(10,29) = 0
         pd(19,1) = 0
         pd(20,52) = 0
         pd(29,46) = 0
         pd(34,4) = 0
         pd(36,54) = 0
         pd(38,45) = -1848.D0/24445.D0/delt
         pd(40,39) = 0
         pd(49,36) = 0
         pd(53,7) = 0
         pd(1,47) = 0
         pd(2,22) = -502775.D0/29334.D0/delt
         pd(3,47) = 0
         pd(8,16) = 0
         pd(9,19) = 0
         pd(17,19) = 0
         pd(20,37) = 0
         pd(23,47) = 0
         pd(29,31) = 0
         pd(38,30) = 0
         pd(40,24) = 0
         pd(41,50) = 0
         pd(49,21) = 0
         pd(2,26) = -(-1+por(j))*(178329*por(j)-89570)/por(j)**2/delt/82
     +68
         pd(3,52) = 0
         pd(8,21) = 0
         pd(9,24) = -1.D0+por(j)
         pd(20,42) = -eq1_al
         pd(23,52) = 0
         pd(29,36) = 0
         pd(38,35) = 0
         pd(40,29) = 0
         pd(41,55) = 0
         pd(49,26) = 0
         pd(3,37) = -69.D0/689.D0*(-1+por(j))/por(j)/delt
         pd(8,50) = 0
         pd(9,9) = kspo4*por(j)
         pd(10,48) = 0
         pd(14,54) = 0
         pd(16,19) = 0
         pd(17,9) = 0
         pd(20,27) = 0
         pd(23,37) = 0
         pd(29,21) = 0
         pd(38,20) = -32.D0/5.D0*(-1+por(j))/por(j)*sw28*kolive*sp(51,j)
     +*sp(40,j)*sp(38,j)**0.16D1*sp(11,j)**0.4D0/sp(20,j)**5/kspolive
         pd(40,14) = 0
         pd(41,40) = -sw26*kglass*sp(47,j)*(sp(20,j)**3/sp(42,j))**(1.D0
     +/3.D0)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/kspglass
         pd(49,11) = 0
         pd(3,42) = 0
         pd(8,11) = 0
         pd(8,55) = 0
         pd(9,14) = 0
         pd(10,53) = 0
         pd(16,23) = 0
         pd(17,14) = 1/delt/12
         pd(20,32) = 0
         pd(23,42) = 0
         pd(29,26) = 0
         pd(38,25) = 0
         pd(40,19) = 0
         pd(41,45) = 0
         pd(49,16) = 0
         pd(14,44) = 0
         pd(16,11) = 0
         pd(17,43) = -286783.D0/29334.D0/delt
         pd(20,17) = 0
         pd(23,27) = 0
         pd(29,11) = 0
         pd(32,50) = 0
         pd(38,10) = 0
         pd(41,30) = 0
         pd(49,1) = 0
         pd(54,48) = -2450.D0/4889.D0
         pd(9,5) = 0
         pd(10,43) = 0
         pd(14,49) = 0
         pd(17,4) = 0
         pd(20,22) = 0
         pd(23,32) = 0
         pd(29,16) = 0
         pd(32,55) = 0
         pd(36,15) = 0
         pd(38,15) = 0
         pd(41,35) = 0
         pd(49,6) = 0
         pd(54,53) = 0
         pd(5,4) = 0
         pd(13,35) = 0
         pd(19,36) = 0
         pd(20,9) = 0
         pd(29,1) = 0
         pd(31,20) = -0.735D0*sw27*kplagio*sp(48,j)/(sp(20,j)**3/sp(42,j
     +))**0.65D0*(0.1D1-sp(40,j)**0.23D1*sp(46,j)**0.17D1*sp(49,j)**0.3D
     +0*sp(22,j)**0.7D0/kspplagio)*sp(20,j)**2/sp(42,j)
         pd(33,6) = 0
         pd(35,48) = 0
         pd(37,21) = -1/delt
         pd(39,43) = 0
         pd(41,20) = sw26*kglass*sp(47,j)/(sp(20,j)**3/sp(42,j))**(2.D0/
     +3.D0)*(0.1D1-sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/kspglass)
     +*sp(20,j)**2/sp(42,j)+0.108D1*sw26*kglass*sp(47,j)*(sp(20,j)**3/sp
     +(42,j))**(1.D0/3.D0)*sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.208D1/k
     +spglass
         pd(47,50) = 0.1619476123D1*por(j)/(-1+por(j))*sw30*killite*sp(4
     +0,j)**0.35D1*sp(42,j)**0.23D1/sp(50,j)**0.4D0*sp(38,j)**0.25D0/sp(
     +20,j)**8/kspillite+151475.D0/33672.D0*por(j)/(-1+por(j))/delt
         pd(53,42) = 0
         pd(5,9) = 0
         pd(7,25) = (-1+por(j))/por(j)*kfesdiss*(0.1D1-sw13)*(0.1D1-sp(1
     +1,j)*sp(13,j)/sp(20,j)/KsFeS)+2*(-1+por(j))/por(j)/delt
         pd(13,40) = 0
         pd(19,41) = 0
         pd(26,2) = 0
         pd(29,6) = 0
         pd(33,11) = 0
         pd(35,53) = 0
         pd(37,26) = -1/delt
         pd(39,48) = 0
         pd(41,25) = 0
         pd(53,47) = 0
         pd(7,11) = kfesprecip*sw13*sp(13,j)/sp(20,j)/KsFeS-(-1+por(j))/
     +por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(13,j)/sp(20,j)/KsFeS
         pd(13,25) = -3*(-1+por(j))/por(j)*kfesdiss*(0.1D1-sw13)*(0.1D1-
     +sp(11,j)*sp(13,j)/sp(20,j)/KsFeS)-6*(-1+por(j))/por(j)/delt
         pd(15,12) = 0
         pd(19,26) = 0
         pd(20,1) = 0
         pd(31,10) = 0
         pd(37,11) = 0
         pd(39,33) = 0
         pd(41,10) = 0
         pd(44,47) = 0
         pd(45,7) = 151.D0/689.D0/delt
         pd(47,40) = 0.9446944048D1*por(j)/(-1+por(j))*sw30*killite*sp(4
     +0,j)**0.25D1*sp(42,j)**0.23D1*sp(50,j)**0.6D0*sp(38,j)**0.25D0/sp(
     +20,j)**8/kspillite
         pd(53,32) = 0
         pd(7,16) = 0
         pd(13,30) = 0
         pd(19,31) = 0
         pd(31,15) = 0
         pd(33,1) = 0
         pd(35,43) = 0
         pd(37,16) = por(j)/(-1+por(j))/delt
         pd(39,38) = 0
         pd(41,15) = 0
         pd(44,52) = 0
         pd(45,12) = -2/delt
         pd(47,45) = 0
         pd(53,37) = 0
         pd(1,27) = 0
         pd(2,51) = 0
         pd(7,1) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))*sw09*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-s
     +w02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw0
     +4+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-
     +sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp
     +(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-s
     +w10)*sp(6,j)/kmso4)/2
         pd(9,47) = 0
         pd(13,15) = 0
         pd(15,2) = 0
         pd(16,52) = 0
         pd(18,55) = 0
         pd(19,16) = 0
         pd(37,1) = 0
         pd(39,23) = 0
         pd(44,37) = 0
         pd(47,30) = 0
         pd(51,45) = -231.D0/4889.D0*por(j)/(-1+por(j))
         pd(53,22) = 0
         pd(7,6) = -kaom*sp(7,j)+(-1+por(j))/por(j)*nu/(inita+sp(32,j))*
     +sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw0
     +2-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw0
     +5*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw
     +02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-
     +sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-s
     +w03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3
     +,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))
     +*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/km
     +feoh3))*(0.1D1-sw10)/kmso4/2-2/delt
         pd(13,20) = 3*kfesprecip*sw13*sp(11,j)*sp(13,j)/sp(20,j)**2/KsF
     +eS-3*(-1+por(j))/por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(11,j)*sp
     +(13,j)/sp(20,j)**2/KsFeS
         pd(15,7) = 0
         pd(19,21) = 0
         pd(31,5) = 0
         pd(37,6) = 0
         pd(39,28) = 0
         pd(41,5) = 0
         pd(44,42) = -eq4_al
         pd(45,2) = 12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*s
     +p(1,j)*sw11*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D
     +1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/
     +kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)
     +/kmmno2)-sw07*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.
     +1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02
     +)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,
     +j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(-(0.1D1-sw02
     +)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw
     +05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-(0.1D1-sw
     +02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-
     +sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)
     +*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-
     +sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))+12.D0/5
     +3.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*(-(0.1D1-
     +sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3
     +)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw0
     +4)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-(0.1D
     +1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmn
     +o3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-s
     +w04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.
     +1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+12.D
     +0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw05*(-(0.1
     +D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)+12.D0/53.D0*(-1+por(j))/p
     +or(j)*nu/(inita+sp(32,j))*sp(1,j)*sw07*(-(0.1D1-sw02)/kmo2+sw03*(0
     +.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw
     +02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))
     +*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/km
     +feoh3)
         pd(47,35) = 0
         pd(53,27) = 0
         pd(1,18) = 0
         pd(2,9) = 0
         pd(2,41) = -5655.D0/4889.D0*(-1+por(j))/por(j)/delt
         pd(4,48) = 245869.D0/58668.D0/delt
         pd(12,46) = 0
         pd(13,5) = -3.D0/2.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp
     +(1,j)*sw11*(-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3
     +)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0
     +.1D1-sw06)*sp(4,j)/kmmno2))*(0.1D1-sw08)/kmfeoh3+sw09*sw07*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(s
     +w04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2
     +))*(0.1D1-sw08)/kmfeoh3*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))+3.D0/2.
     +D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*sw07*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(
     +sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno
     +2))*(0.1D1-sw08)/kmfeoh3*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)
         pd(16,43) = 0
         pd(18,45) = 0
         pd(19,6) = 0
         pd(25,25) = -kfeso2*sp(2,j)-kfesdiss*(0.1D1-sw13)*(0.1D1-sp(11,
     +j)*sp(13,j)/sp(20,j)/KsFeS)-1/delt
         pd(38,50) = 20912.D0/73335.D0/delt
         pd(39,13) = 0
         pd(40,44) = 0
         pd(43,2) = 0
         pd(44,27) = 0
         pd(46,55) = 0
         pd(47,20) = -30295.D0/1403.D0*por(j)/(-1+por(j))*sw30*killite*s
     +p(40,j)**0.35D1*sp(42,j)**0.23D1*sp(50,j)**0.6D0*sp(38,j)**0.25D0/
     +sp(20,j)**9/kspillite
         pd(50,51) = 0
         pd(53,12) = 0
         pd(2,46) = 249925.D0/58668.D0/delt
         pd(4,53) = 0
         pd(9,42) = 0
         pd(12,51) = 0
         pd(13,10) = 0
         pd(16,48) = 0
         pd(18,50) = 0
         pd(19,11) = 0
         pd(25,29) = 0
         pd(38,54) = 0
         pd(39,18) = 0
         pd(42,49) = 0
         pd(43,7) = 0
         pd(44,32) = 0
         pd(47,25) = 0
         pd(53,17) = 0
         pd(2,47) = -18096.D0/24445.D0*(-1+por(j))/por(j)/delt
         pd(8,41) = 0
         pd(9,43) = 0
         pd(10,39) = 0
         pd(13,11) = -3*kfesprecip*sw13*sp(13,j)/sp(20,j)/KsFeS+3*(-1+po
     +r(j))/por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(13,j)/sp(20,j)/KsFe
     +S
         pd(16,49) = 0
         pd(19,12) = 0
         pd(34,15) = 0
         pd(40,5) = ksfsi*sp(40,j)
         pd(43,8) = 0
         pd(46,16) = 0
         pd(49,47) = 0
         pd(53,18) = 0
         pd(3,28) = 0
         pd(4,43) = -351715.D0/14667.D0*por(j)/(-1+por(j))/delt
         pd(6,53) = 0
         pd(11,52) = 0
         pd(18,40) = 0
         pd(21,41) = 0
         pd(25,21) = 0
         pd(32,5) = 0
         pd(39,8) = 0
         pd(39,53) = 0
         pd(44,22) = 0
         pd(47,15) = 0
         pd(50,46) = 38596.D0/41843.D0/delt
         pd(54,3) = 0
         pd(10,13) = 0
         pd(14,17) = 0
         pd(32,22) = 0
         pd(39,25) = 0
         pd(41,2) = 0
         pd(42,55) = 0
         pd(44,39) = 0
         pd(47,32) = 0
         pd(54,20) = 0
         pd(4,16) = 7329.D0/689.D0*por(j)/(-1+por(j))/delt
         pd(5,24) = 0
         pd(15,37) = 0
         pd(21,13) = 0
         pd(22,49) = 0
         pd(23,12) = 0
         pd(24,29) = 0
         pd(28,39) = -401889.D0/0.78224D7/delt
         pd(30,13) = 0
         pd(31,41) = 0
         pd(42,12) = 0
         pd(43,53) = 0
         pd(45,38) = -12875.D0/4889.D0/delt
         pd(55,5) = 0
         pd(21,30) = 0
         pd(24,46) = 0
         pd(26,35) = 0
         pd(30,30) = -1/delt
         pd(33,43) = 0
         pd(35,2) = 0
         pd(42,28) = 0
         pd(44,11) = 0
         pd(45,55) = 0
         pd(52,5) = 0
         pd(55,22) = -5015.D0/4889.D0*por(j)/(-1+por(j))
         pd(2,20) = -65.D0/12.D0/delt
         pd(7,14) = 0
         pd(13,28) = -9*(-1+por(j))/por(j)/delt
         pd(15,15) = -keq1
         pd(16,25) = 0
         pd(19,29) = 0
         pd(31,13) = 0
         pd(34,32) = 0
         pd(37,14) = por(j)/(-1+por(j))/delt
         pd(45,10) = -2/delt
         pd(46,33) = 0
         pd(53,35) = 0
         pd(5,14) = 0
         pd(7,30) = 0
         pd(13,45) = 0
         pd(15,27) = 0
         pd(16,39) = 0
         pd(19,46) = 0
         pd(26,7) = 0
         pd(33,16) = 0
         pd(34,48) = 0
         pd(37,31) = -1/delt
         pd(46,50) = 0.2577311D7/0.78224D7/delt
         pd(48,7) = 0
         pd(48,52) = 0
         pd(51,5) = 0
         pd(53,52) = 0
         pd(8,30) = 0
         pd(17,32) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))**2*sp(1,j)*(
     +sw02+(0.1D1-sw02)*sp(2,j)/kmo2)
         pd(20,8) = 0
         pd(32,39) = 0
         pd(35,47) = 0
         pd(39,42) = 0
         pd(41,19) = 0
         pd(46,5) = 0
         pd(47,49) = 0
         pd(54,37) = -5900.D0/4889.D0
         pd(1,4) = 0
         pd(5,19) = 0
         pd(7,35) = 0
         pd(13,51) = 0
         pd(15,33) = 0
         pd(17,5) = 0
         pd(19,52) = 0
         pd(20,23) = 0
         pd(22,1) = 0
         pd(23,33) = 0
         pd(26,13) = por(j)/(-1+por(j))*kh2smno2pr*sp(33,j)
         pd(29,17) = 0
         pd(33,22) = 0
         pd(37,37) = -1/delt
         pd(41,36) = 0
         pd(49,7) = 0
         pd(4,5) = 2*nu/(inita+sp(32,j))*sp(1,j)*sw11*(-sw07*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/k
     +mo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw0
     +2)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+
     +(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(
     +0.1D1-sw08)/kmfeoh3+sw09*sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno
     +3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(0.1D1-sw08)/kmfeoh3*(sw10
     ++(0.1D1-sw10)*sp(6,j)/kmso4))-2*nu/(inita+sp(32,j))*sp(1,j)*sw09*s
     +w07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(
     +4,j)/kmmno2))*(0.1D1-sw08)/kmfeoh3*(sw10+(0.1D1-sw10)*sp(6,j)/kmso
     +4)+2*nu/(inita+sp(32,j))*sp(1,j)*sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(
     +2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1
     +-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-s
     +w03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3
     +,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(0.1D1-sw08)/kmfeo
     +h3+kh2sfeoh3*(sp(12,j)+sp(13,j))+kfeage/2+1/delt/2
         pd(8,36) = 0
         pd(14,39) = 0
         pd(17,38) = -682375.D0/58668.D0/delt
         pd(22,40) = 0
         pd(26,52) = 0
         pd(27,1) = 0
         pd(32,45) = 0
         pd(38,5) = 0
         pd(46,11) = -0.64538D0*(-1+por(j))/por(j)*sw29*kpyrox*sp(52,j)*
     +sp(40,j)**2/sp(11,j)**0.54D0*sp(38,j)**0.84D0*sp(22,j)**0.7D0/sp(2
     +0,j)**4/ksppyrox
         pd(50,6) = 0
         pd(52,22) = -8375.D0/4889.D0*por(j)/(-1+por(j))
         pd(54,43) = 2800.D0/4889.D0*por(j)/(-1+por(j))
         pd(4,21) = 0.86293733D8/0.6737042D7/delt
         pd(6,30) = 0
         pd(8,52) = 0
         pd(9,11) = 0
         pd(10,50) = 0
         pd(11,31) = 0
         pd(18,17) = 0
         pd(27,18) = -16.D0/3.D0/delt
         pd(38,22) = 5088.D0/4889.D0/delt
         pd(40,16) = 0
         pd(50,23) = 0
         pd(52,39) = -13167.D0/97780.D0
         pd(16,53) = 0
         pd(21,52) = 0
         pd(25,30) = 0
         pd(27,49) = 0
         pd(30,53) = 0
         pd(35,25) = 1
         pd(43,13) = 0
         pd(48,24) = 0
         pd(51,22) = 3180.D0/4889.D0*por(j)/(-1+por(j))
         pd(55,45) = -9649.D0/9778.D0*por(j)/(-1+por(j))
         pd(1,22) = 0
         pd(3,11) = 0
         pd(25,47) = 0
         pd(28,17) = 99.D0/424.D0*por(j)/(-1+por(j))/delt
         pd(43,30) = 0
         pd(45,15) = 151.D0/689.D0/delt
         pd(48,41) = 0.1004232D8/0.29622451D8/delt
         pd(51,55) = 0
         pd(3,50) = 0
         pd(7,52) = 0
         pd(8,19) = 0
         pd(10,18) = 0
         pd(17,22) = -409955.D0/29334.D0/delt
         pd(20,40) = 0
         pd(22,18) = 0
         pd(23,22) = 0
         pd(23,50) = 0
         pd(29,34) = 2*kh2sfeoh3mr*(sp(12,j)+sp(13,j))+1/delt
         pd(33,38) = 0
         pd(36,10) = 0
         pd(37,53) = 0
         pd(41,53) = 0
         pd(49,24) = 0
         pd(5,51) = 0
         pd(6,9) = 0
         pd(10,34) = 0
         pd(16,4) = 0
         pd(24,12) = 0
         pd(29,51) = 0
         pd(34,9) = 0
         pd(49,41) = 0
         pd(2,30) = 0
         pd(4,38) = -836875.D0/29334.D0*por(j)/(-1+por(j))/delt
         pd(6,47) = -4176.D0/122225.D0*(-1+por(j))/por(j)/delt
         pd(16,35) = 0
         pd(18,34) = 0
         pd(21,35) = 0
         pd(25,15) = 0
         pd(38,39) = -92169.D0/0.122225D7*(-1+por(j))/por(j)/delt
         pd(39,2) = 0
         pd(40,33) = 0
         pd(44,16) = 0
         pd(47,9) = 0
         pd(50,40) = 78224.D0/41843.D0*sw32*kkaoli*sp(42,j)**2*sp(40,j)/
     +sp(20,j)**6/kspkaoli-19040.D0/41843.D0/delt
         pd(53,1) = 0
         pd(6,15) = -113.D0/424.D0/delt
         pd(11,17) = 0
         pd(12,7) = 0
         pd(18,1) = 0
         pd(21,2) = 0
         pd(24,18) = 0
         pd(30,2) = 0
         pd(31,30) = 0
         pd(42,2) = 0
         pd(43,42) = -eq3_al
         pd(45,27) = -2/delt
         pd(1,33) = 0
         pd(2,4) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw11*(
     +-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw
     +06)/kmmno2+sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3))*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09
     +*(-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-
     +sw06)/kmmno2+sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*
     +(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/
     +kmno3))*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(
     +sw10+(0.1D1-sw10)*sp(6,j)/kmso4))+(-1+por(j))/por(j)*nu/(inita+sp(
     +32,j))*sp(1,j)*sw09*(-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-s
     +w03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3
     +,j)/kmno3))*(0.1D1-sw06)/kmmno2+sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)
     +*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0
     +.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*
     +sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)
         pd(11,5) = 0
         pd(12,12) = -keq3
         pd(21,7) = 0
         pd(23,15) = 0
         pd(24,23) = 0
         pd(28,33) = 0
         pd(30,7) = -por(j)/(-1+por(j))/delt/106
         pd(31,35) = 0
         pd(36,7) = 0
         pd(42,7) = 0
         pd(43,47) = 0
         pd(45,32) = -12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))
     +**2*sp(1,j)*sw11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)
     +-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.
     +1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/km
     +o2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*
     +sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmn
     +o3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j
     +)/kmfeoh3)-sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-
     +sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1
     +D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno
     +3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)
     +/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))-12.D0/53.D0*(-1+por(
     +j))/por(j)*nu/(inita+sp(32,j))**2*sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw
     +04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-s
     +w04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(
     +2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*
     +(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmn
     +o2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,
     +j)/kmso4)-12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))**2*sp
     +(1,j)*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw0
     +6+(0.1D1-sw06)*sp(4,j)/kmmno2)-12.D0/53.D0*(-1+por(j))/por(j)*nu/(
     +inita+sp(32,j))**2*sp(1,j)*sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/k
     +mo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)
     +*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,
     +j)/kmfeoh3)
         pd(2,53) = 0
         pd(7,3) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*(
     +-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw
     +05*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*
     +(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3+sw05*sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw04)/kmno3*(sw06+(0.1D1-sw06)*sp(4,j
     +)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)
     +*sp(6,j)/kmso4)/2
         pd(8,47) = 0
         pd(9,6) = 0
         pd(9,49) = 0
         pd(13,17) = 0
         pd(15,4) = 0
         pd(19,18) = 1
         pd(31,2) = 0
         pd(34,21) = 0
         pd(37,3) = 53.D0/46.D0*por(j)/(-1+por(j))/delt
         pd(46,22) = -0.9821D0*(-1+por(j))/por(j)*sw29*kpyrox*sp(52,j)*s
     +p(40,j)**2*sp(11,j)**0.46D0*sp(38,j)**0.84D0/sp(22,j)**0.3D0/sp(20
     +,j)**4/ksppyrox+94001.D0/39112.D0/delt
         pd(51,47) = -3696.D0/122225.D0
         pd(53,24) = 0
         pd(2,15) = -811.D0/8268.D0/delt
         pd(7,8) = 0
         pd(13,22) = 0
         pd(15,9) = 0
         pd(16,20) = sp(17,j)
         pd(17,52) = 0
         pd(19,23) = 0
         pd(31,7) = 0
         pd(34,26) = (-1+por(j))/por(j)/delt
         pd(37,8) = -689.D0/69.D0*por(j)/(-1+por(j))*knit*sp(2,j)-53.D0/
     +6.D0*por(j)/(-1+por(j))/delt
         pd(45,4) = 12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*s
     +p(1,j)*sw11*(-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno
     +3))*(0.1D1-sw06)/kmmno2+sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw0
     +4)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/
     +kmfeoh3)-sw09*(-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3))*(0.1D1-sw06)/kmmno2+sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-s
     +w04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j
     +)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))+12.D0/53.D0*(-1+por
     +(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*(-sw05*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2
     +)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2+sw07*sw05
     +*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw0
     +2)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/k
     +mmno2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6
     +,j)/kmso4)+12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1
     +,j)*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1
     +-sw06)/kmmno2-12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*s
     +p(1,j)*sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)
     +)*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)
         pd(46,27) = 0
         pd(53,29) = 0
         pd(14,23) = 0
         pd(32,28) = 0
         pd(39,31) = 0
         pd(41,8) = 0
         pd(44,45) = 0
         pd(47,38) = 0.6747817178D0*por(j)/(-1+por(j))*sw30*killite*sp(4
     +0,j)**0.35D1*sp(42,j)**0.23D1*sp(50,j)**0.6D0/sp(38,j)**0.75D0/sp(
     +20,j)**8/kspillite
         pd(54,26) = 0
         pd(1,13) = 0
         pd(10,23) = 0
         pd(14,28) = 0
         pd(17,27) = -53.D0/6.D0/delt
         pd(20,3) = 0
         pd(32,33) = 0
         pd(36,27) = 0
         pd(39,36) = 0
         pd(41,13) = 0
         pd(44,50) = 0
         pd(47,43) = 0
         pd(54,31) = 0
         pd(5,46) = 0
         pd(11,47) = 0
         pd(12,37) = 0
         pd(24,52) = 0
         pd(26,41) = 0
         pd(27,34) = 0
         pd(30,36) = 1/delt/106
         pd(33,49) = 0
         pd(35,8) = 0
         pd(42,34) = 0
         pd(52,11) = 0
         pd(55,28) = 0
         pd(12,1) = 0
         pd(12,41) = 0
         pd(22,34) = 0
         pd(26,46) = 0
         pd(27,38) = 206000.D0/14667.D0/delt
         pd(30,41) = 0
         pd(33,54) = 0
         pd(35,13) = 0
         pd(42,39) = 0
         pd(51,38) = -3200.D0/4889.D0*por(j)/(-1+por(j))
         pd(52,16) = 0
         pd(55,33) = 0
         pd(1,38) = 0
         pd(9,38) = 0
         pd(11,14) = 0
         pd(27,39) = 133963.D0/122225.D0*(-1+por(j))/por(j)/delt
         pd(30,42) = 0
         pd(34,54) = 0
         pd(35,14) = 0
         pd(36,18) = 0
         pd(42,40) = 0
         pd(48,13) = 0
         pd(51,11) = 0
         pd(55,34) = 0
         pd(21,46) = 0
         pd(27,43) = 173152.D0/14667.D0/delt
         pd(30,47) = 0
         pd(35,19) = 0
         pd(40,49) = 0
         pd(42,45) = 0
         pd(48,18) = 0
         pd(51,16) = 0
         pd(55,39) = -1894351.D0/0.39112D7
         pd(3,39) = 0
         pd(7,41) = 0
         pd(8,8) = ksnh4*por(j)
         pd(11,16) = 0
         pd(17,11) = -53.D0/6.D0/delt
         pd(20,29) = 0
         pd(22,7) = 0
         pd(23,39) = 0
         pd(26,19) = 0
         pd(29,23) = 0
         pd(33,28) = 0
         pd(36,4) = 0
         pd(37,43) = 0
         pd(41,42) = -sw26*kglass*sp(47,j)/(sp(20,j)**3/sp(42,j))**(2.D0
     +/3.D0)*(0.1D1-sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/kspglass
     +)*sp(20,j)**3/sp(42,j)**2/3-0.36D0*sw26*kglass*sp(47,j)*(sp(20,j)*
     +*3/sp(42,j))**(1.D0/3.D0)*sp(40,j)/sp(42,j)**0.64D0/sp(20,j)**0.10
     +8D1/kspglass
         pd(49,13) = 0
         pd(3,44) = 0
         pd(7,46) = 0
         pd(8,13) = 0
         pd(15,41) = 0
         pd(16,7) = 0
         pd(17,16) = 9.D0/2.D0/delt
         pd(20,34) = 0
         pd(22,12) = 0
         pd(23,44) = 0
         pd(26,24) = 0
         pd(29,28) = 0
         pd(33,33) = 1
         pd(36,25) = 0
         pd(41,47) = sw26*kglass*(sp(20,j)**3/sp(42,j))**(1.D0/3.D0)*(0.
     +1D1-sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/kspglass)+1/delt
         pd(49,18) = 0
         pd(1,50) = 0
         pd(4,27) = 4*por(j)/(-1+por(j))*kdi*sw19-53.D0/3.D0*por(j)/(-1+
     +por(j))/delt
         pd(6,36) = 0.1155809D7/0.2072936D7*(-1+por(j))/por(j)/delt
         pd(9,17) = 0
         pd(18,23) = 0
         pd(25,4) = 0
         pd(27,23) = -4.D0/3.D0*(-1+por(j))/por(j)/delt
         pd(36,31) = 0
         pd(38,28) = 0
         pd(40,22) = 0
         pd(50,29) = 0
         pd(52,45) = -660.D0/4889.D0*por(j)/(-1+por(j))
         pd(4,32) = -2*nu/(inita+sp(32,j))**2*sp(1,j)*sw11*(0.1D1-sw02-(
     +0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)
     +*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0
     +.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07
     +*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw0
     +2)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw0
     +2-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/
     +kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j
     +)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2
     +)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*
     +sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.
     +1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*
     +(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02
     +)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/k
     +mo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)
     +/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*
     +sp(6,j)/kmso4))-2*nu/(inita+sp(32,j))**2*sp(1,j)*sw09*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/km
     +o2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02
     +)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(
     +0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw0
     +7*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw
     +02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,
     +j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10
     +)*sp(6,j)/kmso4)-2*nu/(inita+sp(32,j))**2*sp(1,j)*sw07*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/k
     +mo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw0
     +2)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+
     +(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(
     +sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)
         pd(6,41) = -261.D0/4889.D0*(-1+por(j))/por(j)/delt
         pd(9,22) = 0
         pd(11,41) = 0
         pd(12,31) = 0
         pd(18,28) = 0
         pd(25,9) = 0
         pd(27,28) = -4.D0/3.D0*(-1+por(j))/por(j)/delt
         pd(38,33) = 0
         pd(40,27) = 0
         pd(47,3) = 0
         pd(50,34) = 0
         pd(52,50) = -9185.D0/39112.D0*por(j)/(-1+por(j))
         pd(1,36) = 0
         pd(3,17) = 69.D0/689.D0/delt
         pd(15,23) = 0
         pd(25,53) = 0
         pd(28,23) = 1/delt/16
         pd(31,24) = 0
         pd(43,36) = 0
         pd(45,21) = 0.3183549D7/0.3368521D7*(-1+por(j))/por(j)/delt
         pd(48,47) = 0.32135424D8/0.148112255D9/delt
         pd(3,22) = 0
         pd(28,28) = -1/delt/16
         pd(31,29) = 0
         pd(43,41) = 0
         pd(45,26) = -(-1+por(j))*(2907*por(j)-1378)/por(j)**2/delt/689
         pd(47,54) = 0
         pd(5,29) = 0
         pd(12,21) = 0
         pd(21,18) = 0
         pd(23,3) = 0
         pd(24,34) = ksfp*sp(9,j)
         pd(28,44) = -5933.D0/19556.D0*por(j)/(-1+por(j))/delt
         pd(30,18) = 0
         pd(31,46) = 0.119D1*sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j)
     +)**0.35D0*sp(40,j)**0.23D1*sp(46,j)**0.7D0*sp(49,j)**0.3D0*sp(22,j
     +)**0.7D0/kspplagio
         pd(37,47) = 0
         pd(42,16) = 0
         pd(45,43) = -10822.D0/4889.D0/delt
         pd(55,10) = 0
         pd(2,52) = 0
         pd(7,2) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*(
     +-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,
     +j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0
     +.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07
     +*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(
     +3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+
     +(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(
     +sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kms
     +o4)/2
         pd(8,46) = 0
         pd(9,48) = 0
         pd(13,16) = 0
         pd(15,3) = 0
         pd(16,16) = -keq2
         pd(19,17) = 0
         pd(31,1) = 0
         pd(34,20) = 0
         pd(37,2) = -689.D0/69.D0*por(j)/(-1+por(j))*knit*sp(8,j)
         pd(40,53) = 0
         pd(46,21) = -94001.D0/39112.D0*(-1+por(j))/por(j)/delt
         pd(51,46) = -231.D0/4889.D0*por(j)/(-1+por(j))
         pd(53,23) = 0
         pd(1,34) = 0
         pd(2,5) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw11*(
     +-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(
     +2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*s
     +p(4,j)/kmmno2))*(0.1D1-sw08)/kmfeoh3+sw09*sw07*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw
     +04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-s
     +w04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(0.1D1-sw
     +08)/kmfeoh3*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))-(-1+por(j))/por(j)*
     +nu/(inita+sp(32,j))*sp(1,j)*sw09*sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(
     +2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1
     +-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-s
     +w03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3
     +,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(0.1D1-sw08)/kmfeo
     +h3*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)
         pd(11,6) = 0
         pd(12,13) = sp(20,j)
         pd(21,8) = 0
         pd(23,16) = 0
         pd(24,24) = 0
         pd(28,34) = 0
         pd(30,8) = 0
         pd(31,36) = 0
         pd(42,8) = 0
         pd(43,48) = 0
         pd(45,33) = 0
         pd(1,19) = 0
         pd(2,10) = -kmnox*sp(2,j)/2-65.D0/6.D0/delt
         pd(2,42) = -340405.D0/19556.D0/delt
         pd(10,35) = 0
         pd(13,6) = 3.D0/2.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(
     +1,j)*sw11*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-s
     +w02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-s
     +w05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D
     +1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2
     +-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp
     +(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3
     +))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/
     +kmfeoh3))*(0.1D1-sw10)/kmso4-3.D0/2.D0*(-1+por(j))/por(j)*nu/(init
     +a+sp(32,j))*sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw0
     +3*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j
     +)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(
     +sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp
     +(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D
     +1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-
     +sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(
     +3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08
     +)*sp(5,j)/kmfeoh3))*(0.1D1-sw10)/kmso4+6/delt
         pd(16,44) = 0
         pd(19,7) = 0
         pd(29,52) = 0
         pd(34,10) = 0
         pd(38,51) = -8.D0/5.D0*(-1+por(j))/por(j)*sw28*kolive*(0.1D1-sp
     +(40,j)*sp(38,j)**0.16D1*sp(11,j)**0.4D0/sp(20,j)**4/kspolive)
         pd(40,45) = 0
         pd(43,3) = 0
         pd(49,42) = 0
         pd(53,13) = 0
         pd(1,17) = 0
         pd(2,2) = -kfeo2*sp(11,j)/4-kmnox*sp(10,j)/2+(-1+por(j))/por(j)
     +*nu/(inita+sp(32,j))*sp(1,j)*sw11*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-
     +sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/k
     +mo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw0
     +6+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(-(0.1D1-sw02)/kmo2+sw03*(0.1D
     +1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)
     +/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(s
     +w06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeo
     +h3)-sw09*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-s
     +w04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo
     +2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/km
     +mno2)-sw07*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1
     +-sw04)*sp(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/k
     +mo2*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/
     +kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*s
     +p(6,j)/kmso4))+(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09
     +*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*sp(
     +3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+
     +(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw
     +07*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3)-sw05*(-(0.1D1-sw02)/kmo2+sw03*(0.1D1-sw02)/kmo2*(sw0
     +4+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))
     +*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/k
     +mso4)-1/delt
         pd(8,35) = 0
         pd(14,38) = 0
         pd(17,37) = 98929.D0/19556.D0*(-1+por(j))/por(j)/delt
         pd(20,12) = 0
         pd(29,5) = -kfeage
         pd(32,44) = 0
         pd(35,52) = 0
         pd(38,4) = 0
         pd(39,47) = 0
         pd(41,24) = 0
         pd(46,10) = 0
         pd(54,42) = 2800.D0/4889.D0*por(j)/(-1+por(j))
         pd(5,45) = 0
         pd(11,46) = 0
         pd(12,36) = 0
         pd(15,53) = 0
         pd(24,51) = 0
         pd(26,40) = 0
         pd(27,33) = 0
         pd(30,35) = 0
         pd(33,48) = 0
         pd(35,7) = 0
         pd(42,33) = 0
         pd(52,10) = 0
         pd(52,55) = 0
         pd(55,27) = 0
         pd(1,14) = 0
         pd(14,29) = 0
         pd(32,34) = 0
         pd(35,42) = 0
         pd(39,37) = 0
         pd(41,14) = 0
         pd(44,51) = 0
         pd(47,44) = 0
         pd(54,32) = 0
         pd(1,12) = 0
         pd(1,53) = 0
         pd(5,36) = 0
         pd(11,37) = 0
         pd(12,27) = 0
         pd(15,47) = 0
         pd(21,25) = 0
         pd(24,41) = 0
         pd(26,30) = 0
         pd(28,51) = 0
         pd(30,25) = 0
         pd(31,53) = 0
         pd(36,48) = 0
         pd(42,23) = 0
         pd(44,6) = 0
         pd(45,50) = -68239.D0/58668.D0/delt
         pd(55,17) = 0
         pd(9,53) = 0
         pd(25,35) = 0
         pd(27,54) = 0
         pd(28,5) = 0
         pd(35,30) = 0
         pd(43,18) = 0
         pd(48,29) = 0
         pd(51,27) = 0
         pd(55,50) = 41843.D0/39112.D0*por(j)/(-1+por(j))
         pd(3,38) = 0
         pd(7,40) = 0
         pd(8,7) = 0
         pd(17,10) = -53.D0/6.D0/delt
         pd(20,28) = 0
         pd(22,6) = 0
         pd(23,38) = 0
         pd(26,18) = 0
         pd(29,22) = 0
         pd(33,27) = 0
         pd(36,3) = 0
         pd(37,42) = 0
         pd(41,41) = 0
         pd(49,12) = 0
         pd(21,47) = 0
         pd(27,44) = 94928.D0/14667.D0/delt
         pd(30,48) = 0
         pd(35,20) = 0
         pd(38,55) = 0
         pd(40,50) = 0
         pd(42,46) = 0
         pd(48,19) = 0
         pd(51,17) = 0
         pd(55,40) = 2380.D0/4889.D0*por(j)/(-1+por(j))
         pd(5,15) = 0
         pd(7,31) = 0
         pd(13,46) = 0
         pd(15,28) = 0
         pd(19,47) = 0
         pd(20,18) = 0
         pd(23,28) = 0
         pd(26,8) = 0
         pd(28,29) = 7.D0/16.D0/delt
         pd(29,12) = 2*kh2sfeoh3mr*sp(34,j)
         pd(33,17) = 0
         pd(37,32) = 0
         pd(41,31) = 0
         pd(48,53) = 0
         pd(49,2) = 0
         pd(53,53) = 1
         pd(2,35) = 0
         pd(4,42) = -340405.D0/9778.D0*por(j)/(-1+por(j))/delt
         pd(6,52) = 0
         pd(9,32) = 0
         pd(11,51) = 0
         pd(18,39) = 0
         pd(21,40) = 0
         pd(25,20) = -kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(11,j)*sp(13,j)/s
     +p(20,j)**2/KsFeS+por(j)/(-1+por(j))*kfesprecip*sw13*sp(11,j)*sp(13
     +,j)/sp(20,j)**2/KsFeS
         pd(38,44) = -1848.D0/24445.D0/delt
         pd(39,7) = 0
         pd(40,38) = 0
         pd(44,21) = 0
         pd(47,14) = 0
         pd(50,45) = 38596.D0/41843.D0/delt
         pd(53,6) = 0
         pd(3,16) = 69.D0/689.D0/delt
         pd(15,22) = 0
         pd(25,52) = 0
         pd(28,22) = -7735.D0/9778.D0*por(j)/(-1+por(j))/delt
         pd(43,35) = 0
         pd(45,20) = -1/delt
         pd(48,46) = 0.1004232D8/0.29622451D8*por(j)/(-1+por(j))/delt
         pd(1,1) = 1
         pd(4,33) = kh2smno2pr*(sp(12,j)+sp(13,j))
         pd(6,42) = -15711.D0/19556.D0/delt
         pd(9,23) = 0
         pd(12,32) = 0
         pd(18,29) = 0
         pd(25,10) = 0
         pd(27,29) = -28.D0/3.D0*(-1+por(j))/por(j)/delt
         pd(38,34) = 0
         pd(40,28) = 0
         pd(47,4) = 0
         pd(50,35) = 0
         pd(52,51) = 0
         pd(3,6) = 0
         pd(25,42) = 0
         pd(28,12) = 0
         pd(35,37) = 0
         pd(43,25) = 0
         pd(48,36) = -148124220.D0/0.207357157D9/delt
         pd(51,34) = 0
         pd(1,28) = 0
         pd(4,54) = 0
         pd(10,7) = 0
         pd(12,52) = 0
         pd(14,11) = 0
         pd(18,51) = 0
         pd(32,16) = 0
         pd(39,19) = 0
         pd(42,50) = 0
         pd(44,33) = 0
         pd(47,26) = 0
         pd(54,14) = 0
         pd(10,12) = 0
         pd(14,16) = 0
         pd(32,21) = 0
         pd(39,24) = 0
         pd(41,1) = 0
         pd(44,38) = 0
         pd(47,31) = 0
         pd(54,19) = 0
         pd(3,29) = 0
         pd(4,44) = -385645.D0/29334.D0*por(j)/(-1+por(j))/delt
         pd(6,54) = 0
         pd(11,53) = 0
         pd(12,42) = 0
         pd(14,1) = 0
         pd(17,1) = -(-1+por(j))/por(j)*nu/(inita+sp(32,j))*(sw02+(0.1D1
     +-sw02)*sp(2,j)/kmo2)
         pd(18,41) = 0
         pd(21,42) = 0
         pd(25,22) = 0
         pd(32,6) = 0
         pd(39,9) = 0
         pd(39,54) = 0
         pd(44,23) = 0
         pd(47,16) = 0
         pd(50,47) = 128636.D0/0.1046075D7*(-1+por(j))/por(j)/delt
         pd(54,4) = 0
         pd(3,34) = 0
         pd(4,49) = 0
         pd(8,3) = 0
         pd(10,2) = 0
         pd(12,47) = 0
         pd(14,6) = 0
         pd(18,46) = 0
         pd(25,26) = 0
         pd(32,11) = 0
         pd(39,14) = 0
         pd(44,28) = 0
         pd(47,21) = 0
         pd(50,52) = 0
         pd(54,9) = 0
         pd(5,8) = 0
         pd(7,24) = 0
         pd(9,27) = 0
         pd(13,39) = 0
         pd(19,40) = 0
         pd(26,1) = 0
         pd(33,10) = -por(j)/(-1+por(j))
         pd(37,25) = 0
         pd(46,44) = 46299.D0/244450.D0/delt
         pd(48,1) = 0
         pd(53,46) = 0
         pd(5,13) = 8*por(j)/(-1+por(j))*kh2so2*sp(2,j)+16*por(j)/(-1+po
     +r(j))/delt
         pd(7,29) = 0
         pd(13,44) = 0
         pd(16,38) = 0
         pd(19,45) = 0
         pd(26,6) = 0
         pd(33,15) = 0
         pd(34,47) = 0
         pd(37,30) = 0
         pd(46,49) = 0
         pd(48,6) = 0
         pd(51,4) = 0
         pd(53,51) = 0
         pd(2,21) = 0.86293733D8/0.13474084D8*(-1+por(j))/por(j)/delt
         pd(7,15) = 0
         pd(13,29) = 0
         pd(19,30) = 0
         pd(31,14) = 0
         pd(34,33) = 0
         pd(37,15) = por(j)/(-1+por(j))/delt
         pd(45,11) = -2/delt
         pd(46,34) = 0
         pd(51,53) = 0
         pd(53,36) = 0
         pd(2,25) = 53.D0/6.D0*(-1+por(j))/por(j)/delt
         pd(5,3) = 0
         pd(7,20) = -kfesprecip*sw13*sp(11,j)*sp(13,j)/sp(20,j)**2/KsFeS
     ++(-1+por(j))/por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(11,j)*sp(13,
     +j)/sp(20,j)**2/KsFeS
         pd(13,34) = 0
         pd(15,19) = 0
         pd(16,30) = 0
         pd(19,35) = 0
         pd(31,19) = 0
         pd(33,5) = 0
         pd(34,38) = 0
         pd(37,20) = 0
         pd(46,39) = 0.18473301D8/0.9778D8*(-1+por(j))/por(j)/delt
         pd(51,41) = -231.D0/4889.D0
         pd(53,41) = 0
         pd(6,24) = 0
         pd(10,44) = 0
         pd(11,26) = 0
         pd(14,50) = 0
         pd(17,47) = -73776.D0/122225.D0*(-1+por(j))/por(j)/delt
         pd(18,11) = 0
         pd(27,12) = 8.D0/3.D0*(-1+por(j))/por(j)*kpyr*sp(25,j)
         pd(38,16) = 0
         pd(40,10) = 0
         pd(49,52) = 0
         pd(50,17) = 0
         pd(52,33) = 0
         pd(54,54) = 1
         pd(4,20) = -65.D0/6.D0*por(j)/(-1+por(j))/delt
         pd(6,29) = (-1+por(j))/por(j)/delt/2
         pd(8,51) = 0
         pd(9,10) = 0
         pd(10,49) = 0
         pd(11,30) = 0
         pd(14,55) = 0
         pd(18,16) = 0
         pd(22,53) = 0
         pd(27,17) = -264.D0/53.D0/delt
         pd(38,21) = -5088.D0/4889.D0*(-1+por(j))/por(j)/delt
         pd(40,15) = 0
         pd(50,22) = 40120.D0/41843.D0/delt
         pd(52,38) = -825.D0/9778.D0*por(j)/(-1+por(j))
         pd(4,6) = -2*nu/(inita+sp(32,j))*sp(1,j)*sw11*sw09*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/km
     +o2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02
     +)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(
     +0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw0
     +7*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw
     +02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)
     +/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,
     +j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(0.1D1-sw10)/kmso
     +4+2*nu/(inita+sp(32,j))*sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-
     +sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw
     +03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,
     +j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)
     +*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1
     +D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08
     ++(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(0.1D1-sw10)/kmso4
         pd(8,37) = 0
         pd(14,40) = 0
         pd(17,39) = -7100039.D0/0.78224D7*(-1+por(j))/por(j)/delt
         pd(22,41) = 0
         pd(26,53) = 0
         pd(27,2) = 16.D0/3.D0/delt
         pd(32,46) = 0
         pd(38,6) = 0
         pd(46,12) = 0
         pd(50,7) = 0
         pd(52,23) = 0
         pd(54,44) = 2800.D0/4889.D0*por(j)/(-1+por(j))
         pd(4,11) = -kfemno2*sp(4,j)-65.D0/3.D0*por(j)/(-1+por(j))/delt
         pd(6,20) = -1/delt/4
         pd(9,1) = 0
         pd(11,22) = 0
         pd(14,45) = 0
         pd(16,12) = 0
         pd(18,6) = 0
         pd(22,45) = 0
         pd(27,7) = -264.D0/53.D0/delt
         pd(32,51) = 0
         pd(38,11) = 0.64D0*(-1+por(j))/por(j)*sw28*kolive*sp(51,j)*sp(4
     +0,j)*sp(38,j)**0.16D1/sp(11,j)**0.6D0/sp(20,j)**4/kspolive
         pd(50,12) = 0
         pd(52,28) = 0
         pd(54,49) = 0
         pd(6,3) = -1/delt/16
         pd(10,28) = 0
         pd(14,33) = 0
         pd(20,51) = 0
         pd(22,29) = 0
         pd(24,6) = 0
         pd(29,45) = 0
         pd(34,3) = 0
         pd(36,53) = 0
         pd(49,35) = 0
         pd(5,50) = 0
         pd(6,8) = -1/delt/16
         pd(10,33) = 0
         pd(16,3) = 0
         pd(24,11) = 0
         pd(29,50) = 0
         pd(34,8) = 0
         pd(49,40) = 0
         pd(3,51) = 0
         pd(7,53) = 0
         pd(8,20) = 0
         pd(17,23) = 53.D0/12.D0*(-1+por(j))/por(j)/delt
         pd(20,41) = 0
         pd(22,19) = 0
         pd(23,51) = 0
         pd(29,35) = 0
         pd(37,54) = 0
         pd(41,54) = 0
         pd(49,25) = 0
         pd(5,41) = 0
         pd(8,25) = 0
         pd(20,46) = 0
         pd(22,24) = 0
         pd(23,5) = 0
         pd(24,1) = 0
         pd(29,40) = 0
         pd(36,50) = 0
         pd(49,30) = 0
         pd(10,22) = 0
         pd(14,27) = 0
         pd(17,26) = -(-1+por(j))*(213*por(j)-106)/por(j)**2/delt/12
         pd(32,32) = 1
         pd(36,26) = 0
         pd(39,35) = 0
         pd(41,12) = 0
         pd(44,49) = 0
         pd(47,42) = 0.6207991803D1*por(j)/(-1+por(j))*sw30*killite*sp(4
     +0,j)**0.35D1*sp(42,j)**0.13D1*sp(50,j)**0.6D0*sp(38,j)**0.25D0/sp(
     +20,j)**8/kspillite
         pd(54,30) = 0
         pd(5,34) = 0
         pd(15,45) = 0
         pd(21,23) = 0
         pd(24,39) = 0
         pd(26,28) = 0
         pd(28,49) = 0
         pd(30,23) = 0
         pd(31,51) = 0
         pd(33,37) = 0
         pd(42,21) = 0
         pd(44,4) = 0
         pd(45,48) = 18913.D0/48890.D0*(-1+por(j))/por(j)/delt
         pd(55,15) = 0
         pd(22,33) = 0
         pd(26,45) = 0
         pd(30,40) = 0
         pd(33,53) = 0
         pd(35,12) = 0
         pd(42,38) = 0
         pd(51,37) = -3180.D0/4889.D0
         pd(52,15) = 0
         pd(55,32) = 0
         pd(5,7) = 0
         pd(7,23) = 0
         pd(9,26) = 0
         pd(13,38) = 0
         pd(16,34) = 0
         pd(19,39) = 0
         pd(31,23) = 0
         pd(33,9) = 0
         pd(37,24) = 0
         pd(46,43) = 46299.D0/244450.D0/delt
         pd(53,45) = 0
         pd(3,35) = 0
         pd(4,50) = -4435535.D0/352008.D0*por(j)/(-1+por(j))/delt
         pd(8,4) = 0
         pd(10,3) = 0
         pd(12,48) = 0
         pd(14,7) = (0.1D1-sw17)*kdis*sw18*sp(14,j)+sw17*kgas
         pd(18,47) = 0
         pd(23,8) = 0
         pd(32,12) = 0
         pd(39,15) = 0
         pd(44,29) = 0
         pd(47,22) = 0
         pd(50,53) = 0
         pd(54,10) = 0
         pd(6,16) = -7.D0/424.D0/delt
         pd(11,1) = 0
         pd(11,18) = 0
         pd(12,8) = 0
         pd(18,2) = 0
         pd(21,3) = 0
         pd(24,19) = 0
         pd(30,3) = 0
         pd(31,31) = 0
         pd(42,3) = 0
         pd(43,43) = 0
         pd(45,28) = 4*(-1+por(j))/por(j)/delt
         pd(5,31) = 0
         pd(12,23) = 0
         pd(15,42) = 0
         pd(21,20) = 0
         pd(22,55) = 0
         pd(24,36) = 0
         pd(27,19) = 0
         pd(28,46) = 3845.D0/19556.D0*por(j)/(-1+por(j))/delt
         pd(30,20) = 0
         pd(31,48) = -7.D0/10.D0*sw27*kplagio*(sp(20,j)**3/sp(42,j))**0.
     +35D0*(0.1D1-sp(40,j)**0.23D1*sp(46,j)**0.17D1*sp(49,j)**0.3D0*sp(2
     +2,j)**0.7D0/kspplagio)-7.D0/10.D0/delt
         pd(42,18) = 0
         pd(44,1) = 0
         pd(45,45) = -1044.D0/4889.D0/delt
         pd(55,12) = 0
         pd(2,11) = -kfeo2*sp(2,j)/4-65.D0/6.D0/delt
         pd(2,54) = 0
         pd(7,4) = (-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*(
     +-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw
     +06)/kmmno2+sw07*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3))*(0.1D1-sw06)/kmmno2*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw
     +10+(0.1D1-sw10)*sp(6,j)/kmso4)/2
         pd(8,48) = 0
         pd(9,7) = 0
         pd(13,18) = 0
         pd(15,5) = 0
         pd(16,54) = 0
         pd(17,48) = 0.1002389D7/586680.D0*(-1+por(j))/por(j)/delt
         pd(19,19) = 1
         pd(31,3) = 0
         pd(34,22) = 0
         pd(37,4) = 0
         pd(46,23) = 0
         pd(51,48) = -11823.D0/24445.D0
         pd(53,25) = 0
         pd(4,31) = -(174195*por(j)-85436)/por(j)/delt/4134
         pd(6,40) = 261.D0/4889.D0/delt
         pd(9,21) = 0
         pd(12,30) = 0
         pd(18,27) = 0
         pd(25,8) = 0
         pd(27,27) = 8.D0/3.D0/delt
         pd(38,32) = 0
         pd(40,26) = 0
         pd(47,2) = 0
         pd(50,33) = 0
         pd(52,49) = 0
         pd(3,4) = 0
         pd(25,40) = 0
         pd(28,10) = -3.D0/8.D0*por(j)/(-1+por(j))/delt
         pd(35,35) = 1
         pd(43,23) = 0
         pd(48,34) = 0
         pd(51,32) = 0
         pd(51,51) = 1
         pd(55,55) = 1
         pd(3,21) = -69.D0/689.D0*(-1+por(j))/por(j)/delt
         pd(15,26) = 0
         pd(28,27) = kfess0*sp(25,j)/8-por(j)/(-1+por(j))/delt/8
         pd(31,28) = 0
         pd(43,40) = 0
         pd(45,25) = 2*(-1+por(j))/por(j)/delt
         pd(47,53) = 0
         pd(48,51) = 0
         pd(6,2) = -1/delt/4
         pd(10,27) = 0
         pd(20,50) = 0
         pd(22,28) = 0
         pd(24,5) = ksfp*sp(9,j)
         pd(29,44) = 0
         pd(34,2) = 0
         pd(36,52) = 0
         pd(49,34) = 0
         pd(4,12) = kh2sfeoh3*sp(5,j)+kh2smno2pr*sp(33,j)-53.D0/3.D0*por
     +(j)/(-1+por(j))/delt
         pd(9,2) = 0
         pd(10,40) = 0
         pd(14,46) = 0
         pd(16,13) = 0
         pd(18,7) = 0
         pd(27,8) = 4.D0/3.D0/delt
         pd(32,52) = 0
         pd(38,12) = 0
         pd(50,13) = 0
         pd(52,29) = 0
         pd(54,50) = 29225.D0/29334.D0*por(j)/(-1+por(j))
         pd(1,39) = 0
         pd(9,39) = 0
         pd(11,15) = 0
         pd(27,40) = -5568.D0/4889.D0/delt
         pd(30,43) = 0
         pd(34,55) = 0
         pd(35,15) = 0
         pd(36,19) = 0
         pd(42,41) = 0
         pd(48,14) = 0
         pd(51,12) = 0
         pd(55,35) = 0
         pd(3,1) = 46.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sw
     +03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,
     +j)/kmno3)
         pd(9,55) = 0
         pd(23,18) = 0
         pd(25,37) = 0
         pd(28,7) = 99.D0/424.D0*por(j)/(-1+por(j))/delt
         pd(35,32) = 0
         pd(43,20) = 3*sp(45,j)*sp(20,j)**2
         pd(48,31) = 0
         pd(51,29) = 0
         pd(55,52) = 0
         pd(3,40) = 0
         pd(7,42) = 0
         pd(8,9) = 0
         pd(15,38) = 0
         pd(17,12) = -53.D0/6.D0/delt
         pd(20,30) = 0
         pd(22,8) = 0
         pd(23,40) = 0
         pd(26,20) = 0
         pd(29,24) = 0
         pd(33,29) = 0
         pd(36,5) = 0
         pd(36,11) = 0
         pd(37,44) = 0
         pd(41,43) = 0
         pd(49,14) = 0
         pd(2,19) = 0
         pd(7,13) = kfesprecip*sw13*sp(11,j)/sp(20,j)/KsFeS-(-1+por(j))/
     +por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(11,j)/sp(20,j)/KsFeS-1/de
     +lt
         pd(13,27) = 3*kdi*sw19+3/delt
         pd(15,14) = 0
         pd(19,28) = 0
         pd(31,12) = 0
         pd(34,31) = 0
         pd(37,13) = 0
         pd(45,9) = 0
         pd(46,32) = 0
         pd(53,34) = 0
         pd(1,2) = 0
         pd(2,23) = 569.D0/156.D0*(-1+por(j))/por(j)/delt
         pd(5,1) = 0
         pd(7,18) = 0
         pd(13,32) = 3.D0/2.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))**2
     +*sp(1,j)*sw11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw
     +05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-s
     +w02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1
     +-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-
     +sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(
     +3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)
     +)*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/k
     +mfeoh3)-sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw0
     +2-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw0
     +5*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw
     +02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-
     +sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-s
     +w03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3
     +,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))
     +*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/km
     +feoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))+3.D0/2.D0*(-1+por(j))/p
     +or(j)*nu/(inita+sp(32,j))**2*sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)
     +*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0
     +.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/km
     +o2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*
     +sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw0
     +2-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/
     +kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw
     +02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04
     ++(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*
     +(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/km
     +so4)
         pd(16,28) = 0
         pd(19,33) = 0
         pd(23,24) = 0
         pd(31,17) = 0
         pd(33,3) = 0
         pd(34,36) = 0
         pd(37,18) = 0
         pd(46,37) = -94001.D0/39112.D0*(-1+por(j))/por(j)/delt
         pd(53,39) = 0
         pd(8,29) = 0
         pd(17,31) = -(-1+por(j))*(213*por(j)-106)/por(j)**2/delt/12
         pd(20,7) = 0
         pd(32,38) = 0
         pd(35,46) = 0
         pd(39,41) = 0
         pd(41,18) = 0
         pd(44,55) = 0
         pd(46,4) = 0
         pd(47,48) = 0
         pd(54,36) = -5900.D0/4889.D0
         pd(1,16) = 0
         pd(8,34) = 0
         pd(14,37) = 0
         pd(17,36) = 98929.D0/19556.D0*(-1+por(j))/por(j)/delt
         pd(29,4) = 0
         pd(32,43) = 0
         pd(35,51) = 0
         pd(38,3) = 0
         pd(39,46) = 0
         pd(41,23) = 0
         pd(46,9) = 0
         pd(54,41) = 2800.D0/4889.D0
         pd(1,20) = 0
         pd(2,43) = -351715.D0/29334.D0/delt
         pd(10,36) = 0
         pd(13,7) = -3*kaomo2*sp(2,j)-3/delt
         pd(16,45) = 0
         pd(19,8) = 0
         pd(29,53) = 0
         pd(34,11) = por(j)/(-1+por(j))/delt
         pd(38,52) = 0
         pd(40,1) = 0
         pd(40,46) = 0
         pd(43,4) = 0
         pd(49,43) = 0
         pd(53,14) = 0
         pd(2,48) = 245869.D0/117336.D0*(-1+por(j))/por(j)/delt
         pd(8,42) = 0
         pd(9,44) = 0
         pd(13,12) = 3/delt
         pd(17,44) = -314449.D0/58668.D0/delt
         pd(19,13) = 0
         pd(34,16) = 0
         pd(40,6) = 0
         pd(43,9) = 0
         pd(46,17) = 0
         pd(49,48) = -3.D0/10.D0*(-1+por(j))/por(j)
         pd(53,19) = 0
         pd(1,30) = 0
         pd(10,9) = 0
         pd(12,54) = 0
         pd(14,13) = 0
         pd(18,53) = 0
         pd(32,18) = 0
         pd(39,21) = 0
         pd(44,35) = 0
         pd(47,28) = 0
         pd(54,16) = 0
         pd(10,14) = 0
         pd(14,18) = 0
         pd(32,23) = 0
         pd(39,26) = 0
         pd(41,3) = 0
         pd(44,40) = 0
         pd(47,33) = 0
         pd(54,21) = -5900.D0/4889.D0
         pd(3,49) = 0
         pd(7,51) = 0
         pd(8,18) = 0
         pd(10,17) = (-1+por(j))/por(j)*kmnco3precip*sw12*sp(10,j)/KsMnC
     +O3
         pd(17,21) = 98929.D0/19556.D0*(-1+por(j))/por(j)/delt
         pd(20,39) = 0
         pd(22,17) = -sw22*karappt*sp(22,j)/kspara+(-1+por(j))/por(j)*sw
     +21*karadis*sp(36,j)*sp(22,j)/kspara
         pd(23,21) = 0
         pd(23,49) = 0
         pd(29,33) = 0
         pd(36,9) = 0
         pd(36,30) = 0
         pd(37,52) = 0
         pd(41,52) = 0
         pd(49,23) = 0
         pd(3,54) = 0
         pd(5,39) = 0
         pd(8,23) = -1.D0+por(j)
         pd(20,44) = 0
         pd(22,22) = -sw22*karappt*sp(17,j)/kspara+(-1+por(j))/por(j)*sw
     +21*karadis*sp(36,j)*sp(17,j)/kspara
         pd(23,54) = 0
         pd(29,38) = 0
         pd(49,28) = 0
         pd(2,29) = 65.D0/6.D0*(-1+por(j))/por(j)/delt
         pd(4,37) = 0.86293733D8/0.6737042D7/delt
         pd(6,46) = 3845.D0/19556.D0/delt
         pd(18,33) = 0
         pd(21,34) = 0
         pd(25,14) = 0
         pd(34,42) = 0
         pd(38,38) = 0.256D1*(-1+por(j))/por(j)*sw28*kolive*sp(51,j)*sp(
     +40,j)*sp(38,j)**0.6D0*sp(11,j)**0.4D0/sp(20,j)**4/kspolive-5120.D0
     +/4889.D0/delt
         pd(39,1) = 0
         pd(40,32) = 0
         pd(44,15) = 0
         pd(47,8) = 0
         pd(50,39) = 0.1894351D7/0.41843D7*(-1+por(j))/por(j)/delt
         pd(1,10) = 0
         pd(2,34) = 0
         pd(4,41) = -11310.D0/4889.D0/delt
         pd(6,51) = 0
         pd(11,50) = 0
         pd(12,40) = 0
         pd(18,38) = 0
         pd(21,39) = 0
         pd(25,19) = 0
         pd(27,37) = -9246472.D0/777351.D0*(-1+por(j))/por(j)/delt
         pd(38,43) = -1848.D0/24445.D0/delt
         pd(39,6) = 0
         pd(40,37) = 0
         pd(44,20) = 4*sp(46,j)*sp(20,j)**3
         pd(47,13) = 0
         pd(50,44) = 38596.D0/41843.D0/delt
         pd(53,5) = 0
         pd(7,32) = -(-1+por(j))/por(j)*nu/(inita+sp(32,j))**2*sp(1,j)*s
     +w09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(
     +4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3
     +)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0
     +.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(
     +sw10+(0.1D1-sw10)*sp(6,j)/kmso4)/2
         pd(13,47) = 0
         pd(15,29) = 0
         pd(19,48) = 0
         pd(20,19) = 0
         pd(23,29) = 0
         pd(26,9) = 0
         pd(29,13) = 2*kh2sfeoh3mr*sp(34,j)
         pd(33,18) = 0
         pd(37,33) = 0
         pd(41,32) = 0
         pd(48,54) = 0
         pd(49,3) = 0
         pd(53,54) = 0
         pd(1,5) = 0
         pd(7,36) = 0
         pd(13,52) = 0
         pd(15,34) = 0
         pd(17,6) = 0
         pd(19,53) = 0
         pd(20,24) = 0
         pd(22,2) = 0
         pd(23,34) = 0
         pd(26,14) = 0
         pd(29,18) = 0
         pd(33,23) = 0
         pd(37,38) = 0
         pd(41,37) = 0
         pd(49,8) = 0
         pd(6,26) = -(-1+por(j))*(311*por(j)-212)/por(j)**2/delt/424
         pd(10,46) = 0
         pd(11,8) = 0
         pd(14,52) = 0
         pd(18,13) = 0
         pd(27,14) = -264.D0/53.D0/delt
         pd(38,18) = 0
         pd(40,12) = 0
         pd(42,13) = 0
         pd(49,54) = 0
         pd(50,19) = 0
         pd(52,35) = 0
         pd(4,22) = -502775.D0/14667.D0*por(j)/(-1+por(j))/delt
         pd(6,31) = -3.D0/424.D0*(-1+por(j))*(86*por(j)-53)/por(j)**2/de
     +lt
         pd(8,53) = 0
         pd(9,12) = 0
         pd(10,51) = 0
         pd(11,32) = 0
         pd(16,21) = 0
         pd(18,18) = sp(20,j)
         pd(38,23) = 0
         pd(40,17) = 0
         pd(50,24) = 0
         pd(52,40) = 660.D0/4889.D0*por(j)/(-1+por(j))
         pd(5,12) = 8*por(j)/(-1+por(j))*kh2so2*sp(2,j)+16*por(j)/(-1+po
     +r(j))/delt
         pd(7,28) = 3*(-1+por(j))/por(j)/delt
         pd(9,31) = 0
         pd(13,43) = 0
         pd(19,44) = 0
         pd(26,5) = 0
         pd(33,14) = 0
         pd(34,46) = 0
         pd(37,29) = 0
         pd(46,48) = -15338999.D0/0.9778D7*(-1+por(j))/por(j)/delt
         pd(48,5) = 0
         pd(51,3) = 0
         pd(53,50) = -5.D0/3.D0*por(j)/(-1+por(j))
         pd(8,28) = 0
         pd(14,32) = 0
         pd(20,6) = 0
         pd(32,37) = 0
         pd(35,45) = 0
         pd(39,40) = sw25*(kBSidis*bd+(kBSidis-kBSidis*bd)*exp(-ad*x_pos
     +))*sp(39,j)/BSisat
         pd(41,17) = 0
         pd(44,54) = 0
         pd(46,3) = 0
         pd(47,47) = 0
         pd(54,35) = 0
         pd(2,24) = 0
         pd(5,2) = 8*por(j)/(-1+por(j))*kh2so2*(sp(12,j)+sp(13,j))
         pd(7,19) = 0
         pd(13,33) = 0
         pd(16,29) = 0
         pd(19,34) = 0
         pd(31,18) = 0
         pd(33,4) = 1
         pd(34,37) = 0
         pd(37,19) = 0
         pd(46,38) = -0.117852D1*(-1+por(j))/por(j)*sw29*kpyrox*sp(52,j)
     +*sp(40,j)**2*sp(11,j)**0.46D0/sp(38,j)**0.16D0*sp(22,j)**0.7D0/sp(
     +20,j)**4/ksppyrox+46299.D0/391120.D0/delt
         pd(53,40) = 0
         pd(1,52) = 0
         pd(14,22) = 0
         pd(32,27) = 0
         pd(39,30) = 0
         pd(41,7) = 0
         pd(44,44) = 0
         pd(47,37) = 0
         pd(54,25) = 0
         pd(2,16) = 7329.D0/1378.D0/delt
         pd(7,9) = 0
         pd(13,23) = 0
         pd(15,10) = 0
         pd(17,53) = 0
         pd(19,24) = 0
         pd(31,8) = 0
         pd(34,27) = 0
         pd(36,45) = 0
         pd(37,9) = 0
         pd(45,5) = 12.D0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*s
     +p(1,j)*sw11*(-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno
     +3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(
     +0.1D1-sw06)*sp(4,j)/kmmno2))*(0.1D1-sw08)/kmfeoh3+sw09*sw07*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(
     +sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno
     +2))*(0.1D1-sw08)/kmfeoh3*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))-12.D0/
     +53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw09*sw07*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2
     +)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/km
     +mno2))*(0.1D1-sw08)/kmfeoh3*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+12.D
     +0/53.D0*(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw07*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(
     +sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno
     +2))*(0.1D1-sw08)/kmfeoh3
         pd(46,28) = 0
         pd(53,30) = 0
         pd(1,29) = 0
         pd(4,55) = 0
         pd(10,8) = 0
         pd(12,53) = 0
         pd(14,12) = 0
         pd(18,52) = 0
         pd(32,17) = 0
         pd(39,20) = 0
         pd(44,34) = 0
         pd(47,27) = 0
         pd(54,15) = 0
         pd(2,6) = -(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw11*
     +sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp
     +(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno
     +3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(
     +0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*
     +(0.1D1-sw10)/kmso4+(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*
     +sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp
     +(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno
     +3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(
     +0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))*
     +(0.1D1-sw10)/kmso4
         pd(2,49) = 0
         pd(8,43) = 0
         pd(9,45) = 0
         pd(13,13) = -3*kfesprecip*sw13*sp(11,j)/sp(20,j)/KsFeS+3*(-1+po
     +r(j))/por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(11,j)/sp(20,j)/KsFe
     +S+3/delt
         pd(19,14) = 0
         pd(34,17) = 0
         pd(40,7) = 0
         pd(40,51) = 0
         pd(43,10) = 0
         pd(46,18) = 0
         pd(49,49) = 1
         pd(53,20) = 0
         pd(3,30) = 0
         pd(4,45) = -11310.D0/4889.D0*por(j)/(-1+por(j))/delt
         pd(6,55) = 0
         pd(11,10) = 0
         pd(11,54) = 0
         pd(12,43) = 0
         pd(14,2) = 0
         pd(17,2) = -(-1+por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*(0.1
     +D1-sw02)/kmo2
         pd(18,42) = 0
         pd(21,43) = 0
         pd(32,7) = 0
         pd(39,10) = 0
         pd(39,55) = 0
         pd(44,24) = 0
         pd(47,17) = 0
         pd(50,48) = -249526.D0/209215.D0*(-1+por(j))/por(j)/delt
         pd(54,5) = 0
         pd(6,7) = 99.D0/424.D0/delt
         pd(10,32) = 0
         pd(20,55) = 0
         pd(24,10) = 0
         pd(29,49) = 0
         pd(34,7) = 0
         pd(49,39) = 0
         pd(2,28) = 53.D0/3.D0*(-1+por(j))/por(j)/delt
         pd(4,36) = 0.86293733D8/0.6737042D7/delt
         pd(6,45) = -261.D0/4889.D0/delt
         pd(18,32) = 0
         pd(21,33) = 0
         pd(25,13) = kfesdiss*(0.1D1-sw13)*sp(25,j)*sp(11,j)/sp(20,j)/Ks
     +FeS-por(j)/(-1+por(j))*kfesprecip*sw13*sp(11,j)/sp(20,j)/KsFeS
         pd(34,41) = 0
         pd(38,37) = -5088.D0/4889.D0*(-1+por(j))/por(j)/delt
         pd(40,31) = 0
         pd(44,14) = 0
         pd(47,7) = 0
         pd(50,38) = 11900.D0/41843.D0/delt
         pd(3,55) = 0
         pd(5,40) = 0
         pd(8,24) = 0
         pd(20,45) = 0
         pd(22,23) = 0
         pd(23,55) = 0
         pd(29,39) = 0
         pd(49,29) = 0
         pd(1,49) = 0
         pd(4,26) = -(178329*por(j)-89570)/por(j)/delt/4134
         pd(6,35) = 0
         pd(9,16) = 0
         pd(10,55) = 0
         pd(11,36) = 0
         pd(12,26) = 0
         pd(18,22) = 0
         pd(25,3) = 0
         pd(27,22) = 247520.D0/14667.D0/delt
         pd(38,27) = 0
         pd(40,21) = 0
         pd(50,28) = 0
         pd(52,44) = -660.D0/4889.D0*por(j)/(-1+por(j))
         pd(3,45) = 0
         pd(7,47) = 0
         pd(8,14) = 0
         pd(16,8) = 0
         pd(17,17) = 107.D0/12.D0/delt
         pd(20,35) = 0
         pd(22,13) = 0
         pd(23,45) = 0
         pd(26,25) = 0
         pd(29,29) = 0
         pd(33,34) = 0
         pd(37,48) = 0
         pd(41,48) = 0
         pd(49,19) = 0
         pd(6,25) = 0
         pd(10,45) = 0
         pd(12,18) = 0
         pd(14,51) = 0
         pd(16,17) = sp(20,j)
         pd(18,12) = 0
         pd(27,13) = 8.D0/3.D0*(-1+por(j))/por(j)*kpyr*sp(25,j)-16.D0/3.
     +D0/delt
         pd(38,17) = 0
         pd(40,11) = 0
         pd(49,53) = 0
         pd(50,18) = 0
         pd(52,34) = 0
         pd(54,55) = 0
         pd(7,37) = 0
         pd(13,53) = 0
         pd(15,48) = 0
         pd(17,7) = 1/delt/12
         pd(19,54) = 0
         pd(20,25) = 0
         pd(22,3) = 0
         pd(23,35) = 0
         pd(26,15) = 0
         pd(29,19) = 0
         pd(33,24) = 0
         pd(37,39) = 0
         pd(41,38) = 0
         pd(49,9) = 0
         pd(4,7) = 15725.D0/4134.D0*por(j)/(-1+por(j))/delt
         pd(8,38) = 0
         pd(14,41) = 0
         pd(17,40) = 4611.D0/4889.D0/delt
         pd(22,42) = 0
         pd(26,54) = 0
         pd(27,3) = 4.D0/3.D0/delt
         pd(32,47) = 0
         pd(38,7) = 0
         pd(46,13) = 0
         pd(50,8) = 0
         pd(52,24) = 0
         pd(54,45) = 2800.D0/4889.D0*por(j)/(-1+por(j))
         pd(5,44) = 0
         pd(11,45) = 0
         pd(12,35) = 0
         pd(24,50) = 0
         pd(26,39) = 0
         pd(27,32) = 0
         pd(30,34) = 0
         pd(33,47) = 0
         pd(35,6) = 0
         pd(42,32) = 0
         pd(52,9) = 0
         pd(52,54) = 0
         pd(55,26) = 0
         pd(5,49) = 0
         pd(16,2) = 0
         pd(22,32) = 0
         pd(24,55) = 0
         pd(26,44) = 0
         pd(30,39) = 0
         pd(33,52) = 0
         pd(35,11) = -por(j)/(-1+por(j))
         pd(42,37) = 0
         pd(51,36) = -3180.D0/4889.D0
         pd(52,14) = 0
         pd(55,31) = 0
         pd(1,11) = 0
         pd(5,35) = 0
         pd(15,46) = 0
         pd(21,24) = 0
         pd(24,40) = 0
         pd(26,29) = 0
         pd(28,50) = -68239.D0/234672.D0*por(j)/(-1+por(j))/delt
         pd(30,24) = -1/delt
         pd(31,52) = 0
         pd(36,47) = 0
         pd(42,22) = 0
         pd(44,5) = 0
         pd(45,49) = 0
         pd(55,16) = 0
         pd(21,29) = 0
         pd(24,45) = 0
         pd(26,34) = 0
         pd(28,55) = 0
         pd(30,29) = 0
         pd(33,42) = 0
         pd(35,1) = 0
         pd(42,27) = 0
         pd(44,10) = 0
         pd(45,54) = 0
         pd(52,4) = 0
         pd(55,21) = 5015.D0/4889.D0
         pd(4,17) = 88759.D0/4134.D0*por(j)/(-1+por(j))/delt
         pd(5,25) = -16/delt
         pd(11,27) = 0
         pd(21,14) = 0
         pd(22,50) = 0
         pd(24,30) = -1
         pd(28,40) = 261.D0/4889.D0*por(j)/(-1+por(j))/delt
         pd(30,14) = -por(j)/(-1+por(j))/delt/106
         pd(31,42) = 0.245D0*sw27*kplagio*sp(48,j)/(sp(20,j)**3/sp(42,j)
     +)**0.65D0*(0.1D1-sp(40,j)**0.23D1*sp(46,j)**0.17D1*sp(49,j)**0.3D0
     +*sp(22,j)**0.7D0/kspplagio)*sp(20,j)**3/sp(42,j)**2
         pd(43,54) = 0
         pd(45,39) = -401889.D0/0.19556D7*(-1+por(j))/por(j)/delt
         pd(55,6) = 0
         pd(5,30) = 0
         pd(12,22) = 0
         pd(21,19) = 0
         pd(22,54) = 0
         pd(23,4) = 0
         pd(24,35) = ksfp*sp(9,j)
         pd(28,45) = -261.D0/4889.D0*por(j)/(-1+por(j))/delt
         pd(30,19) = 0
         pd(31,47) = 0
         pd(42,17) = 0
         pd(45,44) = -5933.D0/4889.D0/delt
         pd(55,11) = 0
         pd(5,16) = 0
         pd(6,17) = 99.D0/424.D0/delt
         pd(11,2) = 0
         pd(11,19) = 0
         pd(12,9) = 0
         pd(18,3) = 0
         pd(21,4) = 0
         pd(24,20) = 0
         pd(28,30) = 0
         pd(30,4) = 0
         pd(31,32) = 0
         pd(42,4) = 0
         pd(43,44) = 0
         pd(45,29) = 2*(-1+por(j))/por(j)/delt
         pd(1,35) = 0
         pd(5,20) = 0
         pd(6,21) = 0.1155809D7/0.2072936D7*(-1+por(j))/por(j)/delt
         pd(11,7) = 0
         pd(11,23) = 0
         pd(12,14) = 0
         pd(21,9) = 0
         pd(22,46) = 0
         pd(23,17) = -6.D0/53.D0*por(j)/(-1+por(j))*kfeco3precip*sw14*sp
     +(11,j)/KsFeCO3
         pd(24,25) = 0
         pd(28,35) = 0
         pd(30,9) = por(j)/(-1+por(j))*sw16*kapa+por(j)/(-1+por(j))/delt
         pd(31,37) = 0
         pd(42,9) = 0
         pd(43,49) = 0
         pd(45,34) = 0
         pd(55,1) = 0
         pd(3,15) = 69.D0/689.D0/delt
         pd(25,51) = 0
         pd(28,21) = 0.1155809D7/0.2072936D7/delt
         pd(43,34) = 0
         pd(45,19) = 0
         pd(48,45) = 0.1004232D8/0.29622451D8*por(j)/(-1+por(j))/delt
         pd(3,20) = 0
         pd(20,11) = 0
         pd(28,26) = -(569*por(j)-371)/por(j)/delt/848
         pd(31,27) = 0
         pd(43,39) = 0
         pd(45,24) = 0
         pd(47,52) = 0
         pd(48,50) = 0.34938905D8/0.59244902D8*por(j)/(-1+por(j))/delt
         pd(3,5) = 0
         pd(25,41) = 0
         pd(28,11) = -7.D0/16.D0*por(j)/(-1+por(j))/delt
         pd(35,36) = 0
         pd(43,24) = 0
         pd(48,35) = 0
         pd(51,33) = 0
         pd(51,52) = 0
         pd(1,21) = 0
         pd(3,10) = 0
         pd(15,18) = 0
         pd(25,46) = 0
         pd(28,16) = -7.D0/424.D0*por(j)/(-1+por(j))/delt
         pd(35,41) = 0
         pd(36,17) = 0
         pd(43,29) = 0
         pd(45,14) = 151.D0/689.D0/delt
         pd(48,40) = -502116.D0/212065.D0*por(j)/(-1+por(j))*sw31*ksmect
     +*sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)**0.17D1*sp(40,j)**3/s
     +p(20,j)**6/kspsmect-10042320.D0/0.29622451D8*por(j)/(-1+por(j))/de
     +lt
         pd(51,54) = 0
         pd(21,53) = 0
         pd(25,31) = 0
         pd(27,50) = 272956.D0/44001.D0/delt
         pd(28,1) = 0
         pd(30,54) = 0
         pd(35,26) = -(-1+por(j))/por(j)
         pd(40,54) = 0
         pd(42,51) = 0
         pd(43,14) = 0
         pd(48,25) = 0
         pd(51,23) = 0
         pd(55,46) = -9649.D0/9778.D0*por(j)/(-1+por(j))
         pd(9,54) = 0
         pd(25,36) = 0
         pd(27,55) = 0
         pd(28,6) = 0
         pd(35,31) = 0
         pd(43,19) = 0
         pd(48,30) = 0
         pd(51,28) = 0
         pd(55,51) = 0
         pd(1,40) = 0
         pd(25,23) = 0
         pd(30,44) = 0
         pd(35,16) = 0
         pd(36,20) = 0
         pd(42,42) = -eq2_al
         pd(48,15) = 0
         pd(51,13) = 0
         pd(55,36) = 5015.D0/4889.D0
         pd(16,50) = 0
         pd(21,48) = 0
         pd(25,27) = 0
         pd(27,45) = 5568.D0/4889.D0/delt
         pd(30,49) = 0
         pd(35,21) = 0
         pd(42,47) = 0
         pd(48,20) = 753174.D0/212065.D0*por(j)/(-1+por(j))*sw31*ksmect*
     +sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)**0.17D1*sp(40,j)**4/sp
     +(20,j)**7/kspsmect
         pd(51,18) = 0
         pd(55,41) = -2380.D0/4889.D0
      end
