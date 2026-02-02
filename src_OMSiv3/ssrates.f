c      
c     SUBROUTINE ssrates
c      
      subroutine ssrates(rat,drdc,isp,j)
        include 'common_geo.inc'
        include 'common.inc'
        call switches(j)
            if (isp.eq.1) then
            rat = 0.D0
            drdc = 0
            endif
            if (isp.eq.2) then
            rat = -65.D0/53.D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))
     +*sp(1,j)*(sw02+(0.1D1-sw02)*sp(2,j)/kmo2)-0.2D1*knit*sp(8,j)*sp(2,
     +j)-0.2D1*kaomo2*sp(7,j)*sp(2,j)-0.2D1*kh2so2*(sp(12,j)+sp(13,j))*s
     +p(2,j)-0.25D0*kfeo2*sp(11,j)*sp(2,j)-0.2D1*(0.1D1-por(j))/por(j)*k
     +feso2*sp(25,j)*sp(2,j)-0.5D0*kmnox*sp(10,j)*sp(2,j)
            drdc = -65.D0/53.D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j)
     +)*sp(1,j)*(0.1D1-sw02)/kmo2-0.2D1*knit*sp(8,j)-0.2D1*kaomo2*sp(7,j
     +)-0.2D1*kh2so2*(sp(12,j)+sp(13,j))-0.25D0*kfeo2*sp(11,j)-0.2D1*(0.
     +1D1-por(j))/por(j)*kfeso2*sp(25,j)-0.5D0*kmnox*sp(10,j)
            endif
            if (isp.eq.3) then
            rat = -0.8679245284D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,
     +j))*sp(1,j)*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1
     +D1-sw04)*sp(3,j)/kmno3)+knit*sp(8,j)*sp(2,j)
            drdc = -0.8679245284D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32
     +,j))*sp(1,j)*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(0.1D1-sw
     +04)/kmno3
            endif
            if (isp.eq.4) then
            rat = -0.2D1*nu/(inita+sp(32,j))*sp(1,j)*sw05*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2
     +)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/km
     +mno2)+kmnox*sp(10,j)*sp(2,j)/(0.1D1-por(j))*por(j)-kfemno2*sp(11,j
     +)*sp(4,j)-kh2smno2*sp(4,j)*(sp(12,j)+sp(13,j))-kmnage*sp(4,j)
            drdc = -0.2D1*nu/(inita+sp(32,j))*sp(1,j)*sw05*(0.1D1-sw02-(
     +0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(0.1D1-sw06)/kmmno2-kfemno2*
     +sp(11,j)-kh2smno2*(sp(12,j)+sp(13,j))-kmnage
            endif
            if (isp.eq.5) then
            rat = -4*nu/(inita+sp(32,j))*sp(1,j)*sw07*(0.1D1-sw02-(0.1D1
     +-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(s
     +w04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-
     +sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0
     +.1D1-sw08)*sp(5,j)/kmfeoh3)-0.2D1*kh2sfeoh3*sp(5,j)*(sp(12,j)+sp(1
     +3,j))+0.1D1*kfeo2*sp(11,j)*sp(2,j)/(0.1D1-por(j))*por(j)+0.2D1*kfe
     +mno2*sp(11,j)*sp(4,j)+0.2D1*kfemno2pr*sp(11,j)*sp(33,j)-kfeage*sp(
     +5,j)
            drdc = -4*nu/(inita+sp(32,j))*sp(1,j)*sw07*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(
     +sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(
     +2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1
     +-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(0.1D1-
     +sw08)/kmfeoh3-0.2D1*kh2sfeoh3*(sp(12,j)+sp(13,j))-kfeage
            endif
            if (isp.eq.6) then
            rat = -0.5D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,
     +j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)
     +*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(
     +0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/k
     +mno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw0
     +6+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3
     +))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)-kaom*sp(7,j)*sp(6,j)+0.5D0*kh
     +2so2*(sp(12,j)+sp(13,j))*sp(2,j)+(0.1D1-por(j))/por(j)*kfeso2*sp(2
     +5,j)*sp(2,j)
            drdc = -0.5D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1
     +,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*
     +sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06
     +)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*
     +(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/
     +kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw
     +06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh
     +3))*(0.1D1-sw10)/kmso4-kaom*sp(7,j)
            endif
            if (isp.eq.7) then
            rat = 0.5D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j
     +)*sw11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp
     +(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*
     +sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(
     +0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06
     ++(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)
     +-sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D
     +1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(
     +2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*s
     +p(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmn
     +o3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+
     +(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3))
     +*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))-kaom*sp(7,j)*sp(6,j)-kaomo2*sp
     +(7,j)*sp(2,j)+(0.1D1-sw17)*kdis*sw18*sp(14,j)*(ch4eq-sp(7,j))-sw17
     +*kgas*(sp(7,j)-ch4eq)
            drdc = -kaom*sp(6,j)-kaomo2*sp(2,j)-(0.1D1-sw17)*kdis*sw18*s
     +p(14,j)-sw17*kgas
            endif
            if (isp.eq.8) then
            rat = 6.D0/53.D0*(0.1D1-por(j))/por(j)*(nu/(inita+sp(32,j))*
     +sp(1,j)*(sw02+(0.1D1-sw02)*sp(2,j)/kmo2)+nu/(inita+sp(32,j))*sp(1,
     +j)*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(
     +0.1D1-sw06)*sp(4,j)/kmmno2)+nu/(inita+sp(32,j))*sp(1,j)*sw07*(0.1D
     +1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(
     +2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*
     +(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmn
     +o2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)+nu/(inita+sp(32,j))*sp(1,
     +j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)
     +*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(
     +0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/k
     +mno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw0
     +6+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3
     +))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+nu/(inita+sp(32,j))*sp(1,j)*s
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
     +w10+(0.1D1-sw10)*sp(6,j)/kmso4)))-knit*sp(8,j)*sp(2,j)-(kf6*sp(8,j
     +)*por(j)-kb6*sp(23,j)*(1.D0-por(j)))/por(j)
            drdc = -knit*sp(2,j)-kf6
            endif
            if (isp.eq.9) then
            rat = (0.1D1-por(j))/por(j)*(nu/(inita+sp(32,j))*sp(1,j)*(sw
     +02+(0.1D1-sw02)*sp(2,j)/kmo2)+nu/(inita+sp(32,j))*sp(1,j)*sw03*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmn
     +o3)+nu/(inita+sp(32,j))*sp(1,j)*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-
     +sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)+nu/(inita
     ++sp(32,j))*sp(1,j)*sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03
     +*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)
     +/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw0
     +2-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(s
     +w06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeo
     +h3)+nu/(inita+sp(32,j))*sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2
     +,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-
     +sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw
     +03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,
     +j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)
     +*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1
     +D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08
     ++(0.1D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+
     +nu/(inita+sp(32,j))*sp(1,j)*sw11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/
     +kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04
     +)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(
     +0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/k
     +mno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-
     +sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw
     +04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,
     +j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-s
     +w04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.
     +1D1-sw08)*sp(5,j)/kmfeoh3)-sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/k
     +mo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)
     +*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-s
     +w02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw0
     +4+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j
     +)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw
     +04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1
     +D1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)))/106
     +-sw16*kapa*(sp(9,j)-po4_eq)-(kf5*sp(9,j)*por(j)-kb5*sp(24,j)*(1.D0
     +-por(j)))/por(j)-(kf7*(sp(5,j)+sp(34,j)+sp(35,j))*sp(9,j)-kb7*sp(3
     +0,j))/por(j)
            drdc = -sw16*kapa-kf5-kf7*(sp(5,j)+sp(34,j)+sp(35,j))/por(j)
            endif
            if (isp.eq.10) then
            rat = 0.2D1*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j
     +)*sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0
     +.1D1-sw06)*sp(4,j)/kmmno2)-kmnox*sp(10,j)*sp(2,j)+(kfemno2*sp(11,j
     +)*sp(4,j)+kfemno2pr*sp(11,j)*sp(33,j))*(0.1D1-por(j))/por(j)+(kh2s
     +mno2*sp(4,j)*(sp(12,j)+sp(13,j))+kh2smno2pr*sp(33,j)*(sp(12,j)+sp(
     +13,j)))*(0.1D1-por(j))/por(j)-kmnco3precip*sw12*(sp(10,j)*sp(17,j)
     +/KsMnCO3-0.1D1)
            drdc = -kmnox*sp(2,j)-kmnco3precip*sw12*sp(17,j)/KsMnCO3
            endif
            if (isp.eq.11) then
            rat = 0.4D1*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j
     +)*sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp
     +(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*
     +sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)+0.2D1*(0.1D1-
     +por(j))/por(j)*(kh2sfeoh3*sp(5,j)*(sp(12,j)+sp(13,j))+kh2sfeoh3mr*
     +sp(34,j)*(sp(12,j)+sp(13,j))+kh2sfeoh3pr*sp(35,j)*(sp(12,j)+sp(13,
     +j)))-kfeo2*sp(11,j)*sp(2,j)+(0.1D1-por(j))/por(j)*kfeso2*sp(25,j)*
     +sp(2,j)-kfesprecip*sw13*(sp(11,j)*sp(13,j)/sp(20,j)/KsFeS-0.1D1)-k
     +feco3precip*sw14*(sp(11,j)*sp(17,j)/KsFeCO3-0.1D1)+(0.1D1-por(j))/
     +por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*(0.1D1-sp(11,j)*sp(13,j)/sp(
     +20,j)/KsFeS)-(kf8*sp(11,j)*por(j)-kb8*sp(29,j)*(1.D0-por(j)))/por(
     +j)-0.2D1*(kfemno2*sp(11,j)*sp(4,j)+kfemno2pr*sp(11,j)*sp(33,j))*(0
     +.1D1-por(j))/por(j)+0.46D0*(0.1D1-por(j))/por(j)*r67+0.4D0*(0.1D1-
     +por(j))/por(j)*r66
            drdc = -kfeo2*sp(2,j)-kfesprecip*sw13*sp(13,j)/sp(20,j)/KsFe
     +S-kfeco3precip*sw14*sp(17,j)/KsFeCO3-(0.1D1-por(j))/por(j)*kfesdis
     +s*(0.1D1-sw13)*sp(25,j)*sp(13,j)/sp(20,j)/KsFeS-kf8-0.2D1*(kfemno2
     +*sp(4,j)+kfemno2pr*sp(33,j))*(0.1D1-por(j))/por(j)
            endif
            if (isp.eq.12) then
            rat = 0.5D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j
     +)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp
     +(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*
     +sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(
     +0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06
     ++(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)
     +)*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+kaom*sp(7,j)*sp(6,j)-kh2so2*(s
     +p(12,j)+sp(13,j))*sp(2,j)+kf3*sp(20,j)*sp(13,j)-kb3*sp(12,j)-(0.1D
     +1-por(j))/por(j)*(kh2sfeoh3*sp(5,j)*(sp(12,j)+sp(13,j))+kh2sfeoh3m
     +r*sp(34,j)*(sp(12,j)+sp(13,j))+kh2sfeoh3pr*sp(35,j)*(sp(12,j)+sp(1
     +3,j)))-kfesprecip*sw13*(sp(11,j)*sp(13,j)/sp(20,j)/KsFeS-0.1D1)-(0
     +.1D1-por(j))/por(j)*kpyr*sp(25,j)*(sp(12,j)+sp(13,j))+0.3D1*kdi*sw
     +19*sp(27,j)+(0.1D1-por(j))/por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*(
     +0.1D1-sp(11,j)*sp(13,j)/sp(20,j)/KsFeS)-(kh2smno2*sp(4,j)*(sp(12,j
     +)+sp(13,j))+kh2smno2pr*sp(33,j)*(sp(12,j)+sp(13,j)))*(0.1D1-por(j)
     +)/por(j)
            drdc = -kh2so2*sp(2,j)-kb3-(0.1D1-por(j))/por(j)*(kh2sfeoh3*
     +sp(5,j)+kh2sfeoh3mr*sp(34,j)+kh2sfeoh3pr*sp(35,j))-(0.1D1-por(j))/
     +por(j)*kpyr*sp(25,j)-(kh2smno2*sp(4,j)+kh2smno2pr*sp(33,j))*(0.1D1
     +-por(j))/por(j)
            endif
            if (isp.eq.13) then
            rat = -kf3*sp(20,j)*sp(13,j)+kb3*sp(12,j)
            drdc = -kf3*sp(20,j)
            endif
            if (isp.eq.14) then
            rat = -(0.1D1-sw17)*kdis*sw18*sp(14,j)*(ch4eq-sp(7,j))+sw17*
     +kgas*(sp(7,j)-ch4eq)
            drdc = -(0.1D1-sw17)*kdis*sw18*(ch4eq-sp(7,j))
            endif
            if (isp.eq.15) then
            rat = 0.6D1*sw32*kkaoli*(sp(42,j)**2*sp(40,j)**2/sp(20,j)**6
     +/kspkaoli-0.1D1)+0.2D1*kfeo2*sp(11,j)*sp(2,j)+0.2D1*kdi*sw19*sp(27
     +,j)+0.2D1*kmnox*sp(10,j)*sp(2,j)+0.2D1*kfesprecip*sw13*(sp(11,j)*s
     +p(13,j)/sp(20,j)/KsFeS-0.1D1)-kaom*sp(7,j)*sp(6,j)+kaomo2*sp(7,j)*
     +sp(2,j)-0.2D1*(0.1D1-por(j))/por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)
     +*(0.1D1-sp(11,j)*sp(13,j)/sp(20,j)/KsFeS)+0.1132075472D1*(0.1D1-po
     +r(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*(sw02+(0.1D1-sw02)*sp(2,j
     +)/kmo2)+0.2D1*(kfemno2*sp(11,j)*sp(4,j)+kfemno2pr*sp(11,j)*sp(33,j
     +))*(0.1D1-por(j))/por(j)-0.2D1*(kh2smno2*sp(4,j)*(sp(12,j)+sp(13,j
     +))+kh2smno2pr*sp(33,j)*(sp(12,j)+sp(13,j)))*(0.1D1-por(j))/por(j)-
     +0.4D1*(0.1D1-por(j))/por(j)*(kh2sfeoh3*sp(5,j)*(sp(12,j)+sp(13,j))
     ++kh2sfeoh3mr*sp(34,j)*(sp(12,j)+sp(13,j))+kh2sfeoh3pr*sp(35,j)*(sp
     +(12,j)+sp(13,j)))+0.6D1*sw31*ksmect*(sp(22,j)**0.165D0*sp(38,j)**0
     +.33D0*sp(42,j)**0.17D1*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1D1)+kf1
     +*sp(20,j)*sp(16,j)+0.8D1*sw30*killite*(sp(40,j)**0.35D1*sp(42,j)**
     +0.23D1*sp(50,j)**0.6D0*sp(38,j)**0.25D0/sp(20,j)**8/kspillite-0.1D
     +1)-kb1*sp(15,j)+0.2D1*knit*sp(8,j)*sp(2,j)+0.2D1*kh2so2*(sp(12,j)+
     +sp(13,j))*sp(2,j)-0.7094339623D1*(0.1D1-por(j))/por(j)*nu/(inita+s
     +p(32,j))*sp(1,j)*sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(
     +0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/k
     +mno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw0
     +6+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3
     +)-0.9433962264D-1*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j
     +)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1
     +D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1
     +D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp
     +(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*
     +sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(
     +0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06
     ++(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3)
     +)*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)-0.3094339623D1*(0.1D1-por(j))/
     +por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw05*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1
     +D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)+0.1509
     +433962D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw03*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/km
     +no3)+0.4056603774D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1
     +,j)*sw11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0
     +.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0
     +.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*
     +sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06
     +)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*
     +(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/
     +kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02
     +-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw
     +06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh
     +3)-sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.
     +1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.
     +1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*s
     +p(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)
     +*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(
     +0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/k
     +mno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-
     +(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw0
     +6+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08)*sp(5,j)/kmfeoh3
     +))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))-0.108D1*(0.1D1-por(j))/por(j
     +)*sw26*kglass*sp(47,j)*(sp(20,j)**3/sp(42,j))**(1.D0/3.D0)*(0.1D1-
     +sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/kspglass)-0.4D1*(0.1D1
     +-por(j))/por(j)*sw28*kolive*sp(51,j)*(0.1D1-sp(40,j)*sp(38,j)**0.1
     +6D1*sp(11,j)**0.4D0/sp(20,j)**4/kspolive)-0.4D1*(0.1D1-por(j))/por
     +(j)*sw29*kpyrox*sp(52,j)*(0.1D1-sp(40,j)**2*sp(11,j)**0.46D0*sp(38
     +,j)**0.84D0*sp(22,j)**0.7D0/sp(20,j)**4/ksppyrox)
            drdc = -kb1
            endif
            if (isp.eq.16) then
            rat = -0.6D1*sw32*kkaoli*(sp(42,j)**2*sp(40,j)**2/sp(20,j)**
     +6/kspkaoli-0.1D1)-0.2D1*kfeo2*sp(11,j)*sp(2,j)-0.2D1*kdi*sw19*sp(2
     +7,j)-0.2D1*kmnox*sp(10,j)*sp(2,j)-0.2D1*kfesprecip*sw13*(sp(11,j)*
     +sp(13,j)/sp(20,j)/KsFeS-0.1D1)+0.2D1*kaom*sp(7,j)*sp(6,j)+0.2D1*(0
     +.1D1-por(j))/por(j)*kfesdiss*(0.1D1-sw13)*sp(25,j)*(0.1D1-sp(11,j)
     +*sp(13,j)/sp(20,j)/KsFeS)-0.1320754717D0*(0.1D1-por(j))/por(j)*nu/
     +(inita+sp(32,j))*sp(1,j)*(sw02+(0.1D1-sw02)*sp(2,j)/kmo2)-0.2D1*(k
     +femno2*sp(11,j)*sp(4,j)+kfemno2pr*sp(11,j)*sp(33,j))*(0.1D1-por(j)
     +)/por(j)+0.2D1*(kh2smno2*sp(4,j)*(sp(12,j)+sp(13,j))+kh2smno2pr*sp
     +(33,j)*(sp(12,j)+sp(13,j)))*(0.1D1-por(j))/por(j)+0.4D1*(0.1D1-por
     +(j))/por(j)*(kh2sfeoh3*sp(5,j)*(sp(12,j)+sp(13,j))+kh2sfeoh3mr*sp(
     +34,j)*(sp(12,j)+sp(13,j))+kh2sfeoh3pr*sp(35,j)*(sp(12,j)+sp(13,j))
     +)-0.6D1*sw31*ksmect*(sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)**
     +0.17D1*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1D1)-kf1*sp(20,j)*sp(16,
     +j)-0.8D1*sw30*killite*(sp(40,j)**0.35D1*sp(42,j)**0.23D1*sp(50,j)*
     +*0.6D0*sp(38,j)**0.25D0/sp(20,j)**8/kspillite-0.1D1)+kf2*sp(20,j)*
     +sp(17,j)+kb1*sp(15,j)-kb2*sp(16,j)-0.2D1*knit*sp(8,j)*sp(2,j)-0.2D
     +1*kh2so2*(sp(12,j)+sp(13,j))*sp(2,j)+0.8094339623D1*(0.1D1-por(j))
     +/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw07*(0.1D1-sw02-(0.1D1-sw02)*
     +sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.
     +1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw
     +08)*sp(5,j)/kmfeoh3)+0.1094339623D1*(0.1D1-por(j))/por(j)*nu/(init
     +a+sp(32,j))*sp(1,j)*sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw0
     +3*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j
     +)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw
     +02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(
     +sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*sp
     +(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D
     +1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-
     +sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(
     +3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw08
     +)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4)+0.4094339623
     +D1*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j))*sp(1,j)*sw05*(0.1D1-s
     +w02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j
     +)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4
     +,j)/kmmno2)+0.8490566038D0*(0.1D1-por(j))/por(j)*nu/(inita+sp(32,j
     +))*sp(1,j)*sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D
     +1-sw04)*sp(3,j)/kmno3)+0.9433962264D-1*(0.1D1-por(j))/por(j)*nu/(i
     +nita+sp(32,j))*sp(1,j)*sw11*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-
     +sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(
     +3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1
     +-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3)
     +)*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)
     +*sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0
     +.1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/km
     +o2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*
     +sp(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-s
     +w08)*sp(5,j)/kmfeoh3)-sw09*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-s
     +w03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3
     +,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2-sw03*(0.1D1-
     +sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*sp(3,j)/kmno3))
     +*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1D1-sw02-(0.1D1-sw02)*
     +sp(2,j)/kmo2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.
     +1D1-sw04)*sp(3,j)/kmno3)-sw05*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo
     +2-sw03*(0.1D1-sw02-(0.1D1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1D1-sw04)*s
     +p(3,j)/kmno3))*(sw06+(0.1D1-sw06)*sp(4,j)/kmmno2))*(sw08+(0.1D1-sw
     +08)*sp(5,j)/kmfeoh3))*(sw10+(0.1D1-sw10)*sp(6,j)/kmso4))+0.108D1*(
     +0.1D1-por(j))/por(j)*sw26*kglass*sp(47,j)*(sp(20,j)**3/sp(42,j))**
     +(1.D0/3.D0)*(0.1D1-sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/ksp
     +glass)+0.4D1*(0.1D1-por(j))/por(j)*sw28*kolive*sp(51,j)*(0.1D1-sp(
     +40,j)*sp(38,j)**0.16D1*sp(11,j)**0.4D0/sp(20,j)**4/kspolive)+0.4D1
     +*(0.1D1-por(j))/por(j)*sw29*kpyrox*sp(52,j)*(0.1D1-sp(40,j)**2*sp(
     +11,j)**0.46D0*sp(38,j)**0.84D0*sp(22,j)**0.7D0/sp(20,j)**4/ksppyro
     +x)
            drdc = -kf1*sp(20,j)-kb2
            endif
            if (isp.eq.17) then
            rat = -kf2*sp(20,j)*sp(17,j)+kb2*sp(16,j)+(0.1D1-por(j))/por
     +(j)*sw15*kcaldiss*sp(21,j)*(0.1D1-sp(22,j)*sp(17,j)/kspcal)-sw20*k
     +calppt*(sp(22,j)*sp(17,j)/kspcal-0.5D1)+(0.1D1-por(j))/por(j)*sw21
     +*karadis*sp(36,j)*(0.1D1-sp(22,j)*sp(17,j)/kspara)-sw22*karappt*(s
     +p(22,j)*sp(17,j)/kspara-0.5D1)+(0.1D1-por(j))/por(j)*rate46-rate47
     +-kmnco3precip*sw12*(sp(10,j)*sp(17,j)/KsMnCO3-0.1D1)-kfeco3precip*
     +sw14*(sp(11,j)*sp(17,j)/KsFeCO3-0.1D1)
            drdc = -kf2*sp(20,j)-(0.1D1-por(j))/por(j)*sw15*kcaldiss*sp(
     +21,j)*sp(22,j)/kspcal-sw20*kcalppt*sp(22,j)/kspcal-(0.1D1-por(j))/
     +por(j)*sw21*karadis*sp(36,j)*sp(22,j)/kspara-sw22*karappt*sp(22,j)
     +/kspara-kmnco3precip*sw12*sp(10,j)/KsMnCO3-kfeco3precip*sw14*sp(11
     +,j)/KsFeCO3
            endif
            if (isp.eq.18) then
            rat = -kf4*sp(20,j)*sp(18,j)+kb4*sp(19,j)
            drdc = -kf4*sp(20,j)
            endif
            if (isp.eq.19) then
            rat = kf4*sp(20,j)*sp(18,j)-kb4*sp(19,j)
            drdc = -kb4
            endif
            if (isp.eq.20) then
            rat = -kf1*sp(20,j)*sp(16,j)+kb1*sp(15,j)-kf2*sp(20,j)*sp(17
     +,j)+kb2*sp(16,j)-kf3*sp(20,j)*sp(13,j)+kb3*sp(12,j)-kf4*sp(20,j)*s
     +p(18,j)+kb4*sp(19,j)-kf10*sp(43,j)*sp(20,j)+kb10*sp(42,j)-kf11*sp(
     +44,j)*sp(20,j)+kb11*sp(43,j)-kf12*sp(45,j)*sp(20,j)+kb12*sp(44,j)-
     +kf13*sp(46,j)*sp(20,j)+kb13*sp(45,j)
            drdc = -kf1*sp(16,j)-kf2*sp(17,j)-kf3*sp(13,j)-kf4*sp(18,j)-
     +kf10*sp(43,j)-kf11*sp(44,j)-kf12*sp(45,j)-kf13*sp(46,j)
            endif
            if (isp.eq.21) then
            rat = -sw15*kcaldiss*sp(21,j)*(0.1D1-sp(22,j)*sp(17,j)/kspca
     +l)+0.1D1*sw20*kcalppt*(sp(22,j)*sp(17,j)/kspcal-0.5D1)/(0.1D1-por(
     +j))*por(j)
            drdc = -sw15*kcaldiss*(0.1D1-sp(22,j)*sp(17,j)/kspcal)
            endif
            if (isp.eq.22) then
            rat = (0.1D1-por(j))/por(j)*sw15*kcaldiss*sp(21,j)*(0.1D1-sp
     +(22,j)*sp(17,j)/kspcal)-sw20*kcalppt*(sp(22,j)*sp(17,j)/kspcal-0.5
     +D1)+(0.1D1-por(j))/por(j)*sw21*karadis*sp(36,j)*(0.1D1-sp(22,j)*sp
     +(17,j)/kspara)-sw22*karappt*(sp(22,j)*sp(17,j)/kspara-0.5D1)+(0.1D
     +1-por(j))/por(j)*rate46-rate47+0.7D0*(0.1D1-por(j))/por(j)*sw29*kp
     +yrox*sp(52,j)*(0.1D1-sp(40,j)**2*sp(11,j)**0.46D0*sp(38,j)**0.84D0
     +*sp(22,j)**0.7D0/sp(20,j)**4/ksppyrox)+0.3D0*(0.1D1-por(j))/por(j)
     +*sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j))**0.35D0*(0.1D1-sp(40
     +,j)**0.23D1*sp(42,j)**0.17D1*sp(49,j)**0.7D0*sp(22,j)**0.3D0/ksppl
     +agio)-0.165D0*sw31*ksmect*(sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(4
     +2,j)**0.17D1*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1D1)
            drdc = -(0.1D1-por(j))/por(j)*sw15*kcaldiss*sp(21,j)*sp(17,j
     +)/kspcal-sw20*kcalppt*sp(17,j)/kspcal-(0.1D1-por(j))/por(j)*sw21*k
     +aradis*sp(36,j)*sp(17,j)/kspara-sw22*karappt*sp(17,j)/kspara-0.49D
     +0*(0.1D1-por(j))/por(j)*sw29*kpyrox*sp(52,j)*sp(40,j)**2*sp(11,j)*
     +*0.46D0*sp(38,j)**0.84D0/sp(22,j)**0.3D0/sp(20,j)**4/ksppyrox-0.9D
     +-1*(0.1D1-por(j))/por(j)*sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,
     +j))**0.35D0*sp(40,j)**0.23D1*sp(42,j)**0.17D1*sp(49,j)**0.7D0/sp(2
     +2,j)**0.7D0/kspplagio-0.27225D-1*sw31*ksmect/sp(22,j)**0.835D0*sp(
     +38,j)**0.33D0*sp(42,j)**0.17D1*sp(40,j)**4/sp(20,j)**6/kspsmect
            endif
            if (isp.eq.23) then
            rat = (kf6*sp(8,j)*por(j)-kb6*sp(23,j)*(1.D0-por(j)))/(0.1D1
     +-por(j))
            drdc = -kb6*(1.D0-por(j))/(0.1D1-por(j))
            endif
            if (isp.eq.24) then
            rat = (kf5*sp(9,j)*por(j)-kb5*sp(24,j)*(1.D0-por(j)))/(0.1D1
     +-por(j))
            drdc = -kb5*(1.D0-por(j))/(0.1D1-por(j))
            endif
            if (isp.eq.25) then
            rat = 0.1D1*kfesprecip*sw13*(sp(11,j)*sp(13,j)/sp(20,j)/KsFe
     +S-0.1D1)/(0.1D1-por(j))*por(j)-kfeso2*sp(25,j)*sp(2,j)-kpyr*sp(25,
     +j)*(sp(12,j)+sp(13,j))-kfess0*sp(25,j)*sp(27,j)-kfesdiss*(0.1D1-sw
     +13)*sp(25,j)*(0.1D1-sp(11,j)*sp(13,j)/sp(20,j)/KsFeS)
            drdc = -kfeso2*sp(2,j)-kpyr*(sp(12,j)+sp(13,j))-kfess0*sp(27
     +,j)-kfesdiss*(0.1D1-sw13)*(0.1D1-sp(11,j)*sp(13,j)/sp(20,j)/KsFeS)
            endif
            if (isp.eq.26) then
            rat = 0.1D1*kfeco3precip*sw14*(sp(11,j)*sp(17,j)/KsFeCO3-0.1
     +D1)/(0.1D1-por(j))*por(j)
            drdc = 0
            endif
            if (isp.eq.27) then
            rat = 0.1D1*(0.1D1-por(j))/por(j)*(kh2sfeoh3*sp(5,j)*(sp(12,
     +j)+sp(13,j))+kh2sfeoh3mr*sp(34,j)*(sp(12,j)+sp(13,j))+kh2sfeoh3pr*
     +sp(35,j)*(sp(12,j)+sp(13,j)))-0.4D1*kdi*sw19*sp(27,j)-(0.1D1-por(j
     +))/por(j)*kfess0*sp(25,j)*sp(27,j)+0.1D1*(kh2smno2*sp(4,j)*(sp(12,
     +j)+sp(13,j))+kh2smno2pr*sp(33,j)*(sp(12,j)+sp(13,j)))*(0.1D1-por(j
     +))/por(j)
            drdc = -0.4D1*kdi*sw19-(0.1D1-por(j))/por(j)*kfess0*sp(25,j)
            endif
            if (isp.eq.28) then
            rat = 0.1D1*kpyr*sp(25,j)*(sp(12,j)+sp(13,j))+0.1D1*kfess0*s
     +p(25,j)*sp(27,j)
            drdc = 0
            endif
            if (isp.eq.29) then
            rat = (kf8*sp(11,j)*por(j)-kb8*sp(29,j)*(1.D0-por(j)))/(0.1D
     +1-por(j))
            drdc = -kb8*(1.D0-por(j))/(0.1D1-por(j))
            endif
            if (isp.eq.30) then
            rat = (kf7*(sp(5,j)+sp(34,j)+sp(35,j))*sp(9,j)-kb7*sp(30,j))
     +/(0.1D1-por(j))
            drdc = -kb7/(0.1D1-por(j))
            endif
            if (isp.eq.31) then
            rat = 0.1D1*kmnco3precip*sw12*(sp(10,j)*sp(17,j)/KsMnCO3-0.1
     +D1)/(0.1D1-por(j))*por(j)
            drdc = 0
            endif
            if (isp.eq.32) then
            rat = 0.D0
            drdc = 0
            endif
            if (isp.eq.33) then
            rat = -kfemno2pr*sp(11,j)*sp(33,j)-kh2smno2pr*sp(33,j)*(sp(1
     +2,j)+sp(13,j))+kmnage*sp(4,j)
            drdc = -kfemno2pr*sp(11,j)-kh2smno2pr*(sp(12,j)+sp(13,j))
            endif
            if (isp.eq.34) then
            rat = -0.2D1*kh2sfeoh3mr*sp(34,j)*(sp(12,j)+sp(13,j))+kfeage
     +*sp(5,j)
            drdc = -0.2D1*kh2sfeoh3mr*(sp(12,j)+sp(13,j))
            endif
            if (isp.eq.35) then
            rat = -0.2D1*kh2sfeoh3pr*sp(35,j)*(sp(12,j)+sp(13,j))
            drdc = -0.2D1*kh2sfeoh3pr*(sp(12,j)+sp(13,j))
            endif
            if (isp.eq.36) then
            rat = -sw21*karadis*sp(36,j)*(0.1D1-sp(22,j)*sp(17,j)/kspara
     +)+sw22*karappt*(sp(22,j)*sp(17,j)/kspara-0.5D1)/(0.1D1-por(j))*por
     +(j)
            drdc = -sw21*karadis*(0.1D1-sp(22,j)*sp(17,j)/kspara)
            endif
            if (isp.eq.37) then
            rat = -rate46+rate47/(0.1D1-por(j))*por(j)
            drdc = 0
            endif
            if (isp.eq.38) then
            rat = 0.84D0*(0.1D1-por(j))/por(j)*sw29*kpyrox*sp(52,j)*(0.1
     +D1-sp(40,j)**2*sp(11,j)**0.46D0*sp(38,j)**0.84D0*sp(22,j)**0.7D0/s
     +p(20,j)**4/ksppyrox)+0.16D1*(0.1D1-por(j))/por(j)*sw28*kolive*sp(5
     +1,j)*(0.1D1-sp(40,j)*sp(38,j)**0.16D1*sp(11,j)**0.4D0/sp(20,j)**4/
     +kspolive)-0.25D0*sw30*killite*(sp(40,j)**0.35D1*sp(42,j)**0.23D1*s
     +p(50,j)**0.6D0*sp(38,j)**0.25D0/sp(20,j)**8/kspillite-0.1D1)-0.33D
     +0*sw31*ksmect*(sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)**0.17D1
     +*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1D1)
            drdc = -0.7056D0*(0.1D1-por(j))/por(j)*sw29*kpyrox*sp(52,j)*
     +sp(40,j)**2*sp(11,j)**0.46D0/sp(38,j)**0.16D0*sp(22,j)**0.7D0/sp(2
     +0,j)**4/ksppyrox-0.256D1*(0.1D1-por(j))/por(j)*sw28*kolive*sp(51,j
     +)*sp(40,j)*sp(38,j)**0.6D0*sp(11,j)**0.4D0/sp(20,j)**4/kspolive-0.
     +625D-1*sw30*killite*sp(40,j)**0.35D1*sp(42,j)**0.23D1*sp(50,j)**0.
     +6D0/sp(38,j)**0.75D0/sp(20,j)**8/kspillite-0.1089D0*sw31*ksmect*sp
     +(22,j)**0.165D0/sp(38,j)**0.67D0*sp(42,j)**0.17D1*sp(40,j)**4/sp(2
     +0,j)**6/kspsmect
            endif
            if (isp.eq.39) then
            rat = -sw25*(kBSidis*bd+(kBSidis-kBSidis*bd)*exp(-ad*x_pos))
     +*sp(39,j)*(0.1D1-sp(40,j)/BSisat)
            drdc = -sw25*(kBSidis*bd+(kBSidis-kBSidis*bd)*exp(-ad*x_pos)
     +)*(0.1D1-sp(40,j)/BSisat)
            endif
            if (isp.eq.40) then
            rat = sw25*(kBSidis*bd+(kBSidis-kBSidis*bd)*exp(-ad*x_pos))*
     +sp(39,j)*(0.1D1-sp(40,j)/BSisat)*(0.1D1-por(j))/por(j)-(kf9*(sp(5,
     +j)+sp(34,j)+sp(35,j))*sp(40,j)-kb9*sp(41,j))/por(j)+(0.1D1-por(j))
     +/por(j)*sw28*kolive*sp(51,j)*(0.1D1-sp(40,j)*sp(38,j)**0.16D1*sp(1
     +1,j)**0.4D0/sp(20,j)**4/kspolive)+(0.1D1-por(j))/por(j)*sw26*kglas
     +s*sp(47,j)*(sp(20,j)**3/sp(42,j))**(1.D0/3.D0)*(0.1D1-sp(40,j)*sp(
     +42,j)**0.36D0/sp(20,j)**0.108D1/kspglass)+0.23D1*(0.1D1-por(j))/po
     +r(j)*sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j))**0.35D0*(0.1D1-s
     +p(40,j)**0.23D1*sp(42,j)**0.17D1*sp(49,j)**0.7D0*sp(22,j)**0.3D0/k
     +spplagio)-0.35D1*sw30*killite*(sp(40,j)**0.35D1*sp(42,j)**0.23D1*s
     +p(50,j)**0.6D0*sp(38,j)**0.25D0/sp(20,j)**8/kspillite-0.1D1)+0.2D1
     +*(0.1D1-por(j))/por(j)*sw29*kpyrox*sp(52,j)*(0.1D1-sp(40,j)**2*sp(
     +11,j)**0.46D0*sp(38,j)**0.84D0*sp(22,j)**0.7D0/sp(20,j)**4/ksppyro
     +x)-0.4D1*sw31*ksmect*(sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)*
     +*0.17D1*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1D1)-0.2D1*sw32*kkaoli*
     +(sp(42,j)**2*sp(40,j)**2/sp(20,j)**6/kspkaoli-0.1D1)
            drdc = -sw25*(kBSidis*bd+(kBSidis-kBSidis*bd)*exp(-ad*x_pos)
     +)*sp(39,j)/BSisat*(0.1D1-por(j))/por(j)-kf9*(sp(5,j)+sp(34,j)+sp(3
     +5,j))/por(j)-(0.1D1-por(j))/por(j)*sw28*kolive*sp(51,j)*sp(38,j)**
     +0.16D1*sp(11,j)**0.4D0/sp(20,j)**4/kspolive-(0.1D1-por(j))/por(j)*
     +sw26*kglass*sp(47,j)*(sp(20,j)**3/sp(42,j))**(1.D0/3.D0)*sp(42,j)*
     +*0.36D0/sp(20,j)**0.108D1/kspglass-0.529D1*(0.1D1-por(j))/por(j)*s
     +w27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j))**0.35D0*sp(40,j)**0.13
     +D1*sp(42,j)**0.17D1*sp(49,j)**0.7D0*sp(22,j)**0.3D0/kspplagio-0.12
     +25D2*sw30*killite*sp(40,j)**0.25D1*sp(42,j)**0.23D1*sp(50,j)**0.6D
     +0*sp(38,j)**0.25D0/sp(20,j)**8/kspillite-0.4D1*(0.1D1-por(j))/por(
     +j)*sw29*kpyrox*sp(52,j)*sp(40,j)*sp(11,j)**0.46D0*sp(38,j)**0.84D0
     +*sp(22,j)**0.7D0/sp(20,j)**4/ksppyrox-0.16D2*sw31*ksmect*sp(22,j)*
     +*0.165D0*sp(38,j)**0.33D0*sp(42,j)**0.17D1*sp(40,j)**3/sp(20,j)**6
     +/kspsmect-0.4D1*sw32*kkaoli*sp(42,j)**2*sp(40,j)/sp(20,j)**6/kspka
     +oli
            endif
            if (isp.eq.41) then
            rat = (kf9*(sp(5,j)+sp(34,j)+sp(35,j))*sp(40,j)-kb9*sp(41,j)
     +)/(0.1D1-por(j))
            drdc = -kb9/(0.1D1-por(j))
            endif
            if (isp.eq.42) then
            rat = 0.36D0*(0.1D1-por(j))/por(j)*sw26*kglass*sp(47,j)*(sp(
     +20,j)**3/sp(42,j))**(1.D0/3.D0)*(0.1D1-sp(40,j)*sp(42,j)**0.36D0/s
     +p(20,j)**0.108D1/kspglass)-0.23D1*sw30*killite*(sp(40,j)**0.35D1*s
     +p(42,j)**0.23D1*sp(50,j)**0.6D0*sp(38,j)**0.25D0/sp(20,j)**8/kspil
     +lite-0.1D1)+0.25D-2*sw25*(kBSidis*bd+(kBSidis-kBSidis*bd)*exp(-ad*
     +x_pos))*sp(39,j)*(0.1D1-sp(40,j)/BSisat)*(0.1D1-por(j))/por(j)-0.1
     +7D1*sw31*ksmect*(sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)**0.17
     +D1*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1D1)-0.2D1*sw32*kkaoli*(sp(4
     +2,j)**2*sp(40,j)**2/sp(20,j)**6/kspkaoli-0.1D1)+kf10*sp(43,j)*sp(2
     +0,j)-kb10*sp(42,j)+0.17D1*(0.1D1-por(j))/por(j)*sw27*kplagio*sp(48
     +,j)*(sp(20,j)**3/sp(42,j))**0.35D0*(0.1D1-sp(40,j)**0.23D1*sp(42,j
     +)**0.17D1*sp(49,j)**0.7D0*sp(22,j)**0.3D0/kspplagio)
            drdc = -0.12D0*(0.1D1-por(j))/por(j)*sw26*kglass*sp(47,j)/(s
     +p(20,j)**3/sp(42,j))**(2.D0/3.D0)*(0.1D1-sp(40,j)*sp(42,j)**0.36D0
     +/sp(20,j)**0.108D1/kspglass)*sp(20,j)**3/sp(42,j)**2-0.1296D0*(0.1
     +D1-por(j))/por(j)*sw26*kglass*sp(47,j)*(sp(20,j)**3/sp(42,j))**(1.
     +D0/3.D0)*sp(40,j)/sp(42,j)**0.64D0/sp(20,j)**0.108D1/kspglass-0.52
     +9D1*sw30*killite*sp(40,j)**0.35D1*sp(42,j)**0.13D1*sp(50,j)**0.6D0
     +*sp(38,j)**0.25D0/sp(20,j)**8/kspillite-0.289D1*sw31*ksmect*sp(22,
     +j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)**0.7D0*sp(40,j)**4/sp(20,j)*
     +*6/kspsmect-0.4D1*sw32*kkaoli*sp(42,j)*sp(40,j)**2/sp(20,j)**6/ksp
     +kaoli-kb10-0.595D0*(0.1D1-por(j))/por(j)*sw27*kplagio*sp(48,j)/(sp
     +(20,j)**3/sp(42,j))**0.65D0*(0.1D1-sp(40,j)**0.23D1*sp(42,j)**0.17
     +D1*sp(49,j)**0.7D0*sp(22,j)**0.3D0/kspplagio)*sp(20,j)**3/sp(42,j)
     +**2-0.289D1*(0.1D1-por(j))/por(j)*sw27*kplagio*sp(48,j)*(sp(20,j)*
     +*3/sp(42,j))**0.35D0*sp(40,j)**0.23D1*sp(42,j)**0.7D0*sp(49,j)**0.
     +7D0*sp(22,j)**0.3D0/kspplagio
            endif
            if (isp.eq.43) then
            rat = -kf10*sp(43,j)*sp(20,j)+kb10*sp(42,j)+kf11*sp(44,j)*sp
     +(20,j)-kb11*sp(43,j)
            drdc = -kf10*sp(20,j)-kb11
            endif
            if (isp.eq.44) then
            rat = -kf11*sp(44,j)*sp(20,j)+kb11*sp(43,j)+kf12*sp(45,j)*sp
     +(20,j)-kb12*sp(44,j)
            drdc = -kf11*sp(20,j)-kb12
            endif
            if (isp.eq.45) then
            rat = -kf12*sp(45,j)*sp(20,j)+kb12*sp(44,j)+kf13*sp(46,j)*sp
     +(20,j)-kb13*sp(45,j)
            drdc = -kf12*sp(20,j)-kb13
            endif
            if (isp.eq.46) then
            rat = -kf13*sp(46,j)*sp(20,j)+kb13*sp(45,j)
            drdc = -kf13*sp(20,j)
            endif
            if (isp.eq.47) then
            rat = -sw26*kglass*sp(47,j)*(sp(20,j)**3/sp(42,j))**(1.D0/3.
     +D0)*(0.1D1-sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/kspglass)
            drdc = -sw26*kglass*(sp(20,j)**3/sp(42,j))**(1.D0/3.D0)*(0.1
     +D1-sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/kspglass)
            endif
            if (isp.eq.48) then
            rat = -sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j))**0.35D0*
     +(0.1D1-sp(40,j)**0.23D1*sp(42,j)**0.17D1*sp(49,j)**0.7D0*sp(22,j)*
     +*0.3D0/kspplagio)
            drdc = -sw27*kplagio*(sp(20,j)**3/sp(42,j))**0.35D0*(0.1D1-s
     +p(40,j)**0.23D1*sp(42,j)**0.17D1*sp(49,j)**0.7D0*sp(22,j)**0.3D0/k
     +spplagio)
            endif
            if (isp.eq.49) then
            rat = 0.7D0*(0.1D1-por(j))/por(j)*sw27*kplagio*sp(48,j)*(sp(
     +20,j)**3/sp(42,j))**0.35D0*(0.1D1-sp(40,j)**0.23D1*sp(42,j)**0.17D
     +1*sp(49,j)**0.7D0*sp(22,j)**0.3D0/kspplagio)
            drdc = -0.49D0*(0.1D1-por(j))/por(j)*sw27*kplagio*sp(48,j)*(
     +sp(20,j)**3/sp(42,j))**0.35D0*sp(40,j)**0.23D1*sp(42,j)**0.17D1/sp
     +(49,j)**0.3D0*sp(22,j)**0.3D0/kspplagio
            endif
            if (isp.eq.50) then
            rat = -0.6D0*sw30*killite*(sp(40,j)**0.35D1*sp(42,j)**0.23D1
     +*sp(50,j)**0.6D0*sp(38,j)**0.25D0/sp(20,j)**8/kspillite-0.1D1)
            drdc = -0.36D0*sw30*killite*sp(40,j)**0.35D1*sp(42,j)**0.23D
     +1/sp(50,j)**0.4D0*sp(38,j)**0.25D0/sp(20,j)**8/kspillite
            endif
            if (isp.eq.51) then
            rat = -sw28*kolive*sp(51,j)*(0.1D1-sp(40,j)*sp(38,j)**0.16D1
     +*sp(11,j)**0.4D0/sp(20,j)**4/kspolive)
            drdc = -sw28*kolive*(0.1D1-sp(40,j)*sp(38,j)**0.16D1*sp(11,j
     +)**0.4D0/sp(20,j)**4/kspolive)
            endif
            if (isp.eq.52) then
            rat = -sw29*kpyrox*sp(52,j)*(0.1D1-sp(40,j)**2*sp(11,j)**0.4
     +6D0*sp(38,j)**0.84D0*sp(22,j)**0.7D0/sp(20,j)**4/ksppyrox)
            drdc = -sw29*kpyrox*(0.1D1-sp(40,j)**2*sp(11,j)**0.46D0*sp(3
     +8,j)**0.84D0*sp(22,j)**0.7D0/sp(20,j)**4/ksppyrox)
            endif
            if (isp.eq.53) then
            rat = sw30*killite*(sp(40,j)**0.35D1*sp(42,j)**0.23D1*sp(50,
     +j)**0.6D0*sp(38,j)**0.25D0/sp(20,j)**8/kspillite-0.1D1)/(0.1D1-por
     +(j))*por(j)
            drdc = 0
            endif
            if (isp.eq.54) then
            rat = sw31*ksmect*(sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(42,
     +j)**0.17D1*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1D1)/(0.1D1-por(j))*
     +por(j)
            drdc = 0
            endif
            if (isp.eq.55) then
            rat = sw32*kkaoli*(sp(42,j)**2*sp(40,j)**2/sp(20,j)**6/kspka
     +oli-0.1D1)/(0.1D1-por(j))*por(j)
            drdc = 0
            endif
      end
