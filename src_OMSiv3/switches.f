c      
c     SUBROUTINE switches
c      
      subroutine switches(j)
          include 'common_geo.inc'
          include 'common.inc'
          integer j
          real*8 x_pos
          real*8 y_pos
          real*8 z_pos
          x_pos = x(j)
            if (0.lt.xbiot-x_pos) then
              sw01 = 0.1D1
              else
              sw01 = 0.D0
            endif
            if (0.lt.sp(2,j)-kmo2) then
              sw02 = 0.1D1
              else
              sw02 = 0.D0
            endif
            if (0.lt.0.1D1-sw02) then
              sw03 = 0.1D1
              else
              sw03 = 0.D0
            endif
            if (0.lt.sw03*(sp(3,j)-kmno3)) then
              sw04 = 0.1D1
              else
              sw04 = 0.D0
            endif
            if (0.lt.-sw03*(sw04-0.1D1)) then
              sw05 = 0.1D1
              else
              sw05 = 0.D0
            endif
            if (0.lt.sw05*(sp(4,j)-kmmno2)) then
              sw06 = 0.1D1
              else
              sw06 = 0.D0
            endif
            if (0.lt.-sw05*(sw06-0.1D1)) then
              sw07 = 0.1D1
              else
              sw07 = 0.D0
            endif
            if (0.lt.sw07*(sp(5,j)-kmfeoh3)) then
              sw08 = 0.1D1
              else
              sw08 = 0.D0
            endif
            if (0.lt.-sw07*(sw08-0.1D1)) then
              sw09 = 0.1D1
              else
              sw09 = 0.D0
            endif
            if (0.lt.sw09*(sp(6,j)-kmso4)) then
              sw10 = 0.1D1
              else
              sw10 = 0.D0
            endif
            if (0.lt.-sw09*(sw10-0.1D1)) then
              sw11 = 0.1D1
              else
              sw11 = 0.D0
            endif
            if (0.lt.sp(10,j)*sp(17,j)/KsMnCO3-0.1D1) then
              sw12 = 0.1D1
              else
              sw12 = 0.D0
            endif
            if (0.lt.sp(11,j)*sp(13,j)/sp(20,j)/KsFeS-0.1D1) then
              sw13 = 0.1D1
              else
              sw13 = 0.D0
            endif
            if (0.lt.sp(11,j)*sp(17,j)/KsFeCO3-0.1D1) then
              sw14 = 0.1D1
              else
              sw14 = 0.D0
            endif
            if (0.lt.0.1D1-sp(22,j)*sp(17,j)/kspcal) then
              sw15 = 0.1D1
              else
              sw15 = 0.D0
            endif
            if (0.lt.sp(9,j)-po4_eq) then
              sw16 = 0.1D1
              else
              sw16 = 0.D0
            endif
            if (0.lt.sp(7,j)-ch4eq) then
              sw17 = 0.1D1
              else
              sw17 = 0.D0
            endif
            if (0.lt.sp(7,j)) then
              sw18 = 0.1D1
              else
              sw18 = 0.D0
            endif
            if (0.lt.h2sstar-sp(12,j)-sp(13,j)) then
              sw19 = 0.1D1
              else
              sw19 = 0.D0
            endif
            if (0.lt.sp(22,j)*sp(17,j)/kspcal-0.5D1) then
              sw20 = 0.1D1
              else
              sw20 = 0.D0
            endif
            if (0.lt.0.1D1-sp(22,j)*sp(17,j)/kspara) then
              sw21 = 0.1D1
              else
              sw21 = 0.D0
            endif
            if (0.lt.sp(22,j)*sp(17,j)/kspara-0.5D1) then
              sw22 = 0.1D1
              else
              sw22 = 0.D0
            endif
            if (0.lt.0.1D1-sp(22,j)*sp(17,j)/kspmgc) then
              sw23 = 0.1D1
              else
              sw23 = 0.D0
            endif
            if (0.lt.sp(22,j)*sp(17,j)/kspmgc-0.1D1) then
              sw24 = 0.1D1
              else
              sw24 = 0.D0
            endif
            if (0.lt.-sp(40,j)+BSisat) then
              sw25 = 0.1D1
              else
              sw25 = 0.D0
            endif
            if (0.lt.0.1D1-sp(40,j)*sp(42,j)**0.36D0/sp(20,j)**0.108D1/k
     +spglass) then
              sw26 = 0.1D1
              else
              sw26 = 0.D0
            endif
            if (0.lt.0.1D1-sp(40,j)**0.23D1*sp(42,j)**0.17D1*sp(49,j)**0
     +.7D0*sp(22,j)**0.3D0/kspplagio) then
              sw27 = 0.1D1
              else
              sw27 = 0.D0
            endif
            if (0.lt.0.1D1-sp(40,j)*sp(38,j)**0.16D1*sp(11,j)**0.4D0/sp(
     +20,j)**4/kspolive) then
              sw28 = 0.1D1
              else
              sw28 = 0.D0
            endif
            if (0.lt.0.1D1-sp(40,j)**2*sp(11,j)**0.46D0*sp(38,j)**0.84D0
     +*sp(22,j)**0.7D0/sp(20,j)**4/ksppyrox) then
              sw29 = 0.1D1
              else
              sw29 = 0.D0
            endif
            if (0.lt.sp(40,j)**0.35D1*sp(42,j)**0.23D1*sp(50,j)**0.6D0*s
     +p(38,j)**0.25D0/sp(20,j)**8/kspillite-0.1D1) then
              sw30 = 0.1D1
              else
              sw30 = 0.D0
            endif
            if (0.lt.sp(22,j)**0.165D0*sp(38,j)**0.33D0*sp(42,j)**0.17D1
     +*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1D1) then
              sw31 = 0.1D1
              else
              sw31 = 0.D0
            endif
            if (0.lt.sp(42,j)**2*sp(40,j)**2/sp(20,j)**6/kspkaoli-0.1D1)
     + then
              sw32 = 0.1D1
              else
              sw32 = 0.D0
            endif
      end
