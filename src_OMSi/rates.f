c      
c     SUBROUTINE rates
c      
      subroutine rates(j)
        include 'common_geo.inc'
        include 'common.inc'
        call switches(j)
            r(1,j) = 0.0
            r(2,j) = nu/(inita+sp(32,j))*sp(1,j)*(sw02+(0.1E1-sw02)*sp(2
     +,j)/kmo2)
            r(3,j) = nu/(inita+sp(32,j))*sp(1,j)*sw03*(0.1E1-sw02-(0.1E1
     +-sw02)*sp(2,j)/kmo2)*(sw04+(0.1E1-sw04)*sp(3,j)/kmno3)
            r(4,j) = nu/(inita+sp(32,j))*sp(1,j)*sw05*(0.1E1-sw02-(0.1E1
     +-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(s
     +w04+(0.1E1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1E1-sw06)*sp(4,j)/kmmno2
     +)
            r(5,j) = nu/(inita+sp(32,j))*sp(1,j)*sw07*(0.1E1-sw02-(0.1E1
     +-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(s
     +w04+(0.1E1-sw04)*sp(3,j)/kmno3)-sw05*(0.1E1-sw02-(0.1E1-sw02)*sp(2
     +,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1E1-
     +sw04)*sp(3,j)/kmno3))*(sw06+(0.1E1-sw06)*sp(4,j)/kmmno2))*(sw08+(0
     +.1E1-sw08)*sp(5,j)/kmfeoh3)
            r(6,j) = nu/(inita+sp(32,j))*sp(1,j)*sw09*(0.1E1-sw02-(0.1E1
     +-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(s
     +w04+(0.1E1-sw04)*sp(3,j)/kmno3)-sw05*(0.1E1-sw02-(0.1E1-sw02)*sp(2
     +,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1E1-
     +sw04)*sp(3,j)/kmno3))*(sw06+(0.1E1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1
     +E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp
     +(2,j)/kmo2)*(sw04+(0.1E1-sw04)*sp(3,j)/kmno3)-sw05*(0.1E1-sw02-(0.
     +1E1-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)
     +*(sw04+(0.1E1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1E1-sw06)*sp(4,j)/kmm
     +no2))*(sw08+(0.1E1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1E1-sw10)*sp(6
     +,j)/kmso4)
            r(7,j) = nu/(inita+sp(32,j))*sp(1,j)*sw11*(0.1E1-sw02-(0.1E1
     +-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(s
     +w04+(0.1E1-sw04)*sp(3,j)/kmno3)-sw05*(0.1E1-sw02-(0.1E1-sw02)*sp(2
     +,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1E1-
     +sw04)*sp(3,j)/kmno3))*(sw06+(0.1E1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1
     +E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp
     +(2,j)/kmo2)*(sw04+(0.1E1-sw04)*sp(3,j)/kmno3)-sw05*(0.1E1-sw02-(0.
     +1E1-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)
     +*(sw04+(0.1E1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1E1-sw06)*sp(4,j)/kmm
     +no2))*(sw08+(0.1E1-sw08)*sp(5,j)/kmfeoh3)-sw09*(0.1E1-sw02-(0.1E1-
     +sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(sw
     +04+(0.1E1-sw04)*sp(3,j)/kmno3)-sw05*(0.1E1-sw02-(0.1E1-sw02)*sp(2,
     +j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*(sw04+(0.1E1-s
     +w04)*sp(3,j)/kmno3))*(sw06+(0.1E1-sw06)*sp(4,j)/kmmno2)-sw07*(0.1E
     +1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(
     +2,j)/kmo2)*(sw04+(0.1E1-sw04)*sp(3,j)/kmno3)-sw05*(0.1E1-sw02-(0.1
     +E1-sw02)*sp(2,j)/kmo2-sw03*(0.1E1-sw02-(0.1E1-sw02)*sp(2,j)/kmo2)*
     +(sw04+(0.1E1-sw04)*sp(3,j)/kmno3))*(sw06+(0.1E1-sw06)*sp(4,j)/kmmn
     +o2))*(sw08+(0.1E1-sw08)*sp(5,j)/kmfeoh3))*(sw10+(0.1E1-sw10)*sp(6,
     +j)/kmso4))
            r(8,j) = knit*sp(8,j)*sp(2,j)
            r(9,j) = kmnox*sp(10,j)*sp(2,j)
            r(10,j) = kfemno2*sp(11,j)*sp(4,j)
            r(11,j) = kfeo2*sp(11,j)*sp(2,j)
            r(12,j) = kh2so2*(sp(12,j)+sp(13,j))*sp(2,j)
            r(13,j) = kh2smno2*sp(4,j)*(sp(12,j)+sp(13,j))
            r(14,j) = kh2sfeoh3*sp(5,j)*(sp(12,j)+sp(13,j))
            r(15,j) = kaom*sp(7,j)*sp(6,j)
            r(16,j) = kaomo2*sp(7,j)*sp(2,j)
            r(17,j) = kfeso2*sp(25,j)*sp(2,j)
            r(18,j) = kmnco3precip*sw12*(sp(10,j)*sp(17,j)/KsMnCO3-0.1E1
     +)
            r(19,j) = kfesprecip*sw13*(sp(11,j)*sp(13,j)/sp(20,j)/KsFeS-
     +0.1E1)
            r(20,j) = kfesdiss*(0.1E1-sw13)*sp(25,j)*(0.1E1-sp(11,j)*sp(
     +13,j)/sp(20,j)/KsFeS)
            r(21,j) = kfeco3precip*sw14*(sp(11,j)*sp(17,j)/KsFeCO3-0.1E1
     +)
            r(22,j) = kpyr*sp(25,j)*(sp(12,j)+sp(13,j))
            r(23,j) = kfess0*sp(25,j)*sp(27,j)
            r(24,j) = sw15*kcaldiss*sp(21,j)*(0.1E1-sp(22,j)*sp(17,j)/ks
     +pcal)
            r(25,j) = sw16*kapa*(sp(9,j)-po4_eq)
            r(26,j) = (0.1E1-sw17)*kdis*sw18*sp(14,j)*(ch4eq-sp(7,j))
            r(27,j) = sw17*kgas*(sp(7,j)-ch4eq)
            r(28,j) = kdi*sw19*sp(27,j)
            r(29,j) = kf1*sp(20,j)*sp(16,j)-kb1*sp(15,j)
            r(30,j) = kf2*sp(20,j)*sp(17,j)-kb2*sp(16,j)
            r(31,j) = kf3*sp(20,j)*sp(13,j)-kb3*sp(12,j)
            r(32,j) = kf4*sp(20,j)*sp(18,j)-kb4*sp(19,j)
            r(33,j) = kf5*sp(9,j)*por(j)-kb5*sp(24,j)*(1.0-por(j))
            r(34,j) = kf6*sp(8,j)*por(j)-kb6*sp(23,j)*(1.0-por(j))
            r(35,j) = kf7*(sp(5,j)+sp(34,j)+sp(35,j))*sp(9,j)-kb7*sp(30,
     +j)
            r(36,j) = kf8*sp(11,j)*por(j)-kb8*sp(29,j)*(1.0-por(j))
            r(37,j) = kfemno2pr*sp(11,j)*sp(33,j)
            r(38,j) = kh2smno2pr*sp(33,j)*(sp(12,j)+sp(13,j))
            r(39,j) = kmnage*sp(4,j)
            r(40,j) = kh2sfeoh3mr*sp(34,j)*(sp(12,j)+sp(13,j))
            r(41,j) = kh2sfeoh3pr*sp(35,j)*(sp(12,j)+sp(13,j))
            r(42,j) = kfeage*sp(5,j)
            r(43,j) = sw20*kcalppt*(sp(22,j)*sp(17,j)/kspcal-0.5E1)
            r(44,j) = sw21*karadis*sp(36,j)*(0.1E1-sp(22,j)*sp(17,j)/ksp
     +ara)
            r(45,j) = sw22*karappt*(sp(22,j)*sp(17,j)/kspara-0.5E1)
            r(46,j) = sw23*kmgcdis*sp(37,j)*(0.1E1-sp(22,j)*sp(17,j)/ksp
     +mgc)
            r(47,j) = sw24*kmgcppt*(sp(22,j)*sp(17,j)/kspmgc-0.1E1)
            r(48,j) = sw25*(kBSidis*bd+(kBSidis-kBSidis*bd)*exp(-ad*x_po
     +s))*sp(39,j)*(0.1E1-sp(40,j)/BSisat)
            r(49,j) = kf9*(sp(5,j)+sp(34,j)+sp(35,j))*sp(40,j)-kb9*sp(41
     +,j)
            r(50,j) = kf10*sp(43,j)*sp(20,j)-kb10*sp(42,j)
            r(51,j) = kf11*sp(44,j)*sp(20,j)**2-kb11*sp(42,j)
            r(52,j) = kf12*sp(45,j)*sp(20,j)**3-kb12*sp(42,j)
            r(53,j) = kf13*sp(46,j)*sp(20,j)**4-kb13*sp(42,j)
            r(54,j) = sw26*kglass*sp(47,j)*(sp(20,j)**3/sp(42,j))**(1.0/
     +3.0)*(0.1E1-sp(40,j)*sp(42,j)**0.36E0/sp(20,j)**0.108E1/kspglass)
            r(55,j) = sw27*kplagio*sp(48,j)*(sp(20,j)**3/sp(42,j))**0.35
     +E0*(0.1E1-sp(40,j)**0.23E1*sp(46,j)**0.17E1*sp(49,j)**0.3E0*sp(22,
     +j)**0.7E0/kspplagio)
            r(56,j) = sw28*kolive*sp(51,j)*(0.1E1-sp(40,j)*sp(38,j)**0.1
     +6E1*sp(11,j)**0.4E0/sp(20,j)**4/kspolive)
            r(57,j) = sw29*kpyrox*sp(52,j)*(0.1E1-sp(40,j)**2*sp(11,j)**
     +0.46E0*sp(38,j)**0.84E0*sp(22,j)**0.7E0/sp(20,j)**4/ksppyrox)
            r(58,j) = sw30*killite*(sp(40,j)**0.35E1*sp(42,j)**0.23E1*sp
     +(50,j)**0.6E0*sp(38,j)**0.25E0/sp(20,j)**8/kspillite-0.1E1)
            r(59,j) = sw31*ksmect*(sp(22,j)**0.165E0*sp(38,j)**0.33E0*sp
     +(42,j)**0.17E1*sp(40,j)**4/sp(20,j)**6/kspsmect-0.1E1)
            r(60,j) = sw32*kkaoli*(sp(42,j)**2*sp(40,j)**2/sp(20,j)**6/k
     +spkaoli-0.1E1)
      end
