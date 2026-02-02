c      
c     SUBROUTINE initialcond
c      
      subroutine initialcond()
          include 'common_geo.inc'
          include 'common.inc'
          include 'common_drive.inc'
        real*8 spi(ncomp)
          if (ic.eq.1) then
          open(unit=85,file='initialconc.txt',status='old')
c           
          do 1000, j=1,nx,2
            read(85,*) de,(sp(i,j),i=1,ncomp) 
 1000     continue
c         
          close(85)
          endif
          if (ic.eq.2) then
              spi(1) = 0.2083333333D-2
              spi(2) = 0.195D-6
              spi(3) = 0.173D-7
              spi(4) = 0.D0
              spi(5) = 0.D0
              spi(6) = 0.28D-4
              spi(7) = 0.D0
              spi(8) = 0.D0
              spi(9) = 0.1D-8
              spi(10) = 0.D0
              spi(11) = 0.D0
              spi(12) = 0.D0
              spi(13) = 0.D0
              spi(14) = 0.D0
              spi(15) = 0.14985724D-7
              spi(16) = 0.1913571534D-5
              spi(17) = 0.1586379795D-6
              spi(18) = 0.732678405D-7
              spi(19) = 0.3427321595D-6
              spi(20) = 0.7943282347D-11
              spi(21) = 0.178D-2
              spi(22) = 0.1D-4
              spi(23) = 0.D0
              spi(24) = 0.D0
              spi(25) = 0.D0
              spi(26) = 0.D0
              spi(27) = 0.D0
              spi(28) = 0.D0
              spi(29) = 0.D0
              spi(30) = 0.D0
              spi(31) = 0.D0
              spi(32) = 0.D0
              spi(33) = 0.D0
              spi(34) = 0.D0
              spi(35) = 0.D0
              spi(36) = 0.953D-3
              spi(37) = 0.D0
              spi(38) = 0.523D-4
              spi(39) = 0.15D-2
              spi(40) = 0.173D-7
              spi(41) = 0.D0
              spi(42) = 0.3045803248D-17
              spi(43) = 0.1935093015D-14
              spi(44) = 0.1760766819D-13
              spi(45) = 0.5911556509D-12
              spi(46) = 0.1993892985D-9
              spi(47) = 0.72D-3
              spi(48) = 0.D0
              spi(49) = 0.4691D-3
              spi(50) = 0.102D-4
              spi(51) = 0.17D-3
              spi(52) = 0.D0
              spi(53) = 0.D0
              spi(54) = 0.D0
              spi(55) = 0.D0
c             
            do 1001, i=1,ncomp
c               
              do 1002, j=1,nx,2
                sp(i,j) = spi(i)
 1002         continue
c             
 1001       continue
c           
          endif
          if (ic.eq.3) then
              open(unit=101,file='dummy_1.inp',status='old')
              open(unit=102,file='dummy_2.inp',status='old')
              open(unit=103,file='dummy_3.inp',status='old')
              open(unit=104,file='dummy_4.inp',status='old')
              open(unit=105,file='dummy_5.inp',status='old')
              open(unit=106,file='dummy_6.inp',status='old')
              open(unit=107,file='dummy_7.inp',status='old')
              open(unit=108,file='dummy_8.inp',status='old')
              open(unit=109,file='dummy_9.inp',status='old')
              open(unit=110,file='dummy_10.inp',status='old')
              open(unit=111,file='dummy_11.inp',status='old')
              open(unit=112,file='dummy_12.inp',status='old')
              open(unit=113,file='dummy_13.inp',status='old')
              open(unit=114,file='dummy_14.inp',status='old')
              open(unit=115,file='dummy_15.inp',status='old')
              open(unit=116,file='dummy_16.inp',status='old')
              open(unit=117,file='dummy_17.inp',status='old')
              open(unit=118,file='dummy_18.inp',status='old')
              open(unit=119,file='dummy_19.inp',status='old')
              open(unit=120,file='dummy_20.inp',status='old')
              open(unit=121,file='dummy_21.inp',status='old')
              open(unit=122,file='dummy_22.inp',status='old')
              open(unit=123,file='dummy_23.inp',status='old')
              open(unit=124,file='dummy_24.inp',status='old')
              open(unit=125,file='dummy_25.inp',status='old')
              open(unit=126,file='dummy_26.inp',status='old')
              open(unit=127,file='dummy_27.inp',status='old')
              open(unit=128,file='dummy_28.inp',status='old')
              open(unit=129,file='dummy_29.inp',status='old')
              open(unit=130,file='dummy_30.inp',status='old')
              open(unit=131,file='dummy_31.inp',status='old')
              open(unit=132,file='dummy_32.inp',status='old')
              open(unit=133,file='dummy_33.inp',status='old')
              open(unit=134,file='dummy_34.inp',status='old')
              open(unit=135,file='dummy_35.inp',status='old')
              open(unit=136,file='dummy_36.inp',status='old')
              open(unit=137,file='dummy_37.inp',status='old')
              open(unit=138,file='dummy_38.inp',status='old')
              open(unit=139,file='dummy_39.inp',status='old')
              open(unit=140,file='dummy_40.inp',status='old')
              open(unit=141,file='dummy_41.inp',status='old')
              open(unit=142,file='dummy_42.inp',status='old')
              open(unit=143,file='dummy_43.inp',status='old')
              open(unit=144,file='dummy_44.inp',status='old')
              open(unit=145,file='dummy_45.inp',status='old')
              open(unit=146,file='dummy_46.inp',status='old')
              open(unit=147,file='dummy_47.inp',status='old')
              open(unit=148,file='dummy_48.inp',status='old')
              open(unit=149,file='dummy_49.inp',status='old')
              open(unit=150,file='dummy_50.inp',status='old')
              open(unit=151,file='dummy_51.inp',status='old')
              open(unit=152,file='dummy_52.inp',status='old')
              open(unit=153,file='dummy_53.inp',status='old')
              open(unit=154,file='dummy_54.inp',status='old')
              open(unit=155,file='dummy_55.inp',status='old')
c             
            do 1003, j=1,nx,2
                read(101,2000) sp(1,j),depth
 2000           format(1x,e14.7,2x,f8.4)
                read(102,2001) sp(2,j),depth
 2001           format(1x,e14.7,2x,f8.4)
                read(103,2002) sp(3,j),depth
 2002           format(1x,e14.7,2x,f8.4)
                read(104,2003) sp(4,j),depth
 2003           format(1x,e14.7,2x,f8.4)
                read(105,2004) sp(5,j),depth
 2004           format(1x,e14.7,2x,f8.4)
                read(106,2005) sp(6,j),depth
 2005           format(1x,e14.7,2x,f8.4)
                read(107,2006) sp(7,j),depth
 2006           format(1x,e14.7,2x,f8.4)
                read(108,2007) sp(8,j),depth
 2007           format(1x,e14.7,2x,f8.4)
                read(109,2008) sp(9,j),depth
 2008           format(1x,e14.7,2x,f8.4)
                read(110,2009) sp(10,j),depth
 2009           format(1x,e14.7,2x,f8.4)
                read(111,2010) sp(11,j),depth
 2010           format(1x,e14.7,2x,f8.4)
                read(112,2011) sp(12,j),depth
 2011           format(1x,e14.7,2x,f8.4)
                read(113,2012) sp(13,j),depth
 2012           format(1x,e14.7,2x,f8.4)
                read(114,2013) sp(14,j),depth
 2013           format(1x,e14.7,2x,f8.4)
                read(115,2014) sp(15,j),depth
 2014           format(1x,e14.7,2x,f8.4)
                read(116,2015) sp(16,j),depth
 2015           format(1x,e14.7,2x,f8.4)
                read(117,2016) sp(17,j),depth
 2016           format(1x,e14.7,2x,f8.4)
                read(118,2017) sp(18,j),depth
 2017           format(1x,e14.7,2x,f8.4)
                read(119,2018) sp(19,j),depth
 2018           format(1x,e14.7,2x,f8.4)
                read(120,2019) sp(20,j),depth
 2019           format(1x,e14.7,2x,f8.4)
                read(121,2020) sp(21,j),depth
 2020           format(1x,e14.7,2x,f8.4)
                read(122,2021) sp(22,j),depth
 2021           format(1x,e14.7,2x,f8.4)
                read(123,2022) sp(23,j),depth
 2022           format(1x,e14.7,2x,f8.4)
                read(124,2023) sp(24,j),depth
 2023           format(1x,e14.7,2x,f8.4)
                read(125,2024) sp(25,j),depth
 2024           format(1x,e14.7,2x,f8.4)
                read(126,2025) sp(26,j),depth
 2025           format(1x,e14.7,2x,f8.4)
                read(127,2026) sp(27,j),depth
 2026           format(1x,e14.7,2x,f8.4)
                read(128,2027) sp(28,j),depth
 2027           format(1x,e14.7,2x,f8.4)
                read(129,2028) sp(29,j),depth
 2028           format(1x,e14.7,2x,f8.4)
                read(130,2029) sp(30,j),depth
 2029           format(1x,e14.7,2x,f8.4)
                read(131,2030) sp(31,j),depth
 2030           format(1x,e14.7,2x,f8.4)
                read(132,2031) sp(32,j),depth
 2031           format(1x,e14.7,2x,f8.4)
                read(133,2032) sp(33,j),depth
 2032           format(1x,e14.7,2x,f8.4)
                read(134,2033) sp(34,j),depth
 2033           format(1x,e14.7,2x,f8.4)
                read(135,2034) sp(35,j),depth
 2034           format(1x,e14.7,2x,f8.4)
                read(136,2035) sp(36,j),depth
 2035           format(1x,e14.7,2x,f8.4)
                read(137,2036) sp(37,j),depth
 2036           format(1x,e14.7,2x,f8.4)
                read(138,2037) sp(38,j),depth
 2037           format(1x,e14.7,2x,f8.4)
                read(139,2038) sp(39,j),depth
 2038           format(1x,e14.7,2x,f8.4)
                read(140,2039) sp(40,j),depth
 2039           format(1x,e14.7,2x,f8.4)
                read(141,2040) sp(41,j),depth
 2040           format(1x,e14.7,2x,f8.4)
                read(142,2041) sp(42,j),depth
 2041           format(1x,e14.7,2x,f8.4)
                read(143,2042) sp(43,j),depth
 2042           format(1x,e14.7,2x,f8.4)
                read(144,2043) sp(44,j),depth
 2043           format(1x,e14.7,2x,f8.4)
                read(145,2044) sp(45,j),depth
 2044           format(1x,e14.7,2x,f8.4)
                read(146,2045) sp(46,j),depth
 2045           format(1x,e14.7,2x,f8.4)
                read(147,2046) sp(47,j),depth
 2046           format(1x,e14.7,2x,f8.4)
                read(148,2047) sp(48,j),depth
 2047           format(1x,e14.7,2x,f8.4)
                read(149,2048) sp(49,j),depth
 2048           format(1x,e14.7,2x,f8.4)
                read(150,2049) sp(50,j),depth
 2049           format(1x,e14.7,2x,f8.4)
                read(151,2050) sp(51,j),depth
 2050           format(1x,e14.7,2x,f8.4)
                read(152,2051) sp(52,j),depth
 2051           format(1x,e14.7,2x,f8.4)
                read(153,2052) sp(53,j),depth
 2052           format(1x,e14.7,2x,f8.4)
                read(154,2053) sp(54,j),depth
 2053           format(1x,e14.7,2x,f8.4)
                read(155,2054) sp(55,j),depth
 2054           format(1x,e14.7,2x,f8.4)
 1003       continue
c           
              close(101)
              close(102)
              close(103)
              close(104)
              close(105)
              close(106)
              close(107)
              close(108)
              close(109)
              close(110)
              close(111)
              close(112)
              close(113)
              close(114)
              close(115)
              close(116)
              close(117)
              close(118)
              close(119)
              close(120)
              close(121)
              close(122)
              close(123)
              close(124)
              close(125)
              close(126)
              close(127)
              close(128)
              close(129)
              close(130)
              close(131)
              close(132)
              close(133)
              close(134)
              close(135)
              close(136)
              close(137)
              close(138)
              close(139)
              close(140)
              close(141)
              close(142)
              close(143)
              close(144)
              close(145)
              close(146)
              close(147)
              close(148)
              close(149)
              close(150)
              close(151)
              close(152)
              close(153)
              close(154)
              close(155)
          endif
      end
