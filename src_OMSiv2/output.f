c      
c     SUBROUTINE out
c      
      subroutine out(j,nt,time,depth,v_out,v_int)
        include 'common_geo.inc'
        include 'common.inc'
        real*8 time
        if (nt.eq.1.and.j.eq.1) then
          open(unit=11,file='ch2o.dat',status='replace')
          close(11)
          open(unit=11,file='zzo2.dat',status='replace')
          close(11)
          open(unit=11,file='zno3.dat',status='replace')
          close(11)
          open(unit=11,file='mno2.dat',status='replace')
          close(11)
          open(unit=11,file='feoh3.dat',status='replace')
          close(11)
          open(unit=11,file='zso4.dat',status='replace')
          close(11)
          open(unit=11,file='zch4.dat',status='replace')
          close(11)
          open(unit=11,file='znh4.dat',status='replace')
          close(11)
          open(unit=11,file='zpo4.dat',status='replace')
          close(11)
          open(unit=11,file='zzmn.dat',status='replace')
          close(11)
          open(unit=11,file='zzfe.dat',status='replace')
          close(11)
          open(unit=11,file='zh2s.dat',status='replace')
          close(11)
          open(unit=11,file='zzhs.dat',status='replace')
          close(11)
          open(unit=11,file='ch4g.dat',status='replace')
          close(11)
          open(unit=11,file='h2co.dat',status='replace')
          close(11)
          open(unit=11,file='hco3.dat',status='replace')
          close(11)
          open(unit=11,file='zco3.dat',status='replace')
          close(11)
          open(unit=11,file='boh4.dat',status='replace')
          close(11)
          open(unit=11,file='boh3.dat',status='replace')
          close(11)
          open(unit=11,file='zzph.dat',status='replace')
          close(11)
          open(unit=11,file='caco.dat',status='replace')
          close(11)
          open(unit=11,file='zzca.dat',status='replace')
          close(11)
          open(unit=11,file='snh4.dat',status='replace')
          close(11)
          open(unit=11,file='spo4.dat',status='replace')
          close(11)
          open(unit=11,file='zfes.dat',status='replace')
          close(11)
          open(unit=11,file='feco.dat',status='replace')
          close(11)
          open(unit=11,file='zzs0.dat',status='replace')
          close(11)
          open(unit=11,file='fes2.dat',status='replace')
          close(11)
          open(unit=11,file='zsfe.dat',status='replace')
          close(11)
          open(unit=11,file='zfp.dat',status='replace')
          close(11)
          open(unit=11,file='mnco.dat',status='replace')
          close(11)
          open(unit=11,file='age.dat',status='replace')
          close(11)
          open(unit=11,file='mno2pr.dat',status='replace')
          close(11)
          open(unit=11,file='feoh3mr.dat',status='replace')
          close(11)
          open(unit=11,file='feoh3pr.dat',status='replace')
          close(11)
          open(unit=11,file='arago.dat',status='replace')
          close(11)
          open(unit=11,file='mgcal.dat',status='replace')
          close(11)
          open(unit=11,file='mg.dat',status='replace')
          close(11)
          open(unit=11,file='BSi.dat',status='replace')
          close(11)
          open(unit=11,file='DSi.dat',status='replace')
          close(11)
          open(unit=11,file='sfSi.dat',status='replace')
          close(11)
          open(unit=11,file='al.dat',status='replace')
          close(11)
          open(unit=11,file='aloh.dat',status='replace')
          close(11)
          open(unit=11,file='aloh2.dat',status='replace')
          close(11)
          open(unit=11,file='aloh3.dat',status='replace')
          close(11)
          open(unit=11,file='aloh4.dat',status='replace')
          close(11)
          open(unit=11,file='glass.dat',status='replace')
          close(11)
          open(unit=11,file='plagio.dat',status='replace')
          close(11)
          open(unit=11,file='na.dat',status='replace')
          close(11)
          open(unit=11,file='kplus.dat',status='replace')
          close(11)
          open(unit=11,file='olive.dat',status='replace')
          close(11)
          open(unit=11,file='pyrox.dat',status='replace')
          close(11)
          open(unit=11,file='illite.dat',status='replace')
          close(11)
          open(unit=11,file='smect.dat',status='replace')
          close(11)
          open(unit=11,file='kaoli.dat',status='replace')
          close(11)
          open(unit=11,file='xrate1.dat',status='replace')
          close(11)
          open(unit=11,file='xrate2.dat',status='replace')
          close(11)
          open(unit=11,file='xrate3.dat',status='replace')
          close(11)
          open(unit=11,file='xrate4.dat',status='replace')
          close(11)
          open(unit=11,file='xrate5.dat',status='replace')
          close(11)
          open(unit=11,file='xrate6.dat',status='replace')
          close(11)
          open(unit=11,file='xrate7.dat',status='replace')
          close(11)
          open(unit=11,file='xrate8.dat',status='replace')
          close(11)
          open(unit=11,file='xrate9.dat',status='replace')
          close(11)
          open(unit=11,file='xrate10.dat',status='replace')
          close(11)
          open(unit=11,file='xrate11.dat',status='replace')
          close(11)
          open(unit=11,file='xrate12.dat',status='replace')
          close(11)
          open(unit=11,file='xrate13.dat',status='replace')
          close(11)
          open(unit=11,file='xrate14.dat',status='replace')
          close(11)
          open(unit=11,file='xrate15.dat',status='replace')
          close(11)
          open(unit=11,file='xrate16.dat',status='replace')
          close(11)
          open(unit=11,file='xrate17.dat',status='replace')
          close(11)
          open(unit=11,file='xrate18.dat',status='replace')
          close(11)
          open(unit=11,file='xrate19.dat',status='replace')
          close(11)
          open(unit=11,file='xrate20.dat',status='replace')
          close(11)
          open(unit=11,file='xrate21.dat',status='replace')
          close(11)
          open(unit=11,file='xrate22.dat',status='replace')
          close(11)
          open(unit=11,file='xrate23.dat',status='replace')
          close(11)
          open(unit=11,file='xrate24.dat',status='replace')
          close(11)
          open(unit=11,file='xrate25.dat',status='replace')
          close(11)
          open(unit=11,file='xrate26.dat',status='replace')
          close(11)
          open(unit=11,file='xrate27.dat',status='replace')
          close(11)
          open(unit=11,file='xrate28.dat',status='replace')
          close(11)
          open(unit=11,file='xrate29.dat',status='replace')
          close(11)
          open(unit=11,file='xrate30.dat',status='replace')
          close(11)
          open(unit=11,file='xrate31.dat',status='replace')
          close(11)
          open(unit=11,file='xrate32.dat',status='replace')
          close(11)
          open(unit=11,file='xrate33.dat',status='replace')
          close(11)
          open(unit=11,file='xrate34.dat',status='replace')
          close(11)
          open(unit=11,file='xrate35.dat',status='replace')
          close(11)
          open(unit=11,file='xrate36.dat',status='replace')
          close(11)
          open(unit=11,file='xrate37.dat',status='replace')
          close(11)
          open(unit=11,file='xrate38.dat',status='replace')
          close(11)
          open(unit=11,file='xrate39.dat',status='replace')
          close(11)
          open(unit=11,file='xrate40.dat',status='replace')
          close(11)
          open(unit=11,file='xrate41.dat',status='replace')
          close(11)
          open(unit=11,file='xrate42.dat',status='replace')
          close(11)
          open(unit=11,file='xrate43.dat',status='replace')
          close(11)
          open(unit=11,file='xrate44.dat',status='replace')
          close(11)
          open(unit=11,file='xrate45.dat',status='replace')
          close(11)
          open(unit=11,file='xrate46.dat',status='replace')
          close(11)
          open(unit=11,file='xrate47.dat',status='replace')
          close(11)
          open(unit=11,file='xrate48.dat',status='replace')
          close(11)
          open(unit=11,file='xrate49.dat',status='replace')
          close(11)
          open(unit=11,file='xrate50.dat',status='replace')
          close(11)
          open(unit=11,file='xrate51.dat',status='replace')
          close(11)
          open(unit=11,file='xrate52.dat',status='replace')
          close(11)
          open(unit=11,file='xrate53.dat',status='replace')
          close(11)
          open(unit=11,file='xrate54.dat',status='replace')
          close(11)
          open(unit=11,file='xrate55.dat',status='replace')
          close(11)
          open(unit=11,file='xrate56.dat',status='replace')
          close(11)
          open(unit=11,file='xrate57.dat',status='replace')
          close(11)
          open(unit=11,file='xrate58.dat',status='replace')
          close(11)
          open(unit=11,file='xrate59.dat',status='replace')
          close(11)
          open(unit=11,file='xrate60.dat',status='replace')
          close(11)
          open(unit=11,file='ch2o.inp',status='replace')
          close(11)
          open(unit=11,file='zzo2.inp',status='replace')
          close(11)
          open(unit=11,file='zno3.inp',status='replace')
          close(11)
          open(unit=11,file='mno2.inp',status='replace')
          close(11)
          open(unit=11,file='feoh3.inp',status='replace')
          close(11)
          open(unit=11,file='zso4.inp',status='replace')
          close(11)
          open(unit=11,file='zch4.inp',status='replace')
          close(11)
          open(unit=11,file='znh4.inp',status='replace')
          close(11)
          open(unit=11,file='zpo4.inp',status='replace')
          close(11)
          open(unit=11,file='zzmn.inp',status='replace')
          close(11)
          open(unit=11,file='zzfe.inp',status='replace')
          close(11)
          open(unit=11,file='zh2s.inp',status='replace')
          close(11)
          open(unit=11,file='zzhs.inp',status='replace')
          close(11)
          open(unit=11,file='ch4g.inp',status='replace')
          close(11)
          open(unit=11,file='h2co.inp',status='replace')
          close(11)
          open(unit=11,file='hco3.inp',status='replace')
          close(11)
          open(unit=11,file='zco3.inp',status='replace')
          close(11)
          open(unit=11,file='boh4.inp',status='replace')
          close(11)
          open(unit=11,file='boh3.inp',status='replace')
          close(11)
          open(unit=11,file='zzph.inp',status='replace')
          close(11)
          open(unit=11,file='caco.inp',status='replace')
          close(11)
          open(unit=11,file='zzca.inp',status='replace')
          close(11)
          open(unit=11,file='snh4.inp',status='replace')
          close(11)
          open(unit=11,file='spo4.inp',status='replace')
          close(11)
          open(unit=11,file='zfes.inp',status='replace')
          close(11)
          open(unit=11,file='feco.inp',status='replace')
          close(11)
          open(unit=11,file='zzs0.inp',status='replace')
          close(11)
          open(unit=11,file='fes2.inp',status='replace')
          close(11)
          open(unit=11,file='zsfe.inp',status='replace')
          close(11)
          open(unit=11,file='zfp.inp',status='replace')
          close(11)
          open(unit=11,file='mnco.inp',status='replace')
          close(11)
          open(unit=11,file='age.inp',status='replace')
          close(11)
          open(unit=11,file='mno2pr.inp',status='replace')
          close(11)
          open(unit=11,file='feoh3mr.inp',status='replace')
          close(11)
          open(unit=11,file='feoh3pr.inp',status='replace')
          close(11)
          open(unit=11,file='arago.inp',status='replace')
          close(11)
          open(unit=11,file='mgcal.inp',status='replace')
          close(11)
          open(unit=11,file='mg.inp',status='replace')
          close(11)
          open(unit=11,file='BSi.inp',status='replace')
          close(11)
          open(unit=11,file='DSi.inp',status='replace')
          close(11)
          open(unit=11,file='sfSi.inp',status='replace')
          close(11)
          open(unit=11,file='al.inp',status='replace')
          close(11)
          open(unit=11,file='aloh.inp',status='replace')
          close(11)
          open(unit=11,file='aloh2.inp',status='replace')
          close(11)
          open(unit=11,file='aloh3.inp',status='replace')
          close(11)
          open(unit=11,file='aloh4.inp',status='replace')
          close(11)
          open(unit=11,file='glass.inp',status='replace')
          close(11)
          open(unit=11,file='plagio.inp',status='replace')
          close(11)
          open(unit=11,file='na.inp',status='replace')
          close(11)
          open(unit=11,file='kplus.inp',status='replace')
          close(11)
          open(unit=11,file='olive.inp',status='replace')
          close(11)
          open(unit=11,file='pyrox.inp',status='replace')
          close(11)
          open(unit=11,file='illite.inp',status='replace')
          close(11)
          open(unit=11,file='smect.inp',status='replace')
          close(11)
          open(unit=11,file='kaoli.inp',status='replace')
          close(11)
        v_out = 0.36144D3
        v_int = 0.4016D2
        endif
        if (time.le.v_out.and.v_out.lt.time+delt) then
          open(unit=11,file='ch2o.dat',
     +      status='old',access='append')
          write(11,2000) sp(1,j),depth
 2000     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzo2.dat',
     +      status='old',access='append')
          write(11,2001) sp(2,j),depth
 2001     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zno3.dat',
     +      status='old',access='append')
          write(11,2002) sp(3,j),depth
 2002     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mno2.dat',
     +      status='old',access='append')
          write(11,2003) sp(4,j),depth
 2003     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='feoh3.dat',
     +      status='old',access='append')
          write(11,2004) sp(5,j),depth
 2004     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zso4.dat',
     +      status='old',access='append')
          write(11,2005) sp(6,j),depth
 2005     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zch4.dat',
     +      status='old',access='append')
          write(11,2006) sp(7,j),depth
 2006     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='znh4.dat',
     +      status='old',access='append')
          write(11,2007) sp(8,j),depth
 2007     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zpo4.dat',
     +      status='old',access='append')
          write(11,2008) sp(9,j),depth
 2008     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzmn.dat',
     +      status='old',access='append')
          write(11,2009) sp(10,j),depth
 2009     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzfe.dat',
     +      status='old',access='append')
          write(11,2010) sp(11,j),depth
 2010     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zh2s.dat',
     +      status='old',access='append')
          write(11,2011) sp(12,j),depth
 2011     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzhs.dat',
     +      status='old',access='append')
          write(11,2012) sp(13,j),depth
 2012     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='ch4g.dat',
     +      status='old',access='append')
          write(11,2013) sp(14,j),depth
 2013     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='h2co.dat',
     +      status='old',access='append')
          write(11,2014) sp(15,j),depth
 2014     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='hco3.dat',
     +      status='old',access='append')
          write(11,2015) sp(16,j),depth
 2015     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zco3.dat',
     +      status='old',access='append')
          write(11,2016) sp(17,j),depth
 2016     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='boh4.dat',
     +      status='old',access='append')
          write(11,2017) sp(18,j),depth
 2017     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='boh3.dat',
     +      status='old',access='append')
          write(11,2018) sp(19,j),depth
 2018     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzph.dat',
     +      status='old',access='append')
          write(11,2019) sp(20,j),depth
 2019     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='caco.dat',
     +      status='old',access='append')
          write(11,2020) sp(21,j),depth
 2020     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzca.dat',
     +      status='old',access='append')
          write(11,2021) sp(22,j),depth
 2021     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='snh4.dat',
     +      status='old',access='append')
          write(11,2022) sp(23,j),depth
 2022     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='spo4.dat',
     +      status='old',access='append')
          write(11,2023) sp(24,j),depth
 2023     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zfes.dat',
     +      status='old',access='append')
          write(11,2024) sp(25,j),depth
 2024     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='feco.dat',
     +      status='old',access='append')
          write(11,2025) sp(26,j),depth
 2025     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzs0.dat',
     +      status='old',access='append')
          write(11,2026) sp(27,j),depth
 2026     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='fes2.dat',
     +      status='old',access='append')
          write(11,2027) sp(28,j),depth
 2027     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zsfe.dat',
     +      status='old',access='append')
          write(11,2028) sp(29,j),depth
 2028     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zfp.dat',
     +      status='old',access='append')
          write(11,2029) sp(30,j),depth
 2029     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mnco.dat',
     +      status='old',access='append')
          write(11,2030) sp(31,j),depth
 2030     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='age.dat',
     +      status='old',access='append')
          write(11,2031) sp(32,j),depth
 2031     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mno2pr.dat',
     +      status='old',access='append')
          write(11,2032) sp(33,j),depth
 2032     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='feoh3mr.dat',
     +      status='old',access='append')
          write(11,2033) sp(34,j),depth
 2033     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='feoh3pr.dat',
     +      status='old',access='append')
          write(11,2034) sp(35,j),depth
 2034     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='arago.dat',
     +      status='old',access='append')
          write(11,2035) sp(36,j),depth
 2035     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mgcal.dat',
     +      status='old',access='append')
          write(11,2036) sp(37,j),depth
 2036     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mg.dat',
     +      status='old',access='append')
          write(11,2037) sp(38,j),depth
 2037     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='BSi.dat',
     +      status='old',access='append')
          write(11,2038) sp(39,j),depth
 2038     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='DSi.dat',
     +      status='old',access='append')
          write(11,2039) sp(40,j),depth
 2039     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='sfSi.dat',
     +      status='old',access='append')
          write(11,2040) sp(41,j),depth
 2040     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='al.dat',
     +      status='old',access='append')
          write(11,2041) sp(42,j),depth
 2041     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='aloh.dat',
     +      status='old',access='append')
          write(11,2042) sp(43,j),depth
 2042     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='aloh2.dat',
     +      status='old',access='append')
          write(11,2043) sp(44,j),depth
 2043     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='aloh3.dat',
     +      status='old',access='append')
          write(11,2044) sp(45,j),depth
 2044     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='aloh4.dat',
     +      status='old',access='append')
          write(11,2045) sp(46,j),depth
 2045     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='glass.dat',
     +      status='old',access='append')
          write(11,2046) sp(47,j),depth
 2046     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='plagio.dat',
     +      status='old',access='append')
          write(11,2047) sp(48,j),depth
 2047     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='na.dat',
     +      status='old',access='append')
          write(11,2048) sp(49,j),depth
 2048     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='kplus.dat',
     +      status='old',access='append')
          write(11,2049) sp(50,j),depth
 2049     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='olive.dat',
     +      status='old',access='append')
          write(11,2050) sp(51,j),depth
 2050     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='pyrox.dat',
     +      status='old',access='append')
          write(11,2051) sp(52,j),depth
 2051     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='illite.dat',
     +      status='old',access='append')
          write(11,2052) sp(53,j),depth
 2052     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='smect.dat',
     +      status='old',access='append')
          write(11,2053) sp(54,j),depth
 2053     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='kaoli.dat',
     +      status='old',access='append')
          write(11,2054) sp(55,j),depth
 2054     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate1.dat',
     +      status='old',access='append')
          write(11,2055) r(1,j),depth
 2055     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate2.dat',
     +      status='old',access='append')
          write(11,2056) r(2,j),depth
 2056     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate3.dat',
     +      status='old',access='append')
          write(11,2057) r(3,j),depth
 2057     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate4.dat',
     +      status='old',access='append')
          write(11,2058) r(4,j),depth
 2058     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate5.dat',
     +      status='old',access='append')
          write(11,2059) r(5,j),depth
 2059     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate6.dat',
     +      status='old',access='append')
          write(11,2060) r(6,j),depth
 2060     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate7.dat',
     +      status='old',access='append')
          write(11,2061) r(7,j),depth
 2061     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate8.dat',
     +      status='old',access='append')
          write(11,2062) r(8,j),depth
 2062     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate9.dat',
     +      status='old',access='append')
          write(11,2063) r(9,j),depth
 2063     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate10.dat',
     +      status='old',access='append')
          write(11,2064) r(10,j),depth
 2064     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate11.dat',
     +      status='old',access='append')
          write(11,2065) r(11,j),depth
 2065     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate12.dat',
     +      status='old',access='append')
          write(11,2066) r(12,j),depth
 2066     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate13.dat',
     +      status='old',access='append')
          write(11,2067) r(13,j),depth
 2067     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate14.dat',
     +      status='old',access='append')
          write(11,2068) r(14,j),depth
 2068     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate15.dat',
     +      status='old',access='append')
          write(11,2069) r(15,j),depth
 2069     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate16.dat',
     +      status='old',access='append')
          write(11,2070) r(16,j),depth
 2070     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate17.dat',
     +      status='old',access='append')
          write(11,2071) r(17,j),depth
 2071     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate18.dat',
     +      status='old',access='append')
          write(11,2072) r(18,j),depth
 2072     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate19.dat',
     +      status='old',access='append')
          write(11,2073) r(19,j),depth
 2073     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate20.dat',
     +      status='old',access='append')
          write(11,2074) r(20,j),depth
 2074     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate21.dat',
     +      status='old',access='append')
          write(11,2075) r(21,j),depth
 2075     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate22.dat',
     +      status='old',access='append')
          write(11,2076) r(22,j),depth
 2076     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate23.dat',
     +      status='old',access='append')
          write(11,2077) r(23,j),depth
 2077     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate24.dat',
     +      status='old',access='append')
          write(11,2078) r(24,j),depth
 2078     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate25.dat',
     +      status='old',access='append')
          write(11,2079) r(25,j),depth
 2079     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate26.dat',
     +      status='old',access='append')
          write(11,2080) r(26,j),depth
 2080     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate27.dat',
     +      status='old',access='append')
          write(11,2081) r(27,j),depth
 2081     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate28.dat',
     +      status='old',access='append')
          write(11,2082) r(28,j),depth
 2082     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate29.dat',
     +      status='old',access='append')
          write(11,2083) r(29,j),depth
 2083     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate30.dat',
     +      status='old',access='append')
          write(11,2084) r(30,j),depth
 2084     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate31.dat',
     +      status='old',access='append')
          write(11,2085) r(31,j),depth
 2085     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate32.dat',
     +      status='old',access='append')
          write(11,2086) r(32,j),depth
 2086     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate33.dat',
     +      status='old',access='append')
          write(11,2087) r(33,j),depth
 2087     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate34.dat',
     +      status='old',access='append')
          write(11,2088) r(34,j),depth
 2088     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate35.dat',
     +      status='old',access='append')
          write(11,2089) r(35,j),depth
 2089     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate36.dat',
     +      status='old',access='append')
          write(11,2090) r(36,j),depth
 2090     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate37.dat',
     +      status='old',access='append')
          write(11,2091) r(37,j),depth
 2091     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate38.dat',
     +      status='old',access='append')
          write(11,2092) r(38,j),depth
 2092     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate39.dat',
     +      status='old',access='append')
          write(11,2093) r(39,j),depth
 2093     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate40.dat',
     +      status='old',access='append')
          write(11,2094) r(40,j),depth
 2094     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate41.dat',
     +      status='old',access='append')
          write(11,2095) r(41,j),depth
 2095     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate42.dat',
     +      status='old',access='append')
          write(11,2096) r(42,j),depth
 2096     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate43.dat',
     +      status='old',access='append')
          write(11,2097) r(43,j),depth
 2097     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate44.dat',
     +      status='old',access='append')
          write(11,2098) r(44,j),depth
 2098     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate45.dat',
     +      status='old',access='append')
          write(11,2099) r(45,j),depth
 2099     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate46.dat',
     +      status='old',access='append')
          write(11,2100) r(46,j),depth
 2100     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate47.dat',
     +      status='old',access='append')
          write(11,2101) r(47,j),depth
 2101     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate48.dat',
     +      status='old',access='append')
          write(11,2102) r(48,j),depth
 2102     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate49.dat',
     +      status='old',access='append')
          write(11,2103) r(49,j),depth
 2103     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate50.dat',
     +      status='old',access='append')
          write(11,2104) r(50,j),depth
 2104     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate51.dat',
     +      status='old',access='append')
          write(11,2105) r(51,j),depth
 2105     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate52.dat',
     +      status='old',access='append')
          write(11,2106) r(52,j),depth
 2106     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate53.dat',
     +      status='old',access='append')
          write(11,2107) r(53,j),depth
 2107     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate54.dat',
     +      status='old',access='append')
          write(11,2108) r(54,j),depth
 2108     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate55.dat',
     +      status='old',access='append')
          write(11,2109) r(55,j),depth
 2109     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate56.dat',
     +      status='old',access='append')
          write(11,2110) r(56,j),depth
 2110     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate57.dat',
     +      status='old',access='append')
          write(11,2111) r(57,j),depth
 2111     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate58.dat',
     +      status='old',access='append')
          write(11,2112) r(58,j),depth
 2112     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate59.dat',
     +      status='old',access='append')
          write(11,2113) r(59,j),depth
 2113     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='xrate60.dat',
     +      status='old',access='append')
          write(11,2114) r(60,j),depth
 2114     format(1x,e14.7,2x,f12.4)
          close(11)
        if (j.eq.nx) then
        v_out = v_out+v_int
        endif
        endif
        if (time.eq.endt) then
          open(unit=11,file='ch2o.inp',
     +      status='old',access='append')
          write(11,2115) sp(1,j),depth
 2115     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzo2.inp',
     +      status='old',access='append')
          write(11,2116) sp(2,j),depth
 2116     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zno3.inp',
     +      status='old',access='append')
          write(11,2117) sp(3,j),depth
 2117     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mno2.inp',
     +      status='old',access='append')
          write(11,2118) sp(4,j),depth
 2118     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='feoh3.inp',
     +      status='old',access='append')
          write(11,2119) sp(5,j),depth
 2119     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zso4.inp',
     +      status='old',access='append')
          write(11,2120) sp(6,j),depth
 2120     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zch4.inp',
     +      status='old',access='append')
          write(11,2121) sp(7,j),depth
 2121     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='znh4.inp',
     +      status='old',access='append')
          write(11,2122) sp(8,j),depth
 2122     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zpo4.inp',
     +      status='old',access='append')
          write(11,2123) sp(9,j),depth
 2123     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzmn.inp',
     +      status='old',access='append')
          write(11,2124) sp(10,j),depth
 2124     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzfe.inp',
     +      status='old',access='append')
          write(11,2125) sp(11,j),depth
 2125     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zh2s.inp',
     +      status='old',access='append')
          write(11,2126) sp(12,j),depth
 2126     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzhs.inp',
     +      status='old',access='append')
          write(11,2127) sp(13,j),depth
 2127     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='ch4g.inp',
     +      status='old',access='append')
          write(11,2128) sp(14,j),depth
 2128     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='h2co.inp',
     +      status='old',access='append')
          write(11,2129) sp(15,j),depth
 2129     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='hco3.inp',
     +      status='old',access='append')
          write(11,2130) sp(16,j),depth
 2130     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zco3.inp',
     +      status='old',access='append')
          write(11,2131) sp(17,j),depth
 2131     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='boh4.inp',
     +      status='old',access='append')
          write(11,2132) sp(18,j),depth
 2132     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='boh3.inp',
     +      status='old',access='append')
          write(11,2133) sp(19,j),depth
 2133     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzph.inp',
     +      status='old',access='append')
          write(11,2134) sp(20,j),depth
 2134     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='caco.inp',
     +      status='old',access='append')
          write(11,2135) sp(21,j),depth
 2135     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzca.inp',
     +      status='old',access='append')
          write(11,2136) sp(22,j),depth
 2136     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='snh4.inp',
     +      status='old',access='append')
          write(11,2137) sp(23,j),depth
 2137     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='spo4.inp',
     +      status='old',access='append')
          write(11,2138) sp(24,j),depth
 2138     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zfes.inp',
     +      status='old',access='append')
          write(11,2139) sp(25,j),depth
 2139     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='feco.inp',
     +      status='old',access='append')
          write(11,2140) sp(26,j),depth
 2140     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zzs0.inp',
     +      status='old',access='append')
          write(11,2141) sp(27,j),depth
 2141     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='fes2.inp',
     +      status='old',access='append')
          write(11,2142) sp(28,j),depth
 2142     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zsfe.inp',
     +      status='old',access='append')
          write(11,2143) sp(29,j),depth
 2143     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='zfp.inp',
     +      status='old',access='append')
          write(11,2144) sp(30,j),depth
 2144     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mnco.inp',
     +      status='old',access='append')
          write(11,2145) sp(31,j),depth
 2145     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='age.inp',
     +      status='old',access='append')
          write(11,2146) sp(32,j),depth
 2146     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mno2pr.inp',
     +      status='old',access='append')
          write(11,2147) sp(33,j),depth
 2147     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='feoh3mr.inp',
     +      status='old',access='append')
          write(11,2148) sp(34,j),depth
 2148     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='feoh3pr.inp',
     +      status='old',access='append')
          write(11,2149) sp(35,j),depth
 2149     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='arago.inp',
     +      status='old',access='append')
          write(11,2150) sp(36,j),depth
 2150     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mgcal.inp',
     +      status='old',access='append')
          write(11,2151) sp(37,j),depth
 2151     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='mg.inp',
     +      status='old',access='append')
          write(11,2152) sp(38,j),depth
 2152     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='BSi.inp',
     +      status='old',access='append')
          write(11,2153) sp(39,j),depth
 2153     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='DSi.inp',
     +      status='old',access='append')
          write(11,2154) sp(40,j),depth
 2154     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='sfSi.inp',
     +      status='old',access='append')
          write(11,2155) sp(41,j),depth
 2155     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='al.inp',
     +      status='old',access='append')
          write(11,2156) sp(42,j),depth
 2156     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='aloh.inp',
     +      status='old',access='append')
          write(11,2157) sp(43,j),depth
 2157     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='aloh2.inp',
     +      status='old',access='append')
          write(11,2158) sp(44,j),depth
 2158     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='aloh3.inp',
     +      status='old',access='append')
          write(11,2159) sp(45,j),depth
 2159     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='aloh4.inp',
     +      status='old',access='append')
          write(11,2160) sp(46,j),depth
 2160     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='glass.inp',
     +      status='old',access='append')
          write(11,2161) sp(47,j),depth
 2161     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='plagio.inp',
     +      status='old',access='append')
          write(11,2162) sp(48,j),depth
 2162     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='na.inp',
     +      status='old',access='append')
          write(11,2163) sp(49,j),depth
 2163     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='kplus.inp',
     +      status='old',access='append')
          write(11,2164) sp(50,j),depth
 2164     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='olive.inp',
     +      status='old',access='append')
          write(11,2165) sp(51,j),depth
 2165     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='pyrox.inp',
     +      status='old',access='append')
          write(11,2166) sp(52,j),depth
 2166     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='illite.inp',
     +      status='old',access='append')
          write(11,2167) sp(53,j),depth
 2167     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='smect.inp',
     +      status='old',access='append')
          write(11,2168) sp(54,j),depth
 2168     format(1x,e14.7,2x,f12.4)
          close(11)
          open(unit=11,file='kaoli.inp',
     +      status='old',access='append')
          write(11,2169) sp(55,j),depth
 2169     format(1x,e14.7,2x,f12.4)
          close(11)
        endif
      end
