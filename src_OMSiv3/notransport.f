c      
c     SUBROUTINE notransport
c      
      subroutine notransport(k,itransp)
        include 'common_geo.inc'
        include 'common.inc'
        integer k,itransp
            if (k.eq.1) then
              itransp = 1
            endif
      end
