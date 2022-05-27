      subroutine sigset(ci, si, del, sl, cl, rpi, km)
! ===== modified to run regional spectral model =====
      use physcons, rocp => con_rocp
! sl and si are given
      dimension ci(km+1), si(km+1),                                      &          
     & del(km), sl(km), cl(km), rpi(km-1)
!
      print *,' ----- begin rsetsig ----'
!
      do 10 k=1,km
      del(k)=si(k)-si(k+1)
      cl(k)=1.0e0-sl(k)
10    continue
      do 20 k=1,km+1
      ci(k)=1.0e0-si(k)
20    continue
      sumdel=0.e0
      do 30 k=1,km
      sumdel=sumdel+del(k)
30    continue
!
      rk = rocp
! compute pi ratios for temp. matrix.
      do 40 le=1,km-1
      base = sl(le+1)/sl(le)
40    rpi(le) = base**rk
!
      do 50 le=1,km+1
      print 100, le, ci(le), si(le)
100   format (1h , 'level=', i2, 2x, 'ci=', f6.3, 2x, 'si=', f6.3)
50    continue
      print 97
97    format (1h0)
      do 60 le=1,km
      print 101, le, cl(le), sl(le), del(le)
101   format (1h , 'layer=', i2, 2x, 'cl=', f6.3, 2x, 'sl=', f6.3, 2x,   &                        
     & 'del=', f6.3)
60    continue
      print 102, (rpi(le), le=1,km-1)
102   format (1h0, 'rpi=', (18(1x,f6.3)) )
      print 99,sumdel
99    format (1h0,' ---- end sigse --- sumdel(should be 1)=',e13.4)
      return
      end
