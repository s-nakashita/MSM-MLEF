      subroutine lnczs(coef,sm,iw,jw,order)
!
      dimension coef(iw*jw)
      dimension sm(iw*jw)
      save icall
      data icall/0/
      pi = asin(1.0) * 2.0
      if( icall .eq. 0 ) then
        icall = 1
!       piom = pi / (iw-1)
!       pion = pi / (jw-1)
        piom = pi /  iw
        pion = pi /  jw
        mn=0
        do 10 mm=1,iw
        m = mm-1
        sx = float(m)*piom
        if( m .eq. 0 ) then
            sx=1.0
!       else if( mm .eq. iw ) then
!           sx=0.0
        else
            sx = sin( sx ) / sx
        endif
        do 10 nn=1,jw
        n = nn-1
        sy = float(n)*pion
        if( n .eq. 0 ) then
            sy=1.0
!       else if( nn .eq. jw ) then
!           sy=0.0
        else
            sy = sin( sy ) / sy
        endif
        mn = mn + 1
        if( sx.eq.0.0  .or.  sy.eq.0.0 ) then
           sm(mn) = 0.0
        else
           sm(mn) = ( sx * sy ) ** order
        endif
 10     continue
      endif
!
      do 100 l=1,iw*jw
      coef(l) = sm(l) * coef(l)
100   continue
      return
      end
