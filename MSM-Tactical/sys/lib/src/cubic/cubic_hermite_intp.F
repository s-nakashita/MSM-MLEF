      subroutine cubic_hermite_intp(leni,loci,vali,slpi,leno,loco,valo)
!
! use hermite cubic spline to interpolate vali at loci to valo at loco
! output location should be inside of input location.
!
      implicit none

      integer            leni,leno
      real               loci(leni),vali(leni),slpi(leni)
      real               loco(leno),valo(leno)
!
      integer            i,j,k
      real               delx,loct,loct2,loct3,h00,h01,h10,h11
!
      k=1
      do i=1,leno
        do j=k,leni-1
          if( loco(i).ge.loci(j) .and. loco(i).lt.loci(j+1) ) then
            k=j
            go to 100
          endif
        enddo
 100    delx=loci(k+1)-loci(k)
        loct=(loco(i)-loci(k))/delx
        loct2=loct*loct
        loct3=loct2*loct
        h00 = 2.*loct3 - 3.*loct2 + 1.
        h10 = loct3 - 2.*loct2 + loct 
        h01 = -2.*loct3 + 3.*loct2
        h11 = loct3 - loct2
        valo(i) = h00*vali(k)+h10*slpi(k)+h01*vali(k+1)+h11*slpi(k+1)
      enddo
!
      return
      end
