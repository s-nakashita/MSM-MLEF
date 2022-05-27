      subroutine cubic_mass_intp(leni,loci,massi,vali,                  &
     &                           leno,loco,masso,valo)
!
!  based on intgral mass to solve the progile for next grid with integral 
!  mass. The profile is based on cubic hermite spline. The integral of
!  the profile is given to solve the interface values, and derivatives.
!
      implicit none
!
      integer      leni,leno
      real         massi(leni),masso(leno)
      real         loci(leni+1),loco(leno+1)
      real         vali(leni+1),valo(leno+1)
!
      real         deli(leni),delo(leno),fdel,gdel
      real         aa(leni),bb(leni),cc(leni),dd(leni),ff(leni)
      integer      k
!
! prepare coefficients to solve the profile in terms of value at interfaces
!
      do k=1,leni
        deli(k)=loci(k+1)-loci(k)
      enddo
!
      aa(1)=0.0
      aa(2)=0.0
      do k=3,leni
        aa(k)=-deli(k)/deli(k-1)
      enddo
!
      bb(1)=0.0
      do k=2,leni
        bb(k)=12.+deli(k)/deli(k-1)
      enddo
      bb(leni)=bb(leni)+1.0
!
      do k=1,leni-1
        cc(k)=12+deli(k)/deli(k+1)
      enddo
      cc(1)=cc(1)+1.0
      cc(leni)=11
!
      do k=1,leni-1
        dd(k)=-deli(k)/deli(k+1)
      enddo
      dd(leni)=0.0
!
      vali(1)=massi(1)
!2    vali(1)=massi(1)-(massi(2)-massi(1))/(deli(2)+deli(1))*deli(1)
      fdel=deli(1)/(deli(1)+deli(2))*(1.+deli(1)/deli(2))
      gdel=deli(1)/(deli(2)+deli(3))
!3    vali(1)=(1.+fdel)*massi(1)-(fdel+gdel)*massi(2)+gdel*massi(3)
      do k=1,leni
        ff(k)=24.*massi(k)
      enddo
      ff(1)=ff(1)-11.*vali(1)
      ff(2)=ff(2)+deli(2)/deli(1)*vali(1)
!
      call quad_diag_solver(leni,aa,bb,cc,dd,ff,vali(2:leni+1))
!
      return
      end
