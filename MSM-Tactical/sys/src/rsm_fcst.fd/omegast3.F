      subroutine omegast3(im,ix,nz,                                      &   
     &      dphi,dlam,ug,vg,dg,del,rcl,vvel,ps,sl)
!fpp$ noconcur r
!....   code lifted from post (mcp1840) jun 88--computes vvel (cb/sec)
!....    input ps in cb,output vvel in cb/sec
!....   do loops altered for better vectorization possibilities..k.a.c.
!c
      use machine , only : kind_phys
      implicit none
!!
      integer              i,k,le,nz,ix,im
      real(kind=kind_phys) rcl
!c
      real(kind=kind_phys) dphi(ix),dlam(ix),ps(ix)
      real(kind=kind_phys)                                               &  
     & cg(ix,nz),ug(ix,nz),vg(ix,nz),                                    &
     & dg(ix,nz),del(nz),sl(nz)
!...   vvel contains omega in layers on return from subroutine...
      real(kind=kind_phys) vvel(ix,nz)
      real(kind=kind_phys) db(ix,nz),cb(ix,nz), dot(ix,nz+1)
!!
      do 1 k=1,nz+1
        do 49 i=1,im
          dot(i,k) = 0.e0
   49 continue
    1 continue
!...  compute c=v(true)*del(ln(ps)).divide by cos for del cos for v
      do 48 i=1,im
        dphi(i)=dphi(i)*rcl
        dlam(i)=dlam(i)*rcl
   48 continue
      do 5 le=1,nz
        do 50 i=1,im
          cg(i,le)=ug(i,le)*dlam(i)+vg(i,le)*dphi(i)
   50 continue
    5 continue
      do 51 i=1,im
        db(i,1)=del(1)*dg(i,1)
        cb(i,1)=del(1)*cg(i,1)
   51 continue
!!
      do 6 le=1,nz-1
        do 52 i=1,im
          db(i,le+1)=db(i,le)+del(le+1)*dg(i,le+1)
          cb(i,le+1)=cb(i,le)+del(le+1)*cg(i,le+1)
   52 continue
    6 continue
!!
!...    sigma dot computed only at interior interfaces
      do 7 k=1,nz-1
        do 53 i=1,im
          dot(i,k+1)=dot(i,k)+del(k)                                     &
     &               *(db(i,nz)+cb(i,nz)-dg(i,k)-cg(i,k))
   53 continue
    7 continue
!!
      do 8 k=1,nz
        do 54 i=1,im
          vvel(i,k)=  sl(k)*(cg(i,k)-cb(i,nz)-db(i,nz))-                 &     
     &                0.5*(dot(i,k+1)+dot(i,k))
          vvel(i,k)=vvel(i,k)*ps(i)
   54 continue
    8 continue
!!
      return
      end
