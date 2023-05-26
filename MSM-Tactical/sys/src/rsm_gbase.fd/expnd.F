      subroutine expnd(origin,orislm,indsol,expand,imask,imax,jmax)
!
! written by shyh chen		ucsd,epcp,noaa
!
! input
!   origin  original value (imax,jmax)
!   orislm  original sea land mask (imax,jmax)
!   indsol  index for sea:0 or land:1
!   imask   working array for sea or land
!   imax    i dimension
!   jmax    j dimension
! output
!   expand  expanded value (imax,jmax)
!
      dimension orislm(imax,jmax),origin(imax,jmax),expand(imax,jmax)
      dimension temp(imax,jmax)
      dimension imask(imax,jmax),jmask(imax,jmax)
!
      do j=1,jmax
      do i=1,imax
         imask(i,j)=nint(orislm(i,j))
         if(imask(i,j).ge.2)imask(i,j)=0      ! get only land=1, or ocean=0
         expand(i,j)=origin(i,j)
         temp(i,j)=origin(i,j)
      enddo
      enddo
!
   25 continue
      ntot=0
      jmask=0
      do j=1,jmax
      do i=1,imax
       if(imask(i,j).ne.indsol)then    ! not over indsol
        ip1=min(i+1,imax)
        im1=max(i-1,1)
        jp1=min(j+1,jmax)
        jm1=max(j-1,1)
        tsum=0.
        np=0
        call acsum(imask(ip1,j),temp(ip1,j),tsum,indsol,np)
        call acsum(imask(im1,j),temp(im1,j),tsum,indsol,np)
        call acsum(imask(i,jp1),temp(i,jp1),tsum,indsol,np)
        call acsum(imask(i,jp1),temp(i,jp1),tsum,indsol,np)
        if(np.ge.1)then
          expand(i,j)=tsum/np
          jmask(i,j)=1      
          ntot=ntot+1
        endif
       endif
      enddo
      enddo
      if(ntot.ge.1) then
        temp=expand
        imask=imask+(indsol-imask)*jmask
        go to 25
      endif
      return
      end

      subroutine acsum(imask,origin,tsum,indsol,np)
      if(imask.eq.indsol)then
        tsum=tsum+origin
        np=np+1
      endif
      return
      end
