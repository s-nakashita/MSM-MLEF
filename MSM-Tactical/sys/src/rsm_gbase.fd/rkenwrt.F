      subroutine rkenwrt(nrkenp,label,fhour,idate,si,sl)
      use machine , only : kind_io4
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#endif
#include <rscomgpd.h>
!cc
      character*8 label(4)
      dimension idate(4),si(levr+1),sl(levr)
      real(kind=kind_io4) fhours,sis(levr+1),sls(levr)                    &
     &           ,svdatas(nvrken,nptken,nstken)                           &
     &           ,gpdlats(nptken),gpdlons(nptken),gpdgzss(nptken)         &
     &           ,gpmlats(nptken),gpmlons(nptken),gpmgzss(nptken)
!..................................................................
#ifdef MP
        if(mype.eq.master) then
#endif
        print*,' enter rkenwrt !!'
        print 1047,itnum,npoint
 1047  format(1h0,i6,' steps of ken(campana) gridpt data saved for ',    & 
     &            i5,' points')
#ifdef MP
        endif
#endif
!          in this case, to get the radiation,clds into ken pts,
!
        do j=1,npoint
         do k=2,itnum
!...      if olr.le zero,then radiation fields have not been filled
!           for this timestep, so carry the previous data forward
          if (svdata(44,j,k).le.0.) then      !tsear<0
           do i=25,27
            svdata(i,j,k) = svdata(i,j,k-1)
           enddo
           svdata(36,j,k) = svdata(36,j,k-1)
           do i=41,49
            svdata(i,j,k) = svdata(i,j,k-1)
           enddo
           svdata(61,j,k) = svdata(61,j,k-1)
          endif
          if (svdata(50,j,k).le.0.) then      !longwave upward flux at top < 0, down +
            svdata(47,j,k) = svdata(47,j,k-1)
            svdata(50,j,k) = svdata(50,j,k-1)
            svdata(59,j,k) = svdata(59,j,k-1)
            svdata(60,j,k) = svdata(60,j,k-1)
           endif
         enddo
        enddo
!
        fhours=fhour
        call ful2sgl(si,sis,levr+1)
        call ful2sgl(sl,sls,levr)
        call ful2sgl(gpdlat,gpdlats,nptken)
        call ful2sgl(gpdlon,gpdlons,nptken)
        call ful2sgl(gpdgzs,gpdgzss,nptken)
        call ful2sgl(gpmlat,gpmlats,nptken)
        call ful2sgl(gpmlon,gpmlons,nptken)
        call ful2sgl(gpmgzs,gpmgzss,nptken)
#ifdef MP
        if(mype.eq.master) then
#endif
        write(nrkenp) label
        write(nrkenp) fhours,idate,sis,sls
        write(nrkenp) nvrken,nptken,nstken,npoint,itnum
        write(nrkenp) (gpdnam(n),                                        &  
     &                 gpdlats(n),gpdlons(n),gpdgzss(n),                 &
     &                 gpmlats(n),gpmlons(n),gpmgzss(n),n=1,npoint)
#ifdef MP
        endif
#endif
        call ful2sgl(svdata,svdatas,nvrken*nptken*nstken)
        do j=1,npoint                          !precipitation unit from meter->mm
         do k=1,itnum
           do i=11,12
            svdatas(i,j,k) = svdatas(i,j,k)*1000.0
           enddo
          enddo
        enddo
#ifdef MP
        if(mype.eq.master) then
#endif
        do j=1,npoint
         write(nrkenp) ((svdatas(i,j,k),k=1,itnum),i=1,nvrken)
        enddo
        rewind nrkenp
#ifdef MP
        endif
#endif
!
      return
      end
