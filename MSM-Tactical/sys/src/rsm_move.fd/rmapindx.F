      subroutine rmapindx(im,jm,ijm,                                  &
     &                    flato,flono,flatn,flonn,map,imap,fmap)
!
      use machine , only : kind_io4
!
      integer  map(ijm),imap(ijm)
      real(kind=kind_io4) flato(ijm),flono(ijm)
      real(kind=kind_io4) flatn(ijm),flonn(ijm)
      real(kind=kind_io4) olat(jm),olon(im),fmap(ijm,4)
  
!
      print *,' enter subroutine rmapindx '
!
      olatmin=minval(flato)
      olatmax=maxval(flato)
      olonmin=minval(flono)
      olonmax=maxval(flono)
!     print *,' old domain ',olonmin,olonmax,olatmin,olatmax

      do i=1,im
        olon(i)=flono(i)
      enddo
      do j=1,jm
        olat(j)=flato((j-1)*im+1)
      enddo

      do i=1,ijm
         if( flatn(i).ge.olatmin .and. flatn(i).le.olatmax .and.      &
     &       flonn(i).ge.olonmin .and. flonn(i).le.olonmax ) then
           map(i) = 1
           do ii=1,im-1
             if( flonn(i).ge.olon(ii) .and.                           &
     &           flonn(i).le.olon(ii+1) ) then
                 ipick = ii
                 dx=(flonn(i)-olon(ii))/(olon(ii+1)-olon(ii))
                 go to 100
             endif
           enddo
 100       continue
           do jj=1,jm-1
             if( flatn(i).ge.olat(jj) .and.                           &
     &           flatn(i).le.olat(jj+1) ) then
                 jpick = jj
                 dy=(flatn(i)-olat(jj))/(olat(jj+1)-olat(jj))
                 go to 200
             endif
           enddo
 200       continue
           imap(i) = (jpick-1)*im+ipick
           fmap(i,1) = (1.-dx)*(1-dy)
           fmap(i,2) =     dx *(1-dy)
           fmap(1,3) = (1.-dx)*   dy
           fmap(i,4) =     dx *   dy
         else
           map(i) = 0
           imap(i) = 0
           fmap(i,:) = 0.0
         endif
      enddo
! check print
      do j=jm,1,-1
        ij=(j-1)*im
        write(*,111) (map(ij+i),i=1,65)
 111    format(65I1)
      enddo
!
      print *,' end of rmapindx '
!
      return
      end
