      module comrpln
!
      implicit none

      real, allocatable :: ccosg(:,:),csing(:,:)
      real, allocatable :: gcosc(:,:),gsinc(:,:)

      contains


      subroutine rplnini(im,jm,iw,jw)

      implicit none
      integer 	im,jm,iw,jw
!
      double precision    pi, rad, cc, ss, cogr, cowv
      double precision, allocatable :: fc(:),fs(:)
      integer	i,ii,l,ll,j,lat,nl,nf,jl,jl0
      integer	igrd1,jgrd1,jgrd,jgrd12
      integer	iwav1,jwav1

! jgrd1 should be even number.
        igrd1=im
        iwav1=iw
        jgrd1=jm
        jwav1=jw
        allocate ( ccosg(jwav1,jgrd1) )
        allocate ( csing(jwav1,jgrd1) )
        allocate ( gcosc(jwav1,jgrd1) )
        allocate ( gsinc(jwav1,jgrd1) )
        jgrd=jgrd1-1
        allocate( fc(jgrd), fs(jgrd) )
        jgrd12 = jgrd1/2
cc
        pi = asin(1.0 d 0) * 2.0 d 0
        do 10 i=2,jgrd12
           ii = i - 1
           rad = pi * float(ii) / float(jgrd)
           fc(i) = cos(rad)
           fs(i) = sin(rad)
           l = jgrd + 2 - i
           fc(l) = - fc(i)
           fs(l) =   fs(i)
 10     continue
           fc(1) = 1.0 d 0
           fs(1) = 0.0 d 0
c
c
        do 100 lat = 1, jgrd12
          j = lat - 1
c
          cogr=2.0 d 0 / float(jgrd)
          if( lat.eq.1 .or. lat.eq.jgrd1 ) cogr=cogr * 0.5 d 0
c
          do 100 ll=1,jwav1
             cowv=1.0 d 0
             if( ll.eq.1 .or. ll.eq.jgrd1 ) cowv=0.5 d 0
             l = ll - 1
             nl = l * iwav1
c
             jl0 = j * l
             jl = mod(jl0, jgrd)
             nf = mod(jl0/jgrd, 2)
             if( nf .eq. 1 ) then
               cc = -1.0
               ss = -1.0
             else
               cc =  1.0
               ss =  1.0
             endif
             jl = jl + 1
             ccosg(ll,lat) =  fc(jl)*cowv*cc
             csing(ll,lat) =  fs(jl)*cowv*ss
             gcosc(ll,lat) =  fc(jl)*cogr*cc
             gsinc(ll,lat) =  fs(jl)*cogr*ss
c
  100     continue
cc
      end subroutine rplnini

      end module comrpln
