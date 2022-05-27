      subroutine sumffc(fln,ap,qln,                                      &                         
     &           inclat,igrd12s,lnwavs,iwav1s,jwav1,igrd1s,n)
!fpp$ noconcur r
!
! sum coefficient (fln) of cosine function in y direction into ap
! before doing fft in x direction
!
      dimension ap(igrd12s,inclat,n), qln(jwav1,inclat), fln(lnwavs,n)
!c
#ifdef OPENMP
!$omp parallel
#endif
!
      do 2 k=1,n
!
      do l=1,inclat
      do i=1,iwav1s
        sev = qln(1,l) * fln((i-1)*jwav1+1,k)
        sod = 0.0
!c
        jplus0 = (i-1)*jwav1
        do j=2,jwav1,2
          sod = sod + qln(j,l) * fln(j+jplus0,k)
          sev = sev + qln(j+1,l) * fln(j+1+jplus0,k)
        enddo
!
        ap(i ,l,k) = sev + sod
        ap(i+igrd1s,l,k) = sev - sod
      enddo
      enddo
!
    2 continue
      return
      end
