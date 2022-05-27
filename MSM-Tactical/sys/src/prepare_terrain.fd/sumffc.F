      subroutine sumffc(fln,ap,qln,igrd12s,lnwavs,iwav1s,jwav1,igrd1s,n)
!
! sum coefficient (fln) of cosine function in y direction into ap
! before doing fft in x direction
!
      dimension ap(igrd12s,n), qln(jwav1), fln(lnwavs,n)
!
      do 2 k=1,n
!
      do i=1,iwav1s
        sev = qln(1) * fln((i-1)*jwav1+1,k)
        sod = 0.0
!
        jplus0 = (i-1)*jwav1
        do j=2,jwav1,2
          sod = sod + qln(j) * fln(j+jplus0,k)
          sev = sev + qln(j+1) * fln(j+1+jplus0,k)
        enddo
!
        ap(i ,k) = sev + sod
        ap(i+igrd1s,k) = sev - sod
      enddo
!
    2 continue
      return
      end
