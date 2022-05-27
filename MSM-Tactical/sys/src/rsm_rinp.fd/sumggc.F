      subroutine sumggc(ap,fln,qln,                                      &                         
     &                  igrd12s,lnwavs,iwav1s,jwav1,igrd1s,n)
!
! sum coefficient of cosine function at ap in y direction
! into fln after fft
!
      dimension ap(igrd12s,n), qln(jwav1), fln(lnwavs,n)
!
!
#ifdef ORIGIN_THREAD
!$doacross local(sev,sod,i,j,k,jplus0)
#endif
#ifdef CRAY_THREAD
!mic$ do all private(sev,sod) autoscope
#endif
#ifdef OPENMP
!$omp parallel do private(sev,sod,i,j,k,jplus0)
#endif
!
      do 2 k=1,n
!
      do i=1,iwav1s
        jplus0 = (i-1)*jwav1
        sev = ap(i,k) + ap(i+igrd1s,k)
        sod = ap(i,k) - ap(i+igrd1s,k)
        fln(jplus0 + 1,k) = fln(jplus0 + 1,k) + qln(1) * sev
        do j=2,jwav1,2
          fln(j+jplus0,k) = fln(j+jplus0,k) + qln(j) * sod
          fln(j+1+jplus0,k) = fln(j+1+jplus0,k) + qln(j+1) * sev
        enddo
      enddo
!c
!
    2 continue
      return
      end
