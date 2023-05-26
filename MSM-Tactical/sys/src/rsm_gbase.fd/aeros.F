      subroutine aeros(ib,kprf,idxc,cmix,denn,rh                        &
     &,                tau,ssa,asy                                      &
     &,                l, imax, nae, nbd, ndm, nxc, ndn                 &
     &,                haer, idm, dz, hz)
!fpp$ noconcur r
!********************************************************************
!  compute aerosols optical properties in eight uv+vis bands and
!  four nir bands. there are seven different vertical profile
!  structures. in the troposphere, aerosol distribution at each
!  grid point is composed from up to six components out of a total
!  of ten different substances.
!  ref: wmo report wcp-112 (1986)
!
!  1999-10-13 y.h. updated to opac data (1998)
!
!  band: 1. 0.175-0.225 (uv-c)     2. 0.225-0.245;0.260-0.280 (uv-c)
!        3. 0.245-0.260 (uv-c)     4. 0.280-0.295 (uv-b)
!        5. 0.295-0.310 (uv-b)     6. 0.310-0.320 (uv-b)
!        7. 0.320-0.400 (uv-a)     8. 0.400-0.700 (par)
!        9. 2.27 - 4.0  (nir)     10. 1.22 - 2.27 (nir)
!       11. 0.70 - 1.22 (nir)     12. 0.70 - 4.0  (nir)
!
!  input variables:
!     ib   - spectral band index                   -     1
!     kaer - =0 do not compute aerosols            -     1
!            =1 compute aerosol profiles
!     kprf - indecies of aerosol prof structures   -     imax
!     idxc - indecies of aerosol components        -     nxc*imax
!     cmix - mixing ratioes of aerosol components  -     nxc*imax
!     denn - aerosol number densities              -     ndn*imax
!     rh   - relative humidity                     -     imax*l
!
!  output variables:
!     tau  - optical depth                         -     imax*l
!     ssa  - single scattering albedo              -     imax*l
!     asy  - asymmetry parameter                   -     imax*l
!     taurs- rayleigh scattering optical depth     -     imax*l
!
!  varialbes in common block:
!     haer - scale height of aerosols              km    ndm*nae
!     idm  - aerosol domain index                  -     l*nae
!     dz   - layer thickness                       km    l
!     hz   - level height                          km    l+1
!     taur - rayleigh scattering optical depth     -     l*nbd
!********************************************************************
!
      use machine , only : kind_rad
      implicit none
!
      integer ncm1, ncm2, ncm, ncf, nbdd
      parameter (ncm1=6, ncm2=4, ncm=ncm1+ncm2, ncf=3, nbdd=12)
!
! --- input
      integer l, imax, nae, nbd, ndm, nxc, ndn
      integer  idxc(nxc,imax), kprf(imax), idm(l,nae)
!
      real (kind=kind_rad) cmix(nxc,imax), denn(ndn,imax), rh(imax,l)
! --- output
      real (kind=kind_rad)                                              &
     &  tau(imax,l),  ssa(imax,l),  asy(imax,l)
! --- aerosol data
      real (kind=kind_rad)                                              &
     &  ext0(ncm1,nbdd),    sca0(ncm1,nbdd),    ssa0(ncm1,nbdd)         &
     &, aext(ncf,ncm2,nbdd),bsca(ncf,ncm2,nbdd),asf0(ncm1,nbdd)         &
     &, cssa(ncf,ncm2,nbdd),dasf(ncf,ncm2,nbdd),abpw(ncm2,nbdd)         &
     &, estr(nbdd)
!
      real (kind=kind_rad) haer(ndm,nae),  dz(imax,l),  hz(imax,l+1)
!
      real (kind=kind_rad) crt1, crt2
      data  crt1,crt2 / 30.0, 0.03333 /
      save crt1, crt2                                                   &
     &,    ext0,sca0,ssa0,aext,bsca,asf0,cssa,dasf,abpw,estr
!
      integer i, j, k, idom, kpf, icmp, ib, ic, ic1
!
! --- extinction coeffs of 6 none rh dep compnts in 12 bands
!         inso      soot      minm      miam      micm      mitr
      data  ext0                                                        &
     & /8.052e-03,1.356e-06,1.032e-04,2.821e-03,7.597e-02,5.445e-03,    &
     &  8.076e-03,1.313e-06,1.023e-04,2.835e-03,7.608e-02,5.465e-03,    &
     &  8.060e-03,1.342e-06,1.029e-04,2.826e-03,7.601e-02,5.452e-03,    &
     &  8.112e-03,1.248e-06,1.010e-04,2.857e-03,7.625e-02,5.496e-03,    &
     &  8.148e-03,1.173e-06,9.940e-05,2.879e-03,7.641e-02,5.527e-03,    &
     &  8.156e-03,1.154e-06,9.895e-05,2.884e-03,7.645e-02,5.534e-03,    &
     &  8.282e-03,8.612e-07,9.008e-05,2.969e-03,7.703e-02,5.650e-03,    &
     &  8.524e-03,5.432e-07,6.925e-05,3.140e-03,7.811e-02,5.876e-03,    &
     &  6.435e-03,7.664e-08,3.429e-06,2.452e-03,9.026e-02,6.150e-03,    &
     &  9.062e-03,1.471e-07,1.413e-05,3.365e-03,8.368e-02,6.691e-03,    &
     &  9.021e-03,2.626e-07,3.506e-05,3.411e-03,8.040e-02,6.348e-03,    &
     &  8.823e-03,2.132e-07,2.628e-05,3.325e-03,8.214e-02,6.442e-03/
!
! --- scattering coeffs of 6 none rh dep compnts in 12 bands
!         inso      soot      minm      miam      micm      mitr
      data  sca0                                                        &
     & /4.447e-03,4.177e-07,8.264e-05,1.625e-03,4.142e-02,3.034e-03,    &
     &  4.723e-03,4.061e-07,8.314e-05,1.662e-03,4.150e-02,3.080e-03,    &
     &  4.539e-03,4.138e-07,8.281e-05,1.637e-03,4.145e-02,3.049e-03,    &
     &  5.136e-03,3.887e-07,8.389e-05,1.718e-03,4.162e-02,3.149e-03,    &
     &  5.404e-03,3.623e-07,8.453e-05,1.795e-03,4.184e-02,3.252e-03,    &
     &  5.423e-03,3.540e-07,8.464e-05,1.817e-03,4.192e-02,3.285e-03,    &
     &  5.729e-03,2.301e-07,8.277e-05,2.178e-03,4.367e-02,3.840e-03,    &
     &  6.255e-03,1.129e-07,6.675e-05,2.750e-03,5.242e-02,4.909e-03,    &
     &  5.553e-03,7.201e-10,2.423e-06,2.140e-03,6.357e-02,5.264e-03,    &
     &  7.757e-03,6.464e-09,1.348e-05,3.178e-03,6.539e-02,6.172e-03,    &
     &  7.229e-03,2.859e-08,3.417e-05,3.205e-03,6.186e-02,5.805e-03,    &
     &  7.253e-03,1.972e-08,2.546e-05,3.119e-03,6.317e-02,5.886e-03/
!
! --- sing sctr albedoes of 6 none rh dep compnts in 12 bands
!         inso      soot      minm      miam      micm      mitr
      data  ssa0                                                        &
     & /5.524e-01,3.081e-01,8.004e-01,5.760e-01,5.452e-01,5.571e-01,    &
     &  5.846e-01,3.095e-01,8.125e-01,5.860e-01,5.455e-01,5.634e-01,    &
     &  5.631e-01,3.086e-01,8.044e-01,5.793e-01,5.453e-01,5.592e-01,    &
     &  6.329e-01,3.116e-01,8.307e-01,6.011e-01,5.459e-01,5.728e-01,    &
     &  6.631e-01,3.084e-01,8.509e-01,6.230e-01,5.476e-01,5.884e-01,    &
     &  6.647e-01,3.061e-01,8.560e-01,6.298e-01,5.483e-01,5.935e-01,    &
     &  6.918e-01,2.672e-01,9.189e-01,7.336e-01,5.670e-01,6.797e-01,    &
     &  7.336e-01,2.025e-01,9.651e-01,8.748e-01,6.708e-01,8.345e-01,    &
     &  8.612e-01,8.972e-03,7.039e-01,8.739e-01,7.035e-01,8.567e-01,    &
     &  8.565e-01,4.103e-02,9.460e-01,9.438e-01,7.816e-01,9.224e-01,    &
     &  8.009e-01,1.024e-01,9.733e-01,9.395e-01,7.693e-01,9.142e-01,    &
     &  8.228e-01,7.671e-02,9.463e-01,9.367e-01,7.693e-01,9.133e-01/
!
! --- asymmetry factors of 6 none rh dep compnts in 12 bands
!         inso      soot      minm      miam      micm      mitr
      data  asf0                                                        &
     & /9.390e-01,5.020e-01,7.320e-01,9.030e-01,9.460e-01,9.310e-01,    &
     &  9.219e-01,4.873e-01,7.272e-01,8.946e-01,9.469e-01,9.256e-01,    &
     &  9.333e-01,4.971e-01,7.304e-01,9.002e-01,9.463e-01,9.292e-01,    &
     &  8.963e-01,4.653e-01,7.200e-01,8.820e-01,9.482e-01,9.175e-01,    &
     &  8.798e-01,4.468e-01,7.126e-01,8.668e-01,9.484e-01,9.064e-01,    &
     &  8.787e-01,4.437e-01,7.109e-01,8.627e-01,9.481e-01,9.031e-01,    &
     &  8.600e-01,3.960e-01,6.880e-01,8.030e-01,9.400e-01,8.500e-01,    &
     &  8.280e-01,3.306e-01,6.616e-01,7.342e-01,8.923e-01,7.742e-01,    &
     &  8.989e-01,7.289e-02,3.909e-01,6.921e-01,8.067e-01,7.122e-01,    &
     &  8.136e-01,1.494e-01,5.319e-01,6.878e-01,8.103e-01,6.977e-01,    &
     &  7.864e-01,2.302e-01,6.087e-01,6.942e-01,8.406e-01,7.178e-01,    &
     &  8.045e-01,1.937e-01,5.688e-01,6.920e-01,8.282e-01,7.110e-01/
!
! --- fitting coeffs of ext of 4 rh dep compnts in 12 bands
!               waso/sscm                      ssam/suso
      data (((aext(i,j,k),i=1,ncf),j=1,ncm2),k=1,6)                      &
     & /1.595e-05,1.330e-05,4.237e-10, 2.304e-03,2.784e-03,1.272e-09,    &
     &  1.448e-01,1.770e-01,9.004e-08, 2.309e-04,2.389e-04,7.845e-10,    &
     &  1.528e-05,1.268e-05,4.174e-10, 2.320e-03,2.798e-03,1.275e-09,    &
     &  1.451e-01,1.773e-01,9.000e-08, 2.302e-04,2.409e-04,7.955e-10,    &
     &  1.573e-05,1.310e-05,4.216e-10, 2.309e-03,2.789e-03,1.273e-09,    &
     &  1.449e-01,1.771e-01,9.003e-08, 2.307e-04,2.396e-04,7.882e-10,    &
     &  1.426e-05,1.176e-05,4.079e-10, 2.344e-03,2.820e-03,1.279e-09,    &
     &  1.455e-01,1.778e-01,8.994e-08, 2.292e-04,2.440e-04,8.120e-10,    &
     &  1.331e-05,1.090e-05,3.976e-10, 2.368e-03,2.842e-03,1.282e-09,    &
     &  1.458e-01,1.782e-01,8.997e-08, 2.272e-04,2.461e-04,8.286e-10,    &
     &  1.312e-05,1.073e-05,3.951e-10, 2.374e-03,2.848e-03,1.283e-09,    &
     &  1.459e-01,1.782e-01,9.000e-08, 2.265e-04,2.462e-04,8.322e-10/
      data (((aext(i,j,k),i=1,ncf),j=1,ncm2),k=7,nbdd)                   &
     & /1.011e-05,8.090e-06,3.512e-10, 2.468e-03,2.964e-03,1.291e-09,    &
     &  1.470e-01,1.805e-01,8.984e-08, 2.115e-04,2.436e-04,8.894e-10,    &
     &  6.646e-06,5.898e-06,6.175e-11, 2.609e-03,3.191e-03,1.322e-09,    &
     &  1.485e-01,1.805e-01,9.088e-08, 1.718e-04,2.079e-04,9.422e-10,    &
     &  4.815e-07,8.023e-07,3.772e-12, 1.482e-03,2.153e-03,1.589e-09,    &
     &  1.673e-01,1.966e-01,9.462e-08, 2.483e-05,3.554e-05,1.950e-11,    &
     &  9.357e-07,7.900e-07,5.441e-12, 2.246e-03,3.217e-03,1.599e-09,    &
     &  1.580e-01,1.893e-01,9.297e-08, 4.494e-05,6.263e-05,3.156e-11,    &
     &  2.732e-06,2.527e-06,1.241e-11, 2.666e-03,3.547e-03,1.433e-09,    &
     &  1.524e-01,1.844e-01,9.179e-08, 9.999e-05,1.327e-04,1.971e-10,    &
     &  2.007e-06,1.856e-06,9.589e-12, 2.439e-03,3.323e-03,1.496e-09,    &
     &  1.553e-01,1.868e-01,9.237e-08, 7.643e-05,1.000e-04,1.695e-10/
!
! --- fitting coeffs of sca of 4 rh dep compnts in 12 bands
!               waso/sscm                      ssam/suso
      data (((bsca(i,j,k),i=1,ncf),j=1,ncm2),k=1,6)                      &
     & /1.431e-05,1.314e-05,4.229e-10, 2.303e-03,2.784e-03,1.272e-09,    &
     &  1.448e-01,1.771e-01,8.997e-08, 2.309e-04,2.389e-04,7.845e-10,    &
     &  1.400e-05,1.257e-05,4.168e-10, 2.319e-03,2.798e-03,1.275e-09,    &
     &  1.451e-01,1.774e-01,8.996e-08, 2.302e-04,2.409e-04,7.955e-10,    &
     &  1.421e-05,1.295e-05,4.209e-10, 2.309e-03,2.789e-03,1.273e-09,    &
     &  1.449e-01,1.772e-01,8.997e-08, 2.307e-04,2.396e-04,7.882e-10,    &
     &  1.355e-05,1.172e-05,4.077e-10, 2.344e-03,2.820e-03,1.279e-09,    &
     &  1.455e-01,1.779e-01,8.993e-08, 2.292e-04,2.440e-04,8.120e-10,    &
     &  1.294e-05,1.091e-05,3.976e-10, 2.368e-03,2.843e-03,1.282e-09,    &
     &  1.458e-01,1.782e-01,8.997e-08, 2.272e-04,2.461e-04,8.286e-10,    &
     &  1.276e-05,1.074e-05,3.951e-10, 2.374e-03,2.848e-03,1.283e-09,    &
     &  1.459e-01,1.782e-01,9.000e-08, 2.265e-04,2.462e-04,8.322e-10/
      data (((bsca(i,j,k),i=1,ncf),j=1,ncm2),k=7,nbdd)                   &
     & /9.919e-06,8.093e-06,3.512e-10, 2.468e-03,2.964e-03,1.291e-09,    &
     &  1.470e-01,1.805e-01,8.984e-08, 2.115e-04,2.436e-04,8.894e-10,    &
     &  6.505e-06,5.900e-06,6.173e-11, 2.609e-03,3.191e-03,1.322e-09,    &
     &  1.485e-01,1.805e-01,9.088e-08, 1.718e-04,2.079e-04,9.422e-10,    &
     &  1.224e-07,1.088e-07,1.425e-12, 1.088e-03,1.420e-03,1.191e-09,    &
     &  1.223e-01,1.253e-01,6.140e-08, 9.093e-06,1.152e-05,1.221e-11,    &
     &  8.152e-07,7.708e-07,5.416e-12, 2.240e-03,3.214e-03,1.588e-09,    &
     &  1.542e-01,1.867e-01,8.824e-08, 4.481e-05,6.249e-05,3.148e-11,    &
     &  2.570e-06,2.513e-06,1.240e-11, 2.665e-03,3.547e-03,1.432e-09,    &
     &  1.518e-01,1.844e-01,9.171e-08, 9.999e-05,1.327e-04,1.971e-10,    &
     &  1.848e-06,1.798e-06,9.430e-12, 2.411e-03,3.277e-03,1.467e-09,    &
     &  1.507e-01,1.814e-01,8.835e-08, 7.545e-05,9.867e-05,1.674e-10/
!
! --- fitting coeffs of ssa of 4 rh dep compnts in 12 bands
!               waso/sscm                      ssam/suso
      data (((cssa(i,j,k),i=1,ncf),j=1,ncm2),k=1,6)                      &
     & /8.820e-01,1.329e-01,8.925e-02, 9.999e-01,2.130e-04,4.523e-05,    &
     &  9.994e-01,1.319e-03,-8.368e-4, 1.000e+00,0.000e+00,0.000e+00,    &
     &  9.071e-01,1.059e-01,6.894e-02, 9.999e-01,1.776e-04,-2.168e-5,    &
     &  9.995e-01,1.062e-03,-6.559e-4, 1.000e+00,0.000e+00,0.000e+00,    &
     &  8.904e-01,1.239e-01,8.248e-02, 9.999e-01,2.012e-04,2.292e-05,    &
     &  9.994e-01,1.233e-03,-7.760e-4, 1.000e+00,0.000e+00,0.000e+00,    &
     &  9.449e-01,6.534e-02,3.848e-02, 1.000e+00,1.244e-04,-1.202e-4,    &
     &  9.997e-01,6.763e-04,-3.884e-4, 1.000e+00,0.000e+00,0.000e+00,    &
     &  9.684e-01,3.971e-02,1.988e-02, 1.000e+00,7.586e-05,-1.400e-4,    &
     &  9.998e-01,3.888e-04,-2.249e-4, 1.000e+00,0.000e+00,0.000e+00,    &
     &  9.697e-01,3.814e-02,1.904e-02, 1.000e+00,6.654e-05,-1.230e-4,    &
     &  9.999e-01,3.521e-04,-2.188e-4, 1.000e+00,0.000e+00,0.000e+00/
      data (((cssa(i,j,k),i=1,ncf),j=1,ncm2),k=7,nbdd)                   &
     & /9.790e-01,2.712e-02,1.340e-02, 1.000e+00,0.000e+00,0.000e+00,    &
     &  1.000e+00,0.000e+00,0.000e+00, 1.000e+00,0.000e+00,0.000e+00,    &
     &  9.742e-01,3.376e-02,1.754e-02, 1.000e+00,0.000e+00,0.000e+00,    &
     &  1.000e+00,0.000e+00,0.000e+00, 1.000e+00,0.000e+00,0.000e+00,    &
     &  4.741e-01,-2.284e-2,4.737e-01, 7.713e-01,-2.185e-1,3.434e-01,    &
     &  7.285e-01,-2.238e-1,2.247e-01, 4.767e-01,2.866e-01,2.205e-01,    &
     &  8.438e-01,1.973e-01,1.269e-01, 9.962e-01,5.396e-04,-7.588e-3,    &
     &  9.782e-01,-7.483e-3,-5.631e-2, 9.936e-01,4.980e-03,6.300e-04,    &
     &  9.198e-01,1.049e-01,6.285e-02, 9.995e-01,1.023e-03,-7.783e-4,    &
     &  9.960e-01,7.559e-03,-4.647e-3, 1.000e+00,-1.103e-7,-1.787e-6,    &
     &  8.678e-01,1.263e-01,1.084e-01, 9.842e-01,-1.268e-2,1.762e-02,    &
     &  9.728e-01,-1.375e-2,-1.121e-2, 9.651e-01,1.992e-02,1.374e-02/
!
! --- fitting coeffs of asf of 4 rh dep compnts in 12 bands
!               waso/sscm                      ssam/suso
      data (((dasf(i,j,k),i=1,ncf),j=1,ncm2),k=1,6)                      &
     & /7.240e-01,8.256e-02,3.256e-02, 7.758e-01,1.140e-01,1.584e-02,    &
     &  8.495e-01,2.280e-02,-1.158e-1, 7.431e-01,7.601e-02,-1.690e-2,    &
     &  7.178e-01,9.182e-02,3.936e-02, 7.739e-01,1.174e-01,1.304e-02,    &
     &  8.500e-01,2.590e-02,-1.115e-1, 7.470e-01,7.510e-02,-2.346e-2,    &
     &  7.220e-01,8.564e-02,3.483e-02, 7.752e-01,1.151e-01,1.491e-02,    &
     &  8.497e-01,2.383e-02,-1.144e-1, 7.444e-01,7.571e-02,-1.909e-2,    &
     &  7.085e-01,1.057e-01,4.957e-02, 7.710e-01,1.225e-01,8.826e-03,    &
     &  8.507e-01,3.054e-02,-1.052e-1, 7.527e-01,7.373e-02,-3.331e-2,    &
     &  7.013e-01,1.156e-01,5.817e-02, 7.681e-01,1.260e-01,7.093e-03,    &
     &  8.505e-01,3.608e-02,-9.727e-2, 7.574e-01,7.191e-02,-3.971e-2,    &
     &  7.003e-01,1.167e-01,5.963e-02, 7.674e-01,1.263e-01,7.395e-03,    &
     &  8.502e-01,3.756e-02,-9.510e-2, 7.581e-01,7.138e-02,-4.017e-2/
      data (((dasf(i,j,k),i=1,ncf),j=1,ncm2),k=7,nbdd)                   &
     & /6.851e-01,1.307e-01,8.454e-02, 7.614e-01,1.239e-01,4.612e-03,    &
     &  8.486e-01,5.475e-02,-6.403e-2, 7.681e-01,6.680e-02,-4.495e-2,    &
     &  6.554e-01,1.454e-01,1.182e-01, 7.606e-01,1.143e-01,-2.758e-2,    &
     &  8.424e-01,6.848e-02,-4.633e-2, 7.632e-01,8.161e-02,-2.990e-2,    &
     &  3.633e-01,1.483e-01,2.637e-01, 7.303e-01,1.911e-01,8.386e-03,    &
     &  8.526e-01,1.819e-01,-1.123e-1, 5.014e-01,2.269e-01,2.352e-01,    &
     &  5.068e-01,1.733e-01,2.345e-01, 7.797e-01,1.068e-01,-9.434e-2,    &
     &  8.157e-01,1.042e-01,4.413e-03, 6.629e-01,1.697e-01,1.018e-01,    &
     &  5.899e-01,1.605e-01,1.771e-01, 7.728e-01,9.568e-02,-8.258e-2,    &
     &  8.304e-01,8.425e-02,-3.121e-2, 7.264e-01,1.228e-01,2.347e-02,    &
     &  5.476e-01,1.640e-01,2.019e-01, 7.725e-01,1.059e-01,-7.896e-2,    &
     &  8.272e-01,9.706e-02,-2.339e-2, 6.906e-01,1.454e-01,6.351e-02/
!
! --- power factor for ext, sca fitting coeffs of 4 rh dep compnts
!        waso  ssam  sscm  suso   waso  ssam  sscm  suso
      data abpw                                                          &
     & / 24.0, 33.0, 33.0, 28.0,  24.0, 33.0, 33.0, 28.0,                &
     &   24.0, 33.0, 33.0, 28.0,  24.0, 33.0, 33.0, 28.0,                &
     &   24.0, 33.0, 33.0, 28.0,  24.0, 33.0, 33.0, 28.0,                &
     &   24.0, 33.0, 33.0, 28.0,  27.0, 33.0, 33.0, 28.0,                &
     &   29.0, 33.0, 33.0, 34.0,  29.0, 33.0, 33.0, 34.0,                &
     &   29.0, 33.0, 33.0, 31.0,  29.0, 33.0, 33.0, 31.0 /
! --- extingction coeffs in stratosphere for 12 bands
      data  estr                                                         &
     & / 3.39e-4, 3.34e-4, 3.38e-4, 3.28e-4, 3.22e-4, 3.18e-4,           &
     &   3.01e-4, 2.09e-4, 1.70e-5, 5.01e-5, 1.03e-4, 7.72e-5 /
!
      real (kind=kind_rad) drh,  ext1, sca1, ssa1, asf1, drh1, drh2      &
     &,                     ex00, sc00, ss00, as00, ssa2, asf2, ext2     &
     &,                     ex01, sc01, ss01, as01                       &
     &,                     ex02, sc02, ss02, as02                       &
     &,                     ex03, sc03, ss03, as03, hd,   sig0u          &
     &,                     sig0l, ratio, hdi, tt
      if (nbd .ne. nbdd) then
         print *,' in aeros nbd =', nbd,' nbdd=',nbdd
         call abort
      endif
      do i=1,imax
!
        kpf = kprf(i)
        do k=1,l
          idom = idm(k,kpf)
          drh = rh(i,k) - 0.5
!
          if (idom .eq. 1) then
! --- 1st doman - mixing layer
            ext1 = 0.0
            sca1 = 0.0
            ssa1 = 0.0
            asf1 = 0.0
            do icmp=1,nxc
              ic = idxc(icmp,i)
              if (ic .gt. ncm1) then
                ic1 = ic - ncm1
                drh1 = exp(abpw(ic1,ib)*drh)
                drh2 = drh * drh
                ex00 = aext(1,ic1,ib) + aext(2,ic1,ib)*drh              &
     &               + aext(3,ic1,ib)*drh1
                sc00 = bsca(1,ic1,ib) + bsca(2,ic1,ib)*drh              &
     &               + bsca(3,ic1,ib)*drh1
                ss00 = cssa(1,ic1,ib) + cssa(2,ic1,ib)*drh              &
     &               + cssa(3,ic1,ib)*drh2
                as00 = dasf(1,ic1,ib) + dasf(2,ic1,ib)*drh              &
     &               + dasf(3,ic1,ib)*drh2
              else if (ic .gt. 0) then
                ex00 = ext0(ic,ib)
                sc00 = sca0(ic,ib)
                ss00 = ssa0(ic,ib)
                as00 = asf0(ic,ib)
              else
                ex00 = 0.0
                sc00 = 0.0
                ss00 = 0.0
                as00 = 0.0
              end if
              ext1 = ext1 + cmix(icmp,i) * ex00
              sca1 = sca1 + cmix(icmp,i) * sc00
              ssa1 = ssa1 + cmix(icmp,i) * ss00 * ex00
              asf1 = asf1 + cmix(icmp,i) * as00 * sc00
            end do
            ext2 = ext1 * denn(1,i)
            ssa2 = ssa1 / ext1
            asf2 = asf1 / sca1
          else if (idom .eq. 2) then
! --- 2nd domain - mineral transport layers
            ext2 = ext0(6,ib) * denn(2,i)
            ssa2 = ssa0(6,ib)
            asf2 = asf0(6,ib)
          else if (idom .eq. 3) then
! --- 3rd domain - free tropospheric layers
!   1:inso 0.17e-3; 2:soot 0.4; 7:waso 0.59983; n:730
            drh1 = exp(abpw(1,ib)*drh)
            drh2 = drh * drh
            ex01 = ext0(1,ib)
            sc01 = sca0(1,ib)
            ss01 = ssa0(1,ib)
            as01 = asf0(1,ib)
            ex02 = ext0(2,ib)
            sc02 = sca0(2,ib)
            ss02 = ssa0(2,ib)
            as02 = asf0(2,ib)
            ex03 = aext(1,1,ib) + aext(2,1,ib)*drh + aext(3,1,ib)*drh1
            sc03 = bsca(1,1,ib) + bsca(2,1,ib)*drh + bsca(3,1,ib)*drh1
            ss03 = cssa(1,1,ib) + cssa(2,1,ib)*drh + cssa(3,1,ib)*drh2
            as03 = dasf(1,1,ib) + dasf(2,1,ib)*drh + dasf(3,1,ib)*drh2
            ext1 = 0.17e-3*ex01 + 0.4*ex02 + 0.59983*ex03
            sca1 = 0.17e-3*sc01 + 0.4*sc02 + 0.59983*sc03
            ssa1 = 0.17e-3*ss01*ex01 + 0.4*ss02*ex02 + 0.59983*ss03*ex03
            asf1 = 0.17e-3*as01*sc01 + 0.4*as02*sc02 + 0.59983*as03*sc03
            ext2 = ext1 * 730.0
            ssa2 = ssa1 / ext1
            asf2 = asf1 / sca1
          else if (idom .eq. 4) then
! --- 4th domain - stratospheric layers
            ext2 = estr(ib)
            ssa2 = 0.9
            asf2 = 0.6
          else
! --- upper stratosphere assume no aerosol
            ext2 = 0.0
            ssa2 = 1.0
            asf2 = 0.0
          end if
!
          hd = haer(idom,kpf)
          if (hd .gt. 0.0e0) then
            hdi      = 1.0 / hd
            sig0u    = exp(-hz(i,k)  *hdi)
            sig0l    = exp(-hz(i,k+1)*hdi)
            tau(i,k) = ext2 * hd*(sig0l - sig0u)
          else
            tau(i,k) = ext2 * dz(i,k)
!           tau(i,k) = (ext2-hd*hh(k)*alog(0.5*(sig0u+sig0l)))*dz(k)
          end if
          ssa(i,k) = ssa2
          asy(i,k) = asf2
!         write(6,112) ib,k,i,idom,hd,hh(k),drh,dz(k),sig0u,sig0l,      &
!    &                 denn(1,i),denn(2,i),ext2,tau(i,k),ssa2,asf2
!112      format(1x,'ib,k,i=',3i3,' idom,hd,hh,drh=',i2,3f5.2,          &
!    &           ' dz,sig0u,sig0l=',3f6.3,/' denn=',2f8.2,              &
!    &           ' ext2,tau,ssa,asf=',4f6.3)
        end do
      end do
!
!===> ... smooth profile at domain boundaries
!
      do k=2,l
        do i=1,imax
          ratio = 1.0
          if (tau(i,k) .gt. 0.0) ratio = tau(i,k-1) / tau(i,k)
          tt = tau(i,k) + tau(i,k-1)
          if (ratio .gt. crt1) then
            tau(i,k) = 0.2 * tt
            tau(i,k-1) = tt - tau(i,k)
          else if (ratio .lt. crt2) then
            tau(i,k) = 0.8 * tt
            tau(i,k-1) = tt - tau(i,k)
          end if
        enddo
      enddo
!
      return
      end
