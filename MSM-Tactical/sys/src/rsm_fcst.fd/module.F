      module hcon

      use machine
      implicit none
      save


      real (kind=kind_rad) amolwt, csubp,    diffctr,  g, ginv           &
     &,                     gravdr, o3difctr, p0,       p0inv            &
     &,                     gp0inv, p0xzp2,   p0xzp8,   p0x2             &
     &,                     radcon, radcon1,  ratco2mw, rath2omw         &
     &,                     rgask,  rgassp,   secpda
!
!***the following data are level-independent
      data g/980.665/
!
      real (kind=kind_rad) hundred, hninety, sixty, fifty, ten, eight    &   
     &,                     five,    four,    three, two,   one, haf     &   
     &,                     quartr,  zero
!
      real (kind=kind_rad) h83e26,   h71e26,  h1e15 ,  h1e13,   h1e11    &
     &,                     h1e8,     h2e6,    h1e6,    h69766e5,h4e5    &
     &,                     h165e5,   h5725e4, h488e4,  h1e4,    h24e3   &
     &,                     h20788e3, h2075e3, h18e3,   h1224e3          &
     &,                     h67390e2, h5e2,    h3082e2, h3e2,    h2945e2 &
     &,                     h29316e2, h26e2,   h25e2,   h23e2,   h2e2    &
     &,                     h15e2,    h1386e2, h1036e2, h8121e1, h35e1   &
     &,                     h3116e1,  h28e1,   h181e1,  h18e1,   h161e1  &
     &,                     h16e1,    h1226e1, h9p94,   h6p08108,h3p6    &
     &,                     h3p5,     h2p9,    h2p8,    h2p5,    h1p8    &
     &,                     h1p4387,  h1p41819,h1p4,    h1p25892,h1p082  &
     &,                     hp816,    hp805,   hp8,     hp60241          &
     &,                     hp602409, hp6,     hp526315,hp518,   hp5048  &
     &,                     hp3795,   hp369,   hp26,    hp228,   hp219   &
     &,                     hp166666, hp144,   hp118666,hp1
       real (kind=kind_rad) h658m2,  h625m2,   h44871m2, h44194m2        &
     &,                     h42m2,   h41666m2, h28571m2, h2118m2         &
     &,                     h129m2,  h1m2,     h559m3,   h3m3            &
     &,                     h235m3,  h1m3,     h987m4,   h323m4          &
     &,                     h3m4,    h285m4,   h1m4,     h75826m4        &
     &,                     h6938m5, h394m5,   h37412m5, h15m5           &
     &,                     h1439m5, h128m5,   h102m5,   h1m5            &
     &,                     h7m6,    h4999m6,  h451m6,   h25452m6        &
     &,                     h1m6,    h391m7,   h1174m7,  h8725m8         &
     &,                     h327m8,  h257m8,   h1m8,     h23m10          &
     &,                     h14m10,  h11m10,   h1m10,    h83m11          &
     &,                     h82m11,  h8m11,    h77m11,   h72m11          &
     &,                     h53m11 , h48m11,   h44m11,   h42m11          &
     &,                     h37m11,  h35m11,   h32m11,   h3m11           &
     &,                     h28m11,  h24m11,   h23m11,   h2m11           &
     &,                     h18m11,  h15m11,   h14m11,   h114m11         &
     &,                     h11m11,  h1m11,    h96m12,   h93m12          &
     &,                     h77m12,  h74m12,   h65m12,   h62m12          &
     &,                     h6m12,   h45m12,   h44m12,   h4m12           &
     &,                     h38m12,  h37m12,   h3m12,    h29m12          &
     &,                     h28m12,  h24m12,   h21m12,   h16m12          &
     &,                     h14m12,  h12m12,   h8m13,    h46m13          &
     &,                     h36m13,  h135m13,  h12m13,   h1m13           &
     &,                     h3m14,   h15m14,   h14m14,   h101m16         &
     &,                     h1m16,   h1m17,    h1m18,    h1m19           &
     &,                     h1m20,   h1m21,    h1m22,    h1m23           &
     &,                     h1m24,   h26m30,   h14m30,   h25m31          &
     &,                     h21m31,  h12m31,   h9m32,    h55m32          &
     &,                     h45m32,  h4m33,    h62m34
!
      real (kind=kind_rad) hm2m2,    hm6666m2, hmp5,     hmp575          &
     &,                     hmp66667, hmp805,   hm1ez,    hm13ez         &
     &,                     hm19ez,   hm1e1,    hm1597e1, hm161e1        &
     &,                     hm1797e1, hm181e1,  hm8e1,    hm1e2 
!
      end module hcon

      module rnddta

      use machine
      implicit none

       save
!
      integer nblw, nblx, nbly, nblm
!
      parameter (nblw=163,nblx=47,nbly=15)
      parameter (nblm=nbly-1)
!
!    common block bandta contains random band parameters for the lw
!    calculations using 10 cm-1 wide bands.the 15 um co2 complex
!    is 2 bands,560-670 and 670-800 cm-1. ozone coefficients are
!    in 3 bands,670-800 (14.1 um),990-1070 and 1070-1200 (9.6 um).
!    the  (nblw) bands now include:
!                56 bands, 10  cm-1 wide    0  -   560  cm-1
!                 2 bands, 15 um complex  560  -   670  cm-1
!                                         670  -   800  cm-1
!                 3 "continuum" bands     800  -   900  cm-1
!                                         900  -   990  cm-1
!                                        1070  -   1200 cm-1
!                 1 band for 9.6 um band  990  -   1070 cm-1
!               100 bands, 10 cm-1 wide  1200  -   2200 cm-1
!                 1 band for 4.3 um src  2270  -   2380 cm-1
!    thus nblw presently equals    163
!    all bands are arranged in order of increasing wavenumber
!
!        arndm   =   random "a" parameter for (nblw) bands
!        brndm   =   random "b" parameter for (nblw) bands
!        betad   =   continuum coefficients for (nblw) bands
!        ap,bp   =   capphi coefficients for (nblw) bands
!        atp,btp =   cappsi coefficients for (nblw) bands
!        bandlo  =   lowest frequency in each of (nblw) freq. bands
!        bandhi  =   highest frequency in each of (nblw) freq. bands
!        ao3rnd  =   random "a" parameter for ozone in (3) ozone
!                    bands
!        bo3rnd  =   random "b" parameter for ozone in (3) ozone
!                    bands
!        ab15    =   the product arndm*brndm for the two bands
!                    representing the 15 um band complex of co2
!     data for arndm,brndm,ap,bp,atp,btp,ao3rnd,bo3rnd are obtained by
!     using the afgl 1982 catalog. continuum coefficients are from
!     roberts (1976).
      real (kind=kind_rad) arndm(nblw),brndm(nblw),betad(nblw),ap(nblw)  &   
     &,                 bp(nblw),atp(nblw),btp(nblw),bandlo(nblw)        &
     &,                 bandhi(nblw),ao3rnd(3),bo3rnd(3),ab15(2)
      data ao3rnd /                                                      &
     &   0.543368e+02,  0.234676e+04,  0.384881e+02/
      data bo3rnd /                                                      &
     &   0.526064e+01,  0.922424e+01,  0.496515e+01/
!
!    common block bdwide contains random band parameters for specific
!    wide bands. at present,the information consists of 1) random
!    model parameters for the 15 um band,560-800 cm-1; 2) the
!    continuum coefficient for the 800-990,1070-1200 cm-1 band
!        specifically:
!        awide       =   random "a" parameter for  band
!        bwide       =   random "b" parameter for  band
!        betawd      =   continuum coefficients for band
!        apwd,bpwd   =   capphi coefficients for  band
!        atpwd,btpwd =   cappsi coefficients for band
!        bdlowd      =   lowest frequency in each  freq  band
!        bdhiwd      =   highest frequency in each freq  band
!        ab15wd      =   the product arndm*brndm for the one band
!                        representing the 15 um band complex of co2
!        betinw      =   cont.coefficient for a specified wide
!                        freq.band (800-990 and 1070-1200 cm-1).
!        sko2d       =   1./betinw, used in spa88 for cont. coeffs
!        skc1r       =   betawd/betinw, used for cont. coeff. for
!                        15 um band in fst88
!        sko3r       =   ratio of cont. coeff. for 9.9 um band to
!                        betinw, used for 9.6 um cont coeff in fst88
!     data for awide,bwide,apwd,bpwd,atpwd,btpwd,ao3wd,bo3wd are
!     obtained by using the afgl 1982 catalog. continuum coefficients
!     are from roberts (1976).
      real (kind=kind_rad) awide,bwide,betawd,apwd,bpwd,atpwd,btpwd,     &
     &                  bdlowd,bdhiwd,betinw,ab15wd,sko2d,skc1r,sko3r
      data awide  / 0.309801e+01/
      data bwide  / 0.495357e-01/
      data apwd   / 0.177115e-01/
      data bpwd   /-0.545226e-04/
      data atpwd  / 0.187967e-01/
      data btpwd  /-0.567449e-04/
      data betawd / 0.347839e+02/
      data betinw / 0.766811e+01/
      data bdlowd / 0.560000e+03/
      data bdhiwd / 0.800000e+03/
!
!    common block bdcomb contains random band parameters for the lw
!    calculations using combined wide frequency bands between 160 and
!    1200 cm-1,as well as the 2270-2380 band for source calc.
!        bands 1-8: combined wide frequency bands for 160-560 cm-1
!        bands 9-14: frequency bands,as in bandta (narrow bands)
!                    for 560-1200 cm-1
!        band  15:  frequency band 2270-2380 cm-1,used for source
!                   calculation only
!        thus nbly presently equals   15
!
!        bands are arranged in order of increasing wavenumber
!        acomb       =   random "a" parameter for (nbly) bands
!        bcomb       =   random "b" parameter for (nbly) bands
!        betacm      =   continuum coefficients for (nbly) bands
!        apcm,bpcm   =   capphi coefficients for (nbly) bands
!        atpcm,btpcm =   cappsi coefficients for (nbly) bands
!        bdlocm      =   lowest frequency in each of (nbly) freq. bands
!        bdhicm      =   highest frequency in each of (nbly) freq. bands
!        ao3cm       =   random "a" parameter for ozone in (3) ozone
!                        bands
!        bo3cm       =   random "b" parameter for ozone in (3) ozone
!                        bands
!        ab15cm      =   the product arndm*brndm for the two bands
!                        representing the 15 um band complex of co2
!        betinc      =   cont.coefficient for a specified wide
!                        freq.band (800-990 and 1070-1200 cm-1).
!        iband       =   index no of the 40 wide bands used in
!                        combined wide band calculations. in other
!                        words,index telling which of the 40 wide
!                        bands between 160-560 cm-1 are included in
!                        each of the first 8 combined wide bands
!     data for acomb,bcomb,apcm,bpcm,atpcm,btpcm,ao3cm,bo3cm are
!     obtained by using the afgl 1982 catalog. continuum coefficients
!     are from roberts (1976). iband index values are obtained by
!     experimentation.
      integer  iband(40)
      real (kind=kind_rad) acomb(nbly),bcomb(nbly),                      &      
     &                  betacm(nbly),apcm(nbly),bpcm(nbly),atpcm(nbly),  &
     &                  btpcm(nbly),bdlocm(nbly),bdhicm(nbly),betinc,    &
     &                  ao3cm(3),bo3cm(3),ab15cm(2)
      data acomb  /                                                      &
     &   0.152070e+05,  0.332194e+04,  0.527177e+03,  0.163124e+03,      &
     &   0.268808e+03,  0.534591e+02,  0.268071e+02,  0.123133e+02,      &
     &   0.600199e+01,  0.640803e+00,  0.501549e-01,  0.167961e-01,      &
     &   0.178110e-01,  0.170166e+00,  0.537083e-02/
      data bcomb  /                                                      &
     &   0.152538e+00,  0.118677e+00,  0.103660e+00,  0.100119e+00,      &
     &   0.127518e+00,  0.118409e+00,  0.904061e-01,  0.642011e-01,      &
     &   0.629660e-01,  0.643346e-01,  0.717082e-01,  0.629730e-01,      &
     &   0.875182e-01,  0.857907e-01,  0.214005e+00/
      data apcm   /                                                      &
     &  -0.671879e-03,  0.654345e-02,  0.143657e-01,  0.923593e-02,      &
     &   0.117022e-01,  0.159596e-01,  0.181600e-01,  0.145013e-01,      &
     &   0.170062e-01,  0.233303e-01,  0.256735e-01,  0.274745e-01,      &
     &   0.279259e-01,  0.197002e-01,  0.349782e-01/
      data bpcm   /                                                      &
     &  -0.113520e-04, -0.323965e-04, -0.448417e-04, -0.230779e-04,      &
     &  -0.361981e-04, -0.145117e-04,  0.198349e-04, -0.486529e-04,      &
     &  -0.550050e-04, -0.684057e-04, -0.447093e-04, -0.778390e-04,      &
     &  -0.982953e-04, -0.772497e-04, -0.748263e-04/
      data atpcm  /                                                      &
     &  -0.106346e-02,  0.641531e-02,  0.137362e-01,  0.922513e-02,      &
     &   0.136162e-01,  0.169791e-01,  0.206959e-01,  0.166223e-01,      &
     &   0.171776e-01,  0.229724e-01,  0.275530e-01,  0.302731e-01,      &
     &   0.281662e-01,  0.199525e-01,  0.370962e-01/
      data btpcm  /                                                      &
     &  -0.735731e-05, -0.294149e-04, -0.505592e-04, -0.280894e-04,      &
     &  -0.492972e-04, -0.341508e-04, -0.362947e-04, -0.250487e-04,      &
     &  -0.521369e-04, -0.746260e-04, -0.744124e-04, -0.881905e-04,      &
     &  -0.933645e-04, -0.664045e-04, -0.115290e-03/
      data betacm /                                                      &
     &   0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,      &
     &   0.188625e+03,  0.144293e+03,  0.174098e+03,  0.909366e+02,      &
     &   0.497489e+02,  0.221212e+02,  0.113124e+02,  0.754174e+01,      &
     &   0.589554e+01,  0.495227e+01,  0.000000e+00/
      data iband  /                                                      &
     &    2,   1,   2,   2,   1,   2,   1,   3,   2,   2,                &
     &    3,   2,   2,   4,   2,   4,   2,   3,   3,   2,                &
     &    4,   3,   4,   3,   7,   5,   6,   7,   6,   5,                &
     &    7,   6,   7,   8,   6,   6,   8,   8,   8,   8/
!
!***the following data are level-independent
!cccc data rco2/3.3e-4/
!     data g/980.665/
!cccc data ctauda/.5/
!cccc data csolar/1.96/
!cccc data ccosz/.5/
!   b0,b1,b2,b3 are coefficients used to correct for the use of 250k in
!   the planck function used in evaluating planck-weighted co2
!   transmission functions. (see ref. 4)
!     data b0,b1,b2,b3/-.51926410e-4,-.18113332e-3,
!    1 -.10680132e-5,-.67303519e-7/
!    *******************************************************************
!    *                                                                 *
!    *                           b l c k f s  from g f d l             *
!    *  unused data cleaned out - nov 86 and mar 89 ..k.a.campana....  *
!    &                                                                 *
!    *******************************************************************
!
!     for seasonal variation
!        season=1,2,3,4 for winter,spring,summer,fall only (not active)
!        season=5 - seasonal variation(i.e.interpolate to day of fcst)
!
!     integer season
!     common/diucon/season,fcstda,jtime(5),daz(12),jdnmc,                &   
!    &              fjdnmc,tslag,rlag,timin,tpi,hpi,year,day,dhr,ixxxx
!     data season/5/
!     data tslag/45.25/,  rlag/14.8125/
!     data day/86400./,  year/365.25/
!     data tpi/6.283185308/,  hpi/1.570796327/
!     data day/86400./,  year/365.25/
!     data tpi/6.283185308/,  hpi/1.570796327/
!     data jtime/0,1,0,0,0/
!     data dhr/2./
!     data daz/0.,31.,59.,90.,120.,151.,181.,212.,243.,273.,304.,334./
!
!     sea surface albedo data
!
      real (kind=kind_rad) albd(21,20),za(20),trn(21),dza(19)
      data za/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,66.,62.,58.,54.,   & 
     &        50.,40.,30.,20.,10.,0.0/
      data trn/.00,.05,.10,.15,.20,.25,.30,.35,.40,.45,.50,.55,.60,.65,  &
     &         .70,.75,.80,.85,.90,.95,1.00/
      data dza/8*2.0,6*4.0,5*10.0/
!
      real (kind=kind_rad)                                               &
     &      em1v(5040),em1vw(5040),t1(5040),                             &
     &      t2(5040),t4(5040),em3v(5040)                            
!
      real (kind=kind_rad) delcm(nbly)
      data delcm  /                                                      &   
     &   0.300000e+02,  0.110000e+03,  0.600000e+02,  0.400000e+02,      &
     &   0.200000e+02,  0.500000e+02,  0.400000e+02,  0.500000e+02,      &
     &   0.110000e+03,  0.130000e+03,  0.100000e+03,  0.900000e+02,      &
     &   0.800000e+02,  0.130000e+03,  0.110000e+03/
!
!      real (kind=kind_rad) sc
!      data sc/2.0/
!
      end module rnddta
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module comcd1

      use machine
      implicit none

      save

      real (kind=kind_rad) ptopc(4,2),cvtop,vvcld(2),clapse
      real (kind=kind_rad) crhrh,pstrt,clapkc,dclps,clpse
      integer klowt,klowb,llyr

      end module comcd1
